# PURPOSE: WHAT A REGRESSION COLUMN ESTIMATES -- one COMPOSED library answering the user's three
#   questions at once.
#
#     link    = which measure the MODEL estimates   ("odds_ratio" | "ratio" | "difference")
#     measure = which measure is REPORTED           (the same words, plus "raw_coefficient")
#     effect  = where the number comes from         ("conditional" | "marginal" | "at_reference")
#
# THE RULE EVERYTHING HERE DERIVES FROM. A LINK IS A MEASURE -- the one a model estimates directly --
# so the argument that names the model takes the same words as the argument that names the report,
# and the statistician's vocabulary never surfaces:
#
#     difference <-> identity        ratio <-> log        odds_ratio <-> logit
#
# and a RAW COEFFICIENT exists only where the two agree -- and only as the model's OWN number, i.e.
# `effect = "conditional"`: there is no marginal coefficient, and asking for one says so. Otherwise the measure is applied to the model's
# PREDICTIONS, averaged over the sample or read at one profile. Those are the only glm links whose
# coefficient names a measure of deviation -- probit, cauchit, sqrt and inverse name none, which is
# exactly why such models are reported through marginal effects.
#
# THE CASCADE. `"auto"` means FOLLOW FROM THE LEFT: family -> link -> measure -> effect. Set any one
# and everything to its right re-derives. ONE clause qualifies it, and it is the only place an
# argument reads one to its right: `"auto"` never resolves to a PREDICTED odds ratio. A marginal
# odds ratio is a specialist quantity (Karlson & Jann 2023), so it is asked for by name; auto falls
# back to the LEVEL's own measure, which for a percentage is "x times as likely".
#
# WHAT A LEVEL IS decides which measures exist at all, and an ORDERED outcome's level is a RANK --
# compared by pairs of people rather than by shares, which is what lets it report in ONE column.
# REG_LEVEL_MEASURES holds that vocabulary and the reason it is not a `var_kind`.
#
# THE LIBRARY IS COMPOSED, NEVER WRITTEN. reg_compose_library() emits one row per BUILDABLE
# (link, effect, measure) from four facts a family declares -- `level`, `fits`, `words`, `note` --
# and two shared maps (REG_MEASURE_LINK, REG_LEVEL_MEASURES). So adding a family is one REG_FAMILIES
# row and adding a link one map entry, and a family CANNOT declare a scale, a header word and a
# crude shape that disagree with each other, because it declares none of the three.
#
# THE STATES a user must be able to tell apart. A row in the table -> build it. No row -> one of four
# refusals, DERIVED from the clause that failed rather than declared: `impossible` (the level has no
# such measure -- an odds ratio of a continuous outcome is not a thing, whatever anyone implements),
# `no_link` (tabxplor fits no such model here), `no_coefficient` (the model estimates something else,
# so name the two cures), `not_offered` (a hole, with what IS offered enumerated from the table). A
# fifth exists only at run time: the fit that did not converge (reg_fit()).
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
# ALSO HERE: REG_FAMILIES -- what each model family is CALLED, what it fits and where it may be
# named, so the footer sentence, the Excel filename tag and the two jamovi picker labels cannot
# disagree; `ui = NA` IS the fact "not offered in the picker", and declaring no `fits` IS the fact
# "not a user family".
#
# WARNING -- i18n: every user-visible string in these tables (`long`, `why`, `note`) is a BARE MSGID,
# gettext()'d by its reader at render. A top-level gettext() would evaluate once at load and freeze
# the build locale, making the language switch a no-op.
# See: CLAUDE.md section "tabxplor architecture" (the regression subsystem).


# --- the measure vocabulary --------------------------------------------------------------------------
#
# THREE base geometries + `raw_coefficient`, which is NOT a peer: it is the model's own coefficient,
# the fit un-transformed. A precise spelling (`log_odds` / `log_risk` / `log_rate`) additionally PINS
# which base. ⚠ It is TOTAL: on a link that already is additive (identity) there is nothing to
# un-exponentiate, so it resolves to the additive row itself -- see reg_estimand()'s fall-through.
# ⚠ And it is CONDITIONAL-ONLY: a raw coefficient is the fit's own, so there is no such thing as a
# marginal one -- reg_compose_log() emits a log twin of the conditional rows alone.
#
# THE ACRONYMS ARE NOT DECLARED HERE. They are MEASURE_ACRONYMS (R/fmt_class.R), the one table every
# argument that names a measure reads, so `tab(color = "RR")` and `tab_reg(measure = "RR")` cannot
# answer differently. This is the REGRESSION VIEW of it: the same acronyms, plus the three a model
# alone estimates (MEASURE_ACRONYMS_REG), plus the `log*` family and the cascade's `auto` -- the two
# things a crosstab has no use for. Composed, never listed, so a spelling exists once.
# ⚠ Case-sensitive, and deliberately: `Difference` / `ODDS_RATIO` / `Rom` are not spellings anyone
#   should be taught, and a tolower() fallback made them legal for free.
# The canonical values of `measure` -- what the argument is TAUGHT, and what every acronym resolves to.
#' @keywords internal
#' @noRd
REG_MEASURES_VALUES <- c("auto", "difference", "ratio", "odds_ratio", "raw_coefficient")

# The BASE a `log_*` spelling pins ("" = the family's default estimand, i.e. bare "raw_coefficient").
# ⚠ These ARE the spellings of `measure = "raw_coefficient"`: on a logit or a log link the model's own
# coefficient IS the log of the reported measure, which is what a user typing "log_odds" means. The
# short forms are permanent aliases -- the argument teaches the complete word, and every spelling a
# user already knows keeps resolving to it silently.
#' @keywords internal
#' @noRd
REG_LOG_BASE <- c(raw_coefficient = "", raw_coef = "", raw_coeff = "",
                  coefficient = "", coef = "", coeff = "", log = "", log_odds = "odds_ratio",
                  log_risk = "ratio", log_rate = "ratio", log_ratio = "ratio")

#' @keywords internal
#' @noRd
REG_MEASURE_SPELLINGS <- {
  v <- c(stats::setNames(REG_MEASURES_VALUES, REG_MEASURES_VALUES),
         measure_twins(c(MEASURE_ACRONYMS, MEASURE_ACRONYMS_REG)),
         stats::setNames(rep("raw_coefficient", length(REG_LOG_BASE)), names(REG_LOG_BASE)))
  v[!duplicated(names(v))]
}

# reg_measure_key() -- one spelling -> (measure, log_base). The twin of measure_key() on the colour
# side. Returns NULL for an unknown spelling, so the caller aborts naming the argument.
#' @keywords internal
#' @noRd
reg_measure_key <- function(x) {
  if (is.null(x) || length(x) != 1L || is.na(x)) return(list(measure = "auto", log_base = ""))
  x   <- as.character(x)
  key <- unname(REG_MEASURE_SPELLINGS[x])
  if (is.na(key)) return(NULL)
  base <- if (identical(key, "raw_coefficient")) unname(REG_LOG_BASE[x]) else ""
  list(measure = key, log_base = if (is.na(base)) "" else base)
}

# REG_CELL_DIGITS -- how many decimals a REGRESSION cell prints, per estimate scale. ONE declaration,
# read by every builder, because the crude and model columns of one comparison must print the same
# quantity to the same precision.
#
# It is the LEVEL's precision, not the estimate's: a token too coarse at 0 raises itself through
# DISPLAY_TOKENS$min_digits.
# ⚠ IT IS THE LEVEL'S, so on a scale whose ESTIMATE needs more than its level does it must not be
# read as the estimate's: in format() a 0 means "unset" and is what lets a token's own minimum
# apply, while a 1 SILENCES it -- which is how `score_ratio` printed x1.4 where the `ratio` token
# asks for x1.44. Those scales state the estimate's precision separately (EST_SCALES$est_digits),
# and this stays the number a mean score, a baseline and an aside are all read at.
# WARNING: a crosstab's digits are its own (tab(digits =)); this is the regression side only.
#' @keywords internal
#' @noRd
REG_CELL_DIGITS <- c(odds_ratio = 0L, score_odds_ratio = 1L, pct_ratio = 0L, score_ratio = 1L,
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
# branch; the declared `measure = "raw_coefficient"` rows carry no `log_of` and need none (their
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
REG_EFFECTS_VALUES  <- c("auto", "conditional", "marginal", "at_reference")
# The three CONTRASTS themselves -- the value set minus the cascade's sentinel.
#' @keywords internal
#' @noRd
REG_CONTRAST_VALUES <- setdiff(REG_EFFECTS_VALUES, "auto")

# --- a link IS a measure ------------------------------------------------------------------------------
#
# THE ONE PLACE THE STATISTICIAN'S WORD APPEARS. Adding a link is one row here plus one `fits` entry
# on the families that can fit it; the availability rule, the scale, the header word and the crude
# companion all derive (cloglog -> hazard ratio is the one credible candidate, and after it the well
# runs dry).
#' @keywords internal
#' @noRd
REG_MEASURE_LINK <- c(difference = "identity", ratio = "log", odds_ratio = "logit")

#' @keywords internal
#' @noRd
REG_LINKS_VALUES <- c("auto", names(REG_MEASURE_LINK))

# The glm spellings, accepted silently and never taught. ⚠ THE ONE VOCABULARY NOT SHARED, and the
# reason it stays a table of its own: on `link` the word "log" means the LOG LINK, while on `measure`
# it is a SPELLING of "raw_coefficient". Consulted FIRST by reg_link_key() for exactly that reason.
#' @keywords internal
#' @noRd
REG_LINK_ALIASES <- c(identity = "difference", log = "ratio", logit = "odds_ratio")

# reg_link_key() -- one spelling -> a link, NULL for an unknown one (so the caller aborts naming the
# argument). A link takes `measure`'s own values, which is what keeps four arguments feeling like two,
# plus the INTERNAL FIT KEYS (REG_FIT_SPELLINGS), so what reg_formulas() prints in its `fit` column
# can be typed straight back into `link`.
#' @keywords internal
#' @noRd
reg_link_key <- function(x) {
  if (is.null(x) || length(x) != 1L || is.na(x)) return("auto")
  x <- as.character(x)[[1L]]
  a <- unname(REG_LINK_ALIASES[x])
  if (!is.na(a)) return(a)
  f <- unname(REG_FIT_SPELLINGS[x])
  if (!is.na(f)) return(f)
  k <- reg_measure_key(x)
  if (is.null(k) || identical(k$measure, "raw_coefficient")) return(NULL)
  k$measure
}

# The marginaleffects contrast a REPORTED link asks for. NA = the additive default.
#' @keywords internal
#' @noRd
REG_MARGINAL_COMPARISON <- c(identity = NA_character_, log = "lnratioavg", logit = "lnoravg")


# --- what a LEVEL can be compared by ------------------------------------------------------------------
#
# A measure exists for an outcome when the outcome's LEVEL can be transformed by that measure's link:
# a PERCENTAGE has an identity, a log and a logit, so all three; a MEAN or a COUNT has no odds, so no
# odds ratio.
#
# ⚠ A LEVEL KIND IS NOT A `var_kind`. The two coincided while every level was a number a cell holds
# (pct / mean / count, EST_SCALES' own words), and `rank` is where they part: it asks WHICH MEASURES
# EXIST, while a var_kind says how a number formats and colours. A rank's two measures are stamped
# with ordinary `points` / `pct_ratio` scales, so nothing downstream learns a fourth var_kind.
#
# A RANK is what an ORDERED outcome's cell is: not a share of one category but a share of PAIRS. Of
# two people, one from this group and one from the reference group, who ends up higher? That pair --
# (win, loss) -- has the same two readings as a level and its reference, additive and multiplicative,
# and it is the exact K-category generalisation of a binary outcome's own measures: at K = 2,
# win - loss IS the risk difference and win / loss IS the odds ratio. It carries no `at_reference`
# row (reg_compose_row()) because a pair drawn at one profile is a different, matched estimand.
#
# Each cell names the EST_SCALES row the column is stamped with and the base acronym its header
# prints. ⚠ ORDER IS LOAD-BEARING: the first entry is the LEVEL'S OWN measure, which is what the
# cascade's one clause falls back to rather than marginalise a non-collapsible link.
#' @keywords internal
#' @noRd
REG_LEVEL_MEASURES <- list(
  pct   = list(ratio      = c(scale = "pct_ratio",  word = "RR"),
               difference = c(scale = "points",     word = "RD"),
               odds_ratio = c(scale = "odds_ratio", word = "OR")),
  mean  = list(difference = c(scale = "raw_diff",   word = "diff"),
               ratio      = c(scale = "mean_ratio", word = "RoM")),
  count = list(difference = c(scale = "raw_diff",   word = "diff"),
               ratio      = c(scale = "mean_ratio", word = "IRR")),
  # DESIGN: Somers' D first -- it is stable in K (measured: 0.212 at K=4, 0.227 at K=20 for a
  # cumulative OR of 2) where the win ratio drifts (1.79 -> 1.63), so it is what `auto` falls back to.
  rank  = list(difference = c(scale = "points",     word = "D"),
               ratio      = c(scale = "pct_ratio",  word = "WR"),
               odds_ratio = c(scale = "odds_ratio", word = "OR"))
)

#' @keywords internal
#' @noRd
reg_level_measures <- function(level) {
  if (is.null(level) || !length(level) || is.na(level[[1]]) ||
      !level[[1]] %in% names(REG_LEVEL_MEASURES)) return(character(0))
  names(REG_LEVEL_MEASURES[[level[[1]]]])
}

# --- which acronyms only a MODEL can print ------------------------------------------------------------
#
# The (acronym -> measure) pairs the library DERIVES: one per REG_LEVEL_MEASURES cell, plus a family's
# own header-word override. This IS the shared acronym vocabulary (MEASURE_ACRONYMS +
# MEASURE_ACRONYMS_REG, R/fmt_class.R), and two foreign keys check the two directions at load -- so an
# acronym cannot name a measure no header prints, nor a header print one no argument takes.
#' @keywords internal
#' @noRd
reg_word_measures <- function() {
  lv <- unlist(lapply(REG_LEVEL_MEASURES, function(l)
    vapply(names(l), function(m) paste0(l[[m]][["word"]], "->", m), character(1))), use.names = FALSE)
  fw <- unlist(lapply(REG_FAMILIES, function(r)
    if (is.null(r$words)) character(0) else
      vapply(names(r$words), function(m) paste0(r$words[[m]], "->", m), character(1))),
    use.names = FALSE)
  unique(c(lv, fw))
}

# ...and of those, the ones a CROSSTAB can never print: a crosstab cell is a share, a mean or a count,
# while a RANK is a share of PAIRS -- which only a model estimates. That is the whole rule behind
# MEASURE_ACRONYMS_REG, stated where the level kinds live rather than at the colour engine.
#' @keywords internal
#' @noRd
reg_model_only_words <- function() {
  crosstab <- unlist(lapply(REG_LEVEL_MEASURES[setdiff(names(REG_LEVEL_MEASURES), "rank")],
                            function(l) vapply(l, function(z) z[["word"]], character(1))),
                     use.names = FALSE)
  setdiff(unique(sub("->.*$", "", reg_word_measures())), crosstab)
}


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
# WARNING: this list and the shared acronym vocabulary (MEASURE_ACRONYMS + MEASURE_ACRONYMS_REG,
# R/fmt_class.R) are the SAME SET, so what a header prints can always be typed back into `measure`,
# and no acronym can name a measure the table does not print. Foreign keys check both directions at
# load -- one for the names, one for the (acronym -> measure) pairs REG_LEVEL_MEASURES derives.
#' @keywords internal
#' @noRd
REG_WORDS <- list(
  OR    = list(long = function() gettext("odds ratio"),            noncollapsible = TRUE),
  cumOR = list(long = function() gettext("cumulative odds ratio"), noncollapsible = TRUE),
  RR    = list(long = function() gettext("risk ratio"),            noncollapsible = FALSE),
  RD    = list(long = function() gettext("risk difference"),       noncollapsible = FALSE),
  IRR   = list(long = function() gettext("incidence-rate ratio"),  noncollapsible = FALSE),
  RoM   = list(long = function() gettext("ratio of means"),        noncollapsible = FALSE),
  diff  = list(long = function() gettext("mean difference"),       noncollapsible = FALSE),
  # DESIGN: both COLLAPSIBLE, and measured rather than assumed: with a covariate independent of the
  # exposure (so no confounding to find), the cumulative odds ratio moves 1.47 -> 2.24 while the
  # superiority pair does not move at all. That is what makes `color = "adjustment"` a TEST here.
  D     = list(long = function() gettext("Somers' D"),             noncollapsible = FALSE),
  WR    = list(long = function() gettext("win ratio"),             noncollapsible = FALSE)
)

# REG_CONTRASTS -- how each `effect` marks the measure it rides on. `mark` is a PREFIX on the acronym
# (unmarked = conditional, as the literature reads an unqualified odds ratio), `long` wraps the
# expansion the same way. One row per contrast, asserted at load.
#
# The markers are prefixes rather than suffixes so the measure stays the last token of every header,
# and they are plain letters so the name remains a syntactic R name -- `t$Model_refRR` works without
# backticks, where an `@` would parse as an S4 slot access and fail with an unrelated message.
#' @keywords internal
#' @noRd
REG_CONTRASTS <- list(
  conditional  = list(mark = "",    long = function(l) l),
  marginal     = list(mark = "m",   long = function(l) gettextf("marginal %s", l)),
  at_reference = list(mark = "ref", long = function(l) gettextf("%s at the reference profile", l))
)

stopifnot("every contrast declares its marker" =
            setequal(names(REG_CONTRASTS), REG_CONTRAST_VALUES))

# reg_own_word() -- the acronym a family's OWN measure is named by ("OR", "IRR", "cumOR"), DERIVED
# from the three facts the family already declares: `level`, the first entry of `fits` (its own
# link), and any `words` override. Read by fmt_coef_label(), so a new family names its coefficient
# with no row of its own. NA on the internal link keys, which declare no `fits`.
#' @keywords internal
#' @noRd
reg_own_word <- function(family) {
  r <- REG_FAMILIES[[family %||% ""]]
  if (is.null(r) || !length(r$fits)) return(NA_character_)
  m <- names(r$fits)[[1]]
  r$words[[m]] %||% REG_LEVEL_MEASURES[[r$level]][[m]][["word"]] %||% NA_character_
}

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
  if (identical(est$measure, "raw_coefficient")) base <- gettextf("log %s", base)
  REG_CONTRASTS[[est$effect]]$long(base)
}

# The log wrapper, shared by the header and by the crude column: on an exponentiated link
# `measure = "raw_coefficient"` shows the SAME estimand un-exponentiated, so it names what it logs rather
# than collapsing to one greek letter. On an identity link there is nothing to wrap -- the fall-through
# in reg_estimand() means the row is the additive one, which keeps its own word.
#' @keywords internal
#' @noRd
reg_word_logged <- function(word, measure)
  if (identical(measure, "raw_coefficient")) paste0("log(", word, ")") else word

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


# REG_FAMILIES -- WHAT EACH MODEL FAMILY IS, what it can FIT and where it may be NAMED. Every other
# name table (reg_family_display_name(), reg_family_short(), the UI labels) is DERIVED from this one,
# and so is the whole estimand library -- so adding a family is one row.
#
# Every row writes its fields in ONE order -- display, short, ui, ui_binary, outcome, outcome_level,
# level, fits, words, odds_pred, crude, note, why -- so the families read down a column. They are
# grouped below by what they are FOR, not by that order.
#
# The estimand columns, which are what the library composes from:
#   level      what a cell of this outcome IS: "pct" | "mean" | "count" | "rank". It decides which
#              measures exist (REG_LEVEL_MEASURES) and which is the level's own. Declaring it (with
#              `fits`) IS the fact "a user family". ⚠ NOT a var_kind: the first three happen to
#              share EST_SCALES' words, "rank" does not -- see REG_LEVEL_MEASURES.
#   fits       THE VALUE SET OF `link`, measure-keyed, mapping to the internal fit key reg_fit()
#              takes. ⚠ ORDER IS LOAD-BEARING: the first entry is the family's own link, which is
#              what `link = "auto"` resolves to and what the default table shows.
#   words      per-measure header override, or NULL. Only ordinal declares one: a cumulative odds
#              ratio is not a plain one, and the header says so.
#   note       the QUALIFIER clause of the "Model:" footer line on a COEFFICIENT row -- what the
#              estimand is measured against. The prediction routes generate their own.
#   odds_pred  HOW a PREDICTED odds ratio is defined here, or NA. "complement" = the level and its
#              complement, so both prediction routes offer it (a binary outcome). "vsrest" = only at
#              a profile, each category versus the rest -- the `vsrest` builder, which is the one
#              answer a 3+ category outcome has to "versus what?".
#   crude      the REG_EMPIRICAL block this family's observed cells come from, when it is not its
#              own name. Only quasipoisson needs it: it differs from poisson in the VARIANCE
#              assumption, not in what it estimates.
#
# The naming columns:
#   display    a CLOSURE -- gettext() at render (so the footer follows options(tabxplor.lang)) while
#              staying statically extractable by potools. The CI_METHOD_LABELS precedent.
#   short      the filename tag (Excel sheet names).
#   ui         the PICKER label, or NA. ⚠ `NA` IS THE FACT "not offered in the picker".
#   ui_binary  the picker label OVERRIDE on a 2-level outcome.
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
                      short = "linear",   ui = "gaussian (linear)",   ui_binary = NA_character_,
                      outcome = NA_character_, outcome_level = NA_character_,
                      level = "mean",  fits = c(difference = "gaussian", ratio = "mr"),
                      note = function() gettext("vs the reference category")),
  binomial     = list(display = function() gettext("logistic regression"),
                      short = "logit",    ui = "binomial (logistic)", ui_binary = "binomial (logistic)",
                      outcome = NA_character_, outcome_level = "modelled",
                      level = "pct",   fits = c(odds_ratio = "binomial", ratio = "rr", difference = "rd"),
                      odds_pred = "complement",
                      note = function() gettext("vs the reference category")),
  poisson      = list(display = function() gettext("Poisson regression"),
                      short = "poisson",  ui = "poisson (counts)",    ui_binary = NA_character_,
                      outcome = NA_character_, outcome_level = NA_character_,
                      level = "count", fits = c(ratio = "poisson"),
                      note = function() gettext("vs the reference category")),
  multinomial  = list(display = function() gettext("multinomial logistic regression"),
                      short = "mlogit",   ui = "multinomial",         ui_binary = NA_character_,
                      outcome = NA_character_, outcome_level = "baseline",
                      level = "pct",   fits = c(odds_ratio = "multinomial"),
                      odds_pred = "vsrest",
                      note = function() gettext("each category vs the reference")),
  ordinal      = list(display = function() gettext("ordinal logistic regression"),
                      short = "ologit",   ui = "ordinal",             ui_binary = NA_character_,
                      outcome = NA_character_, outcome_level = NA_character_,
                      level = "rank",  fits = c(odds_ratio = "ordinal"),
                      words = list(odds_ratio = "cumOR"),
                      note = function() gettext("proportional-odds model"),
                      why = function() gettext(
                        "an ordinal outcome must keep the order of its levels, so none can be singled out")),
  # a USER family the picker does not offer (the checkbox route is `family = "poisson"` + a
  # dispersion warning): `ui = NA` says so once, where the label lives. It differs from poisson in
  # the VARIANCE assumption, not in what it estimates, so it borrows poisson's crude cells.
  quasipoisson = list(display = function() gettext("quasi-Poisson regression"),
                      short = "qpoisson", ui = NA_character_,         ui_binary = NA_character_,
                      outcome = NA_character_,
                      level = "count", fits = c(ratio = "quasipoisson"),
                      crude = "poisson",
                      note = function() gettext("vs the reference category")),
  # the three internal LINK keys: never named by a user, never offered by a picker, and declaring no
  # `fits` -- which IS what makes them internal. `link` is how they are reached.
  rr           = list(display = function() gettext("modified Poisson regression"),
                      short = "rr",       ui = NA_character_,         ui_binary = NA_character_,
                      outcome = "binomial"),
  rd           = list(display = function()
                        gettext("additive-risk regression (identity link, robust standard errors)"),
                      short = "rd",       ui = NA_character_,         ui_binary = NA_character_,
                      outcome = "binomial"),
  mr           = list(display = function()
                        gettext("log-link mean regression (Poisson pseudo-likelihood, robust standard errors)"),
                      short = "mr",       ui = NA_character_,         ui_binary = NA_character_,
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

# ...and which MEASURE each of them fits, DERIVED by inverting `fits` (measure -> fit key) on the
# families that offer them. So `reg_formulas()`'s `fit` column round-trips: what it printed can be
# typed back into `link`. Read only by reg_link_key(); never a `measure` spelling, because "mr" and
# "rd" name a MODEL, not a quantity to report.
#' @keywords internal
#' @noRd
REG_FIT_SPELLINGS <- {
  f <- unlist(lapply(REG_FAMILIES, function(r) r$fits))
  f <- f[f %in% REG_FIT_ONLY_FAMILIES]
  v <- stats::setNames(sub("^.*\\.", "", names(f)), unname(f))
  v[!duplicated(names(v))]
}
stopifnot("every internal fit family is reachable from some family's `fits`" =
            setequal(names(REG_FIT_SPELLINGS), REG_FIT_ONLY_FAMILIES))

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

# The link picker's labels. A LINK IS A MEASURE, so the label IS the measure's own word -- the same
# spelling `measure =` offers, and the same order, which is what lets a reader carry one vocabulary
# down the cascade. The glm spelling ("logit", "rd") is the fit's business and is not shown.
# `reg_family_ui_labels()` is its sibling -- both feed the jamovi Model table through
# dev/generate_jamovi_js.R, which is why the words live here and not in a .yaml (a jamovi
# CustomControl renders them from JS).
#' @keywords internal
#' @noRd
reg_link_ui_labels <- function()
  c(auto = "auto (family based)",
    stats::setNames(names(REG_MEASURE_LINK), names(REG_MEASURE_LINK)))

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


# --- the library, COMPOSED ----------------------------------------------------------------------------
#
# ONE ROW per (link, effect, measure) the package can BUILD. A refusal is NOT a row: reg_estimand()
# derives it from the clause that failed, so a hole and its reason cannot drift apart.
#
# The composed members, and where each comes from:
#
#   link          which measure the MODEL estimates -- the key into the family's `fits`.
#   effect        the contrast: "conditional" | "marginal" | "at_reference".
#   measure       what is REPORTED; "raw_coefficient" on a logged row, whose base is `base_measure`.
#   base_measure  the measure a logged row is the log OF (itself, on every other row). It is the
#                 LOOKUP key: under one (link, effect) a ratio and an odds ratio both log, so
#                 "raw_coefficient" alone would name two rows.
#   measure_link  the link the REPORTED comparison is taken on -- REG_EMPIRICAL$*$link's own
#                 vocabulary, and what the g-computation sweep and the crude leg both read. It
#                 EQUALS the model's link exactly on a coefficient row.
#   builder       which of reg_build()'s three column builders runs: "coef" | "ame" | "vsrest".
#   fit           the internal family key handed to reg_fit(), i.e. `fits[[link]]`. It is where a
#                 link becomes a different MODEL: "rr" = modified Poisson (a conditional risk
#                 ratio), "rd" = identity link (a risk difference), "mr" = log-link pseudo-ML.
#   exp           exponentiate the tidy estimate: multiplicative, and not logged.
#   word          the BASE measure acronym, a key into REG_WORDS. The contrast marker and the log
#                 wrapper are composed onto it by reg_word(), never declared.
#   scale         the EST_SCALES key stamped on the column. Its `est_field` says which fmt field
#                 the estimate is written into, so a scale change needs no builder change.
#   crude_fam     which REG_EMPIRICAL block the observed companion comes from; "auto" =
#                 reg_crude_key(fit, trials), which is what carries `trials` -> grouped_binomial.
#   crude_shape   which shape row inside it: the block's shape on `measure_link`.
#   comparison    the marginaleffects `comparison =` value (NA = the additive default).
#   note          a closure returning the QUALIFIER clause of the "Model:" footer line -- what the
#                 estimand is measured against, and any assumption worth one phrase. The measure
#                 itself is not repeated: the footer composes "<word> = <long> (<note>)".
#   log_of        on a logged row, the scale it is the log OF. `log_coef` is one row shared by every
#                 logged measure, so a link-scale column cannot say on its own whether its
#                 exponential is an odds or a level -- and its baseline row differs by exactly that.
#   status        "ok". Every row in this table builds; that is what makes it a table of rows.
#
# `builder` and `engine` are the two closed vocabularies the columns above are keyed on, each
# checked at load via a foreign key (R/zzz-fact-keys.R).
#' @keywords internal
#' @noRd
REG_BUILDERS <- c("coef", "ame", "vsrest")

#' @keywords internal
#' @noRd
REG_MARGINAL_ENGINES <- c("gcomp", "marginaleffects")

# reg_marginal_engine() -- WHICH ENGINE computes a row's marginal quantities. Stated once, DERIVED
# rather than declared, because every row answers it the same way:
#   "gcomp"           tabxplor's own g-computation -- one counterfactual sweep giving the estimate,
#                     the adjusted predictions and an ANALYTIC jacobian, whose delta-method interval
#                     reproduces marginaleffects to 1e-8 (glm and weighted svyglm alike) at ~25x the
#                     speed;
#   "marginaleffects" the numerical-jacobian route -- everything at `at_reference`, whose contrast
#                     lives on a one-row profile grid that g-computation does not build.
# It is a PERMISSION, not a promise: the producer returns NULL rather than a wrong number and
# reg_marginal() then falls back for the WHOLE call, so one column always carries one convention. It
# is ALSO the dependency rule: `marginaleffects` is a hard requirement exactly where this resolves to
# it, and every other row runs dependency-free.
#' @keywords internal
#' @noRd
reg_marginal_engine <- function(est)
  if (identical(est$effect, "at_reference")) "marginaleffects" else "gcomp"

# May an `obs` (crude) value be attached cell by cell? Not at the reference profile, where the model
# is conditional while the observed columns stay marginal.
#' @keywords internal
#' @noRd
reg_estimand_obs <- function(est) !identical(est$effect, "at_reference")

# The scale + acronym one (level, measure) pair carries. A family may override the WORD -- an
# ordinal model's cumulative odds ratio is not a plain one, and its header says so.
#' @keywords internal
#' @noRd
reg_measure_cell <- function(family, measure) {
  fam  <- REG_FAMILIES[[family]]
  cell <- REG_LEVEL_MEASURES[[fam$level %||% ""]][[measure]]
  if (is.null(fam$level) || is.null(cell)) return(NULL)
  w <- if (measure %in% names(fam$words)) fam$words[[measure]] else unname(cell[["word"]])
  list(scale = unname(cell[["scale"]]), word = w)
}

# REG_FAMILY_MULT_WORD -- the MULTIPLICATIVE effect word of a fit key: what exp(coef) is CALLED for
# this link (OR / RR / IRR / RoM / cumOR; NA where the link's coefficient is additive).
#
# ⚠ keyed on the FIT, not on the family that declares it: a binomial outcome fits BOTH the logit
# ("OR") and the modified Poisson ("RR"), so "the binomial family's word" is ambiguous where "the fit
# key's" is not. Two families claiming one fit with two words fails to LOAD.
#' @keywords internal
#' @noRd
REG_FAMILY_MULT_WORD <- local({
  pairs <- Filter(Negate(is.null), unlist(lapply(names(REG_FAMILIES), function(f) {
    fits <- REG_FAMILIES[[f]]$fits
    lapply(names(fits), function(m)
      if (identical(unname(REG_MEASURE_LINK[m]), "identity")) NULL
      else list(fit = unname(fits[[m]]), word = reg_measure_cell(f, m)$word))
  }), recursive = FALSE))
  vapply(stats::setNames(nm = names(REG_FAMILIES)), function(k) {
    w <- unique(vapply(Filter(function(p) identical(p$fit, k), pairs),
                       function(p) p$word, character(1)))
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

# The PUBLIC family vocabulary -- what `tab_reg(family =)` accepts. Declaring `fits` IS the fact "a
# user family": the internal link keys (rr / rd / mr) declare none and are reached through `link`.
#' @keywords internal
#' @noRd
REG_USER_FAMILIES <- Filter(function(f) !is.null(REG_FAMILIES[[f]]$fits), names(REG_FAMILIES))

# reg_emp_block() -- the REG_EMPIRICAL block a family-or-fit key resolves to: its own if it has
# one, else the outcome family it belongs to. ONE rule with TWO readers -- the library composes each
# row's crude companion through it, and reg_crude_key() (R/tab_reg.R) answers the same question at
# run time, where `trials` may move the block to grouped_binomial first.
#' @keywords internal
#' @noRd
reg_emp_block <- function(key) {
  if (is.null(key) || length(key) != 1L || is.na(key)) return(NA_character_)
  if (!is.null(REG_EMPIRICAL[[key]])) return(key)
  out <- REG_FAMILIES[[key]]$crude %||% unname(REG_FIT_FAMILY[key])
  if (is.null(out) || is.na(out) || is.null(REG_EMPIRICAL[[out]])) NA_character_ else out
}

# The observed companion, derived. It is the block's shape on the REPORTED link -- and where the
# outcome's own block has none, the block named by the `fits` entry for that measure, which IS the
# two cross-family borrows (a binary RATIO reuses REG_EMPIRICAL$rr, a gaussian one REG_EMPIRICAL$mr).
# ⚠ `crude_fam = "auto"` is kept wherever the own block suffices, so `trials` can still move it.
#' @keywords internal
#' @noRd
reg_compose_crude <- function(family, fit, measure, measure_link, logged) {
  none <- list(fam = "auto", shape = NA_character_)
  sh   <- reg_emp_shape_on(reg_emp_block(fit), measure_link, logged)
  if (!is.null(sh)) return(list(fam = "auto", shape = sh))
  borrow <- REG_FAMILIES[[family]]$fits[measure]
  if (is.na(borrow[[1]])) return(none)
  blk <- reg_emp_block(unname(borrow[[1]]))
  sh  <- reg_emp_shape_on(blk, measure_link, logged)
  if (is.null(sh)) return(none)
  list(fam = blk, shape = sh)
}

# The three PREDICTION-route phrases, generated once rather than written per row.
#' @keywords internal
#' @noRd
# `has_num` is the ONE fact the phrase cannot know at composition time: whether the model holds a
# NUMBER, which decides whether the reference profile is "their reference level" alone or also a
# "mean". reg_estimand_note() reads it off meta$predictor_types and passes it in; every other caller
# leaves it NA and gets the honest both-ways wording.
est_note_marginal <- function(kind, at_ref = FALSE, measure = "difference") {
  function(has_num = NA) {
    where <- if (!at_ref) gettext("sample-averaged")
      else if (isTRUE(has_num))  gettext("other predictors held at their reference level or mean")
      else if (isFALSE(has_num)) gettext("other predictors held at their reference level")
      else gettext("other predictors held at their reference level / mean")
    prob <- identical(kind, "prob")
    what <- if (identical(measure, "odds_ratio"))
      gettext("the odds ratio of adjusted proportions")
    else if (identical(measure, "ratio") && prob)
      gettext("the ratio of adjusted proportions")
    else if (identical(measure, "ratio"))
      gettext("the ratio of adjusted predicted values")
    else if (prob)
      gettext("on adjusted proportions, in percentage points")
    else gettext("on the response scale")
    # the separator is the TRANSLATION's (French: " ; "). A COMMA on the sample-averaged route: one
    # qualifier, not two clauses.
    if (at_ref) gettextf("%s; %s", what, where) else gettextf("%s, %s", what, where)
  }
}

# The RANK prediction phrase -- the one sentence that teaches the whole measure, so it says what is
# compared rather than naming the statistic again.
#' @keywords internal
#' @noRd
est_note_rank <- function(measure) {
  function() {
    # WARNING -- i18n: every piece is its OWN literal gettext(). A gettext() over a paste0() would
    # look up a msgid no catalogue holds, and silently return English.
    who <- gettext(
      "how often someone from this group ends up higher than someone from the reference group")
    what <- if (identical(measure, "ratio"))
      gettext("as a ratio of wins to losses")
    else gettext("as a difference in percentage points, wins minus losses")
    gettextf("%s, %s; %s", who, what, gettext("sample-averaged"))
  }
}

# The estimand phrase of one composed row: the family's own qualifier on a coefficient (saying so
# when the scale is percentage points), the generated prediction phrase otherwise.
#' @keywords internal
#' @noRd
reg_compose_note <- function(family, effect, measure, scale, vsrest) {
  if (vsrest) return(function() gettext(
    "each outcome category versus the rest; other predictors held at their reference level / mean; profile-conditional"))
  if (identical(effect, "conditional")) {
    own <- REG_FAMILIES[[family]]$note
    return(if (identical(scale, "points"))
      function() gettextf("in percentage points, %s", own()) else own)
  }
  if (identical(REG_FAMILIES[[family]]$level, "rank")) return(est_note_rank(measure))
  est_note_marginal(if (identical(REG_FAMILIES[[family]]$level, "pct")) "prob" else "raw",
                    at_ref = identical(effect, "at_reference"), measure = measure)
}

# reg_compose_row() -- ONE cell, or NULL where the cell does not build. The three clauses that can
# return NULL are exactly the three refusals reg_estimand() words.
#' @keywords internal
#' @noRd
reg_compose_row <- function(family, link, effect, measure) {
  fam  <- REG_FAMILIES[[family]]
  cell <- reg_measure_cell(family, measure)
  if (is.null(cell)) return(NULL)                                  # the level has no such measure
  pred <- !identical(effect, "conditional")
  if (!pred && !identical(measure, link)) return(NULL)              # a coefficient IS the link's
  rank <- identical(fam$level, "rank")
  # DESIGN: a superiority pair compares two people DRAWN FROM THE POPULATION. At one profile both are
  # the same person, so the pair collapses to a matched comparison -- a different estimand, and a
  # non-collapsible one (measured: it moves under adjustment with zero confounding, the marginal one
  # does not). Refused rather than silently renamed.
  if (rank && identical(effect, "at_reference")) return(NULL)
  vsrest <- FALSE
  if (pred && identical(measure, "odds_ratio")) {
    # an odds ratio needs a percentage AND its complement, which only a binary outcome has; a 3+
    # category one must be asked "versus what?", and the profile builder is the one answer to it.
    op <- fam$odds_pred %||% NA_character_
    if (identical(op, "vsrest")) {
      if (!identical(effect, "at_reference")) return(NULL)
      vsrest <- TRUE
    } else if (!identical(op, "complement")) return(NULL)
  }
  mlink <- unname(REG_MEASURE_LINK[[measure]])
  fit   <- unname(fam$fits[[link]])
  crude <- reg_compose_crude(family, fit, measure, mlink, FALSE)
  list(link = link, effect = effect, measure = measure, base_measure = measure,
       level = fam$level %||% NA_character_,
       measure_link = mlink, builder = if (!pred) "coef" else if (vsrest) "vsrest" else "ame",
       # DESIGN: WHETHER THIS ESTIMAND NEEDS ONE COLUMN PER OUTCOME CATEGORY is a fact about the
       # estimand, not about the family -- the crude side has always modelled it that way
       # (REG_EMPIRICAL$*$per_category). Derived, never declared: a prediction about a SHARE needs one
       # column per share, unless the outcome has exactly two of them (`outcome_level = "modelled"`
       # IS that fact) -- while a prediction about a RANK is one number by construction.
       per_level = pred && (vsrest || (identical(fam$level, "pct") &&
                                       !identical(fam$outcome_level, "modelled"))),
       fit = fit, exp = !identical(mlink, "identity"),
       word = cell$word, scale = cell$scale,
       crude_fam = crude$fam, crude_shape = crude$shape,
       comparison = if (!pred) NA_character_ else if (vsrest) "lnor"
                    else unname(REG_MARGINAL_COMPARISON[[mlink]]),
       note = reg_compose_note(family, effect, measure, cell$scale, vsrest),
       status = "ok")
}

# The LOG twin of a multiplicative row: the same fit, un-exponentiated, on the link-scale ladder. An
# additive row has no ratio to take the log of and gets none.
# DESIGN: CONDITIONAL ONLY. `raw_coefficient` names the MODEL'S OWN coefficient, and a model has no
# marginal one -- the log of a sample-averaged ratio is a quantity nobody reports. Asking for it is
# refused by reg_estimand()'s ladder, which names the cure, rather than composed as 12 exotic rows.
#' @keywords internal
#' @noRd
reg_compose_log <- function(family, r) {
  if (!isTRUE(r$exp) || !identical(r$effect, "conditional")) return(NULL)
  crude <- reg_compose_crude(family, r$fit, r$base_measure, r$measure_link, TRUE)
  r$log_of      <- r$scale
  r$measure     <- "raw_coefficient"
  r$exp         <- FALSE
  r$scale       <- "log_coef"
  r$crude_fam   <- crude$fam
  r$crude_shape <- crude$shape
  r
}

#' @keywords internal
#' @noRd
reg_compose_rows <- function(family) {
  fam  <- REG_FAMILIES[[family]]
  meas <- reg_level_measures(fam$level)
  out  <- list()
  for (lk in names(fam$fits)) for (ef in REG_CONTRAST_VALUES) for (m in meas) {
    r <- reg_compose_row(family, lk, ef, m)
    if (is.null(r)) next
    out <- c(out, list(r), list(reg_compose_log(family, r)))
  }
  Filter(Negate(is.null), out)
}

#' @keywords internal
#' @noRd
reg_compose_library <- function()
  stats::setNames(lapply(REG_USER_FAMILIES, function(f) list(rows = reg_compose_rows(f))),
                  REG_USER_FAMILIES)

#' @keywords internal
#' @noRd
REG_ESTIMANDS <- reg_compose_library()

# Build-time integrity: this table's own SELF-consistency. Cross-table foreign keys (EST_SCALES,
# REG_EMPIRICAL, reg_fit()) are declared separately, in R/zzz-fact-keys.R.
local({
  for (fam in names(REG_ESTIMANDS)) {
    rows <- REG_ESTIMANDS[[fam]]$rows
    keys <- vapply(rows, function(r) paste(r$link, r$effect, r$base_measure, r$measure), character(1))
    stopifnot(
      "every family composes at least one row"     = length(rows) > 0L,
      "no (link, effect, measure) cell is composed twice" = !anyDuplicated(keys),
      "every row's contrast is a declared value"   =
        all(vapply(rows, function(r) r$effect %in% REG_CONTRAST_VALUES, logical(1))),
      "every row's link is one the family fits"    =
        all(vapply(rows, function(r) r$link %in% names(REG_FAMILIES[[fam]]$fits), logical(1))),
      "every row carries an estimand phrase"       =
        all(vapply(rows, function(r) is.function(r$note), logical(1))),
      "a coefficient row reports the model's own measure" =
        all(vapply(rows, function(r) !identical(r$effect, "conditional") ||
                     identical(r$base_measure, r$link), logical(1))),
      "a logged row is the log of a multiplicative one" =
        all(vapply(rows, function(r) !identical(r$measure, "raw_coefficient") ||
                     !identical(r$measure_link, "identity"), logical(1)))
    )
  }
})

# --- the resolvers (the ONLY readers) ----------------------------------------------------------------

# One family's rows, at ONE link -- the shape the lister, the help and the error messages all read.
# `link = NULL` means the family's own, which is the table a user sees unless they name another model.
#' @keywords internal
#' @noRd
reg_estimands_for <- function(family, link = NULL) {
  fr <- REG_ESTIMANDS[[family]]
  if (is.null(fr)) return(NULL)
  lk <- link %||% reg_family_link(family)
  Filter(function(r) identical(r$link, lk), fr$rows)
}

# The family's OWN link -- what `link = "auto"` resolves to. The first `fits` entry; the order there
# is load-bearing for exactly this reason.
#' @keywords internal
#' @noRd
reg_family_link <- function(family) {
  f <- REG_FAMILIES[[family]]$fits
  if (is.null(f) || !length(f)) NA_character_ else names(f)[[1L]]
}

# The `link = "..."` calls a family accepts, as literal strings -- cli collapses a vector INSIDE a
# {.code} span, so the spellings are built before they are inlined.
#' @keywords internal
#' @noRd
reg_link_calls <- function(family, exclude = character(0)) {
  lk <- setdiff(names(REG_FAMILIES[[family]]$fits), exclude)
  if (!length(lk)) character(0) else paste0('link = "', lk, '"')
}

# What `measure = "auto"` resolves to: THE LINK'S OWN MEASURE -- follow from the left. The one clause
# that reads an argument to its RIGHT, and its reason: a marginal odds ratio is a specialist quantity
# and must be asked for by name, so on a PREDICTION route auto falls back to the level's own measure.
#' @keywords internal
#' @noRd
reg_auto_measure <- function(family, link, effect) {
  if (identical(effect, "auto") || identical(effect, "conditional")) return(link)
  cell <- reg_measure_cell(family, link)
  if (is.null(cell) || !reg_word_noncollapsible(cell$word)) return(link)
  reg_level_measures(REG_FAMILIES[[family]]$level)[[1L]]
}

# reg_estimand() -- THE cascade, and THE row it lands on. Formals are in CASCADE order, so a call
# reads the way the arguments resolve; every one is validated against its own vocabulary, so a stale
# positional call aborts rather than silently meaning something else.
#
# Returns one composed row, or a typed refusal (`status` in "unknown_family" / "unknown_link" /
# "unknown_measure" / "impossible" / "no_link" / "no_coefficient" / "not_offered"), which
# reg_estimand_abort() turns into a message naming the cure.
#' @keywords internal
#' @noRd
reg_estimand <- function(family, link = "auto", measure = "auto", effect = "auto") {
  fam <- REG_FAMILIES[[family]]
  fr  <- REG_ESTIMANDS[[family]]
  if (is.null(fr) || is.null(fam$fits)) return(list(status = "unknown_family", family = family))

  ef <- reg_effect_key(effect)

  lk <- reg_link_key(link)
  if (is.null(lk)) return(list(status = "unknown_link", family = family, link = link))
  if (identical(lk, "auto")) lk <- reg_family_link(family)
  if (!lk %in% names(fam$fits))
    return(list(status = "no_link", family = family, link = lk, effect = ef))

  mk <- if (is.list(measure)) measure else reg_measure_key(measure)
  if (is.null(mk)) return(list(status = "unknown_measure", family = family, measure = measure))
  logged <- identical(mk$measure, "raw_coefficient")
  # "raw_coefficient" reports the model's own coefficient, i.e. the measure the cascade would otherwise
  # report, un-transformed; a `log_*` spelling pins another base.
  base <- if (logged && nzchar(mk$log_base)) mk$log_base
          else if (logged || identical(mk$measure, "auto")) reg_auto_measure(family, lk, effect)
          else mk$measure

  # DESIGN: `raw_coefficient` is TOTAL. On a link that is already additive there is nothing to
  # un-exponentiate -- the model's own coefficient IS the additive row -- so the request falls
  # through to it rather than being refused. That is what lets one mixed-family table (a logistic
  # outcome beside a gaussian one) be asked for its coefficients at all.
  # ⚠ `asked_raw` outlives the fall-through, because the CONDITIONAL-ONLY rule is about the word the
  # user typed, not about the row it lands on: without it a marginal raw coefficient would be
  # refused on a logit link and silently answered with a marginal difference on an identity one.
  asked_raw <- logged
  if (logged && identical(unname(REG_MEASURE_LINK[[base]]), "identity")) logged <- FALSE

  if (identical(ef, "auto")) ef <- if (identical(base, lk)) "conditional" else "marginal"

  hit <- if (asked_raw && !identical(ef, "conditional")) list() else
    Filter(function(r) identical(r$link, lk) && identical(r$effect, ef) &&
             identical(r$base_measure, base) &&
             identical(identical(r$measure, "raw_coefficient"), logged), fr$rows)
  if (length(hit)) return(c(hit[[1L]], list(family = family)))

  # --- no row: which clause failed, said in the user's own terms -------------------------------
  # A RAW COEFFICIENT IS THE FIT'S OWN, so it is asked of the model and never of its predictions.
  # Said before the measure clauses below, which would otherwise blame the base measure.
  if (asked_raw && !identical(ef, "conditional"))
    return(list(status = "no_raw_coefficient", family = family, link = lk, effect = ef,
                measure = base, why = function() gettext(
                  "a raw coefficient is the model's own, so it has no marginal or at-reference form")))
  if (is.null(reg_measure_cell(family, base)))
    return(list(status = "impossible", family = family, effect = ef, link = lk, measure = base,
                why = reg_no_measure_why(family)))
  if (identical(ef, "conditional"))
    return(list(status = "no_coefficient", family = family, link = lk, effect = ef, measure = base,
                why = reg_no_coefficient_why(lk)))
  if (identical(base, "odds_ratio"))
    return(list(status = "not_offered", family = family, link = lk, effect = ef, measure = base,
                why = function() gettext(
                  "an odds ratio needs a percentage and its complement, so a 3+ category outcome has to be asked \"versus what?\" first")))
  if (identical(REG_FAMILIES[[family]]$level, "rank") && identical(ef, "at_reference"))
    return(list(status = "not_offered", family = family, link = lk, effect = ef, measure = base,
                why = function() gettext(
                  "this measure compares two people drawn from the population, and one profile holds only one")))
  list(status = "not_offered", family = family, link = lk, effect = ef, measure = base)
}

# Why a measure has no COEFFICIENT here: it is not the one the model estimates. Carried on the
# refusal so reg_measures()' note column says it too, not only the abort.
#' @keywords internal
#' @noRd
reg_no_coefficient_why <- function(link)
  function() gettextf("the model estimates %s, so this measure has no coefficient here", link)

# Why a LEVEL has no such measure -- one sentence per kind of level, so an `impossible` cell is
# generated rather than declared per family.
#' @keywords internal
#' @noRd
reg_no_measure_why <- function(family) {
  if (identical(REG_FAMILIES[[family]]$level, "rank")) return(function() gettext(
    "an ordinal outcome's cell is a rank, so its measures compare PAIRS of people, not shares"))
  what <- switch(REG_FAMILIES[[family]]$level %||% "",
                 mean = gettext("continuous"), count = gettext("a count"), NULL)
  if (is.null(what)) return(function() gettext("this outcome does not support that measure"))
  function() gettextf(
    "an odds ratio needs a probability to take the odds of; this outcome is %s", what)
}

# The enumerated refusal, generated from the table so it cannot go stale: it names which clause
# applies, then the cure, then what IS available.
#' @keywords internal
#' @noRd
reg_estimand_abort <- function(res, outcome = NULL) {
  who <- if (is.null(outcome)) "" else cli::format_inline(" for {.val {outcome}}")
  fam <- res$family
  if (identical(res$status, "unknown_family"))
    cli::cli_abort(c("Unknown {.arg family} {.val {fam}}.",
                     "i" = "Valid: {.or {.val {REG_USER_FAMILIES}}}."))
  if (identical(res$status, "unknown_link"))
    cli::cli_abort(c("Unknown {.arg link} {.val {res$link}}.",
                     "i" = "Valid: {.or {.val {REG_LINKS_VALUES}}}."))
  if (identical(res$status, "unknown_measure"))
    cli::cli_abort(c("Unknown {.arg measure} {.val {res$measure}}.",
                     "i" = "Valid: {.or {.val {REG_MEASURES_VALUES}}}."))
  fits <- reg_link_calls(fam)
  if (identical(res$status, "no_link"))
    cli::cli_abort(c(
      "tabxplor fits no {.val {res$link}} model for a {.val {fam}} outcome{who}.",
      "i" = "It fits: {.or {.code {fits}}}.",
      "i" = paste0("Any measure can still be REPORTED from those, through the model's predictions ",
                   "({.code effect = \"marginal\"}).")))
  # A COEFFICIENT is the model's own measure, so the two cures are: report it from the predictions,
  # or fit the model that estimates it -- named only where the family can actually fit it.
  if (identical(res$status, "no_coefficient")) {
    cure <- if (res$measure %in% names(REG_FAMILIES[[fam]]$fits))
      cli::format_inline("Or fit the model that estimates it: {.code link = \"{res$measure}\"}.")
    else cli::format_inline(
      "tabxplor fits no {.val {res$measure}}-link model for a {.val {fam}} outcome, so the predictions are the only route.")
    cli::cli_abort(c(
      "The model estimates {.val {res$link}}, so {.val {res$measure}} cannot be read off its coefficients{who}.",
      "i" = "Drop {.arg effect} to report it from the model's predictions instead.",
      "i" = cure))
  }
  # A RAW COEFFICIENT IS CONDITIONAL, and the cure depends on WHY this one is not: an `effect` the
  # user wrote (drop it), or a `log_*` spelling pinning a base the model does not estimate (fit it).
  if (identical(res$status, "no_raw_coefficient")) {
    link_cure <- if (!identical(res$measure, res$link) &&
                     res$measure %in% names(REG_FAMILIES[[fam]]$fits))
      cli::format_inline("Or fit the model whose own coefficient it is: {.code link = \"{res$measure}\"}.")
    cli::cli_abort(c(
      "{.code measure = \"raw_coefficient\"} has no {.val {res$effect}} form{who}: {res$why()}.",
      "i" = "Drop {.arg effect} to read the model's own coefficient.",
      "i" = "Or ask for the measure itself: {.code measure = {.val {res$measure}}}.",
      if (!is.null(link_cure)) c("i" = link_cure)))
  }
  offered <- reg_estimand_offer_lines(fam, res$link, res$effect)
  if (identical(res$status, "impossible"))
    cli::cli_abort(c("{.code measure = {.val {res$measure}}} is not defined{who}: {res$why()}.",
                     stats::setNames(offered, rep("i", length(offered)))))
  head <- if (is.function(res$why))
    cli::format_inline("tabxplor does not offer {.code effect = {.val {res$effect}}, measure = {.val {res$measure}}}{who}: {res$why()}.")
  else
    cli::format_inline("tabxplor does not offer {.code effect = {.val {res$effect}}, measure = {.val {res$measure}}}{who}.")
  cli::cli_abort(c(head, stats::setNames(offered, rep("i", length(offered)))))
}

# REG_ASIDE_NOTE -- what a cell's PARENTHETICAL holds, keyed by the token the display resolves to and
# then by the column ROLE. This is the one clause of the "Model:" line that depends on the LAYOUT
# rather than on the estimand, so it is read off the display itself and cannot claim a prediction
# where the cell prints a difference. `{ci}`, `{n}` and `{n_range}` name themselves and get no clause.
#
# DESIGN: the gloss is per ROLE because the same slot holds two different quantities -- an adjusted
# prediction on the model column, the counted one on its crude twin -- and the footer names each by
# the abbreviation the table itself prints (`adj%` / `obs%`, through display_token_label()). A role
# with no entry gets no gloss, so a table without `empirical` says nothing about a column it lacks.
#' @keywords internal
REG_ASIDE_NOTE <- list(
  obs   = list(model = function() gettext("observed (crude) effect")),
  pct   = list(model = function() gettext("adjusted/predicted proportion"),
               emp   = function() gettext("observed proportion")),
  # the same token on a RANK column is a different quantity: 50 % there means "no difference".
  rank_pct = list(model = function() gettext("probability of superiority, 50 % being a coin flip"),
                  emp   = function() gettext("observed probability of superiority")),
  mean  = list(model = function() gettext("adjusted/predicted mean"),
               emp   = function() gettext("observed mean")),
  diff  = list(model = function() gettext("the same effect as a difference"),
               emp   = function() gettext("the observed effect as a difference")),
  ratio = list(model = function() gettext("the same effect as a ratio"),
               emp   = function() gettext("the observed effect as a ratio")),
  or    = list(model = function() gettext("the same effect as an odds ratio"),
               emp   = function() gettext("the observed effect as an odds ratio")),
  coef  = list(model = function() gettext("coefficient on the model's own link scale")),
  gap   = list(model = function() gettext("distance to the observed effect"))
)

# The REG_ASIDE_NOTE key of a display token, on a given estimand: one token can name two quantities.
#' @keywords internal
#' @noRd
reg_aside_key <- function(aside, est)
  if (identical(aside, "pct") && identical(est$level, "rank")) "rank_pct" else aside

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

# reg_estimand_note() -- the estimand phrase of the "Model:" footer line, plus one gloss per part of
# the cell. It is a LIST OF "<abbreviation>: <what it is>" items, the abbreviations being exactly the
# strings the table prints above its columns, so the footer reads as the key to the cell rather than
# as a sentence about it. `role_cols` is one representative column per role (reg_role_cols()).
#' @keywords internal
#' @noRd
reg_estimand_note <- function(est, aside = "", role_cols = list(), has_num = NA) {
  if (is.null(est) || !is.function(est$note)) return("")
  # a family's own `note` takes no argument; only the generated prediction phrases ask about the data.
  qual <- if (length(formals(est$note))) est$note(has_num) else est$note()
  # "OR: odds ratio (vs the reference category)": the acronym the header prints, its expansion and
  # the qualifier, composed from one declaration each so the three cannot drift apart.
  # the two templates carry their own punctuation, so French can put its space before ":" and ";".
  out  <- gettextf("%s: %s (%s)", reg_word(est), reg_word_long(est), qual)
  gl   <- REG_ASIDE_NOTE[[reg_aside_key(aside, est)]]
  if (is.null(gl) || !length(role_cols)) return(out)
  # ⚠ ONE ITEM PER LABEL, not per role. On a rank family BOTH roles print `sup%` (the crude twin
  # measures the same thing), and glossing one label twice with two different sentences reads as two
  # quantities. The model's reading wins -- that is the column the estimand line describes.
  rs   <- names(role_cols)
  labs <- vapply(rs, function(r) if (is.null(gl[[r]])) NA_character_
                                 else display_token_label(aside, role_cols[[r]]), character(1))
  # `fromLast` drops the EARLIER of two identical labels, so the MODEL's reading survives while the
  # reading order (the crude column sits left of its twin) is kept.
  for (i in which(!is.na(labs) & !duplicated(labs, fromLast = TRUE)))
    out <- paste0(out, gettextf("; %s: %s", labs[[i]], gl[[rs[[i]]]]()))
  out
}

# reg_normalize_color() -- THE `tab_reg(color =)` boundary. What is left to CHOOSE is "compared to
# what": `adjustment` / `between_groups`, the measures whose baseline is another column. The
# allow-list is measure_nameable("reg") -- the SAME accessor tab()'s refusal and the generated
# @param read, so the three cannot state three different rules.
#
# Grammar (positional c(text, background)):
#   "measure" (default) / TRUE / NULL / "auto"   the column's own measure  (sentinel NA_character_)
#   FALSE / "no"          no colour anywhere
#   "adjustment" / "between_groups" (either channel)
#   c("measure", "adjustment")  the headline: effect size in the text, adjustment behind it
# DESIGN: "measure" is the DEFAULT spelling because a regression column states what it estimates, so
# naming its own measure is the whole of the choice -- and it makes the two-channel headline one
# plain character vector instead of the c(TRUE, "adjustment") that c() coerced anyway. It is NOT a
# MEASURES row: measure_nameable("reg") is what the refusal below enumerates, and it must keep
# naming only the two measures whose baseline is another column.
#' @keywords internal
#' @noRd
reg_normalize_color <- function(color) {
  if (is.null(color) || isTRUE(color) || identical(color, "measure")) return(NA_character_)
  if (isFALSE(color))                   return("no")
  out <- vapply(seq_along(color), function(i) {
    v <- color[[i]]
    # WARNING: `c(TRUE, "adjustment")` is COERCED by c() to strings, so string spellings must be
    # accepted too; `is.na()` is the sentinel throughout. A background slot has no geometry of its
    # own, so an auto/TRUE there means "no background colour".
    if (isTRUE(v)  || identical(v, "auto") || identical(v, "TRUE") || identical(v, "measure") ||
        is.na(v))
      return(if (i == 1L) NA_character_ else "no")
    if (isFALSE(v) || identical(v, "no")   || identical(v, "FALSE") || identical(v, "")) return("no")
    v <- as.character(v)
    key <- measure_key(v)
    own <- measure_nameable("reg")
    if (!is.na(key) && nzchar(key) && key %in% own) return(key)
    cli::cli_abort(c(
      "{.arg color} = {.val {v}} is not a {.fn tab_reg} colour.",
      "i" = "A model column chooses what to compare its effect TO: {.or {.val {own}}}.",
      "i" = '{.code color = TRUE} colours by the effect itself.'))
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

# reg_effect_key() -- validate ONE `effect` value: WHERE THE NUMBER COMES FROM, the third question.
# "auto" is a value here, resolved by reg_estimand() once the measure is known.
#' @keywords internal
#' @noRd
reg_effect_key <- function(x) {
  if (is.null(x) || length(x) != 1L || is.na(x)) return("auto")
  x <- as.character(x)
  if (x %in% REG_EFFECTS_VALUES) return(x)
  if (identical(x, "raw_coefficient"))
    cli::cli_abort(c(
      '{.arg effect} = {.val coefficient} is now {.val conditional}.'))
  cli::cli_abort(c("Unknown {.arg effect} value {.val {x}}.",
                   "i" = "Valid: {.or {.val {REG_EFFECTS_VALUES}}}."))
}

# "here is what this outcome DOES offer" -- shared by the abort and by reg_measures(), and read at
# ONE link, because that is what the user asked about.
#' @keywords internal
#' @noRd
reg_estimand_offer_lines <- function(family, link = NULL, effect = NULL) {
  lk   <- link %||% reg_family_link(family)
  rows <- reg_estimands_for(family, lk)
  if (is.null(rows) || !length(rows)) return(character(0))
  ok <- Filter(function(r) is.null(effect) || identical(r$effect, effect), rows)
  if (!length(ok)) ok <- rows
  # ⚠ SPELLINGS, not rows: under one (link, effect) a ratio and an odds ratio BOTH log, so listing
  # every logged row would offer `measure = "raw_coefficient"` twice. Bare "raw_coefficient" reaches the one
  # the cascade picks; the pinned `log_odds` / `log_risk` spellings are expert and stay in ?tab_reg.
  ok <- Filter(function(r) !identical(r$measure, "raw_coefficient") ||
                 identical(r$base_measure, reg_auto_measure(family, lk, r$effect)), ok)
  lines <- vapply(ok, function(r) cli::format_inline(
    "{.code measure = \"{r$measure}\"} -> {.val {reg_word(r)}}, the {reg_word_long(r)}"), character(1))
  head <- if (is.null(effect) || !length(ok) || !identical(ok[[1]]$effect, effect))
    cli::format_inline("A {.val {family}} outcome offers, with {.code link = \"{lk}\"}:")
  else cli::format_inline(
    "A {.val {family}} outcome offers, with {.code link = \"{lk}\", effect = \"{effect}\"}:")
  other <- reg_link_calls(family, exclude = lk)
  c(head, unique(lines),
    if (length(other)) cli::format_inline("Other models: {.or {.code {other}}}."),
    cli::format_inline("Call {.fn reg_measures} on your outcome for every model it can take."))
}


# --- consumer 3: the lister the user can call on their own outcome -----------------------------------

#' What can this outcome be modelled as?
#'
#' Lists what [tab_reg()] can report for one outcome: which models it could fit, and which measure
#' of deviation each of them yields. It reads the same runtime table the argument validator and the
#' error messages read, so what it prints is what the function does. The section below is the same
#' table for every kind of outcome, read without any data.
#'
#' **The table has two blocks**, because the grid factors:
#' * one row per model you could fit — its **`link`**, and the measure that model's own coefficients
#'   carry (`effect = "conditional"`);
#' * then the measures read off the model's **predictions**, which are the same whichever model you
#'   fit — `link` reads `"(any)"` there.
#'
#' So `link` is the choice that matters, and it decides only which measure comes with a
#' *coefficient*: everything else is available from any of them.
#'
#' Only what can be built is listed. A measure this kind of outcome does not have simply has no row,
#' and the message says why (an odds ratio needs a probability to take the odds of). One state exists
#' only at run time: a link that does not converge on your data. `tab_reg()` says so and, for the
#' risk difference, falls back to the linear probability model.
#'
#' @param data A data frame (or a `survey` design), as for [tab_reg()].
#' @param outcome The outcome column name.
#' @param family The model family. `"auto"` (default) lists every family this kind of outcome can
#'   take, the detected one first — which is the choice to make before the others.
#' @param link Which measure the model estimates. `"all"` (default) lists every link the family
#'   fits; name one to read the table at that model alone.
#'
#' @return A tibble of `family` (only when several are listed), `link`, `measure`, `effect`,
#'   `header` (the column name it would produce) and `reads_as` (what that header's acronym means).
#' @eval reg_measures_rd()
#' @seealso [tab_reg()] to build the table, [reg_formulas()] to see the formula each column was
#'   fitted with.
#' @export
#' @examples
#' reg_measures(car_arrests, "released")
#' reg_measures(car_salaries, "salary")
reg_measures <- function(data, outcome, family = "auto", link = "all") {
  svy <- svy_unwrap_data(data, "reg_measures")
  if (!is.null(svy)) data <- svy$data
  auto_fam <- identical(family, "auto")
  fams <- if (!auto_fam) family else {
    kind <- reg_outcome_kind(data[[outcome]])
    if (!nzchar(kind)) reg_detect_family(data, outcome) else REG_OUTCOME_KINDS[[kind]]$offers
  }
  for (f in fams)
    if (is.null(REG_ESTIMANDS[[f]])) cli::cli_abort("Unknown {.arg family} {.val {f}}.")

  out <- purrr::list_rbind(purrr::map(fams, function(f) reg_measures_one(f, link, outcome)))
  if (!nrow(out))
    reg_estimand_abort(reg_estimand(fams[[1]], link = link), outcome = outcome)
  if (length(fams) == 1L) out$family <- NULL

  # ⚠ the family alone, never its short name: reg_family_short() names the family's OWN link
  # ("logit"), which would read as a claim about this table whenever a different `link` was asked
  # for -- and the `link` column now says which model each row belongs to, once per row.
  head <- if (auto_fam)
    cli::format_inline("{.val {outcome}} is a {.code family = \"{fams[[1]]}\"} outcome.")
  else cli::format_inline("{.val {outcome}}, as a {.code family = \"{fams[[1]]}\"} outcome.")
  others <- if (auto_fam && length(fams) > 1L)
    cli::format_inline("It can also be asked as {.or {.code {paste0('family = \"', fams[-1], '\"')}}}.")
  # a binomial on a NUMBER is the grouped-binomial route and needs to be told out of how many
  trials <- if ("binomial" %in% fams && identical(REG_FAMILIES[[fams[[1]]]]$level, "mean"))
    cli::format_inline(
      "{.code family = \"binomial\"} reads it as a score out of q items: pass {.arg trials}.")
  cli::cli_inform(c(
    "i" = head,
    if (!is.null(others)) c("i" = others),
    if (!is.null(trials)) c("i" = trials),
    "i" = "Any of these also reads on the model's own scale: {.code measure = \"raw_coefficient\"}."))
  out
}

# ONE family's block. THE FACTORISATION: a conditional row is a property of the LINK (a model's
# coefficients carry exactly one measure), while a prediction-based row is a property of the FAMILY
# -- g-computation averages the fitted probabilities, and averaging them does not care which link
# produced them. Listing the second block once per link is what made this table 35 rows long.
#' @keywords internal
#' @noRd
reg_measures_one <- function(fam, link = "all", outcome = NULL) {
  fits <- names(REG_FAMILIES[[fam]]$fits)
  lks  <- if (identical(link, "all")) fits else {
    lk <- reg_link_key(link)
    if (is.null(lk)) reg_estimand_abort(reg_estimand(fam, link = link), outcome = outcome)
    if (identical(lk, "auto")) reg_family_link(fam) else lk
  }
  lks <- intersect(lks, fits)                       # a link this family does not fit has no row
  row <- function(r, lk_shown) tibble::tibble(
    family = fam, link = lk_shown, measure = r$base_measure, effect = r$effect,
    header = paste0("Model_", reg_word(r)), reads_as = reg_word_long(r))

  cond <- purrr::list_rbind(purrr::map(lks, function(lk) {
    r <- reg_estimand(fam, link = lk, measure = "auto", effect = "conditional")
    if (!identical(r$status, "ok")) return(NULL)
    row(r, lk)
  }))
  # the prediction block, read at ONE link (any of them) and shown once
  base <- if (length(lks)) lks[[1L]] else reg_family_link(fam)
  pred <- purrr::list_rbind(purrr::map(
    setdiff(REG_MEASURES_VALUES, c("auto", "raw_coefficient")), function(m) {
      hits <- purrr::keep(purrr::map(c("marginal", "at_reference"), function(ef)
        reg_estimand(fam, link = base, measure = m, effect = ef)),
        function(r) identical(r$status, "ok"))
      if (!length(hits)) return(NULL)
      # BOTH forms of one measure are ONE row: `effect` is an override knob, not a third axis, and
      # a reader choosing a measure does not want to choose it twice.
      # the HEADER is the one `effect = "auto"` produces, i.e. the first form; naming both would
      # double the column's width to say what the `ref` marker already says.
      dplyr::mutate(row(hits[[1L]], gettext("(any)")),
                    effect = paste(purrr::map_chr(hits, "effect"), collapse = "|"))
    }))
  dplyr::bind_rows(cond, pred)
}

# --- consumer 4: the generated `?tab_reg` section ----------------------------------------------------
# Called from a roxygen `@eval` block, so the documentation renders FROM the resolver at document()
# time and cannot drift. (jamovi's eligibility rule is the same table's fifth reader.)
#' @keywords internal
#' @noRd
reg_measures_rd <- function() {
  line <- function(fam) {
    fits <- REG_FAMILIES[[fam]]$fits
    mods <- vapply(names(fits), function(m) sprintf(
      "\\code{link = \"%s\"}%s (%s)", m, if (identical(m, names(fits)[[1]])) ", the default" else "",
      reg_family_display_name(fits[[m]])), character(1))
    meas <- vapply(c(reg_level_measures(REG_FAMILIES[[fam]]$level), "raw_coefficient"),
                   function(m) sprintf("\\code{\"%s\"}", m), character(1))
    lvl <- c(pct = "a percentage", mean = "a mean", count = "a count",
             rank = "a position on an ordered scale")[[REG_FAMILIES[[fam]]$level]]
    paste0("  \\item \\strong{", fam, "} --- a cell is ", lvl, ". Models: ",
           paste(mods, collapse = "; "),
           ". Reported: \\code{measure = }", paste(meas, collapse = ", "), ".")
  }
  c("@section Which models each outcome offers, and which measures:",
    "Generated from the package's own resolution table, so it cannot drift from what",
    "\\code{\\link{tab_reg}()} builds. A measure that IS the model's own is read off its",
    "coefficients; any other is computed from its predictions (\\code{effect = \"marginal\"} or",
    "\\code{\"at_reference\"}).",
    "\\itemize{", vapply(setdiff(names(REG_ESTIMANDS), "quasipoisson"), line, character(1)), "}")
}

# --- consumer 5: the generated acronym grid ----------------------------------------------------
# Acronym | what it means | which outcome families print it. Generated from REG_WORDS x
# REG_ESTIMANDS so the taught vocabulary cannot drift from the headers the package builds.
#' @keywords internal
#' @noRd
reg_words_rd <- function() {
  rows <- unlist(lapply(setdiff(names(REG_ESTIMANDS), "quasipoisson"),
                        function(f) lapply(REG_ESTIMANDS[[f]]$rows,
                                           function(r) c(f = f, w = r$word))),
                 recursive = FALSE)
  fams_of <- function(w) unique(vapply(Filter(function(r) identical(r[["w"]], w), rows),
                                       function(r) r[["f"]], character(1)))
  used <- Filter(function(w) length(fams_of(w)) > 0L, names(REG_WORDS))
  item <- function(w) sprintf("  \\item \\code{%s} --- %s (%s)", w, REG_WORDS[[w]]$long(),
                              paste(fams_of(w), collapse = ", "))
  c("@section The header acronyms:",
    "A column header names the \\strong{measure}; the \\strong{contrast} is a marker on it ---",
    "no marker for a conditional effect, \\code{m} for a marginal one, \\code{ref} at the reference",
    "profile --- and \\code{measure = \"log\"} wraps it (\\code{Model_mRR}, \\code{Model_refRD},",
    "\\code{Model_log(OR)}). The observed companion carries the measure alone (\\code{Obs_RR}).",
    "\\itemize{", vapply(used, item, character(1)), "}")
}


# The "every link key is answerable by the assumption checks" assertion lives in
# R/zzz-fact-keys.R, checked at REG_ESTIMANDS$fit grain so no family's fit is missed.
