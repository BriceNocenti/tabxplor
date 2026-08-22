# PURPOSE: THE MODEL CHECKS of a tab_reg() table, the `shape =` CURE for what they flag, and the
#   primitives their plots are drawn from.
# ⚠ `shape` ITSELF LIVES IN R/var-shape.R -- one vocabulary, one cutter, one label rule, shared with
#   tab(). What stays here is the half that is genuinely about a FIT: the quadratic TERM
#   (reg_shape_term), its per-model injection (reg_shape_add), and the Linearity check whose cure it
#   is -- one builder, so the check and its cure can only ever be the same object.
# ROLE: one fact table (REG_CHECKS), one selection rule (reg_checks_for), one producer
#   (reg_check_rows) and one label builder (reg_check_label). Adding a check is ONE row: the footer
#   label, its `stats =` value, its `check =` value and its panel title all derive from that row, so
#   they cannot drift.
#
# THE IDEA. tabxplor's headline feature is a comparison -- Model_OR beside Obs_OR, coloured by the
# gap and tested by `gap_se`. Every check here is that same comparison applied to something other
# than an effect: the SHAPE of a numeric predictor's effect (Linearity), the SPREAD of the outcome
# (Dispersion), the MEANING of an ordinal effect (Proportionality), the WEIGHT of one respondent
# (Influence). Collinearity is the declared exception and says so -- it is a property of the design
# matrix and biases nothing -- and it is here because it is what every textbook, and jamovi's own
# Assumption Checks pane, puts first, so its absence would read as an omission.
#
# NOTHING HERE IS A NEW STATISTICS ENGINE. Four of the five reuse code the package already owns:
#   Linearity       reg_fit(add_terms =) + reg_nested_test()  -- both fits in hand, no second one
#   Proportionality reg_ordinal_diagnostic()                  -- the Brant test, run where its row is
#   Dispersion      reg_check_influence_pass() + reg_if_se()  -- the sandwich, design-aware
#   Influence       reg_check_influence_pass()                -- the SAME sweep, read the other way
#   Collinearity    car::vif()                                -- the one new Suggest
#
# EACH COSTS WHAT IT SAYS, AND EACH DECLARES ITS OWN DEFAULT. Two of the five need a model fit --
# Linearity one per numeric predictor, Proportionality the Brant test's auxiliary logits -- and
# REG_CHECKS$cost declares that; the other three are arithmetic on the fit in hand, and the two
# influence-based ones share one sweep. ⚠ COST DOES NOT DECIDE THE DEFAULT SET: `footer_default`
# does, because what a table must say and what it costs are two questions. Proportionality is the
# case that proves it -- a refit, and a default on every ordinal outcome, because a cumulative odds
# ratio that fails it is not one number but a fiction. Linearity is the one left to `stats =`.
# What is opt-in is the p-value, not the diagnostic: reg_curves() bins the observed shape with no fit
# at all, and the sparkline and the reg_check_plots() panels draw it for free. Two of the three free
# checks also declare a `flag` -- the conventional value past which their footer cell is marked --
# which is a rule of thumb wearing the faintest shade, never a test.
#
# THE OBSERVED SHAPE HAS THREE PIECES, at the file tail: reg_curves() (one record PER OUTCOME, each
# binning its predictors with no fit), rd_spark() (the glyph run, drawn in a window with a FLOOR so
# noise cannot read as a shape) and reg_shape_table() (where the runs go when a cell cannot hold
# them -- the console always, several outcomes everywhere).
#
# THE CURE IS PART OF THE CHECK. `shape =` is how a user fixes a non-linearity without leaving the
# framework, and its design rule keeps it small: a shape either RECODES THE COLUMN or ADDS ONE TERM,
# nothing else. A quantile-cut predictor genuinely IS a factor, so it inherits the saturated crude
# twin, the per-level counts, colours and gap tests for free. It reads the package's shared
# per-predictor grammar (per_variable(), R/reg-resolve.R), so `shape = "quintiles"` cuts every
# continuous predictor and a named value overrides one. ⚠ ORDER: a shape recodes the column FIRST --
# it defines what the model's variable is -- and `ref`'s anchor then applies to the result, which is
# why the quadratic term below takes its square around 0 rather than measuring a centre of its own.
#
# WARNING -- i18n. `noun` and the `types` values (the instrument) are BARE MSGIDS and are never
# gettext()'d in the list: a top-level list evaluates ONCE at load, which would freeze the msgid at
# the build locale and make with_legend_lang()'s LANGUAGE switch a no-op. gettext() is applied by
# reg_check_label(), at render. Because those calls are DYNAMIC (gettext(ck$noun)), potools cannot
# see them -- hence the dead-code extraction anchor at the bottom of this file, the same device
# legend_measure_word() uses for the MEASURES words.
#
# WARNING: this file sorts BEFORE R/reg-estimand.R, so REG_CHECK_FAMILIES cannot be derived from
# REG_FIT_FAMILY; the exhaustiveness is asserted at load instead (R/zzz-fact-keys.R).
# See: CLAUDE.md section "tabxplor architecture" (the regression subsystem);
#      dev/regression_assumptions_plots.md (the panel designs and their measurements).


# === SECTION: the fact table ========================================================================

# Every family a check can be asked about. `grouped` (a summed-score binomial) is a flag on top of
# "binomial", never a family of its own, so it needs no entry.
#
# ⚠ must name every INTERNAL LINK KEY too (`rr` / `rd` / `mr`, REG_FIT_FAMILY in R/reg-estimand.R),
# not just the outcome families -- a link chosen to reach a MEASURE is still the same distribution
# underneath. (Load order: see the file header; exhaustiveness is asserted at the end of reg-estimand.R.)
#' @keywords internal
REG_CHECK_FAMILIES <- c("gaussian", "binomial", "poisson", "quasipoisson",
                        "multinomial", "ordinal", "rr", "rd", "mr")

# The DISTRIBUTION behind a fit key: `rd`/`rr` are binomial, `mr` is gaussian, everything else is
# itself. Every check that dispatches on family reads this, never the raw key, or a link key falls
# through every arm into the last `else` (e.g. `mr` landing on pbinom()).
#' @keywords internal
reg_check_family_of <- function(f) {
  d <- unname(REG_FIT_FAMILY[f])
  ifelse(is.na(d), f, d)
}

# ONE row per check.
#   noun          the assumption, as a word the reader already knows (a msgid)
#   types         discriminator -> INSTRUMENT (a msgid). The label is "<noun> (<instrument>)" (the
#                 crosstab summary's own convention, "pvalue (Chi2, Welch F)"). A term test carries
#                 three discriminators because exactly one of LR / F / Wald fires, and which one is a
#                 fact about the model the reader should see. EMPTY = the check is TAUGHT but never
#                 SCORED: it contributes a panel and no footer row.
#   kind/digits   the reg_footer_spec() rendering (a p-value cell, or a gof number with `digits`)
#   families      where the check is defined at all
#   weighted_ok   FALSE = refused on a weighted / design fit (never approximated)
#   per_predictor one row per (model column x predictor) rather than one per model column
#   cost          "free"  = arithmetic on the fit already in hand -- in the DEFAULT `stats` set.
#                 "refit" = it fits a model, so the user asks for it by name. `stats = "all"` turns
#                 every one of them on.
#   flag          a gof check only: the value past which the footer cell is MARKED as worth a look
#                 (the faintest under-shade, a warning -- never the p-value's deep red). ⚠ these are
#                 CONVENTIONS, not tests: no threshold on a VIF or a dfbeta has a null distribution
#                 behind it, so the mark says "look at this", never "this is significant".
#   panel         the reg_check_plots() panel this check draws (NA = no panel), and the `check =`
#                 vocabulary. ⚠ INDEPENDENT of `cost`: a panel is always free, which is why
#                 reg_check_plots() never filters on it.
#   footer_default TRUE = printed by the default `stats =`. DECLARED, not derived from `cost`: what a
#                 table must say and what it costs are two questions, and proportionality is the case
#                 where they disagree. `stats = "<key>"` reaches any check, default or not.
#   panel_default TRUE = drawn by `check = "auto"`. FALSE = reachable, but left out of the default
#                 grid because its footer row already says the whole thing; `check = "all"` restores it.
#   panel_marks   the reference line(s) that panel draws. DESIGN: a panel and a footer row are one
#                 check, so they must read one threshold -- `flag` where the check declares one.
#' @keywords internal
REG_CHECKS <- list(
  # 1. the ESTIMATE: is this predictor's effect really one straight line?
  linearity = list(
    noun = "Linearity",
    types = c(linearity_lr = "LR", linearity_f = "F", linearity_wald = "Wald"),
    kind = "pvalue", digits = NA_integer_,
    families = REG_CHECK_FAMILIES, weighted_ok = TRUE, per_predictor = TRUE,
    cost = "refit", footer_default = FALSE, panel = "linearity", panel_default = TRUE),
  # 2. what the estimate MEANS: is one odds ratio enough for every cut?
  proportionality = list(
    noun = "Proportionality",
    types = c(proportionality = "Brant"),
    kind = "pvalue", digits = NA_integer_,
    families = "ordinal", weighted_ok = FALSE, per_predictor = FALSE,
    cost = "refit", footer_default = TRUE, panel = "proportionality", panel_default = TRUE),
  # 3. the INTERVAL: are the standard errors wide enough?
  dispersion = list(
    noun = "Dispersion",
    types = c(dispersion = "robust/model SE"),
    kind = "gof", digits = 2L,
    families = REG_CHECK_FAMILIES, weighted_ok = TRUE, per_predictor = FALSE,
    # one number against one number: the footer row says it all, so the panel is opt-in.
    cost = "free", footer_default = TRUE, panel = "dispersion", panel_default = FALSE),
  # 4. is it REAL: does one respondent carry the result?
  influence = list(
    noun = "Influence",
    types = c(influence = "max dfbetas"),
    # |dfbetas| >= 1: ONE respondent moves a coefficient by a full standard error. Belsley, Kuh &
    # Welsch's small-sample rule -- their 2/sqrt(n) large-sample one is useless at survey n, where it
    # flags thousands of points.
    kind = "gof", digits = 2L, flag = 1,
    families = REG_CHECK_FAMILIES, weighted_ok = TRUE, per_predictor = FALSE,
    cost = "free", footer_default = TRUE, panel = "influence", panel_default = TRUE, panel_marks = 1),
  # 5. why is it WIDE: can the data tell these predictors apart?
  collinearity = list(
    noun = "Collinearity",
    types = c(collinearity = "max VIF"),
    # VIF >= 10: the textbook convention (Kutner et al.). ⚠ O'Brien (2007) argues explicitly against
    # any such cut-off -- hence a warning shade and a documented rule of thumb, not a verdict.
    kind = "gof", digits = 2L, flag = 10,
    families = setdiff(REG_CHECK_FAMILIES, "multinomial"), weighted_ok = TRUE,
    # a design property that biases nothing: the footer number is the decision, the bars are colour.
    per_predictor = FALSE, cost = "free", footer_default = TRUE, panel = "collinearity", panel_default = FALSE,
    panel_marks = c(5, 10)),
  # TAUGHT, NEVER SCORED. Both were measured not to discriminate as verdicts, but both are the
  # canonical lessons, so they keep their panel and give up their row -- an empty `types` IS that
  # statement.
  residuals = list(
    noun = "Residuals", types = character(0), kind = NA_character_, digits = NA_integer_,
    families = setdiff(REG_CHECK_FAMILIES, "multinomial"), weighted_ok = TRUE,
    per_predictor = FALSE, cost = "free", footer_default = FALSE, panel = "residuals", panel_default = TRUE),
  normality = list(
    noun = "Normality", types = character(0), kind = NA_character_, digits = NA_integer_,
    families = setdiff(REG_CHECK_FAMILIES, "multinomial"), weighted_ok = TRUE,
    per_predictor = FALSE, cost = "free", footer_default = FALSE, panel = "normality", panel_default = TRUE)
)

# Every discriminator the checks can emit (the `test` values that are check rows).
#' @keywords internal
reg_check_types <- function() unlist(lapply(REG_CHECKS, function(ck) names(ck$types)),
                                     use.names = FALSE)

# THE selection rule: which checks apply to this fit? Read by reg_footer_stats(), reg_check_rows()
# and reg_check_plots(). `has_fit` is FALSE on the jamovi digest path (no model frame kept) -- checks
# degrade to absent there rather than to a wrong number.
#' @keywords internal
reg_checks_for <- function(family, weighted = FALSE, has_fit = TRUE,
                           what = c("footer", "panel")) {
  what <- match.arg(what)
  if (!isTRUE(has_fit)) return(character(0))
  keys <- names(REG_CHECKS)
  keys[vapply(keys, function(k) {
    ck <- REG_CHECKS[[k]]
    ok <- family %in% ck$families && (isTRUE(ck$weighted_ok) || !isTRUE(weighted))
    ok && if (what == "footer") length(ck$types) > 0L else !is.na(ck$panel)
  }, logical(1))]
}

# The DEFAULT footer set, DECLARED (`footer_default`) rather than derived from `cost`: what a table
# must say is not the same question as what it costs. Proportionality is the case that proves it --
# a refit, and still a default, because a cumulative odds ratio that fails it is not one number but
# a fiction. A check named explicitly in `stats =` is still computed and shown -- default set vs
# vocabulary, not vocabulary vs nothing.
#' @keywords internal
reg_checks_default <- function(family, weighted = FALSE, has_fit = TRUE) {
  keys <- reg_checks_for(family, weighted, has_fit, what = "footer")
  keys[vapply(keys, function(k) isTRUE(REG_CHECKS[[k]]$footer_default), logical(1))]
}

# The DEFAULT panel set: the applicable panels their row opts into. `check = "all"` restores the
# others -- default grid vs vocabulary, exactly as reg_checks_default() is to `stats =`.
#' @keywords internal
reg_panels_default <- function(family, weighted = FALSE) {
  keys <- reg_checks_for(family, weighted, has_fit = TRUE, what = "panel")
  keys[vapply(keys, function(k) isTRUE(REG_CHECKS[[k]]$panel_default), logical(1))]
}

# The reference line(s) a panel draws, from the row that declares them (numeric(0) = none).
#' @keywords internal
reg_panel_marks <- function(key) as.numeric(REG_CHECKS[[key]]$panel_marks %||% numeric(0))

# The checks that cost a model fit, as a sentence -- read by ?tab_reg's generated `stats` prose, so
# the argument names them from the table rather than from a hand-kept list.
#' @keywords internal
reg_checks_costly <- function()
  names(REG_CHECKS)[vapply(REG_CHECKS, function(ck) identical(ck$cost, "refit"), logical(1))]

# The table's own consistency, at build time (the fmt_attr_rules / MEASURES idiom).
stopifnot(
  # every row declares a cost, and only the two legal values exist
  all(vapply(REG_CHECKS, function(ck) isTRUE(ck$cost %in% c("free", "refit")), logical(1))),
  # a taught-but-never-scored row has no footer row to opt into, so it can only be free
  all(vapply(REG_CHECKS, function(ck) length(ck$types) > 0L || identical(ck$cost, "free"),
             logical(1))),
  # every panel says whether it is drawn by default, and no row without a panel pretends to
  all(vapply(REG_CHECKS, function(ck) is.na(ck$panel) || is.logical(ck$panel_default), logical(1))),
  # a declared mark that also has a `flag` must BE that flag: one check, one threshold
  all(vapply(REG_CHECKS, function(ck)
    is.null(ck$panel_marks) || is.null(ck$flag) || ck$flag %in% ck$panel_marks, logical(1)))
)

# A `stats =` value the user writes is a check KEY ("linearity"); a `test` row carries a
# DISCRIMINATOR ("linearity_lr"). One expansion, so both vocabularies stay in this file.
#' @keywords internal
reg_check_expand <- function(stats) {
  out <- unlist(lapply(stats, function(s)
    if (!is.null(REG_CHECKS[[s]])) names(REG_CHECKS[[s]]$types) else s), use.names = FALSE)
  # a TAUGHT-BUT-NEVER-SCORED key expands to nothing, and so does an empty `stats` -- character(0),
  # never NULL: every caller uses the result as a vector (`%in%`, `[`).
  if (is.null(out)) character(0) else out
}

# The TEST_ROWS rows the checks contribute -- one per discriminator. GENERATED rather than declared
# literally: REG_CHECKS owns facts TEST_ROWS must not (`families`, `weighted_ok`, `panel`), so
# declaring them twice would make `types` a second encoding of the same ladder.
# ⚠ `digits` is emitted only for a "gof" check. See the `digits` note in R/tab-test-display.R.
#' @keywords internal
#' @noRd
test_rows_from_checks <- function(keys = names(REG_CHECKS)) {
  out <- list()
  for (ck in REG_CHECKS[keys]) for (d in names(ck$types)) {
    # ⚠ NA, never NULL: TEST_ROWS defaults its members through utils::modifyList(), which REMOVES an
    # entry whose value is NULL instead of setting it -- so a check whose instrument names a quantity
    # ("max VIF") rather than a term test would lose the member outright.
    row <- list(producer = "reg", kind = ck$kind, render = "grid",
                noun = ck$noun, instrument = ck$types[[d]],
                stat = REG_CHECK_KEY_OF[[d]] %||% NA_character_,
                method = REG_CHECK_METHOD[[ck$types[[d]]]] %||% NA_character_)
    if (identical(ck$kind, "gof")) {
      row$digits <- as.integer(ck$digits)
      if (!is.null(ck$flag)) row$flag <- as.numeric(ck$flag)
    }
    out[[d]] <- row
  }
  out
}

# discriminator -> the check KEY that requests it (the `stats =` word). The inverse of `types`.
#' @keywords internal
#' @noRd
REG_CHECK_KEY_OF <- local({
  k <- unlist(lapply(names(REG_CHECKS),
                     function(n) stats::setNames(rep(n, length(REG_CHECKS[[n]]$types)),
                                                 names(REG_CHECKS[[n]]$types))))
  as.list(k)
})

# instrument msgid -> the TEST_ROWS `method` key. Only the three real term-test instruments map; a
# check naming a quantity ("max VIF") has no method, and NULL becomes NA in TEST_ROWS' defaulting.
#' @keywords internal
#' @noRd
REG_CHECK_METHOD <- list(LR = "lr", F = "f", Wald = "wald")

# "<noun> (<instrument>)" -- the ONE label shape of a check row, shared with the `global` test's rows
# (which are not a check, but ask their question in the same words).
#' @keywords internal
reg_check_label <- function(noun, instrument) {
  if (is.null(instrument) || is.na(instrument) || !nzchar(instrument)) return(gettext(noun))
  gettextf("%s (%s)", gettext(noun), gettext(instrument))
}


# === SECTION: the five statistics ===================================================================

# The design object a sandwich must be computed against: a linearized survey design when the fit is
# one, NULL otherwise (then reg_if_se() falls back to the sum of squares). A replicate design needs
# withReplicates, which svyrecvar cannot do -- refuse rather than approximate.
#' @keywords internal
reg_check_design <- function(fit) {
  des <- if (inherits(fit, "svyglm")) fit$survey.design else NULL
  if (inherits(des, "svyrep.design")) return(NULL)
  des
}

# THE fit's coefficient covariance, resolved once (checks 3 and 4 both need it). Returns NULL rather
# than a substitute: `fit$var` is a SANDWICH, and handing it in as the bread would double-count the
# design (the trap reg_score_polr()'s own WARNING documents).
#' @keywords internal
reg_fit_vcov <- function(fit) tryCatch(stats::vcov(fit), error = function(e) NULL)

# The model-based standard errors, on the fit's NATIVE scale, in vcov order -- the denominator of
# both Dispersion and Influence. Taken from vcov(), not the printed `tidy`: vcov() already carries a
# quasi-likelihood's dispersion (so quasipoisson reads ~1) while plain poisson does not (~sqrt(phi)).
#' @keywords internal
reg_check_model_se <- function(fit, V = NULL) {
  if (is.null(V)) V <- reg_fit_vcov(fit)
  if (is.null(V)) {
    V <- tryCatch(fit$var, error = function(e) NULL)          # svy_vglm stores $var
    if (is.null(V)) return(NULL)
  }
  V <- as.matrix(V)
  if (!nrow(V) || nrow(V) != ncol(V)) return(NULL)
  se <- suppressWarnings(sqrt(diag(V)))
  if (!all(is.finite(se)) || any(se <= 0)) return(NULL)
  se
}

# Check 3 -- DISPERSION, as max_j |SE_robust,j / SE_model,j|: one number replacing four textbook
# checks, reading ~1 when the family's variance assumption holds and above 1 under over-dispersion,
# heteroscedasticity or clustering. It never touches df.residual, so it works on a clustered design
# where the Pearson phi does not (df.residual of an svyglm is the DESIGN df).
#' @keywords internal
reg_check_dispersion <- function(fit, V = NULL)
  reg_check_influence_pass(fit, "dispersion", V)[["dispersion"]]

# Check 4 -- INFLUENCE, as max_j max_i |dfbetas_ij|: no single respondent moves any coefficient by
# more than X of its own SE. dfbetas rather than Cook's distance (unreadable at survey n). The
# one-step dfbeta IS the influence function the package already computes, so it exists for
# polr / multinom (unlike base R) and is design-aware.
#' @keywords internal
reg_check_influence <- function(fit, V = NULL)
  reg_check_influence_pass(fit, "influence", V)[["influence"]]

# THE pass both checks 3 and 4 are: ONE decomposition read two ways -- the same vcov, the same
# influence closure, the same p unit contrasts. Dispersion keeps `reg_if_se(d)`, influence keeps
# `max|d|`; `want` lets a table that asked for just one of them pay for just one.
#
# WARNING: never materialise the n x p matrix (the memory contract R/reg-influence.R states). The
# loop keeps two running maxima and discards each length-n vector.
#' @keywords internal
reg_check_influence_pass <- function(fit, want = c("dispersion", "influence"), V = NULL) {
  none <- c(dispersion = NA_real_, influence = NA_real_)
  if (is.null(V)) V <- reg_fit_vcov(fit)
  se_mod <- reg_check_model_se(fit, V)
  if (is.null(se_mod)) return(none)
  cif <- reg_coef_if_maker(fit, V)
  if (is.null(cif)) return(none)
  do_d <- "dispersion" %in% want
  do_i <- "influence"  %in% want
  des  <- if (do_d) reg_check_design(fit) else NULL
  p    <- length(se_mod)
  disp <- NA_real_
  infl <- NA_real_
  for (j in seq_len(p)) {
    e <- rep(0, p); e[[j]] <- 1
    d <- cif(e)
    if (is.null(d)) return(none)                              # the closure's p disagrees with vcov's
    if (do_d) {
      s <- reg_if_se(d, des)
      if (is.finite(s)) {
        r <- s / se_mod[[j]]
        if (is.na(disp) || r > disp) disp <- r
      }
    }
    if (do_i) {
      m <- suppressWarnings(max(abs(as.numeric(d)), na.rm = TRUE)) / se_mod[[j]]
      if (is.finite(m) && (is.na(infl) || m > infl)) infl <- m
    }
  }
  c(dispersion = disp, influence = infl)
}

# Check 5 -- COLLINEARITY, as the largest variance inflation factor.
#
# car::vif() returns a bare VIF per term when every term is 1-df, and a (GVIF, Df, GVIF^(1/(2Df)))
# matrix otherwise -- different scales, so the matrix form is squared back onto the familiar VIF scale
# (what performance::check_collinearity() reports), and the usual 5 / 10 readings apply either way.
#
# `car` is Suggests-only: absent -> no row, never a hand-rolled substitute.
#' @keywords internal
reg_check_collinearity <- function(fit) {
  if (!requireNamespace("car", quietly = TRUE)) return(NA_real_)
  # ⚠ suppressMessages too: on an interacted fit car::vif() prints a note about higher-order terms,
  # which describes a model the user deliberately asked for and says nothing they can act on.
  v <- tryCatch(suppressMessages(suppressWarnings(car::vif(fit))), error = function(e) NULL)
  if (is.null(v) || !length(v)) return(NA_real_)
  val <- if (is.matrix(v)) {
    if (ncol(v) >= 3L) v[, 3]^2 else v[, 1]
  } else as.numeric(v)
  val <- val[is.finite(val)]
  if (!length(val)) return(NA_real_)
  max(val)
}

# Check 1 -- LINEARITY, per numeric predictor: the model plus this predictor's CENTRED SQUARED term
# (car::residualPlots()'s curvature test). NOT the cheaper no-refit Rao score test, whose p can
# disagree with the design-based Wald by orders of magnitude.
#
# It IS what drop1() returns on both arms: the LR arm doubles the log-likelihood difference; the F
# arm reads the two deviances against the AUGMENTED fit's `deviance / df.residual` -- drop1.glm's
# default `scale = 0`. ⚠ NOT the Pearson dispersion `summary()` reports, nor what
# `anova(base, aug, test = "F")` uses.
#
# `nnet:::drop1.multinom` has no `test` argument, so the multinomial arm needs its own route (below).
# `use_f` is the caller's family fact, never re-derived here; returns NULL for a non-nested pair, two
# fits on different rows, or an unusable logLik / deviance.
#' @keywords internal
reg_nested_test <- function(base, aug, use_f = FALSE) {
  num <- function(expr) tryCatch(as.numeric(expr), error = function(e) NA_real_)
  # two fits on different N are not nested; the augmented term is a function of a predictor already
  # in the model, so the complete-case set cannot change. An engine with no nobs() method stays eligible.
  n0 <- num(stats::nobs(base)); n1 <- num(stats::nobs(aug))
  if (!is.na(n0) && !is.na(n1) && !isTRUE(all.equal(n0, n1))) return(NULL)

  if (use_f) {
    r0 <- num(stats::df.residual(base)); r1 <- num(stats::df.residual(aug))
    d0 <- num(stats::deviance(base));    d1 <- num(stats::deviance(aug))
    if (anyNA(c(r0, r1, d0, d1))) return(NULL)
    k <- r0 - r1
    disp <- d1 / r1
    if (!is.finite(k) || k <= 0 || !is.finite(disp) || disp <= 0) return(NULL)
    s <- ((d0 - d1) / k) / disp
    if (!is.finite(s) || s < 0) return(NULL)
    return(list(stat = s, df = k, df2 = r1, p = stats::pf(s, k, r1, lower.tail = FALSE)))
  }

  ll <- function(f) num(stats::logLik(f))
  df <- function(f) tryCatch({
    e <- f$edf
    if (is.null(e)) e <- attr(stats::logLik(f), "df")
    as.numeric(e)
  }, error = function(e) NA_real_)
  l0 <- ll(base); l1 <- ll(aug); d0 <- df(base); d1 <- df(aug)
  if (anyNA(c(l0, l1, d0, d1))) return(NULL)
  k <- d1 - d0
  s <- 2 * (l1 - l0)
  if (!is.finite(k) || k <= 0 || !is.finite(s) || s < 0) return(NULL)
  list(stat = s, df = k, df2 = NA_real_, p = stats::pchisq(s, k, lower.tail = FALSE))
}

#' @keywords internal
reg_check_linearity_rows <- function(data, sp, shared, fit_first_col_i, base_fit = NULL) {
  # A predictor the user has already CURED gets no row: `shape = "quadratic"` puts this very term in
  # the model, so adding it again is a collinear duplicate. (`log`/`sqrt` recode the column, so the
  # check asks the right new question; a quantile-cut predictor is a factor, with no form to mis-specify.)
  num <- setdiff(reg_numeric_preds(data, sp$predictors), names(shared$shape_terms))
  if (length(num) == 0L) return(NULL)
  wtc <- shared$design_spec$wt
  wv  <- if (!is.null(wtc) && is.character(wtc) && length(wtc) == 1L && wtc %in% names(data))
           data[[wtc]] else NULL
  weighted <- isTRUE(shared$weighted)
  use_f    <- reg_fam_disp_estimated(sp$fit_family)
  use_wald <- reg_fam_svy_fitted(sp$fit_family, weighted)
  # the check's own three discriminators, read off TEST_ROWS instead of respelled here.
  types    <- test_row_types("linearity")

  purrr::flatten(purrr::map(num, function(v) {
    tm <- reg_shape_term(data[[v]], v, "quadratic", w = wv)
    if (is.null(tm)) return(NULL)
    # a diagnostic refit must be SILENT: reg_fit() would otherwise repeat, once per numeric predictor,
    # every message the real fit already gave. Its only output is a p-value.
    f2 <- tryCatch(suppressWarnings(suppressMessages(
            reg_fit(data, sp$outcome, sp$predictors, sp$fit_family, shared$design_spec, isTRUE(sp$est$exp),
                    reg_outcome_level_of(sp$outcome_level) %||% shared$outcome_level,
                    shared$conf_level, "wald", trials = sp$trials, formula = NULL,
                    multiplier = NULL, add_terms = tm))),
                   error = function(e) NULL)
    if (is.null(f2) || is.null(f2$fit)) return(NULL)

    # THE FAST ROUTE: both fits are in hand and nested by construction, so their comparison IS drop1's
    # answer, bit for bit, with no second fit. A design fit is excluded on principle, not cost -- a
    # design-based Wald is not a likelihood ratio.
    if (!use_wald && !is.null(base_fit)) {
      nt <- reg_nested_test(base_fit, f2$fit, use_f)
      if (!is.null(nt))
        return(list(reg_test_row(types[[if (use_f) "f" else "lr"]], fit_first_col_i, v,
                                 nt$stat, nt$df, nt$df2, nt$p, f2$nobs, outcome = sp$outcome)))
    }

    # the slow route: drop1() refits through update(), which re-evaluates the fit's stored `data`
    # SYMBOL -- a local of reg_fit() long gone by now; reg_selfheal_call() restores it.
    fit2 <- reg_selfheal_call(f2$fit, f2$data)
    # WARNING: the scope must be the FIT's own term label, verbatim -- terms() may re-spell what we
    # pasted, and drop1() then rejects the scope.
    have <- tryCatch(attr(stats::terms(fit2), "term.labels"), error = function(e) character(0))
    lab  <- have[length(have)]
    if (!length(lab) || is.na(lab)) return(NULL)
    purrr::compact(reg_term_tests(fit2, v, lab, use_f, use_wald, types = types,
                                  col_var = fit_first_col_i, nobs = f2$nobs,
                                  outcome = sp$outcome))
  }))
}

# THE producer: the check rows of ONE fit. A sibling of reg_gof_rows() / reg_global_rows(), and the
# only one of the three that needs `data` -- the Linearity check refits with an added term.
#' @keywords internal
reg_check_rows <- function(data, f, sp, shared, stats, col_var, grouped) {
  weighted <- isTRUE(shared$weighted)
  gof <- function(test, col_var, value, nobs, outcome = NA_character_)
    if (is.null(value) || is.na(value)) NULL
    else reg_test_row(test, col_var, "", value, NA_real_, NA_real_, NA_real_, nobs, outcome = outcome)

  grouped <- isTRUE(grouped)
  rows <- (function() {
    if (is.null(f)) return(NULL)
    keep <- reg_footer_stats(sp$fit_family, weighted, grouped, stats)
    keys <- reg_checks_for(sp$fit_family, weighted, has_fit = !is.null(f$fit))
    keys <- keys[vapply(keys, function(k) any(names(REG_CHECKS[[k]]$types) %in% keep), logical(1))]
    if (length(keys) == 0L) return(NULL)
    cv  <- col_var
    fit <- f$fit
    out <- list()
    if ("linearity" %in% keys && !isTRUE(sp$compound))
      out <- c(out, reg_check_linearity_rows(data, sp, shared, cv, base_fit = fit))
    # the Brant test runs HERE, where its row is built, and nowhere else -- one producer, one
    # consumer, one warning.
    if ("proportionality" %in% keys) {
      bp <- reg_ordinal_diagnostic(
        fit, asked = is.character(stats) && any(c("proportionality", "all") %in% stats))
      if (!is.null(bp) && !is.na(bp))
        out <- c(out, list(reg_test_row("proportionality", cv, "", NA_real_, NA_real_,
                                       NA_real_, bp, f$nobs, outcome = sp$outcome)))
    }
    # ONE vcov and ONE influence sweep for both checks, computed only if either is wanted.
    if (any(c("dispersion", "influence") %in% keys)) {
      di <- reg_check_influence_pass(fit, intersect(c("dispersion", "influence"), keys))
      if ("dispersion" %in% keys) out <- c(out, list(gof("dispersion", cv, di[["dispersion"]], f$nobs, sp$outcome)))
      if ("influence"  %in% keys) out <- c(out, list(gof("influence",  cv, di[["influence"]],  f$nobs, sp$outcome)))
    }
    if ("collinearity" %in% keys) out <- c(out, list(gof("collinearity", cv, reg_check_collinearity(fit), f$nobs, sp$outcome)))
    out
  })()
  rows <- purrr::compact(rows)
  if (length(rows) == 0) return(NULL)
  dplyr::bind_rows(rows)
}


# === SECTION: the quadratic TERM -- the one `shape` a fit adds rather than a recode ================
#
# The design rule is stated in the file header (THE CURE IS PART OF THE CHECK). Before this feature,
# `predictors = c("race", "poly(age, 2)")` errored, and the formula escape hatch silently disabled
# `empirical =`, `color = "adjustment"`, `multiplier` and the per-predictor tests.
# The recoding shapes (quantile groups, sd bands, log, sqrt) are in R/var-shape.R: they change the
# COLUMN, which is a fact about the data and belongs to both producers. Only a curvature is a fact
# about the MODEL, so only it is here.
#
# WARNING -- poly() / ns() / bs() are NEVER emitted, and that is a wrong-number refusal, not taste:
# `marginaleffects` returns AME = 0.000000 for them, silently, through every contrast form. I(x^2),
# raw polynomials and log() are correct through every route.

# The extra model TERM a numeric predictor's non-linear SHAPE emits, its scale frozen as a LITERAL
# in the formula string -- frozen for the same reason the multiplier's SD is: `scale()` inside a
# formula re-scales on new data. Returns NULL when the column cannot supply a finite scale.
#
# ONE builder, two consumers: the Linearity check refits with this term, and `shape = "quadratic"`
# emits the same one, so the check and its cure are the same object.
#
# ⚠ THE COLUMN IS ALREADY CENTRED WHEN THIS RUNS: `ref` anchors every continuous predictor at the
# argument boundary (R/reg-resolve.R, block Y), so the square is taken around the DECLARED anchor
# and needs no centre of its own. A second, self-measured centre would put the curve at the mean
# while the linear row sits at the anchor -- two answers to one question. Centring is not cosmetic:
# uncentred, the pair's own VIF is 38.7 against 1.2 centred.
#
# WHY THE LINEAR TERM STAYS RAW: eta = a*x + b*(x/s)^2 and eta = A*z + B*z^2 are the same model with
# A = a*s, B = b -- so with the default `multiplier = "sd"` the printed linear row ALREADY is the
# per-SD slope of the centred parametrisation.
#' @keywords internal
reg_shape_term <- function(x, var, shape = "quadratic", w = NULL, digits = 8L) {
  if (!identical(shape, "quadratic")) return(NULL)
  s <- wtd_sd(x, w)
  if (!is.finite(s) || s <= 0) return(NULL)
  num <- function(v) format(signif(v, digits), scientific = FALSE)
  # WARNING: return the DEPARSED form, not the pasted one. A model-matrix column is named by the
  # formula's own term label, which R produces by deparsing -- and deparse drops the spaces around `/`
  # that a hand-pasted string keeps. Without this the skeleton's `term` misses the fit's by two
  # characters and the curvature row renders EMPTY (measured).
  s2l <- tryCatch(str2lang(paste0("I((`", var, "` / ", num(s), ")^2)")),
                  error = function(e) NULL)
  if (is.null(s2l)) return(NULL)
  paste(deparse(s2l, width.cutoff = 500L), collapse = "")
}

# The quadratic terms a `shape` asks for, named by variable so the skeleton can key its extra row on
# the same string the formula carries; the centre and scale are weighted whenever the call is.
#' @keywords internal
reg_shape_terms <- function(data, shapes, w = NULL) {
  q <- names(shapes)[vapply(shapes, function(s) identical(s$kind, "quadratic"), logical(1))]
  if (length(q) == 0L) return(stats::setNames(character(0), character(0)))
  wv <- if (!is.null(w) && is.character(w) && length(w) == 1L && w %in% names(data)) data[[w]] else NULL
  tm <- vapply(q, function(v) {
    t <- reg_shape_term(data[[v]], v, "quadratic", wv)
    if (is.null(t)) NA_character_ else t
  }, character(1))
  tm[!is.na(tm)]
}

# The display label of a numeric predictor's squared row: "age²" -- also the skeleton `level`, which
# must differ from the variable name (`level == var` is what marks the plain linear row).
#' @keywords internal
reg_shape_sq_level <- function(var) paste0(var, "\u00b2")     # U+00B2 SUPERSCRIPT TWO

# The `add_terms` one model contributes: its own predictors' quadratic terms. A model COMPARISON is
# why this filter exists -- a term for a variable the model does not carry would abort the fit.
#' @keywords internal
reg_shape_add <- function(shape_terms, predictors) {
  if (is.null(shape_terms) || length(shape_terms) == 0L) return(NULL)
  keep <- intersect(predictors, names(shape_terms))
  if (length(keep) == 0L) return(NULL)
  unname(shape_terms[keep])
}

# A model TERM as a reader should see it. A quadratic term is a scaled square carrying a frozen
# literal (`I((\`age\`/12.34)^2)`); nothing but noise reaches the eye from that, so the stored
# `shape_terms` is inverted back to the display level the table already uses ("age²"). Everything
# else keeps its own name, minus the backticks the formula needed.
# WARNING: invert the STORED vector, never parse the string -- the literal scale is data-dependent.
#' @keywords internal
reg_term_label <- function(term, shape_terms = NULL) {
  if (!length(term)) return(character(0))
  out <- gsub("`", "", as.character(term), fixed = TRUE)
  if (length(shape_terms)) {
    hit <- match(as.character(term), unname(shape_terms))
    sq  <- !is.na(hit)
    out[sq] <- reg_shape_sq_level(names(shape_terms)[hit[sq]])
  }
  out
}


# === SECTION: the plot primitives ===================================================================
#
# Five base-R functions, no dependency. They are the ONLY producers of the numbers every panel and
# the row sparkline draw.
#
# WARNING for whoever adds a panel later: never `geom_smooth()`. Its `method = "auto"` switches
# loess -> gam at 1000 observations in the largest GROUP, so a facetted 50 000-row plot gets loess and
# an unfacetted 1200-row one gets gam -- and its message is assembled dynamically, so it cannot be
# regex-suppressed. Nothing here smooths: the comparator of a linearity panel is the shape the MODEL
# fits (rd_comparator), which is the whole point of the panel.

# The MATH a link scale is, as plotmath -- the second half of every linearity y axis, so the word and
# the formula come from one place. A character element renders verbatim in plotmath, which is why the
# outcome's name is interpolated as a string and never as a name (a name with a space would break).
#
# ⚠⚠ WARNING -- THE PLOTMATH GLYPHS THAT MUST NEVER BE USED HERE. R draws a math-mode SPACE (`~`),
# PARENTHESES from a function call or `group()`, and the operators `=` (`==`), `<`, `>` from the Adobe
# SYMBOL font. Only cairo resolves that font reliably: on `ragg` -- which is what Positron and RStudio
# draw with -- each of them comes out as a MISSING-GLYPH BOX, and the whole formula reads as a row of
# empty rectangles (measured: `~`, `~~`, `P(Y)`, `Y == k`, `Y > k`, `group()` all tofu; the same
# expression is perfect on `png(type = "cairo")`). So every one of them is written as a plain string
# instead -- `":  "` for the space, `"log("` / `")"` for the parentheses, `"p = P(x)"` for the
# relation. What IS safe is anything plotmath draws with a RULE, or as ordinary text at another size:
# `frac()`, `bar()` and a SUBSCRIPT (`"%"[level]`) render identically on both devices, which is why
# the fraction and the qualified percentage survive. test-tab_reg-plots.R locks the safe list.
#' @keywords internal
rd_link_expr <- function(kind, lab, outcome = NULL, level = NULL) {
  # THE UNITS THE TABLE PRINTS, not the statistician's: `p` and `P(y)` name nothing a reader of this
  # package has met, and a literary student cannot act on them. A percentage is written the way the
  # table writes one -- a `%` sign qualified by WHAT it is a percentage of, in subscript -- and the
  # qualifier is the MODELLED LEVEL where there is one ("% of Married", not of the variable), because
  # that is what the curve actually plots. Long level labels cost little: a subscript is small.
  nm  <- rd_label_of(level %||% outcome %||% "y")
  pct <- bquote("%"[.(nm)])
  switch(kind,
         # a number needs no gloss -- the formula IS the word, so it takes no prefix
         mean    = gettextf("mean of %s", rd_label_of(outcome %||% "y")),
         logmean = gettextf("log(mean of %s)", rd_label_of(outcome %||% "y")),
         # a percentage does: the technical name, then what it is in the table's own units
         logit   = bquote(.(lab) * ":  log(" * frac(.(pct), "1 - " * .(pct)) * ")"),
         risk    = bquote(.(lab) * ":  " * .(pct)),
         logrisk = bquote(.(lab) * ":  log(" * .(pct) * ")"),
         lab)
}

# A level or a variable as a LABEL: always through the package's one cleaner, so an axis reads
# "$25000 or more" where the data says "4-$25000 or more", exactly as every table header does.
#' @keywords internal
rd_label_of <- function(x) reg_cleanup(as.character(x)[[1L]], TRUE)

# The per-observation outcome a check reads, on the family's own LINK scale, plus that scale's label
# and its formula. An ordinal / multinomial outcome has no single curve, so it is read as "beyond the
# first category" -- stated in the axis label, never implied. (rd_link_cuts() is the plots' richer
# read of the same fact; this one is what the row sparkline needs, and one curve is all it can draw.)
#' @keywords internal
rd_link_y <- function(y, family, trials = NULL, positive_level = NULL, outcome = NULL) {
  fit    <- family                               # the LINK key itself, before it reads as a family
  family <- reg_check_family_of(family)          # a LINK key (rd/rr/mr) reads as its distribution
  out <- function(y, link, kind, lab)
    list(y = y, link = link, kind = kind, lab = lab,
         expr = rd_link_expr(kind, lab, outcome, positive_level))
  # WARNING: the curve belongs on the scale the MODEL fits, which is the LINK and not the
  # distribution -- an empirical logit beside a modified-Poisson fit would answer a question that
  # model never asked. `link = ` is a measure, and this is where the plots read it.
  risk <- function() {
    if (!is.null(trials))         return(as.numeric(y) / trials)
    if (!is.null(positive_level)) return(as.numeric(as.character(y) == positive_level))
    as.numeric(as.factor(y)) - 1
  }
  if (fit == "rr") return(out(risk(), "logrisk",  "logrisk", gettext("log(risk)")))
  if (fit == "rd") return(out(risk(), "identity", "risk",    gettext("risk")))
  if (fit == "mr") return(out(as.numeric(y), "log", "logmean", gettext("log(mean)")))
  if (family == "gaussian")
    return(out(as.numeric(y), "identity", "mean", gettext("mean")))
  if (reg_fam_count(family))
    return(out(as.numeric(y), "log", "logmean", gettext("log(mean)")))
  if (reg_fam_binary(family))
    return(out(risk(), "logit", "logit", gettext("empirical logit")))
  # ordinal / multinomial: the one cut every K-category outcome has.
  out(as.numeric(as.numeric(as.factor(y)) > 1), "logit", "logit",
      gettext("empirical logit (beyond the first category)"))
}

# THE curves a linearity panel draws, one per reading of the outcome. Everything but an ordered or an
# unordered factor has exactly one, and it is rd_link_y()'s -- so the ordinary panel is unchanged.
#   ordinal      one per CUT, y = 1{Y > k}: the observed cumulative logit. Non-parallel curves are a
#                proportional-odds departure for a NUMERIC predictor, which the Brant test scores but
#                the factor-only Proportionality panel cannot show.
#   multinomial  one per non-reference category, on the rows in {ref, k} only: log(p/(1-p)) there IS
#                the empirical generalised logit the model estimates.
# `keep` is the rows a curve is measured on, so the caller subsets x, the weights and the design rows
# with one index and rd_bin() stays untouched.
#' @keywords internal
rd_link_cuts <- function(y, family, trials = NULL, positive_level = NULL, outcome = NULL) {
  fam <- reg_check_family_of(family)
  n   <- length(y)
  if (!fam %in% c("ordinal", "multinomial")) {
    ly <- rd_link_y(y, family, trials, positive_level, outcome)
    return(list(link = ly$link, lab = ly$lab, expr = ly$expr,
                curves = list(list(keep = seq_len(n), y = ly$y, cut = NA_character_))))
  }
  f  <- as.factor(y)
  lv <- levels(f)
  i  <- as.integer(f)
  if (length(lv) < 2L) return(NULL)
  if (fam == "ordinal") {
    lab <- gettext("empirical cumulative logit")
    hi <- gettext("above the cut"); lo <- gettext("up to the cut")
    return(list(link = "logit", lab = lab,
                expr = bquote(.(lab) * ":  log(" * frac("%"[.(hi)], "%"[.(lo)]) * ")"),
                curves = lapply(seq_len(length(lv) - 1L), function(k)
                  list(keep = seq_len(n), y = as.numeric(i > k),
                       cut = gettextf("> %s", lv[[k]])))))
  }
  ref <- lv[[1L]]
  # the fraction names the reference, so the WORD must not: "empirical logit vs 1-Democrat:
  # log(% of the category / % of 1-Democrat)" said it twice on one axis.
  lab <- gettext("empirical logit")
  num <- gettext("this category"); den <- rd_label_of(ref)
  list(link = "logit", lab = lab,
       expr = bquote(.(lab) * ":  log(" * frac("%"[.(num)], "%"[.(den)]) * ")"),
       curves = lapply(seq_along(lv)[-1L], function(k) {
         keep <- which(i %in% c(1L, k))
         list(keep = keep, y = as.numeric(i[keep] == k), cut = lv[[k]])
       }))
}

# The comparator of a linearity panel: the shape the MODEL fits, drawn through the observed bins.
# Not a smoother -- the assumption IS the shape, so a smoother would trace the very departure the
# panel exists to show. A predictor cured by `shape = "quadratic"` is fitted as a parabola, because
# that is what the model now assumes; every other predictor is fitted as the straight line it assumes.
#' @keywords internal
rd_comparator <- function(x, y, quadratic = FALSE) {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < (if (quadratic) 3L else 2L)) return(rep(NA_real_, length(x)))
  d   <- data.frame(x = x[ok], y = y[ok])
  fml <- if (quadratic) y ~ x + I(x^2) else y ~ x
  fit <- tryCatch(stats::lm(fml, data = d), error = function(e) NULL)
  if (is.null(fit)) return(rep(NA_real_, length(x)))
  out <- rep(NA_real_, length(x))
  out[ok] <- as.numeric(stats::predict(fit, newdata = d))
  out
}

# Wrap a panel subtitle so a longer translation still fits above the plot. `strwrap` is base R, and
# the width is what a 3-across grid can hold at size 8.5.
#' @keywords internal
rd_wrap <- function(txt, width = 68L) paste(strwrap(txt, width = width), collapse = "\n")

# Weighted quantile bins of y against x, on the link scale: the OBSERVED shape, with no fit in it.
# The band is the theoretical one, 2*sqrt(p(1-p)/n) as ROS SS14.5 p.253 specifies -- not
# `arm::binnedplot`'s empirical 2*sd(y)/sqrt(n), which ignores weights. Zero cells use
# Haldane-Anscombe (k + 0.5)/(n + 1) -- symmetric, never infinite, no arbitrary floor.
#
# The bin's EFFECTIVE base (`ne = num / Var(mean of y in the bin)`) uses the package's one device,
# not a hand-rolled Kish: a survey DESIGN reaches svyrecvar, weights alone use the exact flat closed
# form (svy_flat_neff_rows), unweighted uses `sw`. A design whose variance cannot be computed for a
# bin falls through to the flat form, never to a wrong number.
#' @keywords internal
rd_bin_neff <- function(sw, num, w, y, g, design = NULL, des_rows = NULL) {
  nb   <- length(sw)
  nobs <- length(y)
  flat <- function() vapply(seq_len(nb), function(i) {
    k  <- g == i
    ne <- svy_flat_neff_rows(w[k], y[k], rep(1, sum(k)), nobs, num = num[[i]])
    if (isTRUE(is.finite(ne) && ne > 0)) ne else sw[[i]]
  }, double(1))
  if (!is.null(design) && !is.null(des_rows) && length(des_rows) == nobs) {
    V <- tryCatch(svy_var_mean(prep  = svy_var_prep(design, des_rows),
                               keys  = list(as.character(seq_len(nb))), n_tab = 0L,
                               mkeys = list(as.character(g)),
                               xs    = list(y = as.numeric(y)))$v,
                  error = function(e) NULL)
    if (!is.null(V) && nrow(V) == nb) {
      ne  <- num / V[, 1L]
      bad <- !is.finite(ne) | ne <= 0
      if (any(bad)) { fb <- flat(); ne[bad] <- fb[bad] }
      return(ne)
    }
  }
  flat()
}

# RD_LINK_SCALES -- per reading of the outcome (rd_link_y()'s `kind`, the same five the axis label is
# built from): the FIRST COLOUR RUNG expressed on that reading's own scale, and how a span on it is
# read back. The conversion is exact rather than a convention: every ladder in COLOR_SCALES is the
# same ladder written at a 50 % reference (their declared `anchor`s), so x1.2 on the odds scale, x1.1
# on a ratio and 5 points on a probability ARE one rung -- and 0.1 SD is that same rung on a mean,
# which is why a probability needs no row of its own on the identity scale (SD = 0.5 at p = 0.5,
# so 0.1 SD = 5 points).
# `rung = NA` means "0.1 x the outcome's own SD", the one rung that cannot be a constant.
#' @keywords internal
RD_LINK_SCALES <- list(
  logit   = list(rung = log(1.2), span = "mult"),
  logmean = list(rung = log(1.1), span = "mult"),
  logrisk = list(rung = log(1.1), span = "mult"),
  risk    = list(rung = NA_real_, span = "points"),
  mean    = list(rung = NA_real_, span = "sd")
)

#' @keywords internal
rd_link_rung <- function(kind, y, w) {
  r <- (RD_LINK_SCALES[[kind %||% "mean"]] %||% RD_LINK_SCALES$mean)$rung
  if (is.na(r)) 0.1 * wtd_sd(y, w) else r
}

#' @keywords internal
rd_bin <- function(x, y, w = NULL, nbins = 10L, link = "identity",
                   design = NULL, des_rows = NULL, kind = NULL) {
  x <- as.numeric(x); y <- as.numeric(y)
  wtd <- !is.null(w)
  w <- if (is.null(w)) rep(1, length(x)) else as.numeric(w)
  ok <- is.finite(x) & is.finite(y) & is.finite(w) & w > 0
  if (sum(ok) < 2L) return(NULL)
  x <- x[ok]; y <- y[ok]; w <- w[ok]
  if (!is.null(des_rows)) des_rows <- des_rows[ok]
  br <- unique(shape_wquantile(x, seq(0, 1, length.out = nbins + 1L), w))
  br[[1L]] <- min(x) - 1e-9; br[[length(br)]] <- max(x) + 1e-9
  if (length(br) < 3L) return(NULL)
  g  <- findInterval(x, br, rightmost.closed = TRUE)
  g  <- pmax(pmin(g, length(br) - 1L), 1L)
  sw <- as.numeric(rowsum(w, g))
  mx <- as.numeric(rowsum(w * x, g)) / sw
  my <- as.numeric(rowsum(w * y, g)) / sw
  vy <- as.numeric(rowsum(w * (y - my[g])^2, g)) / sw   # the bin's own (weighted) variance
  # the EFFECTIVE base of each bin, so a weighted band is not a sample-size fiction. `num` = the
  # numerator of Korn-Graubard's device for THIS link (see rd_bin_neff).
  num <- switch(link, "logit" = my * (1 - my), "log" = pmax(my, 0),
                "logrisk" = pmax(my * (1 - my), 0), vy)
  ne  <- if (wtd || !is.null(design)) rd_bin_neff(sw, num, w, y, g, design, des_rows) else sw
  out <- switch(
    link,
    "logit" = {
      p <- (my * ne + 0.5) / (ne + 1)
      list(y = log(p / (1 - p)), se = sqrt(1 / (ne * p * (1 - p))))
    },
    "log" = {
      m <- pmax(my, 0.5 / ne)
      list(y = log(m), se = sqrt(1 / (ne * m)))
    },
    # a log-link RISK is not a log-link count: Var(log p-hat) = (1 - p) / (n p), which the Poisson
    # form above overstates by 1 / (1 - p).
    "logrisk" = {
      p <- (my * ne + 0.5) / (ne + 1)
      list(y = log(p), se = sqrt((1 - p) / (ne * p)))
    },
    list(y = my, se = sqrt(vy / ne))
  )
  # the central 95 % of x, for the sparkline's own axis: it is drawn to SCALE, so one far outlier
  # would otherwise squeeze the whole curve into the first cell. Computed here because this is the
  # only place x and its weights are both in scope; the panel plot ignores it (it draws real points).
  # ⚠ COLUMNS, not an attribute: the curve is mutate()d, bound per tab_vars group and sliced again
  # before it is drawn, and only a column survives all three.
  q95 <- tryCatch(shape_wquantile(x, c(0.025, 0.975), w), error = function(e) range(x))
  # ⚠ BEFORE the tibble: tibble() masks sequentially, so a `y` inside it would be the binned COLUMN
  # (one value per bin) rather than the outcome this rung is 0.1 SD of.
  rung <- rd_link_rung(kind, y, w)
  tibble::tibble(x = mx, y = out$y, n = sw, se = out$se,
                 xlo = min(q95), xhi = max(q95), rung = rung)
}

# THE SPARKLINE: the binned curve DRAWN TO SCALE on the predictor's own axis, then flattened to 8
# block levels. There is no plain-text ladder: eight ASCII ranks (". , - ~ + = * #") do not read as a
# CURVE at all, and a reader who cannot see the shape is better served by no sparkline.
#
# ⚠ THE VERTICAL WINDOW HAS A FLOOR, and that is what makes a flat curve LOOK flat. A pure min-max
# rescale always spends all eight levels, so noise on a near-constant outcome came out as a dramatic
# shape -- the standing objection to sparklines (Few, "Best Practices for Scaling Sparklines"). The
# window is the curve's own range, but never narrower than:
#   * 8 x the median bin SE. The range of k independent bins is ~3.1 SE for k = 10 whatever the data
#     says, so a window of 8 SE leaves pure noise under half the height -- visibly a line. Measured,
#     not chosen: at 4 SE a noise curve still spent ~78 % of the run. Self-calibrating, no ladder.
#   * the first COLOUR RUNG on this reading's scale (rd_link_rung) -- so "uses the full height" means
#     "reaches a deviation this package would colour", the same threshold everywhere in the table.
# Centred on the curve's midrange, so a sub-floor curve sits in the middle glyphs and reads as a line.
# It still answers "is it a line?" first; the floor is what stops it answering it wrongly.
#
# WHY IT IS RESAMPLED. The bins are equal-COUNT (robust: every point rests on the same amount of
# data), so drawing one glyph per bin plots the curve against RANK, not against x -- and a monotone
# `shape` (log, sqrt) leaves rank order untouched, which is why the same curve came out for every
# transform. Interpolating onto a grid equally spaced in the SHAPED variable's own units is what
# every standard linearity diagnostic does (component+residual, empirical-logit plots), what the
# assumption panel already does, and what lets a reader SEE whether the transform straightened it.
# It also fixes the run's length, so every sparkline is the same width.
#' @keywords internal
rd_spark_glyphs <- function() {
  # U+2581..U+2588 (lower one-eighth block .. full block), as escapes: the source stays ASCII.
  c("\u2581", "\u2582", "\u2583", "\u2584", "\u2585", "\u2586", "\u2587", "\u2588")
}

# Remove a glyph run (and the non-breaking space that ties it to its label) from a rendered string --
# a graphics device substitutes its own font and has no block glyphs, so grid would draw garbage.
# The console, markdown, Excel and the html <svg> keep the glyphs; a ggplot never does.
#' @keywords internal
tx_spark_strip <- function(x) {
  gsub(tx_spark_pattern(), "", x)
}

# THE one pattern for "this string carries a sparkline", so the strippers, the html <svg> upgrade and
# the Excel per-cell write cannot disagree about what a run is.
#' @keywords internal
tx_spark_pattern <- function(sep = TRUE) {
  gl <- paste(rd_spark_glyphs(), collapse = "")
  paste0(if (sep) "[ \u00a0]?", "[", gl, "]{3,}")
}

#' @keywords internal
tx_has_spark <- function(x) !is.na(x) & grepl(tx_spark_pattern(FALSE), x)

# The window a curve is drawn in, and whether the FLOOR set it (which is what "this reads flat"
# means). One producer for the glyph run and for the shape table's `span` column, so the picture and
# the number beside it cannot state two scales.
#' @keywords internal
rd_spark_window <- function(curve) {
  y <- if (is.data.frame(curve)) curve$y else curve
  span <- if (length(y) < 2L) 0 else diff(range(y))
  se   <- if (is.data.frame(curve) && !is.null(curve$se)) stats::median(curve$se, na.rm = TRUE) else NA
  rung <- if (is.data.frame(curve) && !is.null(curve$rung)) curve$rung[[1L]] else NA_real_
  flr  <- suppressWarnings(max(c(8 * se, rung)[is.finite(c(8 * se, rung))]))
  if (!is.finite(flr)) flr <- 0
  list(span = span, floor = flr, window = max(span, flr), flat = span < flr)
}

#' @keywords internal
rd_spark <- function(curve, on = TRUE, n = 10L) {
  y <- if (is.data.frame(curve)) curve$y else curve
  if (isFALSE(on) || is.null(y) || length(y) < 3L || !all(is.finite(y))) return(NA_character_)
  win <- rd_spark_window(curve)
  if (is.data.frame(curve) && !is.null(curve$x) && all(is.finite(curve$x)) &&
      diff(range(curve$x)) > 0) {
    lo <- max(min(curve$x), min(curve$xlo %||% curve$x))
    hi <- min(max(curve$x), max(curve$xhi %||% curve$x))
    if (!(is.finite(lo) && is.finite(hi) && hi > lo)) { lo <- min(curve$x); hi <- max(curve$x) }
    y  <- stats::approx(curve$x, y, xout = seq(lo, hi, length.out = n), rule = 2)$y
  }
  gl  <- rd_spark_glyphs()
  mid <- mean(range(y))
  lo  <- mid - win$window / 2
  i   <- if (win$window <= 0) rep(ceiling(length(gl) / 2), length(y))
         else 1L + floor((y - lo) / win$window * (length(gl) - 1e-9))
  paste(gl[pmax(pmin(i, length(gl)), 1L)], collapse = "")
}

# The window as a NUMBER a reader can check the picture against: the curve's own range on the
# reading's own measure, or "< <floor>" where the floor is what is drawn (the curve is smaller than
# anything this package would colour, so it reads flat BY CONSTRUCTION rather than by luck).
#' @keywords internal
rd_span_label <- function(curve, kind) {
  win <- rd_spark_window(curve)
  v   <- if (isTRUE(win$flat)) win$floor else win$span
  if (!is.finite(v)) return("")
  sp  <- (RD_LINK_SCALES[[kind %||% "mean"]] %||% RD_LINK_SCALES$mean)$span
  # ⚠ a MEAN is read in SD, like its own colour ladder (COLOR_SCALES$mean_diff is standardized), and
  # the SD is recovered from the stored rung rather than re-derived: the rung IS 0.1 SD.
  sd  <- if (is.data.frame(curve)) 10 * (curve$rung[[1L]] %||% NA_real_) else NA_real_
  lab <- switch(sp,
                mult   = paste0(stringi::stri_unescape_unicode("\\u00d7"),
                                formatC(exp(v), format = "f", digits = if (exp(v) < 10) 1 else 0)),
                points = paste0("+", formatC(100 * v, format = "f", digits = 0), "%"),
                if (is.finite(sd) && sd > 0)
                  paste0("+", formatC(v / sd, format = "f", digits = 1), " ", gettext("SD"))
                else paste0("+", formatC(v, format = "f", digits = 1)))
  if (isTRUE(win$flat)) paste0("<", lab) else lab
}

# ONE residual per family, for the teaching panels: a raw residual takes exactly two values for a
# binary outcome (ROS SS14.5), so every non-gaussian family gets the RANDOMISED QUANTILE residual
# (Dunn & Smyth 1996), standard normal under a correct model whatever the family. Multinomial is
# REFUSED: two level orderings give residuals correlated -0.705, an artefact of the coding.
#
# WARNING: qnorm(1) = Inf -- u must be clamped, or a single saturated fitted value returns Inf.
#' @keywords internal
rd_resid <- function(fit, family, y, trials = NULL, seed = 20260810) {
  family <- reg_check_family_of(family)          # a LINK key (rd/rr/mr) reads as its distribution
  if (family == "multinomial") return(NULL)
  clamp <- function(u) pmin(pmax(u, 1e-10), 1 - 1e-10)
  draw  <- function(lo, hi) rd_with_seed(seed, stats::qnorm(clamp(stats::runif(length(lo), lo, hi))))
  out <- tryCatch({
    if (family == "gaussian") {
      as.numeric(stats::rstandard(fit))
    } else if (family == "ordinal") {
      # fitted() is the n x K category-probability matrix; the cumulative probability of the observed
      # category and of the one below it bracket the randomised quantile residual.
      P  <- stats::fitted(fit)
      if (is.null(dim(P))) return(NULL)
      cp <- t(apply(P, 1L, cumsum))
      k  <- as.integer(as.factor(y))
      hi <- cp[cbind(seq_along(k), k)]
      lo <- ifelse(k > 1L, cp[cbind(seq_along(k), pmax(k - 1L, 1L))], 0)
      draw(lo, hi)
    } else if (reg_fam_count(family)) {
      mu <- as.numeric(stats::fitted(fit)); yy <- as.numeric(y)
      draw(stats::ppois(yy - 1, mu), stats::ppois(yy, mu))
    } else {                                          # binomial / rr / grouped binomial
      mu <- as.numeric(stats::fitted(fit))
      m  <- if (is.null(trials)) 1 else trials
      yy <- if (is.null(trials)) as.numeric(as.numeric(as.factor(y)) - 1) else as.numeric(y)
      draw(stats::pbinom(yy - 1, m, mu), stats::pbinom(yy, m, mu))
    }
  }, error = function(e) NULL)
  if (is.null(out) || !length(out)) return(NULL)
  out
}

# The ANALYTIC pointwise Q-Q band: the i-th of n uniform order statistics is Beta(i, n-i+1), so the
# band is qnorm(qbeta(alpha/2, i, n-i+1)) .. qnorm(qbeta(1-alpha/2, ...)) -- no simulated envelope
# needed.
#
# WARNING: it is POINTWISE, not simultaneous -- under a true model ~5 % of points fall outside AT
# EACH POSITION. The panel subtitle says so; the docs alone would not be enough.
#' @keywords internal
rd_qq <- function(r, conf = 0.95, max_pts = 400L) {
  r <- sort(r[is.finite(r)])
  n <- length(r)
  if (n < 5L) return(NULL)
  i   <- if (n > max_pts) unique(round(seq(1, n, length.out = max_pts))) else seq_len(n)
  a   <- (1 - conf) / 2
  tibble::tibble(
    theoretical = stats::qnorm((i - 0.5) / n),
    sample      = r[i],
    lo          = stats::qnorm(stats::qbeta(a,     i, n - i + 1)),
    hi          = stats::qnorm(stats::qbeta(1 - a, i, n - i + 1)))
}

# Thin a POINT LAYER (never a statistic) toward the extremes: the influence and Q-Q panels exist to
# surface the rare extreme observation, so a uniform subsample would defeat them.
#' @keywords internal
rd_thin <- function(v, max_points = 2000L, seed = 20260810) {
  n <- length(v)
  if (!is.finite(max_points) || n <= max_points) return(seq_len(n))
  keep <- order(abs(v - stats::median(v, na.rm = TRUE)), decreasing = TRUE)[seq_len(max_points %/% 4L)]
  rest <- setdiff(seq_len(n), keep)
  rd_with_seed(seed, sort(c(keep, sample(rest, max_points - length(keep)))))
}

# Evaluate under a fixed seed and give the caller its RNG stream back (`seed = NULL` = a fresh draw).
# Base R, not withr::with_seed(): withr is Suggests-only and these primitives have no dependency.
#' @keywords internal
rd_with_seed <- function(seed, expr) {
  if (is.null(seed)) return(expr)
  has <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old <- if (has) get(".Random.seed", envir = globalenv(), inherits = FALSE) else NULL
  on.exit(if (has) assign(".Random.seed", old, envir = globalenv())
          else suppressWarnings(rm(".Random.seed", envir = globalenv())), add = TRUE)
  set.seed(seed)
  expr
}


# === SECTION: the stored curves =====================================================================

# THE observed curve of every continuous predictor: 10 weighted quantile bins of the outcome against
# the predictor, on the family's own link scale. Computed ONCE per predictor, because it contains no
# fit: a 5-model comparison stores five references to one tibble, and it survives the jamovi digest
# path, where no fit exists.
#
# ONE RECORD PER OUTCOME, keyed by it: with several outcomes there is no single observed shape, and
# a sparkline describing only one of them would be a lie the reader cannot see -- so each gets its
# own, and the display decides where they go (a base-count cell where the table has one, the shape
# table where it has several).
#
# THE CURVE IS THE STORED FACT, never the glyph run: the sparkline is drawn from it at display time
# (materialize_specs()$reg_spark), which is what makes `options(tabxplor.spark = )` a display option
# and what lets each `tab_vars` group carry its own curve -- measured on the group's own data, into
# the group's own base-count cell. Keyed by (variable, group), and `linear_level` names the row it
# belongs to, since `shape = "quadratic"` gives a predictor two of them.
#' @keywords internal
reg_curves <- function(data, specs, numeric_preds, wt = NULL, positive_level = NULL, nbins = 10L,
                       design = NULL) {
  if (length(numeric_preds) == 0L || length(specs) == 0L) return(NULL)
  w  <- if (!is.null(wt) && is.character(wt) && length(wt) == 1L && wt %in% names(data))
          data[[wt]] else NULL
  # under a survey design the bands take the DESIGN variance, reached through `.svy_row`, as every
  # other design quantity in the package.
  dr <- if (!is.null(design)) data[[svy_row_col]] else NULL
  # the first spec of each outcome: everything read below (family, trials, the modelled level) is an
  # outcome fact, so a comparison of several models on one outcome draws one curve, not one per model.
  deps <- unique(vapply(specs, function(s) s$outcome, character(1)))
  pos  <- if (length(positive_level) == length(deps)) as.list(positive_level)
          else stats::setNames(rep(list(positive_level), length(deps)), deps)
  out <- purrr::compact(stats::setNames(purrr::map(deps, function(dep) {
    sp <- specs[[which(vapply(specs, function(s) s$outcome, character(1)) == dep)[[1L]]]]
    if (isTRUE(sp$compound) || is.null(data[[dep]])) return(NULL)
    # WARNING: the MODELLED level, taken from the fit, never the factor's first level -- reading the
    # level order instead draws the curve of the COMPLEMENT (an upside-down sparkline).
    ly <- rd_link_y(data[[dep]], sp$fit_family, sp$trials, pos[[dep]])
    curves <- purrr::compact(stats::setNames(
      purrr::map(numeric_preds, function(v)
        rd_bin(data[[v]], ly$y, w, nbins, ly$link, design = design, des_rows = dr, kind = ly$kind)),
      numeric_preds))
    if (length(curves) == 0L) return(NULL)
    list(outcome = dep, family = sp$fit_family, link = ly$link, kind = ly$kind, ylab = ly$lab,
         curves = curves)
  }), deps))
  if (length(out) == 0L) NULL else out
}


# === SECTION: potools extraction anchor =============================================================

# Nothing here ever runs -- see the file header (i18n) for why this anchor exists.
#' @keywords internal
reg_check_msgid_anchor <- function() {
  if (FALSE) c(
    gettext("Linearity"), gettext("Proportionality"), gettext("Dispersion"),
    gettext("Influence"), gettext("Collinearity"), gettext("Overall association"),
    gettext("Pearson dispersion"), gettext("Residuals"), gettext("Normality"),
    gettext("LR"), gettext("F"), gettext("Wald"), gettext("Brant"),
    gettext("robust/model SE"), gettext("max dfbetas"), gettext("max VIF"), gettext("phi")
  )
  invisible(NULL)
}


# === SECTION: the shape table =======================================================================
#
# WHERE A CURVE GOES is a medium question, decided once (tab_wants_shape_table) and answered in two
# ways -- inside the base-count cell of the row it belongs to, or in a small table of its own below
# the footer. The cell is the better reading (the curve sits beside its coefficient) and is kept
# wherever it works; the table is what the two cases that break it get:
#   * the CONSOLE, whose block glyphs are East-Asian-width-ambiguous. A terminal that draws them wide
#     shifts every column to their right, and a package cannot choose its reader's font. In a table of
#     its own the run is the LAST column, so a wide glyph costs nothing.
#   * SEVERAL OUTCOMES, which share one base-count column: a cell of it could show only one of them.
#
# The curve is FIT-FREE (rd_bin bins the observed outcome), so the predictor column names the variable
# and never its parametrisation -- `shape = "quadratic"` changes the model, not what was observed.
#' @keywords internal
#' @noRd
tab_wants_shape_table <- function(tab, medium = "console") {
  if (!tab_is_reg(tab) || isFALSE(tx_option("spark")) || identical(medium, "plot")) return(FALSE)
  a <- get_assumptions(tab)
  if (is.null(a) || length(a) == 0L) return(FALSE)
  identical(medium, "console") || length(a) > 1L
}

# THE producer: one row per (outcome, group, numeric predictor), already rendered. Returns NULL when
# there is nothing to draw. `headers` and `note` ride as attributes -- one producer, and every medium
# renders the same four columns in the same order.
#' @keywords internal
#' @noRd
reg_shape_table <- function(tab, n = 20L) {
  a <- get_assumptions(tab)
  if (is.null(a) || length(a) == 0L) return(NULL)
  rows <- purrr::list_rbind(purrr::map(a, function(rec) {
    purrr::list_rbind(purrr::imap(rec$curves, function(cu, v) {
      grp <- unique(as.character(cu$group %||% ""))
      purrr::list_rbind(purrr::map(grp, function(g) {
        cg <- cu[(cu$group %||% "") == g, , drop = FALSE]
        gl <- rd_spark(cg, n = n)
        if (is.na(gl)) return(NULL)
        tibble::tibble(outcome = rec$outcome, group = g, var = v,
                       span = rd_span_label(cg, rec$kind), shape = gl,
                       ylab = rec$ylab %||% "")
      }))
    }))
  }))
  if (is.null(rows) || nrow(rows) == 0L) return(NULL)
  keep_group <- length(unique(rows$group)) > 1L
  out <- rows[, c("outcome", if (keep_group) "group", "var", "span", "shape"), drop = FALSE]
  # the outcome (and the group) named ONCE per run, like the row-variable block of a table
  out$outcome[duplicated(out$outcome)] <- ""
  if (keep_group) out$group[duplicated(rows[, c("outcome", "group")])] <- ""
  structure(
    out,
    headers = c(gettext("outcome"), if (keep_group) gettext("group"),
                gettext("numeric predictor"), gettext("span"), gettext("observed shape")),
    align   = c("left", if (keep_group) "left", "left", "right", "left"),
    # ⚠ ONE STRING LITERAL PER gettext() CALL. potools extracts each literal it sees, while gettext()
    # looks the EVALUATED string up -- so a message built with paste0() INSIDE the call is extracted
    # in pieces and can never be found at run time. Two sentences, two calls, joined after.
    note    = paste(
      gettextf("the outcome's observed shape across the central 95%% of each predictor, on the model's own scale (%s);",
               paste(unique(rows$ylab), collapse = ", ")),
      gettext("span = the curve's whole range; \"<\" = smaller than the window it is drawn in, so it reads flat.")))
}

# A GFM pipe table from already-rendered character columns -- the console's shape table and the
# Markdown exporter's are the same lines, so they are built once. Widths are counted in CHARACTERS,
# exact in a monospace medium and near enough in a proportional one.
#' @keywords internal
#' @noRd
tx_pipe_table <- function(df, headers, align) {
  cols <- lapply(seq_along(df), function(j) c(headers[[j]], as.character(df[[j]])))
  w    <- vapply(cols, function(c) max(nchar(c, type = "chars")), integer(1))
  pad  <- function(s, j) formatC(s, width = w[[j]], flag = if (align[[j]] == "right") "" else "-")
  emit <- function(cells) paste0("| ", paste(cells, collapse = " | "), " |")
  c(emit(vapply(seq_along(df), function(j) pad(headers[[j]], j), character(1))),
    paste0("|", paste(vapply(seq_along(df), function(j) mk_align(w[[j]], align[[j]]), character(1)),
                      collapse = "|"), "|"),
    vapply(seq_len(nrow(df)), function(i)
      emit(vapply(seq_along(df), function(j) pad(as.character(df[[j]])[[i]], j), character(1))),
      character(1)))
}

# The console rendering: the pipe table, one blank line under the footer grid, then the note.
#' @keywords internal
#' @noRd
shape_render_console <- function(tab) {
  st <- reg_shape_table(tab)
  if (is.null(st)) return(invisible(NULL))
  # the footer grid already closes with a blank line -- one separates the two tables, never two.
  cli::cat_line(tx_pipe_table(st, attr(st, "headers"), attr(st, "align")))
  cli::cat_line(cli::col_grey(paste0("# ", attr(st, "note"))))
  cli::cat_line()
  invisible(NULL)
}
