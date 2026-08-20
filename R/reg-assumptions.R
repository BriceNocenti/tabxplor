# PURPOSE: THE MODEL CHECKS of a tab_reg() table, the `shape =` CURE for what they flag, and the
#   primitives their plots are drawn from.
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
# EACH COSTS WHAT IT SAYS. Two of the five need a model fit -- Linearity one per numeric predictor,
# Proportionality the Brant test's auxiliary logits -- and REG_CHECKS$cost declares that, which is
# what keeps them out of the default `stats` set and reachable by name. The other three are
# arithmetic on the fit already in hand, and the two influence-based ones share one sweep. What is
# opt-in is the p-value, not the diagnostic: reg_curves() bins the observed shape with no fit at all,
# and the base-count-cell sparkline and the reg_check_plots() panels draw it for free.
#
# THE CURE IS PART OF THE CHECK. `shape =` is how a user fixes a non-linearity without leaving the
# framework, and its design rule keeps it small: a shape either RECODES THE COLUMN or ADDS ONE TERM,
# nothing else. A quantile-cut predictor genuinely IS a factor, so it inherits the saturated crude
# twin, the per-level counts, colours and gap tests for free. It reads the package's shared
# per-predictor grammar (reg_per_predictor(), R/reg-resolve.R), so `shape = "quintiles"` cuts every
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
#   panel         the reg_check_plots() panel this check draws (NA = no panel), and the `check =`
#                 vocabulary. `auto` draws every panel the family allows. ⚠ INDEPENDENT of `cost`:
#                 a panel is always free, which is why reg_check_plots() never filters on it.
#' @keywords internal
REG_CHECKS <- list(
  # 1. the ESTIMATE: is this predictor's effect really one straight line?
  linearity = list(
    noun = "Linearity",
    types = c(linearity_lr = "LR", linearity_f = "F", linearity_wald = "Wald"),
    kind = "pvalue", digits = NA_integer_,
    families = REG_CHECK_FAMILIES, weighted_ok = TRUE, per_predictor = TRUE,
    cost = "refit", panel = "linearity"),
  # 2. what the estimate MEANS: is one odds ratio enough for every cut?
  proportionality = list(
    noun = "Proportionality",
    types = c(proportionality = "Brant"),
    kind = "pvalue", digits = NA_integer_,
    families = "ordinal", weighted_ok = FALSE, per_predictor = FALSE,
    cost = "refit", panel = "proportionality"),
  # 3. the INTERVAL: are the standard errors wide enough?
  dispersion = list(
    noun = "Dispersion",
    types = c(dispersion = "robust/model SE"),
    kind = "gof", digits = 2L,
    families = REG_CHECK_FAMILIES, weighted_ok = TRUE, per_predictor = FALSE,
    cost = "free", panel = "dispersion"),
  # 4. is it REAL: does one respondent carry the result?
  influence = list(
    noun = "Influence",
    types = c(influence = "max dfbetas"),
    kind = "gof", digits = 2L,
    families = REG_CHECK_FAMILIES, weighted_ok = TRUE, per_predictor = FALSE,
    cost = "free", panel = "influence"),
  # 5. why is it WIDE: can the data tell these predictors apart?
  collinearity = list(
    noun = "Collinearity",
    types = c(collinearity = "max VIF"),
    kind = "gof", digits = 2L,
    families = setdiff(REG_CHECK_FAMILIES, "multinomial"), weighted_ok = TRUE,
    per_predictor = FALSE, cost = "free", panel = "collinearity"),
  # TAUGHT, NEVER SCORED. Both were measured not to discriminate as verdicts, but both are the
  # canonical lessons, so they keep their panel and give up their row -- an empty `types` IS that
  # statement.
  residuals = list(
    noun = "Residuals", types = character(0), kind = NA_character_, digits = NA_integer_,
    families = setdiff(REG_CHECK_FAMILIES, "multinomial"), weighted_ok = TRUE,
    per_predictor = FALSE, cost = "free", panel = "residuals"),
  normality = list(
    noun = "Normality", types = character(0), kind = NA_character_, digits = NA_integer_,
    families = setdiff(REG_CHECK_FAMILIES, "multinomial"), weighted_ok = TRUE,
    per_predictor = FALSE, cost = "free", panel = "normality")
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

# The DEFAULT footer set: the applicable checks that cost no model fit. A check named explicitly in
# `stats =` is still computed and shown -- default set vs vocabulary, not vocabulary vs nothing.
#' @keywords internal
reg_checks_default <- function(family, weighted = FALSE, has_fit = TRUE) {
  keys <- reg_checks_for(family, weighted, has_fit, what = "footer")
  keys[vapply(keys, function(k) identical(REG_CHECKS[[k]]$cost, "free"), logical(1))]
}

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
             logical(1)))
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
test_rows_from_checks <- function() {
  out <- list()
  for (ck in REG_CHECKS) for (d in names(ck$types)) {
    # ⚠ NA, never NULL: TEST_ROWS defaults its members through utils::modifyList(), which REMOVES an
    # entry whose value is NULL instead of setting it -- so a check whose instrument names a quantity
    # ("max VIF") rather than a term test would lose the member outright.
    row <- list(producer = "reg", kind = ck$kind, render = "grid",
                noun = ck$noun, instrument = ck$types[[d]],
                stat = REG_CHECK_KEY_OF[[d]] %||% NA_character_,
                method = REG_CHECK_METHOD[[ck$types[[d]]]] %||% NA_character_)
    if (identical(ck$kind, "gof")) row$digits <- as.integer(ck$digits)
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
  v <- tryCatch(suppressWarnings(car::vif(fit)), error = function(e) NULL)
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
      bp <- reg_ordinal_diagnostic(fit)
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


# === SECTION: `shape` -- fitting a predictor as something other than a line =========================
#
# The design rule is stated in the file header (THE CURE IS PART OF THE CHECK). Before this feature,
# `predictors = c("race", "poly(age, 2)")` errored, and the formula escape hatch silently disabled
# `empirical =`, `color = "adjustment"`, `multiplier` and the per-predictor tests.
#
# WARNING -- poly() / ns() / bs() are NEVER emitted, and that is a wrong-number refusal, not taste:
# `marginaleffects` returns AME = 0.000000 for them, silently, through every contrast form. I(x^2),
# raw polynomials and log() are correct through every route.

# The closed vocabulary. Anything else is an integer k (k quantile groups) or an error -- there is no
# alias table, so what the docs list is what the parser accepts.
#' @keywords internal
REG_SHAPES <- c("linear", "quadratic", "log", "sqrt", "quartiles", "quintiles")

# The number of quantile groups a value asks for (NA = it is not a cut request).
#' @keywords internal
reg_shape_k <- function(value) {
  if (identical(value, "quartiles")) return(4L)
  if (identical(value, "quintiles")) return(5L)
  k <- suppressWarnings(as.integer(value))
  if (!is.na(k) && k >= 2L && k <= 20L && identical(trimws(as.character(value)), as.character(k)))
    k else NA_integer_
}

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
  s <- reg_predictor_sd(x, w)
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

# One value -> the shape spec it names, or NULL for the default. The whole vocabulary is here: a
# quantile count, or one of REG_SHAPES.
#' @keywords internal
reg_shape_value <- function(val, var) {
  kind <- if (is.character(val)) trimws(tolower(val)) else val
  k    <- reg_shape_k(kind)
  if (!is.na(k)) return(list(kind = "quantiles", k = k))
  if (!is.character(kind) || length(kind) != 1L || !kind %in% REG_SHAPES)
    cli::cli_abort(c(
      "{.arg shape} for {.val {var}} must be one of {.or {.val {REG_SHAPES}}}, or a number of groups.",
      "x" = "Got {.val {as.character(val)[[1]]}}.",
      "i" = '{.val quintiles} (or an integer) cuts it into quantile groups -- one estimate each.'),
      call = NULL)
  if (identical(kind, "linear")) return(NULL)          # the default, spelled out: nothing to emit
  list(kind = kind, k = NA_integer_)
}

# The whole `shape` argument -> a named list of list(kind, k), on the package's shared per-predictor
# grammar (reg_per_predictor(), R/reg-resolve.R). Validated against the data, so every refusal names
# the variable and the value the user wrote.
#' @keywords internal
reg_resolve_shape <- function(shape, data, predictors) {
  if (is.null(shape) || length(shape) == 0L) return(list())
  preds <- intersect(predictors, names(data))
  reg_check_continuous_names(shape, data, preds, "shape")
  vals  <- reg_per_predictor(shape, reg_numeric_preds(data, preds), "shape")
  purrr::compact(purrr::imap(vals, function(v, nm) reg_shape_value(v, nm)))
}

# k quantile groups of a continuous column, as an ordinary (unordered) factor. Breaks are WEIGHTED
# quantiles when the call carries weights (equal-share of the POPULATION, not the sample), with the
# extremes forced to the observed range so no value falls out.
#' @keywords internal
reg_cut_quantiles <- function(x, k, w = NULL, var = "x", breaks = NULL) {
  x  <- as.numeric(x)
  if (!is.null(breaks))
    return(local({ f <- cut(x, breaks = breaks, include.lowest = TRUE, right = FALSE, dig.lab = 4L)
                   factor(as.character(f), levels = levels(f)) }))
  br <- rd_wquantile(x, seq(0, 1, length.out = k + 1L), w)
  if (all(is.finite(x[!is.na(x)]))) {
    br[[1L]]      <- min(x, na.rm = TRUE)
    br[[k + 1L]]  <- max(x, na.rm = TRUE)
  }
  br <- unique(br[is.finite(br)])
  if (length(br) < 3L) {
    cli::cli_abort(c("{.arg shape} cannot cut {.val {var}} into {k} groups.",
                     "x" = "Its distribution has too few distinct values.",
                     "i" = "Use fewer groups, or pass it as a factor."))
  }
  f <- cut(x, breaks = br, include.lowest = TRUE, right = FALSE, dig.lab = 4L)
  # ⚠ the BREAKS ride out with the factor: reg_prepare_replay() must cut a refit's frame at exactly
  # the same places, and a weighted quantile of a different frame would not land there.
  structure(factor(as.character(f), levels = levels(f)), tabxplor_breaks = br)
}

# Apply every column-recoding shape ONCE, and return the display labels the transformed ones need
# ("log(age)") plus the shapes with their quantile BREAKS filled in. `quadratic` is not a recode --
# it emits a term -- so it passes through untouched.
#' @keywords internal
reg_shape_apply <- function(data, shapes, w = NULL) {
  labels <- character(0)
  wv <- if (!is.null(w) && is.character(w) && length(w) == 1L && w %in% names(data)) data[[w]] else NULL
  for (v in names(shapes)) {
    kind <- shapes[[v]]$kind
    x    <- as.numeric(data[[v]])
    if (kind == "quadratic") next
    if (kind == "log") {
      if (any(x <= 0, na.rm = TRUE)) {
        cli::cli_abort(c('{.code shape = "log"} needs strictly positive values.',
                         "x" = "{.val {v}} has values <= 0.",
                         "i" = 'Use {.val sqrt}, {.val quintiles}, or shift the variable first.'))
      }
      data[[v]] <- log(x)
      labels[[v]] <- paste0("log(", v, ")")
    } else if (kind == "sqrt") {
      if (any(x < 0, na.rm = TRUE)) {
        cli::cli_abort(c('{.code shape = "sqrt"} needs non-negative values.',
                         "x" = "{.val {v}} has negative values."))
      }
      data[[v]] <- sqrt(x)
      labels[[v]] <- paste0("sqrt(", v, ")")
    } else if (kind == "quantiles") {
      f <- reg_cut_quantiles(x, shapes[[v]]$k, wv, var = v, breaks = shapes[[v]]$breaks)
      shapes[[v]]$breaks <- attr(f, "tabxplor_breaks") %||% shapes[[v]]$breaks
      attr(f, "tabxplor_breaks") <- NULL
      data[[v]] <- f
    }
  }
  list(data = data, labels = labels, shapes = shapes)
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


# === SECTION: the plot primitives ===================================================================
#
# Five base-R functions, no dependency. They are the ONLY producers of the numbers every panel and
# the row sparkline draw.
#
# WARNING for whoever adds a panel later: never `geom_smooth(method = "auto")`. It switches loess -> gam
# at 1000 observations in the largest GROUP, so a facetted 50 000-row plot gets loess and an unfacetted
# 1200-row one gets gam -- and its message is assembled dynamically, so it cannot be regex-suppressed.
# Nothing here smooths: the comparator of a linearity panel must be the STRAIGHT line the model assumes.

# Weighted quantiles (the midpoint / Hmisc definition). One producer for the sparkline bins, the panel
# bins and `shape = "quintiles"`, so a cut group and its curve can never disagree about where a break is.
#' @keywords internal
rd_wquantile <- function(x, probs, w = NULL) {
  x <- as.numeric(x)
  ok <- is.finite(x)
  w  <- if (is.null(w)) rep(1, length(x)) else as.numeric(w)
  ok <- ok & is.finite(w) & w > 0
  if (!any(ok)) return(rep(NA_real_, length(probs)))
  x <- x[ok]; w <- w[ok]
  o <- order(x); x <- x[o]; w <- w[o]
  if (length(x) == 1L) return(rep(x, length(probs)))
  cw <- (cumsum(w) - 0.5 * w) / sum(w)
  stats::approx(cw, x, xout = probs, rule = 2, ties = "ordered")$y
}

# The per-observation outcome a check reads, on the family's own LINK scale, plus that scale's label.
# An ordinal / multinomial outcome has no single curve, so it is read as "beyond the first category"
# -- stated in the axis label, never implied.
#' @keywords internal
rd_link_y <- function(y, family, trials = NULL, positive_level = NULL) {
  family <- reg_check_family_of(family)          # a LINK key (rd/rr/mr) reads as its distribution
  if (family == "gaussian")
    return(list(y = as.numeric(y), link = "identity", lab = gettext("mean")))
  if (reg_fam_count(family))
    return(list(y = as.numeric(y), link = "log", lab = gettext("log(mean)")))
  if (reg_fam_binary(family) && !is.null(trials))
    return(list(y = as.numeric(y) / trials, link = "logit", lab = gettext("empirical logit")))
  if (reg_fam_binary(family)) {
    yy <- if (!is.null(positive_level)) as.numeric(as.character(y) == positive_level)
          else                          as.numeric(as.factor(y)) - 1
    return(list(y = yy, link = "logit", lab = gettext("empirical logit")))
  }
  # ordinal / multinomial: the one cut every K-category outcome has.
  list(y = as.numeric(as.numeric(as.factor(y)) > 1), link = "logit",
       lab = gettext("empirical logit (beyond the first category)"))
}

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

#' @keywords internal
rd_bin <- function(x, y, w = NULL, nbins = 10L, link = "identity",
                   design = NULL, des_rows = NULL) {
  x <- as.numeric(x); y <- as.numeric(y)
  wtd <- !is.null(w)
  w <- if (is.null(w)) rep(1, length(x)) else as.numeric(w)
  ok <- is.finite(x) & is.finite(y) & is.finite(w) & w > 0
  if (sum(ok) < 2L) return(NULL)
  x <- x[ok]; y <- y[ok]; w <- w[ok]
  if (!is.null(des_rows)) des_rows <- des_rows[ok]
  br <- unique(rd_wquantile(x, seq(0, 1, length.out = nbins + 1L), w))
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
  num <- switch(link, "logit" = my * (1 - my), "log" = pmax(my, 0), vy)
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
    list(y = my, se = sqrt(vy / ne))
  )
  tibble::tibble(x = mx, y = out$y, n = sw, se = out$se)
}

# The 8-level block sparkline of a curve, min-max rescaled WITHIN the predictor -- so it answers
# "is it a line?" and never "is the effect big?". `style` is TRUE / FALSE: there is no plain-text
# ladder, because eight ASCII ranks (". , - ~ + = * #") do not read as a CURVE at all -- the shape
# is the whole point, and a reader who cannot see it is better served by no sparkline.
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

#' @keywords internal
rd_spark <- function(y, on = TRUE) {
  if (isFALSE(on) || is.null(y) || length(y) < 3L || !all(is.finite(y))) return(NA_character_)
  r <- range(y)
  gl <- rd_spark_glyphs()
  i  <- if (diff(r) <= 0) rep(ceiling(length(gl) / 2), length(y))
        else 1L + floor((y - r[[1L]]) / diff(r) * (length(gl) - 1e-9))
  paste(gl[pmax(pmin(i, length(gl)), 1L)], collapse = "")
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
# WITH SEVERAL OUTCOMES there is no single observed shape, so the whole thing is NULL rather than the
# first outcome's silently: a sparkline describing only one of several outcomes would be a lie the
# reader cannot see.
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
  deps <- unique(vapply(specs, function(s) s$outcome, character(1)))
  if (length(deps) != 1L) return(NULL)
  sp <- specs[[1L]]
  if (isTRUE(sp$compound) || is.null(data[[deps]])) return(NULL)
  # WARNING: the MODELLED level, taken from the fit, never the factor's first level -- reading the
  # level order instead draws the curve of the COMPLEMENT (an upside-down sparkline).
  ly <- rd_link_y(data[[deps]], sp$fit_family, sp$trials, positive_level)
  w  <- if (!is.null(wt) && is.character(wt) && length(wt) == 1L && wt %in% names(data))
          data[[wt]] else NULL
  # under a survey design the bands take the DESIGN variance, reached through `.svy_row`, as every
  # other design quantity in the package.
  dr <- if (!is.null(design)) data[[svy_row_col]] else NULL
  curves <- purrr::compact(stats::setNames(
    purrr::map(numeric_preds, function(v)
      rd_bin(data[[v]], ly$y, w, nbins, ly$link, design = design, des_rows = dr)),
    numeric_preds))
  if (length(curves) == 0L) return(NULL)
  list(outcome = deps, family = sp$fit_family, link = ly$link, ylab = ly$lab, curves = curves)
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
