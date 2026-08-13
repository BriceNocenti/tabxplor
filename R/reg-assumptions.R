# Phase 18z15 -- THE model checks of a `tab_reg()` table, their CURE (`shape =`) and the
# primitives its plots are drawn from.
#
# ROLE: one fact table (REG_CHECKS), one selection rule (reg_checks_for), one producer
# (reg_check_rows) and one label builder (reg_check_spec_entries). Adding a check is ONE row: the
# footer label, the `stats =` value, the `check =` value and the panel title all derive from it, so
# they cannot drift -- the REG_EMPIRICAL / reg_crude_shape pattern of Phase 17h.
#
# THE IDEA. tabxplor's headline feature is a comparison: `Model_OR` beside `Obs_OR`, coloured by the
# gap and tested by `gap_se`. Every check here is that same comparison applied to something other than
# an effect -- the SHAPE of a numeric predictor's effect (Linearity), the SPREAD of the outcome
# (Dispersion), the MEANING of an ordinal effect (Proportionality), the WEIGHT of one respondent
# (Influence). Collinearity is the one exception and says so (it is a property of the design matrix,
# it biases nothing) -- it is here because it is what every textbook and jamovi's own Assumption
# Checks pane put first, so its absence would read as an omission.
#
# NOTHING HERE IS A NEW STATISTIC ENGINE. Four of the five reuse code the package already owns:
#   Linearity       reg_fit(add_terms =) + reg_term_tests()  -- the dispatcher `global`/`interaction` use
#   Proportionality attr(fit, "brant_po"), already computed at fit time by reg_fit_ordinal()
#   Dispersion      reg_coef_if_maker() + reg_if_se()        -- the sandwich, design-aware
#   Influence       reg_coef_if_maker()                      -- the same closure, per observation
#   Collinearity    car::vif()                               -- the one new Suggest
#
# WARNING -- i18n. `noun` and the `types` values (the instrument) are BARE MSGIDS and are never
# gettext()'d in the list: a top-level list evaluates ONCE at load, which would freeze the msgid at
# the build locale and make with_legend_lang()'s LANGUAGE switch a no-op. gettext() is applied by
# reg_check_spec_entries(), at render. Because those gettext() calls are DYNAMIC (gettext(ck$noun)),
# potools cannot see them -- hence the dead-code extraction anchor at the bottom of this file, the
# same device legend_measure_word() uses for the MEASURES words.
#
# See: dev/regression_assumptions_plots.md (the design, its measurements and its twelve rulings).


# === SECTION: the fact table ========================================================================

# Every family a check can be asked about. `grouped` (a summed-score binomial) is a flag on top of
# "binomial", never a family of its own, so it needs no entry.
#' @keywords internal
REG_CHECK_FAMILIES <- c("gaussian", "binomial", "poisson", "quasipoisson", "rr",
                        "multinomial", "ordinal")

# ONE row per check.
#   noun          the assumption, as a word the reader already knows (a msgid)
#   types         discriminator -> INSTRUMENT (a msgid). The label is "<noun> (<instrument>)", the
#                 convention Phase 18m set for the crosstab summary ("pvalue (Chi2, Welch F)").
#                 A term test carries three discriminators because exactly one of LR / F / Wald fires,
#                 and which one is a fact about the model the reader should see.
#                 EMPTY = the check is TAUGHT but never SCORED: it contributes a panel and no footer
#                 row (SS14 -- the two panels that measured as non-discriminating checks).
#   kind/digits   the reg_footer_spec() rendering (a p-value cell, or a gof number with `digits`)
#   families      where the check is defined at all
#   weighted_ok   FALSE = refused on a weighted / design fit (never approximated)
#   per_predictor one row per (model column x predictor) rather than one per model column
#   panel         the reg_check_plots() panel this check draws (NA = no panel), and the `check =`
#                 vocabulary. `auto` draws every panel the family allows.
#' @keywords internal
REG_CHECKS <- list(
  # 1. the ESTIMATE: is this predictor's effect really one straight line?
  linearity = list(
    noun = "Linearity",
    types = c(linearity_lr = "LR", linearity_f = "F", linearity_wald = "Wald"),
    kind = "pvalue", digits = NA_integer_,
    families = REG_CHECK_FAMILIES, weighted_ok = TRUE, per_predictor = TRUE,
    panel = "linearity"),
  # 2. what the estimate MEANS: is one odds ratio enough for every cut?
  proportionality = list(
    noun = "Proportionality",
    types = c(proportionality = "Brant"),
    kind = "pvalue", digits = NA_integer_,
    families = "ordinal", weighted_ok = FALSE, per_predictor = FALSE,
    panel = "proportionality"),
  # 3. the INTERVAL: are the standard errors wide enough?
  dispersion = list(
    noun = "Dispersion",
    types = c(dispersion = "robust/model SE"),
    kind = "gof", digits = 2L,
    families = REG_CHECK_FAMILIES, weighted_ok = TRUE, per_predictor = FALSE,
    panel = "dispersion"),
  # 4. is it REAL: does one respondent carry the result?
  influence = list(
    noun = "Influence",
    types = c(influence = "max dfbetas"),
    kind = "gof", digits = 2L,
    families = REG_CHECK_FAMILIES, weighted_ok = TRUE, per_predictor = FALSE,
    panel = "influence"),
  # 5. why is it WIDE: can the data tell these predictors apart?
  collinearity = list(
    noun = "Collinearity",
    types = c(collinearity = "max VIF"),
    kind = "gof", digits = 2L,
    families = setdiff(REG_CHECK_FAMILIES, "multinomial"), weighted_ok = TRUE,
    per_predictor = FALSE, panel = "collinearity"),
  # TAUGHT, NEVER SCORED (SS14). Both were measured NOT to discriminate as verdicts -- binned residuals
  # put 45 % of bins outside the band for the mis-specified model against 40 % for the corrected one,
  # and normality is irrelevant to coefficient inference at survey n -- but both are the canonical
  # lessons, so they keep their panel and give up their row. An empty `types` IS that statement.
  residuals = list(
    noun = "Residuals", types = character(0), kind = NA_character_, digits = NA_integer_,
    families = setdiff(REG_CHECK_FAMILIES, "multinomial"), weighted_ok = TRUE,
    per_predictor = FALSE, panel = "residuals"),
  normality = list(
    noun = "Normality", types = character(0), kind = NA_character_, digits = NA_integer_,
    families = setdiff(REG_CHECK_FAMILIES, "multinomial"), weighted_ok = TRUE,
    per_predictor = FALSE, panel = "normality")
)

# Every discriminator the checks can emit (the `test` values that are check rows).
#' @keywords internal
reg_check_types <- function() unlist(lapply(REG_CHECKS, function(ck) names(ck$types)),
                                     use.names = FALSE)

# THE selection rule: which checks apply to this fit? Read by reg_footer_stats() (the default set +
# the `stats =` vocabulary), by reg_check_rows() (what to compute) and by reg_check_plots()
# (`what = "panel"`, which keeps the taught-but-unscored rows and drops any check with no panel).
# `has_fit` is FALSE on the jamovi digest path, which deliberately keeps no model frame -- every
# check reads the fit, so they degrade to absent there rather than to a wrong number.
#' @keywords internal
reg_checks_for <- function(family, weighted = FALSE, grouped = FALSE, has_fit = TRUE,
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

# The reg_footer_spec() entries the checks contribute -- one per discriminator, label built HERE (at
# render, under the ambient locale, like every other footer label).
#' @keywords internal
reg_check_spec_entries <- function() {
  out <- list()
  for (ck in REG_CHECKS) for (d in names(ck$types)) {
    out[[d]] <- list(label = reg_check_label(ck$noun, ck$types[[d]]),
                     kind = ck$kind, digits = ck$digits)
  }
  out
}

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
# withReplicates, which svyrecvar cannot do -- refuse rather than approximate, as z8-B already does.
#' @keywords internal
reg_check_design <- function(fit) {
  des <- if (inherits(fit, "svyglm")) fit$survey.design else NULL
  if (inherits(des, "svyrep.design")) return(NULL)
  des
}

# The model-based standard errors, on the fit's NATIVE coefficient scale, in vcov order. This is the
# denominator of both Dispersion and Influence, and taking it from vcov() rather than from the printed
# `tidy` is what makes Dispersion answer the question SS9.2 states: vcov() already carries a
# quasi-likelihood's estimated dispersion (so a quasipoisson fit reads ~1) while a plain poisson's
# does not (so it reads ~sqrt(phi)) -- the two families' rows then say different, true things.
#' @keywords internal
reg_check_model_se <- function(fit) {
  V <- tryCatch(stats::vcov(fit), error = function(e) NULL)
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

# Check 3 -- DISPERSION, as max_j |SE_robust,j / SE_model,j|.
#
# One number replacing four textbook checks: it reads ~1 when the family's variance assumption holds,
# and above 1 under over-dispersion, heteroscedasticity or clustering. It never touches df.residual,
# which is why it is computable on a clustered design where the Pearson phi is not (df.residual of an
# svyglm is the DESIGN df).
#' @keywords internal
reg_check_dispersion <- function(fit) {
  se_mod <- reg_check_model_se(fit)
  if (is.null(se_mod)) return(NA_real_)
  cif <- reg_coef_if_maker(fit)
  if (is.null(cif)) return(NA_real_)
  des <- reg_check_design(fit)
  p   <- length(se_mod)
  out <- NA_real_
  for (j in seq_len(p)) {
    e <- rep(0, p); e[[j]] <- 1
    d <- cif(e)
    if (is.null(d)) return(NA_real_)                          # the closure's p disagrees with vcov's
    s <- reg_if_se(d, des)
    if (!is.finite(s)) next
    r <- s / se_mod[[j]]
    if (is.na(out) || r > out) out <- r
  }
  out
}

# Check 4 -- INFLUENCE, as max_j max_i |dfbetas_ij|: "no single respondent moves any coefficient by
# more than X of its own standard error". dfbetas rather than Cook's distance because Cook's D is
# unreadable at survey n (its conventional cutoff of 1 fires at no sample size measured) while a
# standardized change is scale-free at any n.
#
# The one-step dfbeta IS the influence function the package already computes: IF_i(e_j) =
# (A^-1 X_i' W_i r_i)_j, which is dfbeta_ij up to the (1 - h_i) leverage correction -- measured
# against stats::dfbetas() at correlation 0.999999 (0.214 vs 0.215) on the vignette's own model. And
# unlike base R it exists for polr / multinom, and it is design-aware.
#
# WARNING: never materialise the n x p matrix (reg-influence.R's memory contract). The loop keeps a
# running maximum and discards each length-n vector.
#' @keywords internal
reg_check_influence <- function(fit) {
  se_mod <- reg_check_model_se(fit)
  if (is.null(se_mod)) return(NA_real_)
  cif <- reg_coef_if_maker(fit)
  if (is.null(cif)) return(NA_real_)
  p   <- length(se_mod)
  out <- NA_real_
  for (j in seq_len(p)) {
    e <- rep(0, p); e[[j]] <- 1
    d <- cif(e)
    if (is.null(d)) return(NA_real_)
    m <- suppressWarnings(max(abs(as.numeric(d)), na.rm = TRUE)) / se_mod[[j]]
    if (!is.finite(m)) next
    if (is.na(out) || m > out) out <- m
  }
  out
}

# Check 5 -- COLLINEARITY, as the largest variance inflation factor.
#
# car::vif() returns a bare VIF per term when every term is 1-df, and a (GVIF, Df, GVIF^(1/(2Df)))
# matrix as soon as one is not. Those are different scales, so the matrix form is squared back onto
# the familiar VIF scale (what performance::check_collinearity() reports) -- a 1-df term then gives
# exactly its VIF either way, and the usual 5 / 10 readings apply to one column of numbers.
#
# `car` is Suggests-only: absent -> no row, never a hand-rolled substitute (the det-ratio alternative
# was measured at 11.45 where car returns 1.01 on a polr fit).
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

# Check 1 -- LINEARITY, per numeric predictor: the model plus this predictor's CENTRED SQUARED term,
# tested by the shared reg_term_tests() dispatcher (drop1 unweighted, survey::regTermTest on a design,
# F for gaussian / quasipoisson). This is car::residualPlots()'s curvature test, design-correct for
# free -- and deliberately NOT the cheaper no-refit Rao score test, which returns the IDENTICAL p on a
# weights-only and on a stratified+clustered design where the design-based Wald differs by thirty
# orders of magnitude.
#
# The squared term is built by reg_shape_term(), which the `shape = "quadratic"` remedy will emit --
# so the check and its cure are provably the same object, not two spellings of one idea. Centring is
# not cosmetic: uncentred, the pair's own VIF is 38.7 against 1.2 centred, so check 5 would flag every
# curved model as broken.
# The likelihood-ratio test between two NESTED maximum-likelihood fits, from their log-likelihoods.
# It is not an approximation of what drop1() returns -- it IS what drop1() returns: verified equal to
# 1e-10 on a glm. It exists because `nnet:::drop1.multinom` computes only Df and AIC (it has no `test`
# argument and no p-value at all), so the multinomial arm had no test otherwise. Returns NULL for a
# quasi-likelihood or anything without a usable logLik, never a wrong number.
#' @keywords internal
reg_nested_lr <- function(base, aug) {
  ll <- function(f) tryCatch(as.numeric(stats::logLik(f)), error = function(e) NA_real_)
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
  list(stat = s, df = k, p = stats::pchisq(s, k, lower.tail = FALSE))
}

#' @keywords internal
reg_check_linearity_rows <- function(data, sp, shared, fit_first_col_i, row, base_fit = NULL) {
  # A predictor the user has already CURED gets no row: `shape = "quadratic"` puts this very term in
  # the model, so adding it again is a collinear duplicate the engine silently drops. (`log`/`sqrt`
  # recode the column, so the check then asks the right new question -- does log(x) still curve? -- and
  # a quantile-cut predictor is a factor, which has no functional form to mis-specify.)
  num <- setdiff(reg_numeric_preds(data, sp$predictors), names(shared$shape_terms))
  if (length(num) == 0L) return(NULL)
  weighted <- isTRUE(shared$weighted)
  use_f    <- sp$family %in% c("gaussian", "quasipoisson")
  use_wald <- weighted || sp$family == "rr"
  types    <- c(wald = "linearity_wald", f = "linearity_f", lr = "linearity_lr")

  purrr::flatten(purrr::map(num, function(v) {
    tm <- reg_shape_term(data[[v]], v, "quadratic")
    if (is.null(tm)) return(NULL)
    # A diagnostic refit must be SILENT: reg_fit() informs about the detected binary level, about an
    # ordinal outcome being ordered, about svyolr's untested parallel lines, and warns about
    # over-dispersion -- all already said once by the real fit, and this runs once per numeric
    # predictor. Its only output is a p-value.
    f2 <- tryCatch(suppressWarnings(suppressMessages(
            reg_fit(data, sp$dependent, sp$predictors, sp$family, shared$design_spec, sp$do_exp,
                    if (is.null(sp$inverse)) shared$inverse_two_level_factors else sp$inverse,
                    shared$conf_level, "wald", trials = sp$trials, formula = NULL,
                    multiplier = NULL, add_terms = tm))),
                   error = function(e) NULL)
    if (is.null(f2) || is.null(f2$fit)) return(NULL)
    # drop1() refits through update(), which re-evaluates the fit's stored `data` SYMBOL -- a local of
    # reg_fit() that is long gone by now. multinom / polr keep that symbol, so without this the whole
    # multinomial and ordinal arm silently produced no row (the failure was hidden behind
    # drop1.multinom's own cat() progress until reg_term_tests started capturing it).
    fit2 <- reg_selfheal_call(f2$fit, f2$data)
    # WARNING: the scope must be the FIT's own term label, verbatim -- terms() may re-spell what we
    # pasted, and drop1() then rejects the scope (the trap z8 documented for interactions).
    have <- tryCatch(attr(stats::terms(fit2), "term.labels"), error = function(e) character(0))
    lab  <- have[length(have)]
    if (!length(lab) || is.na(lab)) return(NULL)
    got <- purrr::compact(reg_term_tests(fit2, v, lab, use_f, use_wald, types = types,
                                         col_var = fit_first_col_i, nobs = f2$nobs, row = row))
    if (length(got)) return(got)
    # The shared dispatcher produced nothing: the engine's drop1 method has no p-value (multinomial).
    # Both fits are in hand and nested, so the likelihood ratio between them IS the same test -- but
    # only where a likelihood ratio is valid, so a design fit stops here rather than inventing one.
    if (use_wald || is.null(base_fit)) return(NULL)
    lr <- reg_nested_lr(base_fit, fit2)
    if (is.null(lr)) return(NULL)
    list(row(types[["lr"]], fit_first_col_i, v, lr$stat, lr$df, NA_real_, lr$p, f2$nobs))
  }))
}

# THE producer: the check rows of every fit, appended to the GOF tibble. A sibling of
# reg_compare_rows() / reg_global_rows(), and placed beside them so `data` (the Linearity refit needs
# it) and `specs` are in scope -- reg_gof_tibble() has neither.
#' @keywords internal
reg_check_rows <- function(reg_gof, data, fits, specs, shared, stats, fit_first_col,
                           grouped_by_fit) {
  weighted <- isTRUE(shared$weighted)
  row <- function(test, col_var, predictor, statistic, df1, df2, pvalue, nobs)
    tibble::tibble(row_var = "", col_var = col_var, test = test, term = predictor,
                   statistic = statistic,
                   df1 = df1, df2 = df2, pvalue = pvalue, n = nobs, min_e = NA_real_)
  gof <- function(test, col_var, value, nobs)
    if (is.null(value) || is.na(value)) NULL
    else row(test, col_var, "", value, NA_real_, NA_real_, NA_real_, nobs)

  rows <- purrr::map(seq_along(specs), function(i) {
    sp <- specs[[i]]
    f  <- fits[[i]]
    if (is.null(f)) return(NULL)
    grouped <- isTRUE(grouped_by_fit[[i]])
    keep <- reg_footer_stats(sp$family, weighted, grouped, stats)
    keys <- reg_checks_for(sp$family, weighted, grouped, has_fit = !is.null(f$fit))
    keys <- keys[vapply(keys, function(k) any(names(REG_CHECKS[[k]]$types) %in% keep), logical(1))]
    if (length(keys) == 0L) return(NULL)
    cv  <- fit_first_col[[i]]
    fit <- f$fit
    out <- list()
    if ("linearity" %in% keys && !isTRUE(sp$compound))
      out <- c(out, reg_check_linearity_rows(data, sp, shared, cv, row, base_fit = fit))
    if ("proportionality" %in% keys) {
      bp <- attr(fit, "brant_po")
      if (!is.null(bp) && !is.na(bp))
        out <- c(out, list(row("proportionality", cv, "", NA_real_, NA_real_, NA_real_, bp, f$nobs)))
    }
    if ("dispersion"   %in% keys) out <- c(out, list(gof("dispersion",   cv, reg_check_dispersion(fit),   f$nobs)))
    if ("influence"    %in% keys) out <- c(out, list(gof("influence",    cv, reg_check_influence(fit),    f$nobs)))
    if ("collinearity" %in% keys) out <- c(out, list(gof("collinearity", cv, reg_check_collinearity(fit), f$nobs)))
    out
  })
  rows <- purrr::compact(purrr::flatten(purrr::compact(rows)))
  if (length(rows) == 0) return(reg_gof)
  dplyr::bind_rows(reg_gof, dplyr::bind_rows(rows))
}


# === SECTION: `shape` -- fitting a predictor as something other than a line =========================
#
# The checks FIND a non-linearity; `shape` is how the user FIXES it without leaving the framework.
# Before it, they could not: `predictors = c("race", "poly(age, 2)")` errors, and the formula escape
# hatch silently disables `empirical =`, `color = "adjustment"`, `multiplier` and the per-predictor
# tests.
#
# THE DESIGN RULE, and it is what makes the whole feature ~60 lines: a shape either RECODES THE COLUMN
# or ADDS ONE TERM, and nothing else.
#   * log / sqrt / quantile groups recode `data[[v]]` at ONE boundary in tab_reg(). Every downstream
#     subsystem then works untouched, because the predictor genuinely IS its new self: a quantile-cut
#     `age` is a FACTOR, so it inherits one OR per group, a SATURATED crude twin, per-level N,
#     per-level colours and adjustment gaps with no code at all (SS12.4 -- the sociologist's remedy,
#     and the one this package renders best).
#   * quadratic adds reg_shape_term()'s centred squared term -- the SAME object the Linearity check
#     refits with -- plus one skeleton row. The predictor stays ONE predictor, which is the property
#     every downstream site keys on.
# Nothing here needs a new fmt field, a new column attribute or a new alignment key.
#
# WARNING -- poly() / ns() / bs() are NEVER emitted, and that is a wrong-number refusal, not taste:
# `marginaleffects` returns AME = 0.000000 for them, silently, through every contrast form (the basis
# is re-evaluated on the perturbed data and an orthogonal basis absorbs a location shift exactly).
# I(x^2), raw polynomials and log() are correct through every route.

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

# Phase 18z15 -- the extra model TERM a numeric predictor's non-linear SHAPE emits, with its centre
# and scale frozen as LITERALS in the formula string. Frozen for the reason z9 freezes the multiplier's
# SD: `scale()` inside a formula re-scales on new data, so predict(newdata =) would silently disagree
# with the fit. Returns NULL (never a broken term) when the column cannot supply a finite scale.
#
# ONE builder, two consumers: the Linearity check refits "the model plus this term" (reg_fit(add_terms =))
# and the `shape = "quadratic"` remedy emits the same term -- so the check and its cure are the same
# object rather than two spellings of one idea. Centring is not cosmetic: uncentred, the pair's own VIF
# is 38.7 against 1.2 centred, so the Collinearity check would flag every curved model as broken. It
# leaves the curvature p-value untouched, since {x, (x-a)^2} spans {x, x^2} for any a with 1 and x in
# the model.
#
# WHY THE LINEAR TERM STAYS RAW. eta = a*x + b*((x-m)/s)^2 and eta = A*z + B*z^2 are the same model with
# A = a*s, B = b -- so with `multiplier = "sd"` (the default, which multiplies a numeric coefficient by
# its SD) the printed linear row ALREADY is the per-SD slope of the centred parametrisation. The table
# reads as SS12.3 specifies with no second scaling rule.
#' @keywords internal
reg_shape_term <- function(x, var, shape = "quadratic", w = NULL, digits = 8L) {
  if (!identical(shape, "quadratic")) return(NULL)
  m <- reg_weighted_mean(x, w)
  s <- reg_predictor_sd(x, w)
  if (!is.finite(m) || !is.finite(s) || s <= 0) return(NULL)
  num <- function(v) format(signif(v, digits), scientific = FALSE)
  # WARNING: return the DEPARSED form, not the pasted one. A model-matrix column is named by the
  # formula's own term label, which R produces by deparsing -- and deparse drops the spaces around `/`
  # that a hand-pasted string keeps. Without this the skeleton's `term` misses the fit's by two
  # characters and the curvature row renders EMPTY (measured).
  s2l <- tryCatch(str2lang(paste0("I(((`", var, "` - ", num(m), ") / ", num(s), ")^2)")),
                  error = function(e) NULL)
  if (is.null(s2l)) return(NULL)
  paste(deparse(s2l, width.cutoff = 500L), collapse = "")
}

# Parse the whole `shape` argument -> a named list of list(kind, k). Validated against the data, so
# every refusal names the variable and the value the user wrote.
#' @keywords internal
reg_resolve_shape <- function(shape, data, predictors) {
  if (is.null(shape) || length(shape) == 0L) return(list())
  if (is.null(names(shape)) || !all(nzchar(names(shape)))) {
    cli::cli_abort(c("{.arg shape} must be a NAMED vector over numeric predictors.",
                     "i" = 'e.g. {.code shape = c(age = "quadratic")}.'))
  }
  bad <- setdiff(names(shape), predictors)
  if (length(bad) > 0L) {
    cli::cli_abort(c("{.arg shape} names must be predictors of the model.",
                     "x" = "Not {?a predictor/predictors}: {.val {bad}}."))
  }
  out <- list()
  for (v in names(shape)) {
    if (reg_is_factor_var(data[[v]])) {
      cli::cli_abort(c("{.arg shape} applies to continuous predictors only.",
                       "x" = "{.val {v}} is already {.cls {class(data[[v]])}}."))
    }
    val  <- shape[[v]]
    kind <- if (is.character(val)) trimws(tolower(val)) else val
    k    <- reg_shape_k(kind)
    if (!is.na(k)) { out[[v]] <- list(kind = "quantiles", k = k); next }
    if (!is.character(kind) || length(kind) != 1L || !kind %in% REG_SHAPES) {
      cli::cli_abort(c(
        "{.arg shape} for {.val {v}} must be one of {.or {.val {REG_SHAPES}}}, or a number of groups.",
        "x" = "Got {.val {val}}.",
        "i" = '{.val quintiles} (or an integer) cuts it into quantile groups -- one estimate each.'))
    }
    if (identical(kind, "linear")) next                 # the default, spelled out: nothing to emit
    out[[v]] <- list(kind = kind, k = NA_integer_)
  }
  out
}

# k quantile groups of a continuous column, as an ordinary (unordered) factor. The breaks are WEIGHTED
# quantiles when the call carries weights -- a survey package's "age groups" are equal-share of the
# POPULATION, not of the sample -- with the extremes forced to the observed range so no value falls out.
#' @keywords internal
reg_cut_quantiles <- function(x, k, w = NULL, var = "x") {
  x  <- as.numeric(x)
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
  factor(as.character(f), levels = levels(f))           # unordered: reg_fit de-orders predictors anyway
}

# Apply every column-recoding shape ONCE, and return the display labels the transformed ones need
# ("log(age)"). `quadratic` is not a recode -- it emits a term -- so it passes through untouched.
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
      data[[v]] <- reg_cut_quantiles(x, shapes[[v]]$k, wv, var = v)
    }
  }
  list(data = data, labels = labels)
}

# The quadratic terms a `shape` asks for, named by variable so the skeleton can key its extra row on
# the same string the formula carries. `w` is the WEIGHT COLUMN NAME (as everywhere else in tab_reg),
# resolved here -- the centre and scale are weighted whenever the call is.
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

# The display label of a numeric predictor's squared row: "age²". It is also the skeleton `level`,
# so it must differ from the variable name (the level is the alignment key of every crude / marginal
# join, and `level == var` is what marks the plain linear row).
#' @keywords internal
reg_shape_sq_level <- function(var) paste0(var, "\u00b2")     # U+00B2 SUPERSCRIPT TWO

# The `add_terms` one model contributes: its own predictors' quadratic terms, in predictor order.
# A model COMPARISON is why this filter exists -- a shaped predictor may be in some models and not
# others, and a term for a variable the model does not carry would abort the fit.
#' @keywords internal
reg_shape_add <- function(shape_terms, predictors) {
  if (is.null(shape_terms) || length(shape_terms) == 0L) return(NULL)
  keep <- intersect(predictors, names(shape_terms))
  if (length(keep) == 0L) return(NULL)
  unname(shape_terms[keep])
}


# === SECTION: the plot primitives ===================================================================
#
# Five base-R functions, no dependency, each measured against the reference the design names (SS23).
# They are the ONLY producers of the numbers every panel and the row sparkline draw.
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

# The per-observation outcome a check reads, on the family's own LINK scale, plus the label of that
# scale. ONE dispatch: an ordinal / multinomial outcome has no single curve, so it is read as
# "beyond the first category" -- stated in the axis label, never implied.
#' @keywords internal
rd_link_y <- function(y, family, trials = NULL, positive_level = NULL) {
  if (family == "gaussian")
    return(list(y = as.numeric(y), link = "identity", lab = gettext("mean")))
  if (family %in% c("poisson", "quasipoisson"))
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
# Returns x (weighted bin mean), y (link-scale bin estimate), n (sum of weights) and se (the
# THEORETICAL +/-1 SE of that estimate, from the family's own variance function).
#
# The band is deliberately the theoretical one, 2*sqrt(p(1-p)/n) as ROS SS14.5 p.253 specifies -- NOT
# `arm::binnedplot`'s empirical 2*sd(y)/sqrt(n), which its own book does not describe: measured, they
# agree on average (ratio 0.997) but differ +/-30 % per bin, and the empirical one ignores weights.
# Zero cells use Haldane-Anscombe (k + 0.5)/(n + 1) -- symmetric, never infinite, no arbitrary floor.
# Phase 18z16-iv (W-G.4): the bin's EFFECTIVE base is the package's one device, not a hand-rolled
# Kish -- `ne = num / Var(mean of y in the bin)`, where `num` is what that variance would be times n
# under simple random sampling (p(1-p) for a share, the mean for a count, the within-bin variance for
# a mean). Three inputs, one formula:
#   a survey DESIGN  -> Var comes from svyrecvar (strata, clusters, fpc, calibration all reach the band)
#   weights only     -> the EXACT flat closed form (svy_flat_neff_rows), the ids = ~1 design
#   unweighted       -> `sw` (= n), which is what Kish returns at equal weights, so bands do not move
# A design whose variance cannot be computed for a bin falls through to the flat form, never to a
# wrong number.
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
# "is it a line?" and never "is the effect big?". `style`: TRUE = block glyphs, "ascii" = a plain-text
# ladder for fonts without them (skimr documents the Windows failure), FALSE = no sparkline.
#' @keywords internal
rd_spark_glyphs <- function(style = TRUE) {
  if (identical(style, "ascii")) return(c(".", ",", "-", "~", "+", "=", "*", "#"))
  # U+2581..U+2588 (lower one-eighth block .. full block), as escapes: the source stays ASCII.
  c("\u2581", "\u2582", "\u2583", "\u2584", "\u2585", "\u2586", "\u2587", "\u2588")
}

# Remove a glyph run (and the non-breaking space that ties it to its label) from a rendered string.
# THE plot medium's answer to SS17's font trap: a graphics device substitutes its own font and has no
# block glyphs, so grid emits one "conversion failure in mbcsToSbcs" per label and draws garbage. The
# console, markdown, Excel and the html <svg> all keep it; a ggplot never does.
#' @keywords internal
tx_spark_strip <- function(x) {
  gl <- paste(rd_spark_glyphs(TRUE), collapse = "")
  gsub(paste0("\u00a0?[", gl, "]{3,}"), "", x)
}

#' @keywords internal
rd_spark <- function(y, style = TRUE) {
  if (isFALSE(style) || is.null(y) || length(y) < 3L || !all(is.finite(y))) return(NA_character_)
  r <- range(y)
  gl <- rd_spark_glyphs(style)
  i  <- if (diff(r) <= 0) rep(ceiling(length(gl) / 2), length(y))
        else 1L + floor((y - r[[1L]]) / diff(r) * (length(gl) - 1e-9))
  paste(gl[pmax(pmin(i, length(gl)), 1L)], collapse = "")
}

# ONE residual per family, for the teaching panels. A raw residual takes exactly two values given
# p-hat for a binary outcome (ROS SS14.5), so every non-gaussian family gets the RANDOMISED QUANTILE
# residual (Dunn & Smyth 1996), which is standard normal under a correct model whatever the family --
# including ordinal, whose fitted() matrix gives cumulative probabilities exactly as ppois() does for a
# count. Multinomial is REFUSED: two level orderings give residuals correlated -0.705, so every plot
# would be an artefact of the coding.
#
# WARNING: qnorm(1) = Inf -- u must be clamped, or a single saturated fitted value returns Inf.
#' @keywords internal
rd_resid <- function(fit, family, y, trials = NULL, seed = 20260810) {
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
    } else if (family %in% c("poisson", "quasipoisson")) {
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
# band is qnorm(qbeta(alpha/2, i, n-i+1)) .. qnorm(qbeta(1-alpha/2, ...)). 28 ms for every point
# against 1182 ms for a 19-replicate simulated envelope, agreeing to 0.19 on the most extreme one.
#
# WARNING: it is POINTWISE, not simultaneous -- under a true model ~5 % of points fall outside AT EACH
# POSITION. The panel subtitle says so; the docs alone would not be enough.
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

# Evaluate under a fixed seed and give the caller its RNG stream back. `seed = NULL` is a fresh draw --
# the honest way to check that a pattern in a randomised residual is not a randomisation artefact.
# Base R rather than withr::with_seed(): withr is Suggests-only, and these primitives have no dependency.
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
# the predictor, on the family's own link scale. ~1.6 KB each, computed ONCE per predictor -- never per
# model, never per rendering -- because it contains no fit: a 5-model comparison stores five references
# to one tibble, and it survives the jamovi digest path, where no fit exists.
#
# WITH SEVERAL OUTCOMES there is no single observed shape, so the whole thing is NULL rather than the
# first outcome's silently: a row label is shared by every model column, and a sparkline that described
# only one of them would be a lie the reader cannot see.
#' @keywords internal
reg_curves <- function(data, specs, numeric_preds, wt = NULL, positive_level = NULL, nbins = 10L,
                       design = NULL) {
  if (length(numeric_preds) == 0L || length(specs) == 0L) return(NULL)
  deps <- unique(vapply(specs, function(s) s$dependent, character(1)))
  if (length(deps) != 1L) return(NULL)
  sp <- specs[[1L]]
  if (isTRUE(sp$compound) || is.null(data[[deps]])) return(NULL)
  # WARNING: the MODELLED level, taken from the fit, never the factor's first level. `Married` before
  # `Not married` in the data is exactly the case inverse_two_level_factors exists for, and reading the
  # level order here instead drew the curve of the COMPLEMENT -- an upside-down sparkline beside a
  # correct odds ratio, which is worse than none.
  ly <- rd_link_y(data[[deps]], sp$family, sp$trials, positive_level)
  w  <- if (!is.null(wt) && is.character(wt) && length(wt) == 1L && wt %in% names(data))
          data[[wt]] else NULL
  # Phase 18z16-iv (W-G.4): under a survey design the bands take the DESIGN variance, reached
  # through `.svy_row` -- the position each prepared row holds in the original design -- exactly as
  # every other design quantity in the package is.
  dr <- if (!is.null(design)) data[[svy_row_col]] else NULL
  curves <- purrr::compact(stats::setNames(
    purrr::map(numeric_preds, function(v)
      rd_bin(data[[v]], ly$y, w, nbins, ly$link, design = design, des_rows = dr)),
    numeric_preds))
  if (length(curves) == 0L) return(NULL)
  list(dependent = deps, family = sp$family, link = ly$link, ylab = ly$lab, curves = curves)
}


# === SECTION: potools extraction anchor =============================================================

# The check nouns and instruments are gettext()'d DYNAMICALLY (gettext(ck$noun)), which potools cannot
# see. This dead branch makes every msgid literal exactly once, so `Rscript dev/update_translations.R`
# extracts them; nothing here ever runs. Same device as legend_measure_word()'s anchor.
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
