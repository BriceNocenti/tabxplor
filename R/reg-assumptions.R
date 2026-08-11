# Last Phase z15 -- THE model checks of a `tab_reg()` table.
#
# ROLE: one fact table (REG_CHECKS), one selection rule (reg_checks_for), one producer
# (reg_check_rows) and one label builder (reg_check_spec_entries). Adding a check is ONE row: the
# footer label, the `stats =` value and (later) the panel title all derive from it, so they cannot
# drift -- the REG_EMPIRICAL / reg_crude_shape pattern of Phase 17h.
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
#                 convention Last Phase m set for the crosstab summary ("pvalue (Chi2, Welch F; Kish)").
#                 A term test carries three discriminators because exactly one of LR / F / Wald fires,
#                 and which one is a fact about the model the reader should see.
#   kind/digits   the reg_footer_spec() rendering (a p-value cell, or a gof number with `digits`)
#   families      where the check is defined at all
#   weighted_ok   FALSE = refused on a weighted / design fit (never approximated)
#   per_predictor one row per (model column x predictor) rather than one per model column
#' @keywords internal
REG_CHECKS <- list(
  # 1. the ESTIMATE: is this predictor's effect really one straight line?
  linearity = list(
    noun = "Linearity",
    types = c(linearity_lr = "LR", linearity_f = "F", linearity_wald = "Wald"),
    kind = "pvalue", digits = NA_integer_,
    families = REG_CHECK_FAMILIES, weighted_ok = TRUE, per_predictor = TRUE),
  # 2. what the estimate MEANS: is one odds ratio enough for every cut?
  proportionality = list(
    noun = "Proportionality",
    types = c(proportionality = "Brant"),
    kind = "pvalue", digits = NA_integer_,
    families = "ordinal", weighted_ok = FALSE, per_predictor = FALSE),
  # 3. the INTERVAL: are the standard errors wide enough?
  dispersion = list(
    noun = "Dispersion",
    types = c(dispersion = "robust/model SE"),
    kind = "gof", digits = 2L,
    families = REG_CHECK_FAMILIES, weighted_ok = TRUE, per_predictor = FALSE),
  # 4. is it REAL: does one respondent carry the result?
  influence = list(
    noun = "Influence",
    types = c(influence = "max dfbetas"),
    kind = "gof", digits = 2L,
    families = REG_CHECK_FAMILIES, weighted_ok = TRUE, per_predictor = FALSE),
  # 5. why is it WIDE: can the data tell these predictors apart?
  collinearity = list(
    noun = "Collinearity",
    types = c(collinearity = "max VIF"),
    kind = "gof", digits = 2L,
    families = setdiff(REG_CHECK_FAMILIES, "multinomial"), weighted_ok = TRUE,
    per_predictor = FALSE)
)

# Every discriminator the checks can emit (the `test` values that are check rows).
#' @keywords internal
reg_check_types <- function() unlist(lapply(REG_CHECKS, function(ck) names(ck$types)),
                                     use.names = FALSE)

# THE selection rule: which checks apply to this fit? Read by reg_footer_stats() (the default set +
# the `stats =` vocabulary) and by reg_check_rows() (what to compute). `has_fit` is FALSE on the
# jamovi digest path, which deliberately keeps no model frame -- every check reads the fit, so they
# degrade to absent there rather than to a wrong number.
#' @keywords internal
reg_checks_for <- function(family, weighted = FALSE, grouped = FALSE, has_fit = TRUE) {
  if (!isTRUE(has_fit)) return(character(0))
  keys <- names(REG_CHECKS)
  keys[vapply(keys, function(k) {
    ck <- REG_CHECKS[[k]]
    family %in% ck$families && (isTRUE(ck$weighted_ok) || !isTRUE(weighted))
  }, logical(1))]
}

# A `stats =` value the user writes is a check KEY ("linearity"); a `test` row carries a
# DISCRIMINATOR ("linearity_lr"). One expansion, so both vocabularies stay in this file.
#' @keywords internal
reg_check_expand <- function(stats) {
  unlist(lapply(stats, function(s)
    if (!is.null(REG_CHECKS[[s]])) names(REG_CHECKS[[s]]$types) else s), use.names = FALSE)
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
  num <- reg_numeric_preds(data, sp$predictors)
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


# === SECTION: potools extraction anchor =============================================================

# The check nouns and instruments are gettext()'d DYNAMICALLY (gettext(ck$noun)), which potools cannot
# see. This dead branch makes every msgid literal exactly once, so `Rscript dev/update_translations.R`
# extracts them; nothing here ever runs. Same device as legend_measure_word()'s anchor.
#' @keywords internal
reg_check_msgid_anchor <- function() {
  if (FALSE) c(
    gettext("Linearity"), gettext("Proportionality"), gettext("Dispersion"),
    gettext("Influence"), gettext("Collinearity"), gettext("Overall association"),
    gettext("Pearson dispersion"),
    gettext("LR"), gettext("F"), gettext("Wald"), gettext("Brant"),
    gettext("robust/model SE"), gettext("max dfbetas"), gettext("max VIF"), gettext("phi")
  )
  invisible(NULL)
}
