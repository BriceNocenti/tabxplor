# PURPOSE: `tabxplor_fitdigest` -- the fit-free record of a fitted model, and the model frame's own
#   recipe.
# ROLE: reg_fit() distils every fit into one. The marginal engines (R/reg-influence.R), the influence
#   functions, the column builders and the jamovi cache read the DIGEST; the fitted object exists
#   only for the length of one build, and only the checks and the term tests that genuinely refit
#   still touch it. What a digest cannot answer needs a refit, and reg_digest_revive() is the one
#   route back.
# KEY CONSTRAINTS:
#   - THE TWO TABLES ARE THE CONTRACT. REG_FIT_KINDS declares one fitting backend, REG_DIGEST_PARTS
#     one stored part; reg_digest() is a loop over them. A NEW MODEL BACKEND IS ONE ROW -- plus a
#     part row for anything it carries that no other kind does. zzz-fact-keys.R checks the edges at
#     load, so a row naming a key that does not exist fails the install.
#   - A DIGEST HOLDS NOTHING LENGTH-n. The model frame is rebuilt from the live data by
#     reg_digest_frame(), through the SAME reg_fit_frame() the fitter uses -- one prep, no drift.
#   - ⚠ A terms object CARRIES ITS ENVIRONMENT, and reg_svyglm_env() deliberately binds
#     survey::svyglm into it -- so reg_digest_terms() rebases it to baseenv(), or a digest drags the
#     whole fitting scope across the cache and the process boundary.
#   - ⚠ A PART A KIND DOES NOT DECLARE IS ABSENT, and a consumer handed one it was not given gets
#     NULL -- the refuse-rather-than-guess contract reg_gcomp_maker() already keeps.
#   - ONE CACHE SEAM, TWO CALLERS. reg_fit_cached() is what reg_spec_build_one() (the model fit) and
#     reg_empirical_fit() (each observed/crude one) both go through, so a record cannot be fetched,
#     distilled or rehydrated two ways. Its gate is two predicates because its two clauses are about
#     two different things: `profile` refuses ANY fit, a model comparison only the model ones.
#   - ⚠ WHAT IS IN THE KEY IS WHAT CAN MOVE A FIT, and nothing else. The estimand left it in Phase
#     22j and `multiplier` in 22i: both are REPORTING choices applied by reg_tidy_finalize(), which
#     the seam calls on the way out -- so each of them is a hit that re-reports, never a refit.
#   - ⚠ this file sorts BEFORE R/reg-empirical.R, R/reg-influence.R, R/reg-spec-build.R and
#     R/tab_reg.R, so no top-level code here may read one of their objects (function bodies run
#     after the namespace is built, and are fine).
# See: CLAUDE.md section "tabxplor architecture" (the regression subsystem).


# === SECTION: THE TWO FACT TABLES ================================================================

# One row per FITTING BACKEND, in DISPATCH order (most specific class first: svyglm inherits glm
# inherits lm). Each row says what the backend is, not what any caller wants of it:
#   equations  "single" (one linear predictor) / "categorical" (one per outcome category)
#   score      which influence engine reads it -- NA = it exposes no per-observation score, so the
#              gap test is refused rather than approximated
#   parts      the REG_DIGEST_PARTS rows this kind carries beyond the ones declared for "all"
#   revive     can reg_digest_revive() refit it from the recipe?
#' @keywords internal
#' @noRd
REG_FIT_KINDS <- list(
  # ⚠ svy_vglm exposes no terms(), no family() and no model.matrix(): reg_prob_engine() has always
  # returned NULL for it. The digest states that as a fact rather than inventing a terms object.
  svy_vglm = list(classes = "svy_vglm", equations = "categorical", score = NA_character_,
                  parts = character(0), revive = TRUE),
  svyolr   = list(classes = "svyolr",   equations = "categorical", score = NA_character_,
                  parts = c("zeta", "y_levels"), revive = TRUE),
  polr     = list(classes = "polr",     equations = "categorical", score = "polr",
                  parts = c("zeta", "y_levels"), revive = TRUE),
  multinom = list(classes = "multinom", equations = "categorical", score = "multinom",
                  parts = "y_levels", revive = TRUE),
  svyglm   = list(classes = "svyglm",   equations = "single",      score = "irls",
                  parts = c("family", "design_n"), revive = TRUE),
  glm      = list(classes = "glm",      equations = "single",      score = "irls",
                  parts = "family", revive = TRUE),
  lm       = list(classes = "lm",       equations = "single",      score = "irls",
                  parts = "family", revive = TRUE)
)

REG_SCORE_ENGINES <- c("irls", "multinom", "polr")

# One row per STORED PART: what it is, which kinds carry it, and the consumer that reads it. `kinds`
# = "all" means every kind that has a terms() method (svy_vglm has none, and takes only the core
# scalars). `extract` is a closure over the fit; it returns NULL where the fit cannot supply it.
#' @keywords internal
#' @noRd
REG_DIGEST_PARTS <- list(
  terms = list(
    kinds = "all", why = "every model matrix and every counterfactual sweep",
    extract = function(fit, rec) reg_digest_terms(fit)),
  xlevels = list(
    kinds = "all", why = "model.frame(xlev =) must reproduce the fit's own factor coding",
    extract = function(fit, rec) reg_digest_xlevels(fit)),
  contrasts = list(
    kinds = "all", why = "model.matrix(contrasts.arg =), the other half of that coding",
    extract = function(fit, rec) tryCatch(fit$contrasts, error = function(e) NULL)),
  coef = list(
    kinds = "all", why = "coef(); a multinomial's is the (K-1) x p MATRIX, verbatim",
    extract = function(fit, rec) tryCatch(stats::coef(fit), error = function(e) NULL)),
  vcov = list(
    kinds = "all", why = "vcov(); on an svyglm / svyolr it is already the design-based sandwich",
    extract = function(fit, rec) reg_digest_vcov(fit)),
  nobs = list(
    kinds = "all", why = "nobs(), and the assertion that a rebuilt frame is the fitted one",
    extract = function(fit, rec) tryCatch(as.integer(stats::nobs(fit)), error = function(e) NA_integer_)),
  df_residual = list(
    kinds = "all", why = "df.residual(): the reference distribution every interval refers to",
    extract = function(fit, rec) reg_df_residual(fit)),
  family = list(
    kinds = c("lm", "glm", "svyglm"), why = "family()$linkinv / $mu.eta / $variance: g-computation and the IRLS weights",
    extract = function(fit, rec) reg_digest_family_spec(fit)),
  zeta = list(
    kinds = c("polr", "svyolr"), why = "the cumulative-logit cut points, part of the parameter vector",
    extract = function(fit, rec) fit$zeta),
  y_levels = list(
    kinds = c("multinom", "polr", "svyolr"), why = "reg_prob_engine()'s level set, replacing predict() / model.frame()",
    extract = function(fit, rec) rec$y_levels %||% reg_fit_y_levels(fit)),
  design_n = list(
    kinds = "svyglm", why = "reg_if_align(): `[` does not drop rows on a calibrated or PPS design",
    extract = function(fit, rec) reg_digest_design_n(fit))
)

# The table's own consistency, at build time (the REG_CHECKS / fmt_attr_rules idiom).
stopifnot(
  all(vapply(REG_FIT_KINDS, function(k)
    isTRUE(k$equations %in% c("single", "categorical")), logical(1))),
  all(vapply(REG_FIT_KINDS, function(k)
    is.na(k$score) || k$score %in% REG_SCORE_ENGINES, logical(1))),
  all(vapply(REG_DIGEST_PARTS, function(p)
    identical(p$kinds, "all") || all(p$kinds %in% names(REG_FIT_KINDS)), logical(1))),
  all(vapply(REG_FIT_KINDS, function(k)
    all(k$parts %in% names(REG_DIGEST_PARTS)), logical(1))),
  all(vapply(REG_DIGEST_PARTS, function(p) is.function(p$extract) && nzchar(p$why), logical(1)))
)

# Which kinds a part belongs to, resolved once ("all" = every kind that declares it or has terms()).
#' @keywords internal
#' @noRd
reg_digest_part_kinds <- function(name) {
  p <- REG_DIGEST_PARTS[[name]]
  if (!identical(p$kinds, "all")) return(p$kinds)
  setdiff(names(REG_FIT_KINDS), "svy_vglm")
}

# The kind of a fitted object, by the declared class list, in the table's own order.
#' @keywords internal
#' @noRd
reg_fit_kind <- function(fit) {
  for (k in names(REG_FIT_KINDS)) if (inherits(fit, REG_FIT_KINDS[[k]]$classes)) return(k)
  NA_character_
}

# THE kind of any model, fitted or distilled -- so no dispatch anywhere reads `inherits(fit, ...)`
# and silently takes the wrong arm when handed a digest.
#' @keywords internal
#' @noRd
reg_model_kind <- function(x) if (is_reg_digest(x)) x$kind else reg_fit_kind(x)

# Does this model have ONE EQUATION PER OUTCOME CATEGORY? The declared read of REG_FIT_KINDS'
# `equations`, and the one predicate that fans a marginal sweep out over the categories.
#' @keywords internal
#' @noRd
reg_model_categorical <- function(x) {
  k <- reg_model_kind(x)
  !is.na(k) && identical(REG_FIT_KINDS[[k]]$equations, "categorical")
}


# === SECTION: THE EXTRACTORS =====================================================================

# ⚠ THE ENVIRONMENT IS THE POINT: terms() carries one, and reg_svyglm_env() binds survey::svyglm
# into it on purpose -- so a stored terms object would drag the fitting scope (megabytes) into the
# cache. baseenv() is safe because a terms object is only ever re-evaluated against a data frame.
#' @keywords internal
#' @noRd
reg_digest_terms <- function(fit) {
  tt <- tryCatch(stats::terms(fit), error = function(e) NULL)
  if (is.null(tt)) return(NULL)
  attr(tt, ".Environment") <- baseenv()
  tt
}

# The fit's OWN observed levels, which are not the data's: lm / glm build their frame with
# drop.unused.levels = TRUE, so an unused level is absent from the coding and must stay absent.
#' @keywords internal
#' @noRd
reg_digest_xlevels <- function(fit) {
  xl <- tryCatch(fit$xlevels, error = function(e) NULL)
  if (!is.null(xl)) return(xl)
  tryCatch(stats::.getXlevels(stats::terms(fit), stats::model.frame(fit)), error = function(e) NULL)
}

# ⚠ A FAMILY OBJECT IS NOT SMALL: its closures carry the environment they were built in, and a
# fitted glm's serialises at ~10 MB where a fresh binomial("logit") is 15 KB -- which would defeat
# the whole point of a digest. A family is fully determined by its NAME and its LINK, so that pair
# is what is stored and stats rebuilds it on read; an object no stats generator can rebuild is kept
# verbatim rather than approximated.
#' @keywords internal
#' @noRd
reg_digest_family_spec <- function(fit) {
  fam <- tryCatch(stats::family(fit), error = function(e) NULL)
  if (is.null(fam)) return(NULL)
  gen <- tryCatch(get(fam$family, envir = asNamespace("stats"), mode = "function"),
                  error = function(e) NULL)
  ok  <- !is.null(gen) &&
    isTRUE(tryCatch(identical(gen(fam$link)$family, fam$family), error = function(e) FALSE))
  list(name = fam$family, link = fam$link, obj = if (ok) NULL else fam)
}

#' @keywords internal
#' @noRd
reg_digest_family <- function(spec) {
  if (is.null(spec)) return(NULL)
  if (!is.null(spec$obj)) return(spec$obj)
  tryCatch(get(spec$name, envir = asNamespace("stats"), mode = "function")(spec$link),
           error = function(e) NULL)
}

# svy_vglm stores $var rather than answering vcov(); every other backend answers it.
#' @keywords internal
#' @noRd
reg_digest_vcov <- function(fit) {
  v <- tryCatch(stats::vcov(fit), error = function(e) NULL)
  if (is.null(v) && !is.null(fit$var)) v <- fit$var
  v
}

# THE OUTCOME'S LEVEL SET, read off the fit where no recipe supplies it (a digest taken of a
# stand-alone fit). It is the ONE thing reg_prob_engine() used to reach into a fitted object for --
# once, here, while the object still exists.
#' @keywords internal
#' @noRd
reg_fit_y_levels <- function(fit) {
  if (inherits(fit, "multinom")) {
    B <- tryCatch(stats::coef(fit), error = function(e) NULL)
    P <- tryCatch(colnames(stats::predict(fit, type = "probs")), error = function(e) NULL)
    if (!is.matrix(B) || is.null(P)) return(NULL)
    # the reference is the level with no coefficients, and it comes first
    return(c(setdiff(P, rownames(B)), rownames(B)))
  }
  tryCatch(levels(stats::model.frame(fit)[[1L]]), error = function(e) NULL)
}

# The DESIGN's row count, which is not the frame's on a calibrated or PPS design (survey keeps every
# row at prob = Inf). reg_coef_if_maker() pads its influence vector back up to it.
#' @keywords internal
#' @noRd
reg_digest_design_n <- function(fit) {
  des <- tryCatch(fit$survey.design, error = function(e) NULL)
  if (is.null(des) || is.null(des$variables)) return(NA_integer_)
  as.integer(nrow(des$variables))
}


# === SECTION: THE CONSTRUCTOR ====================================================================

# `rec` is the REFIT RECIPE: reg_fit()'s own arguments, a few strings, plus the two facts the frame
# prep resolved (`positive_level`, `grouped`) and the outcome's level set. It is what rebuilds the
# frame and what revives the fit, so a digest is self-sufficient given the data.
#' @keywords internal
#' @noRd
new_reg_recipe <- function(outcome = NA_character_, predictors = character(0), family = NA_character_,
                           outcome_level = NULL, trials = NULL, formula = NULL, cross = NULL,
                           drop_extra = NULL, add_terms = NULL, design_spec = NULL,
                           conf_level = 0.95, method = "wald", multiplier = NULL,
                           y_levels = NULL, positive_level = NULL, grouped = FALSE,
                           drop_vars = character(0)) {
  as.list(environment())
}

#' @keywords internal
#' @noRd
reg_digest <- function(fit, rec = new_reg_recipe()) {
  if (is_reg_digest(fit)) return(fit)
  kind <- reg_fit_kind(fit)
  if (is.na(kind)) return(NULL)
  want <- names(REG_DIGEST_PARTS)[vapply(names(REG_DIGEST_PARTS),
                                         function(p) kind %in% reg_digest_part_kinds(p), logical(1))]
  out <- lapply(REG_DIGEST_PARTS[want], function(p) p$extract(fit, rec))
  names(out) <- want
  out$kind   <- kind
  out$recipe <- rec
  # the PARTS STAMP: a store serialized by an older build carries another set, and is discarded
  # rather than read with the wrong fields (the JMVREG_CACHE_SCHEMA idiom, applied to the digest).
  out$parts  <- want
  structure(out, class = "tabxplor_fitdigest")
}

#' @keywords internal
#' @noRd
is_reg_digest <- function(x) inherits(x, "tabxplor_fitdigest")

# Everything a marginal engine may be handed: a live fit while one exists, else the digest. ONE
# expression, so no call site grows an `if`.
#' @keywords internal
#' @noRd
# ⚠ THE DIGEST WINS where both exist: it is the one that carries the recipe, so the numbers a build
# computes do not depend on whether its fit happened to still be around.
reg_model_of <- function(f) f$digest %||% f$fit

# The DESIGN the fit was built on -- the one reg_if_se() must refer a gap SE to. The live object
# while one exists (identical, and free), else the one reg_fit_rehydrate() rebuilt from the recipe:
# a shortcut, not a second answer.
#' @keywords internal
#' @noRd
reg_model_design <- function(f) {
  d <- tryCatch(f$fit$survey.design, error = function(e) NULL)
  d %||% f$design
}

# CAN A RECORD BE SERVED FROM A STORE? Two clauses, each naming one thing a distilled record cannot
# carry -- and a wrong TRUE here is a missing footer row, never a wrong number, because everything
# else is recomputed from the digest.
#   profile     the bounds are an OUTPUT of the likelihood at one confidence level, so they are the
#               one quantity not rebuildable from (estimate, std.error). TRUE OF ANY FIT.
#   comparison  an LR / F test between models is a test between the FIT OBJECTS. A fact about the
#               MODEL fits alone: a CRUDE fit is univariable and takes part in no comparison, which
#               is why the two clauses are two functions and the profile one keeps a single home.
#' @keywords internal
#' @noRd
reg_crude_cacheable <- function(method) !identical(method, "profile")

#' @keywords internal
#' @noRd
reg_fit_cacheable <- function(sp, method, compare = "none")
  reg_crude_cacheable(method) && identical(compare, "none")

# reg_fit_cached() -- THE ONE CACHE SEAM, shared by the model path (reg_spec_build_one) and the
# crude one (reg_empirical_fit). `key = NULL` means "this fit is not cacheable"; `fit_cache = NULL`
# means "there is no store", and both fall through to the thunk.
# ⚠ A RECORD WHOSE FRAME COULD NOT BE REBUILT IS RECOMPUTED, NEVER SERVED: reg_digest_frame()
# returning NULL is a refusal (the fit's domain moved under the key), and a NULL frame reads
# downstream as "no gap SE" / "no marginal sweep" rather than as an error.
#' @keywords internal
#' @noRd
reg_fit_cached <- function(fit_cache, key, thunk, data, do_exp, conf_level, multiplier = NULL) {
  f <- if (is.null(fit_cache) || is.null(key)) thunk()
       else jmvreg_cached(fit_cache, "fit", key, function() reg_fit_distil(thunk()))
  f <- reg_fit_rehydrate(f, data, do_exp, conf_level, multiplier)
  if (is.null(f$data)) thunk() else f
}

# reg_fit_distil() -- the cache boundary: what the store holds. The fitted object and the frame go,
# the digest and everything the eager stage computed off the fit stay. `tidy` goes too -- it is the
# one estimand-dependent member, and reg_tidy_finalize() rewrites it on the way out -- with the
# `multiplier` scaling, which is likewise a reporting choice and so likewise out of the KEY.
#' @keywords internal
#' @noRd
reg_fit_distil <- function(f) {
  # ⚠ `f["x"] <- list(NULL)`, never `f$x <- NULL`: the latter REMOVES the name, and `$` then
  # PARTIAL-MATCHES -- with `tidy` gone, `f$tidy` would silently return `tidy_native`.
  f[c("fit", "data", "design", "tidy")] <- list(NULL)
  f
}

# reg_fit_rehydrate() -- the other side: give a distilled record back its frame (rebuilt, never
# cached), the design that frame implies, and this build's own `tidy`. Idempotent, and a no-op on a
# record that still has its fit.
#' @keywords internal
#' @noRd
reg_fit_rehydrate <- function(f, data, do_exp, conf_level, multiplier = NULL) {
  if (is.null(f$data)) f$data <- reg_digest_frame(f$digest, data)
  if (is.null(f$fit) && is.null(f$design))
    f$design <- reg_digest_design(f$digest, f$data, data)
  f$tidy <- reg_tidy_finalize(f, do_exp, conf_level, multiplier)
  f
}

# The survey design a distilled svyglm was fitted on, rebuilt through reg_resolve_design() -- the
# same call make_design() made, on the same inputs, so it is the same design.
#' @keywords internal
#' @noRd
reg_digest_design <- function(digest, frame, data) {
  if (is.null(digest) || !identical(digest$kind, "svyglm") || is.null(frame)) return(NULL)
  ds <- digest$recipe$design_spec
  if (is.null(ds)) return(NULL)
  tryCatch(reg_resolve_design(ds, frame, data, digest$recipe$drop_vars), error = function(e) NULL)
}


# === SECTION: THE S3 SURFACE =====================================================================
#
# A digest answers the generics a fitted object answers, so reg_gcomp_maker(), reg_gcomp_cat_maker(),
# reg_prob_engine(), reg_gcomp_baseline(), reg_delta_se() and reg_basis_vars() read one without
# knowing it is not a fit. Anything a digest does not carry errors exactly as an unsupported fit
# does, which is what those functions' tryCatch()es already expect.

#' @importFrom stats coef
#' @export
coef.tabxplor_fitdigest <- function(object, ...) object$coef

#' @importFrom stats vcov
#' @export
vcov.tabxplor_fitdigest <- function(object, ...) object$vcov

#' @importFrom stats terms
#' @export
terms.tabxplor_fitdigest <- function(x, ...) {
  if (is.null(x$terms)) stop("this fit digest carries no terms", call. = FALSE)
  x$terms
}

#' @importFrom stats formula
#' @export
formula.tabxplor_fitdigest <- function(x, ...) stats::formula(stats::terms(x))

#' @importFrom stats family
#' @export
family.tabxplor_fitdigest <- function(object, ...) {
  fam <- reg_digest_family(object$family)
  if (is.null(fam)) stop("this fit digest carries no family", call. = FALSE)
  fam
}

#' @importFrom stats nobs
#' @export
nobs.tabxplor_fitdigest <- function(object, ...) object$nobs

#' @importFrom stats df.residual
#' @export
df.residual.tabxplor_fitdigest <- function(object, ...) object$df_residual

#' @export
print.tabxplor_fitdigest <- function(x, ...) {
  cat("<tabxplor_fitdigest>", x$kind, "-", x$recipe$outcome,
      "-", format(x$nobs), "obs\n")
  cat("parts:", paste(x$parts, collapse = ", "), "\n")
  invisible(x)
}


# === SECTION: THE FRAME, REBUILT =================================================================

# reg_digest_frame() -- the model frame a digest was fitted on, rebuilt from the live data through
# the SAME reg_fit_frame() the fitter used. Nothing length-n is ever cached because of this.
#
# ⚠ `data` must be the PREPARED frame the fit saw (post `ref` anchor shift, post relevel), not the
# user's raw one. The assertion below is what turns a mistake into a refusal: a rebuilt frame of the
# wrong size means the domain moved, and every number computed on it would be silently wrong.
#' @keywords internal
#' @noRd
reg_digest_frame <- function(digest, data) {
  if (is.null(digest) || is.null(data)) return(NULL)
  rec <- digest$recipe
  fr  <- tryCatch(reg_fit_frame(
    data, rec$outcome, rec$predictors, rec$family, rec$design_spec,
    outcome_level = rec$outcome_level, trials = rec$trials, formula = rec$formula,
    cross = rec$cross, drop_extra = rec$drop_extra, add_terms = rec$add_terms,
    quiet = TRUE), error = function(e) NULL)
  if (is.null(fr)) return(NULL)
  if (!is.na(digest$nobs) && !identical(as.integer(nrow(fr$frame)), as.integer(digest$nobs)))
    return(NULL)
  fr$frame
}

# reg_digest_revive() -- back to a real fit, for the two things no digest can answer: an
# `at_reference` profile (marginaleffects works on the fitted object) and a compound formula's
# skeleton. It goes through reg_fit() itself, so a revived fit cannot differ from the original.
#' @keywords internal
#' @noRd
reg_digest_revive <- function(f, data) {
  if (!is.null(f$fit)) return(f)
  rec <- f$digest$recipe
  if (is.null(rec) || !isTRUE(REG_FIT_KINDS[[f$digest$kind]]$revive)) return(f)
  g <- tryCatch(suppressMessages(reg_fit(
    data, rec$outcome, rec$predictors, rec$family, rec$design_spec, FALSE,
    rec$outcome_level, rec$conf_level, rec$method,
    trials = rec$trials, formula = rec$formula, multiplier = rec$multiplier,
    cross = rec$cross, drop_extra = rec$drop_extra, add_terms = rec$add_terms)),
    error = function(e) NULL)
  if (is.null(g)) return(f)
  f$fit  <- g$fit
  f$data <- g$data
  f
}


# === SECTION: THE WORKING PARTS -- an IRLS fit's influence, without the fit ========================
#
# reg_coef_if_maker() used to read `model.matrix(fit)`, `fit$weights` and
# `residuals(fit, type = "working")`. All three are functions of (terms, coef, family, frame):
#
#   eta = X b + offset      mu = linkinv(eta)      r = (y - mu) / mu.eta(eta)
#   W   = prior_w * mu.eta(eta)^2 / variance(mu)
#
# ⚠ `r` and `eta` come back EXACT; `W` differs at ~1e-8 relative, because glm.fit stores the weights
# of the last IRLS update, evaluated at the PREVIOUS eta -- a lag by construction, not a tolerance
# to tighten. The reconstruction is the value at the converged coefficients.
# ⚠ A GLOBAL scaling of W cancels in reg_if_from_parts() (U scales by c, A^-1 by 1/c), which is why
# svyglm's own rescaling of the design weights need not be reproduced.

# The model frame, the model matrix and the response, in the fit's OWN coding. The `xlev` /
# `contrasts.arg` pair is what makes the columns reproduce: a level unused in the fit would otherwise
# add an all-zero column and every consumer would refuse on `ncol(X) != length(b)`.
#' @keywords internal
#' @noRd
# `response = FALSE` for a COUNTERFACTUAL or PROFILE frame, which carries predictors only.
reg_digest_mm <- function(digest, frame, response = TRUE) {
  tt0 <- tryCatch(stats::terms(digest), error = function(e) NULL)
  if (is.null(tt0) || is.null(frame)) return(NULL)
  tt  <- if (response) tt0 else stats::delete.response(tt0)
  mf  <- tryCatch(stats::model.frame(tt, frame, xlev = digest$xlevels), error = function(e) NULL)
  if (is.null(mf) || !nrow(mf)) return(NULL)
  X   <- tryCatch(stats::model.matrix(stats::delete.response(tt0), mf,
                                      contrasts.arg = digest$contrasts), error = function(e) NULL)
  if (is.null(X)) return(NULL)
  list(mf = mf, X = X, y = if (response) stats::model.response(mf) else NULL,
       offset = stats::model.offset(mf) %||% rep(0, nrow(X)))
}

# ⚠ THE ROW SPACE AN INFLUENCE VECTOR MUST LIVE IN is the DESIGN's, not the frame's: `[` does not
# drop rows on a calibrated or PPS design, so a leg built on the complete-case frame is shorter than
# the design reg_if_se() hands it to. Scattering with zeros is exact -- the padded rows carry design
# weight 0. A no-op everywhere else.
#' @keywords internal
#' @noRd
reg_digest_pad <- function(digest, frame) {
  n <- digest$design_n
  if (is.null(n) || is.na(n) || identical(as.integer(n), as.integer(nrow(frame))))
    return(function(v) v)
  rows <- frame[[svy_row_col]]
  function(v) reg_if_align(v, as.integer(n), rows)
}

# The response and the prior weights, taken through the FAMILY'S OWN `initialize` expression -- the
# only route that turns a `cbind(succ, fail)` response into (y = succ/n, prior = base * n) without
# re-deriving the rule glm() already owns.
#' @keywords internal
#' @noRd
reg_digest_response <- function(digest, mf, base_w) {
  fam <- tryCatch(stats::family(digest), error = function(e) NULL)
  y0  <- stats::model.response(mf)
  if (is.null(fam) || is.null(y0) || is.null(fam$initialize)) return(NULL)
  n0  <- NROW(y0)
  # ⚠ `family` must be bound: several initialize expressions read `family$link` (gaussian does), and
  # glm.fit evaluates them in a frame where it is the argument's name.
  e   <- new.env(parent = baseenv())
  assign("family", fam, envir = e)
  assign("y", y0, envir = e)
  assign("weights", if (length(base_w) == n0) as.numeric(base_w) else rep(1, n0), envir = e)
  assign("nobs", n0, envir = e); assign("n", n0, envir = e)
  assign("mustart", NULL, envir = e); assign("etastart", NULL, envir = e)
  assign("start", NULL, envir = e)
  ok <- tryCatch({ eval(fam$initialize, envir = e); TRUE }, error = function(err) FALSE)
  if (!ok) return(NULL)
  y <- get("y", envir = e); w <- get("weights", envir = e)
  if (length(y) != n0 || length(w) != n0) return(NULL)
  list(y = as.numeric(y), prior = as.numeric(w))
}

# The sampling weights the fit was given, BEFORE the family's own adjustment -- the `weights`
# argument, never `fit$prior.weights`, which is that vector AFTER it (a cbind() binomial multiplies
# it by the trials, and reg_digest_response() runs the same initialize again).
# ⚠ THE FRAME MUST BE THE MODEL FRAME, which carries the design's `.svy_weights` column; a bare
# model.frame(fit) does not, so a weighted fit is refused there rather than silently weighted by 1.
#' @keywords internal
#' @noRd
reg_digest_base_weights <- function(digest, frame) {
  wt <- digest$recipe$design_spec$wt
  # a digest taken of a stand-alone fit has no recipe, but a tabxplor model frame still names its
  # own weights -- so read them rather than silently weighting by 1.
  if (is.null(wt) && svy_wt_col %in% names(frame)) wt <- svy_wt_col
  if (is.null(wt)) return(1)
  if (!wt %in% names(frame)) return(NULL)
  as.numeric(frame[[wt]])
}

# THE IRLS parts, as reg_if_from_parts() wants them.
#' @keywords internal
#' @noRd
reg_digest_working <- function(digest, frame) {
  if (!identical(REG_FIT_KINDS[[digest$kind]]$score, "irls")) return(NULL)
  mm <- reg_digest_mm(digest, frame)
  if (is.null(mm)) return(NULL)
  b  <- digest$coef
  if (is.null(b) || ncol(mm$X) != length(b)) return(NULL)
  keep <- !is.na(b)
  X    <- mm$X[, keep, drop = FALSE]
  colnames(X) <- stringi::stri_replace_all_regex(colnames(X), "`", "")
  fam  <- tryCatch(stats::family(digest), error = function(e) NULL)
  bw   <- reg_digest_base_weights(digest, frame)
  if (is.null(fam) || is.null(bw)) return(NULL)
  rsp  <- reg_digest_response(digest, mm$mf, bw)
  if (is.null(rsp)) return(NULL)
  eta  <- as.vector(X %*% b[keep]) + mm$offset
  mu   <- fam$linkinv(eta); me <- fam$mu.eta(eta); v <- fam$variance(mu)
  list(X = X, W = rsp$prior * me^2 / v, r = (rsp$y - mu) / me)
}
