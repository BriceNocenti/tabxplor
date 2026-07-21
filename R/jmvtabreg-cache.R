# PURPOSE: The jmvtabreg live-UI fit cache (Phase 15b) + the engine-free build core jmvtab_reg_build().
# ROLE: Drives tab_reg() with a mutable cache environment injected via its internal `.fit_cache` arg.
#       reg_build() (R/tab_reg.R) fetches through jmvreg_cached(): on the single-equation GLM
#       coefficient path a KB-sized "digest" (coef + vcov + reference-invariant glance) whose key is
#       reference-INDEPENDENT (a reference change is reparametrized live, no refit); on the heavy paths
#       (ame / profile / mnl-vs-rest / compound / multinomial / ordinal / split) the raw reg_fit result
#       keyed on the (already display-referenced) data + transform settings. Content-addressed,
#       schema-versioned, byte-bounded LRU, persisted to the hidden `cache_state` Image $state.
# ROLE (build core): jmvtab_reg_build() is the pure, engine-free entry the R6 backend (R/jmvtabreg.b.R)
#       calls -- it maps the plain options list onto tab_reg(..., .fit_cache = cache_env) and returns
#       list(tabs, store, hits). Kept engine-free so it is unit-testable without a live jamovi session.
#       Picker folders map the hidden Array UI options into tab_reg() args: jmvtab_reg_ref_vector()
#       (references), jmvtab_reg_models() (the model-comparison "+" builder -> `predictors` list or the
#       flat pool), jmvtab_reg_mult_vector() (numeric-predictor scaling -> `multiplier`).
# KEY CONSTRAINTS:
#   - jmvtabreg.h.R is GENERATED from jmvtabreg.a.yaml (jmvtools::prepare()); never hand-edit it.
#   - Persist plain lists (coef vectors, vcov matrices, tibbles) -- NEVER a live object bound to an env.
#   - The digest key is reference-INDEPENDENT so a reference change is a HIT; the `na` mode + weights are
#     captured through the per-column fingerprint of the (already prepared) data, not as extra key parts.
#   - Rides the SHARED cache kernel (R/jmvtab-cache.R: jmv_cache_config + jmv_store_*) with its own
#     2-tier config (digest / fit); only the store stays decoupled (its tiers + $state differ from the
#     crosstab store). Phase 17i replaced this file's duplicated + O(n^2)-evicting store lifecycle.
#   - INHERITS jmv_col_fp()'s value-edit blind spot (a same-shape edit preserving class / factor levels /
#     NA-count is NOT caught -> can serve a STALE fit after a data edit; best-effort, self-heals on the
#     next structural change). Escape hatch for the paranoid: options(tabxplor.jmv_full_hash = TRUE)
#     forces a full-value column hash (slower, exact) in BOTH modules -- see ?tabxplor-options.
# See: dev/tabxplor_1.4.0_jamovi_dev.md ; CLAUDE.md > 1.4.0 roadmap > Phase 15b/17i.


# === Constants + config ====================================================================
# The reg store rides the shared cache kernel (R/jmvtab-cache.R: jmv_cache_config + jmv_store_*) with
# its own 2-tier config; only the store is decoupled (its tiers + $state differ from the crosstab store).
JMVREG_CACHE_SCHEMA <- 3L   # bump on any store-shape change -> discard stale stores
  #   history: 2 = Phase 17b table attrs merged into one `meta` list
  #            3 = Phase 17i unified kernel entry shape list(value, bytes, seq)
# 2 tiers: KB-sized `digest` (reference-invariant fast path) + raw `fit`. A raw reg_fit (glm + model
# frame + tidy) is ~9-11 MB on survey-scale data and MODEL COMPARISON forces the fit tier (the digest
# fast-path is single-model only), so the fit ceiling MUST clear a realistic fit or comparison never
# caches -> every display / reference toggle refits. The store budget holds a handful of such fits.
JMVREG_CFG <- jmv_cache_config(
  schema      = JMVREG_CACHE_SCHEMA,
  entry_bytes = c(digest = 512L * 1024L, fit = 24L * 1024L * 1024L),
  store_bytes = 96L * 1024L * 1024L   # whole-store budget (serialized to $state every run -> LRU-bounded)
)


# === Store lifecycle (thin wrappers -> the shared kernel with JMVREG_CFG) ===================
#' @keywords internal
#' @noRd
jmvreg_cache_new <- function() jmv_store_new(JMVREG_CFG)

#' @keywords internal
#' @noRd
jmvreg_cache_migrate <- function(store) jmv_store_migrate(JMVREG_CFG, store)

# A mutable cache environment (store + hit/miss tally) passed to tab_reg() as `.fit_cache`.
#' @keywords internal
#' @noRd
jmvreg_cache_env <- function(store = NULL) jmv_store_env(JMVREG_CFG, store)

#' @keywords internal
#' @noRd
jmvreg_cache_evict <- function(store) jmv_store_evict(JMVREG_CFG, store)

# Fetch-or-compute-and-put on the reg store; the tier ceiling + LRU come from JMVREG_CFG. Reference-
# INDEPENDENT digest keys mean a reference change is a HIT (recomputed live, no refit).
#' @keywords internal
#' @noRd
jmvreg_cached <- function(cache_env, tier, key, compute_fn)
  jmv_store_cached(JMVREG_CFG, cache_env, tier, key, compute_fn)

# The content key for one model spec. Reference-INDEPENDENT on the digest path (reference is applied at
# reparametrization time), so a reference change is a cache HIT. The per-column fingerprint (jmv_col_fp)
# of the model + design variables captures a weight / population (`na`) change; `extra` carries the
# transform settings the RAW-fit path additionally keys on (method / effect / display / ...).
#' @keywords internal
#' @noRd
jmvreg_fit_key <- function(sp, data, family, design_spec, extra = NULL) {
  used <- intersect(unique(c(sp$dependent, sp$predictors, reg_design_vars(design_spec))), names(data))
  jmv_hash(list(
    kind       = "jmvreg",
    dependent  = sp$dependent,
    predictors = sp$predictors,
    trials     = sp$trials,
    inverse    = sp$inverse,
    formula    = if (!is.null(sp$formula)) paste(deparse(sp$formula), collapse = " ") else NULL,
    family     = family,
    nrow       = nrow(data),
    fp         = lapply(data[used], jmv_col_fp),
    design     = list(wt = design_spec$wt, ids = design_spec$ids, strata = design_spec$strata,
                      fpc = design_spec$fpc, nest = design_spec$nest,
                      has_design = !is.null(design_spec$design)),
    extra      = extra
  ))
}


# === Reference picker -> tab_reg(reference =) ==============================================

# Fold the per-predictor reference picker (the `refLevels` Array option) into tab_reg()'s `reference`
# named vector. Each element is list(var, ref); an entry with an explicit level contributes
# c(<var> = <level>). Returns NULL when nothing was picked (-> tab_reg() uses its default first-level
# references). Mirrors jmvtab_ref_vector() but has no "auto"/"tot" sentinels (a regression reference is
# always a factor LEVEL).
#' @keywords internal
#' @noRd
jmvtab_reg_ref_vector <- function(refLevels) {
  if (length(refLevels) == 0) return(NULL)
  get1 <- function(e, k) { v <- e[[k]]; if (is.null(v)) NA_character_ else as.character(v) }
  vars <- vapply(refLevels, get1, character(1), k = "var")
  refs <- vapply(refLevels, get1, character(1), k = "ref")
  keep <- !is.na(vars) & nzchar(vars) & !is.na(refs) & nzchar(refs)
  if (!any(keep)) return(NULL)
  stats::setNames(refs[keep], vars[keep])
}


# === Model-comparison builder + predictor scaling -> tab_reg() args =========================

# Fold the model-builder (`models` Array of Group{label, vars}) + the flat predictor pool into
# tab_reg()'s `predictors`. An EMPTY builder -> the flat pool = single model (a character vector, or
# NULL when the pool is empty too -> a NULL table + hint). >=1 card -> a NAMED LIST of character
# vectors = model-comparison mode (one effect column per model). Each card is intersected with the
# pool (pool order, dropping stale vars); a blank label becomes "model{i}" (friendlier than
# tab_reg()'s all-or-nothing rename); empty-var cards are dropped; if nothing survives -> the pool.
#' @keywords internal
#' @noRd
jmvtab_reg_models <- function(models, pool) {
  pool <- if (length(pool)) as.character(pool) else character()
  flat <- if (length(pool)) pool else NULL
  if (length(models) == 0L) return(flat)
  built  <- lapply(models, function(e) intersect(pool, as.character(unlist(e$vars, use.names = FALSE))))
  labels <- vapply(models, function(e) { v <- e$label; if (is.null(v)) "" else as.character(v) },
                   character(1))
  keep   <- vapply(built, length, integer(1)) > 0L
  built  <- built[keep]; labels <- labels[keep]
  if (length(built) == 0L) return(flat)
  blank  <- !nzchar(labels)
  labels[blank] <- paste0("model", seq_along(labels))[blank]
  stats::setNames(built, labels)
}

# Phase h: the staged-comparison gate. A model comparison (>=2 folded models) is heavy -- refitting
# every model on each live edit is what froze the panel, so its table recomputes ONLY on the Run/Export
# action (jmvtabreg.b.R). jmvtab_reg_staged() reuses jmvtab_reg_models() so the predicate matches the
# model set tab_reg() actually sees. jmvtab_reg_compare_sig() fingerprints the resolved build/display
# options (the `.opts()` list, which already excludes the action + export controls), so `.run()` can
# tell an unchanged/just-computed table (re-serve) from an outdated one (banner + Run prompt).
#' @keywords internal
#' @noRd
jmvtab_reg_staged <- function(models, predictors) {
  preds <- jmvtab_reg_models(models, predictors)
  is.list(preds) && length(preds) >= 2L
}

#' @keywords internal
#' @noRd
jmvtab_reg_compare_sig <- function(opts) jmv_hash(opts)

# Fold the per-numeric-predictor scaling picker (the jamovi `multiplicator` Array of Group{var, k})
# into tab_reg()'s named numeric `multiplier`. Blank / non-numeric k dropped; NULL when nothing set.
# Mirrors jmvtab_reg_ref_vector().
#' @keywords internal
#' @noRd
jmvtab_reg_mult_vector <- function(multiplicator) {
  if (length(multiplicator) == 0L) return(NULL)
  get1 <- function(e, k) { v <- e[[k]]; if (is.null(v)) NA_character_ else as.character(v) }
  vars <- vapply(multiplicator, get1, character(1), k = "var")
  ks   <- suppressWarnings(as.numeric(vapply(multiplicator, get1, character(1), k = "k")))
  keep <- !is.na(vars) & nzchar(vars) & !is.na(ks)
  if (!any(keep)) return(NULL)
  stats::setNames(ks[keep], vars[keep])
}


# === Per-dependent Model table -> tab_reg() args (Phase 15d / 15e) ==========================
# The Model table (depFamily / depModelLevel / depTrials arrays) sets one family + modelled level +
# trials per outcome. These three helpers resolve ONE dependent; jmvtab_reg_build() (Phase 15e) passes
# the resolved per-dependent family / inverse / trials VECTORS to ONE tab_reg() call -- a mixed table
# (several outcomes, different families) is now one table, one column-group per outcome (no more
# grouping-by-family / tabxplor_tabs stacking).

# The chosen family for `dep` (an explicit non-blank pick) else auto-detected from the outcome.
#' @keywords internal
#' @noRd
jmvtab_reg_dep_family <- function(depFamily, dep, data) {
  if (length(depFamily)) for (e in depFamily) {
    if (identical(as.character(e$var), dep) && length(e$family) && nzchar(as.character(e$family)))
      return(as.character(e$family))
  }
  reg_detect_family(data, dep)
}

# TRUE (the default) = model the FIRST level of a 2-level factor outcome. depModelLevel stores a level
# ONLY when the user picked a NON-first level as the modelled one -> FALSE.
#' @keywords internal
#' @noRd
jmvtab_reg_dep_modelled_first <- function(depModelLevel, dep) {
  if (length(depModelLevel)) for (e in depModelLevel) {
    if (identical(as.character(e$var), dep) && length(e$level) && nzchar(as.character(e$level)))
      return(FALSE)
  }
  TRUE
}

# The number of trials for a numeric binomial outcome: an explicit entry, else the observed max when the
# outcome is a >1 integer count (a summed score). NA -> ordinary binary logit (a factor, or a 0/1 numeric).
#' @keywords internal
#' @noRd
jmvtab_reg_dep_trials <- function(depTrials, dep, data) {
  if (length(depTrials)) for (e in depTrials) {
    if (identical(as.character(e$var), dep) && length(e$n) && nzchar(as.character(e$n))) {
      n <- suppressWarnings(as.integer(round(as.numeric(e$n))))
      if (!is.na(n) && n >= 1L) return(n)
    }
  }
  x <- data[[dep]]
  if (is.numeric(x) && !is.factor(x)) {
    m <- suppressWarnings(max(x, na.rm = TRUE))
    if (is.finite(m) && m > 1) return(as.integer(round(m)))
  }
  NA_integer_
}


# === The engine-free build core ============================================================

# Drive tab_reg() with the live fit cache injected. `opts` is the plain list the R6 backend's .opts()
# produces (already in tab_reg() vocabulary: exponentiate mapped to nongaussian/TRUE/FALSE, color
# "default" -> NULL, stats resolved, reference a named vector or NULL). Empty variable slots (jamovi
# passes partial selections mid-interaction) yield a NULL table -> the backend renders a friendly hint.
#' @keywords internal
#' @noRd
jmvtab_reg_build <- function(data, opts, store = NULL) {
  cache_env <- jmvreg_cache_env(store)

  nz  <- function(x) if (length(x) && nzchar(as.character(x)[[1]])) as.character(x) else NULL
  dep   <- nz(opts$dependent)
  # `predictors` may be a character vector (single model) OR a named list of character vectors
  # (model comparison, from jmvtab_reg_models()): pass a list through, coerce a vector.
  preds <- opts$predictors
  if (is.list(preds)) {
    if (length(preds) == 0L) preds <- NULL
  } else {
    preds <- if (length(preds)) as.character(preds) else NULL
  }

  if (is.null(dep) || is.null(preds)) {
    return(list(tabs = NULL, store = cache_env$store, hits = 0L))
  }
  # model comparison (a predictor-subset list) needs a single dependent -> a friendly NULL / hint
  # instead of tab_reg()'s abort while the user is still selecting.
  if (is.list(preds) && length(dep) > 1L) {
    return(list(tabs = NULL, store = cache_env$store, hits = 0L))
  }

  # Phase 15e: resolve each outcome's family / modelled level / trials from the Model table, then pass them
  # to ONE tab_reg() call as per-dependent vectors -- so several outcomes with DIFFERENT families render as
  # one mixed table (tab_reg builds one column-group per outcome). No more family-grouping / stacking.
  fams <- vapply(dep, function(d) jmvtab_reg_dep_family(opts$depFamily, d, data), character(1))
  invs <- vapply(dep, function(d) jmvtab_reg_dep_modelled_first(opts$depModelLevel, d), logical(1))
  # trials are binomial-only (grouped / summed-score); non-binomial outcomes never carry one.
  tris <- vapply(seq_along(dep), function(i) {
    if (identical(fams[i], "binomial")) jmvtab_reg_dep_trials(opts$depTrials, dep[i], data)
    else NA_integer_
  }, integer(1))

  fam_arg <- stats::setNames(fams, dep)
  inv_arg <- if (all(invs)) TRUE else stats::setNames(invs, dep)   # scalar unless a pick overrode it
  tri_arg <- if (all(is.na(tris))) NULL else stats::setNames(as.integer(tris), dep)

  tabs <- tab_reg(
    data,
    dependent    = dep,
    predictors   = preds,
    family       = fam_arg,
    wt           = nz(opts$wt),
    ids          = nz(opts$ids),
    strata       = nz(opts$strata),
    fpc          = nz(opts$fpc),
    nest         = isTRUE(opts$nest),
    exponentiate = opts$exponentiate,
    effect       = opts$effect,
    at           = opts$at,
    conf_level   = opts$conf_level,
    method       = opts$method,
    reference    = opts$reference,
    inverse_two_level_factors = inv_arg,
    split_var    = nz(opts$split_var),
    empirical    = isTRUE(opts$empirical),
    stats        = opts$stats,
    estimate_display = opts$estimate_display,
    color        = opts$color,
    color_signif = opts$color_signif,
    stars        = isTRUE(opts$stars),
    na           = opts$na,
    cleannames   = opts$cleannames,
    subtext      = opts$subtext,
    compare      = if (is.null(opts$compare)) "none" else opts$compare,
    baseline     = opts$baseline,
    multiplier   = jmvtab_reg_mult_vector(opts$multiplicator),   # tab_reg skips mnl/ordinal specs per-spec
    trials       = tri_arg,
    .fit_cache   = cache_env
  )

  cache_env$store <- jmvreg_cache_evict(cache_env$store)
  list(tabs = tabs, store = cache_env$store, hits = cache_env$hits)
}
