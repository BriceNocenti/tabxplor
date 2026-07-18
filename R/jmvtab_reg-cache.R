# PURPOSE: The jmvtab_reg live-UI fit cache (Phase 15b) + the engine-free build core jmvtab_reg_build().
# ROLE: Drives tab_reg() with a mutable cache environment injected via its internal `.fit_cache` arg.
#       reg_build() (R/tab_reg.R) fetches through jmvreg_cached(): on the single-equation GLM
#       coefficient path a KB-sized "digest" (coef + vcov + reference-invariant glance) whose key is
#       reference-INDEPENDENT (a reference change is reparametrized live, no refit); on the heavy paths
#       (ame / profile / mnl-vs-rest / compound / multinomial / ordinal / split) the raw reg_fit result
#       keyed on the (already display-referenced) data + transform settings. Content-addressed,
#       schema-versioned, byte-bounded LRU, persisted to the hidden `cache_state` Image $state.
# ROLE (build core): jmvtab_reg_build() is the pure, engine-free entry the R6 backend (R/jmvtab_reg.b.R)
#       calls -- it maps the plain options list onto tab_reg(..., .fit_cache = cache_env) and returns
#       list(tabs, store, hits). Kept engine-free so it is unit-testable without a live jamovi session.
# KEY CONSTRAINTS:
#   - jmvtab_reg.h.R is GENERATED from jmvtab_reg.a.yaml (jmvtools::prepare()); never hand-edit it.
#   - Persist plain lists (coef vectors, vcov matrices, tibbles) -- NEVER a live object bound to an env.
#   - The digest key is reference-INDEPENDENT so a reference change is a HIT; the `na` mode + weights are
#     captured through the per-column fingerprint of the (already prepared) data, not as extra key parts.
#   - Reuses jmvtab-cache.R's tier-agnostic primitives jmv_hash() / jmv_col_fp(); the store lifecycle is
#     its own (2 tiers: digest / fit) to stay decoupled from the crosstab store's tier names.
# See: dev/tabxplor_1.4.0_jamovi_dev.md ; CLAUDE.md > 1.4.0 roadmap > Phase 15b.


# === Constants =============================================================================
JMVREG_CACHE_SCHEMA     <- 1L                    # bump on any store-shape change -> discard stale stores
JMVREG_MAX_DIGEST_BYTES <- 512L * 1024L          # per-entry ceiling for the KB-sized digests
JMVREG_MAX_FIT_BYTES    <- 4L * 1024L * 1024L    # per-entry ceiling for a raw fit (model + frame)
JMVREG_MAX_STORE_BYTES  <- 16L * 1024L * 1024L   # whole-store budget (serialized every run -> bounded)


# === Store lifecycle =======================================================================

# A fresh empty store. `clock` is a monotone logical counter used for LRU (no Sys.time -- determinism).
#' @keywords internal
#' @noRd
jmvreg_cache_new <- function() {
  list(schema = JMVREG_CACHE_SCHEMA, clock = 0L, digest = list(), fit = list())
}

# Restore-or-reset: a NULL state (first run) or a schema mismatch yields a fresh store.
#' @keywords internal
#' @noRd
jmvreg_cache_migrate <- function(store) {
  if (is.null(store) || !is.list(store) || !identical(store$schema, JMVREG_CACHE_SCHEMA)) {
    return(jmvreg_cache_new())
  }
  store
}

# A mutable cache environment: the store + a hit / miss tally (diagnostics + tests). Passed to tab_reg()
# as `.fit_cache`; reg_build() reads/writes ce$store through jmvreg_cached().
#' @keywords internal
#' @noRd
jmvreg_cache_env <- function(store = NULL) {
  ce <- new.env(parent = emptyenv())
  ce$store  <- jmvreg_cache_migrate(store)
  ce$hits   <- 0L
  ce$misses <- 0L
  ce
}

# Byte-bounded LRU: drop the lowest-clock entries across BOTH tiers until the store is under budget.
#' @keywords internal
#' @noRd
jmvreg_cache_evict <- function(store) {
  index <- function() {
    idx <- list()
    for (tier in c("digest", "fit")) {
      for (k in names(store[[tier]])) {
        e <- store[[tier]][[k]]
        idx[[length(idx) + 1L]] <- list(tier = tier, key = k,
                                        clock = e$.clock, bytes = e$.bytes)
      }
    }
    idx
  }
  total <- function(idx) sum(vapply(idx, function(x) as.numeric(x$bytes), numeric(1)))
  idx <- index()
  while (length(idx) > 0L && total(idx) > JMVREG_MAX_STORE_BYTES) {
    victim <- idx[[which.min(vapply(idx, function(x) as.numeric(x$clock), numeric(1)))]]
    store[[victim$tier]][[victim$key]] <- NULL
    idx <- index()
  }
  store
}

# Fetch-or-compute-and-put. Returns compute_fn() unchanged when cache_env is NULL (so tab_reg() /
# reg_build() are usable without a cache). On a hit: refresh the LRU stamp, tally, return the value.
# On a miss: compute, serialize to measure bytes, store only if under the tier ceiling (an oversized
# entry is recomputed next time -- graceful, never an error), evict to the store budget.
#' @keywords internal
#' @noRd
jmvreg_cached <- function(cache_env, tier, key, compute_fn) {
  if (is.null(cache_env)) return(compute_fn())
  store <- cache_env$store
  hit   <- store[[tier]][[key]]
  if (!is.null(hit)) {
    store$clock <- store$clock + 1L
    hit$.clock  <- store$clock
    store[[tier]][[key]] <- hit
    cache_env$store <- store
    cache_env$hits  <- cache_env$hits + 1L
    return(hit$value)
  }
  cache_env$misses <- cache_env$misses + 1L
  value   <- compute_fn()
  ceiling <- if (identical(tier, "fit")) JMVREG_MAX_FIT_BYTES else JMVREG_MAX_DIGEST_BYTES
  bytes   <- length(serialize(value, connection = NULL))
  if (bytes <= ceiling) {
    store$clock <- store$clock + 1L
    store[[tier]][[key]] <- list(value = value, .bytes = bytes, .clock = store$clock)
    store <- jmvreg_cache_evict(store)
    cache_env$store <- store
  }
  value
}

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
  preds <- if (length(opts$predictors)) as.character(opts$predictors) else NULL

  if (is.null(dep) || is.null(preds)) {
    return(list(tabs = NULL, store = cache_env$store, hits = 0L))
  }

  tabs <- tab_reg(
    data,
    dependent    = dep,
    predictors   = preds,
    family       = opts$family,
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
    inverse_two_level_factors = isTRUE(opts$inverse_two_level_factors),
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
    .fit_cache   = cache_env
  )

  cache_env$store <- jmvreg_cache_evict(cache_env$store)
  list(tabs = tabs, store = cache_env$store, hits = cache_env$hits)
}
