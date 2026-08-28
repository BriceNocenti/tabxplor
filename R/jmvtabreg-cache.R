# PURPOSE: The jmvtabreg (Regressions) fit store, plus jmvtab_reg_build(), the engine-free build core.
# ROLE: jmvtab_reg_build() maps the panel's plain options list onto tab_reg(..., .fit_cache =) and
#       returns list(tabs, store, hits); both fit paths reach the store through reg_fit_cached()
#       (R/reg-digest.R) -> jmvreg_cached(). It is kept engine-free so it is testable without a live
#       jamovi session. The picker folders turn the hidden Array options into ordinary tab_reg()
#       arguments: references, the model-comparison builder, the interaction keys, numeric scaling,
#       the per-variable cut (shared with jmvtab) and the per-outcome Model table.
# WHAT THE STORE HOLDS: ONE tier, carrying the DISTILLED fit record -- a `tabxplor_fitdigest`
#       (R/reg-digest.R) plus everything the eager stage computed off the live fit, the fitted object
#       and the model frame thrown away. So the key carries NO ESTIMAND: measure / effect / display /
#       colour / conf_level / multiplier all change without a refit, and a record is kilobytes where
#       a fit is megabytes.
#       ⚠ ONE RECORD SHAPE, TWO KINDS: a MODEL fit, and each observed (crude) univariable one
#       (R/reg-empirical.R). They are told apart by the KEY alone -- a crude key is a synthetic
#       one-predictor spec whose `drop_extra` names the rest of the predictor set, which is what
#       lands it on the model's own complete cases.
# KEY CONSTRAINTS:
#   - jmvtabreg.h.R is GENERATED from jamovi/jmvtabreg.a.yaml; never hand-edit it.
#   - Persist plain lists, never a live object bound to an environment. A digest is one by
#     construction (reg_digest_terms() rebases the terms object's environment for exactly this).
#   - The REFERENCE is in the key for free: the data is relevelled before the fit and jmv_col_fp()
#     fingerprints a column's levels, so a reference change is an honest refit. The level ORDER is
#     not -- it reaches tab_reg() as `.levels_order`, a display permutation of the row skeleton, so a
#     move touches no column and cannot move a key.
#   - Rides the shared kernel (R/jmvtab-cache.R); only the store is decoupled, its tier and its
#     $state differing from the crosstab one.
#   - INHERITS jmv_col_fp()'s blind spot: a same-shape value edit (class, factor levels and NA count
#     unchanged) is not caught, so a stale fit can be served until the next structural change.
#     Escape hatch: JMV_FULL_HASH (R/jmvtab-cache.R) forces an exact full-value hash in both modules.
#   - ⚠ THE STAGED COMPARISON'S RENDER IS KEPT TWICE and both copies earn it: jmvcore's $state
#     survives an engine reset but warns past 5e5 compressed bytes, while JMVREG_RENDERS survives
#     only the process. The state carries the signature always and the HTML while it fits.
# See: CLAUDE.md § tabxplor architecture (jamovi) ; dev/jamovi_module.md.


# === Constants + config ====================================================================

JMVREG_CACHE_SCHEMA <- 10L  # bump on any store-shape change -> discard stale stores

# DESIGN: a distilled record (no fitted object, no frame) is small even for a wide model, so one
# modest tier ceiling holds a whole panel's worth of fits.
JMVREG_CFG <- jmv_cache_config(
  schema      = JMVREG_CACHE_SCHEMA,
  entry_bytes = c(fit = 2L * 1024L * 1024L),
  store_bytes = 32L * 1024L * 1024L   # whole-store budget (serialized to $state every run -> LRU-bounded)
)


# === Store lifecycle (thin wrappers -> the shared kernel with JMVREG_CFG) ===================
#' @noRd
jmvreg_cache_new <- function() jmv_store_new(JMVREG_CFG)

#' @noRd
jmvreg_cache_migrate <- function(store) jmv_store_migrate(JMVREG_CFG, store)

#' @noRd
jmvreg_cache_env <- function(store = NULL) jmv_store_env(JMVREG_CFG, store)

#' @noRd
jmvreg_cache_evict <- function(store) jmv_store_evict(JMVREG_CFG, store)

#' @noRd
jmvreg_cached <- function(cache_env, tier, key, compute_fn)
  jmv_store_cached(JMVREG_CFG, cache_env, tier, key, compute_fn)

# WARNING: `drop_extra` is a key member, fingerprinted like any predictor -- it names variables whose
# missing values narrow this fit's complete-case population without appearing in the formula.
#' @noRd
jmvreg_fit_key <- function(sp, data, family, design_spec, extra = NULL,
                           drop_extra = character(0)) {
  used <- intersect(unique(c(sp$outcome, sp$predictors, drop_extra,
                             reg_design_vars(design_spec))), names(data))
  jmv_hash(list(
    kind       = "jmvreg",
    outcome    = sp$outcome,
    predictors = sp$predictors,
    trials     = sp$trials,
    outcome_level = sp$outcome_level,
    formula    = if (!is.null(sp$formula)) paste(deparse(sp$formula), collapse = " ") else NULL,
    family     = family,
    nrow       = nrow(data),
    fp         = lapply(data[used], jmv_col_fp),
    design     = list(wt = design_spec$wt, has_design = !is.null(design_spec$design)),
    drop_extra = drop_extra,
    extra      = extra
  ))
}


# === Reference picker -> tab_reg(ref =) ====================================================

#' @noRd
jmvtab_reg_ref_vector <- function(ref_levels) {
  if (length(ref_levels) == 0) return(NULL)
  get1 <- function(e, k) { v <- e[[k]]; if (is.null(v)) NA_character_ else as.character(v) }
  vars <- vapply(ref_levels, get1, character(1), k = "var")
  refs <- vapply(ref_levels, get1, character(1), k = "ref")
  keep <- !is.na(vars) & nzchar(vars) & !is.na(refs) & nzchar(refs)
  if (!any(keep)) return(NULL)
  stats::setNames(refs[keep], vars[keep])
}


# === Model-comparison builder + predictor scaling -> tab_reg() args =========================

# WARNING: `a != b` is the last of three defences against `race*race`; the R boundary must not
# trust the picker alone.
#' @noRd
jmvtab_reg_cross_keys <- function(crosses, pool) {
  if (length(crosses) == 0L) return(character(0))
  get1 <- function(e, k) { v <- e[[k]]; if (is.null(v)) NA_character_ else as.character(v)[[1]] }
  a <- vapply(crosses, get1, character(1), k = "var1")
  b <- vapply(crosses, get1, character(1), k = "var2")
  keep <- !is.na(a) & !is.na(b) & nzchar(a) & nzchar(b) & a != b & a %in% pool & b %in% pool
  if (!any(keep)) return(character(0))
  unique(paste(a[keep], b[keep], sep = "*"))
}

#' @noRd
jmvtab_reg_cross_parents <- function(keys) {
  if (length(keys) == 0L) return(character(0))
  unique(unlist(strsplit(keys, "*", fixed = TRUE), use.names = FALSE))
}

# DESIGN: the zero-card fold. With no model card there is nowhere to tick an interaction, so every
# defined pair applies to the single live model and replaces its two parents in place.
#' @noRd
jmvtab_reg_cross_fold <- function(vars, keys) {
  for (k in keys) {
    p <- strsplit(k, "*", fixed = TRUE)[[1]]
    if (!all(p %in% vars)) next
    i <- which(vars %in% p)
    vars[i[[1]]] <- k
    if (length(i) > 1L) vars <- vars[-i[-1]]
  }
  vars
}

# WARNING: `flatten` is the several-outcomes rule -- a comparison needs exactly one outcome, so a
# single card beside several outcomes must arrive as a plain vector, not a one-element named list.
#' @noRd
jmvtab_reg_models <- function(models, pool, cross_keys = character(0), flatten = FALSE) {
  pool <- if (length(pool)) as.character(pool) else character()
  flat <- if (length(pool)) jmvtab_reg_cross_fold(pool, cross_keys) else NULL
  if (length(models) == 0L) return(flat)
  built  <- lapply(models, function(e) {
    v <- intersect(pool, as.character(unlist(e$vars, use.names = FALSE)))
    k <- intersect(cross_keys, as.character(unlist(e$crosses, use.names = FALSE)))
    c(setdiff(v, jmvtab_reg_cross_parents(k)), k)
  })
  labels <- vapply(models, function(e) { v <- e$label; if (is.null(v)) "" else as.character(v) },
                   character(1))
  keep   <- vapply(built, length, integer(1)) > 0L
  built  <- built[keep]; labels <- labels[keep]
  if (length(built) == 0L) return(flat)
  if (isTRUE(flatten) && length(built) == 1L) return(built[[1L]])
  blank  <- !nzchar(labels)
  labels[blank] <- paste0("model", seq_along(labels))[blank]
  stats::setNames(built, labels)
}

#' @noRd
# WARNING: `cross_keys` must reach this predicate -- a card holding only an interaction has an
# empty `vars`, so without them a two-model comparison would run live instead of staged.
jmvtab_reg_staged <- function(models, predictors, cross_keys = character(0)) {
  preds <- jmvtab_reg_models(models, predictors, cross_keys)
  is.list(preds) && length(preds) >= 2L
}

#' @noRd
jmvtab_reg_compare_sig <- function(opts) jmv_hash(opts)

#' @noRd
JMVREG_RENDERS <- new.env(parent = emptyenv())

# jmvcore's own `$state` ceiling -- past it it warns, so the render is dropped from the state
# rather than crossing it, and the process-local mirror above is what still has it.
#' @noRd
JMVREG_STATE_MAX <- 5e5

#' @noRd
jmvtab_reg_render_store <- function(sig, html) {
  keys <- ls(JMVREG_RENDERS)
  if (length(keys) >= 2L && !sig %in% keys) rm(list = keys[[1L]], envir = JMVREG_RENDERS)
  assign(sig, html, envir = JMVREG_RENDERS)
  if (nchar(html, type = "bytes") > JMVREG_STATE_MAX) list(sig = sig)
  else list(sig = sig, html = html)
}

#' @noRd
jmvtab_reg_render_fetch <- function(cst) {
  if (is.null(cst) || is.null(cst$sig)) return(NULL)
  cst$html %||% (if (exists(cst$sig, envir = JMVREG_RENDERS, inherits = FALSE))
                   get(cst$sig, envir = JMVREG_RENDERS) else NULL)
}

#' @noRd
jmvtab_shape_vector <- function(shape) {
  if (length(shape) == 0L) return(NULL)
  get1 <- function(e, k) { v <- e[[k]]; if (is.null(v)) NA_character_ else as.character(v) }
  vars <- vapply(shape, get1, character(1), k = "var")
  shp  <- vapply(shape, get1, character(1), k = "shape")
  keep <- !is.na(vars) & nzchar(vars) & !is.na(shp) & nzchar(shp) &
    !shp %in% c("linear", "auto")
  if (!any(keep)) return(NULL)
  stats::setNames(shp[keep], vars[keep])
}

# The keywords "sd" / "2sd" pass through as text so the picker can offer tab_reg()'s own default.
#' @noRd
jmvtab_reg_mult_vector <- function(multiplier) {
  if (length(multiplier) == 0L) return(NULL)
  get1 <- function(e, k) { v <- e[[k]]; if (is.null(v)) NA_character_ else as.character(v) }
  vars <- vapply(multiplier, get1, character(1), k = "var")
  raw  <- trimws(vapply(multiplier, get1, character(1), k = "k"))
  kw   <- tolower(raw) %in% REG_MULTIPLIER_KEYWORDS
  num  <- suppressWarnings(as.numeric(raw))
  keep <- !is.na(vars) & nzchar(vars) & (kw | !is.na(num))
  if (!any(keep)) return(NULL)
  if (any(kw[keep])) stats::setNames(ifelse(kw[keep], tolower(raw[keep]), raw[keep]), vars[keep])
  else               stats::setNames(num[keep], vars[keep])
}


# === Per-outcome Model table -> tab_reg() args ==============================================

#' @noRd
jmvtab_reg_dep_family <- function(family, dep, data) {
  if (length(family)) for (e in family) {
    if (identical(as.character(e$var), dep) && length(e$family) && nzchar(as.character(e$family)))
      return(as.character(e$family))
  }
  reg_detect_family(data, dep)
}

#' @noRd
jmvtab_reg_link_vector <- function(link) {
  if (length(link) == 0L) return(NULL)
  get1 <- function(e, k) { v <- e[[k]]; if (is.null(v)) NA_character_ else as.character(v) }
  vars <- vapply(link, get1, character(1), k = "var")
  lks  <- vapply(link, get1, character(1), k = "link")
  keep <- !is.na(vars) & nzchar(vars) & !is.na(lks) & nzchar(lks) & lks != "auto"
  if (!any(keep)) return(NULL)
  stats::setNames(lks[keep], vars[keep])
}

# WARNING: this travels as a LEVEL, never a flag -- its meaning is family-specific (the modelled
# level for binomial, the baseline category for multinomial).
#' @noRd
jmvtab_reg_dep_level <- function(outcome_level, dep) {
  if (length(outcome_level)) for (e in outcome_level) {
    if (identical(as.character(e$var), dep) && length(e$level) && nzchar(as.character(e$level)))
      return(as.character(e$level))
  }
  NA_character_
}

#' @noRd
jmvtab_reg_dep_trials <- function(trials, dep) {
  if (length(trials)) for (e in trials) {
    if (identical(as.character(e$var), dep) && length(e$n) && nzchar(as.character(e$n))) {
      n <- suppressWarnings(as.integer(round(as.numeric(e$n))))
      if (!is.na(n) && n >= 1L) return(n)
    }
  }
  NA_integer_
}


# === The engine-free build core ============================================================

#' @noRd
jmvtab_reg_build <- function(data, opts, store = NULL, use_cache = TRUE) {
  # WARNING: always serial, whatever the user's `tabxplor.parallel` option says -- the module runs
  # inside jamovi's own engine process, where spawning daemons is not ours to do.
  .old_par <- options(tabxplor.parallel = FALSE)
  on.exit(options(.old_par), add = TRUE)
  cache_env <- jmvreg_cache_env(if (use_cache) store else NULL)

  nz  <- function(x) if (length(x) && nzchar(as.character(x)[[1]])) as.character(x) else NULL
  dep   <- nz(opts$outcome)
  preds <- opts$predictors
  if (is.list(preds)) {
    if (length(preds) == 0L) preds <- NULL
  } else {
    preds <- if (length(preds)) as.character(preds) else NULL
  }

  collapse <- jmvtab_levels_collapse(opts$levels_collapse)
  # WARNING: translated to merged names here -- an untranslated order would silently name levels the
  # merged table lacks.
  ord      <- jmv_order_after_collapse(jmvtab_levels_order(opts$levels_order), collapse)

  if (is.null(dep) || is.null(preds)) {
    return(list(tabs = NULL, store = cache_env$store, hits = 0L))
  }
  if (is.list(preds) && length(dep) > 1L) {
    return(list(tabs = NULL, store = cache_env$store, hits = 0L))
  }

  fams <- vapply(dep, function(d) jmvtab_reg_dep_family(opts$family, d, data), character(1))
  # ⚠ GATED ON THE FAMILY, exactly as `trials` is just below. The panel keeps a stored
  # `outcome_level` when the family select changes (switching back restores the choice), and an
  # ordinal or gaussian family offers no such level -- so an ungated read aborted tab_reg() on a
  # multinomial -> ordinal switch. Dropping it here protects every caller, not only the panel.
  lvls <- vapply(seq_along(dep), function(i) {
    if (is.na(reg_outcome_level_role(fams[i]))) NA_character_
    else jmvtab_reg_dep_level(opts$outcome_level, dep[i])
  }, character(1))
  tris <- vapply(seq_along(dep), function(i) {
    if (identical(fams[i], "binomial")) jmvtab_reg_dep_trials(opts$trials, dep[i])
    else NA_integer_
  }, integer(1))

  lnk_arg <- jmvtab_reg_link_vector(opts$link)

  fam_arg <- stats::setNames(fams, dep)
  lvl_arg <- if (all(is.na(lvls))) NULL else stats::setNames(lvls, dep)[!is.na(lvls)]
  tri_arg <- if (!any(fams == "binomial")) NULL else stats::setNames(as.integer(tris), dep)

  # WARNING: the four variable-role arguments below are injected with `!!` -- a bare local could be
  # hijacked by a same-named data column.
  tabs <- rlang::inject(tab_reg(
    data,
    outcome      = !!dep,
    predictors   = !!preds,
    family       = fam_arg,
    wt           = !!nz(opts$wt),
    link         = lnk_arg %||% "auto",
    measure      = opts$measure %||% "auto",
    effect       = opts$effect  %||% "auto",
    conf_level   = opts$conf_level,
    ci_method    = opts$ci_method,
    ref          = opts$ref,
    outcome_level = lvl_arg,
    tab_vars     = !!nz(opts$tab_vars),
    empirical    = if (is.null(opts$empirical)) TRUE
                   else if (is.logical(opts$empirical)) isTRUE(opts$empirical)
                   else opts$empirical,
    display      = opts$display %||% "auto",
    # a FLOOR: 0 (the default) means every measure keeps its own precision, not zero decimals.
    digits       = opts$digits %||% 0L,
    shape        = jmvtab_shape_vector(opts$shape),
    color        = opts$color,
    color_signif = opts$color_signif,
    stars        = isTRUE(opts$stars),
    na           = opts$na,
    # WARNING: [["n"]], never $n -- `$` partial-matches a list and would return `opts$na`.
    n            = opts[["n"]] %||% "range",
    cleannames   = opts$cleannames,
    subtext      = opts$subtext,
    multiplier   = jmvtab_reg_mult_vector(opts$multiplier),   # tab_reg skips mnl/ordinal specs per-spec
    trials       = tri_arg,
    .fit_cache   = if (use_cache) cache_env else NULL,
    .levels_collapse = collapse,
    .levels_order    = ord
  ))

  cache_env$store <- jmvreg_cache_evict(cache_env$store)
  list(tabs = tabs, store = if (use_cache) cache_env$store else NULL, hits = cache_env$hits)
}
