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
#       flat pool), jmvtab_reg_mult_vector() (numeric-predictor scaling -> `multiplier`),
#       jmvtab_reg_shape_vector() (per-predictor functional form -> `shape`).
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
#     next structural change). Escape hatch: the JMV_FULL_HASH constant in R/jmvtab-cache.R.
#     forces a full-value column hash (slower, exact) in BOTH modules -- see ?tabxplor-options.
# See: dev/tabxplor_2.0.0_jamovi_dev.md ; CLAUDE.md > 2.0.0 roadmap > Phase 15b/17i.


# === Constants + config ====================================================================
# The reg store rides the shared cache kernel (R/jmvtab-cache.R: jmv_cache_config + jmv_store_*) with
# its own 2-tier config; only the store is decoupled (its tiers + $state differ from the crosstab store).
JMVREG_CACHE_SCHEMA <- 7L   # bump on any store-shape change -> discard stale stores
# 7 (Phase 22b-ix): `crosses` joins jmvreg_fit_key()'s `extra`. A nested cross is a formula TERM,
#   not a column, so jmv_col_fp() cannot see it and a stale hit would be a wrong table.
# 6 (Phase 20g-i): jmvreg_fit_key()'s element for the singled-out outcome level is named
#   `outcome_level` (it was `inverse`, the retired `inverse_two_level_factors` spelling). The key's
#   VALUE is unchanged, but a member name is part of the hash, so every key moves.
# 5 (Phase 19k): `shape` and the measure-valued `color` reach the build from the UI, so a stale store
#   could serve a fit made under a different model.
# 4 (Phase 19e): the raw-fit key's `extra` carries the ESTIMAND (effect, measure, display) instead of
# (effect, at, estimate_display) -- a stale store would key a different estimand to the same digest.
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
  used <- intersect(unique(c(sp$outcome, sp$predictors, reg_design_vars(design_spec))), names(data))
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
    extra      = extra
  ))
}


# === Reference picker -> tab_reg(ref =) ====================================================

# Fold the per-predictor reference picker (the `ref_levels` Array option) into tab_reg()'s `ref`
# named vector. Each element is list(var, ref); an entry with an explicit level contributes
# c(<var> = <level>). Returns NULL when nothing was picked (-> tab_reg() uses its default first-level
# references). Mirrors jmvtab_ref_vector() but has no "auto"/"tot" sentinels (a regression reference is
# always a factor LEVEL).
#' @keywords internal
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

# Fold the model-builder (`models` Array of Group{label, vars}) + the flat predictor pool into
# tab_reg()'s `predictors`. An EMPTY builder -> the flat pool = single model (a character vector, or
# NULL when the pool is empty too -> a NULL table + hint). >=1 card -> a NAMED LIST of character
# vectors = model-comparison mode (one effect column per model). Each card is intersected with the
# pool (pool order, dropping stale vars); a blank label becomes "model{i}" (friendlier than
# tab_reg()'s all-or-nothing rename); empty-var cards are dropped; if nothing survives -> the pool.
#' @keywords internal
#' @noRd
# Fold the interaction picker (the `crosses` Array of Group{var1, var2}) into the `a*b` keys
# tab_reg()'s `predictors` takes. Both variables must be in the pool; the FIRST is the modified
# one, which is the grammar's own reading of `a*b`.
#' @keywords internal
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

# One model's variables, with every applicable pair REPLACED by its key: an interaction supplies
# both parents, so listing them beside it is what tab_reg() refuses. A model that does not hold both
# is left alone, which is what makes a with/without comparison expressible from the "+" builder.
#' @keywords internal
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

jmvtab_reg_models <- function(models, pool, cross_keys = character(0)) {
  pool <- if (length(pool)) as.character(pool) else character()
  flat <- if (length(pool)) jmvtab_reg_cross_fold(pool, cross_keys) else NULL
  if (length(models) == 0L) return(flat)
  built  <- lapply(models, function(e)
    jmvtab_reg_cross_fold(intersect(pool, as.character(unlist(e$vars, use.names = FALSE))),
                          cross_keys))
  labels <- vapply(models, function(e) { v <- e$label; if (is.null(v)) "" else as.character(v) },
                   character(1))
  keep   <- vapply(built, length, integer(1)) > 0L
  built  <- built[keep]; labels <- labels[keep]
  if (length(built) == 0L) return(flat)
  blank  <- !nzchar(labels)
  labels[blank] <- paste0("model", seq_along(labels))[blank]
  stats::setNames(built, labels)
}

# THE `stats =` folder -- three controls, one argument (Phase 20c made the footer set, the model
# comparison and its baseline ONE `tab_reg(stats =)`; Phase 20g-i named the controls after it).
#   stats_compare  : the comparison KEY the user picks; the ComboBox values ARE the R keys, so
#                    "none" -> NULL = tab_reg()'s own default footer set, untouched.
#   stats_baseline : the baseline model POSITION, carried in the key's NAME when it is not the first
#                    (`c(compare_baseline = "2")` -- the grammar `ref = c(var = "level")` also uses).
#   stats_checks   : Phase 20f made the two checks that REFIT the model (linearity, proportional
#                    odds) opt-in, because they were 80-90 % of a build and the panel rebuilds on
#                    every option change. `"all"` is the one value that asks for everything, and it
#                    COMPOSES with a comparison key -- reg_resolve_stats() strips the comparison and
#                    hands the rest on, so c("all", "compare_baseline") is a full footer plus a test.
#' @keywords internal
#' @noRd
jmvtab_reg_stats <- function(compare, baseline, checks = FALSE) {
  cmp <- compare %||% "none"
  key <- if (!nzchar(cmp) || identical(cmp, "none")) {
    NULL
  } else if (identical(cmp, "compare_sequential")) {
    "compare_sequential"
  } else {
    # the spinner is a COLUMN POSITION; 1 is the default, and an unnamed key already means "the first"
    bl <- suppressWarnings(as.integer(baseline %||% NA))
    if (is.na(bl) || bl <= 1L) "compare_baseline" else c(compare_baseline = as.character(bl))
  }
  if (!isTRUE(checks)) return(key)
  if (is.null(key)) "all" else c("all", key)
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

# Fold the per-numeric-predictor shape picker (the jamovi `shape` Array of Group{var, shape}) into
# tab_reg()'s `shape`. Blank / "linear" entries are dropped (linear is the default and needs no
# entry); NULL when nothing was picked. Values come from VAR_SHAPES (R/var-shape.R), which is
# also what the .a.yaml offers -- one vocabulary, checked by test-jamovi-vocabulary.R.
#' @keywords internal
#' @noRd
jmvtab_reg_shape_vector <- function(shape) {
  if (length(shape) == 0L) return(NULL)
  get1 <- function(e, k) { v <- e[[k]]; if (is.null(v)) NA_character_ else as.character(v) }
  vars <- vapply(shape, get1, character(1), k = "var")
  shp  <- vapply(shape, get1, character(1), k = "shape")
  keep <- !is.na(vars) & nzchar(vars) & !is.na(shp) & nzchar(shp) & shp != "linear"
  if (!any(keep)) return(NULL)
  stats::setNames(shp[keep], vars[keep])
}

# Fold the per-numeric-predictor scaling picker (the jamovi `multiplier` Array of Group{var, k})
# into tab_reg()'s `multiplier`. Blank entries dropped; NULL when nothing set -> tab_reg's own default
# ("sd") applies. Mirrors jmvtab_reg_ref_vector().
# Phase 18z9: the keywords "sd" / "2sd" pass THROUGH as text (they used to be coerced with
# as.numeric() and silently dropped), so the picker can offer the per-SD scaling the R default uses.
# A character vector is returned as soon as one entry is a keyword -- tab_reg() parses both.
#' @keywords internal
#' @noRd
jmvtab_reg_mult_vector <- function(multiplier) {
  if (length(multiplier) == 0L) return(NULL)
  get1 <- function(e, k) { v <- e[[k]]; if (is.null(v)) NA_character_ else as.character(v) }
  vars <- vapply(multiplier, get1, character(1), k = "var")
  raw  <- trimws(vapply(multiplier, get1, character(1), k = "k"))
  kw   <- tolower(raw) %in% REG_MULTIPLIER_KEYWORDS   # Phase 19k: THE set, R/tab_reg.R's own
  num  <- suppressWarnings(as.numeric(raw))
  keep <- !is.na(vars) & nzchar(vars) & (kw | !is.na(num))
  if (!any(keep)) return(NULL)
  if (any(kw[keep])) stats::setNames(ifelse(kw[keep], tolower(raw[keep]), raw[keep]), vars[keep])
  else               stats::setNames(num[keep], vars[keep])
}


# === Per-outcome Model table -> tab_reg() args (Phase 15d / 15e) ==========================
# The Model table (the `family` / `outcome_level` / `trials` arrays) sets one of each per outcome.
# These three helpers resolve ONE outcome; jmvtab_reg_build() (Phase 15e) passes the resolved
# per-outcome VECTORS to ONE tab_reg() call -- a mixed table (several outcomes, different families)
# is one table, one column-group per outcome (no more grouping-by-family / tabxplor_tabs stacking).

# The chosen family for `dep` (an explicit non-blank pick) else auto-detected from the outcome.
#' @keywords internal
#' @noRd
jmvtab_reg_dep_family <- function(family, dep, data) {
  if (length(family)) for (e in family) {
    if (identical(as.character(e$var), dep) && length(e$family) && nzchar(as.character(e$family)))
      return(as.character(e$family))
  }
  reg_detect_family(data, dep)
}

# The level the user singled out for this outcome, or NA = the family's own default. What the level
# MEANS is the family's business (REG_FAMILIES$outcome_level: the MODELLED level for a binomial
# outcome, the BASELINE category for a multinomial one), which is why it travels as a level and not
# as a flag -- folding it into a logical is what made ANY pick model the SECOND level (Phase 20c).
#' @keywords internal
#' @noRd
jmvtab_reg_dep_level <- function(outcome_level, dep) {
  if (length(outcome_level)) for (e in outcome_level) {
    if (identical(as.character(e$var), dep) && length(e$level) && nzchar(as.character(e$level)))
      return(as.character(e$level))
  }
  NA_character_
}

# The number of trials the user typed for a binomial outcome, or NA = "take the observed maximum".
# Phase 19k: NA is now tab_reg()'s OWN spelling of that rule (`trials` accepts NA per outcome), so
# this helper stops applying it. It used to take max() itself for any integer outcome -- the same
# rule as R's `trials = TRUE`, but SILENTLY and on a different trigger: one rule, two semantics, and
# the jamovi one could not be reproduced from the R API.
#' @keywords internal
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

# Drive tab_reg() with the live fit cache injected. `opts` is the plain list the R6 backend's .opts()
# produces -- since Phase 20g-i a PASS-THROUGH: every jamovi option is named after the tab_reg()
# argument it drives, so what is left here is the folding of the per-variable picker ARRAYS into
# named vectors, and the per-outcome Model table into the family / outcome_level / trials vectors.
# Empty variable slots (jamovi passes partial selections mid-interaction) yield a NULL table -> the
# backend renders a friendly hint.
#' @keywords internal
#' @noRd
jmvtab_reg_build <- function(data, opts, store = NULL, use_cache = TRUE) {
  # ⚠ ALWAYS serial. The live cache normally forces it -- tab_parallel_workers() returns 0 whenever a
  # `cache_env` is present -- but in STAGED mode `use_cache` is FALSE, so `.fit_cache` is NULL and the
  # build would read getOption("tabxplor.parallel"): a user who set that option once would have jamovi
  # spawning daemons inside its own R process, for a UI that repaints on every click. The module never
  # dispatches; it is the interactive path.
  .old_par <- options(tabxplor.parallel = FALSE)
  on.exit(options(.old_par), add = TRUE)
  # DESIGN (Phase o): in a model COMPARISON the cache is worthless -- the reref digest fast-path is off
  # for comparisons (tab_reg's `reref` needs compare=="none"), so it only ever holds the RAW fits
  # (~10 MB each). Once persisted into cache_state$state they re-serialize on every UI round-trip -> the
  # freeze at 4 models (~40 MB). use_cache=FALSE (set by the backend in staged mode) fits without a cache
  # env and returns store=NULL, so nothing heavy is stored/serialized. Single-model use keeps the cache.
  #
  # ⚠ AND `stats_checks` TURNS IT OFF TOO (Phase 20g-i), because the digest fast path DISTILS THE FIT
  # AWAY: reg_check_rows() asks reg_checks_for(has_fit = !is.null(f$fit)), so with a live cache a
  # single-model table carries only the reference-invariant glance rows (n / lr_null / mcfadden_r2 /
  # aic / bic) and never the per-predictor global test or the model checks. That is the cache working
  # as designed -- a KB digest instead of a 10 MB fit -- but it makes "ask for the slow checks" a
  # promise the module could not keep. So the tick-box means what it says: the fit is kept, at the
  # price of a refit per edit. Default off = today's fast behaviour, byte-unchanged.
  use_cache <- use_cache && !isTRUE(opts$stats_checks)
  cache_env <- jmvreg_cache_env(if (use_cache) store else NULL)

  nz  <- function(x) if (length(x) && nzchar(as.character(x)[[1]])) as.character(x) else NULL
  dep   <- nz(opts$outcome)   # the key .opts() sets, already tab_reg()'s word
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
  # model comparison (a predictor-subset list) needs a single outcome -> a friendly NULL / hint
  # instead of tab_reg()'s abort while the user is still selecting.
  if (is.list(preds) && length(dep) > 1L) {
    return(list(tabs = NULL, store = cache_env$store, hits = 0L))
  }

  # Phase 15e: resolve each outcome's family / modelled level / trials from the Model table, then pass them
  # to ONE tab_reg() call as per-outcome vectors -- so several outcomes with DIFFERENT families render as
  # one mixed table (tab_reg builds one column-group per outcome). No more family-grouping / stacking.
  fams <- vapply(dep, function(d) jmvtab_reg_dep_family(opts$family, d, data), character(1))
  # Phase 20c: the picker asks for a LEVEL and tab_reg() now takes one, so it travels intact.
  # ⚠ it used to be folded into a logical ("did the user pick anything?"), which meant ANY pick
  # modelled the SECOND level -- so choosing the first one in the UI silently modelled the other.
  lvls <- vapply(dep, function(d) jmvtab_reg_dep_level(opts$outcome_level, d), character(1))
  # trials are binomial-only (grouped / summed-score); non-binomial outcomes never carry one.
  tris <- vapply(seq_along(dep), function(i) {
    if (identical(fams[i], "binomial")) jmvtab_reg_dep_trials(opts$trials, dep[i])
    else NA_integer_
  }, integer(1))

  fam_arg <- stats::setNames(fams, dep)
  lvl_arg <- if (all(is.na(lvls))) NULL else stats::setNames(lvls, dep)[!is.na(lvls)]
  # Phase 19k: every BINOMIAL outcome gets an entry -- the typed count, or NA = "take the observed
  # maximum", which is tab_reg()'s own rule and which it applies only where there IS one (a factor
  # or a 0/1 numeric outcome stays an ordinary binary logit). So the module states an intent and R
  # owns the resolution; it used to resolve the same rule here, silently and on a different trigger.
  tri_arg <- if (!any(fams == "binomial")) NULL else stats::setNames(as.integer(tris), dep)

  # Phase 19k: the UI speaks tab_reg()'s OWN estimand vocabulary -- `effect` (which contrast) x
  # `measure` (which measure) x `display` (the cell layout). The jmv_reg_estimand_opts() translator
  # 19e put here for the retired `exponentiate` / `at` / `estimate_display` options is DELETED with
  # them: no argument reaches tab_reg() through a second vocabulary any more.
  # ⚠ rlang::inject() + `!!` on the four VARIABLE ROLES: they are tidy-select since 22b-vi, and a
  # bare local (`dep`, `preds`) would be resolved against the DATA first -- a dataset column named
  # "dep" would hijack the argument. An injected value cannot be mistaken for a column name, which is
  # the same reason jmvtab-cache.R injects tab()'s roles.
  tabs <- rlang::inject(tab_reg(
    data,
    outcome      = !!dep,
    predictors   = !!preds,
    family       = fam_arg,
    wt           = !!nz(opts$wt),
    link         = opts$link    %||% "auto",
    measure      = opts$measure %||% "auto",
    effect       = opts$effect  %||% "auto",
    conf_level   = opts$conf_level,
    ci_method    = opts$ci_method,
    ref          = opts$ref,
    outcome_level = lvl_arg,
    tab_vars     = !!nz(opts$tab_vars),
    # a pass-through: the option's values ARE the argument's ("no" / "column" / "cell"), and an older
    # stored state may still carry the logical the checkbox used to send.
    empirical    = if (is.logical(opts$empirical)) isTRUE(opts$empirical)
                   else opts$empirical %||% "no",
    # Phase 20c: `stats` IS what the module sends now, because the model comparison is one of its
    # keys. NULL keeps its old meaning -- "the model-fit statistics that make sense for this family",
    # which is what the picker wants and what tab_reg() computes by default -- so an unset comparison
    # sends NULL exactly as before, and only a chosen one adds a key.
    stats        = jmvtab_reg_stats(opts$stats_compare, opts$stats_baseline, opts$stats_checks),
    display      = opts$display %||% "auto",
    shape        = jmvtab_reg_shape_vector(opts$shape),
    color        = opts$color,
    color_signif = opts$color_signif,
    stars        = isTRUE(opts$stars),
    na           = opts$na,
    # ⚠ [["n"]], never $n: `$` PARTIAL-MATCHES on a list, so `opts$n` would return `opts$na`.
    n            = opts[["n"]] %||% "range",
    cleannames   = opts$cleannames,
    subtext      = opts$subtext,
    multiplier   = jmvtab_reg_mult_vector(opts$multiplier),   # tab_reg skips mnl/ordinal specs per-spec
    trials       = tri_arg,
    .fit_cache   = if (use_cache) cache_env else NULL,
    # Phase 20g-ii: the per-predictor level-merge tick-boxes, folded by the SAME function jmvtab
    # uses. It needs no cache entry of its own: jmvreg_fit_key() fingerprints the PREPARED frame's
    # levels, and reg_prepare_data() merges before any fit -- so a merge changes the key by
    # construction.
    .levels_collapse = jmvtab_levels_collapse(opts$levels_collapse)
  ))

  cache_env$store <- jmvreg_cache_evict(cache_env$store)
  list(tabs = tabs, store = if (use_cache) cache_env$store else NULL, hits = cache_env$hits)
}
