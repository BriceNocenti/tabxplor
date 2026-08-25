# PURPOSE: The jmvtabreg live-UI fit cache + the engine-free build core jmvtab_reg_build().
# ROLE: Drives tab_reg() with a mutable cache environment injected via its internal `.fit_cache` arg.
#       Both fit paths fetch through reg_fit_cached() (R/reg-digest.R) -> jmvreg_cached(): ONE tier,
#       holding the DISTILLED fit record -- the `tabxplor_fitdigest` (R/reg-digest.R) plus everything
#       the eager stage computed off the live fit, with the fitted object and the model frame thrown
#       away. So the key carries NO ESTIMAND: `measure` / `effect` / `display` / `color` /
#       `conf_level` change without a refit, and the store holds kilobytes where it used to hold
#       ~10 MB fits.
#       ⚠ THE TIER HOLDS TWO KINDS OF RECORD OF ONE SHAPE: a MODEL fit, and each observed (crude)
#       univariable one (R/reg-empirical.R). They are told apart by the key alone -- a crude key is a
#       synthetic one-predictor spec whose `drop_extra` names the rest of the predictor set, since
#       that is what lands it on the model's own complete cases.
#       Content-addressed, schema-versioned, byte-bounded LRU, persisted to the hidden `cache_state`
#       Image $state.
# ROLE (build core): jmvtab_reg_build() is the pure, engine-free entry the R6 backend (R/jmvtabreg.b.R)
#       calls -- it maps the plain options list onto tab_reg(..., .fit_cache = cache_env) and returns
#       list(tabs, store, hits). Kept engine-free so it is unit-testable without a live jamovi session.
#       Picker folders map the hidden Array UI options into tab_reg() args: jmvtab_reg_ref_vector()
#       (references), jmvtab_reg_models() (the model-comparison "+" builder -> `predictors` list or the
#       flat pool -- and, with several outcomes, the FLAT pool even from one card, because a
#       comparison is one outcome), jmvtab_reg_cross_keys() (the interaction picker -> the `a*b`
#       keys `predictors` itself takes: an interaction IS a predictor, so there is no second
#       argument), jmvtab_reg_mult_vector() (numeric-predictor scaling -> `multiplier`),
#       jmvtab_shape_vector() (the per-variable cut -> `shape`, SHARED with jmvtab), and the Model
#       table's own jmvtab_reg_link_vector() (per-outcome link) beside its family / level / trials
#       readers.
#       ⚠ An interaction is DEFINED once (the `crosses` option) and TICKED per model (each card's own
#       `crosses` field), which is what makes an additive model expressible beside a crossed one --
#       the one comparison defining an interaction exists for. Only with NO card does a defined pair
#       apply on its own (jmvtab_reg_cross_fold), because there is then nowhere to tick it.
#       ⚠ `stats =` has NO folder and no control: since Phase 22g-ii tab_reg()'s own default already
#       compares several predictor subsets, so the panel asks nothing and NULL is what it sends.
# KEY CONSTRAINTS:
#   - jmvtabreg.h.R is GENERATED from jmvtabreg.a.yaml (jmvtools::prepare()); never hand-edit it.
#   - Persist plain lists -- NEVER a live object bound to an env. A digest is one by construction
#     (reg_digest_terms() rebases the terms object's environment for exactly this reason).
#   - The REFERENCE is in the key for free: reg_resolve_fit_plan() relevels the data before the fit,
#     and jmv_col_fp() fingerprints a column's levels -- so a reference change is an honest refit.
#     The level ORDER is NOT: it reaches tab_reg() as `.levels_order`, a display permutation of the
#     row skeleton, so a ▲/▼ move touches no column and cannot move a key.
#   - Rides the SHARED cache kernel (R/jmvtab-cache.R: jmv_cache_config + jmv_store_*); only the
#     store stays decoupled (its tier + $state differ from the crosstab store).
#   - INHERITS jmv_col_fp()'s value-edit blind spot (a same-shape edit preserving class / factor levels /
#     NA-count is NOT caught -> can serve a STALE fit after a data edit; best-effort, self-heals on the
#     next structural change). Escape hatch: the JMV_FULL_HASH constant in R/jmvtab-cache.R.
#     forces a full-value column hash (slower, exact) in BOTH modules -- see ?tabxplor-options.
#   - THE STAGED COMPARISON'S RENDER IS KEPT TWICE, and both copies earn it: jmvcore's `$state`
#     survives an engine reset but warns past 5e5 compressed bytes, while JMVREG_RENDERS (below)
#     survives only the process. The state carries the signature always and the HTML while it fits.
# See: dev/tabxplor_2.0.0_jamovi_dev.md (§ Phase 22g-x) ; CLAUDE.md > 2.0.0 roadmap > Phase 22g-x.


# === Constants + config ====================================================================
# The reg store rides the shared cache kernel (R/jmvtab-cache.R: jmv_cache_config + jmv_store_*) with
# its own 2-tier config; only the store is decoupled (its tiers + $state differ from the crosstab store).
JMVREG_CACHE_SCHEMA <- 10L  # bump on any store-shape change -> discard stale stores
# 10 (Phase 22i): `multiplier` LEAVES the key. It scales the tidy at reg_tidy_finalize(), beside the
#   interval and the exponentiation, so it cannot move a fit -- and a scaling pick is now a HIT that
#   re-reports rather than a refit. The stored record's `tidy_native` is genuinely native, which is
#   why a store written before this must be discarded.
# 9 (Phase 22g-x): the CRUDE fits share the tier, under a synthetic one-predictor spec key -- so the
#   `"fit"` tier now holds two kinds of record of the same shape, told apart by the key alone. And
#   `drop_extra` becomes a NAMED, FINGERPRINTED key member where `na_shared_vars` rode in `extra` as
#   names only: it decides the complete-case population, so a value edit to one of those columns
#   must move the key.
# 8 (Phase 22j): ONE tier, holding the DISTILLED fit record (digest + the eager footer rows, no fit
#   and no frame). The key drops the estimand -- `sp_dox`, `conf_level`, `effect`, `measure`,
#   `display` all leave it -- and gains `stats` (which decides which eager rows exist) and
#   `na_shared_vars` (which changes the complete-case set without touching `data`).
#   ⚠ dropping `measure` is safe only because `family` is already a key member: `sp$fit_family` IS
#   the link key, so `rr` / `rd` / `mr` are distinct families here, not one family reported three
#   ways.
# 7 (Phase 22b-ix): `crosses` joins jmvreg_fit_key()'s `extra`. A nested cross is a formula TERM,
#   not a column, so jmv_col_fp() cannot see it and a stale hit would be a wrong table.
# 6 (Phase 20g-i): the singled-out outcome level's key member is named `outcome_level`.
# 5 (Phase 19k): `shape` and the measure-valued `color` reach the build from the UI.
# 4 (Phase 19e): the raw-fit key's `extra` carried the ESTIMAND. 3: unified kernel entry shape.
#   2: table attrs merged into one `meta` list.
# A DISTILLED record is kilobytes (a 21k-row binomial glm serialises at 2.4 MB, its digest at
# 0.03 MB), so one modest ceiling holds a whole panel's worth.
JMVREG_CFG <- jmv_cache_config(
  schema      = JMVREG_CACHE_SCHEMA,
  entry_bytes = c(fit = 2L * 1024L * 1024L),
  store_bytes = 32L * 1024L * 1024L   # whole-store budget (serialized to $state every run -> LRU-bounded)
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

# Fetch-or-compute-and-put on the reg store; the tier ceiling + LRU come from JMVREG_CFG.
#' @keywords internal
#' @noRd
jmvreg_cached <- function(cache_env, tier, key, compute_fn)
  jmv_store_cached(JMVREG_CFG, cache_env, tier, key, compute_fn)

# The content key for one fit -- a model spec, or the synthetic one-predictor spec a CRUDE refit is:
# everything that decides WHICH MODEL IS FITTED, and nothing that decides how it is reported. The
# per-column fingerprint (jmv_col_fp) of the model + design variables captures a weight change and
# the reference relevel; `extra` carries the rest.
# ⚠ `drop_extra` IS A KEY MEMBER, AND ITS COLUMNS ARE FINGERPRINTED. It names the variables whose
# missing values narrow this fit's complete-case population without appearing in its formula -- the
# other models' predictors under `na = "drop_all"`, and, for a crude fit, the WHOLE predictor set
# minus this one. Naming them is not enough: a value edit to one of those columns moves the domain,
# so they join `used` and are hashed like any other.
#' @keywords internal
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

# Fold the interaction picker (the `crosses` Array of Group{var1, var2}) into the `a*b` keys
# tab_reg()'s `predictors` takes. Both variables must be in the pool; the FIRST is the modified
# one, which is the grammar's own reading of `a*b`.
# ⚠ `a != b` is the LAST of three defences against `race*race`, which tab_reg() refuses as
# meaningless: the picker's second drop-down does not offer side 1's variable, reconcileCrosses()
# drops a colliding stored pair, and this line is what keeps the R boundary from trusting either.
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

# The two parents a set of `a*b` keys names. An interaction SUPPLIES both, so listing one beside its
# own key is what reg_parse_crosses() refuses -- which is why every caller subtracts these.
#' @keywords internal
#' @noRd
jmvtab_reg_cross_parents <- function(keys) {
  if (length(keys) == 0L) return(character(0))
  unique(unlist(strsplit(keys, "*", fixed = TRUE), use.names = FALSE))
}

# The ZERO-CARD fold: with no model card there is nowhere to tick an interaction, so every DEFINED
# pair applies to the single live model and replaces its two parents in place.
# ⚠ A card does NOT go through this: since Phase 22g-viii it states its own interactions (its
# `crosses` field), which is what makes an additive model expressible beside a crossed one. Folding
# a card here is what made every card holding both parents BECOME the interaction model.
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

# Fold the model-builder (`models` Array of Group{label, vars, crosses}) + the flat predictor pool
# into tab_reg()'s `predictors`. An EMPTY builder -> the flat pool, cross-folded = single model (a
# character vector, or NULL when the pool is empty too -> a NULL table + hint). >=1 card -> a NAMED
# LIST of character vectors = model-comparison mode (one effect column per model). Each card is its
# `vars` intersected with the pool (pool order, dropping stale vars), then its OWN `crosses` keys
# appended; a blank label becomes "model{i}" (friendlier than tab_reg()'s all-or-nothing rename);
# empty cards are dropped; if nothing survives -> the pool.
#
# ⚠ `flatten` is the SEVERAL-OUTCOMES rule. `is_comparison <- is.list(predictors)` (R/reg-resolve.R),
# and a comparison must have ONE outcome -- so a single card beside two outcomes has to arrive as a
# character vector, i.e. as the ordinary per-outcome table it is. Its typed NAME is what that costs;
# the column is named by its outcome there anyway. TWO cards and two outcomes is still refused, which
# is right: there is no such table.
jmvtab_reg_models <- function(models, pool, cross_keys = character(0), flatten = FALSE) {
  pool <- if (length(pool)) as.character(pool) else character()
  flat <- if (length(pool)) jmvtab_reg_cross_fold(pool, cross_keys) else NULL
  if (length(models) == 0L) return(flat)
  built  <- lapply(models, function(e) {
    v <- intersect(pool, as.character(unlist(e$vars, use.names = FALSE)))
    # only a key still DEFINED in `crosses` counts, so a deleted pair cannot survive in a card...
    k <- intersect(cross_keys, as.character(unlist(e$crosses, use.names = FALSE)))
    # ...and a parent named beside its own key is dropped, because the producer refuses that pair.
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

# Phase h: the staged-comparison gate. A model comparison (>=2 folded models) is heavy -- refitting
# every model on each live edit is what froze the panel, so its table recomputes ONLY on the Run/Export
# action (jmvtabreg.b.R). jmvtab_reg_staged() reuses jmvtab_reg_models() so the predicate matches the
# model set tab_reg() actually sees. jmvtab_reg_compare_sig() fingerprints the resolved build/display
# options (the `.opts()` list, which already excludes the action + export controls), so `.run()` can
# tell an unchanged/just-computed table (re-serve) from an outdated one (banner + Run prompt).
#' @keywords internal
#' @noRd
# ⚠ `cross_keys` is not decoration: a card holding ONLY an interaction has an empty `vars`, and
# without the keys it would be dropped as empty -- so the predicate and `.opts()` must be given the
# same ones, or a two-model comparison would run live instead of staged.
jmvtab_reg_staged <- function(models, predictors, cross_keys = character(0)) {
  preds <- jmvtab_reg_models(models, predictors, cross_keys)
  is.list(preds) && length(preds) >= 2L
}

#' @keywords internal
#' @noRd
jmvtab_reg_compare_sig <- function(opts) jmv_hash(opts)

# THE STAGED COMPARISON'S RENDER, kept twice -- and both copies earn their place.
#
# `$state` on a hidden Image is the one thing that survives an engine RESET, so the render goes
# there; but jmvcore compresses a state and prints "state object ... is too large" past 5e5 bytes
# (its own documented ceiling), and a wide multi-model table's HTML can reach it. So the state
# carries the render only while it fits, and this process-local mirror carries it always: within one
# live engine the last two comparisons re-serve from here even if the state came back empty. Two
# entries, because a user flips between two model sets; keyed on the signature, so a stale render can
# never be served for a changed one.
#' @keywords internal
#' @noRd
JMVREG_RENDERS <- new.env(parent = emptyenv())

# jmvcore's own limit, in ResultsElement$asProtoBuf(); past it the state is still sent, with a
# warning printed to the engine log -- so this is a ceiling worth staying under, not a hard error.
#' @keywords internal
#' @noRd
JMVREG_STATE_MAX <- 5e5

#' @keywords internal
#' @noRd
jmvtab_reg_render_store <- function(sig, html) {
  keys <- ls(JMVREG_RENDERS)
  if (length(keys) >= 2L && !sig %in% keys) rm(list = keys[[1L]], envir = JMVREG_RENDERS)
  assign(sig, html, envir = JMVREG_RENDERS)
  # only what fits rides in `$state`; the signature always does, since it is what tells an
  # unchanged table from an outdated one and it costs nothing.
  if (nchar(html, type = "bytes") > JMVREG_STATE_MAX) list(sig = sig)
  else list(sig = sig, html = html)
}

# The last render for `sig`, from the state if it carried one and from this process otherwise.
#' @keywords internal
#' @noRd
jmvtab_reg_render_fetch <- function(cst) {
  if (is.null(cst) || is.null(cst$sig)) return(NULL)
  cst$html %||% (if (exists(cst$sig, envir = JMVREG_RENDERS, inherits = FALSE))
                   get(cst$sig, envir = JMVREG_RENDERS) else NULL)
}

# Fold the per-numeric-variable shape picker (the jamovi `shape` Array of Group{var, shape}) into
# `shape =`. Blank / "linear" / "auto" entries are dropped -- "linear" is a col_var's default and
# "auto" is not a value the parser accepts at all, it is the ABSENCE of one; NULL when nothing was
# picked. Values come from VAR_SHAPES (R/var-shape.R), so the picker and the argument share one
# vocabulary. ⚠ Producer-agnostic on purpose: BOTH backends call it (jmvtab since 22g-iii).
#' @keywords internal
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

# The link the user picked for this outcome, as tab_reg()'s own named vector. "auto" and blank
# entries are DROPPED: "auto" is the default, so an entry for it would only make the fit key move.
# Mirrors jmvtab_shape_vector(), which drops "linear" for the same reason.
#' @keywords internal
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
  # DESIGN: a model COMPARISON is a test BETWEEN the fitted objects (reg_compare_rows), so its
  # specs cannot be served from a distilled record -- reg_fit_cacheable() refuses them, and the
  # backend's staged mode drops the store entirely so nothing heavy is serialized either.
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

  # The ▲/▼ bar. It is a DISPLAY order and reaches `tab_reg()` as such (`.levels_order` -> the row
  # skeleton), never as a relevel of the data: a predictor is fitted under treatment contrasts, so
  # its order decides ONLY which level is the reference -- and that is `ref =`, which the panel writes
  # off the order's first entry and which relevels the data on its own. So a reorder no longer moves
  # the fit key, i.e. it is a cache HIT, while choosing a baseline is still an honest refit.
  # ⚠ TRANSLATED TO MERGED NAMES HERE, through the same jmv_order_after_collapse() jmvtab uses: the
  # skeleton is built after `.levels_collapse` has run, so a raw level's row IS the merged run that
  # swallowed it, and an untranslated order would silently name levels the table does not have.
  collapse <- jmvtab_levels_collapse(opts$levels_collapse)
  ord      <- jmv_order_after_collapse(jmvtab_levels_order(opts$levels_order), collapse)

  if (is.null(dep) || is.null(preds)) {
    return(list(tabs = NULL, store = cache_env$store, hits = 0L))
  }
  # model comparison (a predictor-subset list) needs a single outcome -> a friendly NULL / hint
  # instead of tab_reg()'s abort while the user is still selecting.
  if (is.list(preds) && length(dep) > 1L) {
    return(list(tabs = NULL, store = cache_env$store, hits = 0L))
  }

  # Resolve each outcome's family / link / modelled level / trials from the Model table, then pass
  # them to ONE tab_reg() call as per-outcome vectors -- so several outcomes with DIFFERENT families
  # (and links) render as one mixed table, tab_reg building one column-group per outcome.
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

  # ...and the link the same way. It is the only one of the four with a value that must NOT reach
  # the argument ("auto" IS the default), so the folder drops those entries instead of sending them.
  lnk_arg <- jmvtab_reg_link_vector(opts$link)

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
    # `link` is per outcome (the Model table's second headed column): a named vector, or NULL
    # where every outcome is on "auto" -- which IS what "auto everywhere" means.
    link         = lnk_arg %||% "auto",
    measure      = opts$measure %||% "auto",
    effect       = opts$effect  %||% "auto",
    conf_level   = opts$conf_level,
    ci_method    = opts$ci_method,
    ref          = opts$ref,
    outcome_level = lvl_arg,
    tab_vars     = !!nz(opts$tab_vars),
    # a pass-through: the option's values ARE the argument's ("no" / "column" / "cell"), and an older
    # stored state may still carry the logical the checkbox used to send.
    # ⚠ Phase 22g-iii: a TICK-BOX, so TRUE is tab_reg()'s own default and R decides WHERE the crude
    # effect goes. A character still passes through (the R API's four modes, and any `.omv` saved
    # while the option was a List); NULL -- the generated .h.R lagging a .a.yaml edit -- takes the
    # argument's default, which is TRUE.
    empirical    = if (is.null(opts$empirical)) TRUE
                   else if (is.logical(opts$empirical)) isTRUE(opts$empirical)
                   else opts$empirical,
    # ⚠ NO `stats =`: Phase 22g-iii deleted the three controls that folded into it. NULL is
    # tab_reg()'s own default -- the model-fit statistics that make sense for this family, PLUS the
    # automatic comparison 22g-ii installed (sequential where the subsets nest, else against the
    # first). A picker offering "none" would have named the opposite of what it did.
    display      = opts$display %||% "auto",
    # a FLOOR, so 0 (the option's default) means "every measure keeps its own precision".
    digits       = opts$digits %||% 0L,
    shape        = jmvtab_shape_vector(opts$shape),
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
    .levels_collapse = collapse,
    # the ▲/▼ order, as DISPLAY: it permutes the row skeleton and never the data, so it is out of the
    # fit key by construction (see the fold above).
    .levels_order    = ord
  ))

  cache_env$store <- jmvreg_cache_evict(cache_env$store)
  list(tabs = tabs, store = if (use_cache) cache_env$store else NULL, hits = cache_env$hits)
}
