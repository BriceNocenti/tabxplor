# PURPOSE: The jmvtab live-UI multi-tier cache (Phase 7e) -- a content-addressed store that lets
#          each button change in the jamovi module redo only what genuinely changed.
# ROLE: Drives the SAME five-stage pipeline tab() uses (tab_setup -> tab_prepare_pop -> tab_aggregate
#       -> tab_transform -> tab_assemble). jmvtab_build() calls tab() with a mutable `cache_env`
#       injected via `.cache`; the aggregate stage's hook (tab_aggregate, R/tab.R) delegates here to
#       jmv_cache_aggregate(), which builds per-(row_var x col_var) count aggregates + per-row_var
#       moment aggregates + tier-2 test keys with content-addressed reuse, mutating cache_env$store.
#       NO math is forked -- the leaves (tab_plain/tab_num) are reused verbatim, and since Phase 19k
#       NO RULE is mirrored either: the population descriptor is tab_cache_keys(), the digits floor
#       num_digits_floor(), the display writer tab_apply_display(), the `ci` anchor
#       resolve_leaf_ci(). The option NAMES and VALUES are tab()'s own (test / display / ref2 / ci /
#       the full-word colour measures), so nothing is translated at this boundary any more.
# KEY CONSTRAINTS:
#   - Persist tiers 1 (aggregates) + 2 (omnibus tests) + 3 (Phase 7f built ARMED tables, stored since
#     Phase 9b-7 as the CARRIER = plain field-frames, re-painted / re-ref'd on read, NOT a live tab).
#   - Store atomic-vector lists, NEVER a live data.table (readRDS breaks .internal.selfref) / a live
#     tab; rebuild with data.table::setDT() / fmt_wrap() on read; preserve factor level order.
#   - Aggregate keyed on FULL names (cleannames is display-tier), NA-kept, raw/full levels
#     (defer_level_merge). na keep/drop SHARE the factor aggregate; numeric na is in the key.
#   - Level reordering (Phase 7g-ii): the STORED aggregate blob stays at RAW level order; a reorder
#     relevels only the in-memory aggregate + ctx$data POST-fetch (jmv_relevel_cols), so it is a
#     tier-3 input (tiers 1-2 reused). Never bake a reorder into the persisted blob.
#   - Tier-4 (always re-applied, never baked into the carrier): digits, colour, display, cleannames,
#     n_min and -- since 19k -- `anova`, because both one-way F rows are stored in `test` and the
#     p-value line is materialised at DISPLAY. What the RE-REF recomputes may differ between two
#     tuples (ref / ref2 / the interval geometry / the CI method); what it copies must not.
#   - Byte-identical to tab(cleannames = FALSE, levels via defer_level_merge) -- locked by
#     test-jmvtab-cache.R; the option VOCABULARY is locked by test-jamovi-vocabulary.R. First cut: exact-grain keying (grain-superset rollup deferred), simple
#     byte-bounded LRU (byte-precise accounting deferred).
# See: dev/tabxplor_jmvtab_cache_design.md ; CLAUDE.md > 2.0.0 roadmap > Phase 7e.


# === Cache kernel (shared by the jmvtab crosstab store AND the jmvtabreg fit store) =========
#
# ONE byte-bounded LRU store, parameterised by a per-module CONFIG (schema + tier names + per-tier
# byte ceiling + whole-store budget). Both modules keep their OWN store (their tiers are different --
# crosstab: agg/test/tab3 ; reg: digest/fit -- and their persisted $state is decoupled), but share
# this implementation instead of the two drifting copies that predated Phase 17i.
#
# Canonical entry shape: list(value = <payload>, bytes = <serialized size>, seq = <LRU clock stamp>).
# `clock` is a monotone LOGICAL counter (never Sys.time -- that would break determinism/reproducibility).
#
# DESIGN: two access patterns, kept deliberately distinct (do NOT "unify" them -- it moves a golden):
#   * jmv_store_fetch/put  = FUNCTIONAL, store threaded by return value, clock bumped on EVERY touch
#     (incl. misses). The crosstab store uses these -- its fetch-or-compute is interleaved with the
#     data.table rebuild + per-row_var key accumulation, so it cannot collapse into a compute_fn closure.
#   * jmv_store_cached     = ENV-MUTATING fetch-or-compute, clock bumped only on a hit or a store (NOT
#     a bare miss). The reg store uses this -- its hit/miss tallies + eviction are byte-locked by
#     test-jmvtabreg-cache.R to this exact semantics.

# Config for one store: `entry_bytes` is a NAMED numeric vector (names = the tiers); the tier name
# selects the per-entry ceiling, so no per-put max_bytes argument is needed.
#' @keywords internal
#' @noRd
jmv_cache_config <- function(schema, entry_bytes, store_bytes) {
  list(schema = schema, tiers = names(entry_bytes),
       entry_bytes = entry_bytes, store_bytes = store_bytes)
}

# A fresh empty store: schema + clock + one empty named list per tier.
#' @keywords internal
#' @noRd
jmv_store_new <- function(cfg) {
  c(list(schema = cfg$schema, clock = 0L),
    stats::setNames(rep(list(list()), length(cfg$tiers)), cfg$tiers))
}

# Restore-or-reset: a NULL state (first run) or a schema mismatch (module upgraded between sessions)
# yields a fresh store rather than a stale-shaped deserialization.
#' @keywords internal
#' @noRd
jmv_store_migrate <- function(cfg, store) {
  if (is.null(store) || !is.list(store) || !identical(store$schema, cfg$schema)) {
    return(jmv_store_new(cfg))
  }
  store
}

# A mutable cache environment: the migrated store + a hit / miss tally (diagnostics + tests). Used by
# the reg module (passed to tab_reg() as `.fit_cache`); the crosstab build wraps its own env inline.
#' @keywords internal
#' @noRd
jmv_store_env <- function(cfg, store = NULL) {
  ce <- new.env(parent = emptyenv())
  ce$store  <- jmv_store_migrate(cfg, store)
  ce$hits   <- 0L
  ce$misses <- 0L
  ce
}

# Fetch an entry, refreshing its LRU stamp on a hit. Bumps the clock on every touch (incl. a miss).
# Returns list(hit, value, store) so the (bumped) store is threaded back.
#' @keywords internal
#' @noRd
jmv_store_fetch <- function(cfg, store, tier, key) {
  store$clock <- store$clock + 1L
  e <- store[[tier]][[key]]
  if (is.null(e)) return(list(hit = FALSE, value = NULL, store = store))
  e$seq <- store$clock
  store[[tier]][[key]] <- e
  list(hit = TRUE, value = e$value, store = store)
}

# Insert/replace an entry unless it exceeds the tier's byte ceiling (recomputing one scan next run
# beats persisting a large blob forever). The ceiling comes from cfg$entry_bytes[[tier]].
#' @keywords internal
#' @noRd
jmv_store_put <- function(cfg, store, tier, key, value) {
  store$clock <- store$clock + 1L
  b <- length(serialize(value, connection = NULL))
  if (b > cfg$entry_bytes[[tier]]) return(store)
  store[[tier]][[key]] <- list(value = value, bytes = b, seq = store$clock)
  store
}

# Evict least-recently-used entries across ALL tiers until the total serialized size is under budget.
# DESIGN: O(n log n) -- flatten the tiers ONCE, sort by seq, drop oldest-first in a single pass (the
# old reg copy re-indexed every tier on every eviction -> O(n^2); this is the surviving implementation).
#' @keywords internal
#' @noRd
jmv_store_evict <- function(cfg, store) {
  tier_ent <- function(tier) lapply(names(store[[tier]]), function(k)
    list(tier = tier, key = k, seq = store[[tier]][[k]]$seq, bytes = store[[tier]][[k]]$bytes))
  ent <- unlist(lapply(cfg$tiers, tier_ent), recursive = FALSE)
  if (length(ent) == 0L) return(store)
  total <- sum(vapply(ent, function(e) e$bytes, numeric(1)))
  if (total <= cfg$store_bytes) return(store)
  ord <- order(vapply(ent, function(e) e$seq, numeric(1)))  # oldest first
  for (i in ord) {
    if (total <= cfg$store_bytes) break
    e <- ent[[i]]
    store[[e$tier]][[e$key]] <- NULL
    total <- total - e$bytes
  }
  store
}

# Env-mutating fetch-or-compute-and-put. Returns compute_fn() unchanged when cache_env is NULL (so the
# caller is usable without a cache). On a hit: refresh the LRU stamp, tally, return the value. On a
# miss: compute, serialize to measure bytes, store only if under the tier ceiling (an oversized entry
# is recomputed next time -- graceful, never an error), evict to the store budget. Clock is bumped on a
# hit or a store, NOT on a bare miss (see DESIGN above).
#' @keywords internal
#' @noRd
jmv_store_cached <- function(cfg, cache_env, tier, key, compute_fn) {
  if (is.null(cache_env)) return(compute_fn())
  store <- cache_env$store
  hit   <- store[[tier]][[key]]
  if (!is.null(hit)) {
    store$clock <- store$clock + 1L
    hit$seq     <- store$clock
    store[[tier]][[key]] <- hit
    cache_env$store <- store
    cache_env$hits  <- cache_env$hits + 1L
    return(hit$value)
  }
  cache_env$misses <- cache_env$misses + 1L
  value <- compute_fn()
  bytes <- length(serialize(value, connection = NULL))
  if (bytes <= cfg$entry_bytes[[tier]]) {
    store$clock <- store$clock + 1L
    store[[tier]][[key]] <- list(value = value, bytes = bytes, seq = store$clock)
    store <- jmv_store_evict(cfg, store)
    cache_env$store <- store
  }
  value
}


# === Constants + config (jmvtab crosstab store) ============================================
JMVTAB_CACHE_SCHEMA <- 17L   # bump on any store-shape change -> discard stale stores
# 17 (Phase 19k): the option vocabulary itself moved (`chi2` -> `test`, `OR` retired onto display /
#   ref2, `anova` re-applied at tier 4, the four method_* keys re-applied instead of structural), so
#   a stale entry's base key describes a different question.
                            # 16 = Phase 19d-tail: the tier-3 TUPLE replaces `display` + `or` with the
                            #     one `comparison` they both encoded. A stale tuple has the old keys,
                            #     so every comparison would read as a mismatch (or, worse, match).
                            # 13 = Phase 19b (KEY 2): a tier-3 carrier's per-column `meta` list carries
                            #     `scale` + `pct_base` instead of `type` + `ci_type`, and gains
                            #     `ci_method`. A pre-13 carrier has the old names -> unusable.
                            # 12 = Phase 18z16-iiiii: the tier-3 tuple folds the five method_* keys
                            #     into one `ci_method` vector, and the columns carry degf/basis.
                            # 9 = Phase 18z8: the fmt record gained the `gap_se` field, so a tier-3
                            #     carrier stored by an older session has a 20-field frame.
                            # (8: Phase 18z5 -- the `obs` field | 7: Phase 18j -- the `test`
                            #  tibble gained effect_size/es_type/pvalue_exact)
  #   history: 2 = Phase 7f tier-3 `tab3` tier | 3 = Phase 9b-7 carrier | 4 = Phase 17b meta-merge
  #            5 = Phase 17d clean colour attrs | 6 = Phase 17i unified kernel entry list(value,bytes,seq)

# 3 tiers: agg/test aggregates (tiers 1-2) + tab3 armed CARRIERS (all 21 fmt fields -> looser ceiling).
JMVTAB_CFG <- jmv_cache_config(
  schema      = JMVTAB_CACHE_SCHEMA,
  entry_bytes = c(agg = 512L * 1024L, test = 512L * 1024L, tab3 = 2L * 1024L * 1024L),
  store_bytes = 12L * 1024L * 1024L   # whole-store budget (serialized every run -> keep bounded)
)


# === Store lifecycle (thin wrappers -> the shared kernel with JMVTAB_CFG) ===================
# These keep the crosstab call sites + tests calling jmv_cache_* unchanged. tab3 (Phase 7f): per
# base-config built ARMED tables, stored since Phase 9b-7 as the CARRIER (plain field-frames,
# jmv_carrier_unwrap), re-painted (exact-tuple hit) / re-ref'd (rerefable tuple) on read -- so
# display/colour/reference toggles skip the O(cells) rebuild.
#' @keywords internal
#' @noRd
jmv_cache_new <- function() jmv_store_new(JMVTAB_CFG)

#' @keywords internal
#' @noRd
jmv_cache_migrate <- function(store) jmv_store_migrate(JMVTAB_CFG, store)

#' @keywords internal
#' @noRd
jmv_cache_fetch <- function(store, tier, key) jmv_store_fetch(JMVTAB_CFG, store, tier, key)

#' @keywords internal
#' @noRd
jmv_cache_put <- function(store, tier, key, payload) jmv_store_put(JMVTAB_CFG, store, tier, key, payload)

#' @keywords internal
#' @noRd
jmv_cache_evict <- function(store) jmv_store_evict(JMVTAB_CFG, store)


# === Hashing ================================================================================

# Content hash of an arbitrary R object (rlang::hash = xxHash, an existing dependency).
#' @keywords internal
#' @noRd
jmv_hash <- function(x) rlang::hash(x)

# Cheap structural fingerprint of ONE column: class / factor levels / NA-count. Keys are built from
# the fingerprints of a pair's OWN columns (not the whole selection), so adding an unrelated variable
# does NOT invalidate other pairs (the add-a-variable reuse goal). Catches relabels, missingness
# edits, type changes. Blind spot (design 7): a same-shape value edit preserving levels + NA-count is
# not caught (best-effort; self-heals on the next change). Opt-in full-value hash for the paranoid.
#' @keywords internal
#' @noRd
jmv_col_fp <- function(col) {
  if (isTRUE(getOption("tabxplor.jmv_full_hash", FALSE))) return(jmv_hash(col))
  jmv_hash(list(class(col), if (is.factor(col)) levels(col) else NULL, sum(is.na(col))))
}

# Per-column fingerprint map over the selected variables, computed once on the original data.
#' @keywords internal
#' @noRd
jmv_fp_map <- function(data, used_vars) {
  used_vars <- unique(intersect(used_vars, names(data)))
  stats::setNames(lapply(used_vars, function(v) jmv_col_fp(data[[v]])), used_vars)
}

# The population tag for the tier-1 key prefix. na in {keep, drop} -> "full" (factor aggregates are
# NA-kept and shared across keep/drop). drop_all / common_base -> a hash of their population-defining
# variables' fingerprints (they legitimately break per-pair reuse -- design 5).
#' @keywords internal
#' @noRd
jmv_pop_tag <- function(population, fp_map, nrow_data) {
  if (is.character(population) && identical(population, "full")) {
    return(list("full", nrow_data))
  }
  list(population$mode, nrow_data,
       lapply(sort(unique(population$vars)), function(v) fp_map[[v]]))
}


# === data.table <-> atomic-vector-list bridge =============================================

# A keyed/aggregated data.table -> a plain named list of atomic vectors (factors kept as factors, so
# level order survives serialization). Drops the data.table class + selfref (unsafe to persist).
#' @keywords internal
#' @noRd
jmv_dt_to_cols <- function(dt) as.list(dt)

# Rebuild a data.table from stored columns. as.data.table() copies, so the cached list is never
# aliased into a live data.table (which would corrupt it on the next `:=`).
#' @keywords internal
#' @noRd
jmv_cols_to_dt <- function(cols) data.table::as.data.table(cols)


# === STAGE 3 replacement: content-addressed tier-1 aggregates + tier-2 test keys ==========

# Drop-in for tab_aggregate() when a jmvtab cache_env is present. Builds, per (row_var x factor
# col_var), the count margin tab_plain(.fine=) marginalises (byte-identical to the fused .fine); per
# row_var, the moment aggregate tab_num(.fine=) adopts; and per row_var the tier-2 test key + a hit
# lookup. Mutates ctx$cache_env$store/hits. Returns ctx with fine_fused (per-pair list) / fine_num /
# cached_tests / tier2_keys set -- the same downstream contract tab_transform() expects.
#' @keywords internal
#' @noRd
jmv_cache_aggregate <- function(ctx) {
  ce    <- ctx$cache_env
  store <- ce$store
  data  <- ctx$data

  row_vars      <- as.character(ctx$row_vars)
  col_vars      <- as.character(ctx$col_vars)
  # Phase 19i: the SETTINGS SPINE is the only carrier of these -- the flat ctx duplicates are gone.
  fct_cols      <- col_vars[ctx$settings$cols$is_text]
  num_cols      <- col_vars[ctx$settings$cols$is_num]
  tab_vars      <- as.character(ctx$tab_vars)
  wt            <- ctx$wt                       # symbol or character()
  weighted      <- length(wt) != 0L
  wt_chr        <- if (weighted) as.character(wt) else ""
  grain         <- sort(tab_vars)
  other         <- ctx$other_if_less_than
  fp            <- ce$fp_map                    # per-column fingerprints (built in jmvtab_build)
  pop_tag       <- jmv_pop_tag(ctx$cache_keys$tier0$population, fp, ce$nrow)
  grain_fp      <- lapply(grain, function(g) fp[[g]])
  wt_fp         <- if (weighted) fp[[wt_chr]] else NULL

  agg_hits  <- logical(0)
  test_hits <- logical(0)

  # --- Tier 1a: per-pair FACTOR count aggregates --------------------------------------------
  fine_fused <- NULL
  fct_keys_by_rv <- stats::setNames(vector("list", length(row_vars)), row_vars)
  if (length(fct_cols) > 0L) {
    fine_fused <- list()
    for (rv in row_vars) {
      for (cv in fct_cols) {
        # Self-crosstab (col_var is also a row/tab var): skip caching -> fine_for_pair() returns
        # NULL -> tab_plain() raw-scans (the _colvarbis machinery only runs on the raw path).
        if (cv %in% c(rv, tab_vars)) next
        key <- jmv_hash(list("fct", pop_tag, rv, fp[[rv]], cv, fp[[cv]],
                             grain, grain_fp, wt_chr, wt_fp, other))
        fct_keys_by_rv[[rv]] <- c(fct_keys_by_rv[[rv]], key)
        got <- jmv_cache_fetch(store, "agg", key)
        store <- got$store
        agg_hits[[paste(rv, cv, sep = "\r")]] <- got$hit
        if (got$hit) {
          pair <- jmv_cols_to_dt(got$value$cols)
        } else {
          keycols <- c(tab_vars, rv, cv)
          dt <- data.table::as.data.table(data[c(keycols, if (weighted) wt_chr)])
          # Phase 18z16-iiiii: Sigma w^2 alongside Sigma w, whenever the table is WEIGHTED (the
          # ruling-8 shape the raw scan and num_moment_scan() already follow: ONE aggregate, so
          # toggling `design_effect` is a cache HIT). Without it this aggregate could not serve the
          # weighted basis, and the checkbox moved the MEAN cell intervals while leaving the
          # PERCENTAGES and both p-values uncorrected -- with the footer denying the one correction
          # that did happen.
          pair <- if (weighted) {
            dt[, list(n = .N, wn = sum(as.numeric(eval(rlang::sym(wt_chr))), na.rm = TRUE),
                      w2 = sum(as.numeric(eval(rlang::sym(wt_chr)))^2, na.rm = TRUE)),
               keyby = keycols]
          } else {
            dt[, list(n = .N), keyby = keycols]
          }
          store <- jmv_cache_put(store, "agg", key,
                                 list(cols = jmv_dt_to_cols(pair), keys = keycols))
        }
        fine_fused[[paste(rv, cv, sep = "\r")]] <- pair
      }
    }
    if (length(fine_fused) == 0L) fine_fused <- NULL
  }

  # --- Tier 1b: per-row_var NUMERIC moment aggregates (all measures in one entry) -----------
  # First-cut simplification: cache per (row_var x measure-SET). Adding/removing a numeric measure
  # re-scans that row_var's moments; per-measure-incremental caching is a documented refinement.
  fine_num <- NULL
  num_keys_by_rv <- stats::setNames(vector("list", length(row_vars)), row_vars)
  if (length(num_cols) > 0L) {
    fine_num <- stats::setNames(vector("list", length(row_vars)), row_vars)
    for (i in seq_along(row_vars)) {
      rv     <- row_vars[[i]]
      na_rv  <- ctx$settings$rows$na_num[[i]]
      msr    <- sort(num_cols)
      msr_fp <- lapply(msr, function(v) fp[[v]])
      key    <- jmv_hash(list("num", pop_tag, rv, fp[[rv]], msr, msr_fp,
                              grain, grain_fp, wt_chr, wt_fp, na_rv))
      num_keys_by_rv[[rv]] <- key
      got <- jmv_cache_fetch(store, "agg", key)
      store <- got$store
      agg_hits[[paste(rv, "<num>", sep = "\r")]] <- got$hit
      if (got$hit) {
        fine_num[[i]] <- jmv_cols_to_dt(got$value$cols)
      } else {
        rv_sym  <- ctx$row_vars[[i]]
        wt_part <- if (weighted) wt else NULL
        # num_cols/tab_vars are already-resolved character vectors. Passing them as bare symbols
        # made tidyselect resolve them as external vectors -- deprecated since tidyselect 1.1.0.
        # as.character() keeps them a CALL (not a bare symbol), which selects by value silently and
        # mirrors tab_aggregate()'s own call at tab.R ~L1709. Do NOT `!!`-inject the value instead:
        # a literal would make quo_miss_na_null_empty_no() read a column named "no" as "no column".
        agg <- rlang::inject(tab_aggregate_num(
          data, !!rv_sym, as.character(num_cols), as.character(tab_vars),
          wt = !!wt_part, na = na_rv
        ))
        fine_num[[i]] <- agg
        store <- jmv_cache_put(store, "agg", key,
                               list(cols = jmv_dt_to_cols(agg), keys = c(tab_vars, rv)))
      }
    }
  }

  # --- Tier 2: whole-table test keys + hit lookup -------------------------------------------
  # Key = (comp, na, the tier-1 pair/measure keys that content-address the shaped aggregate). Excludes
  # pct/ref/ci/levels/color/digits (none change the omnibus test). na disambiguates keep vs drop (the
  # test sees drop's NA-cell removal). Used only when chi2 is on AND non-contrib (contrib writes
  # per-cell ctr/var fields, not in the test tibble -> must recompute).
  chi2      <- ctx$settings$rows$chi2
  color     <- ctx$settings$rows$color         # Phase 19c: the ONE resolved measure (was color_ctr)
  comp      <- ctx$settings$rows$comp
  na_scalar <- ctx$na
  tier2_keys  <- stats::setNames(vector("list", length(row_vars)), row_vars)
  cached_tests <- stats::setNames(vector("list", length(row_vars)), row_vars)
  for (i in seq_along(row_vars)) {
    rv <- row_vars[[i]]
    # Skip when the colour makes the leaf write per-cell contribution fields (not in the test
    # tibble, so it must recompute), and when the colour is still the unresolved "auto" sentinel: that
    # is a numeric-only table, whose test is the ANOVA computed outside this path.
    if (!isTRUE(chi2[[i]]) || identical(color[[i]], "auto") ||
        identical(measure_builds(color[[i]]), "contrib")) next
    tkey <- jmv_hash(list("test", comp[[i]], na_scalar,
                          sort(unlist(fct_keys_by_rv[[rv]])), num_keys_by_rv[[rv]]))
    tier2_keys[[rv]] <- tkey
    got <- jmv_cache_fetch(store, "test", tkey)
    store <- got$store
    test_hits[[rv]] <- got$hit
    if (got$hit) cached_tests[[rv]] <- got$value
  }

  # --- Phase 7g-ii: post-aggregate level reordering (tier-3 input; the STORED blob stays raw) ----
  # A jamovi level reorder relevels the shaped aggregate's factor keys IN MEMORY, after it was built /
  # fetched + stored at raw order (above). Because the stored blob and the tier-1/2 keys (raw
  # fingerprints) never change, a reorder reuses tiers 1-2 and only rebuilds the O(cells) fmt (design
  # 4e). fct_relevel is absolute, so re-releveling a stale-order cache hit to the new order is correct.
  spec <- ctx$levels_order
  new_remove <- NULL
  if (!is.null(spec)) {
    if (!is.null(fine_fused)) {
      for (nm in names(fine_fused)) {
        rvcv <- strsplit(nm, "\r", fixed = TRUE)[[1]]        # name = paste(rv, cv, sep = "\r")
        fine_fused[[nm]] <- jmv_relevel_cols(fine_fused[[nm]], spec,
                                             c(tab_vars, rvcv[[1L]], rvcv[[2L]]))
      }
    }
    if (!is.null(fine_num)) {
      for (i in seq_along(fine_num)) {
        if (is.null(fine_num[[i]])) next
        fine_num[[i]] <- jmv_relevel_cols(fine_num[[i]], spec, c(tab_vars, row_vars[[i]]))
      }
    }
    # ctx$data feeds the raw-scan leaves (self-crosstab factor pairs skipped from fine_fused; the
    # numeric leaf) -> relevel it too so those axes reorder identically.
    data <- jmv_relevel_cols(data, spec, unique(c(row_vars, col_vars, tab_vars)))
    # levels = "first" + reorder: the column KEPT must be the reordered-first. Recompute remove_levels
    # for lv1 col_vars in spec (defer_level_merge appends the explicit "NA" column). Others untouched.
    lv1 <- ctx$settings$cols$lv1
    rl  <- ctx$remove_levels
    if (!is.null(lv1) && any(lv1) && !is.null(rl)) {
      for (cv in intersect(col_vars[lv1], names(spec))) {
        f <- data[[cv]]
        if (is.factor(f)) rl[[cv]] <- c(levels(f)[-1], "NA")
      }
      new_remove <- rl
    }
  }

  ce$store <- store
  ce$hits  <- list(agg = agg_hits, test = test_hits)

  updates <- list(
    fine_num = fine_num, fine_fused = fine_fused,
    cached_tests = cached_tests, tier2_keys = tier2_keys
  )
  if (!is.null(spec)) {
    updates$data <- data
    if (!is.null(new_remove)) updates$remove_levels <- new_remove
  }
  ctx_update(ctx, updates)
}


# Persist freshly-computed tier-2 tests (cache misses) after the per-row_var builds. Idempotent: only
# stores a key not already present, so hits are not re-serialized. Called from tab_build_tables() when
# cache_env is set. ctx$tests is the per-row_var list of PRE-merge tests (the factor chi2 tibble; a
# logical on a numeric-only table -> skipped by the !is.data.frame guard, matching the pre-9a behaviour
# where a numeric-only ctx$tests was a bare logical and !is.list() short-circuited the whole store).
#' @keywords internal
#' @noRd
jmv_cache_store_tests <- function(ctx) {
  ce    <- ctx$cache_env
  tests <- ctx$tests
  keys  <- ctx$tier2_keys
  if (is.null(ce) || !is.list(tests) || is.null(keys)) return(invisible(NULL))
  store <- ce$store
  for (rv in names(keys)) {
    tkey <- keys[[rv]]
    if (is.null(tkey) || !is.null(store$test[[tkey]])) next
    tb <- tests[[rv]]
    if (is.null(tb) || !is.data.frame(tb)) next
    store <- jmv_cache_put(store, "test", tkey, tb)
  }
  ce$store <- jmv_cache_evict(store)
  invisible(NULL)
}


# === Display-tier cleannames (jmvtab only) ===============================================

# Strip cleannames patterns from a BUILT table at display: the row_var/tab_var label-column VALUES and
# the col-level column HEADERS (never the row_var/tab_var column NAMES). jmvtab carries full names
# through the whole pipeline (so na/levels/cleannames stay cheap post-aggregate) and cleans only here.
# Documented divergence from tab() (design 5): two raw levels cleaning alike show as separate
# same-labelled rows/columns (no collision summing) -- accepted, jmvtab needs no back-compat.
#' @keywords internal
#' @noRd
jmvtab_cleannames_display <- function(tabs) {
  cond <- cleannames_condition()
  strip <- function(x) stringi::stri_replace_all_regex(x, cond, "")
  one <- function(tb) {
    vars <- tab_get_vars(tb)
    # Label columns = the row_var / tab_var text columns. For a compacted multi-row_var table these
    # are `levels` (the level labels) + the `row_var` indicator (variable names -- a no-op for the
    # regex); for a tab_vars table one is the grouping column. Clean with a PER-COLUMN
    # dplyr::mutate(!!col := ) (not across(), which can't select a grouping column): it uses
    # tabxplor's mutate method so the tab class + grouping survive, and can target a grouping column
    # (relabelling re-syncs the group keys). Base `[[<-` / `names<-` would downgrade to grouped_df.
    label_cols <- intersect(c(vars$row_var, as.character(vars$tab_vars)), names(tb))
    for (col in label_cols) {
      col_sym <- rlang::sym(col)
      tb <- dplyr::mutate(tb, !!col := if (is.factor(!!col_sym))
        forcats::fct_relabel(!!col_sym, strip) else strip(!!col_sym))
    }
    # Column headers = the col-level columns (never the row_var/tab_var column NAMES).
    hdr <- setdiff(names(tb), label_cols)
    tb <- dplyr::rename_with(tb, strip, tidyselect::all_of(hdr))
    tb
  }
  if (is.list(tabs) && !is.data.frame(tabs)) purrr::map(tabs, one) else one(tabs)
}


# (The jamovi `display` ComboBox is applied by tab_apply_display() -- tab()'s own writer, which takes
#  a tab OR a list of tabs, normalises a one-field template back to its bare token and writes only on
#  genuine value cells. Phase 19k deleted the jmv_apply_display() wrapper that used to hold it, and
#  with it the `ci == "cell"` -> set_display("pct_ci") convenience that followed: since 19j the LEAF
#  stamps that display where it builds the cell interval (plain_core's `isTRUE(ci_res$visible) ~
#  "pct_ci"`, num_core's `if (ci_visible) "mean_ci"`), so the block was redundant -- and harmful
#  twice over. It wrote `pct_ci` onto MEAN columns, whose `pct` field is NA on the numeric leaf, so
#  the cell rendered EMPTY (D11); and, running after the writer, it silently overrode whatever the
#  user had picked in the display ComboBox.)


# Coerce numeric-valued col_vars back to numeric so they become MEAN columns, matching plain R
# tab() exactly (integer/double col_var -> mean). jamovi hands a nominal/ordinal integer to the
# module ALREADY factored (levels "0".."24"), losing its numeric type -- so `tvhours` would wrongly
# become one column per value. A col_var that is numeric, or a factor whose levels ALL parse as
# numbers, is treated as numeric. CAVEAT (documented, dev/tabxplor_2.0.0_jamovi_dev.md): a genuinely
# categorical numeric CODE (e.g. region 1-5) also becomes a mean -- relabel such levels to non-numeric
# text in jamovi, or mark the variable Continuous, to control it. Only col_vars are touched (row/tab
# vars are always categorical axes).
#' @keywords internal
#' @noRd
jmv_coerce_numeric_cols <- function(data, col_vars) {
  for (cv in col_vars) {
    v <- data[[cv]]
    if (is.factor(v)) {
      lv <- levels(v)
      if (length(lv) > 0L && !anyNA(suppressWarnings(as.numeric(lv)))) {
        data[[cv]] <- as.numeric(as.character(v))
      }
    }
  }
  data
}

# Build tab()'s `ref` argument from the Phase 7g-iii reference-level picker. `refLevels` is the jamovi
# Array option = a list of {var, ref} (one per selected axis variable; ref = a chosen level label, or
# "tot" for the total, or NULL / "" when left on the default). If the user picked AT LEAST ONE explicit
# reference, return a named character vector keyed by var (unset entries -> "auto"), read by
# resolve_ref_vector() as a per-variable reference. The keys are row_var names under pct="row"/means and
# col_var names under pct="col" (the caller passes the entries already filtered to the active axis);
# tab_setup() disambiguates by pct. A chosen level LABEL is passed verbatim -- diff_index() matches it
# by EXACT equality first (Phase 7g-iii), so metacharacter labels like "$25000 or more" work and the
# stored `ref` attribute stays human-readable. Otherwise fall back to the expert free-text `ref`.
#' @keywords internal
#' @noRd
jmvtab_ref_vector <- function(refLevels, free_text_ref = "auto") {
  if (length(refLevels) == 0) return(free_text_ref)
  get1 <- function(e, k) { v <- e[[k]]; if (is.null(v)) NA_character_ else as.character(v) }
  vars <- vapply(refLevels, get1, character(1), k = "var")
  refs <- vapply(refLevels, get1, character(1), k = "ref")
  keep <- !is.na(vars) & nzchar(vars)
  vars <- vars[keep]; refs <- refs[keep]
  if (length(vars) == 0) return(free_text_ref)
  if (!any(!is.na(refs) & nzchar(refs))) return(free_text_ref)   # no explicit level -> free-text
  refs[is.na(refs) | !nzchar(refs)] <- "auto"
  stats::setNames(refs, vars)
}

# Build tab()'s internal `.levels_order` from the Phase 7g-ii level-reordering picker. `levelOrder` is
# the jamovi Array option = a list of {var, levels} (one per REORDERED variable; levels = the ordered
# level labels). Return a named list var -> ordered character vector, dropping entries with an empty
# var or no levels; NULL when nothing was reordered (-> tab() runs unchanged). Consumed post-aggregate
# by jmv_cache_aggregate() (design 4e: a reorder is a tier-3 input, tiers 1-2 reused).
#' @keywords internal
#' @noRd
jmvtab_levels_order <- function(levelOrder) {
  if (length(levelOrder) == 0) return(NULL)
  out <- list()
  for (e in levelOrder) {
    v <- e[["var"]]
    if (is.null(v) || !nzchar(as.character(v))) next
    lv <- e[["levels"]]
    if (is.null(lv) || length(lv) == 0) next
    lv <- as.character(unlist(lv, use.names = FALSE))
    lv <- lv[!is.na(lv) & nzchar(lv)]
    if (length(lv) == 0) next
    out[[as.character(v)]] <- lv
  }
  if (length(out) == 0) NULL else out
}

# Reorder the factor level order of `cols` in `x` to match `spec` (a named list var -> ordered levels).
# forcats::fct_relevel is ABSOLUTE (it sets the given levels first, in order; unlisted levels trail in
# their existing order) -> safe on a possibly stale-order cache hit and on partial specs (level drift).
# For a data.table the key columns are re-set afterwards so row order matches a fresh keyby(). Works on
# both a data.table aggregate (in place via set()) and ctx$data (a tibble). Non-factor / unlisted cols
# are left untouched.
#' @keywords internal
#' @noRd
jmv_relevel_cols <- function(x, spec, cols) {
  is_dt <- data.table::is.data.table(x)
  for (col in cols) {
    ord <- spec[[col]]
    if (is.null(ord) || !col %in% names(x)) next
    f <- x[[col]]
    if (!is.factor(f)) next
    ord <- ord[ord %in% levels(f)]
    if (length(ord) == 0) next
    f2 <- forcats::fct_relevel(f, ord)
    if (is_dt) data.table::set(x, j = col, value = f2) else x[[col]] <- f2
  }
  if (is_dt) data.table::setkeyv(x, intersect(cols, names(x)))
  x
}


# === Tier 3: built-table cache (display / colour / reference re-use) =======================

# The tier-3 BASE key: identifies the ref-INDEPENDENT base fields {n, wn, pct, tot_n, mean, var}. It
# hashes the aggregate identity (population tag + per-variable fingerprint + grain + wt + other) plus
# every remaining opt EXCEPT the ones re-applied post-cache (the tier-4 paint: digits/display/
# cleannames/color/color_signif) and the transform-tuple items (ref/ref2/comp/OR/ci-params) -- so pct,
# na, levels, add_n, totaltab, subtext ... any structural/display-baked arg invalidates the entry.
#' @keywords internal
#' @noRd
jmv_tab3_base_key <- function(opts, ce, row_vars, col_vars, tab_vars, wt_chr) {
  fp   <- ce$fp_map
  used <- sort(unique(c(row_vars, col_vars, tab_vars, if (nzchar(wt_chr)) wt_chr)))
  # Phase 19k: THE population descriptor, from tab_cache_keys() itself (R/tab-resolve.R) -- the same
  # function tab_setup() calls to key tier 0/1. It used to be re-implemented here, line for line, in
  # the file that ALSO reads the real one (jmv_cache_aggregate reads ctx$cache_keys$tier0$population).
  pop  <- tab_cache_keys(na = opts$na, row_vars = row_vars, col_vars = col_vars,
                         tab_vars = tab_vars)$tier0$population
  agg_id <- list(
    pop   = jmv_pop_tag(pop, fp, ce$nrow),
    vars  = lapply(used, function(v) list(v, fp[[v]])),
    wt    = wt_chr,
    grain = sort(tab_vars),
    other = opts$other_if_less_than
  )
  # Phase 19k (D12): the four interval-method keys are named by their REAL option names. The list
  # used to say `"ci_method"`, which is not a key of `opts` (the UI keeps one ComboBox per interval
  # kind, folded by jmv_ci_method()), so all four landed in `structural` -- every method toggle
  # forced a full tier-3 rebuild and the cheap re-ref path could never be reached. They are in the
  # TRANSFORM TUPLE, which is where a value that changes the BOUNDS belongs, and jmv_tab3_reref()
  # rebuilds those bounds from the carrier's ref-independent base.
  # Phase 19k: `anova` joins them. It is display intent now (tab()'s own argument, stored in
  # render_extras and read back at render from the `test` attribute, which holds BOTH F rows), so it
  # is re-applied at tier 4 -- it used to sit in `structural` and rebuild the whole table.
  reapplied  <- c("digits", "display", "cleannames", "color", "color_signif",
                  "ref", "ref2", "comp", "OR", "ci", "conf_level",
                  "method_cell", "method_diff", "method_mean_diff", "method_mean_ratio",
                  "stars", "n_min", "anova")
  # Phase 7g-ii: `levels_order` is intentionally NOT in `reapplied` -> it lands in `structural`, so a
  # reorder forces a tier-3 rebuild (fmt/colour) while agg_id (raw fingerprints) is unchanged -> tiers
  # 1-2 hit (design 4e). The rebuild also recomputes the reorder-driven ref shift (ref="first" /
  # common_base first-col), so no tier-3 tuple entry is needed.
  structural <- opts[setdiff(names(opts), reapplied)]
  jmv_hash(list("tab3", agg_id, structural))
}

# The colour "arming class" -> which measure fields the armed table populates. diff/ratio/auto share
# the "diff" class (tab_plain computes diff AND ratio together), so a diff<->ratio toggle is a pure
# re-paint (same tuple). or / contrib populate their own fields; "off" colours nothing.
# Phase 19c: that IS the measure's declared `builds` class (MEASURES, R/fmt_class.R) -- the four-arm
# classification written here was a fourth copy of it. `TRUE` / `"auto"` arm the diff class because
# that is what the smart default resolves to on every table the tier-3 cache handles.
#' @keywords internal
#' @noRd
jmv_tab3_arming <- function(color) {
  if (isFALSE(color)) return("off")
  if (isTRUE(color))  return("diff")
  m <- as.character(color)[1]
  if (identical(m, "auto")) return("diff")
  measure_builds(m)
}

# The tier-3 TRANSFORM tuple: everything that changes field VALUES or POPULATION beyond the base. An
# exact match with the cached entry's tuple -> re-paint only; a difference -> re-ref (7f-4) or rebuild.
# `ci` is the RESOLVED ci (after the color_signif cascade), so grey<->color_all (same ci) re-paint
# while ignore<->grey (ci no<->diff) re-ref.
# The jamovi UI still exposes one ComboBox per interval kind (five separate options); this folds them
# into the ONE named vector the R surface takes (Phase 18z16-iiiii). Absent / empty options fall back
# to the package defaults, so an older .h.R that does not declare them all still builds.
#' @keywords internal
#' @noRd
jmv_ci_method <- function(opts) {
  ui <- list(cell = opts$method_cell, diff = opts$method_diff,
             mean_diff = opts$method_mean_diff, mean_ratio = opts$method_mean_ratio)
  resolve_ci_method(unlist(purrr::compact(ui)), fn = "jmvtab")
}

#' @keywords internal
#' @noRd
jmv_tab3_tuple <- function(opts, ci_resolved, arming, geom) {
  # Phase 19d/19e: `geom` -- which GEOMETRY owns the stored interval -- is part of the tuple, because
  # since 19d the interval follows the comparison: a difference table stores percentage-POINT bounds,
  # a ratio table Katz log-RR ones. `arming` cannot answer it (diff and ratio share one build class),
  # so a diff <-> ratio toggle used to be an exact tuple HIT and re-painted a ratio over the
  # difference interval -- measured against a plain tab() as a wholly different set of bounds.
  # (Since 19k it is not a rebuild either: the re-ref recomputes the bounds on the other scale.)
  # Phase 19d-tail: what the tuple needs from `display` is the COMPARISON it names, not the string.
  # `.return_armed = TRUE` returns before tab_apply_display(), so the armed carrier never carries a
  # display -- it is re-applied at tier 4 on every build. The only way `display` reaches the carrier
  # is by naming the comparison the table is built on (the 19d chain), which is a four-value fact.
  # Keying the raw string made every display toggle -- the second most frequent jamovi interaction --
  # rebuild the whole table. One key, and the display combobox is a re-paint again.
  list(arming = arming, geom = geom,
       comparison = display_comparison(opts$display),
       ref = opts$ref, ref2 = opts$ref2,
       comp = opts$comp, ci = ci_resolved, conf_level = opts$conf_level,
       ci_method = jmv_ci_method(opts), stars = opts$stars)
}

# The MEASURE a jamovi `color` option names ("no"/FALSE -> none, "auto"/TRUE -> the difference, else
# the declared measure/alias). One reading, shared by the arming class, the geometry and the resolved
# `ci` -- three facts that must agree about one option.
#' @keywords internal
#' @noRd
jmv_tab3_measure <- function(color) {
  if (isFALSE(color)) return("no")
  if (isTRUE(color))  return(measure_auto("pct", "text"))
  m <- as.character(color)[1]
  if (identical(m, "auto")) return(measure_auto("pct", "text"))
  k <- measure_key(m)
  if (is.na(k) || !nzchar(k)) "no" else k
}

# Whether a cached armed CARRIER can be RE-REFERENCED (Phase 9b-7): everything jmv_tab3_reref()
# RECOMPUTES may differ; everything it copies from the carrier must be identical. The full structural
# gate (pct="row", single row_var, no numeric cols, levels="all"...) is jmv_reref_shape_ok(), checked
# in jmvtab_build alongside this. Everything else -> the always-correct rebuild (fast: tiers 1-2 hit).
# NOTE the frequent color-driven `color_signif` toggle is NOT a reref: it is a pure re-paint
# (finalize_color_spec) that never enters this branch (same tuple -> exact-hit re-paint).
#
# Phase 19k: the tuple entries the re-ref RECOMPUTES are `ref` / `ref2` (tab_apply_reference) and,
# through leaf_ci_plain(), `geom` -> ci_scale and `ci_method` -> the engine. So they leave the
# identity set: a diff <-> ratio toggle and a CI-method toggle are re-refs now, not rebuilds. Both
# were vestigial restrictions -- 19e's because the re-ref used tab_ci() (the DIFFERENCE engine) and
# 19j replaced it with the leaf's own producer, which takes `ci_scale`; D12's because the four
# method options never reached the tuple at all (they were mis-named in `reapplied`).
#' @keywords internal
#' @noRd
jmv_tab3_rerefable <- function(old_tuple, new_tuple) {
  # what the re-ref does NOT recompute: the arming class (which measure FIELDS the armed build
  # populated), the comparison, the sub-population, the interval anchor, the level and the stars.
  keys    <- c("arming", "comparison", "comp", "ci", "conf_level", "stars")
  recomp  <- c("ref", "ref2", "geom", "ci_method")
  identical(old_tuple[keys], new_tuple[keys]) &&
    !identical(old_tuple[recomp], new_tuple[recomp]) &&   # ... and at least one of them DID change
    identical(new_tuple$arming, "diff") &&                # diff/ratio/auto colour (not OR/contrib)
    # an odds-ratio table -> rebuild (its `or` sweep and its Woolf interval are not the re-ref's)
    !identical(new_tuple$comparison, "odds_ratio")
}

# Structural gate for jmv_tab3_reref(): the STORE-KEY-invariant conditions (pct / row_var count /
# numeric cols / levels / add_pct) that jmv_tab3_reref() reproduces byte-identically. Read from `opts`
# + the already-computed `has_num_col`. Only the pct="row" single-row_var factor case is handled; any
# other shape falls through to the always-correct rebuild. `opts$col_vars`/`row_vars` are the ORIGINAL
# user selections (before jmvtab_build injects the no_row_var/no_col_var dummies), so length 0 -> a
# dummy table -> not rerefable.
#' @keywords internal
#' @noRd
jmv_reref_shape_ok <- function(opts, has_num_col) {
  identical(opts$pct, "row") &&
    length(opts$row_vars) == 1L &&
    length(opts$col_vars) >= 1L &&
    !has_num_col &&
    identical(opts$levels, "all") &&
    !isTRUE(opts$add_pct) &&
    # comp = "all" pools the total table and gives tab() a ref-DEPENDENT assembled shape (row count
    # differs by ref: e.g. 55 vs 61) -- so the base is not ref-invariant and cannot be re-ref'd. Only
    # comp = "tab" (the default, ref-invariant) is rerefable.
    identical(opts$comp, "tab")
  # (Phase 19k: the former `!(color == "auto" && ci == "diff")` exclusion is GONE. It existed because
  #  that pair once resolved to the composite "after_ci" -> a ref-DEPENDENT colour stamped by the CI
  #  step, which the re-ref could not reproduce. 19c deleted that resolution and 19j deleted the step;
  #  the colour is a tier-4 re-paint on every path now, and `ci = "diff"` is not even a spelling `ci`
  #  accepts since 19d.)
}

# Re-reference a cached armed CARRIER (Phase 9b-7): recompute the ref-DEPENDENT fields (diff/ratio +
# in_refrow + the diff-CI ci_inf/ci_sup/pvalue + the `ref` attr) from the ref-INDEPENDENT base fields
# (pct / n / wn / tot_n, all present in the cached carrier), for a new ref -- WITHOUT the O(cells)
# rebuild. Byte-identical to jmv_tab3_build_armed() with the new ref because it reuses the SAME shared
# math: tab_apply_reference() for diff/ratio (proven) and leaf_ci_plain() for the interval, which is
# the leaf's own producer (19j). Phase 10i-B: the armed carrier is the "core" table -- post-compact / grouping but WITHOUT
# p-value rows (materialised at display), so the recompute runs on the whole carrier and the former
# p-value-line exclusion (reconstruct + re-CI on data rows only) is gone. Precondition
# (jmv_reref_shape_ok + jmv_tab3_rerefable): pct="row", one factor row_var, diff arming, no OR.
# `ci_resolved` is jmvtab_build's resolved ci (== opts$ci here). The reref never touches the table attrs
# (test / groups / subtext, all ref-invariant), so they survive verbatim from the cache.
#' @keywords internal
#' @noRd
jmv_tab3_reref <- function(carrier, opts, ci_resolved, tuple) {
  if (is.null(carrier$is_fmt))                                   # output_list -> a list of carriers
    return(purrr::map(carrier, jmv_tab3_reref, opts = opts, ci_resolved = ci_resolved, tuple = tuple))

  row_var  <- opts$row_vars[[1]]
  tab_vars <- as.character(opts$tab_vars)
  comp     <- tuple$comp

  # Resolve the new reference exactly as the factor leaf would ("auto" -> "tot"). From the TUPLE,
  # which is what the armed build was keyed on, so the re-ref and the build cannot read two
  # different baselines.
  ref_v <- resolve_ref_vector(tuple$ref, row_var)
  if (identical(ref_v, "auto")) ref_v <- "tot"

  fmt_names <- names(carrier$fmt)
  pct_cols  <- fmt_names[vapply(fmt_names,
                                function(nm) identical(carrier$fmt[[nm]]$meta$pct_base, "row"),
                                logical(1))]
  n_field   <- carrier$fmt[[fmt_names[[1]]]]$frame$n

  # Phase 10i-B: the carrier is the "core" table -- no p-value rows (materialised at display), so ALL
  # rows are data rows. The former data_mask / p-value-row exclusion (reconstruct + re-CI on data rows
  # only) collapses: reconstruct tab_apply_reference()'s inputs from the WHOLE carrier.
  label_cols <- c(tab_vars, row_var)
  labels     <- stats::setNames(lapply(label_cols, function(cn) carrier$factors[[cn]]), label_cols)
  tabs     <- data.table::as.data.table(labels)
  tabs_pct <- data.table::as.data.table(labels)
  for (nm in pct_cols) tabs_pct[, (nm) := carrier$fmt[[nm]]$frame$pct]

  totrow_vector <- carrier$fmt[[fmt_names[[1]]]]$frame$row_kind == "total"
  tottab_vector <- carrier$fmt[[fmt_names[[1]]]]$frame$in_tottab

  # ONE SWEEP PER col_var, exactly as the build runs one leaf per col_var (tab_transform's pmap).
  # `diff` and `ratio` are column-wise, so pooling every col_var's levels into one sweep gave the
  # right answer and this used to be a single call. The ODDS RATIO is not: its 2x2 is (this level) x
  # (the ref2 level OF THE SAME VARIABLE), so a pooled sweep read `nm` as one variable's level set and
  # compared a partyid level against a race one. Measured on a two-col_var table as ORs in the tens
  # against a rebuild's 1.00 -- latent since 19d made the odds ratio unconditional (before, the re-ref
  # wrote no `or` at all), and invisible because the test that covers it was silently building a
  # one-col_var table. Same fact as the leaf's own `dichotomise`: `or` is the only per-cell field
  # whose value depends on which OTHER columns are present.
  ref_1 <- switch(as.character(ref_v), "no" = "", "tot" = "tot", as.character(ref_v))
  inref <- logical(length(n_field))                             # ref = "tot" -> all FALSE (tab_plain L3062)
  by_cv <- split(pct_cols, vapply(pct_cols,
                                  function(nm) carrier$fmt[[nm]]$meta$col_var %||% "",
                                  character(1)))
  # Phase 19j (KEY 5): the interval is REBUILT IN THE SAME SWEEP, by the leaf's own producer. It used
  # to be a separate fmt_wrap() -> tab_ci() -> fmt_unwrap() round trip below, which existed only
  # because the interval was not part of the build -- the study's own example of a cache path shaped
  # by a pipeline defect. WARNING: `degf` must be passed. tab_ci() derived it implicitly from the
  # columns (tab_inference_degf) because this caller passed none; dropping it silently falls back to z
  # (measured 9 % too narrow at 13 PSUs, test-degraded-attrs.R).
  ci_scale_r <- if (identical(tuple$geom, "ratio")) "ratio" else "diff"
  # the carrier's own smallest design df -- tab_inference_degf()'s rule, read off the stored per-column
  # metas (the carrier is a list of frames, not a table of fmt columns).
  degf_all   <- vapply(fmt_names, function(nm) {
    d <- carrier$fmt[[nm]]$meta$degf; if (length(d) == 0L) NA_real_ else as.double(d[[1]])
  }, double(1))
  degf_all   <- degf_all[is.finite(degf_all) & degf_all > 0]
  degf_r     <- if (length(degf_all)) min(degf_all) else Inf
  grp_r      <- if (identical(comp, "all") || length(tab_vars) == 0L) rep(1L, length(n_field)) else
    do.call(paste, c(lapply(tab_vars, function(v) as.character(carrier$factors[[v]])), sep = "\r"))
  for (grp in by_cv) {
    ref_res <- tab_apply_reference(
      tabs = tabs, tabs_pct = tabs_pct, ref = ref_v, ref2 = tuple$ref2, comp = comp,
      or_compare = TRUE, pct = "row", tab_row_names = label_cols,
      tab_vars = rlang::syms(tab_vars), row_var = rlang::sym(row_var),
      tottab_vector = tottab_vector, totrow_vector = totrow_vector,
      cols = stats::setNames(rep(TRUE, length(grp)), grp),
      # Phase 19m-i: the STORED flag, not the literal "Total". `grp` holds FINAL column names --
      # post leaf_rename_totals() -- so the leaf's own pre-rename convention does not hold here:
      # with total_names = "Ensemble" no column matched and the reference 2x2 was built against the
      # wrong column. Invisible until now only because po/R-fr.po translates "Total" -> "Total".
      # It is the same expression leaf_ci_plain() is handed 20 lines below.
      totcol_vector = vapply(grp, function(nm) isTRUE(carrier$fmt[[nm]]$meta$totcol), logical(1)))

    # --- write diff/ratio (pct cols) + the ref-row marker into the carrier ----------------------
    if (!identical(ref_v, "tot")) inref[] <- ref_res$refrows
    for (nm in grp) {
      carrier$fmt[[nm]]$frame$diff  <- ref_res$diff[[nm]]
      carrier$fmt[[nm]]$frame$ratio <- ref_res$ratio[[nm]]
      # Phase 19d: the odds ratio is a reference-DEPENDENT field on every row/col-% table now, so the
      # re-ref has to refresh it too -- otherwise a reference toggle left the cached `or` describing
      # the OLD baseline while diff/ratio described the new one.
      if (!is.null(ref_res$or)) carrier$fmt[[nm]]$frame$or <- ref_res$or[[nm]]
    }

    if (!identical(ci_resolved, "no")) {
      ci_res <- leaf_ci_plain(
        P     = do.call(cbind, lapply(grp, function(nm) carrier$fmt[[nm]]$frame$pct)),
        tot_n = do.call(cbind, lapply(grp, function(nm) carrier$fmt[[nm]]$frame$tot_n)),
        n_eff = do.call(cbind, lapply(grp, function(nm) carrier$fmt[[nm]]$frame$n_eff)),
        ci = ci_resolved, pct = "row", ci_scale = ci_scale_r,
        grp = grp_r,
        ref_row = if (identical(ref_v, "tot")) totrow_vector else ref_res$refrows,
        totrow  = totrow_vector,
        refcol  = NA_integer_,
        totcol  = vapply(grp, function(nm) isTRUE(carrier$fmt[[nm]]$meta$totcol), logical(1)),
        conf_level = tuple$conf_level, stars = tuple$stars,
        ci_method = tuple$ci_method, degf = degf_r)
      # Phase 19k: the interval's two COLUMN facts travel with its bounds -- what the column
      # estimates (`scale`) and which engine built them (`ci_method`), exactly as plain_core() stamps
      # them from this same CI_GEOMS row. They had to be restamped before a geometry or a method
      # change could take the re-ref path: a ratio interval on a column still saying `points` is
      # 19b's D8/D19 class, and it is what fmt_scale_of()/ci_center()/the legend read.
      # (A NA scale_key is a cell interval, where the LEVEL scale stands -- the same fallback the
      # leaf applies. `pct_base` is "row" throughout here, so `level_pct` is that level.)
      for (j in seq_along(grp)) {
        carrier$fmt[[grp[[j]]]]$frame$ci_inf <- ci_res$inf[, j]
        carrier$fmt[[grp[[j]]]]$frame$ci_sup <- ci_res$sup[, j]
        carrier$fmt[[grp[[j]]]]$frame$pvalue <- ci_res$pvalue[, j]
        carrier$fmt[[grp[[j]]]]$meta$scale     <- if (is.na(ci_res$scale)) "level_pct" else ci_res$scale
        carrier$fmt[[grp[[j]]]]$meta$ci_method <- ci_res$method
      }
    }
  }
  for (nm in fmt_names) {
    carrier$fmt[[nm]]$frame$in_refrow <- inref
    carrier$fmt[[nm]]$meta$ref        <- ref_1
  }

  carrier
}

# Build the ARMED table (pre-finalize) for a tier-3 miss/rebuild: reuse tab() end to end with the
# live cache injected and `.return_armed` so finalize_color_spec() is applied later (as a re-paint).
#' @keywords internal
#' @noRd
jmv_tab3_build_armed <- function(data, opts, color, color_signif, ci, wt_sym,
                                 row_vars, col_vars, tab_vars, ce) {
  rlang::inject(tab(
    data,
    row_vars     = tidyselect::all_of(row_vars),
    col_vars     = tidyselect::all_of(col_vars),
    tab_vars     = tidyselect::all_of(tab_vars),
    wt           = !!wt_sym,
    pct          = opts$pct,
    color        = color,
    color_signif = color_signif,
    # Phase 19k: the UI names the odds ratio the way tab() does -- `display` says which quantity the
    # cell shows and `ref2` picks its 2x2. The retired `OR` option and the tab_deprecate_or() shim
    # 19d put here (so a UI toggle would not emit a lifecycle warning into the results panel) are
    # BOTH gone: there is no second vocabulary left to translate.
    display      = opts$display,
    ref2         = opts$ref2,
    # Phase 18z14-i / z16-iii: `test` is a plain boolean. Phase 19a (D15) corrects what followed: the
    # jamovi `design_effect` checkbox does NOT ride a global option. z16-iiiii made it tab()'s own
    # `design_effect` argument, passed 12 lines below as `design_effect = opts$design_effect`.
    # Phase 19k: `anova` was the last option that DID travel as a global (options() + on.exit around
    # the build in .run()); it is tab()'s own argument now -- and it is deliberately NOT passed here,
    # because it is display intent re-applied at tier 4 (jmv_reapply_anova), which is what makes a
    # welch <-> classic toggle a re-derive instead of a rebuild. So NO option reaches the build
    # through a second vocabulary any more.
    test         = opts$test,
    na           = opts$na,
    levels       = opts$levels,
    ref          = opts$ref,
    comp         = opts$comp,
    ci           = ci,
    conf_level   = opts$conf_level,
    stars        = opts$stars,
    ci_method    = jmv_ci_method(opts),
    design_effect = opts$design_effect,
    cleannames   = FALSE,                        # cleannames applied at display (jmvtab_build)
    totaltab     = opts$totaltab,
    digits       = opts$digits,
    other_if_less_than = opts$other_if_less_than,
    add_n        = opts$add_n,
    add_pct      = opts$add_pct,
    subtext      = opts$subtext,
    totaltab_name = opts$totaltab_name,
    total_names   = opts$total_names,
    other_level   = opts$other_level,
    output_list   = isTRUE(opts$output_list),
    .cache = ce, .defer_level_merge = TRUE, .return_armed = TRUE,
    .levels_order = opts$levels_order          # Phase 7g-ii: post-aggregate reorder (jmv_cache_aggregate)
  ))
}

# === Carrier (Phase 9b-7): tier-3 stores plain field-frames, not a live materialized tab ==========
# The tier-3 cache holds the ARMED table as the CARRIER (fmt_unwrap: per-fmt-col list(frame = the 18
# raw field vectors, meta = the 9 attrs) + the factor columns + the table attrs) instead of a live
# `tabxplor_tab`. This aligns tier-3 with the tiers-1-2 "plain atomic-vector lists" discipline (more
# robust through jamovi's $state), lets the tier-4 field re-paints run on plain vectors (one final
# fmt_wrap() replaces the per-field record reconstruction), and gives jmv_tab3_reref() a plain-field
# base to recompute the reference-dependent fields from (increment 2). fmt_unwrap/fmt_wrap live in
# R/tab.R (byte-identical round-trip, test-carrier-parity.R). These two helpers add the single-tab vs
# output_list dispatch (a single carrier is discriminated by its `is_fmt` slot).
#' @keywords internal
#' @noRd
jmv_carrier_unwrap <- function(tabs) {
  if (is.data.frame(tabs)) fmt_unwrap(tabs) else purrr::map(tabs, fmt_unwrap)
}
#' @keywords internal
#' @noRd
jmv_carrier_wrap <- function(carrier) {
  if (!is.null(carrier$is_fmt)) fmt_wrap(carrier) else purrr::map(carrier, fmt_wrap)
}

# Re-apply the jamovi `digits` option to the armed CARRIER (tier-4, pure display, always runs).
# Proportion / count columns take as.integer(digits) (matching tab_plain()); MEAN columns reproduce
# tab_num()'s magnitude floor (max(digits, 2/1/0) by max cell mean). Phase 10i-B: the carrier is the
# "core" table with NO p-value line (materialised at display with its own fixed digits), so every row
# takes the resolved digits -- the former n = NA skip is gone. Phase 9b-7: operates on the carrier's
# plain `frame$digits` vector, so the single fmt_wrap() at the end of jmvtab_build absorbs this pass's
# record construction (no separate set_digits() reconstruction, and the grouped-tab attribute
# snapshot/restore trick is gone -- grouping rides in carrier$attrs, restored by fmt_wrap). vec_cast to
# integer matches set_digits() (fmt_set_field_factory casts to integer; fmt_wrap's new_fmt() does NOT,
# so a double `digits` arg -- base_d becomes double via max(0, 1L) -- must be cast here). (A mean total
# row is a bounded average of the cell means, so max(frame$mean) equals the build-time max regardless
# of total-row removal.)
#' @keywords internal
#' @noRd
jmv_reapply_digits <- function(carrier, digits) {
  one <- function(cr) {
    for (nm in names(cr$fmt)) {
      frame  <- cr$fmt[[nm]]$frame
      # Phase 19k: THE floor, num_core()'s own (R/tab.R) -- it was byte-duplicated here.
      base_d <- if (identical(est_var_kind(cr$fmt[[nm]]$meta$scale), "mean")) {
        num_digits_floor(digits, frame$mean)
      } else as.integer(digits)
      new_d    <- frame$digits
      new_d[]  <- base_d
      cr$fmt[[nm]]$frame$digits <- vctrs::vec_cast(new_d, integer())
    }
    cr
  }
  if (!is.null(carrier$is_fmt)) one(carrier) else purrr::map(carrier, one)
}

# Re-apply the jamovi `anova` option (WHICH stored one-way F the p-value line shows) to the BUILT
# table -- tier 4, pure display. The `test` attribute holds BOTH F rows and tab_anova() reads this
# intent back at render, so a welch <-> classic toggle never touches a cell. It runs on every path
# (re-paint / re-ref / rebuild), which is what lets `anova` sit in the tier-3 `reapplied` set: it
# used to be baked into the base key and rebuild the whole table.
#' @keywords internal
#' @noRd
jmv_reapply_anova <- function(tabs, anova) {
  if (is.null(anova) || !nzchar(as.character(anova)[[1]])) return(tabs)
  one <- function(tb) {
    re <- get_render_extras(tb)
    re[["anova"]] <- as.character(anova)[[1]]
    set_render_extras(tb, re)
  }
  if (is.data.frame(tabs)) one(tabs) else purrr::map(tabs, one)
}


# === The pure build core (engine-free, testable without a live jamovi session) ============

# Build the jmvtab table(s) from a plain option list + a cache store. Phase 7f: a change that touches
# only DISPLAY / COLOUR / REFERENCE reuses a cached ARMED table (pre-finalize fmt cells) and skips the
# O(cells) rebuild -- an exact tuple match re-paints (colour / digits / display), a rerefable tuple
# re-refs (reference / ci), and only a base change (variables / pct / na / levels / structural) rebuilds
# via tab() end to end. The colour spec + digits + display + cleannames are applied FRESH every time.
# Returns the built tab(s), the updated store (persist to $state), and hit flags.
#' @keywords internal
#' @noRd
jmvtab_build <- function(data, opts, store) {
  row_vars <- opts$row_vars
  col_vars <- opts$col_vars
  tab_vars <- opts$tab_vars
  # DESIGN (Phase 18a bug-fix): a transient 0-row `data` (the jamovi live UI mid-selection, or a
  # fully filtered dataset) must degrade gracefully, not abort. Both the length-0 placeholder injection
  # below (`data$no_row_var <- factor(...)` throws "replacement has 1 row, data has 0") and tab()
  # itself (`stop("data is of length 0")`) crash on 0 rows. Return a plain empty frame the exporters
  # render plainly via tab_render_vars()'s graceful-degrade path.
  if (nrow(data) == 0L) {
    return(list(tabs  = tibble::tibble(),
                store = jmv_cache_migrate(store),
                hits  = list(agg = logical(0), test = logical(0))))
  }
  if (length(row_vars) == 0L) { data$no_row_var <- factor("no_row_var"); row_vars <- "no_row_var" }
  if (length(col_vars) == 0L) { data$no_col_var <- factor("n");          col_vars <- "no_col_var" }
  data   <- jmv_coerce_numeric_cols(data, col_vars)   # integer/numeric col_var -> mean (match R)
  wt     <- opts$wt
  wt_sym <- if (length(wt)) rlang::sym(wt) else NULL
  wt_chr <- if (length(wt)) as.character(wt) else ""

  # Color: "no" -> FALSE, "auto" -> TRUE, else the measure string. For FACTOR columns `color_signif`
  # (the significance policy) is a pure RE-PAINT: it changes only the colour attribute
  # finalize_color_spec() sets, never the fmt FIELDS, because ci = "auto" already computes the diff CI
  # (tab_ci resolves auto -> diff) that grey / color_all merely GATE. So the ARMED table is built
  # canonically with color_signif = "ignore" (legacy colour = the base measure finalize refines to any
  # policy), and ignore <-> grey <-> color_all re-paint instantly. NUMERIC means are the exception:
  # ci = "auto" does NOT compute a mean CI, so a policy there truly ADDS the CI field -- nudge ci =
  # "diff" (and put it in the tuple) ONLY when a numeric col_var is present, so numeric ignore <-> grey
  # correctly rebuilds while factor tables stay instant.
  color        <- switch(opts$color, "no" = FALSE, "auto" = TRUE, opts$color)
  color_signif <- opts$color_signif
  has_num_col  <- any(vapply(col_vars, function(cv) is.numeric(data[[cv]]), logical(1)))
  # Phase 19e: the resolved `ci` drives the cache tuple, the armed build AND the reref, so it MUST be
  # what tab() will actually build. It used to be two hand-mirrored rules (a numeric-column policy
  # nudge + 16f's stars forcing) and they had fallen behind 19d: a factor table with `stars = FALSE`
  # and no policy gets NO interval now, so the reref recomputed a CI the fresh rebuild leaves NA --
  # a cached table that disagreed with a rebuilt one. One call to the shared resolver instead.
  # 19k: the .a.yaml speaks the anchor vocabulary now -- but a SAVED analysis (or a run in the window
  # before the maintainer's next prepare()) can still carry a retired spelling, and a lifecycle
  # warning has no business in the results panel. It resolves silently.
  # WARNING (19l): base R, not withr::with_options() -- withr is Suggests-only, so calling it from
  # live package code is a hard failure on a machine that does not have it (reg-assumptions.R states
  # the same rule and hand-rolls the seed save/restore for it).
  old_lv <- options(lifecycle_verbosity = "quiet")
  r_ci   <- tryCatch(
    resolve_leaf_ci(opts$ci, jmv_tab3_measure(color), color_signif, opts$stars,
                    if (length(opts$ref)) opts$ref else "auto"),
    finally = options(old_lv))
  ci <- r_ci$ci

  ce <- new.env(parent = emptyenv())
  ce$store  <- jmv_cache_migrate(store)
  ce$hits   <- list(agg = logical(0), test = logical(0))
  ce$nrow   <- nrow(data)
  ce$fp_map <- jmv_fp_map(data, c(row_vars, col_vars, tab_vars, if (length(wt)) wt))

  # Phase 5 colour spec -- applied FRESH on every interaction (re-paint of the cached fmt cells).
  spec <- normalize_color_spec(color, color_signif)

  # --- Tier 3: reuse the built ARMED table when only display / colour / reference changed ---------
  base_key <- jmv_tab3_base_key(opts, ce, row_vars, col_vars, tab_vars, wt_chr)
  arming   <- jmv_tab3_arming(color)
  tuple    <- jmv_tab3_tuple(opts, ci, arming, measure_geometry(jmv_tab3_measure(color)))
  got      <- jmv_cache_fetch(ce$store, "tab3", base_key)
  ce$store <- got$store

  # Phase 9b-7: the tier-3 payload is the armed CARRIER (plain field-frames, jmv_carrier_unwrap), so
  # all three branches yield a carrier and the tail below re-paints on plain fields then materializes
  # ONCE. `jmv_reref_shape_ok()` (increment 2) restricts the re-ref to the byte-identical pct="row"
  # case; anything else (or jmv_tab3_rerefable() == FALSE) falls through to the always-correct rebuild.
  if (got$hit && identical(got$value$tuple, tuple)) {
    carrier <- got$value$carrier                                         # exact: display / colour re-paint
    reused  <- TRUE
  } else if (got$hit && jmv_tab3_rerefable(got$value$tuple, tuple) &&
             jmv_reref_shape_ok(opts, has_num_col)) {
    carrier <- jmv_tab3_reref(got$value$carrier, opts, ci, tuple)        # reference / ci re-ref (Phase 9b-7)
    reused  <- TRUE
    ce$store <- jmv_cache_put(ce$store, "tab3", base_key,                # store under the NEW tuple, so a
                              list(carrier = carrier, tuple = tuple))    #   second identical ref is a re-paint hit
  } else {
    armed   <- jmv_tab3_build_armed(data, opts, color, "ignore", ci, wt_sym,
                                    row_vars, col_vars, tab_vars, ce)    # canonical armed (see above)
    carrier <- jmv_carrier_unwrap(armed)
    reused  <- FALSE
    ce$store <- jmv_cache_put(ce$store, "tab3", base_key,
                              list(carrier = carrier, tuple = tuple))
  }
  ce$store <- jmv_cache_evict(ce$store)
  ce$hits$tab3 <- reused          # TRUE = armed carrier reused (re-paint / re-ref) -> no O(cells) rebuild

  # Tier-4 re-paint: digits on the plain carrier -> materialize ONCE -> colour/display on the record.
  # Order is byte-identical to the former finalize->digits->display (colour attrs / digits fields /
  # display fields are independent slots); finalize_color_spec (shared, attr-only) + tab_apply_display
  # (its mutate handles the grouped-class downgrade) stay on the record.
  carrier <- jmv_reapply_digits(carrier, opts$digits)  # digits (proportion + mean magnitude floor)
  tabs <- jmv_carrier_wrap(carrier)                    # materialize the fmt records ONCE
  tabs <- finalize_color_spec(tabs, spec)              # colour / policy (measure diff<->ratio, grey<->all)
  tabs <- jmv_reapply_anova(tabs, opts$anova)          # which stored F the p-value line shows
  # display combobox -- tab()'s own writer. WARNING (19k): a SAVED analysis, or one running against a
  # `.h.R` older than the `.a.yaml`, can still carry a display value tab() retired (`pct_ci`,
  # `OR_pct`...), and validate_display_template() ABORTS on those. A generated layer that lags is a
  # fact of this module (see .opts()'s `%||%` discipline), so an unusable value degrades to "the
  # display the table was built with" instead of blanking the results panel. This is robustness
  # about a stale artefact, NOT a second vocabulary: nothing is translated, the value is dropped.
  tabs <- tryCatch(tab_apply_display(tabs, opts$display), error = function(e) tabs)
  if (isTRUE(opts$cleannames)) tabs <- jmvtab_cleannames_display(tabs)
  # Phase 7g: n_min small-base filter -- tier 4, applied to the RETURNED copy only (never the
  # cached `armed` table), so toggling n_min is a cheap re-derive from the full armed table.
  if (length(opts$n_min) > 0 && any(opts$n_min > 0, na.rm = TRUE)) {
    tabs <- if (is.data.frame(tabs)) tab_apply_n_min(tabs, opts$n_min)
            else purrr::map(tabs, tab_apply_n_min, n_min = opts$n_min)
  }

  list(tabs = tabs, store = ce$store, hits = ce$hits)
}
