# PURPOSE: The jmvtab live-UI multi-tier cache (Phase 7e) -- a content-addressed store that lets
#          each button change in the jamovi module redo only what genuinely changed.
# ROLE: Drives the SAME five-stage pipeline tab() uses (tab_setup -> tab_prepare_pop -> tab_aggregate
#       -> tab_transform -> tab_assemble). jmvtab_build() calls tab() with a mutable `cache_env`
#       injected via `.cache`; the aggregate stage's hook (tab_aggregate, R/tab.R) delegates here to
#       jmv_cache_aggregate(), which builds per-(row_var x col_var) count aggregates + per-row_var
#       moment aggregates + tier-2 test keys with content-addressed reuse, mutating cache_env$store.
#       NO math is forked -- the leaves (tab_plain/tab_num/tab_chi2/tab_ci) are reused verbatim.
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
#   - Byte-identical to tab(cleannames = FALSE, levels via defer_level_merge) -- locked by
#     test-jmvtab-cache.R. First cut: exact-grain keying (grain-superset rollup deferred), simple
#     byte-bounded LRU (byte-precise accounting deferred).
# See: dev/tabxplor_jmvtab_cache_design.md ; CLAUDE.md > 1.4.0 roadmap > Phase 7e.


# === Constants =============================================================================
JMVTAB_CACHE_SCHEMA         <- 3L                  # bump on any store-shape change -> discard stale stores
                                                   #   (2 = Phase 7f: added the tier-3 `tab3` built-table tier)
                                                   #   (3 = Phase 9b-7: tier-3 stores the CARRIER (plain field
                                                   #    frames via fmt_unwrap), not a live materialized tab)
JMVTAB_MAX_ENTRY_BYTES      <- 512L * 1024L        # per-entry ceiling for tiers 1-2 (aggregates / tests)
JMVTAB_TAB3_MAX_ENTRY_BYTES <- 2L * 1024L * 1024L  # tier-3 armed CARRIERS are bigger (all 18 fmt fields) -> looser
JMVTAB_MAX_STORE_BYTES      <- 12L * 1024L * 1024L  # whole-store budget (serialized every run -> keep bounded)


# === Store lifecycle =======================================================================

# A fresh empty store. `clock` is a monotone logical counter used for LRU (no Sys.time -- would break
# reproducibility / determinism). `agg` and `test` are named lists keyed by content hashes.
#' @keywords internal
#' @noRd
jmv_cache_new <- function() {
  # tab3 (Phase 7f): per base-config (aggregate x pct x na x levels x structural) built ARMED tables
  # (pre-finalize), stored since Phase 9b-7 as the CARRIER (plain field-frames, jmv_carrier_unwrap).
  # Reused for display / colour re-paint (exact-tuple hit) and reference re-ref (rerefable tuple), so
  # display/colour/reference toggles skip the O(cells) rebuild.
  list(schema = JMVTAB_CACHE_SCHEMA, clock = 0L, agg = list(), test = list(), tab3 = list())
}

# Restore-or-reset: a NULL state (first run) or a schema mismatch (module upgraded between sessions)
# yields a fresh store rather than a stale-shaped deserialization.
#' @keywords internal
#' @noRd
jmv_cache_migrate <- function(store) {
  if (is.null(store) || !is.list(store) ||
      !identical(store$schema, JMVTAB_CACHE_SCHEMA)) {
    return(jmv_cache_new())
  }
  store
}


# === get / put / evict =====================================================================

# Fetch an entry, refreshing its LRU stamp on a hit. Returns list(hit, value, store) so the (bumped)
# store is threaded back. `tier` is "agg" or "test".
#' @keywords internal
#' @noRd
jmv_cache_fetch <- function(store, tier, key) {
  store$clock <- store$clock + 1L
  e <- store[[tier]][[key]]
  if (is.null(e)) return(list(hit = FALSE, value = NULL, store = store))
  e$seq <- store$clock
  store[[tier]][[key]] <- e
  list(hit = TRUE, value = e$payload, store = store)
}

# Insert/replace an entry unless it exceeds the per-entry byte ceiling (recomputing one scan next run
# beats persisting a large blob forever). `max_bytes` is looser for the tier-3 built tables.
#' @keywords internal
#' @noRd
jmv_cache_put <- function(store, tier, key, payload, max_bytes = JMVTAB_MAX_ENTRY_BYTES) {
  store$clock <- store$clock + 1L
  b <- length(serialize(payload, connection = NULL))
  if (b > max_bytes) return(store)
  store[[tier]][[key]] <- list(payload = payload, bytes = b, seq = store$clock)
  store
}

# Evict least-recently-used entries across ALL tiers until the total serialized size is under budget.
#' @keywords internal
#' @noRd
jmv_cache_evict <- function(store) {
  tier_ent <- function(tier) lapply(names(store[[tier]]), function(k)
    list(tier = tier, key = k, seq = store[[tier]][[k]]$seq, bytes = store[[tier]][[k]]$bytes))
  ent <- c(tier_ent("agg"), tier_ent("test"), tier_ent("tab3"))
  if (length(ent) == 0L) return(store)
  total <- sum(vapply(ent, function(e) e$bytes, numeric(1)))
  if (total <= JMVTAB_MAX_STORE_BYTES) return(store)
  ord <- order(vapply(ent, function(e) e$seq, numeric(1)))  # oldest first
  for (i in ord) {
    if (total <= JMVTAB_MAX_STORE_BYTES) break
    e <- ent[[i]]
    store[[e$tier]][[e$key]] <- NULL
    total <- total - e$bytes
  }
  store
}


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
  fct_cols      <- col_vars[ctx$col_vars_text]
  num_cols      <- col_vars[ctx$col_vars_num]
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
          pair <- if (weighted) {
            dt[, list(n = .N, wn = sum(as.numeric(eval(rlang::sym(wt_chr))), na.rm = TRUE)),
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
      na_rv  <- ctx$na_num[[i]]
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
  chi2      <- ctx$chi2
  color_ctr <- ctx$color_ctr
  comp      <- ctx$comp
  na_scalar <- ctx$na
  tier2_keys  <- stats::setNames(vector("list", length(row_vars)), row_vars)
  cached_tests <- stats::setNames(vector("list", length(row_vars)), row_vars)
  for (i in seq_along(row_vars)) {
    rv <- row_vars[[i]]
    if (!isTRUE(chi2[[i]]) || color_ctr[[i]] != "no") next
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
    lv1 <- ctx$lv1
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


# === Display-field overrides (moved out of jmvtab.b.R, so jmvtab_build is engine-free) ====

# The jamovi `display` ComboBox + the ci="cell" pct_ci convenience, applied to a tab or list of tabs.
#' @keywords internal
#' @noRd
jmv_apply_display <- function(tabs, opts) {
  one <- function(tb) {
    if (opts$display != "auto") {
      tb <- dplyr::mutate(tb, dplyr::across(dplyr::where(is_fmt),
                                            ~ set_display(., opts$display)))
    }
    if (opts$ci == "cell" && opts$pct %in% c("row", "col")) {
      tb <- dplyr::mutate(tb, dplyr::across(
        dplyr::where(is_fmt) &
          -(tidyselect::any_of(c("n", "wn")) & dplyr::where(~ get_type(.) == "n")),
        ~ set_display(., "pct_ci")
      ))
    }
    tb
  }
  if (is.list(tabs) && !is.data.frame(tabs)) purrr::map(tabs, one) else one(tabs)
}


# Coerce numeric-valued col_vars back to numeric so they become MEAN columns, matching plain R
# tab() exactly (integer/double col_var -> mean). jamovi hands a nominal/ordinal integer to the
# module ALREADY factored (levels "0".."24"), losing its numeric type -- so `tvhours` would wrongly
# become one column per value. A col_var that is numeric, or a factor whose levels ALL parse as
# numbers, is treated as numeric. CAVEAT (documented, dev/tabxplor_1.4.0_jamovi_dev.md): a genuinely
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

# The persisted population descriptor for the tier-3 base key. Mirrors tab_cache_keys() (R/tab-resolve.R)
# so the base fields the cached armed table holds are content-addressed exactly like the tier-1
# aggregate they derive from. na keep/drop -> "full"; drop_all / common_base carry their vars.
#' @keywords internal
#' @noRd
jmv_population_descriptor <- function(na, row_vars, col_vars, tab_vars) {
  if (na %in% c("keep", "drop")) return("full")
  if (na == "drop_all")
    return(list(mode = "drop_all", vars = sort(unique(c(row_vars, col_vars, tab_vars)))))
  if (na == "common_base")
    return(list(mode = "common_base",
                vars = c(row_vars, if (length(col_vars) != 0L) col_vars[1] else NULL, tab_vars)))
  "full"
}

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
  pop  <- jmv_population_descriptor(opts$na, row_vars, col_vars, tab_vars)
  agg_id <- list(
    pop   = jmv_pop_tag(pop, fp, ce$nrow),
    vars  = lapply(used, function(v) list(v, fp[[v]])),
    wt    = wt_chr,
    grain = sort(tab_vars),
    other = opts$other_if_less_than
  )
  reapplied  <- c("digits", "display", "cleannames", "color", "color_signif",
                  "ref", "ref2", "comp", "OR", "ci", "conf_level",
                  "method_cell", "method_diff", "stars", "n_min")
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
#' @keywords internal
#' @noRd
jmv_tab3_arming <- function(color) {
  if (isFALSE(color)) return("off")
  if (isTRUE(color))  return("diff")
  m <- as.character(color)[1]
  if (m %in% c("or", "OR"))    "or"
  else if (m == "contrib")     "contrib"
  else if (m %in% c("no", "")) "off"
  else                         "diff"
}

# The tier-3 TRANSFORM tuple: everything that changes field VALUES or POPULATION beyond the base. An
# exact match with the cached entry's tuple -> re-paint only; a difference -> re-ref (7f-4) or rebuild.
# `ci` is the RESOLVED ci (after the color_signif cascade), so grey<->color_all (same ci) re-paint
# while ignore<->grey (ci no<->diff) re-ref.
#' @keywords internal
#' @noRd
jmv_tab3_tuple <- function(opts, ci_resolved, arming) {
  list(arming = arming, or = opts$OR, ref = opts$ref, ref2 = opts$ref2, comp = opts$comp,
       ci = ci_resolved, conf_level = opts$conf_level,
       method_cell = opts$method_cell, method_diff = opts$method_diff, stars = opts$stars)
}

# Whether a cached armed CARRIER can be RE-REFERENCED (Phase 9b-7): only ref/ref2 changed and the
# shape jmv_tab3_reref() reproduces byte-identically (diff-armed, no OR). The full structural gate
# (pct="row", single row_var, no numeric cols, levels="all"...) is jmv_reref_shape_ok(), checked in
# jmvtab_build alongside this. Everything else -> the always-correct rebuild (fast: tiers 1-2 hit).
# NOTE the frequent color-driven `color_signif` toggle is NOT a reref: it is a pure re-paint
# (finalize_color_spec) that never enters this branch (same tuple -> exact-hit re-paint).
#' @keywords internal
#' @noRd
jmv_tab3_rerefable <- function(old_tuple, new_tuple) {
  keys <- c("arming", "or", "comp", "ci", "conf_level", "method_cell", "method_diff", "stars")
  identical(old_tuple[keys], new_tuple[keys]) &&                       # everything but ref/ref2 identical
    !identical(old_tuple[c("ref", "ref2")], new_tuple[c("ref", "ref2")]) &&  # ... and ref/ref2 DID change
    identical(new_tuple$arming, "diff") &&                            # diff/ratio/auto colour (not OR/contrib)
    identical(new_tuple$or, "no")                                     # empirical OR -> rebuild
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
    identical(opts$comp, "tab") &&
    # tab_ci gets color = "no" in the rebuild for every reref-eligible case EXCEPT color = "auto"
    # with an explicit ci = "diff" (which resolves to "after_ci" -> a ref-dependent CI colour the reref
    # would not reproduce). Exclude it -> rebuild. (color = "diff"/"ratio" always give color_ci = "no".)
    !(identical(opts$color, "auto") && identical(opts$ci, "diff"))
}

# Re-reference a cached armed CARRIER (Phase 9b-7): recompute the ref-DEPENDENT fields (diff/ratio +
# in_refrow + the diff-CI ci_inf/ci_sup/pvalue + the `ref` attr) from the ref-INDEPENDENT base fields
# (pct / n / wn / tot_n, all present in the cached carrier), for a new ref -- WITHOUT the O(cells)
# rebuild. Byte-identical to jmv_tab3_build_armed() with the new ref because it reuses the SAME shared
# math: tab_apply_reference() for diff/ratio (proven), tab_ci() for the CI (the diff CI depends on the
# reference). Phase 10i-B: the armed carrier is the "core" table -- post-compact / grouping but WITHOUT
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

  # Resolve the new reference exactly as the factor leaf would (OR off -> "auto" resolves to "tot").
  ref_v <- resolve_ref_vector(opts$ref, row_var)
  if (identical(ref_v, "auto")) ref_v <- "tot"

  fmt_names <- names(carrier$fmt)
  pct_cols  <- fmt_names[vapply(fmt_names,
                                function(nm) identical(carrier$fmt[[nm]]$meta$type, "row"),
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

  totrow_vector <- carrier$fmt[[fmt_names[[1]]]]$frame$in_totrow
  tottab_vector <- carrier$fmt[[fmt_names[[1]]]]$frame$in_tottab

  ref_res <- tab_apply_reference(
    tabs = tabs, tabs_pct = tabs_pct, ref = ref_v, ref2 = opts$ref2, comp = comp,
    OR = "no", color = "no", pct = "row", tab_row_names = label_cols,
    tab_vars = rlang::syms(tab_vars), row_var = rlang::sym(row_var),
    tottab_vector = tottab_vector, totrow_vector = totrow_vector,
    cols = stats::setNames(rep(TRUE, length(pct_cols)), pct_cols))

  # --- write diff/ratio (pct cols) + in_refrow / ref attr (ALL cols) into the carrier ------------
  ref_1 <- switch(as.character(ref_v), "no" = "", "tot" = "tot", as.character(ref_v))
  inref <- logical(length(n_field))                             # ref = "tot" -> all FALSE (tab_plain L3062)
  if (!identical(ref_v, "tot")) inref[] <- ref_res$refrows
  for (nm in pct_cols) {
    carrier$fmt[[nm]]$frame$diff  <- ref_res$diff[[nm]]
    carrier$fmt[[nm]]$frame$ratio <- ref_res$ratio[[nm]]
  }
  for (nm in fmt_names) {
    carrier$fmt[[nm]]$frame$in_refrow <- inref
    carrier$fmt[[nm]]$meta$ref        <- ref_1
  }

  # --- re-run the diff CI (the interval depends on the reference) via tab_ci() -------------------
  # The carrier has no p-value rows (Phase 10i-B), so tab_ci() sees the same grouped table the fresh
  # build's tab_ci() saw -- no ungroup/slice/regroup dance. fmt_wrap restores the carrier's grouping;
  # colour is kept from the cache (color = "no") and re-applied by finalize_color_spec; the input
  # `carrier` is otherwise returned untouched, so its attrs (test/groups/subtext) stay verbatim.
  if (!identical(ci_resolved, "no")) {
    rec  <- fmt_wrap(carrier)
    rec  <- tab_ci(tabs = rec, ci = ci_resolved, comp = comp, conf_level = tuple$conf_level,
                   color = "no", visible = identical(ci_resolved, "cell"), stars = tuple$stars,
                   method_cell = tuple$method_cell, method_diff = tuple$method_diff)
    ci_d <- fmt_unwrap(rec)
    for (nm in names(carrier$fmt)) {
      cd <- ci_d$fmt[[nm]]$frame
      carrier$fmt[[nm]]$frame$ci_inf  <- cd$ci_inf
      carrier$fmt[[nm]]$frame$ci_sup  <- cd$ci_sup
      carrier$fmt[[nm]]$frame$pvalue  <- cd$pvalue
    }
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
    OR           = opts$OR,
    # Phase 14a: tab()'s `chi2` is renamed `test`. The jamovi OPTION keeps the name `chi2` (its
    # .a.yaml/.h.R surface is compiled and regenerated by the maintainer, not renamed here).
    test         = opts$chi2,
    na           = opts$na,
    levels       = opts$levels,
    ref          = opts$ref,
    ref2         = opts$ref2,
    comp         = opts$comp,
    ci           = ci,
    conf_level   = opts$conf_level,
    stars        = opts$stars,
    method_cell  = opts$method_cell,
    method_diff  = opts$method_diff,
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
      base_d <- if (cr$fmt[[nm]]$meta$type == "mean") {
        m <- suppressWarnings(max(frame$mean, na.rm = TRUE))
        if (m <= 1) max(digits, 2L) else if (m <= 10) max(digits, 1L) else digits
      } else as.integer(digits)
      new_d    <- frame$digits
      new_d[]  <- base_d
      cr$fmt[[nm]]$frame$digits <- vctrs::vec_cast(new_d, integer())
    }
    cr
  }
  if (!is.null(carrier$is_fmt)) one(carrier) else purrr::map(carrier, one)
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
  # DESIGN (Last Phase a bug-fix): a transient 0-row `data` (the jamovi live UI mid-selection, or a
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
  ci           <- opts$ci
  has_num_col  <- any(vapply(col_vars, function(cv) is.numeric(data[[cv]]), logical(1)))
  if (has_num_col && !isFALSE(color) && color_signif != "ignore" && ci == "auto") ci <- "diff"
  # Phase 16f: mirror tab_resolve_settings' stars forcing (`stars = TRUE` makes ci = "no" -> "diff" on a
  # factor row/col pct or a mean, non-OR) so this resolved `ci` matches what tab() will actually build --
  # it drives the cache tuple, the armed build AND the reref (which recomputes the diff CI + pvalue only
  # when ci != "no"). Without it, an explicit ci = "no" + stars would arm a pvalue the reref never refreshes.
  if (isTRUE(opts$stars) && identical(ci, "no") &&
      !(opts$OR %in% c("OR", "or", "OR_pct", "or_pct")) &&
      (has_num_col || opts$pct %in% c("row", "col"))) ci <- "diff"

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
  tuple    <- jmv_tab3_tuple(opts, ci, arming)
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
                              list(carrier = carrier, tuple = tuple),    #   second identical ref is an exact
                              JMVTAB_TAB3_MAX_ENTRY_BYTES)               #   re-paint hit
  } else {
    armed   <- jmv_tab3_build_armed(data, opts, color, "ignore", ci, wt_sym,
                                    row_vars, col_vars, tab_vars, ce)    # canonical armed (see above)
    carrier <- jmv_carrier_unwrap(armed)
    reused  <- FALSE
    ce$store <- jmv_cache_put(ce$store, "tab3", base_key,
                              list(carrier = carrier, tuple = tuple), JMVTAB_TAB3_MAX_ENTRY_BYTES)
  }
  ce$store <- jmv_cache_evict(ce$store)
  ce$hits$tab3 <- reused          # TRUE = armed carrier reused (re-paint / re-ref) -> no O(cells) rebuild

  # Tier-4 re-paint: digits on the plain carrier -> materialize ONCE -> colour/display on the record.
  # Order is byte-identical to the former finalize->digits->display (colour attrs / digits fields /
  # display fields are independent slots); finalize_color_spec (shared, attr-only) + jmv_apply_display
  # (its mutate handles the grouped-class downgrade) stay on the record.
  carrier <- jmv_reapply_digits(carrier, opts$digits)  # digits (proportion + mean magnitude floor)
  tabs <- jmv_carrier_wrap(carrier)                    # materialize the fmt records ONCE
  tabs <- finalize_color_spec(tabs, spec)              # colour / policy (measure diff<->ratio, grey<->all)
  tabs <- jmv_apply_display(tabs, opts)                # display combobox + ci="cell" pct_ci
  if (isTRUE(opts$cleannames)) tabs <- jmvtab_cleannames_display(tabs)
  # Phase 7g: n_min small-base filter -- tier 4, applied to the RETURNED copy only (never the
  # cached `armed` table), so toggling n_min is a cheap re-derive from the full armed table.
  if (length(opts$n_min) > 0 && any(opts$n_min > 0, na.rm = TRUE)) {
    tabs <- if (is.data.frame(tabs)) tab_apply_n_min(tabs, opts$n_min)
            else purrr::map(tabs, tab_apply_n_min, n_min = opts$n_min)
  }

  list(tabs = tabs, store = ce$store, hits = ce$hits)
}
