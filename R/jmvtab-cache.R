# PURPOSE: The jmvtab live-UI multi-tier cache (Phase 7e) -- a content-addressed store that lets
#          each button change in the jamovi module redo only what genuinely changed.
# ROLE: Drives the SAME five-stage pipeline tab() uses (tab_setup -> tab_prepare_pop -> tab_aggregate
#       -> tab_transform -> tab_assemble). jmvtab_build() calls tab() with a mutable `cache_env`
#       injected via `.cache`; the aggregate stage's hook (tab_aggregate, R/tab.R) delegates here to
#       jmv_cache_aggregate(), which builds per-(row_var x col_var) count aggregates + per-row_var
#       moment aggregates + tier-2 test keys with content-addressed reuse, mutating cache_env$store.
#       NO math is forked -- the leaves (tab_plain/tab_num/tab_chi2/tab_ci) are reused verbatim.
# KEY CONSTRAINTS:
#   - Persist ONLY tiers 1 (aggregates) + 2 (omnibus tests): fmt assembly is O(cells), cheap to redo.
#   - Store atomic-vector lists, NEVER live data.table (readRDS breaks .internal.selfref); rebuild
#     with data.table::setDT() on read; preserve factor level order.
#   - Aggregate keyed on FULL names (cleannames is display-tier), NA-kept, raw/full levels
#     (defer_level_merge). na keep/drop SHARE the factor aggregate; numeric na is in the key.
#   - Byte-identical to tab(cleannames = FALSE, levels via defer_level_merge) -- locked by
#     test-jmvtab-cache.R. First cut: exact-grain keying (grain-superset rollup deferred), simple
#     byte-bounded LRU (byte-precise accounting deferred).
# See: dev/tabxplor_jmvtab_cache_design.md ; CLAUDE.md > 1.4.0 roadmap > Phase 7e.


# === Constants =============================================================================
JMVTAB_CACHE_SCHEMA    <- 1L                  # bump on any store-shape change -> discard stale stores
JMVTAB_MAX_ENTRY_BYTES <- 512L * 1024L        # per-entry ceiling: skip persisting above this
JMVTAB_MAX_STORE_BYTES <- 4L * 1024L * 1024L  # whole-store budget (serialized every run -> keep small)


# === Store lifecycle =======================================================================

# A fresh empty store. `clock` is a monotone logical counter used for LRU (no Sys.time -- would break
# reproducibility / determinism). `agg` and `test` are named lists keyed by content hashes.
#' @keywords internal
#' @noRd
jmv_cache_new <- function() {
  list(schema = JMVTAB_CACHE_SCHEMA, clock = 0L, agg = list(), test = list())
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
# beats persisting a large blob forever).
#' @keywords internal
#' @noRd
jmv_cache_put <- function(store, tier, key, payload) {
  store$clock <- store$clock + 1L
  b <- length(serialize(payload, connection = NULL))
  if (b > JMVTAB_MAX_ENTRY_BYTES) return(store)
  store[[tier]][[key]] <- list(payload = payload, bytes = b, seq = store$clock)
  store
}

# Evict least-recently-used entries across BOTH tiers until the total serialized size is under budget.
#' @keywords internal
#' @noRd
jmv_cache_evict <- function(store) {
  ent <- c(
    lapply(names(store$agg),  function(k) list(tier = "agg",  key = k,
                                               seq = store$agg[[k]]$seq,  bytes = store$agg[[k]]$bytes)),
    lapply(names(store$test), function(k) list(tier = "test", key = k,
                                               seq = store$test[[k]]$seq, bytes = store$test[[k]]$bytes))
  )
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
        agg <- rlang::inject(tab_aggregate_num(
          data, !!rv_sym, num_cols, tab_vars, wt = !!wt_part, na = na_rv
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

  ce$store <- store
  ce$hits  <- list(agg = agg_hits, test = test_hits)

  ctx_update(ctx, list(
    fine_num = fine_num, fine_fused = fine_fused,
    cached_tests = cached_tests, tier2_keys = tier2_keys
  ))
}


# Persist freshly-computed tier-2 tests (cache misses) after tab_transform(). Idempotent: only stores
# a key not already present, so hits are not re-serialized. Called from tab_build() when cache_env is
# set. ctx$tests is the per-row_var list of test tibbles (factor branch); a logical on numeric-only
# tables (skipped).
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
    if (is.null(tb)) next
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
  strip <- function(x) stringr::str_remove_all(x, cond)
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


# === The pure build core (engine-free, testable without a live jamovi session) ============

# Build the jmvtab table(s) from a plain option list + a cache store, reusing tab() end to end (its
# color spec, na translation, totals, recycling) with the cache injected through a mutable env. Runs
# with cleannames = FALSE + defer_level_merge = TRUE (cleannames applied at display below). Returns
# the built tab(s), the updated store (persist to $state), and per-pair/per-test hit flags.
#' @keywords internal
#' @noRd
jmvtab_build <- function(data, opts, store) {
  row_vars <- opts$row_vars
  col_vars <- opts$col_vars
  tab_vars <- opts$tab_vars
  if (length(row_vars) == 0L) { data$no_row_var <- factor("no_row_var"); row_vars <- "no_row_var" }
  if (length(col_vars) == 0L) { data$no_col_var <- factor("n");          col_vars <- "no_col_var" }
  data   <- jmv_coerce_numeric_cols(data, col_vars)   # integer/numeric col_var -> mean (match R)
  wt     <- opts$wt
  wt_sym <- if (length(wt)) rlang::sym(wt) else NULL

  # Color: "no" -> FALSE, "auto" -> TRUE, else the measure string. A significance policy needs a diff
  # CI; color = TRUE (auto) does not force it, so nudge ci = "diff" when a policy is set and ci = auto.
  color        <- switch(opts$color, "no" = FALSE, "auto" = TRUE, opts$color)
  color_signif <- opts$color_signif
  ci           <- opts$ci
  if (!isFALSE(color) && color_signif != "ignore" && ci == "auto") ci <- "diff"

  ce <- new.env(parent = emptyenv())
  ce$store  <- jmv_cache_migrate(store)
  ce$hits   <- list(agg = logical(0), test = logical(0))
  ce$nrow   <- nrow(data)
  ce$fp_map <- jmv_fp_map(data, c(row_vars, col_vars, tab_vars, if (length(wt)) wt))

  tabs <- rlang::inject(tab(
    data,
    row_vars     = tidyselect::all_of(row_vars),
    col_vars     = tidyselect::all_of(col_vars),
    tab_vars     = tidyselect::all_of(tab_vars),
    wt           = !!wt_sym,
    pct          = opts$pct,
    color        = color,
    color_signif = color_signif,
    OR           = opts$OR,
    chi2         = opts$chi2,
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
    cleannames   = FALSE,                        # cleannames applied at display (below)
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
    .cache = ce, .defer_level_merge = TRUE
  ))

  tabs <- jmv_apply_display(tabs, opts)
  if (isTRUE(opts$cleannames)) tabs <- jmvtab_cleannames_display(tabs)

  list(tabs = tabs, store = ce$store, hits = ce$hits)
}
