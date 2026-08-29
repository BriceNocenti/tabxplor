# PURPOSE: The cache kernel both jamovi modules ride, plus the jmvtab (Crosstables) crosstab store --
#          a content-addressed, byte-bounded LRU that lets one button change redo only what changed.
# ROLE: No math is forked here and no rule is mirrored. jmvtab_build() calls tab() with a mutable
#       `cache_env` injected through `.cache`; tab()'s aggregate stage delegates to
#       jmv_cache_aggregate(), and the leaves (tab_plain / tab_num) run verbatim. The population
#       descriptor is tab_cache_keys(), the digits floor num_digits_floor(), the display writer
#       tab_apply_display(), the `ci` anchor resolve_leaf_ci() -- one definition each, shared with
#       tab(). Option NAMES and VALUES are tab()'s own, so nothing is translated at this boundary.
# THE TIERS -- a key is a content hash; a miss recomputes, a hit reuses:
#   1  per-(row_var x col_var) count aggregates + per-row_var moment aggregates
#   2  the omnibus test
#   3  the built ARMED table, stored as a CARRIER (plain field-frames, not a live tab): re-painted on
#      an exact tuple hit, re-ref'd when only the reference moved, rebuilt otherwise
#   4  re-applied to a fetched carrier and never baked into it -- digits, colour, display, cleannames,
#      n_min, anova: JMV_TAB3_REAPPLIED, whose COMPLEMENT is the tier-3 base key
# KEY CONSTRAINTS:
#   - Store atomic-vector LISTS: never a live data.table (deserialising breaks .internal.selfref) and
#     never a live tab. Rebuild with setDT() / fmt_wrap() on read, preserving factor level order.
#   - Tiers 1-2 are keyed on FULL names, NA kept, raw levels (`.defer_level_merge`): cleannames is
#     display-tier and the level merge pre-aggregate. `na` keep/drop SHARE the factor aggregate;
#     numeric `na` is in the key.
#   - ⚠ A PRE-AGGREGATE RECODE MUST BE IN THE TIER-1/2 KEYS. The level merge and the numeric cut
#     (`shape`) change what is COUNTED, while `fp_map` fingerprints the RAW columns -- so both travel
#     as ONE per-variable slot, and a cut that renames its column reaches its source fingerprint
#     through `ctx$shape_renames`. A LEVEL REORDER is the opposite and must NEVER be baked into the
#     stored blob: it relevels the in-memory aggregate after the fetch, so tiers 1-2 are reused.
#   - What a re-ref RECOMPUTES may differ between two tuples (ref / ref2 / the interval geometry /
#     the CI method); what it COPIES must not.
#   - The result is byte-identical to tab(cleannames = FALSE, .defer_level_merge = TRUE), locked by
#     test-jmvtab-cache.R; the option vocabulary is locked by test-jamovi-vocabulary.R.
# See: CLAUDE.md § tabxplor architecture (jamovi) ; dev/jamovi_module.md.


# === Cache kernel (shared by the jmvtab crosstab store AND the jmvtabreg fit store) =========
#
# DESIGN: `clock` is a monotone LOGICAL counter, never Sys.time -- that would break determinism.
# DESIGN: two access patterns are kept deliberately distinct: jmv_store_fetch/put are FUNCTIONAL
# (store threaded by return value, clock bumped on EVERY touch including a miss); jmv_store_cached is
# ENV-MUTATING (clock bumped only on a hit or a store). `cfg` carries the schema and byte budget, so
# only a function that DECIDES something takes it -- jmv_store_fetch() alone has none.

#' @noRd
jmv_cache_config <- function(schema, entry_bytes, store_bytes) {
  list(schema = schema, tiers = names(entry_bytes),
       entry_bytes = entry_bytes, store_bytes = store_bytes)
}

#' @noRd
jmv_store_new <- function(cfg) {
  c(list(schema = cfg$schema, clock = 0L),
    stats::setNames(rep(list(list()), length(cfg$tiers)), cfg$tiers))
}

#' @noRd
jmv_store_migrate <- function(cfg, store) {
  if (is.null(store) || !is.list(store) || !identical(store$schema, cfg$schema)) {
    return(jmv_store_new(cfg))
  }
  store
}

#' @noRd
jmv_store_env <- function(cfg, store = NULL) {
  ce <- new.env(parent = emptyenv())
  ce$store  <- jmv_store_migrate(cfg, store)
  ce$hits   <- 0L
  ce$misses <- 0L
  ce
}

#' @noRd
jmv_store_fetch <- function(store, tier, key) {
  store$clock <- store$clock + 1L
  e <- store[[tier]][[key]]
  if (is.null(e)) return(list(hit = FALSE, value = NULL, store = store))
  e$seq <- store$clock
  store[[tier]][[key]] <- e
  list(hit = TRUE, value = e$value, store = store)
}

# DESIGN: an oversized entry is silently dropped rather than raising -- recomputing next run beats
# persisting a large blob forever.
#' @noRd
jmv_store_put <- function(cfg, store, tier, key, value) {
  store$clock <- store$clock + 1L
  b <- length(serialize(value, connection = NULL))
  if (b > cfg$entry_bytes[[tier]]) return(store)
  store[[tier]][[key]] <- list(value = value, bytes = b, seq = store$clock)
  store
}

# DESIGN: one O(n log n) pass across all tiers (flatten once, sort by seq, evict oldest-first) rather
# than a per-tier rescan.
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
JMVTAB_CACHE_SCHEMA <- 20L   # bump on any store-shape change -> discard stale stores

# SCHEMA: three tiers -- agg/test hold the tier-1/2 aggregates, tab3 the built ARMED tables (a looser
# ceiling: a carrier carries all 21 fmt fields per column, not sufficient statistics).
JMVTAB_CFG <- jmv_cache_config(
  schema      = JMVTAB_CACHE_SCHEMA,
  entry_bytes = c(agg = 512L * 1024L, test = 512L * 1024L, tab3 = 2L * 1024L * 1024L),
  store_bytes = 12L * 1024L * 1024L   # whole-store budget (serialized every run -> keep bounded)
)


# === Store lifecycle (thin wrappers -> the shared kernel with JMVTAB_CFG) ===================
#' @noRd
jmv_cache_new <- function() jmv_store_new(JMVTAB_CFG)

#' @noRd
jmv_cache_migrate <- function(store) jmv_store_migrate(JMVTAB_CFG, store)

#' @noRd
jmv_cache_fetch <- function(store, tier, key) jmv_store_fetch(store, tier, key)

#' @noRd
jmv_cache_put <- function(store, tier, key, payload) jmv_store_put(JMVTAB_CFG, store, tier, key, payload)

#' @noRd
jmv_cache_evict <- function(store) jmv_store_evict(JMVTAB_CFG, store)


# === Hashing ================================================================================

#' @noRd
jmv_hash <- function(x) rlang::hash(x)

# WARNING: a same-shape value edit (class, factor levels, NA count unchanged) is not caught below --
# a stale entry can be served until a structural change occurs.
# DESIGN: JMV_FULL_HASH forces an exact full-value hash, for diagnosing a stale-cache report.
#' @noRd
JMV_FULL_HASH <- FALSE

#' @noRd
jmv_col_fp <- function(col) {
  if (isTRUE(JMV_FULL_HASH)) return(jmv_hash(col))
  jmv_hash(list(class(col), if (is.factor(col)) levels(col) else NULL, sum(is.na(col))))
}

#' @noRd
jmv_fp_map <- function(data, used_vars) {
  used_vars <- unique(intersect(used_vars, names(data)))
  stats::setNames(lapply(used_vars, function(v) jmv_col_fp(data[[v]])), used_vars)
}

# DESIGN: na keep/drop share the "full" tag -- factor aggregates are NA-kept and shared across both.
# drop_all / common_base hash their population-defining variables' fingerprints instead, since they
# legitimately change the population and must not share the tag.
#' @noRd
jmv_pop_tag <- function(population, fp_map, nrow_data) {
  if (is.character(population) && identical(population, "full")) {
    return(list("full", nrow_data))
  }
  list(population$mode, nrow_data,
       lapply(sort(unique(population$vars)), function(v) fp_map[[v]]))
}


# === data.table <-> atomic-vector-list bridge =============================================

#' @noRd
jmv_dt_to_cols <- function(dt) as.list(dt)

#' @noRd
jmv_cols_to_dt <- function(cols) data.table::as.data.table(cols)


# === STAGE 3 replacement: content-addressed tier-1 aggregates + tier-2 test keys ==========

#' @noRd
jmv_cache_aggregate <- function(ctx) {
  ce    <- ctx$cache_env
  store <- ce$store
  data  <- ctx$data

  row_vars      <- as.character(ctx$row_vars)
  col_vars      <- as.character(ctx$col_vars)
  fct_cols      <- col_vars[ctx$settings$cols$is_text]
  num_cols      <- col_vars[ctx$settings$cols$is_num]
  tab_vars      <- as.character(ctx$tab_vars)
  wt            <- ctx$wt                       # symbol or character()
  weighted      <- length(wt) != 0L
  wt_chr        <- if (weighted) as.character(wt) else ""
  grain         <- sort(tab_vars)
  fp            <- ce$fp_map                    # per-column fingerprints (built in jmvtab_build)
  pop_tag       <- jmv_pop_tag(ctx$cache_keys$tier0$population, fp, ce$nrow)
  grain_fp      <- lapply(grain, function(g) fp[[g]])
  wt_fp         <- if (weighted) fp[[wt_chr]] else NULL
  # WARNING: pre-aggregate recodes (merge + shape cut) must be in these tier-1 keys (see header) --
  # `fp` is fingerprinted on data BEFORE the recode.
  cl            <- ctx$levels_collapse
  sh            <- ctx$shapes
  recode        <- function(v) list(cl[[v]], sh[[v]])
  grain_recode  <- lapply(grain, recode)
  # WARNING: a numeric-keeping shape (log/sqrt) renames its column (`log_age`), so `fp[["log_age"]]`
  # would be NULL and the source fingerprint would silently drop out of the key. `ctx$shape_renames`
  # sends the lookup back to the original column name.
  fp_of         <- function(v) {
    src <- unname(ctx$shape_renames[v])                  # `[` not `[[`: an unknown name gives NA
    fp[[if (length(src) && !is.na(src)) src else v]]
  }

  agg_hits  <- logical(0)
  test_hits <- logical(0)

  fine_fused <- NULL
  fct_keys_by_rv <- stats::setNames(vector("list", length(row_vars)), row_vars)
  if (length(fct_cols) > 0L) {
    fine_fused <- list()
    for (rv in row_vars) {
      for (cv in fct_cols) {
        if (cv %in% c(rv, tab_vars)) next
        key <- jmv_hash(list("fct", pop_tag, rv, fp_of(rv), cv, fp_of(cv),
                             grain, grain_fp, wt_chr, wt_fp,
                             recode(rv), recode(cv), grain_recode))
        fct_keys_by_rv[[rv]] <- c(fct_keys_by_rv[[rv]], key)
        got <- jmv_cache_fetch(store, "agg", key)
        store <- got$store
        agg_hits[[paste(rv, cv, sep = "\r")]] <- got$hit
        if (got$hit) {
          pair <- jmv_cols_to_dt(got$value$cols)
        } else {
          keycols <- c(tab_vars, rv, cv)
          dt <- data.table::as.data.table(data[c(keycols, if (weighted) wt_chr)])
          # DESIGN: sum(w^2) travels beside sum(w) whenever weighted, so toggling `design_effect`
          # stays a cache hit instead of forcing a rebuild.
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

  fine_num <- NULL
  num_keys_by_rv <- stats::setNames(vector("list", length(row_vars)), row_vars)
  if (length(num_cols) > 0L) {
    fine_num <- stats::setNames(vector("list", length(row_vars)), row_vars)
    for (i in seq_along(row_vars)) {
      rv     <- row_vars[[i]]
      na_rv  <- ctx$settings$rows$na_num[[i]]
      msr    <- sort(num_cols)
      msr_fp <- lapply(msr, fp_of)
      key    <- jmv_hash(list("num", pop_tag, rv, fp_of(rv), msr, msr_fp,
                              grain, grain_fp, wt_chr, wt_fp, na_rv,
                              recode(rv), lapply(msr, recode), grain_recode))
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
          data, !!rv_sym, as.character(num_cols), vars_chr(tab_vars),
          wt = !!wt_part, na = na_rv
        ))
        fine_num[[i]] <- agg
        store <- jmv_cache_put(store, "agg", key,
                               list(cols = jmv_dt_to_cols(agg), keys = c(tab_vars, rv)))
      }
    }
  }

  # DESIGN: the test key excludes pct/ref/ci/levels/color/digits -- none change the omnibus test. Used
  # only when chi2 is on and the colour is not a per-cell contribution measure (which writes ctr/var
  # fields not present in the cached test tibble).
  chi2      <- ctx$settings$rows$chi2
  color     <- ctx$settings$rows$color         # the ONE resolved measure
  comp      <- ctx$settings$rows$comp
  na_scalar <- ctx$na
  tier2_keys  <- stats::setNames(vector("list", length(row_vars)), row_vars)
  cached_tests <- stats::setNames(vector("list", length(row_vars)), row_vars)
  for (i in seq_along(row_vars)) {
    rv <- row_vars[[i]]
    if (!isTRUE(chi2[[i]]) || identical(color[[i]], "auto") ||
        identical(measure_builds(color[[i]]), "contrib")) next
    tkey <- jmv_hash(list("test", comp[[i]], na_scalar,
                          sort(unlist(fct_keys_by_rv[[rv]])), num_keys_by_rv[[rv]],
                          lapply(unique(c(rv, col_vars, tab_vars)), recode)))
    tier2_keys[[rv]] <- tkey
    got <- jmv_cache_fetch(store, "test", tkey)
    store <- got$store
    test_hits[[rv]] <- got$hit
    if (got$hit) cached_tests[[rv]] <- got$value
  }

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
    data <- jmv_relevel_cols(data, spec, unique(c(row_vars, col_vars, tab_vars)))
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

#' @noRd
jmvtab_cleannames_display <- function(tabs) {
  cond <- cleannames_condition()
  strip <- function(x) gsub(cond, "", x, perl = TRUE)
  one <- function(tb) {
    vars <- tab_get_vars(tb)
    label_cols <- intersect(c(vars$row_var, as.character(vars$tab_vars)), names(tb))
    for (col in label_cols) {
      col_sym <- rlang::sym(col)
      tb <- dplyr::mutate(tb, !!col := if (is.factor(!!col_sym))
        forcats::fct_relabel(!!col_sym, strip) else strip(!!col_sym))
    }
    hdr <- setdiff(names(tb), label_cols)
    tb <- dplyr::rename_with(tb, strip, tidyselect::all_of(hdr))
    tb
  }
  if (is.list(tabs) && !is.data.frame(tabs)) purrr::map(tabs, one) else one(tabs)
}


# WARNING: jamovi hands a nominal/ordinal integer column already factored, so a col_var that is
# numeric (or whose levels ALL parse as numbers) is coerced back, else it wrongly becomes one column
# per value.
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

#' @noRd
jmvtab_ref_vector <- function(ref_levels, free_text_ref = "auto") {
  if (length(ref_levels) == 0) return(free_text_ref)
  get1 <- function(e, k) { v <- e[[k]]; if (is.null(v)) NA_character_ else as.character(v) }
  vars <- vapply(ref_levels, get1, character(1), k = "var")
  refs <- vapply(ref_levels, get1, character(1), k = "ref")
  keep <- !is.na(vars) & nzchar(vars)
  vars <- vars[keep]; refs <- refs[keep]
  if (length(vars) == 0) return(free_text_ref)
  if (!any(!is.na(refs) & nzchar(refs))) return(free_text_ref)   # no explicit level -> free-text
  refs[is.na(refs) | !nzchar(refs)] <- "auto"
  stats::setNames(refs, vars)
}

#' @noRd
jmvtab_levels_order <- function(levels_order) {
  if (length(levels_order) == 0) return(NULL)
  out <- list()
  for (e in levels_order) {
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

#' @noRd
jmvtab_levels_collapse <- function(levels_collapse) {
  if (length(levels_collapse) == 0) return(NULL)
  out <- list()
  for (e in levels_collapse) {
    v <- e[["var"]]
    if (is.null(v) || !nzchar(as.character(v))) next
    lv <- e[["levels"]]
    if (is.null(lv) || length(lv) == 0) next
    lv <- as.character(unlist(lv, use.names = FALSE))
    lv <- lv[!is.na(lv) & nzchar(lv)]
    if (length(lv) < 2L) next                      # a run of one is not a merge
    lab <- e[["label"]]
    lab <- if (is.null(lab)) "" else as.character(lab)[1]
    if (is.na(lab)) lab <- ""
    v <- as.character(v)
    grp <- out[[v]] %||% list()
    grp[[length(grp) + 1L]] <- lv
    names(grp)[length(grp)] <- lab
    out[[v]] <- grp
  }
  new_lvl_collapse(out)
}

#' @noRd
jmv_order_after_collapse <- function(order, collapse) {
  if (length(order) == 0 || length(collapse) == 0) return(order)
  for (v in intersect(names(order), names(collapse))) {
    grp <- collapse[[v]]
    map <- stats::setNames(rep(names(grp), lengths(grp)), unlist(grp, use.names = FALSE))
    o   <- as.character(order[[v]])
    hit <- o %in% names(map)
    o[hit] <- unname(map[o[hit]])
    order[[v]] <- unique(o)
  }
  order
}

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

# WARNING: every name here must be an exact key of the `opts` list -- this is the NEGATIVE set (the
# tier-3 base key's complement), so a misspelt or retired name silently lands its option in the base
# key and its toggle rebuilds the whole table instead of re-painting.
JMV_TAB3_REAPPLIED <- c("digits", "display", "cleannames", "color", "color_signif",
                        "ref", "ref2", "comp", "ci", "conf_level",
                        "ci_method_cell", "ci_method_diff", "ci_method_mean_diff",
                        "ci_method_mean_ratio",
                        "stars", "n_min", "anova")

# The tier-3 BASE key identifies the ref-INDEPENDENT base fields (n/wn/pct/tot_n/mean/var): it hashes
# the aggregate identity (population + per-variable fingerprint + grain + wt) plus every opt EXCEPT
# JMV_TAB3_REAPPLIED, so pct/na/levels/totaltab/subtext/... any structural arg invalidates the entry.
#' @noRd
jmv_tab3_base_key <- function(opts, ce, row_vars, col_vars, tab_vars, wt_chr) {
  fp   <- ce$fp_map
  used <- sort(unique(c(row_vars, col_vars, tab_vars, if (nzchar(wt_chr)) wt_chr)))
  pop  <- tab_cache_keys(na = opts$na, row_vars = row_vars, col_vars = col_vars,
                         tab_vars = tab_vars)$tier0$population
  agg_id <- list(
    pop   = jmv_pop_tag(pop, fp, ce$nrow),
    vars  = lapply(used, function(v) list(v, fp[[v]])),
    wt    = wt_chr,
    grain = sort(tab_vars)
  )
  # WARNING: the four ci_method_* keys must be named by their REAL option names (jmv_ci_method() folds
  # them into one vector) -- naming them "ci_method" would land all four in `structural`, forcing a
  # full rebuild on every method toggle instead of the cheap re-ref path.
  structural <- opts[setdiff(names(opts), JMV_TAB3_REAPPLIED)]
  jmv_hash(list("tab3", agg_id, structural))
}

# DESIGN: the "arming" class (which measure FIELDS the armed carrier populated) IS MEASURES$builds
# (R/fmt_class.R) -- diff/ratio/auto share one class, so a toggle between them is a pure re-paint.
#' @noRd
jmv_tab3_arming <- function(color) {
  if (isFALSE(color)) return("off")
  if (isTRUE(color))  return("diff")
  m <- as.character(color)[1]
  if (identical(m, "auto")) return("diff")
  measure_builds(m)
}

#' @noRd
jmv_ci_method <- function(opts) {
  ui <- list(cell = opts$ci_method_cell, diff = opts$ci_method_diff,
             mean_diff = opts$ci_method_mean_diff, mean_ratio = opts$ci_method_mean_ratio)
  resolve_ci_method(unlist(purrr::compact(ui)), fn = "jmvtab")
}

#' @noRd
jmv_tab3_tuple <- function(opts, ci_resolved, arming, geom) {
  # DESIGN: `geom` follows the comparison, not the arming class (diff/ratio share one arming class but
  # need different interval geometry: percentage-point bounds vs Katz log-RR). `comparison` is stored
  # rather than the raw `display` string, since the armed carrier is built before tab_apply_display()
  # runs -- keying the string would force a rebuild on every display toggle instead of a re-paint.
  list(arming = arming, geom = geom,
       comparison = display_comparison(opts$display),
       ref = opts$ref, ref2 = opts$ref2,
       comp = opts$comp, ci = ci_resolved, conf_level = opts$conf_level,
       ci_method = jmv_ci_method(opts), stars = opts$stars)
}

#' @noRd
jmv_tab3_measure <- function(color) {
  if (isFALSE(color)) return("no")
  if (isTRUE(color))  return(measure_auto("pct", "text"))
  m <- as.character(color)[1]
  if (identical(m, "auto")) return(measure_auto("pct", "text"))
  k <- measure_key(m)
  if (is.na(k) || !nzchar(k)) "no" else k
}

# NOTE: color_signif is NOT a re-ref -- it is a pure re-paint (finalize_color_spec) that never enters
# this branch, because it never changes the tuple.
#' @noRd
jmv_tab3_rerefable <- function(old_tuple, new_tuple) {
  keys    <- c("arming", "comparison", "comp", "ci", "conf_level", "stars")
  recomp  <- c("ref", "ref2", "geom", "ci_method")
  identical(old_tuple[keys], new_tuple[keys]) &&
    !identical(old_tuple[recomp], new_tuple[recomp]) &&   # ... and at least one of them DID change
    identical(new_tuple$arming, "diff") &&                # diff/ratio/auto colour (not OR/contrib)
    !identical(new_tuple$comparison, "odds_ratio")
}

# The structural gate for jmv_tab3_reref(): pct="row", exactly one row_var, at least one col_var, no
# numeric columns, levels="all", no add_pct, comp="tab" -- any other shape falls through to the
# always-correct rebuild. `opts$row_vars`/`col_vars` are the pre-injection user selections, so an
# empty selection (a dummy table) is correctly excluded.
#' @noRd
jmv_reref_shape_ok <- function(opts, has_num_col) {
  identical(opts$pct, "row") &&
    length(opts$row_vars) == 1L &&
    length(opts$col_vars) >= 1L &&
    !has_num_col &&
    identical(opts$levels, "all") &&
    !isTRUE(opts$add_pct) &&
    identical(opts$comp, "tab")
}

#' @noRd
jmv_tab3_reref <- function(carrier, opts, ci_resolved, tuple) {
  if (is.null(carrier$is_fmt))                                   # output_list -> a list of carriers
    return(purrr::map(carrier, jmv_tab3_reref, opts = opts, ci_resolved = ci_resolved, tuple = tuple))

  row_var  <- opts$row_vars[[1]]
  tab_vars <- as.character(opts$tab_vars)
  comp     <- tuple$comp

  ref_v <- resolve_ref_vector(tuple$ref, row_var)
  if (identical(ref_v, "auto")) ref_v <- "tot"

  fmt_names <- names(carrier$fmt)
  pct_cols  <- fmt_names[vapply(fmt_names,
                                function(nm) identical(carrier$fmt[[nm]]$meta$pct_type, "row"),
                                logical(1))]
  n_field   <- carrier$fmt[[fmt_names[[1]]]]$frame$n

  label_cols <- c(tab_vars, row_var)
  labels     <- stats::setNames(lapply(label_cols, function(cn) carrier$factors[[cn]]), label_cols)
  tabs     <- data.table::as.data.table(labels)
  tabs_pct <- data.table::as.data.table(labels)
  for (nm in pct_cols) tabs_pct[, (nm) := carrier$fmt[[nm]]$frame$pct]

  totrow_vector <- carrier$fmt[[fmt_names[[1]]]]$frame$row_kind == "total"
  tottab_vector <- carrier$fmt[[fmt_names[[1]]]]$frame$in_tottab

  # WARNING: sweep ONE col_var at a time, not pooled -- an odds ratio's 2x2 pairs a level against
  # ref2 OF THE SAME VARIABLE, so pooling every col_var's levels into one sweep reads a wrong pair
  # across variables (diff/ratio are column-wise and pool safely; `or` does not).
  ref_1 <- switch(as.character(ref_v), "no" = "", "tot" = "tot", as.character(ref_v))
  inref <- logical(length(n_field))                             # ref = "tot" -> all FALSE, matching tab_plain()'s own convention
  by_cv <- split(pct_cols, vapply(pct_cols,
                                  function(nm) carrier$fmt[[nm]]$meta$col_var %||% "",
                                  character(1)))
  # WARNING: `degf` must be passed explicitly, or it silently falls back to z (too-narrow CIs).
  ci_scale_r <- if (identical(tuple$geom, "ratio")) "ratio" else "diff"
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
      # WARNING: match the STORED (post-rename) total-column FLAG, never the literal "Total" -- `grp`
      # holds final, already-renamed column names, so a translated locale would silently build the
      # reference 2x2 against the wrong column.
      totcol_vector = vapply(grp, function(nm) isTRUE(carrier$fmt[[nm]]$meta$totcol), logical(1)))

    if (!identical(ref_v, "tot")) inref[] <- ref_res$refrows
    for (nm in grp) {
      carrier$fmt[[nm]]$frame$diff  <- ref_res$diff[[nm]]
      carrier$fmt[[nm]]$frame$ratio <- ref_res$ratio[[nm]]
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

#' @noRd
jmv_tab3_build_armed <- function(data, opts, color, color_signif, ci, wt_sym,
                                 row_vars, col_vars, tab_vars, ce) {
  if (length(opts$total_names)) {
    .old <- options(tabxplor.total_names = tab_total_names_merge(opts$total_names))
    on.exit(options(.old), add = TRUE)
  }
  rlang::inject(tab(
    data,
    row_vars     = tidyselect::all_of(row_vars),
    col_vars     = tidyselect::all_of(col_vars),
    tab_vars     = tidyselect::all_of(tab_vars),
    wt           = !!wt_sym,
    pct          = opts$pct,
    color        = color,
    color_signif = color_signif,
    display      = opts$display,
    ref2         = opts$ref2,
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
    # ⚠ [["n"]], never $n: `$` PARTIAL-MATCHES on a list, so `opts$n` would return `opts$na`.
    n            = opts[["n"]],
    add_pct      = opts$add_pct,
    subtext      = opts$subtext,
    output_list   = isTRUE(opts$output_list),
    .cache = ce, .defer_level_merge = TRUE, .return_armed = TRUE,
    .levels_order = opts$levels_order,         # post-aggregate reorder (jmv_cache_aggregate)
    .levels_collapse = opts$levels_collapse,   # pre-aggregate merge (tab_prepare)
    shape = opts$shape                         # pre-aggregate cut (tab_prepare_pop)
  ))
}

# === Carrier: tier-3 stores plain field-frames, not a live materialized tab ================
# SCHEMA: the tier-3 CARRIER (fmt_unwrap()) is per-fmt-col list(frame = the raw field vectors, meta =
# the per-column attributes) + the factor columns + the table attrs, instead of a live tabxplor_tab --
# matching tiers 1-2's "plain atomic-vector lists" discipline. A single carrier is discriminated from
# an output_list by its `is_fmt` slot.
#' @noRd
jmv_carrier_unwrap <- function(tabs) {
  if (is.data.frame(tabs)) fmt_unwrap(tabs) else purrr::map(tabs, fmt_unwrap)
}
#' @noRd
jmv_carrier_wrap <- function(carrier) {
  if (!is.null(carrier$is_fmt)) fmt_wrap(carrier) else purrr::map(carrier, fmt_wrap)
}

#' @noRd
jmv_reapply_digits <- function(carrier, digits) {
  one <- function(cr) {
    for (nm in names(cr$fmt)) {
      frame  <- cr$fmt[[nm]]$frame
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

#' @noRd
jmvtab_build <- function(data, opts, store) {
  row_vars <- opts$row_vars
  col_vars <- opts$col_vars
  tab_vars <- opts$tab_vars
  # DESIGN: a transient 0-row `data` must degrade gracefully -- both the placeholder injection below
  # and tab() itself would abort on 0 rows.
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

  # DESIGN: `color_signif` is a pure RE-PAINT for FACTOR columns (the armed table is always built
  # with color_signif = "ignore"), but NUMERIC means are the exception: ci = "auto" adds no mean CI,
  # so a policy there truly adds a field and forces a rebuild.
  color        <- switch(opts$color, "no" = FALSE, "auto" = TRUE, opts$color)
  color_signif <- opts$color_signif
  has_num_col  <- any(vapply(col_vars, function(cv) is.numeric(data[[cv]]), logical(1)))
  # `ci` here MUST be resolved through the ONE shared resolve_leaf_ci() call, never a second
  # hand-mirrored rule -- it drives the tuple, the armed build and the reref alike.
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

  spec <- normalize_color_spec(color, color_signif)

  base_key <- jmv_tab3_base_key(opts, ce, row_vars, col_vars, tab_vars, wt_chr)
  arming   <- jmv_tab3_arming(color)
  tuple    <- jmv_tab3_tuple(opts, ci, arming, measure_geometry(jmv_tab3_measure(color)))
  got      <- jmv_cache_fetch(ce$store, "tab3", base_key)
  ce$store <- got$store

  if (got$hit && identical(got$value$tuple, tuple)) {
    carrier <- got$value$carrier                                         # exact: display / colour re-paint
    reused  <- TRUE
  } else if (got$hit && jmv_tab3_rerefable(got$value$tuple, tuple) &&
             jmv_reref_shape_ok(opts, has_num_col)) {
    carrier <- jmv_tab3_reref(got$value$carrier, opts, ci, tuple)        # reference / ci re-ref
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

  carrier <- jmv_reapply_digits(carrier, opts$digits)  # digits (proportion + mean magnitude floor)
  tabs <- jmv_carrier_wrap(carrier)                    # materialize the fmt records ONCE
  tabs <- finalize_color_spec(tabs, spec)              # colour / policy (measure diff<->ratio, grey<->all)
  tabs <- jmv_reapply_anova(tabs, opts$anova)          # which stored F the p-value line shows
  tabs <- tryCatch(tab_apply_display(tabs, opts$display), error = function(e) tabs)
  if (isTRUE(opts$cleannames)) tabs <- jmvtab_cleannames_display(tabs)
  if (length(opts$n_min) > 0 && any(opts$n_min > 0, na.rm = TRUE)) {
    tabs <- if (is.data.frame(tabs)) tab_apply_n_min(tabs, opts$n_min)
            else purrr::map(tabs, tab_apply_n_min, n_min = opts$n_min)
  }

  list(tabs = tabs, store = ce$store, hits = ce$hits)
}
