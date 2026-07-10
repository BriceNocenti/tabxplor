# PURPOSE: Opt-in FULL per-row_var parallelisation of tab_build() (Phase 8, ~3x on the survey workflow).
# ROLE: tab_build() prepares the population ONCE on main (tab_setup + tab_prepare_pop -- this is where
#   the global na="drop_all"/"common_base" drop lives, so it CANNOT move to a worker), then dispatches
#   the whole per-row_var pipeline -- tab_aggregate |> tab_transform |> tab_assemble_tables (the O(N)
#   scan + the O(cells) fmt build + chi2/ci) -- to a persistent mirai daemon pool via tab_pmap(). Main
#   gathers the finished per-row_var tabs and runs the cross-row_var output shape (tab_assemble_output:
#   merge/compact, p-value lines, unwrap). This is byte-identical to the serial build because a
#   single-row_var build now equals its slice of the integrated build (the tab_assemble total-col
#   decoupling fix, tab.R ~L1770), verified for every na mode -- so per-row_var == all-at-once.
# KEY CONSTRAINTS:
#   - mirai is Suggests-only: every use is guarded by requireNamespace(); absent -> serial fallback.
#   - Uses a NAMED compute profile ("tabxplor") so it never clobbers a user's own daemons() pool.
#   - Workers run the INSTALLED tabxplor (a fresh process); byte-identity requires main + workers to
#     run the same source -- automatic once installed (R CMD check installs first). In dev (load_all)
#     the parity test pre-warms the pool with pkgload::load_all() (test-only, see test-parallel-parity.R).
#   - jmvtab (live cache) is ALWAYS serial: tab_parallel_workers() returns 0 when ctx$cache_env is set
#     -> the serial full-ctx path (its cache hooks: jmv_cache_aggregate + jmv_cache_store_tests) is kept.
#   - The default (parallel off) path is UNCHANGED: tab_build() takes the serial full-ctx branch.
# See: CLAUDE.md 1.4.0 roadmap Phase 8 + dev/tabxplor_1.4.0_decisions.md 26.

# The mirai compute profile name -- isolates tabxplor's daemons from the user's default pool.
tabxplor_compute <- "tabxplor"


# tab_parallel_workers() -- resolve the worker count for one build.
# Returns 0L for "run serially" (the default, jmvtab, opt-out, or mirai absent), else N daemons.
# DESIGN: `parallel` (the tab() arg) wins over the option; TRUE = auto (physical cores - 1, capped at
# 8, since the §26 survey sweet spot saturates by ~W=8); an integer is taken verbatim. The
# _R_CHECK_LIMIT_CORES_ cap (2) keeps examples/tests within CRAN's 2-core rule.
#' @keywords internal
#' @noRd
tab_parallel_workers <- function(parallel = NULL, cache_env = NULL) {
  if (!is.null(cache_env)) return(0L)                       # jmvtab live cache: always serial
  p <- if (is.null(parallel)) getOption("tabxplor.parallel", FALSE) else parallel
  if (is.null(p) || isFALSE(p)) return(0L)
  if (!requireNamespace("mirai", quietly = TRUE)) {
    rlang::warn(
      paste0("`parallel` was requested but the {mirai} package is not installed; ",
             "running sequentially. Install it with install.packages(\"mirai\")."),
      .frequency = "once", .frequency_id = "tabxplor_no_mirai"
    )
    return(0L)
  }
  cap <- if (nzchar(Sys.getenv("_R_CHECK_LIMIT_CORES_"))) 2L else Inf
  if (isTRUE(p)) {
    nc <- parallel::detectCores(logical = FALSE)
    if (is.na(nc)) nc <- parallel::detectCores()
    if (is.na(nc)) nc <- 2L
    n <- min(max(1L, nc - 1L), 8L)
  } else {
    n <- as.integer(p)
    if (is.na(n) || n < 1L) return(0L)
  }
  as.integer(min(n, cap))
}


# tab_pool_ensure() -- lazily warm the named daemon pool once, reuse it across calls.
# Only (re)spawns when the current daemon count differs from `workers`, so a pre-warmed pool (e.g. the
# parity test's load_all'd daemons) is reused untouched.
#' @keywords internal
#' @noRd
tab_pool_ensure <- function(workers, compute = tabxplor_compute) {
  have <- tryCatch({
    st <- mirai::status(.compute = compute)
    n  <- st$connections
    if (length(n)) as.integer(n) else 0L
  }, error = function(e) 0L)
  if (isTRUE(have != workers)) {
    if (have > 0L) mirai::daemons(0, .compute = compute)
    mirai::daemons(workers, .compute = compute)
  }
  invisible(workers)
}


#' Stop the tabxplor parallel worker pool
#'
#' Shuts down the persistent \pkg{mirai} daemons tabxplor starts for
#' \code{tab(..., parallel = )}. The pool is otherwise reused for the whole session and cleaned up
#' automatically when the package is unloaded; call this to release the workers earlier.
#'
#' @return \code{invisible(NULL)}, called for its side effect.
#' @export
#' @examples
#' \donttest{
#' # after tab(..., parallel = TRUE)
#' tab_parallel_stop()
#' }
tab_parallel_stop <- function() {
  if (requireNamespace("mirai", quietly = TRUE)) {
    try(mirai::daemons(0, .compute = tabxplor_compute), silent = TRUE)
  }
  invisible(NULL)
}


# tab_pmap_trampoline() -- the per-unit callback that runs INSIDE a daemon.
# It is a top-level tabxplor function (serialized by reference, not by closure), so shipping it to a
# worker carries NO user data. It reads the big shipped objects (data / fine_fused) from the daemon
# global env (put there once by everywhere()), looks up the real worker by name in the tabxplor
# namespace, and calls it with EXACTLY the same named arguments the serial branch uses.
#' @keywords internal
#' @noRd
tab_pmap_trampoline <- function(row, .f_name, .const, .ship_names) {
  f    <- get(.f_name, envir = asNamespace("tabxplor"))
  ship <- mget(.ship_names, envir = .GlobalEnv)
  do.call(f, c(row, .const, ship))
}


# tab_pmap() -- map a namespaced worker over per-unit args, serial OR over a daemon pool.
# BYTE-IDENTITY: both branches call `do.call(f, c(row, .const, .ship))` for each transposed row, in
# input order. Serial passes the shipped objects as ordinary do.call args (copy-on-write, no copy);
# parallel ships them ONCE via everywhere() and the trampoline re-reads them from the daemon global.
# The worker body never branches on execution mode.
#   .l      : named list of per-unit vectors/lists (pmap-style; transposed to per-unit rows here).
#   .f_name : name of the worker in the tabxplor namespace (looked up on both sides).
#   .const  : small shared args, sent per task (parallel) / passed as constants (serial).
#   .ship   : big shared objects (data, fine_fused), shipped ONCE (parallel) / passed as args (serial).
#   workers : 0/1 -> serial; N -> N daemons. Also serial below tabxplor.parallel_min units.
#' @keywords internal
#' @noRd
tab_pmap <- function(.l, .f_name, .const = list(), .ship = list(),
                     workers = 0L, compute = tabxplor_compute) {
  f    <- get(.f_name, envir = asNamespace("tabxplor"))
  # Recycle length-1 per-unit args to the common length (pmap does this; transpose() does not) --
  # e.g. na = "drop_all" makes na_num / na_text a single "keep".
  rows <- purrr::transpose(vctrs::vec_recycle_common(!!!.l))

  serial <- workers <= 1L ||
    length(rows) < getOption("tabxplor.parallel_min", 2L) ||
    !requireNamespace("mirai", quietly = TRUE)

  if (serial) {
    return(purrr::map(rows, ~ do.call(f, c(.x, .const, .ship))))
  }

  tab_pool_ensure(workers, compute)

  # Ship the big objects + a tabxplor/data.table options snapshot + single-thread DT, ONCE. The
  # options snapshot keeps option-sensitive leaf math (e.g. the numeric-CI Kish n_eff) identical on
  # daemons; setDTthreads(1L) avoids workers x DT-threads oversubscription (grouped keyby sums are
  # thread-order-invariant, so this does NOT change results).
  mirai::everywhere(
    {
      options(tabx_opts)
      data.table::setDTthreads(1L)
      list2env(tabx_ship, envir = .GlobalEnv)
    },
    tabx_opts = options()[grepl("^tabxplor\\.|^datatable\\.", names(options()))],
    tabx_ship = .ship,
    .compute  = compute
  )

  mirai::mirai_map(
    rows, tab_pmap_trampoline,
    .args    = list(.f_name = .f_name, .const = .const, .ship_names = names(.ship)),
    .compute = compute
  )[.stop]
}


# tab_build_rowvar() -- build ONE row_var's tables: the numeric tab_num() + the per-col_var factor
# tab_plain()s (UNJOINED). Returns list(num = <tab_num | NULL>, text = <named list of col_var tabs |
# NULL>). This is the fused body of tab_transform()'s two per-row_var pmap sites (numeric + factor),
# extracted VERBATIM so it runs identically in-process or in a daemon. The join, duplicated_levels
# rename and tab_apply_tests() deliberately stay on the caller (main) -- see the file header.
# Args split: per-unit (row_var .. ref_vect_i) then shared (tab_vars .. fine_fused). row_var / wt /
# col_vars arrive as character / symbol (no environment) and are injected with `!!`.
#' @keywords internal
#' @noRd
tab_build_rowvar <- function(row_var, totaltab_i, totrow_i, ref_i, comp_i, color_num_i, ci_i,
                             na_num_i, fine_num_i, pct_i, ref2_i, OR_i, na_text_i, color_diff_OR_i,
                             ref_vect_i,
                             tab_vars, wt, col_vars, col_vars_num, col_vars_text, digits, conf_level,
                             stars, totaltab_name, total_names, by_table, data, fine_fused) {
  rv <- rlang::sym(row_var)
  # `wt` arrives as a character name (or character(0) for no weight) -- do.call() embeds a BARE
  # symbol as an unevaluated expression, so forcing it in the worker would look up a phantom
  # variable. Rebuild the symbol here; `wt = !!wt` below then matches tab_transform's former call
  # (a symbol when weighted, character(0) otherwise).
  wt <- if (length(wt) == 0L) wt else rlang::sym(wt)

  # --- numeric col_vars: one tab_num() (adopts the per-row_var moment aggregate fine_num_i) ---
  num <- NULL
  if (sum(col_vars_num) != 0) {
    num <- tab_num(data,
                   !!rv,
                   as.character(col_vars)[col_vars_num],
                   as.character(tab_vars),
                   wt         = !!wt,
                   na         = na_num_i,
                   digits     = digits[col_vars_num],
                   ref        = ref_i,
                   ci         = ci_i,
                   conf_level = conf_level,
                   stars      = stars,
                   comp       = comp_i,
                   color      = color_num_i,
                   totaltab   = totaltab_i,
                   totaltab_name = totaltab_name,
                   tot        = dplyr::if_else(totrow_i, "row", "no"),
                   total_names= total_names,
                   .fine      = fine_num_i,
                   .by_table  = by_table)
  }

  # --- factor col_vars: one tab_plain() per col_var (by column first), UNJOINED ---
  text <- NULL
  if (sum(col_vars_text) != 0) {
    text <- purrr::pmap(
      list(col_vars[col_vars_text], digits[col_vars_text], na_text_i,
           pct_i[col_vars_text], ref_vect_i[col_vars_text]),
      function(.col_vars, .digits, .na, .pct, .ref)
        tab_plain(data,
                  !!rv,
                  !!.col_vars,
                  as.character(tab_vars),
                  wt         = !!wt,
                  na         = .na,
                  digits     = .digits,
                  pct        = .pct,
                  ref        = .ref,
                  ref2       = ref2_i,
                  comp       = comp_i,
                  OR         = OR_i,
                  color      = color_diff_OR_i,
                  totaltab   = totaltab_i,
                  totaltab_name = totaltab_name,
                  tot        = c("row", "col"),
                  total_names= total_names,
                  .fine      = fine_for_pair(fine_fused, row_var, .col_vars),
                  .by_table  = by_table)
    ) %>%
      purrr::set_names(col_vars[col_vars_text])
  }

  list(num = num, text = text)
}


# tabxplor_rowvar_fields -- the ctx fields indexed per row_var (length == n_row_vars), enumerated from
# a live ctx captured right before tab_aggregate() (dev/inspect via trace). ctx_slice() subsets these
# to a single row_var; every other field (col_var-indexed, or scalar) is shared and kept verbatim.
# WARNING: if tab_setup()/tab_prepare_pop() ever add a new per-row_var ctx field, add it here too --
# a missing field would silently broadcast row_var 1's value to every worker. test-parallel-parity.R
# is byte-exact and catches it.
tabxplor_rowvar_fields <- c(
  "row_vars", "color", "OR", "chi2", "ref", "ref2", "comp", "ci",
  "totaltab", "totrow", "color_diff_OR", "color_ctr", "color_ci", "color_num",
  "ref_vect", "pct_vect", "na_text", "na_num"
)

# ctx_slice() -- narrow a post-prepare ctx to ONE row_var, ready to ship to a worker.
# Drops `data` (shipped once via tab_pmap's .ship) and the setup-only NSE quosures (they carry heavy
# enclosing environments that would drag user data into every task). Recomputes tab_row_names (it
# mixes tab_vars + row_vars, so a plain [i] would be wrong when tab_vars are present). Forces serial
# so the worker never spawns nested daemons.
#' @keywords internal
#' @noRd
ctx_slice <- function(ctx, i) {
  n_rv <- length(ctx$row_vars)
  s <- ctx[!grepl("_quo$", names(ctx))]
  s$data <- NULL
  # Slice ONLY fields actually of row_var length. Some (na_num / na_text under na="drop_all") collapse
  # to a SCALAR "keep" that the serial path recycles via map2() -- keep those as-is so the single-row_var
  # ctx recycles them the same way (slicing a length-1 "keep" at i>1 would yield NA -> tab_aggregate_num
  # assertion failure).
  for (nm in tabxplor_rowvar_fields) {
    x <- s[[nm]]
    if (!is.null(x) && length(x) == n_rv) s[[nm]] <- x[i]
  }
  s$tab_row_names <- as.character(c(ctx$tab_vars, ctx$row_vars[i]))
  s$parallel  <- FALSE
  s$cache_env <- NULL
  s
}

# tab_build_one() -- the per-row_var worker: run the whole aggregate->transform->assemble_tables
# pipeline for a single sliced ctx and return its ONE finished tab (the whole-table test is already
# baked into the tab's `test` attribute). `data` is the prepared population shipped once; reattach it
# here. Top-level (namespaced) so mirai serializes it by reference, carrying no user data.
#' @keywords internal
#' @noRd
tab_build_one <- function(ctx_i, data) {
  ctx_i$data <- data
  ctx_i <- tab_aggregate(ctx_i)
  ctx_i <- tab_transform(ctx_i)
  ctx_i <- tab_assemble_tables(ctx_i)
  ctx_i$tabs[[1]]
}
