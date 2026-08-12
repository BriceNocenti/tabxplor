# PURPOSE: The row axis of tab_build() as ONE outer map, serial OR opt-in parallel (Phase 8: ~3x on the
#   survey workflow; Phase 9a: made the SOLE dispatch, serial included).
# ROLE: tab_build() prepares the population + tier-1 aggregates ONCE on main (tab_setup +
#   tab_prepare_pop -- where the global na="drop_all"/"common_base" drop lives, so it CANNOT move to a
#   worker -- then tab_aggregate: the numeric moment aggregates + the shared factor fine_fused). It then
#   hands off to tab_build_tables() (R/tab.R), which resolves one lean ctx per row_var (tab_rowvar_ctxs)
#   and maps the whole-per-row_var worker tab_build_one() -- transform |> assemble_tables -- over it via
#   tab_pmap(). tab_pmap() IS purrr::map when serial (byte-identical, zero overhead) or a persistent
#   mirai daemon pool when `parallel` is set. Main gathers the finished per-row_var tabs and runs the
#   cross-row_var output shape (tab_assemble_output: merge/compact, p-value lines, unwrap). Byte-identical
#   because a single-row_var build equals its slice of the integrated build (the tab_assemble total-col
#   decoupling fix), verified for every na mode -- so per-row_var == all-at-once.
# KEY CONSTRAINTS:
#   - mirai is Suggests-only: every use is guarded by requireNamespace(); absent -> serial fallback.
#   - Uses a NAMED compute profile ("tabxplor") so it never clobbers a user's own daemons() pool.
#   - Workers run the INSTALLED tabxplor (a fresh process); byte-identity requires main + workers to
#     run the same source -- automatic once installed (R CMD check installs first). In dev (load_all)
#     tab_pool_ensure() now auto-load_all's the current source on each freshly spawned daemon (via
#     tab_dev_pkg_path()), so tab(parallel=) just works under load_all -- no manual pre-warm needed.
#   - jmvtab (live cache) is ALWAYS serial: tab_parallel_workers() returns 0 when ctx$cache_env is set
#     -> the serial map keeps its cache hooks (jmv_cache_aggregate in tab_aggregate; jmv_cache_store_tests
#     in tab_build_tables).
# See: CLAUDE.md 2.0.0 roadmap Phase 8/9a + dev/tabxplor_2.0.0_decisions.md 26, 29.

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


# tab_dev_pkg_path() -- the dev SOURCE path when tabxplor is loaded via devtools/pkgload, else NULL.
# WARNING: the daemons bind the INSTALLED tabxplor namespace (a fresh process). Under load_all that
# namespace is the STALE installed build (no tab_build_one) -> mirai_map() errors. Detect dev via the
# loaded namespace path (wd-independent) + an R/ source check that installed libs fail (they ship no R/
# sources), so tab_pool_ensure() can load_all the current source on the daemons. NULL once installed /
# when pkgload is absent -> zero cost, unchanged behaviour.
#' @keywords internal
#' @noRd
tab_dev_pkg_path <- function() {
  if (!requireNamespace("pkgload", quietly = TRUE)) return(NULL)
  p <- tryCatch(getNamespaceInfo("tabxplor", "path"), error = function(e) NULL)
  if (is.null(p) || !file.exists(file.path(p, "R", "tab-parallel.R"))) return(NULL)
  normalizePath(p)
}


# tab_pool_ensure() -- lazily warm the named daemon pool once, reuse it across calls.
# Only (re)spawns when the current daemon count differs from `workers`, so a pre-warmed pool (e.g. the
# parity test's load_all'd daemons) is reused untouched. On a FRESH spawn in dev, it load_all's the dev
# source on the daemons (once per pool, not per tab() call) so tab(parallel=) works under load_all; inert
# once installed (tab_dev_pkg_path() -> NULL).
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
    dev <- tab_dev_pkg_path()
    if (!is.null(dev)) {
      mirai::everywhere(
        { suppressMessages(pkgload::load_all(dev, quiet = TRUE)) },
        dev = dev, .compute = compute
      )
    }
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
  # Recycle length-1 per-unit args to the common length (pmap does this; transpose() does not).
  rows <- purrr::transpose(vctrs::vec_recycle_common(!!!.l))

  serial <- workers <= 1L ||
    length(rows) < getOption("tabxplor.parallel_min", 2L) ||
    !requireNamespace("mirai", quietly = TRUE)

  if (serial) {
    return(purrr::map(rows, ~ do.call(f, c(.x, .const, .ship))))
  }

  tab_pool_ensure(workers, compute)

  # Ship the big objects + a tabxplor/data.table options snapshot + single-thread DT, ONCE. The
  # options snapshot keeps option-sensitive leaf math (e.g. the numeric-CI effective n) identical on
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


# tab_build_one() -- the per-row_var worker (Phase 9a): run the whole transform -> assemble_tables
# pipeline for ONE lean ctx (from tab_rowvar_ctxs()) and return its single finished tab + whole-table
# test. `data` and the shared aggregate `fine_fused` are the big objects shipped once by tab_pmap();
# reattach them here (a bare do.call() arg cannot carry them into the lean unit). The per-row_var
# numeric moment aggregate rides in ctx_i$fine_num; the tier-1 build (tab_aggregate) already ran once
# on main. Top-level (namespaced) so mirai serializes it by reference, carrying no user data. The
# cross-row_var output shape (merge/pvalue/unwrap) runs on main in tab_assemble_output().
#' @keywords internal
#' @noRd
tab_build_one <- function(ctx_i, data, fine_fused, design = NULL) {
  # ctx_update() (single-bracket [<-) so fine_fused = NULL (the default, fuse off) is PRESERVED as a
  # list element -- `ctx_i$fine_fused <- NULL` would DELETE the key and tab_transform's list2env() then
  # can't find `fine_fused`. Last Phase z14-i: the survey DESIGN is shipped the same way (once per
  # worker, not once per row_var -- a prebuilt design carries the whole dataset); z16-iiiii puts it
  # back into the one `inference` object tab_rowvar_ctxs() emptied it out of.
  ctx_i <- ctx_update(ctx_i, list(data = data, fine_fused = fine_fused))
  ctx_i$inference["design"] <- list(design)
  ctx_i <- tab_transform(ctx_i)
  # Capture the PRE-merge test (the factor chi2 tibble, or the chi2 logical on a numeric-only table)
  # for the jmvtab tier-2 store: assemble then bind_rows the numeric ANOVA into it, so returning the
  # post-assemble test would double-merge the ANOVA on a later cache hit (the store feeds tab_apply_tests
  # -> set_test, then assemble merges chi2_num again). jmv_cache_store_tests only keeps data.frames.
  test  <- ctx_i$tests
  ctx_i <- tab_assemble_tables(ctx_i)
  list(tab = ctx_i$tabs, test = test)
}
