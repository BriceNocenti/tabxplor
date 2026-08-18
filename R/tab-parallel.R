# PURPOSE: The row axis of tab_build() as ONE outer map -- serial, or opt-in parallel.
# ROLE: tab_build() prepares the population and the tier-1 aggregates ONCE on the main process,
#   then hands off to tab_build_tables(), which maps the per-row_var worker tab_build_one()
#   (transform |> assemble_tables) through tab_pmap(). tab_pmap() IS purrr::map when serial -- same
#   result, zero overhead -- or a persistent mirai daemon pool when `parallel` is set. Main gathers
#   the finished per-row_var tabs and runs the cross-row_var output shape. A single-row_var build
#   equals its slice of the integrated build, which is what makes the two paths interchangeable.
# KEY CONSTRAINTS:
#   - mirai is Suggests-only: every use is guarded by requireNamespace(), and its absence falls
#     back to serial.
#   - The pool uses a NAMED compute profile ("tabxplor") so it never clobbers a user's own daemons().
#   - A daemon must NEVER spawn daemons. This file is the one place that rule is enforced.
#   - Workers run the INSTALLED tabxplor (a fresh process), so identity requires main and workers to
#     run the same source. Under load_all() the pool auto-load_all()s each freshly spawned daemon.
#   - jmvtab's live cache is ALWAYS serial: its cache hooks live on the serial map.
#   - Errors and messages are caught IN the worker and relayed, because a daemon's console is not
#     the user's. The declared losses are stated at the relay: a relayed error has no backtrace, and
#     the replay stops at the first failing unit.
# See: CLAUDE.md § tabxplor architecture (the calculation pipeline).


# === SECTION: the daemon pool =======================================

# DESIGN: a NAMED mirai compute profile keeps tabxplor's daemons out of the user's default pool.
tabxplor_compute <- "tabxplor"


# DESIGN: the `parallel` argument beats the option; TRUE = auto (physical cores - 1, capped at 8), an
# integer verbatim. The _R_CHECK_LIMIT_CORES_ cap of 2 keeps examples/tests inside CRAN's 2-core rule.
#' @keywords internal
#' @noRd
tab_parallel_workers <- function(parallel = NULL, cache_env = NULL) {
  if (!is.null(cache_env)) return(0L)                       # jmvtab live cache: always serial
  p <- if (is.null(parallel)) tx_option("parallel") else parallel
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


# WARNING: the daemons bind the INSTALLED tabxplor namespace; under load_all() that is the STALE
# installed build (no tab_build_one) and mirai_map() errors -- so detect the dev source here (loaded
# namespace path, plus an R/ check installed libs fail) for tab_pool_ensure() to load_all on each fresh
# daemon. NULL once installed -> zero cost.
#' @keywords internal
#' @noRd
tab_dev_pkg_path <- function() {
  if (!requireNamespace("pkgload", quietly = TRUE)) return(NULL)
  p <- tryCatch(getNamespaceInfo("tabxplor", "path"), error = function(e) NULL)
  if (is.null(p) || !file.exists(file.path(p, "R", "tab-parallel.R"))) return(NULL)
  normalizePath(p)
}


# DESIGN: respawn only when the daemon count differs, so a pre-warmed pool is reused untouched; a
# fresh spawn load_all's the dev source once per pool, not once per tab() call.
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
#' when the package is unloaded; call this to release the workers earlier.
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


# === SECTION: the map ===============================================

# WARNING: this per-unit daemon callback is top-level on purpose -- mirai serializes it BY REFERENCE,
# not by closure, so shipping it to a worker carries NO user data. Conditions AND the error are caught
# here because a daemon's console is not the user's: a worker's cli_inform() would simply be lost, and
# letting mirai's `[.stop]` re-throw drops both the condition's own classes and every message the
# SUCCESSFUL units had produced (it aborts collection before the replay). Both ride back on the payload.
#' @keywords internal
#' @noRd
tab_pmap_trampoline <- function(row, .f_name, .const, .ship_names) {
  f    <- get(.f_name, envir = asNamespace("tabxplor"))
  ship <- mget(.ship_names, envir = .GlobalEnv)
  cnds <- list()
  err  <- NULL
  value <- tryCatch(
    withCallingHandlers(
      do.call(f, c(row, .const, ship)),
      message = function(m) { cnds[[length(cnds) + 1L]] <<- m; invokeRestart("muffleMessage") },
      warning = function(w) { cnds[[length(cnds) + 1L]] <<- w; invokeRestart("muffleWarning") }
    ),
    error = function(e) { err <<- tab_cnd_strip(e); NULL })
  list(value = value, conditions = cnds, error = err)
}


# WARNING: making a condition safe to send home is NOT optional. An rlang error carries a `trace` of
# calls, and a call can hold VALUES rather than symbols -- reg_fit() builds its survey model with
# do.call(..., design = ...), so the error's own call would drag the whole design back across the
# process boundary. cli bullets survive (cli_abort() renders eagerly, tab_pmap() ships the cli/width
# options). Declared loss: a relayed error has no backtrace; the unit's name takes its place.
#' @keywords internal
#' @noRd
tab_cnd_strip <- function(cnd) {
  if (is.null(cnd)) return(NULL)
  cnd$trace <- NULL
  cl <- cnd$call
  if (is.call(cl)) {
    small <- vapply(as.list(cl)[-1L], function(a)
      is.symbol(a) || is.null(a) || (is.atomic(a) && length(a) <= 10L), logical(1))
    if (!all(small)) cnd$call <- as.call(list(cl[[1L]]))
  }
  cnd$parent <- tab_cnd_strip(cnd$parent)
  cnd
}


# Map a namespaced worker over per-unit args, serial OR over a daemon pool.
# IDENTITY CONTRACT: the two branches call `do.call(f, c(row, .const, .ship))` per transposed row, in
# input order, and return the same plain list of values; the worker never branches on execution mode.
# Serial passes the shipped objects as ordinary args; parallel ships them ONCE via everywhere().
#   .l      : per-unit vectors/lists (pmap-style; transposed to per-unit rows here).
#   .f_name : the worker's name in the tabxplor namespace (looked up on both sides).
#   .const  : small shared args -- sent per task (parallel) / passed as constants (serial).
#   .ship   : big shared objects (data, fine_fused) -- shipped ONCE (parallel) / args (serial).
#   .names  : what to CALL each unit when it fails (row_var, tab_vars level, model, outcome).
#   workers : 0/1 -> serial; N -> N daemons. Also serial below tabxplor.parallel_min units.
#' @keywords internal
#' @noRd
tab_pmap <- function(.l, .f_name, .const = list(), .ship = list(), .names = NULL,
                     workers = 0L, compute = tabxplor_compute) {
  f    <- get(.f_name, envir = asNamespace("tabxplor"))
  # Recycle length-1 per-unit args to the common length (pmap does this; transpose() does not).
  rows <- purrr::transpose(vctrs::vec_recycle_common(!!!.l))

  serial <- workers <= 1L ||
    length(rows) < tx_option("parallel_min") ||
    !requireNamespace("mirai", quietly = TRUE)
  nms <- if (is.null(.names)) as.character(seq_along(rows)) else as.character(.names)

  if (serial) {
    # WARNING: both branches must name the failing unit with the SAME sentence, never purrr's `i In
    # index: 2.`; a worker that already named itself is re-thrown untouched.
    return(lapply(seq_along(rows), function(i)
      rlang::try_fetch(do.call(f, c(rows[[i]], .const, .ship)),
                       error = function(cnd) tab_unit_abort(cnd, nms[[i]]))))
  }

  tab_pool_ensure(workers, compute)

  # DESIGN: the options snapshot keeps option-sensitive leaf math identical on daemons; setDTthreads(1L)
  # avoids workers x DT-threads oversubscription (grouped keyby sums are thread-order-invariant, so this
  # does NOT change results). `cli.*` / `crayon.*` / `width` must ride along because a worker's message
  # is RELAYED here and cli renders its text at signal time -- otherwise a daemon would format with its
  # own glyphs and wrap width, and the relayed message would not match the serial one.
  keep <- "^tabxplor\\.|^datatable\\.|^cli\\.|^crayon\\.|^width$|^useFancyQuotes$"
  opts <- options()[grepl(keep, names(options()))]
  # WARNING: THE NESTING RULE, enforced here and only here. The axes NEST (a crosstab's `row_var`s; a
  # regression's `tab_vars` groups x models x outcomes) and only the OUTERMOST dispatches: a daemon must
  # never spawn daemons. The unit-construction sites each pass `parallel = FALSE`, but the option ships
  # too (`^tabxplor\.` matches `tabxplor.parallel`), so a site forwarding NULL would read a TRUE here.
  opts[["tabxplor.parallel"]] <- FALSE
  mirai::everywhere(
    {
      options(tabx_opts)
      data.table::setDTthreads(1L)
      list2env(tabx_ship, envir = .GlobalEnv)
    },
    tabx_opts = opts,
    tabx_ship = .ship,
    .compute  = compute
  )

  # WARNING: `[]`, NOT `[.stop]` -- the trampoline catches its unit's error and returns it, so
  # collection must complete for the replay below to run at all.
  got <- mirai::mirai_map(
    rows, tab_pmap_trampoline,
    .args    = list(.f_name = .f_name, .const = .const, .ship_names = names(.ship)),
    .compute = compute
  )[]

  # Replay the workers' conditions on main, in UNIT order -- the same messages, in the same relative
  # order, whichever branch ran. WARNING: two declared non-identities. They can only be replayed once
  # the map has collected, so they land AFTER anything the caller signalled around tab_pmap() instead
  # of interleaved with it -- the price of another process. And the replay STOPS at the first failing
  # unit: serially the later units never ran, so replaying them would show output serial cannot produce.
  for (i in seq_along(got)) {
    g <- got[[i]]
    # mirai's own failure (dead daemon, unserialisable return): no payload, nothing to say but its text.
    if (mirai::is_error_value(g)) {
      cli::cli_abort(c("Parallel build failed on {.val {nms[[i]]}}.",
                       "x" = as.character(g)), call = NULL)
    }
    for (cnd in g$conditions) rlang::cnd_signal(cnd)
    if (!is.null(g$error)) tab_unit_abort(g$error, nms[[i]])
  }
  purrr::map(got, "value")
}


# "Which unit failed", said the same way in both branches.
# WARNING: it de-duplicates by NAME, not by class -- the axes NEST, so an inner failure rightly gains
# an outer name ("the m1 model, of the `score` outcome"). Only a unit re-naming ITSELF is dropped.
#' @keywords internal
#' @noRd
tab_unit_abort <- function(cnd, nm) {
  if (identical(cnd$tabxplor_unit, nm)) stop(cnd)
  cli::cli_abort("Build failed on {.val {nm}}.", parent = cnd, call = NULL,
                 class = "tabxplor_unit_named", tabxplor_unit = nm)
}


# === SECTION: the per-row_var worker ================================

# Run transform -> assemble_tables for ONE lean ctx, returning its finished tab + whole-table test.
# `data`, `fine_fused` and the survey `design` are the big objects tab_pmap() ships once per worker and
# this reattaches. Top-level (namespaced), so mirai serializes it by reference, carrying no user data.
#' @keywords internal
#' @noRd
tab_build_one <- function(ctx_i, data, fine_fused, design = NULL) {
  # WARNING: ctx_update() assigns with single brackets, so `fine_fused = NULL` (fuse off) SURVIVES as a
  # list element; `ctx_i$fine_fused <- NULL` would delete the key and tab_transform could not find it.
  ctx_i <- ctx_update(ctx_i, list(data = data, fine_fused = fine_fused))
  ctx_i$inference["design"] <- list(design)
  ctx_i <- tab_transform(ctx_i)
  # DESIGN: capture the PRE-merge test for the jmvtab tier-2 store -- assemble bind_rows() the numeric
  # ANOVA into it, so a post-assemble test would double-merge that ANOVA on a later cache hit.
  test  <- ctx_i$tests
  ctx_i <- tab_assemble_tables(ctx_i)
  list(tab = ctx_i$tabs, test = test)
}
