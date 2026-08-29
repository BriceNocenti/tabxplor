# PURPOSE: The row axis of tab_build() as ONE outer map -- serial, or opt-in parallel.
# ROLE: tab_build() prepares the population and the tier-1 aggregates ONCE on the main process,
#   then hands off to tab_build_tables(), which maps the per-row_var worker tab_build_one()
#   (transform |> assemble_tables) through tab_pmap(). tab_pmap() IS purrr::map when serial -- same
#   result, zero overhead -- or a persistent mirai daemon pool when the option is on. Main gathers
#   the finished per-row_var tabs and runs the cross-row_var output shape. A single-row_var build
#   equals its slice of the integrated build, which is what makes the two paths interchangeable.
# KEY CONSTRAINTS:
#   - mirai is Suggests-only: every use is guarded by requireNamespace(), and its absence falls
#     back to serial.
#   - The pool uses a NAMED compute profile ("tabxplor") so it never clobbers a user's own daemons().
#   - `options(tabxplor.parallel =)` is the ONE switch: there is no `parallel =` argument on either
#     producer. That is what makes the next rule enforceable in one place instead of at every
#     unit-construction site.
#   - A daemon must NEVER spawn daemons, and neither may a nested SERIAL unit. tab_pmap() turns the
#     option off around its whole map -- both branches -- and that is the only place the rule lives.
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


# WARNING: `parallel::detectCores()` IS NOT the number of cores this R may use, and the gap is not
# academic -- measured on this box, it returns 12 under `taskset -c 0,1` and 12 under
# `_R_CHECK_LIMIT_CORES_`, where the true answer is 2 both times. Left uncorrected, `parallel = TRUE`
# spawns a full pool onto two allocated cores in every container, HPC job and CI runner, which suite
# C measured as a 25 % LOSS. The cascade below is one rung per case it gets wrong:
#   1. `_R_CHECK_LIMIT_CORES_` -- CRAN's 2-core rule for examples / tests / vignettes.
#   2. `options(mc.cores)`     -- base R's own convention: a user who set it has already answered.
#   3. parallelly::availableCores() -- cgroups v1/v2, affinity masks, SLURM / PBS / SGE / LSF.
#      Suggests-only, and the reason it is not simply required: mirai imports `nanonext` alone and
#      offers no core count of its own, so gating on mirai would not buy this.
#   4. `nproc`                 -- affinity-aware on Unix, and the fallback when (3) is absent.
#   5. detectCores()           -- the last resort, and on Windows usually the right answer anyway.
# DESIGN: only the MACHINE rungs (3-5) are memoised. They cost a subprocess and cannot change in a
# session; rungs 1-2 are options and are re-read every call, so a user can still change their mind.
tab_cores_cache <- new.env(parent = emptyenv())

#' @keywords internal
#' @noRd
tab_available_cores <- function() {
  if (nzchar(Sys.getenv("_R_CHECK_LIMIT_CORES_"))) return(2L)
  mc <- suppressWarnings(as.integer(getOption("mc.cores") %||% NA))
  if (!is.na(mc) && mc >= 1L) return(mc)
  if (!is.null(tab_cores_cache$n)) return(tab_cores_cache$n)

  n <- NA_integer_
  if (requireNamespace("parallelly", quietly = TRUE))
    n <- suppressWarnings(tryCatch(as.integer(parallelly::availableCores()),
                                   error = function(e) NA_integer_))[1L]
  if ((is.na(n) || n < 1L) && identical(.Platform$OS.type, "unix"))
    n <- suppressWarnings(tryCatch(as.integer(system2("nproc", stdout = TRUE, stderr = FALSE)),
                                   error = function(e) NA_integer_, warning = function(e) NA_integer_))[1L]
  if (is.na(n) || n < 1L) n <- parallel::detectCores(logical = FALSE)
  if (is.na(n)) n <- parallel::detectCores()
  if (is.na(n)) n <- 2L
  tab_cores_cache$n <- max(1L, as.integer(n))
  tab_cores_cache$n
}

# THE auto worker count: half the cores this R may actually use, floored at 2 and capped at 4.
# Every clause is measured (the tables x workers grid):
#   - CAP AT 4 because the whole rest of the machine buys little: 8 workers is +38 % over 4 at 24
#     tables and NOTHING below 8 tables, for four more processes and ~530 MB.
#   - HALF the cores because a build must not saturate the machine it runs on. It costs real speed
#     (on 4 cores, 4 workers give x2.8 against x1.7 for 2) and buys a usable UI, which matters most
#     on the machine where it costs most.
#   - FLOOR OF 2 because `%/% 2` gives 1 on a 2-core machine -- and 1 worker is serial, while 2
#     cores is exactly where 2 workers give x1.75 with no penalty. A 2-core box has no headroom to
#     protect, so there is nothing to spend the halving on.
#' @keywords internal
#' @noRd
tab_auto_workers <- function(avail = tab_available_cores()) {
  if (avail <= 1L) return(1L)                # a single core: 1 is serial, and 2 would oversubscribe
  min(4L, max(2L, avail %/% 2L))
}


# DESIGN: `options(tabxplor.parallel =)` is the ONE switch -- there is no argument, so a nested build
# cannot forget to pass FALSE (tab_pmap() turns the option off around its map). It stays OPT-IN:
# FALSE by default, because the pool spawn (0.9-2.0 s, blocking -- mirai::daemons() returns only once
# the daemons have connected) makes the FIRST parallel table slower than serial, always. `TRUE` /
# `"auto"` = tab_auto_workers(), an integer verbatim. The _R_CHECK_LIMIT_CORES_ cap of 2 keeps
# examples/tests inside CRAN's 2-core rule even when a user asked for more.
#' @keywords internal
#' @noRd
tab_parallel_workers <- function(cache_env = NULL) {
  if (!is.null(cache_env)) return(0L)                       # jmvtab live cache: always serial
  p <- tx_option("parallel")
  if (is.null(p) || isFALSE(p) || identical(p, "no")) return(0L)
  if (!requireNamespace("mirai", quietly = TRUE)) {
    tx_need_pkg("mirai", "options(tabxplor.parallel = TRUE)", severity = "inform")
    return(0L)
  }
  cap <- if (nzchar(Sys.getenv("_R_CHECK_LIMIT_CORES_"))) 2L else Inf
  if (isTRUE(p) || identical(p, "auto")) {
    n <- tab_auto_workers()
  } else {
    n <- suppressWarnings(as.integer(p))
    if (is.na(n) || n < 1L) return(0L)
  }
  as.integer(min(n, cap))
}


# WARNING: THE BYTE-IDENTITY CONTRACT NEEDS THIS, and it is not obvious. A daemon must pin its BLAS
# (see the everywhere() block below) or a glm-bound map thrashes -- but pinning only the WORKERS
# makes them disagree with a main process whose BLAS is still multi-threaded, in the last bits of
# every coefficient. Measured (2 outcomes, an OpenBLAS-pthread build at 12 threads):
#   main 12 / workers 1 -> parallel != serial     main 1 / workers 1 -> parallel == serial
# and, the fact that explains it, a SERIAL build at 1 vs 12 BLAS threads already differed. So the
# thread count is part of the answer, and the two branches must agree on it. Pinning to 1 for the
# duration of a build is what makes them agree -- and it makes a result reproducible across machines
# with different BLAS builds, which the test suite already assumes (setup.R pins it there).
# DESIGN: restore on exit, never a global set: a package must not silently reconfigure a user's BLAS.
#' @keywords internal
#' @noRd
local_blas_threads <- function(n = 1L, frame = parent.frame()) {
  if (!requireNamespace("RhpcBLASctl", quietly = TRUE)) return(invisible(NULL))
  old <- tryCatch(RhpcBLASctl::blas_get_num_procs(), error = function(e) NULL)
  if (is.null(old) || is.na(old)) return(invisible(NULL))
  try(RhpcBLASctl::blas_set_num_threads(n), silent = TRUE)
  # base on.exit in the CALLER's frame -- withr is Suggests-only, so it cannot be used here, and the
  # old count is baked into the expression by value rather than looked up when the frame unwinds.
  do.call(base::on.exit,
          list(bquote(try(RhpcBLASctl::blas_set_num_threads(.(old)), silent = TRUE)),
               add = TRUE, after = FALSE),
          envir = frame)
  invisible(old)
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
#' Shuts down the persistent \pkg{mirai} daemons tabxplor starts under
#' \code{options(tabxplor.parallel = )}. The pool is otherwise reused for the whole session and
#' cleaned up when the package is unloaded; call this to release the workers earlier.
#'
#' @return \code{invisible(NULL)}, called for its side effect.
#' @seealso \link{tabxplor-options} for \code{tabxplor.parallel}, the switch that starts the pool.
#' @export
#' @examples
#' \donttest{
#' # after options(tabxplor.parallel = TRUE)
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

  # WARNING: THE NESTING RULE, enforced here and only here -- for BOTH branches. The axes NEST (a
  # crosstab's `row_var`s; a regression's `tab_vars` groups x models x outcomes) and only the
  # OUTERMOST dispatches. There is no `parallel` argument for a unit-construction site to zero, so
  # the option IS the switch: set it off for the whole map, and every unit -- run here or shipped to
  # a daemon (the `^tabxplor\.` snapshot below carries it) -- reads FALSE.
  old_par <- options(tabxplor.parallel = FALSE)
  on.exit(options(old_par), add = TRUE)
  # BOTH branches, so the two agree bit for bit -- see local_blas_threads().
  local_blas_threads(1L)

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
  #
  # WARNING: A WORKER MUST PIN ITS BLAS TOO, and for the same reason -- pinning data.table alone is
  # only half the rule. A daemon is a fresh R process, so a threaded BLAS (Debian/Ubuntu's default
  # OpenBLAS-pthread) opens one thread PER CORE the first time a worker calls glm(): W workers x
  # C cores of spinning threads on C cores. `tab()`'s units are data.table-bound and never noticed;
  # `tab_reg()`'s units are glm-bound, and the contention is catastrophic rather than marginal.
  # Measured (3 outcomes x 3 workers, 12 cores): serial 0.81 s, parallel 56.91 s
  # UNPINNED, parallel 0.29 s pinned. RhpcBLASctl's RUNTIME call is the only lever that works on an
  # already-running worker -- OpenBLAS-pthread fixes its count from the environment at process
  # start, so setting OMP_NUM_THREADS here would be too late. Suggests-only, hence the guard: a
  # worker without it is no worse than before this line existed.
  keep <- "^tabxplor\\.|^datatable\\.|^cli\\.|^crayon\\.|^width$|^useFancyQuotes$"
  opts <- options()[grepl(keep, names(options()))]
  mirai::everywhere(
    {
      options(tabx_opts)
      data.table::setDTthreads(1L)
      if (requireNamespace("RhpcBLASctl", quietly = TRUE)) {
        try(RhpcBLASctl::blas_set_num_threads(1L), silent = TRUE)
        try(RhpcBLASctl::omp_set_num_threads(1L),  silent = TRUE)
      }
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
