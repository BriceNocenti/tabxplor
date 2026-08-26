library(testthat)
library(tabxplor)

# Pin BLAS/OpenMP BEFORE the workers spawn: an OpenBLAS-pthread build fixes its thread count from
# the environment at process startup. Without it each worker multi-threads on its own and the pool
# oversubscribes the machine several-fold. setup.R re-pins at runtime; see CLAUDE.md § Testing.
if (Sys.getenv("OMP_NUM_THREADS") == "") Sys.setenv(OMP_NUM_THREADS = "1")

# PHYSICAL cores, not SMT siblings. `parallel::detectCores()` reports 12 on this 6-core box and
# `parallelly::availableCores(logical = FALSE)` cannot tell either under WSL2, so read the kernel's
# own topology: one entry per core, shared by its siblings. Measured here, 8 workers on 6 real
# cores buy 6 % over 6 workers while oversubscribing a shared machine.
tx_physical_cores <- function() {
  sib <- Sys.glob("/sys/devices/system/cpu/cpu[0-9]*/topology/thread_siblings_list")
  if (length(sib)) {
    ids <- vapply(sib, function(f) tryCatch(readLines(f, warn = FALSE)[[1]],
                                            error = function(e) NA_character_), character(1))
    ids <- unique(ids[!is.na(ids)])
    if (length(ids)) return(length(ids))
  }
  max(1L, parallel::detectCores() %/% 2L)
}

# Respect an explicit TESTTHAT_CPUS; otherwise use the physical cores, but ONLY off-CRAN (on CRAN
# both guards fail -> testthat's default 2 workers x 1 thread = policy-safe).
if (Sys.getenv("TESTTHAT_CPUS") == "" &&
    identical(Sys.getenv("NOT_CRAN"), "true") &&
    Sys.getenv("_R_CHECK_LIMIT_CORES_") == "") {
  Sys.setenv(TESTTHAT_CPUS = max(1L, min(8L, tx_physical_cores())))
}

test_check("tabxplor")
