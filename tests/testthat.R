library(testthat)
library(tabxplor)

# Pin BLAS/OpenMP BEFORE the parallel workers spawn: they inherit this env at startup, which is
# when the OpenBLAS-pthread build fixes its thread count. Without it, each worker multi-threads
# on its own (~50% of cores) and 8 workers oversubscribe the machine ~14x -- a ~50 s suite runs
# >26 min. setup.R re-pins at runtime (RhpcBLASctl) as belt-and-braces; see CLAUDE.md § Testing.
if (Sys.getenv("OMP_NUM_THREADS") == "") Sys.setenv(OMP_NUM_THREADS = "1")

# Worker count: respect an explicit TESTTHAT_CPUS; otherwise use the cores available, but ONLY
# off-CRAN (on CRAN both guards fail -> testthat's default 2 workers x 1 thread = policy-safe).
if (Sys.getenv("TESTTHAT_CPUS") == "" &&
    identical(Sys.getenv("NOT_CRAN"), "true") &&
    Sys.getenv("_R_CHECK_LIMIT_CORES_") == "") {
  Sys.setenv(TESTTHAT_CPUS = max(1L, min(8L, parallel::detectCores() - 2L)))
}

test_check("tabxplor")
