# Intent: silence lifecycle deprecation signals for the whole test run -- the package tests its own
# soft-deprecated surface pervasively (tab_many(), the combined color strings "diff_ci"/"after_ci"/
# "ci", the totrow/totcol arguments), and a flood of them would be captured by expect_snapshot(cat(..))
# in the golden display cases (a spurious snapshot mismatch).
#
# WARNING (measured 2026-07-16): THIS LINE DOES NOT DO THAT. testthat::local_reproducible_output()
# sets `lifecycle_verbosity = "warning"` and runs inside every test_that() block, so it overrides
# whatever we set here -- verified "warning" both at file level and inside a test_that(), with a
# sentinel proving this file itself does run. The option is kept only because it is harmless and
# correct for a plain source() of a test file.
# => The ONLY thing that actually keeps the suite quiet is not calling the deprecated surface: use
#    the current argument (test =, not chi2 =), or wrap the call in suppressWarnings() where the
#    deprecated form IS the thing under test. lifecycle::expect_deprecated() for the explicit ones.
options(lifecycle_verbosity = "quiet")

# ONE data.table thread per testthat worker (Phase 14a).
# WARNING: this is not a micro-optimisation, it is what keeps the suite from thrashing.
# `Config/testthat/parallel: true` runs N test files in N SEPARATE PROCESSES, and data.table
# defaults each of them to ~50% of the machine's cores. Measured here (12 cores, TESTTHAT_CPUS=8):
# 8 workers -> 165 threads, ~14x oversubscribed -- a suite that should take ~1 min ran >26 min,
# with two workers pegged at ~485% CPU while the rest starved. The workers already give us the
# parallelism; each one only needs a single thread. R/tab-parallel.R:177 does exactly this for the
# mirai daemon pool, for exactly this reason.
# Also the CRAN-friendly setting (checks are limited to 2 cores).
if (requireNamespace("data.table", quietly = TRUE)) data.table::setDTthreads(1L)

# Pin the console colour theme (Phase 14g). tabxplor now DETECTS it at load, from the editor: on a dark
# Positron the default becomes "dark", so any test reading the option would compare a dark palette
# against a light expectation -- passing on CI (which has no editor -> "light") and failing on the
# maintainer's machine, or the reverse. That divergence is precisely what the 2026-07-15 CI green-up
# spent a day on. The suite must not depend on where it runs.
options(tabxplor.color_style_theme = "light")

# Pin console bold OFF (Phase 16f) for the same reason: it is IDE-detected at load (ON in Positron / VS
# Code), so on the maintainer's machine every colour test would render bold cells while CI would not. Bold
# is a no-op under testthat's ANSI-off output anyway, but pin it so nothing depends on the front-end.
options(tabxplor.console_bold = FALSE)
