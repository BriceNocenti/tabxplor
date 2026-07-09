# Silence lifecycle deprecation signals for the whole test run. The package tests its own
# soft-deprecated surface pervasively (tab_many(), the combined color strings "diff_ci"/
# "after_ci"/"ci", the totrow/totcol arguments). Under R CMD check the test code is an "external"
# caller, so deprecate_soft() warns -- flooding the check and, worse, being captured by
# expect_snapshot(cat(...)) in the golden display cases (a spurious snapshot mismatch).
# lifecycle::expect_deprecated() forces warnings locally, so the explicit deprecation tests
# keep working.
options(lifecycle_verbosity = "quiet")
