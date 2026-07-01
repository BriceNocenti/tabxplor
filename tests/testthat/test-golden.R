# PURPOSE: Characterization guardrail. Locks the CURRENT output of tab()/tab_num()/tab_many()
#          across an argument matrix, so ambitious internal refactors reproduce it exactly
#          unless a change is deliberately accepted (and the fixtures regenerated).
# ROLE: Core retro-compatibility net for tabxplor 1.4.0.
# KEY CONSTRAINTS:
#   - Structural fixtures live in tests/testthat/_golden/*.rds, produced by dev/make_golden.R.
#   - Display snapshots (tab_md) live in tests/testthat/_snaps/golden.md.
#   - A FAILURE here means output changed. If unintended -> a regression to fix. If intended ->
#     rerun dev/make_golden.R + testthat::snapshot_accept("golden"), reviewing the git diff.
# See: helper-golden.R (the shared case matrix) and CLAUDE.md golden regeneration protocol.

cases <- golden_cases()

testthat::test_that("all golden fixtures are present (else run dev/make_golden.R)", {
  testthat::skip_if_not(dir.exists(golden_dir()),
                        "no _golden fixtures yet - run: Rscript dev/make_golden.R")
  files   <- file.path(golden_dir(), paste0(names(cases), ".rds"))
  missing <- names(cases)[!file.exists(files)]
  testthat::expect_identical(
    missing, character(0),
    info = paste("missing goldens (run dev/make_golden.R):", paste(missing, collapse = ", "))
  )
})

# Structural equality: one test per case.
for (nm in names(cases)) {
  local({
    name <- nm
    testthat::test_that(paste0("golden structure unchanged: ", name), {
      fx <- file.path(golden_dir(), paste0(name, ".rds"))
      testthat::skip_if_not(file.exists(fx),
                            paste0("no golden for '", name, "' - run dev/make_golden.R"))
      testthat::expect_equal(cases[[name]](), readRDS(fx))
    })
  })
}

# Display parity: user-facing markdown output for a representative subset.
for (nm in golden_display_cases) {
  local({
    name <- nm
    testthat::test_that(paste0("golden display (tab_md) unchanged: ", name), {
      testthat::expect_snapshot(cat(tab_md(cases[[name]](), print = FALSE)))
    })
  })
}

# ref_n motivation (workstream 1). The two col_vars of the synthetic frame have DIFFERENT
# non-NA totals, so with na = "drop" the shared total/reference count today comes from the
# LAST col_var only -- an approximation. When the `ref_n` field lands and makes cross-col_var
# reference counts exact, the `refn_keep`/`refn_drop` goldens above WILL change: regenerate and
# accept them consciously. This test just pins the root-cause asymmetry.
testthat::test_that("ref_n motivation: col_vars have differing non-NA totals", {
  syn <- golden_syn_df()
  testthat::expect_false(sum(!is.na(syn$h)) == sum(!is.na(syn$k)))
})
