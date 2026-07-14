# PURPOSE: Byte-identity guardrail for the Phase 5 color/breaks refactor. Locks the CURRENT
#          rendered per-cell colors (fmt_get_color_code hex) across measure x type x channel x
#          theme x 24-bit, so the findInterval-engine rewrite reproduces them exactly unless a
#          change is deliberately accepted (and the fixtures regenerated).
# ROLE: The color safety net (there was none before Phase 5 -- expect_color() only checked
#        ">=1 colored cell"). Every exporter + the console derive color from the same selection
#        path fmt_get_color_code captures, so this locks them all at once.
# KEY CONSTRAINTS:
#   - Fixtures live in tests/testthat/_color_golden/*.rds, produced by dev/make_color_golden.R.
#   - A FAILURE means the rendered color of some cell changed. If unintended -> regression. If
#     intended (see the Phase 5 ledger below) -> rerun dev/make_color_golden.R, review the
#     git diff, accept consciously.
# See: helper-color-golden.R (shared case matrix) and CLAUDE.md golden regeneration protocol.
#
# ===========================================================================================
# PHASE 5 COLOR TRIPWIRE LEDGER -- which step regenerates which fixture, and why.
# The findInterval engine (Step 3) was verified per render-combo against the Step-0 baseline
# (dev/ compare script). TEXT-channel results for the locked factor modes are byte-identical:
#   c_syn_diff / c_diff / c_or  -> TEXT byte-identical (fold+findInterval reproduces factor diff,
#                       the x2, contrib, OR and the 24-bit palettes exactly). Their `bg/*` combos
#                       are regenerated: fmt_get_color_code(type="bg") now uses the bg slot table
#                       (channel-explicit) instead of the text table the old hex-sniff picked when
#                       the default option palette was "text" -- a capture artifact + the bg_dark
#                       "#000033e" typo fix. Real bg usage (option = bg) is unchanged for bg_light.
#   c_diff_ci        -> TEXT byte-identical on this fixture (no CI boundary cell flips); bg regen.
#   c_contrib        -> TEXT changes ONLY on the chi2 p-value row: its ctr=0/mean_ctr=0 gave 0/0,
#                       which the OLD formula matched in BOTH directions (spurious color + a
#                       length-9 vector for an 8-cell column). New engine drops it (NaN->uncolored).
#                       Data-cell coloring is byte-identical. bg regen.
#   c_after_ci / c_ci-> REGENERATE. pct CI-gated: guaranteed_effect now grades the CI-floor against
#                       pct_diff breaks (fixes the asymmetric upper-arm + the odd-length negative-
#                       direction bug that made c_ci ERROR on the baseline).
#   c_contrib_all / c_contrib_all_notab
#                    -> NEW (Phase 10j-B). Lock contrib + comp = "all" colouring, previously
#                       uncovered and crashing (get_mean_contrib returned length 0 without a total
#                       table). Generated fresh after the grand_totrow() fix -- not a regeneration.
#   c_mean_diff / c_mean_diff_ci / c_mean_after_ci
#                    -> REGENERATE. Numeric diff family now colors the sd-standardized difference
#                       (Glass's delta, mean_diff breaks) instead of the ratio overload.
# ===========================================================================================

cases <- color_golden_cases()

testthat::test_that("all color-golden fixtures are present (else run dev/make_color_golden.R)", {
  testthat::skip_if_not(dir.exists(color_golden_dir()),
                        "no _color_golden fixtures yet - run: Rscript dev/make_color_golden.R")
  files   <- file.path(color_golden_dir(), paste0(names(cases), ".rds"))
  missing <- names(cases)[!file.exists(files)]
  testthat::expect_identical(
    missing, character(0),
    info = paste("missing color goldens (run dev/make_color_golden.R):",
                 paste(missing, collapse = ", "))
  )
})

# Rendered-color equality: one test per case.
for (nm in names(cases)) {
  local({
    name <- nm
    testthat::test_that(paste0("color golden unchanged: ", name), {
      fx <- file.path(color_golden_dir(), paste0(name, ".rds"))
      testthat::skip_if_not(file.exists(fx),
                            paste0("no color golden for '", name, "' - run dev/make_color_golden.R"))
      testthat::expect_equal(cases[[name]](), readRDS(fx))
    })
  })
}
