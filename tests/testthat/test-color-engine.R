# PURPOSE: Edge-case coverage for the findInterval colour engine (fmt_color_plan /
#          fmt_color_slots / fmt_color_channels), Phase 13a. Asserts the resulting per-cell SLOT
#          INTEGERS, not just "some colour".
# ROLE: Locks the engine on degenerate inputs and exact ties, independently of the golden capture.
#
# Slot domain (Phase 13a): 0 = uncoloured; 1:4 = over-represented intensities; 5:8 = under. The
# per-side breaks + slots come from the scale (mk_color_scale precomputes over$slots / under$slots
# via intensity_slots()); the engine folds each cell to a magnitude and findInterval()s per side.

testthat::test_that("intensity_slots: fewer than 4 breaks drop the 2nd, then 4th, then 1st", {
  testthat::expect_equal(intensity_slots(4), c(1L, 2L, 3L, 4L))
  testthat::expect_equal(intensity_slots(3), c(1L, 3L, 4L))
  testthat::expect_equal(intensity_slots(2), c(1L, 3L))
  testthat::expect_equal(intensity_slots(1), 3L)
  testthat::expect_length(intensity_slots(0), 0L)
  testthat::expect_error(intensity_slots(5), "most 4")
})

testthat::test_that("engine: factor-diff ties (strict >) and the over-slot mapping", {
  col  <- color_golden_syn_diff_fmt()
  plan <- fmt_color_plan(col, "text")
  os   <- plan$over_slots                            # c(0, 1, 2, 3, 4) for the 4 default pct_diff breaks
  slot <- fmt_color_slots(col, plan)

  testthat::expect_equal(slot[1],  0L)               # diff 0 -> neutral
  testthat::expect_equal(slot[3],  0L)               # diff EXACTLY 0.05 -> strict `>` -> lower band (0)
  testthat::expect_equal(slot[5],  os[2])            # diff EXACTLY 0.10 -> level 1
  testthat::expect_equal(slot[9],  os[4])            # diff EXACTLY 0.30 -> level 3
  testthat::expect_equal(slot[10], os[5])            # diff 0.40 -> level 4 (top)
  testthat::expect_equal(slot[20], 0L)               # total row -> neutral
  # the old in-text x2 override is gone: a mid-diff over-cell colours by its diff (an over slot 1:4)
  testthat::expect_true(slot[18] %in% os[-1])
})

testthat::test_that("engine: all-NA and cell==reference give slot 0 (uncolored)", {
  na_col <- fmt(n = c(1L, 1L, 1L), type = "row", diff = NA_real_, mean = NA_real_,
                ratio = NA_real_, color = "diff")
  testthat::expect_equal(fmt_color_slots(na_col, fmt_color_plan(na_col, "text")), rep(0L, 3))

  zero <- fmt(n = c(1L, 1L), type = "row", pct = c(0.4, 0.4), diff = c(0, 0.15),
              mean = c(1, 1), ratio = c(1, 1), color = "diff",
              in_totrow = c(FALSE, TRUE), ref = "tot", comp_all = FALSE)
  testthat::expect_equal(fmt_color_slots(zero, fmt_color_plan(zero, "text"))[1], 0L)  # diff == 0
})

testthat::test_that("engine: numeric diff = Glass's delta; sd_ref 0/NA -> uncolored", {
  # ref (total) var = 4 -> sd_ref = 2 ; Glass = diff/sd_ref = 2/2 = 1.0 -> |1.0| > 0.8 -> level 3
  col  <- fmt(n = c(10L, 10L), type = "mean", mean = c(5, 3), diff = c(2, 0), var = c(4, 4),
              color = "diff", in_totrow = c(FALSE, TRUE), ref = "tot", comp_all = FALSE)
  plan <- fmt_color_plan(col, "text")
  os   <- plan$over_slots                            # c(0, 1, 3, 4) for the 3 default mean_diff breaks
  testthat::expect_equal(fmt_color_slots(col, plan)[1], os[4])   # level 3 -> intensity 4

  bad <- fmt(n = c(10L, 10L), type = "mean", mean = c(5, 3), diff = c(2, 0), var = c(0, 0),
             color = "diff", in_totrow = c(FALSE, TRUE), ref = "tot", comp_all = FALSE)
  testthat::expect_equal(fmt_color_slots(bad, fmt_color_plan(bad, "text"))[1], 0L)  # sd_ref 0
})

testthat::test_that("engine: ratio with ref 0 -> Inf/NaN -> uncolored (no crash)", {
  col <- fmt(n = c(10L, 10L), type = "row", pct = c(0.5, 0), ratio = c(Inf, 1),
             mean = c(Inf, 1), color = "ratio", in_totrow = c(FALSE, TRUE),
             ref = "tot", comp_all = FALSE)
  testthat::expect_equal(fmt_color_slots(col, fmt_color_plan(col, "text"))[1], 0L)
})

testthat::test_that("engine: no color mode -> NULL plan -> all slot 0", {
  col <- fmt(n = c(1L, 2L, 3L), type = "row", pct = c(0.1, 0.2, 0.7), color = "")
  testthat::expect_null(fmt_color_plan(col, "text"))
  testthat::expect_equal(fmt_color_slots(col, NULL), rep(0L, 3))
})

# --- guaranteed_effect on the RATIO channel uses a GUARANTEED RATIO, not the raw diff bound.
# The branch must NOT feed a DIFFERENCE bound (centre 0) into the ratio fold (centre 1): a bound of
# ~0.05 would fold to 1/0.05 -> strongest UNDER colour on every significant cell regardless of dir.
testthat::test_that("guaranteed_effect ratio channel colours the guaranteed RATIO", {
  # symmetric pct_ratio so BOTH directions colour (the over-only default has no under side)
  set_color_breaks(pct_ratio = c(1.5, 2, 4)); withr::defer(options("tabxplor.color_breaks" = default_color_scales()))
  p_ref  <- c(0.2, 0.6, 0.7, 0.5)
  pct    <- c(0.6, 0.66, 0.1, 0.52)
  diff   <- pct - p_ref
  ratio  <- pct / p_ref
  ci_inf <- c(0.30, 0.01, -0.70, -0.05)   # cell1/2 sig over, cell3 sig under, cell4 spans 0
  ci_sup <- c(0.50, 0.11, -0.50,  0.09)
  col <- fmt(n = rep(100L, 4), type = "row", pct = pct, diff = diff, ratio = ratio,
             ci_inf = ci_inf, ci_sup = ci_sup, ci_type = "diff")
  col <- set_color(col, c("diff", "ratio"))
  col <- set_color_signif(col, "guaranteed_effect")

  plan      <- fmt_color_plan(col, "bg", color = "ratio")
  guar_diff <- dplyr::case_when(ci_inf > 0 ~ ci_inf, ci_sup < 0 ~ ci_sup, TRUE ~ NA_real_)
  expected  <- 1 + (ratio - 1) * (guar_diff / diff)          # centre-1 guaranteed ratio
  testthat::expect_equal(plan$score, expected)
  testthat::expect_gt(plan$score[1], 1)                      # over-rep guaranteed ratio > 1
  testthat::expect_lt(plan$score[3], 1)                      # under-rep guaranteed ratio < 1

  slot <- fmt_color_slots(col, plan)
  testthat::expect_true(slot[1] >= 1L && slot[1] <= 4L)      # over -> an over slot (1:4), NEVER under
  testthat::expect_true(slot[3] >= 5L)                       # under -> an under slot (5:8)
  testthat::expect_equal(slot[4], 0L)                        # not significant -> uncoloured
})

testthat::test_that("grey_non_signif ratio channel still colours the OBSERVED ratio", {
  set_color_breaks(pct_ratio = c(1.5, 2, 4)); withr::defer(options("tabxplor.color_breaks" = default_color_scales()))
  p_ref  <- c(0.2, 0.5); pct <- c(0.6, 0.52)
  diff   <- pct - p_ref; ratio <- pct / p_ref
  ci_inf <- c(0.30, -0.05); ci_sup <- c(0.50, 0.09)          # cell1 sig over, cell2 not sig
  col <- fmt(n = rep(100L, 2), type = "row", pct = pct, diff = diff, ratio = ratio,
             ci_inf = ci_inf, ci_sup = ci_sup, ci_type = "diff")
  col <- set_color(col, c("diff", "ratio"))
  col <- set_color_signif(col, "grey_non_signif")
  plan <- fmt_color_plan(col, "bg", color = "ratio")
  testthat::expect_equal(plan$score, ratio)                  # observed ratio, not a floor
  slot <- fmt_color_slots(col, plan)
  testthat::expect_true(slot[1] >= 1L && slot[1] <= 4L)      # ratio 3 (>=2) significant -> over colour
  testthat::expect_equal(slot[2], 0L)                        # not significant -> greyed
})
