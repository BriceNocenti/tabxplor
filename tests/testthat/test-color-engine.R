# PURPOSE: Edge-case coverage for the Phase 5 findInterval color engine (fmt_color_plan /
#          fmt_color_slots / fmt_color_channels). Written spec-first (brief §13.1) so the engine
#          is coded against a spec. Asserts the resulting per-cell SLOT INTEGERS, not just
#          "some color". Filled in during Step 3 (engine) / Step 4 (two channels).
# ROLE: Locks the engine's behaviour on degenerate inputs and exact ties independently of the
#        golden capture (which uses realistic values).
#
# SPEC TO COVER (fill at Step 3/4):
#   Engine edge cases -> assert the integer slot vector:
#     - all-NA column -> all slot 0 (uncolored);
#     - single-row / single-column table;
#     - cell equal to its reference (diff == 0, ratio == 1) -> slot 0 (neutral);
#     - empty cells (n == 0);
#     - NA CI bounds -> never significant -> grey_non_signif greys, color_all_signif drops;
#     - ratio with ref_pct == 0 / ref_mean == 0 -> Inf/NaN -> slot 0, no crash;
#     - standardized diff with sd_ref == 0 or NA (Glass delta undefined) -> slot 0;
#     - a length-1 break vector (single shade) and the max length;
#     - the significance boundary (ci_inf exactly 0);
#     - findInterval at exact break values (tie side): the fold + left.open=strict reproduces
#       today's strict `>`/`<` (on-break cell stays in the LOWER band) and contrib's inclusive
#       `>=`/`<=` -- the c_syn_diff golden is the factor-diff reference.
#   Two channels (Step 4):
#     - color = c("diff","ratio") -> text_slot from diff, bg_slot from ratio;
#     - color = c(background="ratio") -> text_slot all 0, bg_slot from ratio;
#     - diff + ratio share ONE cell-vs-ref significance boolean.

# --- Step 2: the channel-explicit level -> palette-slot rule (documented expected vectors) ---
testthat::test_that("color_slot_table / build_slots follow the documented slot rule", {
  withr::defer(set_color_style(type = "text", theme = "light"))
  options("tabxplor.color_style" = NULL)
  set_color_style(type = "text", theme = "light")   # default palette -> text (ELSE) branch

  # the documented rule (explicit expected vectors)
  testthat::expect_equal(color_slot_table(8L, "text"), c(2L, 3L, 4L, 5L, 7L, 8L, 9L, 10L))
  testthat::expect_equal(color_slot_table(8L, "bg"),   c(1L, 2L, 3L, 4L, 6L, 7L, 8L, 10L))
  testthat::expect_equal(color_slot_table(0L, "text"), integer(0))

  # build_slots splits into per-direction maps with a leading neutral (uncolored) 0
  bs <- build_slots(4L, "text")
  testthat::expect_equal(bs$pos_slots, c(0L, 2L, 3L, 4L, 5L))
  testthat::expect_equal(bs$neg_slots, c(0L, 7L, 8L, 9L, 10L))
  testthat::expect_equal(build_slots(0L, "text"), list(pos_slots = 0L, neg_slots = 0L))
})

# --- Step 3: the findInterval engine, per-cell slot integers ---

testthat::test_that("engine: factor-diff ties (strict >), x2 override, top break beats x2", {
  col  <- color_golden_syn_diff_fmt()
  slot <- fmt_color_slots(col, fmt_color_plan(col, "text"))
  ps   <- build_slots(4L, "text")$pos_slots       # c(0, 2, 3, 4, 5)

  testthat::expect_equal(slot[1],  0L)             # diff 0 -> neutral
  testthat::expect_equal(slot[3],  0L)             # diff EXACTLY 0.05 -> strict `>` -> lower band (0)
  testthat::expect_equal(slot[5],  ps[2])          # diff EXACTLY 0.10 -> level 1 (stays in 0.05 band)
  testthat::expect_equal(slot[9],  ps[4])          # diff EXACTLY 0.30 -> level 3 (stays in 0.20 band)
  testthat::expect_equal(slot[10], ps[5])          # diff 0.40 -> level 4 (top)
  testthat::expect_equal(slot[18], 11L)            # diff 0.12 + ratio 2.5 -> x2 wins (slot 11)
  testthat::expect_equal(slot[19], ps[5])          # diff 0.35 (> top break) beats the x2 -> pos5
  testthat::expect_equal(slot[20], 0L)             # total row -> neutral
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
  col <- fmt(n = c(10L, 10L), type = "mean", mean = c(5, 3), diff = c(2, 0), var = c(4, 4),
             color = "diff", in_totrow = c(FALSE, TRUE), ref = "tot", comp_all = FALSE)
  ps  <- build_slots(3L, "text")$pos_slots         # c(0, 3, 4, 5)
  testthat::expect_equal(fmt_color_slots(col, fmt_color_plan(col, "text"))[1], ps[4])

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
