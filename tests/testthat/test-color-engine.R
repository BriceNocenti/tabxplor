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

# --- Bug-fix: color_all_signif on the RATIO channel uses a GUARANTEED RATIO, not the raw diff bound.
# Before the fix the branch fed get_ci_inf/get_ci_sup (a DIFFERENCE, centre 0) straight into the
# ratio fold (centre 1): a bound of ~0.05 folded to 1/0.05 -> strongest UNDER colour on every
# significant cell regardless of direction (over-represented cells wrongly got the /4 colour).
testthat::test_that("color_all_signif ratio channel colours the guaranteed RATIO (Bug 2)", {
  p_ref  <- c(0.2, 0.6, 0.7, 0.5)
  pct    <- c(0.6, 0.66, 0.1, 0.52)
  diff   <- pct - p_ref
  ratio  <- pct / p_ref
  ci_inf <- c(0.30, 0.01, -0.70, -0.05)   # cell1/2 sig over, cell3 sig under, cell4 spans 0
  ci_sup <- c(0.50, 0.11, -0.50,  0.09)
  col <- fmt(n = rep(100L, 4), type = "row", pct = pct, diff = diff, ratio = ratio,
             ci_inf = ci_inf, ci_sup = ci_sup, ci_type = "diff")
  col <- set_color(col, c("diff", "ratio"))
  col <- set_color_signif(col, "color_all_signif")

  plan      <- fmt_color_plan(col, "bg", color = "ratio")
  guar_diff <- dplyr::case_when(ci_inf > 0 ~ ci_inf, ci_sup < 0 ~ ci_sup, TRUE ~ NA_real_)
  expected  <- 1 + (ratio - 1) * (guar_diff / diff)          # centre-1 guaranteed ratio
  testthat::expect_equal(plan$score, expected)
  testthat::expect_false(isTRUE(all.equal(plan$score[1], ci_inf[1])))  # NOT the raw diff bound
  testthat::expect_gt(plan$score[1], 1)                      # over-rep guaranteed ratio > 1
  testthat::expect_lt(plan$score[3], 1)                      # under-rep guaranteed ratio < 1

  slot <- fmt_color_slots(col, plan)
  bs   <- build_slots(length(plan$pos_breaks), "bg")
  pos  <- bs$pos_slots[-1]; neg <- bs$neg_slots[-1]
  testthat::expect_false(slot[1] %in% neg)                   # over-rep NEVER an under-colour (the bug)
  testthat::expect_true(slot[1] %in% pos)                    # strong guaranteed over-ratio -> over
  testthat::expect_false(slot[3] %in% pos)                   # under-rep never an over-colour
  testthat::expect_equal(slot[4], 0L)                        # not significant -> uncoloured
})

testthat::test_that("grey_non_signif ratio channel still colours the OBSERVED ratio (regression)", {
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
  bs   <- build_slots(length(plan$pos_breaks), "bg")
  testthat::expect_true(slot[1] %in% bs$pos_slots[-1])       # ratio 3 (>=2) significant -> over colour
  testthat::expect_equal(slot[2], 0L)                        # not significant -> greyed
})
