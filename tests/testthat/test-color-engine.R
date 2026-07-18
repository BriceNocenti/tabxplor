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


# --- Phase 14a: the guaranteed_effect break offset --------------------------------------------
# Under `guaranteed_effect` the score is the CI FLOOR, so the scale must START at the neutral value:
# "the interval excludes the neutral" IS the definition of a guaranteed effect, and such a cell must
# be coloured. Before 14a the floor was scored against the ordinary magnitude breaks, so a
# significant-but-modest cell (diff +7%, ci_inf +0.4%) stayed grey.

testthat::test_that("offset_guaranteed_breaks shifts each scale onto its neutral", {
  # additive: subtract the first break -> starts at 0
  testthat::expect_equal(offset_guaranteed_breaks(c(0.05, 0.10, 0.20, 0.30), 0),
                         c(0, 0.05, 0.15, 0.25))
  # multiplicative: divide by the first break -> starts at 1
  testthat::expect_equal(offset_guaranteed_breaks(c(1.15, 1.5, 2, 4), 1),
                         c(1.15, 1.5, 2, 4) / 1.15)
  # a single break collapses onto the neutral (any guaranteed effect then takes slot 1)
  testthat::expect_equal(offset_guaranteed_breaks(0.05, 0), 0)
  testthat::expect_equal(offset_guaranteed_breaks(2, 1), 1)
  # an empty side (that measure is off for this column type) is untouched
  testthat::expect_equal(offset_guaranteed_breaks(numeric(0), 0), numeric(0))
  testthat::expect_equal(offset_guaranteed_breaks(numeric(0), 1), numeric(0))
  # the sides are independent: an ASYMMETRIC scale offsets each by its OWN first break
  testthat::expect_equal(offset_guaranteed_breaks(c(1.5, 2, 4), 1), c(1.5, 2, 4) / 1.5)
})

testthat::test_that("guaranteed_effect offsets the plan's breaks; other policies do not", {
  mk <- function(policy) {
    col <- fmt(n = rep(100L, 3), type = "row", pct = c(.6, .4, .5), diff = c(.1, -.1, 0),
               ci_inf = c(.05, -.15, -.02), ci_sup = c(.15, -.05, .02), ci_type = "diff")
    set_color_signif(set_color(col, "diff"), policy)
  }
  ge <- fmt_color_plan(mk("guaranteed_effect"), "text")
  gn <- fmt_color_plan(mk("grey_non_signif"),   "text")
  ig <- fmt_color_plan(mk("ignore"),            "text")
  sc <- color_scales(mk("ignore"))$pct_diff

  testthat::expect_equal(ge$over_breaks,  sc$over$breaks  - sc$over$breaks[1])
  testthat::expect_equal(ge$under_breaks, sc$under$breaks - sc$under$breaks[1])
  testthat::expect_equal(ge$over_breaks[1], 0)                    # the scale starts at the neutral
  # every other policy scores the OBSERVED value -> the ordinary breaks, untouched
  testthat::expect_equal(gn$over_breaks, sc$over$breaks)
  testthat::expect_equal(ig$over_breaks, sc$over$breaks)
})

testthat::test_that("guaranteed_effect: significant => coloured, in the right direction", {
  # the exact shape the maintainer reported: significant (0 outside the CI) but a floor far below
  # the first ordinary break (0.05) -- it MUST be coloured now.
  col <- fmt(n = rep(500L, 4), type = "row", pct = c(.27, .13, .2, .2),
             diff  = c( .07, -.07,  .004, 0),
             ci_inf = c(.004, -.166, -.02, NA),      # cell 1 sig over (floor 0.4% << 5%)
             ci_sup = c(.166, -.004,  .03, NA),      # cell 2 sig under; cell 3 not sig
             ci_type = "diff")
  col  <- set_color_signif(set_color(col, "diff"), "guaranteed_effect")
  plan <- fmt_color_plan(col, "text")
  slot <- fmt_color_slots(col, plan)

  testthat::expect_true(slot[1] >= 1L && slot[1] <= 4L)   # guaranteed +0.4% -> an OVER slot, not grey
  testthat::expect_true(slot[2] >= 5L)                    # guaranteed -0.4% -> an UNDER slot
  testthat::expect_equal(slot[3], 0L)                     # CI spans 0 -> no guaranteed effect -> grey
  testthat::expect_equal(slot[4], 0L)                     # no CI -> grey

  # the invariant, stated directly: no cell may be significant yet uncoloured
  sig <- !is.na(get_ci_inf(col)) & (get_ci_inf(col) > 0 | get_ci_sup(col) < 0)
  testthat::expect_equal(sum(sig & slot == 0L), 0L)
})

testthat::test_that("guaranteed_effect: strict breaks keep an exactly-neutral floor uncoloured", {
  # findInterval(left.open = strict): a floor of exactly 0 is NOT beyond the 0 break -> slot 0.
  # Only a floor strictly beyond the neutral (i.e. a real guaranteed effect) colours.
  col <- fmt(n = rep(500L, 2), type = "row", pct = c(.3, .3), diff = c(.1, .1),
             ci_inf = c(0, 1e-9), ci_sup = c(.2, .2), ci_type = "diff")
  col  <- set_color_signif(set_color(col, "diff"), "guaranteed_effect")
  slot <- fmt_color_slots(col, fmt_color_plan(col, "text"))
  testthat::expect_equal(slot[1], 0L)                     # floor exactly 0 -> not a guaranteed effect
  testthat::expect_true(slot[2] >= 1L)                    # floor just beyond 0 -> coloured
})

testthat::test_that("guaranteed_effect offsets the RATIO (multiplicative) scale around 1", {
  set_color_breaks(pct_ratio = c(1.5, 2, 4))
  withr::defer(options("tabxplor.color_breaks" = default_color_scales()))
  p_ref <- 0.2; pct <- 0.24                               # ratio 1.2 -- below the 1.5 first break
  col <- fmt(n = 500L, type = "row", pct = pct, diff = pct - p_ref, ratio = pct / p_ref,
             ci_inf = 0.01, ci_sup = 0.07, ci_type = "diff")
  col  <- set_color_signif(set_color(col, c("diff", "ratio")), "guaranteed_effect")
  plan <- fmt_color_plan(col, "bg", color = "ratio")
  testthat::expect_equal(plan$over_breaks[1], 1)          # multiplicative neutral
  testthat::expect_true(fmt_color_slots(col, plan)[1] >= 1L)   # significant -> coloured
})

# Last Phase a: contrib gains a significance gate via the stored standardized-residual p-value.
# Previously color="contrib" under a significance policy coloured NOTHING (no CI to gate on).
testthat::test_that("engine: contrib + significance policy gates on the residual p-value", {
  gss <- forcats::gss_cat
  # grey_non_signif: a cell is coloured iff its residual is significant AND its contribution is large
  t_grey <- tab(gss, marital, race, pct = "row", color = "contrib",
                color_signif = "grey_non_signif")
  cols   <- names(t_grey)[purrr::map_lgl(t_grey, is_fmt)]
  any_col <- FALSE
  for (nm in cols) {
    x    <- t_grey[[nm]]
    slot <- fmt_color_channels(x)$text_slot
    pv   <- get_pvalue(x)
    sig  <- !is_totrow(x) & !is.na(pv) & pv < 0.05
    # every coloured cell must be significant & non-total (the gate direction we care about)
    testthat::expect_true(all(slot[slot > 0L] > 0L & sig[slot > 0L]), info = nm)
    if (any(slot > 0L)) any_col <- TRUE
    # a clearly non-significant cell (pv large) is never coloured
    testthat::expect_true(all(slot[!is.na(pv) & pv > 0.5] == 0L), info = nm)
  }
  testthat::expect_true(any_col)   # the fix: SOMETHING is coloured (was nothing)
})

testthat::test_that("engine: contrib + guaranteed_effect colours every significant cell", {
  gss   <- forcats::gss_cat
  t_all <- tab(gss, marital, race, pct = "row", color = "contrib",
               color_signif = "guaranteed_effect")
  cols  <- names(t_all)[purrr::map_lgl(t_all, is_fmt)]
  n_col <- 0L
  for (nm in cols) {
    x    <- t_all[[nm]]
    slot <- fmt_color_channels(x)$text_slot
    pv   <- get_pvalue(x)
    sig  <- !is_totrow(x) & !is.na(pv) & pv < 0.05
    # guaranteed_effect offsets the scale to the neutral, so EVERY significant cell is coloured
    testthat::expect_equal(slot > 0L, sig, info = nm)
    n_col <- n_col + sum(slot > 0L)
  }
  testthat::expect_gt(n_col, 0L)
})
