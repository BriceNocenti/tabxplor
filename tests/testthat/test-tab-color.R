# PURPOSE: the colour engine's slots, and the legend prose that names the measure.
# ROLE: the shipped CONTRACT for R/fmt_class.R (the colour engine) -- a failure here is a user-visible change.
#   The exhaustive sweep around it lives in dev/tests/testthat/.
# See: CLAUDE.md section "Testing" (what belongs in which suite).

# === SECTION: the slot engine =====================================================================

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
  na_col <- fmt(n = c(1L, 1L, 1L), scale = "level_pct", pct_type = "row", diff = NA_real_, mean = NA_real_,
                ratio = NA_real_, color = "diff")
  testthat::expect_equal(fmt_color_slots(na_col, fmt_color_plan(na_col, "text")), rep(0L, 3))

  zero <- fmt(n = c(1L, 1L), scale = "level_pct", pct_type = "row", pct = c(0.4, 0.4), diff = c(0, 0.15),
              mean = c(1, 1), ratio = c(1, 1), color = "diff",
              row_kind = c("data", "total"), ref = "tot", comp_all = FALSE)
  testthat::expect_equal(fmt_color_slots(zero, fmt_color_plan(zero, "text"))[1], 0L)  # diff == 0
})


testthat::test_that("engine: numeric diff = Glass's delta; sd_ref 0/NA -> uncolored", {
  # ref (total) var = 4 -> sd_ref = 2 ; Glass = diff/sd_ref = 2/2 = 1.0 -> |1.0| > 0.8 -> level 4
  col  <- fmt(n = c(10L, 10L), scale = "level_mean", mean = c(5, 3), diff = c(2, 0), var = c(4, 4),
              color = "diff", row_kind = c("data", "total"), ref = "tot", comp_all = FALSE)
  plan <- fmt_color_plan(col, "text")
  os   <- plan$over_slots                            # c(0, 1, 2, 3, 4) for the 4 mean_diff breaks
  testthat::expect_equal(fmt_color_slots(col, plan)[1], os[5])   # level 4 -> intensity 4

  bad <- fmt(n = c(10L, 10L), scale = "level_mean", mean = c(5, 3), diff = c(2, 0), var = c(0, 0),
             color = "diff", row_kind = c("data", "total"), ref = "tot", comp_all = FALSE)
  testthat::expect_equal(fmt_color_slots(bad, fmt_color_plan(bad, "text"))[1], 0L)  # sd_ref 0
})


testthat::test_that("engine: no color mode -> NULL plan -> all slot 0", {
  col <- fmt(n = c(1L, 2L, 3L), scale = "level_pct", pct_type = "row", pct = c(0.1, 0.2, 0.7), color = "")
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
  col <- fmt(n = rep(100L, 4), scale = "points", pct_type = "row", pct = pct, diff = diff, ratio = ratio,
             ci_inf = ci_inf, ci_sup = ci_sup)
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



# --- Phase 14a: the guaranteed_effect break offset --------------------------------------------
# Under `guaranteed_effect` the score is the CI FLOOR, so the ladder must START at the neutral value:
# "the interval excludes the neutral" IS the definition of a guaranteed effect, and such a cell must
# be coloured -- the policy exists to colour MORE, so that everything solid shows in a small table.
# The rule is one rung down, not arithmetic, so every printed threshold is a number the reader
# already knows from the same ladder under `ignore`.

testthat::test_that("guaranteed_breaks prepends the neutral and drops the top rung", {
  # additive and multiplicative alike: no subtraction, no division
  testthat::expect_equal(guaranteed_breaks(c(0.05, 0.10, 0.20, 0.30), 0), c(0, 0.05, 0.10, 0.20))
  testthat::expect_equal(guaranteed_breaks(c(1.1, 1.2, 1.5, 2), 1),       c(1, 1.1, 1.2, 1.5))
  # a single break collapses onto the neutral (any guaranteed effect then takes slot 1)
  testthat::expect_equal(guaranteed_breaks(0.05, 0), 0)
  testthat::expect_equal(guaranteed_breaks(2, 1), 1)
  # an empty side (that measure is off for this column type) is untouched
  testthat::expect_equal(guaranteed_breaks(numeric(0), 0), numeric(0))
  testthat::expect_equal(guaranteed_breaks(numeric(0), 1), numeric(0))
  # the length is preserved, so the slot vector still aligns
  testthat::expect_length(guaranteed_breaks(c(1.5, 2, 4), 1), 3L)
  # `origin` is the ONE exemption, for a ladder written in confidence levels (zscore): re-anchoring
  # there instead, because prepending 0 would give it a structurally empty faintest shade.
  testthat::expect_equal(guaranteed_breaks(c(1.96, 2.58, 3.89, 6), 0, 1.96), c(1.96, 2.58, 3.89, 6))
  testthat::expect_equal(guaranteed_breaks(c(1.96, 2.58, 3.89, 6), 0, 2.58)[1], 2.58)
})


testthat::test_that("the background channel keeps its LOUD rungs, drawn in the faint slots", {
  # a fill is a secondary, at-a-glance voice: COLOR_SCALES$bg_keep says how many rungs survive there.
  # Of four, breaks 3 and 4 survive -- the loud ones, as they always did -- but they are DRAWN with
  # palette slots 1 and 3, so the loudest fill never sits under the text channel's own colour.
  col <- fmt(n = rep(100L, 2), scale = "level_pct", pct_type = "row", pct = c(.6, .5),
             diff = c(.1, 0), ratio = c(1.2, 1))
  col <- set_color(col, c("difference", "ratio"))
  p   <- resolve_color_channel_plans(col)
  testthat::expect_length(p$text$over_breaks, 4L)                 # pct_diff, untouched
  testthat::expect_equal(p$bg$over_breaks,  c(1.5, 2))            # pct_ratio 1.1/1.2/1.5/2 -> top two
  testthat::expect_equal(p$bg$under_breaks, c(2, 4))              # and 1.1/1.25/2/4 -> top two
  testthat::expect_equal(p$bg$over_slots,   c(0L, 1L, 3L))        # in the FAINT fills, not the loud
  testthat::expect_equal(p$bg$under_slots,  c(0L, 5L, 7L))
  # the text channel of the same measure keeps every rung
  testthat::expect_equal(fmt_color_plan(col, "text", color = "ratio")$over_breaks,
                         c(1.1, 1.2, 1.5, 2))
  # the trim runs AFTER the guaranteed_effect shift, never before
  g <- resolve_color_channel_plans(set_color_signif(col, "guaranteed_effect"))
  testthat::expect_equal(g$bg$over_breaks, c(1.2, 1.5))
})


testthat::test_that("guaranteed_effect: significant => coloured, in the right direction", {
  # the exact shape the maintainer reported: significant (0 outside the CI) but a floor far below
  # the first ordinary break (0.05) -- it MUST be coloured now.
  col <- fmt(n = rep(500L, 4), scale = "points", pct_type = "row", pct = c(.27, .13, .2, .2),
             diff  = c( .07, -.07,  .004, 0),
             ci_inf = c(.004, -.166, -.02, NA),      # cell 1 sig over (floor 0.4% << 5%)
             ci_sup = c(.166, -.004,  .03, NA))      # cell 2 sig under; cell 3 not sig
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


testthat::test_that("guaranteed_effect starts the RATIO (multiplicative) ladder at 1", {
  set_color_breaks(pct_ratio = c(1.5, 2, 4))
  withr::defer(options("tabxplor.color_breaks" = default_color_scales()))
  p_ref <- 0.2; pct <- 0.24                               # ratio 1.2 -- below the 1.5 first break
  col <- fmt(n = 500L, scale = "points", pct_type = "row", pct = pct, diff = pct - p_ref, ratio = pct / p_ref,
             ci_inf = 0.01, ci_sup = 0.07)
  col  <- set_color_signif(set_color(col, "ratio"), "guaranteed_effect")
  plan <- fmt_color_plan(col, "text")
  testthat::expect_equal(plan$over_breaks[1], 1)          # multiplicative neutral
  testthat::expect_true(fmt_color_slots(col, plan)[1] >= 1L)   # significant -> coloured
})



# === SECTION: the legend prose ====================================================================

skip_on_cran()


gss <- fx_gss()


# helper: the English plain-prose legend of a table (one string per colour group).
leg_en <- function(tab, ...) {
  suppressWarnings(tab_color_legend(tab, medium = "plain", style = "prose", lang = "en", ...))
}


testthat::test_that("pct diff prose names the MEASURE, the reference and the thresholds", {
  tb <- tab(gss, marital, race, pct = "row", color = "diff")
  l  <- leg_en(tb)
  testthat::expect_length(l, 1)
  # Phase 22f-i: the line leads with the measure in words, not with the palette ("Shades of blue"),
  # and the two sides are ONE sentence.
  testthat::expect_match(l, "Percentage points (risk) difference:", fixed = TRUE)
  testthat::expect_no_match(l, "Shades of")
  testthat::expect_match(l, "cell \u2265 the Total row")
  testthat::expect_match(l, "cell \u2264 the Total row")
  testthat::expect_match(l, "\\+5;.*\\+30 points", perl = TRUE)
  testthat::expect_match(l, "points\\.")
  testthat::expect_no_match(l, "Uncoloured")      # ignore policy -> no significance note
})


testthat::test_that("the CI method + confidence level come from the column stored facts", {
  # the default diff method = newcombe
  tb1 <- tab(gss, marital, race, pct = "row", color = "diff",
             color_signif = "grey_non_signif", ci = "ref")
  l1  <- leg_en(tb1)
  # Phase 22f-i: the note says the ONE thing a reader needs -- what an uncoloured cell means -- and
  # names the first threshold concretely. "Coloured => significant" was a tautology the cells show.
  testthat::expect_match(l1, "Uncoloured: not significantly different from the Total row")
  testthat::expect_match(l1, "under the first colour threshold (\u00b15 points)", fixed = TRUE)
  testthat::expect_no_match(l1, "Coloured: significantly")   # the tautology is gone
  testthat::expect_no_match(l1, "Grey:")
  testthat::expect_match(l1, "Newcombe score interval, 95% confidence")

  # an explicit diff method + a non-default conf_level must be reflected
  tb2 <- tab(gss, marital, race, pct = "row", color = "diff",
             color_signif = "grey_non_signif", ci = "ref",
             ci_method = c(diff = "ac"), conf_level = 0.9)
  l2  <- leg_en(tb2)
  testthat::expect_match(l2, "Wald interval with Agresti-Caffo adjustment, 90% confidence")
})


testthat::test_that("guaranteed_effect carries the guarantee in the head, and names the interval once", {
  tb <- tab(gss, marital, race, pct = "row", color = "diff",
            color_signif = "guaranteed_effect", ci = "ref")
  l  <- leg_en(tb)
  testthat::expect_match(l, "95%-guaranteed percentage points (risk) difference", fixed = TRUE)
  testthat::expect_match(l, "(Newcombe score interval floor)", fixed = TRUE)
  testthat::expect_match(l, "from the Total row", fixed = TRUE)   # ONE merged ladder, both sides
  testthat::expect_equal(lengths(regmatches(l, gregexpr("Newcombe", l))), 1L)
  testthat::expect_match(l, "Uncoloured: not significantly different from the Total row.", fixed = TRUE)
  testthat::expect_no_match(l, "margin of error")
})


testthat::test_that("a column with no stored method names no method (Phase 19b, D8)", {
  # the legend must never CLAIM a method: a hand-built / downgraded column that carries no
  # `ci_method` gets the confidence text alone, where the pre-19b legend fell back to a table-wide
  # default and could name an interval the bounds were never built with.
  tb <- tab(gss, marital, race, pct = "row", color = "diff",
            color_signif = "grey_non_signif", ci = "ref")
  testthat::expect_match(leg_en(tb), "Newcombe score interval, 95% confidence")
  tb2 <- dplyr::mutate(tb, dplyr::across(dplyr::where(is_fmt), ~ tabxplor:::set_ci_method(., "")))
  l2  <- leg_en(tb2)
  testthat::expect_match(l2, "95% confidence")
  testthat::expect_no_match(l2, "Newcombe")
})


testthat::test_that("tab_reg: a mean difference shows SD, IRR says IRR, OR says OR", {
  b <- suppressWarnings(tab_reg(gss, "tvhours", c("marital", "race"), family = "gaussian"))
  lb <- leg_en(b)
  # the acronym is DATA: printed exactly as the header spells it, never capitalised as prose
  testthat::expect_match(lb, "diff \u2265")
  testthat::expect_no_match(lb, "Diff \u2265")
  testthat::expect_no_match(lb, "mean difference:")   # ... and not repeated as a head either
  testthat::expect_match(lb, "SD")
  testthat::expect_no_match(lb, "\\+20%", perl = TRUE)   # the old beta-shows-percent bug (0.2 -> +20%)

  # Phase 14c: a regression table has NO total row -- its baseline is the reference category.
  testthat::expect_match(lb, "the reference category")
  testthat::expect_no_match(lb, "Total")

  i <- suppressWarnings(tab_reg(gss, "tvhours", c("marital", "race"), family = "poisson"))
  li <- leg_en(i)
  testthat::expect_match(li, "IRR \u2265")
  testthat::expect_no_match(li, "OR \u2265")
  # Phase 14c: ci_type "or" is the multiplicative SHAPE (OR / IRR / cumulative OR alike); naming it
  # unconditionally called a Poisson rate ratio an odds ratio.
  testthat::expect_match(li, "Wald interval on the log rate-ratio")
  testthat::expect_no_match(li, "odds-ratio")

  d2 <- dplyr::mutate(gss, married = as.integer(marital == "Married"))
  o  <- suppressWarnings(tab_reg(d2, "married", "race"))
  lo <- leg_en(o)
  testthat::expect_match(lo, "OR \u2265")
  testthat::expect_match(lo, "the reference category")
  testthat::expect_match(lo, "Wald interval on the log odds-ratio")
})


testthat::test_that("md medium wraps break-words in the same pandoc classes as the cells", {
  tb <- tab(gss, marital, race, pct = "row", color = "diff")
  l  <- suppressWarnings(tab_color_legend(tb, medium = "md", style = "prose", lang = "en"))
  # Phase 13d: the class names the palette SLOT, so the FIRST break (+5) is .p1 and the fourth (-30)
  # is .m4 -- the label still carries the threshold, which is the legend's whole job.
  testthat::expect_match(l, "\\[\\+5\\]\\{\\.p1\\}", perl = TRUE)
  testthat::expect_match(l, "\\[-30\\]\\{\\.m4\\}", perl = TRUE)
})


testthat::test_that("French catalog translates the prose when the .mo is available", {
  skip_if_no_gettext()   # helper-i18n.R: catalog compiled + NLS + LANGUAGE actually honoured here
  tb <- tab(gss, marital, race, pct = "row", color = "diff",
            color_signif = "grey_non_signif", ci = "ref")
  l  <- suppressWarnings(tab_color_legend(tb, medium = "plain", style = "prose", lang = "fr"))
  testthat::expect_match(l, "Diff\u00e9rence de points de pourcentage")
  testthat::expect_match(l, "la ligne Total")
  testthat::expect_match(l, "seuil de confiance \u00e0 95 %")
  # Phase 22f-i: the note states only what an UNCOLOURED cell means, translated
  testthat::expect_match(l, "Non color\u00e9 : pas significativement diff\u00e9rent")
  testthat::expect_no_match(l, "Gris\u00e9")
})


# Phase 22g-ii: WEIGHT IN A LEGEND COMES FROM THE PALETTE AND FROM NOTHING ELSE. The column-name
# prefix used to be bold in every medium, putting more emphasis on the legend than the table's own
# bold cells carry; the break-words keep theirs, because it IS the face of the cells they describe.
test_that("a legend's column names are plain; its coloured break-words keep the palette's face", {
  d  <- suppressWarnings(fx_reg_fmt())
  # a crude/model pair: two column blocks, so the legend names the columns it describes
  t  <- suppressMessages(tab_reg(d, "married", c("race", "relig"), family = "binomial",
                                 measure = "difference"))
  md <- paste(tab_md(t, print = FALSE), collapse = "\n")
  ln <- grep("\u2014 RD \u2265", strsplit(md, "\n")[[1]], value = TRUE)[[1]]
  testthat::expect_match(ln, "^Obs_RD, Model_mRD \u2014 ")      # the names ARE there...
  testthat::expect_false(grepl("**", sub(" \u2014 .*", "", ln), fixed = TRUE))   # ...and plain
  testthat::expect_match(ln, "**", fixed = TRUE)               # the break-words still carry theirs
  # and the token model no longer has a hand-set flag to disagree with the palette
  testthat::expect_false("b" %in% names(tabxplor:::.lg_tok("x")))
})
