# PURPOSE: Validation coverage for the Phase 5 color CONFIG layer (breaks list + palette).
#          Written spec-first (brief dev/new_colors_UI.md §13.1) so the new config is coded
#          against a spec, not "looks right". Filled in during Step 1 (breaks) / Step 2 (palette).
# ROLE: First-class tests for how breaks/palettes are WRITTEN and VALIDATED (there were none
#        before Phase 5). Complements the byte-identity net in test-color-golden.R.
#
# SPEC TO COVER (fill as the pieces land):
#   set_color_breaks(list(...)) / tab(color_breaks=):
#     - accepts the 5 named scales (pct_diff, pct_ratio, mean_diff, mean_ratio, contrib);
#     - clear cli error on: non-numeric, negative/zero/NA breaks, non-monotone breaks,
#       too many breaks (> palette steps), unknown list names, ratio breaks <= 1;
#     - per-table color_breaks= overrides the global; unsupplied scales fall back to global;
#     - an empty/NULL scale drops that measure for that column type (§7.4);
#     - old-arg shim: pct_breaks splits into pct_diff (<=1) + pct_ratio (>1); mean_breaks ->
#       mean_ratio; contrib_breaks -> contrib; each emits deprecate_soft once.
#   set_color_style():
#     - a custom palette of the wrong length/format errors (length-11 accepted, the old bug);
#   color / color_signif arg parsing:
#     - unknown color / color_signif value errors; a color vector longer than 2 errors;
#     - contrib / or on the background channel errors.

# Restore the default scales after each test that mutates the global option.
reset_breaks <- function() options("tabxplor.color_breaks" = default_color_scales())

testthat::test_that("set_color_breaks(list()) sets the named scales, keeps the others", {
  withr::defer(reset_breaks())
  reset_breaks()
  set_color_breaks(list(pct_diff = c(0.05, 0.15, 0.3), pct_ratio = c(3)))
  cur <- getOption("tabxplor.color_breaks")
  testthat::expect_equal(cur$pct_diff$pos, c(0.05, 0.15, 0.3))
  testthat::expect_equal(cur$pct_ratio$pos, 3)
  # untouched scales keep their default
  testthat::expect_equal(cur$mean_ratio$pos, c(1.15, 1.5, 2, 4))
  testthat::expect_equal(cur$contrib$pos, c(1, 2, 5, 10))
})

testthat::test_that("mk_color_scale metadata: center / strict / std per scale", {
  testthat::expect_equal(mk_color_scale("pct_diff",  c(0.1))$center, 0)
  testthat::expect_true (mk_color_scale("pct_diff",  c(0.1))$strict)
  testthat::expect_equal(mk_color_scale("pct_ratio", c(2))$center, 1)
  testthat::expect_false(mk_color_scale("contrib",   c(1))$strict)      # inclusive
  testthat::expect_true (mk_color_scale("mean_diff", NULL)$std)          # NULL -> standardized
  testthat::expect_equal(mk_color_scale("mean_diff", NULL)$pos, c(0.2, 0.5, 0.8))
  testthat::expect_false(mk_color_scale("mean_diff", c(200, 500))$std)   # units -> absolute
})

testthat::test_that("an empty/NULL scale drops the measure for its column type (except mean_diff)", {
  testthat::expect_length(mk_color_scale("pct_ratio", numeric())$pos, 0L)
  testthat::expect_length(mk_color_scale("mean_ratio", NULL)$pos, 0L)
  # mean_diff NULL is the ONE exception: restores the standardized default (not "off")
  testthat::expect_length(mk_color_scale("mean_diff", NULL)$pos, 3L)
})

testthat::test_that("set_color_breaks validates its input with clear errors", {
  withr::defer(reset_breaks())
  testthat::expect_error(set_color_breaks(list(pct_ratio = c(0.5))), "must all be > 1")
  testthat::expect_error(set_color_breaks(list(pct_diff  = c(-0.1))), "must all be > 0")
  testthat::expect_error(set_color_breaks(list(pct_diff  = c(0.2, 0.1))), "strictly increasing")
  testthat::expect_error(set_color_breaks(list(nonsense  = c(1))), "Unknown color-break scale")
  testthat::expect_error(set_color_breaks(list(pct_diff  = "a")), "must be numeric")
  testthat::expect_error(set_color_breaks(list(pct_diff  = c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06))),
                         "at most 5")
  testthat::expect_error(set_color_breaks(list(1, 2)), "fully named list")   # unnamed
})

testthat::test_that("get_color_breaks(type = 'all') mirrors additive vs multiplicative scales", {
  reset_breaks()
  # additive scales mirror c(x, -x); multiplicative scales mirror c(x, 1/x)
  testthat::expect_equal(get_color_breaks("pct_diff", "all"),
                         c(0.05, 0.1, 0.2, 0.3, -0.05, -0.1, -0.2, -0.3))
  testthat::expect_equal(get_color_breaks("mean_ratio", "all"),
                         c(1.15, 1.5, 2, 4, 1/1.15, 1/1.5, 1/2, 1/4))
  testthat::expect_equal(get_color_breaks("contrib", "all"), c(1, 2, 5, 10, -1, -2, -5, -10))
})

testthat::test_that("old positional args are soft-deprecated and mapped onto the new scales", {
  withr::defer(reset_breaks())
  reset_breaks()
  # split pct_breaks into pct_diff (<=1) + pct_ratio (>1); deprecate_soft signalled once
  lifecycle::expect_deprecated(
    set_color_breaks(pct_breaks = c(0.05, 0.1, 0.2, 2, 0.3))
  )
  cur <- getOption("tabxplor.color_breaks")
  testthat::expect_equal(cur$pct_diff$pos, c(0.05, 0.1, 0.2, 0.3))
  testthat::expect_equal(cur$pct_ratio$pos, 2)
})

testthat::test_that("get_color_breaks returns the canonical positive-only scales and round-trips", {
  withr::defer(reset_breaks())
  reset_breaks()
  gb <- get_color_breaks()
  testthat::expect_named(gb, c("pct_diff", "pct_ratio", "mean_diff", "mean_ratio", "contrib"))
  testthat::expect_equal(gb$pct_diff, c(0.05, 0.1, 0.2, 0.3))
  testthat::expect_equal(gb$pct_ratio, 2)
  testthat::expect_equal(get_color_breaks("pct_diff"), c(0.05, 0.1, 0.2, 0.3))
  testthat::expect_equal(get_color_breaks("pct"), c(0.05, 0.1, 0.2, 0.3))   # old alias -> pct_diff
  testthat::expect_equal(get_color_breaks("mean"), c(1.15, 1.5, 2, 4))      # old alias -> mean_ratio
  # round-trips through set_color_breaks()
  set_color_breaks(get_color_breaks())
  testthat::expect_equal(get_color_breaks()$pct_diff, c(0.05, 0.1, 0.2, 0.3))
})

# --- Step 4d: the tab() color / color_signif argument forms ---
testthat::test_that("tab() color argument forms set the right channels + policy", {
  d <- forcats::gss_cat
  col1 <- function(t) t[[names(t)[purrr::map_lgl(t, is_fmt)][1]]]

  s <- col1(tab(d, marital, race, pct = "row", color = "diff"))          # scalar -> text only
  testthat::expect_equal(get_color(s), "diff")
  testthat::expect_true(is.na(get_color_bg(s)))

  tt <- col1(tab(d, marital, race, pct = "row", color = TRUE))           # per-type: factor -> diff + ratio
  testthat::expect_equal(get_color(tt), "diff")
  testthat::expect_equal(get_color_bg(tt), "ratio")

  v <- col1(tab(d, marital, race, pct = "row", color = c("diff", "ratio")))
  testthat::expect_equal(c(get_color(v), get_color_bg(v)), c("diff", "ratio"))

  nm <- col1(tab(d, marital, race, pct = "row", color = c(text = "diff", background = "ratio")))
  testthat::expect_equal(c(get_color(nm), get_color_bg(nm)), c("diff", "ratio"))

  bgo <- col1(tab(d, marital, race, pct = "row", color = c(background = "ratio")))  # bg only
  testthat::expect_equal(get_color(bgo), "")
  testthat::expect_equal(get_color_bg(bgo), "ratio")

  g <- col1(tab(d, marital, race, pct = "row", color = "diff", color_signif = "grey_non_signif"))
  testthat::expect_equal(get_color_signif(g), "grey_non_signif")

  off <- col1(tab(d, marital, race, pct = "row", color = FALSE))
  testthat::expect_equal(get_color(off), "")

  cnt <- col1(tab(d, marital, race, pct = "no", color = TRUE))           # counts -> contrib
  testthat::expect_equal(get_color(cnt), "contrib")
})

testthat::test_that("tab() color argument errors are clear", {
  d <- forcats::gss_cat
  testthat::expect_error(tab(d, marital, race, pct = "row", color = "diff", color_signif = "nope"),
                         "Unknown")
  testthat::expect_error(tab(d, marital, race, pct = "row", color = c("diff", "contrib")),
                         "background channel")
})

testthat::test_that("old combined colour strings are soft-deprecated but still colour", {
  d <- forcats::gss_cat
  for (m in c("diff_ci", "after_ci", "ci")) {
    lifecycle::expect_deprecated(tab(d, marital, race, pct = "row", ci = "cell", color = m))
  }
  # they still produce coloured cells (the engine decodes them)
  withr::local_options(lifecycle_verbosity = "quiet")
  t  <- tab(d, marital, race, pct = "row", color = "diff_ci")
  fc <- t[[which(purrr::map_lgl(t, is_fmt))[2]]]
  testthat::expect_true(any(fmt_color_channels(fc)$text_slot != 0L))
  # the new API does NOT emit the deprecation
  testthat::expect_no_condition(
    tab(d, marital, race, pct = "row", color = "diff", color_signif = "grey_non_signif"),
    class = "lifecycle_warning_deprecated"
  )
})

testthat::test_that("two-channel colour: background channel renders independently of text", {
  withr::local_options(lifecycle_verbosity = "quiet")
  d <- forcats::gss_cat
  col2 <- function(t) t[[which(purrr::map_lgl(t, is_fmt))[2]]]

  # color = TRUE (diff text + ratio bg): the text channel colours (diffs always exceed 5%).
  tt <- col2(tab(d, marital, race, pct = "row", color = TRUE))
  testthat::expect_true(any(!is.na(fmt_channel_codes(tt, "text", "light", "no")$text)))

  # background = diff guarantees coloured fills (|diff| > 5%); the text channel stays empty.
  bgo    <- col2(tab(d, marital, race, pct = "row", color = c(background = "diff")))
  codesb <- fmt_channel_codes(bgo, "text", "light", "no")
  testthat::expect_true(all(is.na(codesb$text)))     # no text colour
  testthat::expect_true(any(!is.na(codesb$bg)))       # background coloured

  # both channels present -> both coloured, independently
  both <- col2(tab(d, marital, race, pct = "row", color = c(text = "diff", background = "diff")))
  cb   <- fmt_channel_codes(both, "text", "light", "no")
  testthat::expect_true(any(!is.na(cb$text)))
  testthat::expect_true(any(!is.na(cb$bg)))
})

testthat::test_that("set_color_style(custom_palette=) accepts 11 slots (the ratio slot fixed)", {
  withr::defer({ options("tabxplor.color_style" = NULL); set_color_style(type = "text", theme = "light") })
  pal11 <- sprintf("#%06X", seq_len(11) * 1000L)
  set_color_style(custom_palette = pal11)
  cur <- getOption("tabxplor.color_style")
  testthat::expect_length(cur, 11L)
  testthat::expect_named(cur, c("pos1","pos2","pos3","pos4","pos5",
                                "neg1","neg2","neg3","neg4","neg5", "ratio"))
  testthat::expect_equal(unname(cur[["ratio"]]), pal11[11])   # the ratio slot is now populated
  # wrong length still errors, clearly
  testthat::expect_error(set_color_style(custom_palette = pal11[1:10]), "length 11")
})
