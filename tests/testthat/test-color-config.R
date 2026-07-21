# PURPOSE: Validation coverage for the colour CONFIG layer (breaks list + palette), Phase 13a.
# ROLE: First-class tests for how breaks/palettes are WRITTEN and VALIDATED. Complements the
#        byte-identity net in test-color-golden.R.
#
# The canonical scale shape (Phase 13a) is
#   list(center, strict, std, over = list(breaks, slots), under = list(breaks, slots)).
# Both sides carry POSITIVE magnitudes; the engine folds every cell to a magnitude and picks the
# side by direction. Input: signed / reciprocal literals (one-sided auto-mirrors, NA skips a slot),
# or list(over =, under =) (no mirror; omit a side to switch it off).

reset_breaks <- function() options("tabxplor.color_breaks" = default_color_scales())

testthat::test_that("set_color_breaks() sets named scales, keeps the others (list or ... form)", {
  withr::defer(reset_breaks())
  reset_breaks()
  set_color_breaks(list(pct_diff = c(0.05, 0.15, 0.3), pct_ratio = list(over = 3)))
  cur <- getOption("tabxplor.color_breaks")
  testthat::expect_equal(cur$pct_diff$over$breaks, c(0.05, 0.15, 0.3))
  testthat::expect_equal(cur$pct_ratio$over$breaks, 3)
  testthat::expect_length(cur$pct_ratio$under$breaks, 0L)         # over-only -> no under
  testthat::expect_equal(cur$mean_ratio$over$breaks, c(1.2, 1.5, 2, 4))  # untouched (symmetric default)
  # the `...` named-scale form works too
  set_color_breaks(contrib = c(1, 2, 5))
  testthat::expect_equal(getOption("tabxplor.color_breaks")$contrib$over$breaks, c(1, 2, 5))
})

testthat::test_that("mk_color_scale metadata + notation: mirror / signed / list(over,under) / NA", {
  # metadata
  testthat::expect_equal(mk_color_scale("pct_diff",  c(0.1))$center, 0)
  testthat::expect_true (mk_color_scale("pct_diff",  c(0.1))$strict)
  testthat::expect_equal(mk_color_scale("pct_ratio", list(over = 2))$center, 1)
  testthat::expect_false(mk_color_scale("contrib",   c(1))$strict)      # inclusive
  testthat::expect_true (mk_color_scale("mean_diff", NULL)$std)          # NULL -> standardized
  testthat::expect_equal(mk_color_scale("mean_diff", NULL)$over$breaks, c(0.2, 0.5, 0.8))
  testthat::expect_false(mk_color_scale("mean_diff", c(200, 500))$std)   # units -> absolute

  # one-sided plain vector auto-mirrors (over == under)
  sc <- mk_color_scale("pct_diff", c(0.05, 0.1, 0.2, 0.3))
  testthat::expect_equal(sc$over$breaks, sc$under$breaks)
  testthat::expect_equal(sc$over$slots, c(1L, 2L, 3L, 4L))

  # signed / reciprocal two-sided vector = as-is, no mirror
  sc2 <- mk_color_scale("pct_diff", c(-0.05, 0.1, 0.2))
  testthat::expect_equal(sc2$under$breaks, 0.05)
  testthat::expect_equal(sc2$over$breaks,  c(0.1, 0.2))
  sc3 <- mk_color_scale("mean_ratio", c(1/1.5, 1/2, 1/4, 1.15, 1.5, 2, 4))
  testthat::expect_equal(sc3$under$breaks, c(1.5, 2, 4))     # reciprocals -> magnitudes
  testthat::expect_equal(sc3$over$breaks,  c(1.15, 1.5, 2, 4))

  # list(over =, under =) -> no mirror; omit a side to switch it off
  ov <- mk_color_scale("pct_ratio", list(over = 2))
  testthat::expect_equal(ov$over$breaks, 2)
  testthat::expect_length(ov$under$breaks, 0L)

  # NA slot-skip on a one-sided vector: non-NA positions ARE the intensities
  na <- mk_color_scale("pct_diff", c(NA, 0.05, 0.1, 0.2))
  testthat::expect_equal(na$over$breaks, c(0.05, 0.1, 0.2))
  testthat::expect_equal(na$over$slots,  c(2L, 3L, 4L))       # intensity 1 skipped
})

testthat::test_that("an empty/NULL scale drops the measure for its column type (except mean_diff)", {
  testthat::expect_length(mk_color_scale("pct_ratio", numeric())$over$breaks, 0L)
  testthat::expect_length(mk_color_scale("mean_ratio", NULL)$over$breaks, 0L)
  # mean_diff NULL is the ONE exception: restores the standardized default (not "off")
  testthat::expect_length(mk_color_scale("mean_diff", NULL)$over$breaks, 3L)
})

testthat::test_that("set_color_breaks validates its input with clear errors", {
  withr::defer(reset_breaks())
  testthat::expect_error(set_color_breaks(list(pct_ratio = c(1))), "cannot equal 1")   # the neutral
  testthat::expect_error(set_color_breaks(list(pct_diff  = c(0))), "cannot equal 0")
  testthat::expect_error(set_color_breaks(list(pct_diff  = c(0.2, 0.1))), "strictly increasing")
  testthat::expect_error(set_color_breaks(list(nonsense  = c(1))), "Unknown color-break scale")
  testthat::expect_error(set_color_breaks(list(pct_diff  = "a")), "must be numeric")
  testthat::expect_error(set_color_breaks(list(pct_diff  = c(0.01, 0.02, 0.03, 0.04, 0.05))),
                         "at most 4")
  testthat::expect_error(set_color_breaks(list(1, 2)), "must be named")          # unnamed
  # NA slot-skip is only allowed on a one-sided vector, not a two-sided one
  testthat::expect_error(set_color_breaks(list(pct_diff = c(-0.05, NA, 0.1))), "one-sided")
})

testthat::test_that("get_color_breaks(type = 'all') gives the signed / reciprocal engine breaks", {
  reset_breaks()
  # additive mirror c(-x, x) ; multiplicative mirror c(1/x, x); ascending overall
  testthat::expect_equal(get_color_breaks("pct_diff", "all"),
                         c(-0.3, -0.2, -0.1, -0.05, 0.05, 0.1, 0.2, 0.3))
  testthat::expect_equal(get_color_breaks("mean_ratio", "all"),
                         c(1/4, 1/2, 1/1.5, 1/1.2, 1.2, 1.5, 2, 4))
  testthat::expect_equal(get_color_breaks("contrib", "all"), c(-10, -5, -2, -1, 1, 2, 5, 10))
  # pct_ratio symmetric default (Phase 16c): both sides c(NA, 1.5, 2, 4) -> breaks 1.5, 2, 4
  testthat::expect_equal(get_color_breaks("pct_ratio", "all"), c(1/4, 1/2, 1/1.5, 1.5, 2, 4))
  # odds_ratio (Phase 16c): the dedicated OR scale, symmetric
  testthat::expect_equal(get_color_breaks("odds_ratio", "all"), c(1/4, 1/2, 1/1.5, 1/1.2, 1.2, 1.5, 2, 4))
})

testthat::test_that("get_color_breaks returns a readable form and round-trips", {
  withr::defer(reset_breaks())
  reset_breaks()
  gb <- get_color_breaks()
  testthat::expect_named(gb, c("pct_diff", "pct_ratio", "odds_ratio", "mean_diff", "mean_ratio", "contrib"))
  testthat::expect_equal(gb$pct_diff, c(0.05, 0.1, 0.2, 0.3))    # symmetric -> plain magnitudes
  testthat::expect_equal(gb$pct_ratio, c(1.5, 2, 4))            # symmetric (Phase 16c) -> plain magnitudes
  testthat::expect_equal(get_color_breaks("pct"),  c(0.05, 0.1, 0.2, 0.3))   # old alias
  testthat::expect_equal(get_color_breaks("mean"), c(1.2, 1.5, 2, 4))         # mean_ratio symmetric default
  # round-trips through set_color_breaks()
  set_color_breaks(get_color_breaks())
  testthat::expect_equal(get_color_breaks()$pct_diff, c(0.05, 0.1, 0.2, 0.3))
  testthat::expect_equal(get_color_breaks("pct_ratio"), c(1.5, 2, 4))
})

# --- the tab() color / color_signif argument grammar (position = channel, names = type) ---
testthat::test_that("tab() color argument forms set the right channels + policy", {
  d <- forcats::gss_cat
  col1 <- function(t) t[[names(t)[purrr::map_lgl(t, is_fmt)][1]]]

  s <- col1(tab(d, marital, race, pct = "row", color = "diff"))          # scalar -> text only
  testthat::expect_equal(get_color(s), "diff")
  testthat::expect_true(is.na(get_color_bg(s)))

  tt <- col1(tab(d, marital, race, pct = "row", color = TRUE))           # per-type: factor -> diff + ratio
  testthat::expect_equal(get_color(tt), "diff")
  testthat::expect_equal(get_color_bg(tt), "ratio")

  v <- col1(tab(d, marital, race, pct = "row", color = c("diff", "ratio")))  # positional channels
  testthat::expect_equal(c(get_color(v), get_color_bg(v)), c("diff", "ratio"))

  pt <- col1(tab(d, marital, race, pct = "row", color = c(pct = "ratio")))   # per type (pct)
  testthat::expect_equal(get_color(pt), "ratio")

  lst <- col1(tab(d, marital, race, pct = "row",
                  color = list(pct = c("diff", "ratio"), mean = "ratio")))   # list per type
  testthat::expect_equal(c(get_color(lst), get_color_bg(lst)), c("diff", "ratio"))

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
  testthat::expect_error(tab(d, marital, race, pct = "row", color = c(sex = "diff")),
                         "column type")   # unknown type key
})

testthat::test_that("old combined colour strings are soft-deprecated but still colour", {
  d <- forcats::gss_cat
  for (m in c("diff_ci", "after_ci", "ci")) {
    lifecycle::expect_deprecated(tab(d, marital, race, pct = "row", ci = "diff", color = m))
  }
  withr::local_options(lifecycle_verbosity = "quiet")
  t  <- tab(d, marital, race, pct = "row", color = "diff_ci")
  fc <- t[[which(purrr::map_lgl(t, is_fmt))[2]]]
  testthat::expect_true(any(fmt_color_channels(fc)$text_slot != 0L))
  testthat::expect_no_condition(
    tab(d, marital, race, pct = "row", color = "diff", color_signif = "grey_non_signif"),
    class = "lifecycle_warning_deprecated"
  )
})

testthat::test_that("two-channel colour: background channel renders independently of text", {
  withr::local_options(lifecycle_verbosity = "quiet")
  d <- forcats::gss_cat
  col2 <- function(t) t[[which(purrr::map_lgl(t, is_fmt))[2]]]

  tt <- col2(tab(d, marital, race, pct = "row", color = TRUE))    # diff text + ratio bg
  testthat::expect_true(any(!is.na(fmt_channel_codes(tt, "light")$text)))

  # a lone diff measure on the background: text empty, background coloured
  bgo    <- col2(tab(d, marital, race, pct = "row", color = c("", "diff")))
  codesb <- fmt_channel_codes(bgo, "light")
  testthat::expect_true(all(is.na(codesb$text)))
  testthat::expect_true(any(!is.na(codesb$bg)))

  both <- col2(tab(d, marital, race, pct = "row", color = c("diff", "diff")))
  cb   <- fmt_channel_codes(both, "light")
  testthat::expect_true(any(!is.na(cb$text)))
  testthat::expect_true(any(!is.na(cb$bg)))
})

testthat::test_that("set_color_palette validates and rebuilds the palettes", {
  # reset the base palette to the OKLCH defaults (set_color_palette() accumulates, so re-seed the env)
  withr::defer({ tabxplor_palette_env$base <- default_palette_base(); build_palettes() })
  set_color_palette(text_colors = c("#111111", "#222222", "#333333", "#444444"))
  sty <- get_color_style("color_code", type = "text", theme = "light")
  testthat::expect_length(sty, 8L)                    # 4 over + 4 under
  testthat::expect_equal(toupper(sty[1]), "#111111")  # over intensity 1 overridden
  testthat::expect_error(set_color_palette(text_colors = c("#111", "#222")), "4 hex")  # wrong length
})

testthat::test_that("per-table color_breaks overrides the global at render", {
  d <- forcats::gss_cat
  t_ov <- tab(d, race, marital, pct = "row", color = "diff",
              color_breaks = list(pct_diff = c(0.01, 0.02, 0.03)))
  testthat::expect_false(is.null(get_color_breaks_attr(t_ov)))   # Phase 17b: now in meta$color_breaks
  st <- push_color_breaks(t_ov)
  on.exit(pop_color_breaks(st), add = TRUE)
  # with tiny breaks, a strong cell reaches the top intensity (slot 4 over / 8 under)
  slots <- fmt_color_channels(t_ov$Married)$text_slot
  testthat::expect_true(any(slots %in% c(4L, 8L)))
  pop_color_breaks(st)
  # after pop the global default is restored (no top-intensity flooding on the default breaks)
  testthat::expect_equal(getOption("tabxplor.color_breaks")$pct_diff$over$breaks, c(0.05, 0.1, 0.2, 0.3))
})

# --- COMPAT (Phase 13a): deprecated colour surfaces degrade with no error, mapped to the new API ---
testthat::test_that("deprecated colour arguments / functions are wired, not errors", {
  withr::defer({
    tabxplor_palette_env$base <- default_palette_base(); build_palettes()
    options("tabxplor.color_breaks" = default_color_scales(),
            "tabxplor.color_style_theme" = "light")
  })
  d <- forcats::gss_cat

  # set_color_style() -> options + set_color_palette(), with a soft-deprecation. Phase 14l dropped its
  # `tabxplor.color_style_type` WRITE (that option is deprecated), so only the theme half survives.
  lifecycle::expect_deprecated(set_color_style(type = "bg", theme = "dark"))
  testthat::expect_equal(getOption("tabxplor.color_style_theme"), "dark")
  withr::local_options(lifecycle_verbosity = "quiet")
  set_color_style(type = "text", custom_palette = sprintf("#%06X", seq_len(11) * 1000L))
  testthat::expect_length(get_color_style("color_code", type = "text", theme = "light"), 8L)
  tabxplor_palette_env$base <- default_palette_base(); build_palettes()

  # color = c(text =, background =) -> positional channels
  lifecycle::expect_deprecated(
    tt <- tab(d, race, marital, pct = "row", color = c(text = "diff", background = "ratio")))
  testthat::expect_equal(unname(fmt_color_attr(tt$Married)), c("diff", "ratio"))

  # color_signif = "color_all_signif" -> "guaranteed_effect"
  lifecycle::expect_deprecated(
    g <- tab(d, race, marital, pct = "row", color = "diff", color_signif = "color_all_signif"))
  testthat::expect_equal(get_color_signif(g$Married), "guaranteed_effect")

  # set_color_breaks(pct_breaks =) -> pct_diff (<=1) + pct_ratio (>1)
  lifecycle::expect_deprecated(set_color_breaks(pct_breaks = c(0.05, 0.1, 0.2, 2, 0.3)))
  testthat::expect_equal(getOption("tabxplor.color_breaks")$pct_diff$over$breaks, c(0.05, 0.1, 0.2, 0.3))
  testthat::expect_equal(getOption("tabxplor.color_breaks")$pct_ratio$over$breaks, 2)

  # inert html_24_bit is absorbed, not an error
  testthat::expect_no_error(get_color_style("color_code", type = "text", html_24_bit = "blue_red"))
  testthat::expect_no_error(fmt_get_color_code(g$Married, html_24_bit = "blue_red"))
})


# --- Phase 14l: the `color_type` argument / option are deprecated + inert ------------------------
testthat::test_that("color_type is deprecated on every exporter and does nothing", {
  d  <- forcats::gss_cat
  tb <- tab(d, marital, race, pct = "row", color = TRUE)

  # each of the 7 public surfaces warns once when color_type is explicitly passed
  lifecycle::expect_deprecated(tab_kable(tb, color_type = "bg", engine = "html"))
  lifecycle::expect_deprecated(tab_md(tb, color_type = "bg", print = FALSE))
  lifecycle::expect_deprecated(tab_css(color_type = "bg"))
  lifecycle::expect_deprecated(tab_export(tb, "md", color_type = "bg", print = FALSE))
  lifecycle::expect_deprecated(tab_md_css(color_type = "bg"))
  p <- withr::local_tempfile(fileext = ".xlsx")
  lifecycle::expect_deprecated(suppressMessages(
    tab_xl(tb, color_type = "bg", path = p, open = FALSE, replace = TRUE)))
  testthat::skip_if_not_installed("ggpubr")
  lifecycle::expect_deprecated(tab_plot(tb, color_type = "bg"))
})

testthat::test_that("color_type is INERT: tab_css output is byte-identical with or without it", {
  withr::local_options(lifecycle_verbosity = "quiet")
  # tab_css is a pure function of (palette, theme), so byte-equality is the strongest inert proof.
  testthat::expect_identical(tab_css(color_type = "bg", style_tag = FALSE),
                             tab_css(style_tag = FALSE))
})

testthat::test_that("color_type default is no longer a literal that warns", {
  testthat::skip_if_not_installed("openxlsx2")
  # tab_xl's default was the literal "text" (the one exporter that ignored the option). Now the
  # sentinel: a plain call must NOT warn about color_type.
  tb <- tab(forcats::gss_cat, marital, race, pct = "row", color = TRUE)
  p  <- withr::local_tempfile(fileext = ".xlsx")
  n  <- 0
  withCallingHandlers(
    suppressMessages(tab_xl(tb, path = p, open = FALSE, replace = TRUE)),
    warning = function(w) { if (grepl("color_type", conditionMessage(w))) n <<- n + 1
                            invokeRestart("muffleWarning") })
  testthat::expect_equal(n, 0L)
})

testthat::test_that("the color_style_type OPTION is inert, and warns only when non-default", {
  withr::defer(rlang::reset_warning_verbosity("lifecycle_The option \"tabxplor.color_style_type\""))
  # inert: option = "bg" renders the SAME as an explicit text lookup (channel is chosen by `color=`).
  # Both reads fire the option-deprecation warning (the check is unconditional on `type`), so both are
  # suppressed here -- the warning itself is asserted separately below.
  withr::with_options(list(tabxplor.color_style_type = "bg"), {
    a <- suppressWarnings(get_color_style("color_code", theme = "light"))
    b <- suppressWarnings(get_color_style("color_code", type = "text", theme = "light"))
    testthat::expect_identical(a, b)
  })
  # warns when set non-default
  rlang::reset_warning_verbosity("lifecycle_The option \"tabxplor.color_style_type\"")
  withr::with_options(list(tabxplor.color_style_type = "bg"),
    lifecycle::expect_deprecated(get_color_style("color_code"), "color_style_type"))
  # silent when unset (the normal case now the seed write is gone)
  withr::with_options(list(tabxplor.color_style_type = NULL),
    testthat::expect_no_warning(get_color_style("color_code")))
})

testthat::test_that("the color_type=bg_legend latent abort can't be reached via the option", {
  withr::local_options(lifecycle_verbosity = "quiet")
  tb <- tab(forcats::gss_cat, marital, race, pct = "row", color = c("diff", "ratio"))
  # the option used to reach get_color_style("crayon", type = "bg_legend") via legend_render_line's
  # unvalidated fam("text"); hard-wiring "text" closes it. The DIRECT abort still stands (below).
  withr::with_options(list(tabxplor.color_style_type = "bg_legend"), {
    testthat::expect_no_error(suppressWarnings(print(tb)))
    testthat::expect_no_error(suppressWarnings(tab_color_legend(tb, medium = "console")))
  })
  testthat::expect_error(get_color_style("crayon", type = "bg_legend"), "bg_legend")
})


# --- Phase 14a: a color_signif policy forces the difference CI it gates on ----------------------
# normalize_color_spec() can only fold the policy into the legacy colour string for an EXPLICIT
# diff/ratio measure; `color = TRUE`/"auto" must reach tab_resolve_settings() as "auto" (it
# dispatches per column type), so the policy could not ride the string -> ci stayed "no" ->
# fmt_color_plan()'s gate saw NA bounds -> EVERY cell grey, on the DEFAULT color = TRUE.

testthat::test_that("color = TRUE + a color_signif policy computes the difference CI", {
  for (pol in c("grey_non_signif", "guaranteed_effect")) {
    t <- tab(forcats::gss_cat, race, marital, pct = "row", color = TRUE, color_signif = pol)
    fmt_cols <- t[purrr::map_lgl(t, is_fmt)]
    testthat::expect_true(any(!is.na(unlist(purrr::map(fmt_cols, get_ci_sup)))), label = pol)
    testthat::expect_true(any(get_ci_type(t) == "diff"), label = pol)
  }
})

testthat::test_that("an implicit color_signif CI == the explicit ci = 'diff' table", {
  # the user should not have to write ci = "diff" to get what color_signif asks for
  for (pol in c("grey_non_signif", "guaranteed_effect")) {
    for (cv in list(rlang::expr(marital), rlang::expr(tvhours), rlang::expr(c(marital, tvhours)))) {
      a <- tab(forcats::gss_cat, race, !!cv, pct = "row", color = TRUE, color_signif = pol)
      b <- tab(forcats::gss_cat, race, !!cv, pct = "row", color = TRUE, ci = "diff",
               color_signif = pol)
      testthat::expect_equal(a, b)
    }
  }
})

testthat::test_that("color_signif = 'ignore' does NOT force a CI", {
  t <- tab(forcats::gss_cat, race, marital, pct = "row", color = TRUE)
  fmt_cols <- t[purrr::map_lgl(t, is_fmt)]
  testthat::expect_true(all(is.na(unlist(purrr::map(fmt_cols, get_ci_sup)))))
})

testthat::test_that("an explicit ci = 'cell' with a color_signif policy is an error", {
  testthat::expect_error(
    tab(forcats::gss_cat, race, marital, pct = "row", color = TRUE, ci = "cell",
        color_signif = "grey_non_signif"),
    "cell"
  )
  # ... but ci = "cell" is fine without a policy
  testthat::expect_no_error(
    tab(forcats::gss_cat, race, marital, pct = "row", color = TRUE, ci = "cell"))
})

testthat::test_that("contrib / OR never get a difference CI forced on them", {
  # contrib has no difference CI (documented gap)
  t <- tab(forcats::gss_cat, race, marital, color = "contrib", color_signif = "grey_non_signif")
  testthat::expect_false(any(get_ci_type(t) == "diff"))

  # OR is pct = "row", so it matches the diff-family predicate -- but it carries its OWN ci_type =
  # "or" bounds (centre 1). Forcing a difference CI (centre 0) would have its inf tested against the
  # OR neutral 1 -> never significant -> the policy would grey the WHOLE table.
  o <- tab(forcats::gss_cat, marital, race, pct = "col", OR = "OR", color = TRUE,
           color_signif = "grey_non_signif")
  testthat::expect_false(any(get_ci_type(o) == "diff"))
})
