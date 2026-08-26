
testthat::test_that("engine: ratio with ref 0 -> Inf/NaN -> uncolored (no crash)", {
  col <- fmt(n = c(10L, 10L), scale = "level_pct", pct_type = "row", pct = c(0.5, 0), ratio = c(Inf, 1),
             mean = c(Inf, 1), color = "ratio", row_kind = c("data", "total"),
             ref = "tot", comp_all = FALSE)
  testthat::expect_equal(fmt_color_slots(col, fmt_color_plan(col, "text"))[1], 0L)
})


testthat::test_that("grey_non_signif ratio channel still colours the OBSERVED ratio", {
  set_color_breaks(pct_ratio = c(1.5, 2, 4)); withr::defer(options("tabxplor.color_breaks" = default_color_scales()))
  p_ref  <- c(0.2, 0.5); pct <- c(0.6, 0.52)
  diff   <- pct - p_ref; ratio <- pct / p_ref
  ci_inf <- c(0.30, -0.05); ci_sup <- c(0.50, 0.09)          # cell1 sig over, cell2 not sig
  col <- fmt(n = rep(100L, 2), scale = "points", pct_type = "row", pct = pct, diff = diff, ratio = ratio,
             ci_inf = ci_inf, ci_sup = ci_sup)
  col <- set_color(col, c("diff", "ratio"))
  col <- set_color_signif(col, "grey_non_signif")
  plan <- fmt_color_plan(col, "bg", color = "ratio")
  testthat::expect_equal(plan$score, ratio)                  # observed ratio, not a floor
  slot <- fmt_color_slots(col, plan)
  testthat::expect_true(slot[1] >= 1L && slot[1] <= 4L)      # ratio 3 (>=2) significant -> over colour
  testthat::expect_equal(slot[2], 0L)                        # not significant -> greyed
})


testthat::test_that("guaranteed_effect offsets the plan's breaks; other policies do not", {
  mk <- function(policy) {
    col <- fmt(n = rep(100L, 3), scale = "points", pct_type = "row", pct = c(.6, .4, .5), diff = c(.1, -.1, 0),
               ci_inf = c(.05, -.15, -.02), ci_sup = c(.15, -.05, .02))
    set_color_signif(set_color(col, "diff"), policy)
  }
  ge <- fmt_color_plan(mk("guaranteed_effect"), "text")
  gn <- fmt_color_plan(mk("grey_non_signif"),   "text")
  ig <- fmt_color_plan(mk("ignore"),            "text")
  sc <- color_scales()$pct_diff

  testthat::expect_equal(ge$over_breaks,  c(0, utils::head(sc$over$breaks,  -1L)))
  testthat::expect_equal(ge$under_breaks, c(0, utils::head(sc$under$breaks, -1L)))
  testthat::expect_equal(ge$over_breaks[1], 0)                    # the ladder starts at the neutral
  # every other policy scores the OBSERVED value -> the ordinary breaks, untouched
  testthat::expect_equal(gn$over_breaks, sc$over$breaks)
  testthat::expect_equal(ig$over_breaks, sc$over$breaks)
})


testthat::test_that("guaranteed_effect: strict breaks keep an exactly-neutral floor uncoloured", {
  # findInterval(left.open = strict): a floor of exactly 0 is NOT beyond the 0 break -> slot 0.
  # Only a floor strictly beyond the neutral (i.e. a real guaranteed effect) colours.
  col <- fmt(n = rep(500L, 2), scale = "points", pct_type = "row", pct = c(.3, .3), diff = c(.1, .1),
             ci_inf = c(0, 1e-9), ci_sup = c(.2, .2))
  col  <- set_color_signif(set_color(col, "diff"), "guaranteed_effect")
  slot <- fmt_color_slots(col, fmt_color_plan(col, "text"))
  testthat::expect_equal(slot[1], 0L)                     # floor exactly 0 -> not a guaranteed effect
  testthat::expect_true(slot[2] >= 1L)                    # floor just beyond 0 -> coloured
})


# Phase 18a: contrib gains a significance gate via the stored standardized-residual p-value.
# Previously color="contrib" under a significance policy coloured NOTHING (no CI to gate on).
testthat::test_that("engine: contrib + significance policy gates on the residual p-value", {
  gss <- fx_gss()
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
  gss   <- fx_gss()
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



# --- Phase 16c: a degenerate guaranteed_effect channel is disabled (never the last one) -----------

testthat::test_that("Phase 16c: a single-break guaranteed_effect bg channel is disabled, text kept", {
  # a 1-break-per-side ratio scale (the x2 rule) collapses to the neutral under guaranteed_effect ->
  # a gradient-less "x1" fill. On the BACKGROUND channel (color = TRUE = diff text + ratio bg) it is
  # redundant with the text channel, so it is dropped. The arbiter is checked directly (independent of
  # whether any cell qualifies): the bg plan is NULL under guaranteed_effect, non-NULL otherwise.
  withr::local_options(list(tabxplor.color_breaks = {
    sc <- default_color_scales()
    sc$pct_ratio <- mk_color_scale("pct_ratio", list(over = 2))   # single break per side
    sc
  }))
  col_of <- function(t) t[[names(t)[purrr::map_lgl(t, is_fmt)][1]]]
  t_ge   <- tab(fx_gss(), race, marital, pct = "row", color = TRUE,
                color_signif = "guaranteed_effect")
  t_grey <- tab(fx_gss(), race, marital, pct = "row", color = TRUE,
                color_signif = "grey_non_signif")
  # arbiter: guaranteed_effect drops the degenerate bg, keeps text; a non-degenerate policy keeps bg
  pl_ge <- resolve_color_channel_plans(col_of(t_ge))
  testthat::expect_null(pl_ge$bg)
  testthat::expect_false(is.null(pl_ge$text))
  testthat::expect_false(is.null(resolve_color_channel_plans(col_of(t_grey))$bg))
  # and the cells follow: no background slot anywhere under guaranteed_effect
  bg_any <- function(t) any(purrr::map_lgl(t[purrr::map_lgl(t, is_fmt)],
                                           ~ any(fmt_color_channels(.)$bg_slot > 0)))
  testthat::expect_false(bg_any(t_ge))
})


testthat::test_that("Phase 16c: a LONE degenerate guaranteed_effect channel is NOT disabled", {
  withr::local_options(list(tabxplor.color_breaks = {
    sc <- default_color_scales()
    sc$pct_ratio <- mk_color_scale("pct_ratio", list(over = 2))
    sc
  }))
  tx_any <- function(t) any(purrr::map_lgl(t[purrr::map_lgl(t, is_fmt)],
                                           ~ any(fmt_color_channels(.)$text_slot > 0)))
  # single-channel ratio (no background) -> the only channel, kept even though degenerate
  t <- tab(fx_gss(), race, marital, pct = "row", color = "ratio",
           color_signif = "guaranteed_effect")
  testthat::expect_true(tx_any(t))
})



# === SECTION: the legend prose ====================================================================

skip_on_cran()


gss <- fx_gss()


# helper: the English plain-prose legend of a table (one string per colour group).
leg_en <- function(tab, ...) {
  suppressWarnings(tab_color_legend(tab, medium = "plain", style = "prose", lang = "en", ...))
}


testthat::test_that("ratio prose uses the Total column and the x/1 operators", {
  tb <- tab(gss, marital, race, pct = "col", color = "ratio")
  l  <- leg_en(tb)
  testthat::expect_match(l, "the Total column")
  testthat::expect_match(l, "Relative risk (ratio):", fixed = TRUE)
  testthat::expect_match(l, "\u00d72")            # x2 (the over-only default)
})


testthat::test_that("a publication palette says Unmarked, and keeps its two face words", {
  tb <- tab(gss, marital, race, pct = "row", color = "diff",
            color_signif = "grey_non_signif", ci = "ref")
  l  <- leg_en(tb, theme = "print_minimalistic")
  testthat::expect_match(l, "Underlined: cell \u2265 the Total row")
  testthat::expect_match(l, "Italic: cell \u2264 the Total row")
  testthat::expect_match(l, "Unmarked: not significantly different")
  testthat::expect_no_match(l, "Uncoloured")
})


testthat::test_that("numeric diff prose shows the standardized SD thresholds, not percents", {
  tb <- tab(gss, marital, tvhours, pct = "row", color = "diff")
  l  <- leg_en(tb)
  testthat::expect_match(l, "\\+0.2;.*\\+0.8 SD", perl = TRUE)
  testthat::expect_no_match(l, "points")
  testthat::expect_no_match(l, "\\+20%")          # the old beta-shows-percent bug
})


testthat::test_that("terse console form is compact and coloured-word based", {
  tb <- tab(gss, marital, race, pct = "row", color = "diff")
  l  <- suppressWarnings(tab_color_legend(tb, medium = "console", style = "terse",
                                          lang = "en", colored = FALSE))
  testthat::expect_length(l, 1)
  testthat::expect_match(l, "difference")
  testthat::expect_match(l, "Total")
  # the console keeps the SHORT measure word; the long one belongs to the export footers
  testthat::expect_no_match(l, "Percentage points")
})


testthat::test_that("runs medium returns rich-text runs with hex + bold on the break-words", {
  tb   <- tab(gss, marital, race, pct = "row", color = "diff")
  runs <- suppressWarnings(tab_color_legend(tb, medium = "runs", style = "prose", lang = "en"))
  testthat::expect_type(runs, "list")
  flat <- unlist(runs, recursive = FALSE)
  cols <- vapply(flat, function(r) r$color, character(1))
  bold <- vapply(flat, function(r) isTRUE(r$bold), logical(1))
  testthat::expect_true(any(!is.na(cols)))                       # some coloured runs
  testthat::expect_true(all(bold[!is.na(cols)]))                 # coloured runs are bold
  testthat::expect_true(all(grepl("^#[0-9A-F]{6}$", cols[!is.na(cols)])))
})


testthat::test_that("a runs-medium background break-word uses the darker bg_legend palette", {
  # WHY: a run carries a font colour but no fill, and the background palette (L 0.85-0.97) is
  # invisible drawn as text on Excel's white sheet.
  tb   <- tab(gss, marital, race, pct = "row", color = c("diff", "ratio"))
  # theme pinned: Phase 14g makes the DEFAULT console theme detected from the editor, so an
  # option-driven legend would use the dark palette on a dark machine and never match `*_pal` below.
  hexf <- function(medium) {
    l <- suppressWarnings(tab_color_legend(tb, medium = medium, style = "prose", lang = "en",
                                           theme = "light"))
    if (identical(medium, "runs")) {
      f <- unlist(l, recursive = FALSE)
      toupper(stats::na.omit(vapply(f, function(r) r$color, character(1))))
    } else toupper(unlist(regmatches(l, gregexpr("#[0-9A-Fa-f]{6}", l))))
  }
  bg_pal  <- toupper(get_color_style("color_code", type = "bg",        theme = "light"))
  leg_pal <- toupper(get_color_style("color_code", type = "bg_legend", theme = "light"))
  testthat::expect_false(any(bg_pal %in% leg_pal))               # the two families are disjoint
  testthat::expect_true(any(hexf("runs") %in% leg_pal))          # runs -> darkened
  testthat::expect_false(any(hexf("runs") %in% bg_pal))
  testthat::expect_true(any(hexf("html") %in% bg_pal))           # html CAN fill -> the real hex
  # a custom background palette must not keep the DEFAULT (blue) legend hues
  old <- get("base", envir = tabxplor:::tabxplor_palette_env)
  on.exit({ assign("base", old, envir = tabxplor:::tabxplor_palette_env)
            tabxplor:::build_palettes() }, add = TRUE)
  green <- c("#e8f6e8", "#cceccc", "#a8dda8", "#7cc97c")
  set_color_palette(background_colors = green)
  testthat::expect_equal(unname(get_color_style("color_code", type = "bg_legend",
                                                theme = "light")[1:4]), green)
})


testthat::test_that("get_color_style(type = 'bg_legend') is color_code-only", {
  testthat::expect_length(get_color_style("color_code", type = "bg_legend", theme = "light"), 8L)
  testthat::expect_error(get_color_style("crayon", type = "bg_legend"), "bg_legend")
})


testthat::test_that("the console legend honours its `theme` argument, not the option", {
  # It used to read getOption("tabxplor.color_style_theme") and silently disagree with the palette
  # every other medium resolved from `theme`.
  tb <- tab(gss, marital, race, pct = "row", color = "diff")
  withr::with_options(list(cli.num_colors = 256,
                           tabxplor.color_style_theme = "light"), {
    lite <- suppressWarnings(tab_color_legend(tb, medium = "console", theme = "light"))
    dark <- suppressWarnings(tab_color_legend(tb, medium = "console", theme = "dark"))
    testthat::expect_false(identical(lite, dark))
  })
})


testthat::test_that("a custom palette drops the baked colour-shade names", {
  old <- get("base", envir = tabxplor:::tabxplor_palette_env)
  on.exit({ assign("base", old, envir = tabxplor:::tabxplor_palette_env);
            tabxplor:::build_palettes() }, add = TRUE)
  set_color_palette(text_colors = c("#111111", "#222222", "#333333", "#444444"))
  tb <- tab(gss, marital, race, pct = "row", color = "diff")
  l  <- leg_en(tb)
  testthat::expect_no_match(l, "Shades of")
  testthat::expect_match(l, "the Total row")      # still describes the reference + thresholds
})


testthat::test_that("lang = 'fr' applies the French decimal comma (locale-independent)", {
  # the number formatting follows the resolved `lang` directly (not the gettext catalog), so this
  # holds even when the compiled .mo is absent.
  tb  <- tab(gss, marital, tvhours, comp = "all", color = "ratio")   # mean-ratio: decimal breaks
  l   <- suppressWarnings(tab_color_legend(tb, medium = "plain", style = "prose", lang = "fr"))
  testthat::expect_match(l, "1,2")                # FR decimal comma (not "1.2")
  testthat::expect_no_match(l, "1\\.2", perl = TRUE)
})


# --- Phase 22b-iv: the stars sentence, and the gap lead that matches its own shades ----------------

test_that("the stars sentence names the Constant's null only where that row is populated", {
  d <- suppressWarnings(fx_reg_fmt())
  co <- suppressMessages(tab_reg(d, "married", c("race", "relig"), family = "binomial"))
  testthat::expect_match(tabxplor:::tab_stars_legend(co),
                         "reference category (in bold)", fixed = TRUE)
  testthat::expect_match(tabxplor:::tab_stars_legend(co), "from 1 for the Constant", fixed = TRUE)
  # a marginal table has a Constant ROW but no intercept to show -> no aside
  mg <- suppressMessages(tab_reg(d, "married", c("race", "relig"), family = "binomial",
                                 effect = "marginal", measure = "difference"))
  testthat::expect_false(grepl("Constant", tabxplor:::tab_stars_legend(mg), fixed = TRUE))
})


test_that("a gap measure's legend states distance-from-the-null, which is what it grades", {
  d <- suppressWarnings(fx_reg_fmt())
  t <- suppressMessages(tab_reg(d, "married", c("race", "relig"), family = "binomial",
                                empirical = TRUE, color = "adjustment"))
  lg <- paste(tab_md(t, print = FALSE), collapse = " ")
  testthat::expect_match(lg, "further from no effect (1) than the observed column", fixed = TRUE)
  testthat::expect_match(lg, "closer to no effect (1) than the observed column",  fixed = TRUE)
  # the generic signed-move lead is exactly what the score does NOT compute
  testthat::expect_false(grepl("OR \u2265 the observed", lg))
})
