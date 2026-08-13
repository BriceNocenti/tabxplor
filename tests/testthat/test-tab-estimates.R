# Phase 18z17-i -- the estimate model behind forest_plot().
#
# Everything here runs on a TIBBLE, with no graphics device: that is the point of splitting the model
# out of the renderer (a ggplot has no golden lock; a tibble has). The load-bearing assertions are the
# two no-drift ones -- the plotted estimate is the number the table stores and prints, and the gap
# band's containment IS the gap test -- because they are what makes "the plot cannot disagree with the
# table" a fact rather than a hope.

skip_on_cran()  # ~8 model fits; devtools / covr / r-lib-actions all set NOT_CRAN = true

te_data <- function() gss_cat_data_formatting()

est <- function(...) tabxplor:::tab_estimates(...)

# the model column, by ROLE -- so this file names no column the package might rename (and so it
# stays ASCII: the gaussian one is "Model_\u03b2").
mod_col <- function(t) {
  nm <- names(t)[vapply(t, is_fmt, logical(1))]
  nm[vapply(nm, function(n) identical(as.character(tabxplor:::get_role(t[[n]]))[1], "model"),
            logical(1))][1]
}


# --- the scale record -------------------------------------------------------------------------------

test_that("fmt_scale_of() reads every column shape", {
  d <- te_data()
  key <- function(t, nm, kind = "auto") tabxplor:::fmt_scale_of(t[[nm]], kind)$key

  # crosstabs: the stored interval decides
  expect_identical(key(tab(d, race, party3, pct = "row", ci = "cell"), "1-Democrat"), "level_pct")
  expect_identical(key(tab(d, race, party3, pct = "row", ci = "diff"), "1-Democrat"), "points")
  expect_identical(key(tab(d, race, tvhours, pct = "row", ci = "cell"), "tvhours"),   "level_mean")
  expect_identical(key(tab(d, race, tvhours, pct = "row", ci = "diff"), "tvhours"),   "mean_diff")
  or <- tab(d, race, party3, pct = "row", OR = TRUE, color = "OR", stars = TRUE)
  expect_identical(key(or, "2-Independent, other"), "odds_ratio")

  # regressions: one key per family x effect
  b  <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", empirical = TRUE))
  expect_identical(key(b, "Model_OR"), "odds_ratio")
  bl <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", exponentiate = FALSE))
  expect_identical(key(bl, mod_col(bl)), "log_coef")
  g  <- suppressMessages(tab_reg(d, "tvhours", "race", family = "gaussian"))
  expect_identical(key(g, mod_col(g)), "raw_diff")

  # the `kind` override is a filter on the SAME dispatch, never a second one
  t <- tab(d, race, party3, pct = "row", ci = "diff")
  expect_identical(key(t, "1-Democrat", "level"),  "level_pct")
  expect_identical(key(t, "1-Democrat", "effect"), "points")
  expect_identical(key(tab(d, race, party3, pct = "row", ci = "cell"), "1-Democrat", "effect"),
                   "points")
})

test_that("the record carries the ladder, the SD and the secondary axis", {
  d <- te_data()
  s <- tabxplor:::fmt_scale_of(
    tab(d, race, party3, pct = "row", OR = TRUE, color = "OR", stars = TRUE)[["3-Republican"]])
  expect_equal(s$neutral, 1); expect_identical(s$trans, "log10"); expect_true(s$mult)
  expect_true(all(c(1.2, 1.5, 2, 4) %in% s$breaks))          # the odds_ratio ladder, both sides
  expect_true(all((1 / c(1.2, 1.5, 2, 4)) %in% s$breaks))   # `%in%` binds tighter than `/`
  expect_identical(sort(unique(s$break_dir)), c(-1L, 0L, 1L))

  # a gaussian beta: the ladder is in SD units, the axis in units of Y, so the record carries SD(Y)
  # and the secondary axis that makes the colour ladder legible
  g <- suppressMessages(tab_reg(d, "tvhours", "race", family = "gaussian"))
  sg <- tabxplor:::fmt_scale_of(g[[mod_col(g)]])
  expect_identical(sg$sec, "sd")
  expect_true(is.finite(sg$sd_y))
  expect_equal(sort(sg$breaks[sg$break_dir > 0]), sort(c(0.2, 0.5, 0.8) * sg$sd_y))

  # exponentiate = FALSE: the secondary axis is exp(), the ladder the logged odds_ratio one
  bl <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", exponentiate = FALSE))
  sb <- tabxplor:::fmt_scale_of(bl[[mod_col(bl)]])
  expect_identical(sb$sec, "exp")
  expect_equal(sort(sb$breaks[sb$break_dir > 0]), sort(round(log(c(1.2, 1.5, 2, 4)), 1)))

  # a user's break scale moves the plot's gridlines, which or_plot()'s private ladder never did
  old <- getOption("tabxplor.color_breaks")
  on.exit(options(tabxplor.color_breaks = old), add = TRUE)
  set_color_breaks(odds_ratio = c(1.5, 3))
  s2 <- tabxplor:::fmt_scale_of(
    tab(d, race, party3, pct = "row", OR = TRUE, color = "OR", stars = TRUE)[["3-Republican"]])
  expect_true(all(c(1.5, 3) %in% s2$breaks))
  expect_false(4 %in% s2$breaks)
})

test_that("ci_center() and fmt_gap_scale_key() are the same dispatch", {
  d <- te_data()
  tabs <- list(tab(d, race, party3, pct = "row", ci = "cell"),
               tab(d, race, party3, pct = "row", ci = "diff"),
               tab(d, race, tvhours, pct = "row", ci = "cell"),
               tab(d, race, tvhours, pct = "row", ci = "diff"),
               tab(d, race, party3, pct = "row", OR = TRUE, color = "OR", stars = TRUE),
               suppressMessages(tab_reg(d, "married", "race", family = "binomial",
                                        empirical = TRUE)),
               suppressMessages(tab_reg(d, "tvhours", "race", family = "gaussian",
                                        empirical = TRUE)))
  n <- 0L
  for (t in tabs) for (nm in names(t)[vapply(t, is_fmt, logical(1))]) {
    col <- t[[nm]]
    s   <- tabxplor:::fmt_scale_of(col)
    # they answer the same question wherever an interval exists. Where none does, ci_center() has no
    # subject and fmt_scale_of() falls back to what the column DISPLAYS -- which is what stops
    # `tab(OR = TRUE)`'s reference column (OR bounds NA by construction) reading as a percentage.
    if (tabxplor:::fmt_has_interval(col)) {
      expect_identical(tabxplor:::ci_center(col), vctrs::field(col, s$est_field))
      n <- n + 1L
    }
    expect_identical(tabxplor:::fmt_gap_scale_key(col), s$gap_key)
  }
  expect_gt(n, 10L)
})

test_that("with no stored interval the scale follows the display", {
  d <- te_data()
  t <- tab(d, race, party3, pct = "row", OR = TRUE, color = "OR", stars = TRUE)
  ref <- names(t)[vapply(t, function(c) is_fmt(c) && !tabxplor:::fmt_has_interval(c),
                         logical(1))]
  expect_gt(length(ref), 0L)                                  # the reference column
  for (nm in ref) {
    expect_identical(tabxplor:::fmt_scale_of(t[[nm]])$key, "odds_ratio")
    expect_identical(tabxplor:::fmt_scale_of(t[[nm]])$est_field, "or")
  }
  # so the whole table plots on ONE scale, and the axis is not decided by the reference column
  e <- est(t)
  expect_identical(unique(e$scale_key), "odds_ratio")
})


# --- the long model ---------------------------------------------------------------------------------

test_that("the row axis reads all four label-block shapes", {
  d <- te_data()
  e1 <- est(tab(d, race, party3, pct = "row", ci = "diff"))
  expect_identical(levels(e1$var), "race")                    # the variable names the block
  expect_setequal(as.character(unique(e1$level)), c("White", "Black", "Other"))
  expect_true(all(e1$group == ""))

  e2 <- est(tab(d, c(race, relig), party3, pct = "row", ci = "diff"))   # compacted
  expect_setequal(levels(e2$var), c("race", "relig"))

  e3 <- est(tab(d, race, party3, pct = "row", ci = "diff", tab_vars = black))
  expect_true(all(nzchar(e3$group)))                          # the sub-table level

  e4 <- est(suppressMessages(tab_reg(d, "married", c("race", "rincome"),
                                     family = "binomial")))   # var / levels
  expect_setequal(levels(e4$var), c("race", "rincome"))
  expect_false("Constant" %in% levels(e4$var))                # an intercept is not an effect
  e4i <- est(suppressMessages(tab_reg(d, "married", "race", family = "binomial")),
             intercept = TRUE)
  expect_true("Constant" %in% levels(e4i$var))
})

test_that("totals are dropped by default and restored on request", {
  d <- te_data()
  t <- tab(d, race, party3, pct = "row", ci = "diff")
  expect_false(any(est(t)$is_total))
  expect_false("Total" %in% est(t)$column)
  e <- est(t, totals = TRUE)
  expect_true(any(e$is_total))
  expect_true("Total" %in% e$column)
})

test_that("columns are chosen by ROLE, never by name", {
  d <- te_data()
  r <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", empirical = TRUE,
                                add_n = TRUE))
  expect_identical(unique(est(r)$column), "Model_OR")         # the models are the subject
  expect_false("n" %in% est(r)$column)                        # a count column carries no estimate
  # `observed = "ci"` pairs ONE crude column, by ci_type -- not every Obs_* column
  expect_setequal(unique(est(r, observed = "ci")$column), c("Obs_OR", "Model_OR"))
  expect_setequal(unique(est(r, observed = "ci")$series), c("observed", "modelled"))
  # `columns =` is honoured verbatim, and a typo is named
  expect_identical(unique(est(r, columns = "Obs_%")$column), "Obs_%")
  expect_error(est(r, columns = "Model_HR"), "no such column")
})

test_that("the facet key is one panel per estimate column, merging a crude twin", {
  d <- te_data()
  # a crosstab: one panel per column of the table (the maintainer's layout ruling)
  e <- est(tab(d, race, party3, pct = "row", ci = "diff"))
  xt <- tab(d, race, party3, pct = "row")
  expect_setequal(levels(e$facet), setdiff(names(xt)[vapply(xt, is_fmt, logical(1))], "Total"))
  # a model and its crude twin share a panel (same col_var, different roles)
  r <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", empirical = TRUE))
  expect_length(levels(est(r, observed = "ci")$facet), 1L)
  # multinomial: two MODEL columns under one col_var -> one panel each
  m <- suppressMessages(tab_reg(d, "party3", "race", family = "multinomial"))
  expect_length(levels(est(m)$facet), 2L)
  # model comparison: the single crude block is repeated in every model panel (ruling D7)
  cp <- suppressMessages(tab_reg(d, "married", list(m1 = "race", m2 = c("race", "rincome")),
                                 family = "binomial", empirical = TRUE))
  ec <- est(cp, observed = "ci")
  expect_setequal(levels(ec$facet), c("m1", "m2"))
  expect_setequal(unique(ec$facet[ec$column == "Obs_OR"]), factor(c("m1", "m2"), levels(ec$facet)))
})


# --- no drift ---------------------------------------------------------------------------------------

test_that("the plotted estimate IS the number the table stores and prints", {
  d <- te_data()
  tabs <- list(
    or   = suppressMessages(tab_reg(d, "married", "race", family = "binomial", empirical = TRUE)),
    beta = suppressMessages(tab_reg(d, "tvhours", "race", family = "gaussian", empirical = TRUE)),
    logc = suppressMessages(tab_reg(d, "married", "race", family = "binomial",
                                    exponentiate = FALSE, empirical = TRUE)),
    irr  = suppressWarnings(suppressMessages(
             tab_reg(d, "tvhours", "race", family = "poisson", empirical = TRUE))),
    mnl  = suppressMessages(tab_reg(d, "party3", "race", family = "multinomial")),
    xt_c = tab(d, race, party3, pct = "row", ci = "cell"),
    xt_d = tab(d, race, party3, pct = "row", ci = "diff"),
    xt_m = tab(d, race, tvhours, pct = "row", ci = "diff"),
    xt_o = tab(d, race, party3, pct = "row", OR = TRUE, color = "OR", stars = TRUE))
  if (requireNamespace("marginaleffects", quietly = TRUE))
    tabs$ame <- suppressMessages(tab_reg(d, "married", "race", family = "binomial",
                                         effect = "ame", empirical = TRUE))
  n_checked <- 0L
  for (t in tabs) {
    e <- est(t, observed = "ci")
    for (nm in unique(e$column)) {
      col <- t[[nm]]
      s   <- e[e$column == nm & !duplicated(e$row), , drop = FALSE]
      # (a) the estimate is exactly the field the STORED interval is centred on (where there is one:
      #     an intervalless column follows its `display` instead -- see the fixture above)
      if (tabxplor:::fmt_has_interval(col))
        expect_equal(s$estimate, tabxplor:::ci_center(col)[s$row], tolerance = 0)
      else
        expect_equal(s$estimate,
                     vctrs::field(col, tabxplor:::fmt_scale_of(col)$est_field)[s$row], tolerance = 0)
      # (b) on a model column that is also the primary number format() prints (the reference cell
      #     prints "1" / "0" through a different display token, so it is excluded)
      if (identical(as.character(get_role(col))[1], "model")) {
        k <- !s$is_ref
        expect_equal(s$estimate[k], get_num(col)[s$row][k], tolerance = 0)
      }
      n_checked <- n_checked + 1L
    }
  }
  expect_gt(n_checked, 12L)                                   # never vacuous
})

test_that("the gap band's containment IS the gap test", {
  skip_if_not_installed("survey")
  d <- te_data()
  t <- suppressMessages(tab_reg(d, "married", c("race", "rincome"), family = "poisson",
                                empirical = TRUE, color = c("OR", "adjustment")))
  e <- est(t)
  ok <- is.finite(e$gap_se)
  expect_gt(sum(ok), 3L)                                      # never vacuous
  outside <- e$estimate[ok] < e$gap_lo[ok] | e$estimate[ok] > e$gap_hi[ok]
  alpha   <- 1 - get_conf_level(t[["Model_RR"]])[1]
  expect_identical(outside, e$gap_p[ok] < alpha)
  # and the band is the observed effect, folded on the estimate's own scale
  expect_equal(e$gap_lo[ok] * e$gap_hi[ok], e$obs[ok]^2)      # multiplicative: geometric mean = obs
  expect_true(all(e$gap_tested[ok]))
})

test_that("z17 D2: the gap band needs no colour argument", {
  d <- te_data()
  t <- suppressMessages(tab_reg(d, "married", c("race", "rincome"), family = "poisson",
                                empirical = TRUE))              # no `color = "adjustment"`
  e <- est(t)
  expect_true(any(is.finite(e$gap_se)))
  expect_true(any(is.finite(e$gap_lo) & is.finite(e$gap_hi)))
})


# --- colour -----------------------------------------------------------------------------------------

test_that("the colours are the table's own slots and hexes", {
  d <- te_data()
  t <- tab(d, race, party3, pct = "row", ci = "diff", color = TRUE,
           color_signif = "grey_non_signif")
  e <- est(t, theme = "light")
  for (nm in unique(e$column)) {
    cd <- fmt_channel_codes(t[[nm]], "light")
    s  <- e[e$column == nm, , drop = FALSE]
    # the colour is the EXPORTERS' resolved one (fmt_col_ann): the cell's hex where it has one, the
    # theme's grey where it does not -- not the raw channel code, which is NA there.
    an <- tabxplor:::fmt_col_ann(t[[nm]], list(theme = "light", text = "#000000",
                                               grey = "#9f9f9f", grey2 = "#111111"))
    expect_identical(s$slot_text, cd$text_slot[s$row])
    expect_identical(s$hex_text,  an$font[s$row])
    expect_identical(s$slot_bg,   cd$bg_slot[s$row])
  }
  expect_true(any(e$slot_text > 0L))                          # never vacuous
  expect_identical(unique(e$measure), "diff")
  expect_identical(unique(e$policy),  "grey_non_signif")
  # the print palette speaks through the FACE, which the model carries too
  ep <- est(t, theme = "print")
  expect_true(any(ep$bold) || any(ep$italic))
})

test_that("the gap slot comes from whichever channel carries the gap measure", {
  d <- te_data()
  t <- suppressMessages(tab_reg(d, "married", c("race", "rincome"), family = "poisson",
                                empirical = TRUE, color = c("OR", "adjustment")))
  e <- est(t)
  cd <- fmt_channel_codes(t[["Model_RR"]], tabxplor:::tx_plot_colors(NULL)$theme)
  expect_identical(e$gap_slot, cd$bg_slot[e$row])             # `adjustment` rides the background here
  # with no gap measure at all there is no gap slot
  t2 <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", empirical = TRUE))
  expect_true(all(is.na(est(t2)$gap_slot)))
})


# --- what = -----------------------------------------------------------------------------------------

test_that("what = 'level' pairs the observed and adjusted percentages", {
  skip_if_not_installed("marginaleffects")
  d <- te_data()
  t <- suppressMessages(tab_reg(d, "married", c("race", "rincome"), family = "binomial",
                                effect = "ame", empirical = TRUE))
  e <- est(t, what = "level")
  expect_setequal(unique(e$series), c("observed", "modelled"))
  expect_identical(unique(e$kind), "level")
  expect_length(levels(e$facet), 1L)                          # ONE panel: the pairing is the point
  expect_true(all(is.finite(e$estimate)))
  expect_true(all(is.na(e$ci_inf)))                           # no interval stored on that scale
  # exactly one crude column is taken -- Obs_% and Obs_diff are field-identical, `display` separates
  expect_length(unique(e$column[e$series == "observed"]), 1L)
})

test_that("what = 'effect' on a ci = 'cell' table keeps the point and drops the whisker", {
  d <- te_data()
  t <- tab(d, race, party3, pct = "row", ci = "cell")
  e <- est(t, what = "effect")
  expect_identical(unique(e$scale_key), "points")
  for (nm in unique(e$column)) {
    s <- e[e$column == nm, , drop = FALSE]
    expect_equal(s$estimate, get_diff(t[[nm]])[s$row], tolerance = 0)
  }
  expect_true(all(is.na(e$ci_inf)))
  expect_equal(unique(e$neutral), 0)
})
