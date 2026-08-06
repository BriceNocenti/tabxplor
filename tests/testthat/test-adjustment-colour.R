# PURPOSE: Last Phase z5 -- the `obs` fmt field and the two tab_reg colour measures that read it,
#          `color = "adjustment"` (a model effect vs its OBSERVED/crude counterpart) and
#          `color = "between_groups"` (a split_var group's effect vs the reference group's).
# ROLE: the behavioural lock. The governing claim is that NOTHING is recomputed: `obs` holds exactly
#       the value the neighbouring Obs_* column stores in its own estimate field, already on the model
#       column's scale -- so these tests compare the two fields directly rather than re-deriving.
# KEY CONSTRAINTS:
#   - `obs` is NA wherever there is no counterpart (Constant, numeric predictors, multinomial,
#     ordinal, cross-tables) -> those cells must stay UNCOLOURED, never coloured on a stale value.
#   - The colour SIGN is "away from vs toward the null", not raw up/down: a protective effect
#     (OR < 1) attenuated toward 1 must land on the SAME pole as a risky effect attenuated toward 1.
#     That is the whole reason the score is not est/obs.
#   - `color_signif` does not apply (MEASURES$force_policy = "ignore"): phase 1 is descriptive.
# See: dev/model_vs_observed_effect_colour.md (SS3 collapsibility, SS4 significance, SS7 the engine).

adj_data <- function() {
  d <- forcats::gss_cat
  d$race    <- forcats::fct_drop(d$race)
  d$married <- factor(as.integer(d$marital == "Married"), labels = c("no", "yes"))
  d$party3  <- factor(dplyr::case_when(grepl("dem", d$partyid, ignore.case = TRUE) ~ "Dem",
                                       grepl("rep", d$partyid, ignore.case = TRUE) ~ "Rep",
                                       TRUE ~ "Ind"),
                      levels = c("Ind", "Dem", "Rep"))
  d$inc3    <- factor(dplyr::case_when(d$rincome %in% c("$25000 or more") ~ "hi",
                                       d$rincome %in% c("Not applicable", "No answer",
                                                        "Don't know", "Refused") ~ NA_character_,
                                       TRUE ~ "lo"),
                      levels = c("lo", "hi"))
  d <- d[!is.na(d$tvhours) & !is.na(d$race) & !is.na(d$party3), , drop = FALSE]
  tibble::as_tibble(d)
}

# --- the field carries the crude effect, per family -------------------------------------------------
# One claim, seven shapes: get_obs(<model column>) IS the estimate of the Obs_* column beside it. If a
# family ever routed the crude effect through a different field, this catches it.

test_that("obs == the Obs_* effect column, for every family / effect shape", {
  d <- adj_data()
  chk <- function(t, mcol, ocol, getter) {
    testthat::expect_true(all(!is.na(get_obs(t[[mcol]])[-1])))      # -1 = the Constant row
    testthat::expect_equal(get_obs(t[[mcol]]), getter(t[[ocol]]))
  }
  chk(tab_reg(d, dependent = "married", predictors = c("race", "party3"),
              family = "binomial", empirical = TRUE), "Model_OR", "Obs_OR", get_or)
  chk(suppressMessages(tab_reg(d, dependent = "married", predictors = c("race", "party3"),
                               family = "poisson", empirical = TRUE)), "Model_RR", "Obs_RR", get_or)
  chk(suppressWarnings(tab_reg(d, dependent = "tvhours", predictors = c("race", "party3"),
                               family = "poisson", empirical = TRUE)),   # tvhours is over-dispersed
      "Model_IRR", "Obs_IRR", get_or)
  chk(tab_reg(d, dependent = "age", predictors = c("race", "party3"),
              family = "gaussian", empirical = TRUE), "Model_\u03b2", "Obs_diff", get_diff)
  chk(tab_reg(d, dependent = "married", predictors = c("race", "party3"), family = "binomial",
              exponentiate = FALSE, empirical = TRUE), "Model_\u03b2", "Obs_log(OR)", get_diff)
  t <- tab_reg(d, dependent = "married", predictors = c("race", "party3"),
               family = "binomial", effect = "ame", empirical = TRUE)
  chk(t, names(t)[[5]], "Obs_diff", get_diff)
  t <- tab_reg(d, dependent = "married", predictors = c("race", "party3"),
               family = "binomial", effect = "ame_ratio", empirical = TRUE)
  chk(t, names(t)[[5]], "Obs_RR", get_or)
})

test_that("obs is NA (-> uncoloured) wherever there is no crude counterpart", {
  d <- adj_data()
  # Constant + a NUMERIC predictor: reg_empirical only covers factor predictors.
  t <- tab_reg(d, dependent = "married", predictors = c("race", "age"), family = "binomial",
               empirical = TRUE, color = c("OR", "adjustment"))
  o <- get_obs(t$Model_OR)
  testthat::expect_true(is.na(o[[1]]))                              # Constant
  testthat::expect_true(is.na(o[[length(o)]]))                      # the numeric predictor's row
  testthat::expect_identical(fmt_color_channels(t$Model_OR)$bg_slot[c(1L, length(o))], c(0L, 0L))

  # multinomial: crude companions are tooltip-only, so there is nothing to compare to.
  t <- tab_reg(d, dependent = "party3", predictors = "race", family = "multinomial", empirical = TRUE)
  testthat::expect_true(all(is.na(get_obs(t[[3]]))))

  # a plain cross-table never fills the field.
  testthat::expect_true(all(is.na(get_obs(tab(d, race, party3, color = TRUE)[[2]]))))
})

test_that("multiplier cannot desync obs (it only scales numeric predictors, which have no twin)", {
  d <- adj_data()
  t <- tab_reg(d, dependent = "married", predictors = c("race", "age"), family = "binomial",
               empirical = TRUE, multiplier = c(age = 10))
  i <- which(as.character(t$var) == "age")
  testthat::expect_true(all(is.na(get_obs(t$Model_OR)[i])))
})

# --- the direction rule ------------------------------------------------------------------------------
# The bug this prevents: with a raw sign, a protective effect attenuated toward 1 moves UP while a
# risky one attenuated toward 1 moves DOWN, so the two halves of the palette would mean nothing.

test_that("the score is toward/away from the null, not raw up/down", {
  mk <- function(est, obs) fmt(n = rep(1L, length(est)), or = est, obs = obs,
                              type = "row", display = "or", ci_type = "or", ref = "1",
                              color = "adjustment")
  # both ATTENUATED by the same factor 1.2, one protective one risky -> same side, same magnitude
  s <- tabxplor:::fmt_adjustment_score(mk(c(0.5 * 1.2, 2 / 1.2), c(0.5, 2)))
  testthat::expect_equal(s[[1]], s[[2]])
  testthat::expect_lt(s[[1]], 1)                                   # attenuated = the under side
  # both STRENGTHENED -> the over side
  s <- tabxplor:::fmt_adjustment_score(mk(c(0.5 / 1.2, 2 * 1.2), c(0.5, 2)))
  testthat::expect_equal(s[[1]], s[[2]])
  testthat::expect_gt(s[[1]], 1)
  # crossing the null still reads as strengthened (|log 1.2| > |log 0.9|)
  testthat::expect_gt(tabxplor:::fmt_adjustment_score(mk(1.2, 0.9)), 1)
  # equal estimates are neutral, whatever the scale
  testthat::expect_equal(tabxplor:::fmt_adjustment_score(mk(2, 2)), 1)
  add <- fmt(n = c(1L, 1L), diff = c(0.1, -0.1), obs = c(0.1, -0.1),
             type = "row", display = "diff", ci_type = "diff", color = "adjustment")
  testthat::expect_equal(tabxplor:::fmt_adjustment_score(add), c(0, 0))
})

test_that("an additive effect scores the absolute gap, signed by the null rule", {
  add <- fmt(n = rep(1L, 2), diff = c(0.30, -0.30), obs = c(0.20, -0.20),
             type = "row", display = "diff", ci_type = "diff", color = "adjustment")
  # both moved 0.10 FURTHER from 0 -> same pole, same magnitude (never +0.10 vs -0.10)
  testthat::expect_equal(tabxplor:::fmt_adjustment_score(add), c(0.10, 0.10))
})

# --- reference resolution across the modes -----------------------------------------------------------

test_that("model comparison: every model column is scored against the ONE crude column", {
  d <- adj_data()
  t <- tab_reg(d, dependent = "married",
               predictors = list(m1 = "race", m2 = c("race", "party3")),
               family = "binomial", empirical = TRUE)
  testthat::expect_equal(get_obs(t$m1), get_or(t[["Obs_OR"]]))
  testthat::expect_equal(get_obs(t$m2), get_or(t[["Obs_OR"]]))
})

test_that("several dependents: each fit takes its OWN crude block", {
  d <- adj_data(); d$black <- factor(as.integer(d$race == "Black"), labels = c("no", "yes"))
  t <- tab_reg(d, dependent = c("married", "black"), predictors = "party3",
               family = "binomial", empirical = TRUE)
  testthat::expect_equal(get_obs(t[["Model_OR [married]"]]), get_or(t[["Obs_OR [married]"]]))
  testthat::expect_equal(get_obs(t[["Model_OR [black]"]]),   get_or(t[["Obs_OR [black]"]]))
  # and they are genuinely different outcomes, so the two crude vectors must NOT coincide
  testthat::expect_false(isTRUE(all.equal(get_obs(t[["Model_OR [married]"]]),
                                          get_obs(t[["Model_OR [black]"]]))))
})

# --- between_groups ----------------------------------------------------------------------------------

# Last Phase z8 pinned `color_signif = "ignore"` here: it is the DESCRIPTIVE reading this file locks
# (z5's), and it is now one policy among three -- tab_reg()'s default became grey_non_signif, which
# greys a gap the new test finds non-significant. The policies themselves are tested in
# test-between-groups-gap.R.
test_that("between_groups carries the reference group's estimate, stacked AND spread", {
  d <- adj_data()
  sp <- tab_reg(d, dependent = "married", predictors = "race", split_var = "party3",
                family = "binomial", color = c("OR", "between_groups"), color_signif = "ignore")
  fmt_cols <- names(sp)[vapply(sp, is_fmt, logical(1))]
  testthat::expect_length(fmt_cols, 3L)                            # one column per group
  ref <- get_or(sp[[fmt_cols[[1]]]])
  testthat::expect_true(all(is.na(get_obs(sp[[fmt_cols[[1]]]]))))  # not compared to itself
  testthat::expect_equal(get_obs(sp[[fmt_cols[[2]]]]), ref)
  testthat::expect_equal(get_obs(sp[[fmt_cols[[3]]]]), ref)
  testthat::expect_identical(fmt_color_channels(sp[[fmt_cols[[1]]]])$bg_slot,
                             integer(length(ref)))                 # the baseline stays uncoloured
  testthat::expect_true(any(fmt_color_channels(sp[[fmt_cols[[3]]]])$bg_slot > 0L))

  st <- tab_reg(d, dependent = "married", predictors = "race", split_var = "party3",
                family = "binomial", color = c("OR", "between_groups"), color_signif = "ignore",
                spread_models = FALSE)
  col <- st[[names(st)[vapply(st, is_fmt, logical(1))][[1]]]]
  k   <- length(ref)
  testthat::expect_true(all(is.na(get_obs(col)[seq_len(k)])))      # first group's block
  testthat::expect_equal(get_obs(col)[k + seq_len(k)], ref)        # second group's block
})

test_that("between_groups is off by default and needs no empirical companion", {
  d <- adj_data()
  t <- tab_reg(d, dependent = "married", predictors = "race", split_var = "party3",
               family = "binomial")                                 # no `color` -> auto
  testthat::expect_true(all(vapply(t[vapply(t, is_fmt, logical(1))],
                                   function(c) all(is.na(get_obs(c))), logical(1))))
})

# --- the API boundary --------------------------------------------------------------------------------

test_that("color = 'adjustment' turns empirical on, and the two measures are exclusive", {
  d <- adj_data()
  testthat::expect_message(
    t <- tab_reg(d, dependent = "married", predictors = c("race", "party3"),
                 family = "binomial", color = c("OR", "adjustment")),
    "empirical")
  testthat::expect_true("Obs_OR" %in% names(t))
  testthat::expect_error(
    tab_reg(d, dependent = "married", predictors = "race", family = "binomial",
            color = c("adjustment", "between_groups")),
    "cannot be used together")
  # tab() names them rather than emitting a bare "unknown measure"
  testthat::expect_error(tab(d, race, party3, color = "adjustment"), "tab_reg")
})

# Last Phase z8: `adjustment` alone keeps force_policy -- its two estimates share the same rows, so
# the gap SE needs influence functions (phase B). `between_groups` lost it (disjoint groups).
test_that("color_signif does not apply to `adjustment`: it always reads under `ignore`", {
  d <- adj_data()
  testthat::expect_message(
    t <- tab_reg(d, dependent = "married", predictors = c("race", "party3"), family = "binomial",
                 empirical = TRUE, color = c("OR", "adjustment"),
                 color_signif = "guaranteed_effect"),
    "color_signif")
  pl <- tabxplor:::fmt_color_plan(t$Model_OR, "bg", color = get_color_bg(t$Model_OR))
  testthat::expect_identical(pl$policy, "ignore")
  testthat::expect_identical(pl$measure, "adjustment")
  # the TEXT channel keeps the user's policy -- the neutralisation is per measure, not per column
  testthat::expect_identical(
    tabxplor:::fmt_color_plan(t$Model_OR, "text", color = get_color(t$Model_OR))$policy,
    "guaranteed_effect")
  # ... while `between_groups` now HONOURS the policy (its gap has a test of its own)
  b  <- suppressMessages(tab_reg(d, dependent = "married", predictors = "race", split_var = "party3",
                                 family = "binomial", color = c("OR", "between_groups"),
                                 color_signif = "guaranteed_effect"))
  bc <- b[[names(b)[vapply(b, is_fmt, logical(1))][[2]]]]
  testthat::expect_identical(
    tabxplor:::fmt_color_plan(bc, "bg", color = get_color_bg(bc))$policy, "guaranteed_effect")
})

test_that("the legend names each channel's own baseline, and warns only on a non-collapsible scale", {
  skip_if_no_gettext <- get0("skip_if_no_gettext", ifnotfound = function() invisible(NULL))
  d <- adj_data()
  leg <- function(t) tab_color_legend(t, medium = "plain", style = "prose", lang = "en")
  t <- tab_reg(d, dependent = "married", predictors = c("race", "party3"), family = "binomial",
               empirical = TRUE, color = c("OR", "adjustment"))
  l <- leg(t)
  testthat::expect_true(any(grepl("observed (crude) effect", l, fixed = TRUE)))
  testthat::expect_true(any(grepl("non-collapsibility", l, fixed = TRUE)))
  # a COLLAPSIBLE estimand earns no caveat -- that contrast is the point of the sentence
  t2 <- suppressMessages(tab_reg(d, dependent = "married", predictors = c("race", "party3"),
                                 family = "poisson", empirical = TRUE, color = c("OR", "adjustment")))
  testthat::expect_false(any(grepl("non-collapsibility", leg(t2), fixed = TRUE)))
  # and the group measure names ITS baseline, not the observed effect
  t3 <- tab_reg(d, dependent = "married", predictors = "race", split_var = "party3",
                family = "binomial", color = c("OR", "between_groups"))
  testthat::expect_true(any(grepl("reference group", leg(t3), fixed = TRUE)))
})

# --- the {obs} display token -------------------------------------------------------------------------

test_that("{obs} renders bare and in a composite, and round-trips through get_num/set_num", {
  d <- adj_data()
  t <- tab_reg(d, dependent = "married", predictors = c("race", "party3"),
               family = "binomial", empirical = TRUE)
  x <- t$Model_OR
  bare <- format(set_display(x, "obs"))
  testthat::expect_true(any(grepl("^\\s*\\d+\\.\\d{2}$", bare)))    # an OR-scale number, 2 decimals
  comp <- format(set_display(x, "{or} (obs {obs})"))
  testthat::expect_true(all(grepl("(obs ", comp[!is.na(get_obs(x))], fixed = TRUE)))
  testthat::expect_equal(get_num(set_display(x, "obs")), get_obs(x))
  v <- seq_along(x) + 0
  testthat::expect_equal(get_obs(set_num(set_display(x, "obs"), v)), v)
  # Excel shows the PRIMARY token only, and its code matches the OR mask (bare, 2 decimals)
  testthat::expect_equal(unique(format(set_display(x, "obs"), syntax = "excel")), "#,##0.00")
  # an AME column's obs is a probability difference -> x100, signed, "%" (both media agree)
  a <- tab_reg(d, dependent = "married", predictors = c("race", "party3"), family = "binomial",
               effect = "ame", empirical = TRUE)
  ac <- a[[names(a)[[5]]]]
  testthat::expect_true(any(grepl("%$", format(set_display(ac, "obs")))))
  testthat::expect_true(any(grepl("^\\+", stringi::stri_trim(format(set_display(ac, "obs"))))))
  aok <- !is.na(get_obs(ac))            # an empty cell writes no number, so its code is irrelevant
  testthat::expect_true(all(grepl("%", format(set_display(ac, "obs"), syntax = "excel")[aok])))
})

test_that("the composite needs no set_pvalue exception (unlike the derived resid token)", {
  # A composite blanks the p-value of every NON-primary token so stars ride the primary. `resid` needed
  # an exception because it is DERIVED from that p-value; `obs` is a stored field, so it must survive
  # untouched -- if it did not, the whole template would silently collapse to the bare primary.
  d <- adj_data()
  x <- tab_reg(d, dependent = "married", predictors = c("race", "party3"),
               family = "binomial", empirical = TRUE)$Model_OR
  ok <- !is.na(get_obs(x))
  testthat::expect_equal(format(set_display(x, "{or} ({obs})"))[ok],
                         format(set_display(set_pvalue(x, NA_real_), "{or} ({obs})"))[ok])
  testthat::expect_true(all(grepl("(", format(set_display(x, "{or} ({obs})"))[ok], fixed = TRUE)))
})

test_that("the tooltip carries the comparison value once, and never on a cross-table", {
  d <- adj_data()
  t <- tab_reg(d, dependent = "married", predictors = c("race", "party3"),
               family = "binomial", empirical = TRUE)
  tip <- tabxplor:::tab_kable_print_tooltip(t$Model_OR)
  ok  <- !is.na(get_obs(t$Model_OR))
  testthat::expect_true(all(grepl("obs: ", tip[ok], fixed = TRUE)))
  testthat::expect_equal(lengths(regmatches(tip[ok], gregexpr("obs: ", tip[ok], fixed = TRUE))),
                         rep(1L, sum(ok)))
  testthat::expect_false(any(grepl("obs: ", tip[!ok], fixed = TRUE)))
  # a cross-table has no `obs` -> the fragment never appears (the render snapshots must not move)
  ct <- tab(d, race, party3, color = TRUE)
  testthat::expect_false(any(grepl("obs: ", tabxplor:::tab_kable_print_tooltip(ct[[2]]), fixed = TRUE)))
})

test_that("stars still ride the model estimate under color = 'adjustment'", {
  d <- adj_data()
  t <- tab_reg(d, dependent = "married", predictors = c("race", "party3"), family = "binomial",
               empirical = TRUE, color = c("OR", "adjustment"))
  testthat::expect_true(any(grepl("*", format(t$Model_OR, stars = TRUE), fixed = TRUE)))
  # the reference row keeps its bold anchor when the measure rides the TEXT channel
  t2 <- tab_reg(d, dependent = "married", predictors = c("race", "party3"), family = "binomial",
                empirical = TRUE, color = "adjustment")
  testthat::expect_true(any(is_refrow(t2$Model_OR)))
  testthat::expect_identical(fmt_color_channels(t2$Model_OR)$text_slot[is_refrow(t2$Model_OR)],
                             integer(sum(is_refrow(t2$Model_OR))))   # a baseline is never coloured
})

# --- exports still build ------------------------------------------------------------------------------

test_that("every exporter renders an adjustment-coloured table without error", {
  d <- adj_data()
  t <- tab_reg(d, dependent = "married", predictors = c("race", "party3"), family = "binomial",
               empirical = TRUE, color = c("OR", "adjustment"))
  testthat::expect_no_error(format(t$Model_OR))
  testthat::expect_no_error(tab_md(t))
  testthat::expect_no_error(tab_html(t))
  testthat::expect_no_error(print(t))
  skip_if_not_installed("openxlsx2")
  testthat::expect_no_error(tab_xl(t, path = withr::local_tempfile(fileext = ".xlsx"), open = FALSE))
})
