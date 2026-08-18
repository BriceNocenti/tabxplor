# PURPOSE: Phase 18z5 -- the `obs` fmt field and the two tab_reg colour measures that read it,
#          `color = "adjustment"` (a model effect vs its OBSERVED/crude counterpart) and
#          `color = "between_groups"` (a split_var group's effect vs the reference group's).
# ROLE: the behavioural lock. The governing claim is that NOTHING is recomputed: `obs` holds exactly
#       the value the neighbouring Obs_* column stores in its own estimate field, already on the model
#       column's scale -- so these tests compare the two fields directly rather than re-deriving.
# KEY CONSTRAINTS:
#   - `obs` is NA wherever there is no counterpart -> those cells must stay UNCOLOURED, never coloured
#     on a stale value. Phase 18z9 gave numeric predictors one (their univariable fit) and z10 gave
#     the last three families one, so what is left is the Constant, the compound-formula escape hatch,
#     and cross-tables.
#   - The colour SIGN is "away from vs toward the null", not raw up/down: a protective effect
#     (OR < 1) attenuated toward 1 must land on the SAME pole as a risky effect attenuated toward 1.
#     That is the whole reason the score is not est/obs.
#   - `color_signif` applies only where a `gap_se` exists (MEASURES$force_policy, a predicate on the
#     column since Phase 18z8-B). On a conditional odds ratio it never does -- see below and
#     test-adjustment-gap.R, which owns the significance half.
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
  chk(tab_reg(d, outcome = "married", predictors = c("race", "party3"),
              family = "binomial", empirical = TRUE), "Model_OR", "Obs_OR", get_or)
  chk(suppressMessages(tab_reg(d, outcome = "married", predictors = c("race", "party3"),
                               family = "poisson", empirical = TRUE)), "Model_RR", "Obs_RR", get_or)
  chk(suppressWarnings(tab_reg(d, outcome = "tvhours", predictors = c("race", "party3"),
                               family = "poisson", empirical = TRUE)),   # tvhours is over-dispersed
      "Model_IRR", "Obs_IRR", get_or)
  chk(tab_reg(d, outcome = "age", predictors = c("race", "party3"),
              family = "gaussian", empirical = TRUE), "Model_\u03b2", "Obs_diff", get_diff)
  chk(tab_reg(d, outcome = "married", predictors = c("race", "party3"), family = "binomial",
              measure = "log", empirical = TRUE), "Model_\u03b2", "Obs_log(OR)", get_diff)
  t <- tab_reg(d, outcome = "married", predictors = c("race", "party3"),
               family = "binomial", effect = "marginal", empirical = TRUE)
  chk(t, grep("^Model_", names(t), value = TRUE)[[1]], "Obs_diff", get_diff)
  t <- tab_reg(d, outcome = "married", predictors = c("race", "party3"),
               family = "binomial", effect = "marginal", measure = "ratio", empirical = TRUE)
  chk(t, grep("^Model_", names(t), value = TRUE)[[1]], "Obs_RR", get_or)
})

test_that("obs is NA (-> uncoloured) wherever there is no crude counterpart", {
  d <- adj_data()
  # The Constant has no crude counterpart. (Phase 18z9: a NUMERIC predictor now HAS one -- its
  # univariable fit -- so it is no longer part of this list; see the next test.)
  t <- tab_reg(d, outcome = "married", predictors = c("race", "age"), family = "binomial",
               empirical = TRUE, color = c(TRUE, "adjustment"))
  o <- get_obs(t$Model_OR)
  testthat::expect_true(is.na(o[[1]]))                              # Constant
  testthat::expect_identical(fmt_color_channels(t$Model_OR)$bg_slot[[1L]], 0L)

  # a compound formula has no predictor structure to be crude about -- the one remaining gap.
  t <- suppressMessages(tab_reg(d, married ~ race * age, family = "binomial",
                                empirical = TRUE, color = c(TRUE, "adjustment")))
  testthat::expect_true(all(is.na(get_obs(t[[ncol(t)]]))))

  # a plain cross-table never fills the field.
  testthat::expect_true(all(is.na(get_obs(tab(d, race, party3, color = TRUE)[[2]]))))
})

test_that("a MULTINOMIAL model gets one obs PER OUTCOME CATEGORY (Phase 18z10)", {
  # z10 inverted this test's premise: the univariable multinomial IS saturated, so its crude OR is the
  # {category j, reference category} x {level, reference level} Woolf ratio -- which is exactly what
  # tab(pct = "row", display = "{or}", ref = "first") prints. Each model column carries its own category's `obs`.
  skip_if_not_installed("nnet")
  d <- adj_data()
  t <- suppressMessages(tab_reg(d, outcome = "party3", predictors = "race",
                                family = "multinomial", empirical = TRUE, cleannames = FALSE))
  mcols <- reg_fmt_cols(t)
  testthat::expect_gt(length(mcols), 1L)
  obs <- lapply(mcols, function(nm) get_obs(t[[nm]]))
  testthat::expect_true(all(vapply(obs, function(o) any(!is.na(o)), logical(1))))
  # the categories really differ -- one shared vector would be the bug this keys against
  testthat::expect_false(isTRUE(all.equal(obs[[1]], obs[[2]])))

  # ... and each equals the crude OR tab() shows for that category
  ct <- tab(d, race, party3, pct = "row", display = "{or}", ref = "first", na = "drop", ref2 = 1)
  lv <- levels(forcats::fct_drop(stats::na.omit(d$race)))
  for (j in seq_along(mcols)) {
    cat_j <- sub(" vs .*$", "", mcols[[j]])
    if (!cat_j %in% names(ct)) next
    got  <- get_obs(t[[mcols[[j]]]])[match(lv, as.character(t$levels))]
    want <- get_or(ct[[cat_j]])[match(lv, as.character(ct[[1]]))]
    testthat::expect_equal(unname(got), unname(want), tolerance = 1e-8)
  }
})

test_that("a NUMERIC predictor gets an obs, and `adjustment` colours it", {
  # Phase 18z9 inverted this test's premise: the univariable fit IS the numeric row's crude twin.
  d <- adj_data()
  t <- tab_reg(d, outcome = "married", predictors = c("race", "age"), family = "binomial",
               empirical = TRUE, color = c(TRUE, "adjustment"))
  i <- which(as.character(t$var) == "age")
  testthat::expect_true(all(!is.na(get_obs(t$Model_OR)[i])))
  testthat::expect_true(all(!is.na(get_or(t$Obs_OR)[i])))
})

test_that("multiplier scales obs by the SAME k as the estimate (SS9 Q6)", {
  # Both columns go through reg_fit(multiplier=), so an OR^k model cell is compared to an OR^k crude
  # one -- the desync this test used to be safe from only because numeric rows had no twin at all.
  d  <- adj_data()
  t1 <- tab_reg(d, outcome = "married", predictors = c("race", "age"), family = "binomial",
                empirical = TRUE, multiplier = c(age = 1))
  t10 <- tab_reg(d, outcome = "married", predictors = c("race", "age"), family = "binomial",
                 empirical = TRUE, multiplier = c(age = 10))
  i <- which(as.character(t1$var) == "age")
  testthat::expect_equal(get_obs(t10$Model_OR)[i], get_obs(t1$Model_OR)[i]^10, tolerance = 1e-8)
  testthat::expect_equal(get_or(t10$Model_OR)[i],  get_or(t1$Model_OR)[i]^10,  tolerance = 1e-8)
  # and the crude column itself is the same k-scaled quantity
  testthat::expect_equal(get_or(t10$Obs_OR)[i], get_obs(t10$Model_OR)[i], tolerance = 1e-12)
})

# --- the direction rule ------------------------------------------------------------------------------
# The bug this prevents: with a raw sign, a protective effect attenuated toward 1 moves UP while a
# risky one attenuated toward 1 moves DOWN, so the two halves of the palette would mean nothing.

test_that("the score is toward/away from the null, not raw up/down", {
  mk <- function(est, obs) fmt(n = rep(1L, length(est)), or = est, obs = obs,
                              scale = "odds_ratio", pct_type = "row", display = "or", ref = "1",
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
             scale = "points", pct_type = "row", display = "diff", color = "adjustment")
  testthat::expect_equal(tabxplor:::fmt_adjustment_score(add), c(0, 0))
})

test_that("an additive effect scores the absolute gap, signed by the null rule", {
  add <- fmt(n = rep(1L, 2), diff = c(0.30, -0.30), obs = c(0.20, -0.20),
             scale = "points", pct_type = "row", display = "diff", color = "adjustment")
  # both moved 0.10 FURTHER from 0 -> same pole, same magnitude (never +0.10 vs -0.10)
  testthat::expect_equal(tabxplor:::fmt_adjustment_score(add), c(0.10, 0.10))
})

# --- reference resolution across the modes -----------------------------------------------------------

test_that("model comparison: every model column is scored against the ONE crude column", {
  d <- adj_data()
  t <- tab_reg(d, outcome = "married",
               predictors = list(m1 = "race", m2 = c("race", "party3")),
               family = "binomial", empirical = TRUE)
  testthat::expect_equal(get_obs(t$m1), get_or(t[["Obs_OR"]]))
  testthat::expect_equal(get_obs(t$m2), get_or(t[["Obs_OR"]]))
})

test_that("several dependents: each fit takes its OWN crude block", {
  d <- adj_data(); d$black <- factor(as.integer(d$race == "Black"), labels = c("no", "yes"))
  t <- tab_reg(d, outcome = c("married", "black"), predictors = "party3",
               family = "binomial", empirical = TRUE)
  testthat::expect_equal(get_obs(t[["Model_OR [married]"]]), get_or(t[["Obs_OR [married]"]]))
  testthat::expect_equal(get_obs(t[["Model_OR [black]"]]),   get_or(t[["Obs_OR [black]"]]))
  # and they are genuinely different outcomes, so the two crude vectors must NOT coincide
  testthat::expect_false(isTRUE(all.equal(get_obs(t[["Model_OR [married]"]]),
                                          get_obs(t[["Model_OR [black]"]]))))
})

# --- between_groups ----------------------------------------------------------------------------------

# Phase 18z8 pinned `color_signif = "ignore"` here: it is the DESCRIPTIVE reading this file locks
# (z5's), and it is now one policy among three -- tab_reg()'s default became grey_non_signif, which
# greys a gap the new test finds non-significant. The policies themselves are tested in
# test-between-groups-gap.R.
test_that("between_groups carries the reference group's estimate, stacked AND spread", {
  d <- adj_data()
  sp <- tab_reg(d, outcome = "married", predictors = "race", tab_vars = "party3",
                family = "binomial", color = c(TRUE, "between_groups"), color_signif = "ignore")
  fmt_cols <- reg_fmt_cols(sp)
  testthat::expect_length(fmt_cols, 3L)                            # one column per group
  ref <- get_or(sp[[fmt_cols[[1]]]])
  testthat::expect_true(all(is.na(get_obs(sp[[fmt_cols[[1]]]]))))  # not compared to itself
  testthat::expect_equal(get_obs(sp[[fmt_cols[[2]]]]), ref)
  testthat::expect_equal(get_obs(sp[[fmt_cols[[3]]]]), ref)
  testthat::expect_identical(fmt_color_channels(sp[[fmt_cols[[1]]]])$bg_slot,
                             integer(length(ref)))                 # the baseline stays uncoloured
  testthat::expect_true(any(fmt_color_channels(sp[[fmt_cols[[3]]]])$bg_slot > 0L))

  # the STACKED shape (several models per group, so no side-by-side layout): each group is a block
  # of rows, and `obs` is filled from the first group's block.
  st <- tab_reg(d, outcome = "married", predictors = list(m1 = "race", m2 = "race"),
                tab_vars = "party3", family = "binomial",
                color = c(TRUE, "between_groups"), color_signif = "ignore")
  col <- st[[reg_fmt_cols(st)[[1]]]]
  k   <- length(ref)
  testthat::expect_true(all(is.na(get_obs(col)[seq_len(k)])))      # first group's block
  testthat::expect_equal(get_obs(col)[k + seq_len(k)], ref)        # second group's block
})

test_that("between_groups is off by default and needs no empirical companion", {
  d <- adj_data()
  t <- tab_reg(d, outcome = "married", predictors = "race", tab_vars = "party3",
               family = "binomial")                                 # no `color` -> auto
  testthat::expect_true(all(vapply(t[reg_fmt_cols(t)],
                                   function(c) all(is.na(get_obs(c))), logical(1))))
})

# --- the API boundary --------------------------------------------------------------------------------

test_that("color = 'adjustment' turns empirical on, and the two measures are exclusive", {
  d <- adj_data()
  testthat::expect_message(
    t <- tab_reg(d, outcome = "married", predictors = c("race", "party3"),
                 family = "binomial", color = c(TRUE, "adjustment")),
    "empirical")
  testthat::expect_true("Obs_OR" %in% names(t))
  testthat::expect_error(
    tab_reg(d, outcome = "married", predictors = "race", family = "binomial",
            color = c("adjustment", "between_groups")),
    "cannot be used together")
  # tab() names them rather than emitting a bare "unknown measure"
  testthat::expect_error(tab(d, race, party3, color = "adjustment"), "tab_reg")
})

# Phase 18z8-B: `force_policy` is a PREDICATE ON THE COLUMN for both gap measures -- a gap measure
# reads under `ignore` exactly where no `gap_se` was written. On a CONDITIONAL ODDS RATIO that is by
# design (maintainer ruling Q1(b): the gap is part non-collapsibility, so the test would read
# "significant" everywhere); on a collapsible estimand the policy applies normally -- see
# test-adjustment-gap.R for that half.
test_that("color_signif does not apply to an odds-ratio `adjustment` gap: it reads under `ignore`", {
  d <- adj_data()
  testthat::expect_message(
    t <- tab_reg(d, outcome = "married", predictors = c("race", "party3"), family = "binomial",
                 empirical = TRUE, color = c(TRUE, "adjustment"),
                 color_signif = "guaranteed_effect"),
    "color_signif")
  testthat::expect_true(all(is.na(get_gap_se(t$Model_OR))))    # the reason it reads under `ignore`
  pl <- tabxplor:::fmt_color_plan(t$Model_OR, "bg", color = get_color_bg(t$Model_OR))
  testthat::expect_identical(pl$policy, "ignore")
  testthat::expect_identical(pl$measure, "adjustment")
  # the TEXT channel keeps the user's policy -- the neutralisation is per measure, not per column
  testthat::expect_identical(
    tabxplor:::fmt_color_plan(t$Model_OR, "text", color = get_color(t$Model_OR))$policy,
    "guaranteed_effect")
  # ... while `between_groups` now HONOURS the policy (its gap has a test of its own)
  b  <- suppressMessages(tab_reg(d, outcome = "married", predictors = "race", tab_vars = "party3",
                                 family = "binomial", color = c(TRUE, "between_groups"),
                                 color_signif = "guaranteed_effect"))
  bc <- b[[reg_fmt_cols(b)[[2]]]]
  testthat::expect_identical(
    tabxplor:::fmt_color_plan(bc, "bg", color = get_color_bg(bc))$policy, "guaranteed_effect")
})

test_that("the legend names each channel's own baseline, and warns only on a non-collapsible scale", {
  skip_if_no_gettext <- get0("skip_if_no_gettext", ifnotfound = function() invisible(NULL))
  d <- adj_data()
  leg <- function(t) tab_color_legend(t, medium = "plain", style = "prose", lang = "en")
  t <- tab_reg(d, outcome = "married", predictors = c("race", "party3"), family = "binomial",
               empirical = TRUE, color = c(TRUE, "adjustment"))
  l <- leg(t)
  testthat::expect_true(any(grepl("observed (crude) effect", l, fixed = TRUE)))
  testthat::expect_true(any(grepl("non-collapsibility", l, fixed = TRUE)))
  # a COLLAPSIBLE estimand earns no caveat -- that contrast is the point of the sentence
  t2 <- suppressMessages(tab_reg(d, outcome = "married", predictors = c("race", "party3"),
                                 family = "poisson", empirical = TRUE, color = c(TRUE, "adjustment")))
  testthat::expect_false(any(grepl("non-collapsibility", leg(t2), fixed = TRUE)))
  # and the group measure names ITS baseline, not the observed effect
  t3 <- tab_reg(d, outcome = "married", predictors = "race", tab_vars = "party3",
                family = "binomial", color = c(TRUE, "between_groups"))
  testthat::expect_true(any(grepl("reference group", leg(t3), fixed = TRUE)))
})

# --- the {obs} display token -------------------------------------------------------------------------

test_that("{obs} renders bare and in a composite, and round-trips through get_num/set_num", {
  d <- adj_data()
  t <- tab_reg(d, outcome = "married", predictors = c("race", "party3"),
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
  a <- tab_reg(d, outcome = "married", predictors = c("race", "party3"), family = "binomial",
               effect = "marginal", empirical = TRUE)
  ac <- a[[grep("^Model_", names(a), value = TRUE)[[1]]]]
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
  x <- tab_reg(d, outcome = "married", predictors = c("race", "party3"),
               family = "binomial", empirical = TRUE)$Model_OR
  ok <- !is.na(get_obs(x))
  testthat::expect_equal(format(set_display(x, "{or} ({obs})"))[ok],
                         format(set_display(set_pvalue(x, NA_real_), "{or} ({obs})"))[ok])
  testthat::expect_true(all(grepl("(", format(set_display(x, "{or} ({obs})"))[ok], fixed = TRUE)))
})

test_that("the tooltip carries the comparison value once, and never on a cross-table", {
  d <- adj_data()
  t <- tab_reg(d, outcome = "married", predictors = c("race", "party3"),
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
  t <- tab_reg(d, outcome = "married", predictors = c("race", "party3"), family = "binomial",
               empirical = TRUE, color = c(TRUE, "adjustment"))
  testthat::expect_true(any(grepl("*", format(t$Model_OR, stars = TRUE), fixed = TRUE)))
  # the reference row keeps its bold anchor when the measure rides the TEXT channel
  t2 <- tab_reg(d, outcome = "married", predictors = c("race", "party3"), family = "binomial",
                empirical = TRUE, color = "adjustment")
  testthat::expect_true(any(is_refrow(t2$Model_OR)))
  testthat::expect_identical(fmt_color_channels(t2$Model_OR)$text_slot[is_refrow(t2$Model_OR)],
                             integer(sum(is_refrow(t2$Model_OR))))   # a baseline is never coloured
})

# --- exports still build ------------------------------------------------------------------------------

test_that("every exporter renders an adjustment-coloured table without error", {
  d <- adj_data()
  t <- tab_reg(d, outcome = "married", predictors = c("race", "party3"), family = "binomial",
               empirical = TRUE, color = c(TRUE, "adjustment"))
  testthat::expect_no_error(format(t$Model_OR))
  testthat::expect_no_error(tab_md(t))
  testthat::expect_no_error(tab_html(t))
  testthat::expect_no_error(print(t))
  skip_if_not_installed("openxlsx2")
  testthat::expect_no_error(tab_xl(t, path = withr::local_tempfile(fileext = ".xlsx"), open = FALSE))
})

# --- Phase 18z13 (D2 / D4): the gap ladder reads the ESTIMATE's own scale -------------------------

test_that("D2: the additive gap is unit-invariant (hours / minutes / days colour identically)", {
  skip_if_not_installed("broom")
  d <- adj_data()
  d$tv_hr  <- d$tvhours
  d$tv_min <- d$tvhours * 60
  d$tv_day <- d$tvhours / 24
  slots <- function(v) {
    t  <- suppressMessages(suppressWarnings(
      tab_reg(d, v, c("race", "party3"), family = "gaussian",
              empirical = TRUE, color = c(TRUE, "adjustment"), cleannames = FALSE)))
    mc <- grep("^Model_", names(t), value = TRUE)[[1]]
    fmt_color_channels(t[[mc]])$bg_slot
  }
  s_hr <- slots("tv_hr")
  testthat::expect_true(any(s_hr > 0L))                    # the fixture must actually colour something
  # z5 scored the raw difference against an ABSOLUTE ladder calibrated for percentage points, so the
  # same substantive adjustment saturated in minutes and vanished in days. Standardized by SD(Y), the
  # reading no longer depends on the unit the outcome happens to be recorded in.
  testthat::expect_identical(slots("tv_min"), s_hr)
  testthat::expect_identical(slots("tv_day"), s_hr)
})

test_that("D4: the gap's break glyphs follow the selected scale, not the measure", {
  skip_if_not_installed("broom")
  d   <- adj_data()
  leg <- function(t) paste(tab_color_legend(t, medium = "plain", style = "terse"), collapse = " | ")

  # multiplicative estimate -> a multiplicative ladder
  t_mult <- suppressMessages(tab_reg(d, "married", c("race", "party3"), family = "poisson",
                                     empirical = TRUE, color = c(TRUE, "adjustment"),
                                     cleannames = FALSE))
  l_mult <- leg(t_mult)
  testthat::expect_match(l_mult, "\u00d71.1", fixed = TRUE)   # x1.1
  testthat::expect_match(l_mult, "\u00f71.1", fixed = TRUE)   # div 1.1

  # additive estimate -> a signed ladder in the outcome's SD, never "x0.05"
  t_add <- suppressMessages(suppressWarnings(
    tab_reg(d, "tvhours", c("race", "party3"), family = "gaussian",
            empirical = TRUE, color = c(TRUE, "adjustment"), cleannames = FALSE)))
  # "+0.05" can only come from the gap ladder: `diff`'s own standardized breaks are 0.2/0.5/0.8.
  l_add <- leg(t_add)
  testthat::expect_match(l_add, "+0.05", fixed = TRUE)
  testthat::expect_match(l_add, "-0.05", fixed = TRUE)
  testthat::expect_false(grepl("\u00d70.05", l_add, fixed = TRUE))  # the z5 rendering of "+0.05"
})
