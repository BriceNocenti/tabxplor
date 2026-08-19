# Phase 14z: the empirical odds ratio in tab() carries a Woolf log-OR Wald interval when a colour
# policy (color_signif) or stars need it, so color_signif works on OR exactly as on diff/ratio.
# The 2x2 is conditional on {level j, ref2 level} x {row i, ref row} -- for 3+ levels the "j vs ref2
# baseline" OR (the crude counterpart of the multinomial coefficient).

# small controlled fixture: exact cell counts so the Woolf interval can be hand-computed
or_data <- function() {
  data.frame(
    g = factor(rep(c("a", "b", "c"), each = 100)),
    y = factor(c(rep(c("yes", "no"), c(30, 70)),    # group a: 30 yes / 70 no
                 rep(c("yes", "no"), c(50, 50)),    # group b: 50 / 50
                 rep(c("yes", "no"), c(60, 40))),   # group c: 60 / 40
               levels = c("no", "yes"))             # ref2 = 1 -> "no" is the baseline level
  )
}

woolf <- function(a, b, cc, dd, conf = 0.95) {
  lor <- log((a * dd) / (b * cc)); se <- sqrt(1/a + 1/b + 1/cc + 1/dd); z <- stats::qnorm(1 - (1 - conf) / 2)
  list(or = exp(lor), inf = exp(lor - z * se), sup = exp(lor + z * se),
       p = 2 * stats::pnorm(-abs(lor / se)))
}

test_that("color_signif = 'ignore' (default) leaves the empirical OR without a CI (byte-unchanged)", {
  t <- tab(or_data(), g, y, pct = "row", color = "OR", display = "{or}", ref = "first", ref2 = 1)
  yes <- t[["yes"]]
  expect_false(tabxplor:::fmt_has_interval(yes))
  expect_true(all(is.na(get_ci_inf(yes))))
  expect_true(all(is.na(get_ci_sup(yes))))
  expect_true(all(is.na(get_pvalue(yes))))
})

test_that("a colour policy gives the empirical OR a Woolf interval (matches the closed form)", {
  t   <- tab(or_data(), g, y, pct = "row", color = "OR", display = "{or}", ref = "first", ref2 = 1,
             color_signif = "grey_non_signif")
  yes <- t[["yes"]]
  expect_identical(get_scale(yes), "odds_ratio")

  # group c (row 3) vs the reference row (group a, row 1), on {yes, no=ref2 level}
  w <- woolf(a = 60, b = 40, cc = 30, dd = 70)
  expect_equal(get_or(yes)[3],     w$or,  tolerance = 1e-8)
  expect_equal(get_ci_inf(yes)[3], w$inf, tolerance = 1e-8)
  expect_equal(get_ci_sup(yes)[3], w$sup, tolerance = 1e-8)

  # the reference row carries no interval (OR = 1 by construction)
  expect_true(is.na(get_ci_inf(yes)[1]))            # ref row (group a)
  # Phase 16c: a BINARY col_var references the OTHER level, so BOTH columns carry reciprocal ORs +
  # intervals (no column is forced to "1"); ref2 is ignored. The "no" column is the exact reciprocal
  # of "yes", with reciprocal-swapped bounds; only the reference row stays NA.
  no <- t[["no"]]
  expect_identical(get_scale(no), "odds_ratio")
  expect_equal(get_or(no)[3],     1 / w$or,  tolerance = 1e-8)
  expect_equal(get_ci_inf(no)[3], 1 / w$sup, tolerance = 1e-8)
  expect_equal(get_ci_sup(no)[3], 1 / w$inf, tolerance = 1e-8)
  expect_true(is.na(get_ci_inf(no)[1]))             # ref row still NA
})

test_that("stars = TRUE populates the CI-inversion pvalue (dual of the interval)", {
  t   <- tab(or_data(), g, y, pct = "row", color = "OR", display = "{or}", ref = "first", ref2 = 1,
             color_signif = "grey_non_signif", stars = TRUE)
  yes <- t[["yes"]]
  w   <- woolf(a = 60, b = 40, cc = 30, dd = 70)
  expect_equal(get_pvalue(yes)[3], w$p, tolerance = 1e-8)
  expect_true(is.na(get_pvalue(yes)[1]))            # no p on the reference row
})

test_that("color_signif actually gates the OR colour (greys a big-but-non-significant OR)", {
  # a big observed OR (= 3) on a SMALL sample -> its CI comfortably contains 1 (not significant)
  d <- data.frame(
    g = factor(rep(c("ref", "noisy"), each = 20), levels = c("ref", "noisy")),
    y = factor(c(rep(c("yes", "no"), c(10, 10)),     # ref  : 10 / 10
                 rep(c("yes", "no"), c(15,  5))),     # noisy: 15 /  5  -> OR = 3, n small
               levels = c("no", "yes"))
  )
  slot_ignore <- fmt_color_channels(
    tab(d, g, y, pct = "row", color = "OR", display = "{or}", ref = "first", ref2 = 1)[["yes"]])[[1]][2]
  slot_grey <- fmt_color_channels(
    tab(d, g, y, pct = "row", color = "OR", display = "{or}", ref = "first", ref2 = 1,
        color_signif = "grey_non_signif")[["yes"]])[[1]][2]
  expect_gt(slot_ignore, 0)   # ignore colours the big observed OR
  expect_identical(slot_grey, 0L)   # grey_non_signif greys it (CI contains 1)
})

test_that("3+ level factor: OR of each level vs the ref2 baseline is the conditional 2x2 OR", {
  # y3: levels d1 (baseline via ref2), d2, d3
  set.seed(1)  # NB: only labels the fixture; counts below are deterministic
  d <- data.frame(
    g  = factor(rep(c("ref", "x"), each = 120)),
    y3 = factor(c(rep(c("d1", "d2", "d3"), c(60, 40, 20)),   # group ref
                  rep(c("d1", "d2", "d3"), c(30, 40, 50))),  # group x
                levels = c("d1", "d2", "d3"))
  )
  t <- tab(d, g, y3, pct = "row", color = "OR", display = "{or}", ref = "first", ref2 = 1,
           color_signif = "grey_non_signif")

  # OR of d3 vs d1 (baseline), group x (row 2) vs ref row (group ref, row 1):
  #   conditional 2x2 on {d3, d1}: a = 50, b = 30 (x) ; c = 20, d = 60 (ref)
  w <- woolf(a = 50, b = 30, cc = 20, dd = 60)
  d3 <- t[["d3"]]
  expect_identical(get_scale(d3), "odds_ratio")
  expect_equal(get_or(d3)[2],     w$or,  tolerance = 1e-8)   # = (50*60)/(30*20) = 5
  expect_equal(get_ci_inf(d3)[2], w$inf, tolerance = 1e-8)
  expect_equal(get_ci_sup(d3)[2], w$sup, tolerance = 1e-8)

  # the baseline level (d1 = ref2) column has no interval
  expect_true(all(is.na(get_ci_inf(t[["d1"]]))))
})


# --- Phase 16c: binary-factor odds ratios + the OR total column -----------------------------------

test_that("Phase 16c: a binary col_var references the complement (no column forced to '1')", {
  t   <- tab(or_data(), g, y, pct = "row", color = "OR", display = "{or}", ref = "first", ref2 = 1,
             color_signif = "grey_non_signif")
  yes <- get_or(t[["yes"]]); no <- get_or(t[["no"]])
  # neither level column is a constant OR = 1 (the old forced-ref2 behaviour) ...
  expect_false(all(yes[!is.na(yes)] == 1))
  expect_false(all(no[!is.na(no)]  == 1))
  # ... the two are exact reciprocals, and both carry an OR interval (ref2 is ignored for binary)
  expect_equal(no[!is.na(no)], (1 / yes)[!is.na(yes)], tolerance = 1e-8)
  expect_identical(get_scale(t[["yes"]]), "odds_ratio")
  expect_identical(get_scale(t[["no"]]),  "odds_ratio")
})

test_that("Phase 16c: an OR table's total column shows only the base n, not 100%", {
  t   <- tab(or_data(), g, y, pct = "row", display = "{or}", ref = "first")
  # console/text: the Total cell folds to the base count alone (no "100%" / "{pct}")
  mt  <- tab_materialize_extras(t, backend = "text", pvalue = FALSE)
  d   <- as.character(get_display(mt[["Total"]]))
  expect_true (all(grepl("n_range", d, fixed = TRUE)))
  expect_false(any(grepl("{pct}",   d, fixed = TRUE)))
  # n = "no" drops the meaningless % total column entirely
  t0  <- tab(or_data(), g, y, pct = "row", display = "{or}", ref = "first", n = "no")
  mt0 <- tab_materialize_extras(t0, backend = "text", pvalue = FALSE)
  expect_false("Total" %in% names(mt0))
  # Excel exports only the base-n column, no % total
  mx  <- tab_materialize_extras(t, backend = "xl", pvalue = FALSE)
  expect_false("Total" %in% names(mx))
  expect_true ("n" %in% names(mx))
})
