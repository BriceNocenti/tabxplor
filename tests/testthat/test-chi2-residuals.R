# PURPOSE: Phase 18z4 -- the adjusted standardised (Haberman) residual behind `color = "contrib"`.
#
# What it locks, and why each test exists (every one FAILS on the pre-z4 code):
#   1. The residual IS chisq.test()$stdres. The old gate used the PEARSON residual (o-e)/sqrt(e),
#      whose variance is (1-p_i)(1-p_j) < 1 -- systematically conservative, measured 1.10 to 3.09x
#      too strict on this very table.
#   2. It is invariant to the WEIGHT SCALE. The old one used the weighted N, so multiplying every
#      weight by a constant (population weights) drove every cell p-value to 0.
#   3. `options(tabxplor.design_effect = TRUE)` shrinks it by exactly 1/sqrt(delta-bar) -- Rao-Scott's
#      mean generalized design effect of the table's OWN omnibus test, so the colours and the p of one
#      table describe one design effect (Phase 18z16-iv, W-B).
#   4. The three `color_signif` policies each read a DIFFERENT, documented quantity:
#      ignore / grey_non_signif = the relative contribution (the CA reading, byte-identical to
#      pre-z4); guaranteed_effect = the absolute residual on the `zscore` break scale.
#   5. The `resid` display token + tooltip expose the number, derived from the stored p-value
#      (no fmt field of its own) -- including inside a `{}` composite, whose non-primary tokens
#      have their p-value blanked (which would have rendered NA before the resid exception).
# See: dev/chi2_cell_residuals_and_contributions.md ; CLAUDE.md > Phase 18z4.

# The recoded income used by the vignettes: 4 ordered brackets, so the table matches the worked
# example in dev/chi2_cell_residuals_and_contributions.md (raw gss_cat's rincome keeps its
# "No answer" / "Refused" levels, which dilute the very cell the first defect was found on).
gss_r <- function() {
  d <- gss_cat_data_formatting()
  d[!is.na(d$race) & !is.na(d$rincome), ]
}

# the reference table, computed by base R on exactly the cells tab() keeps (na = "drop")
ref_chisq <- function(d) {
  stats::chisq.test(table(droplevels(d$race), droplevels(d$rincome)), correct = FALSE)
}

testthat::test_that("the contrib residual equals chisq.test()$stdres (adjusted, not Pearson)", {
  d  <- gss_r()
  t1 <- tab(d, race, rincome, color = "contrib", color_signif = "grey_non_signif", na = "drop")
  ct <- ref_chisq(d)

  got <- vapply(colnames(ct$observed), function(cn) fmt_resid(t1[[cn]])[1:3],
                double(3))
  testthat::expect_equal(unname(got), unname(ct$stdres[1:3, ]), tolerance = 1e-8)

  # ... and it is NOT the Pearson residual: the two differ by the 1/sqrt((1-p_i)(1-p_j)) factor that
  # made the old gate conservative -- here up to 3.09x, worst on the large "White" row.
  testthat::expect_gt(max(abs(ct$stdres / ct$residuals)), 3)

  # the stored p-value is that residual's two-sided p (the colour gate reads it)
  testthat::expect_equal(unname(get_pvalue(t1[[colnames(ct$observed)[1]]])[1:3]),
                         unname(2 * stats::pnorm(-abs(ct$stdres[1:3, 1]))), tolerance = 1e-8)
})

testthat::test_that("the cell that the old Pearson gate missed is now significant", {
  # White / $10000-14999: Pearson -1.83 (NOT flagged at 1.96) but adjusted -3.91 (p ~ 9e-5).
  d  <- gss_r()
  ct <- ref_chisq(d)
  i  <- which(rownames(ct$observed) == "White")
  j  <- grep("10000 to", colnames(ct$observed))
  testthat::expect_length(j, 1L)
  testthat::expect_lt(abs(ct$residuals[i, j]), 1.96)   # the old statistic: not significant
  testthat::expect_gt(abs(ct$stdres[i, j]),    3.9)    # the right one: strongly significant

  cn <- colnames(ct$observed)[j]
  t1 <- tab(d, race, rincome, color = "contrib", color_signif = "grey_non_signif", na = "drop")
  testthat::expect_lt(get_pvalue(t1[[cn]])[i], 0.001)   # the gate now sees it (it did not before)

  # ... and this cell is exactly why the ABSOLUTE reading had to exist. It contributes only 0.31x the
  # mean contribution, so the relative scale leaves it uncoloured however significant it is -- while
  # `guaranteed_effect`, which grades |z| itself, colours it (|z| = 3.91 -> the third band).
  testthat::expect_lt(abs(get_ctr(t1[[cn]])[i] / get_mean_contrib(t1[[cn]])[i]), 1)
  testthat::expect_equal(fmt_color_channels(t1[[cn]])$text_slot[i], 0L)

  tg <- tab(d, race, rincome, color = "contrib", color_signif = "guaranteed_effect", na = "drop")
  testthat::expect_gt(fmt_color_channels(tg[[cn]])$text_slot[i], 0L)
})

testthat::test_that("the residual is invariant to the WEIGHT SCALE (population weights)", {
  d <- gss_r()
  set.seed(1)
  d$w1 <- stats::runif(nrow(d), 0.3, 3)
  d$w2 <- d$w1 * 12000                       # population-scale weights
  pv <- function(v) {
    t <- tab(d, race, rincome, wt = !!rlang::sym(v), color = "contrib",
             color_signif = "grey_non_signif", na = "drop")
    get_pvalue(t[["4-$25000 or more"]])[1:3]
  }
  p1 <- pv("w1"); p2 <- pv("w2")
  testthat::expect_equal(p1, p2)
  testthat::expect_true(all(p1 > 0))         # pre-z4 the population-weighted ones were all exactly 0
})

testthat::test_that("design_effect shrinks the residual by exactly 1 / sqrt(delta-bar)", {
  skip_if_not_installed("survey")
  d <- gss_r()
  set.seed(2)
  d$w <- stats::runif(nrow(d), 0.3, 3)
  z_of <- function(on) {
    withr::local_options(list(tabxplor.design_effect = on))
    # pct = "no": the counts table is where `color = "contrib"` is most at home (it is what
    # color = TRUE picks there), and it is the case that used to have no n_eff at all.
    t <- tab(d, race, rincome, wt = w, color = "contrib", color_signif = "grey_non_signif",
             na = "drop", test = TRUE)
    list(z = fmt_resid(t[["4-$25000 or more"]])[1:3], tot = t[["Total"]], test = get_test(t))
  }
  raw <- z_of(FALSE)
  eff <- z_of(TRUE)
  n_tot <- get_n(eff$tot)[length(eff$tot)]
  # Phase 18z16-iv (W-B): the base of an ASSOCIATION residual is the raw n over Rao-Scott's mean
  # generalized design effect of THIS table's own omnibus test -- the `deff` the test row reports.
  # Not the grand cell's `n_eff`: that cell's proportion is 1, so its design variance is 0 and it
  # always fell back to the weights-only B^2/S, at EVERY basis (which is why a stratified design and
  # a flat one used to give residuals identical to the last digit).
  dbar <- eff$test$deff[[1]]
  testthat::expect_true(is.finite(dbar) && dbar > 1)  # unequal weights -> a real design effect
  testthat::expect_equal(eff$test$n[[1]], n_tot)      # `n` is always the RAW count (W8)
  # ONE base for the whole table, so every residual shrinks by the SAME factor. That uniformity is
  # the point: it is what makes a counts table and a percentage table give identical residuals.
  testthat::expect_equal(eff$z / raw$z, rep(1 / sqrt(dbar), 3), tolerance = 1e-8)
  testthat::expect_lt(max(abs(eff$z)), max(abs(raw$z)))   # honestly wider = smaller |z|
  # ...and the same table's cell intervals and its residual now describe ONE design effect: before,
  # the residual's B^2/S and the omnibus test's implied n/delta-bar were two different effective
  # sizes for the same table (measured 2.5 % apart).
  testthat::expect_false(isTRUE(all.equal(
    n_tot / dbar, get_n_eff(eff$tot)[length(eff$tot)], tolerance = 1e-6)))
})

testthat::test_that("the three color_signif policies read three documented quantities", {
  d   <- gss_r()
  mk  <- function(pol, rv = rlang::expr(race))
    tab(d, !!rv, rincome, color = "contrib", color_signif = pol, na = "drop")
  col <- "4-$25000 or more"

  # ignore / grey_non_signif score the RELATIVE contribution (ctr / mean_contrib, NA on a total row)
  ti <- mk("ignore")
  pl <- fmt_color_plan(ti[[col]])
  expect_ctr <- dplyr::if_else(is_totrow(ti[[col]]), NA_real_,
                               get_ctr(ti[[col]]) / get_mean_contrib(ti[[col]]))
  testthat::expect_equal(pl$score, expect_ctr)
  testthat::expect_equal(pl$over_breaks, c(1, 2, 5, 10))
  testthat::expect_equal(fmt_color_plan(mk("grey_non_signif")[[col]])$score, expect_ctr)

  # guaranteed_effect scores the ABSOLUTE residual, on the `zscore` scale anchored at z(conf_level)
  tg <- mk("guaranteed_effect")
  pg <- fmt_color_plan(tg[[col]])
  testthat::expect_equal(pg$score, fmt_resid(tg[[col]]))
  testthat::expect_equal(pg$over_breaks, c(1.96, 2.58, 3.89, 6), tolerance = 1e-4)

  # its first threshold IS the significance threshold, so every significant cell is coloured
  # (the framework invariant, preserved by anchoring the breaks rather than shifting the score)
  fmt_cols <- function(t) names(t)[vapply(t, is_fmt, logical(1))]
  for (nm in fmt_cols(tg)) {
    x   <- tg[[nm]]
    pv  <- get_pvalue(x)
    sig <- !is_totrow(x) & !is.na(pv) & pv < 0.05
    testthat::expect_true(all(fmt_color_channels(x)$text_slot[sig] > 0L))
  }

  # The absolute scale is COMPARABLE ACROSS TABLES -- the property the relative contribution cannot
  # have. Two tables with very different chi2: the slot is a function of |z| alone, so pooling both
  # tables' (band of |z|, slot) pairs must still give one slot per band.
  t2 <- mk("guaranteed_effect", rlang::expr(marital))
  pairs <- do.call(rbind, lapply(list(tg, t2), function(t) do.call(rbind, lapply(fmt_cols(t), function(nm) {
    x  <- t[[nm]]; z <- fmt_resid(x); sl <- fmt_color_channels(x)$text_slot
    ok <- !is.na(z) & !is_totrow(x) & sl > 0L
    if (!any(ok)) return(NULL)
    data.frame(band = findInterval(abs(z[ok]), pg$over_breaks), slot = sl[ok], up = z[ok] > 0)
  }))))
  testthat::expect_gt(nrow(pairs), 20)
  testthat::expect_equal(nrow(unique(pairs)), nrow(unique(pairs[c("band", "up")])))
})

testthat::test_that("`ignore` is byte-identical to the pre-z4 relative contribution", {
  # The CA reading makes no significance claim, so nothing about it changed. Pinned here as well as
  # by the c_contrib* colour goldens: the contribution is still the WEIGHTED share of chi2.
  d  <- gss_r()
  t1 <- tab(d, race, rincome, color = "contrib", na = "drop")
  ct <- ref_chisq(d)
  share <- ct$residuals^2 / sum(ct$residuals^2)
  k     <- length(ct$observed)
  got   <- vapply(colnames(ct$observed),
                  function(cn) (get_ctr(t1[[cn]]) / get_mean_contrib(t1[[cn]]))[1:3], double(3))
  testthat::expect_equal(unname(abs(got)), unname(share[1:3, ] * k), tolerance = 1e-6)
})

testthat::test_that("the `resid` display token renders, bare and in a composite", {
  d  <- gss_r()
  ct <- ref_chisq(d)

  bare <- tab(d, race, rincome, pct = "row", color = "contrib", na = "drop", display = "{resid}")
  txt  <- format(bare[["4-$25000 or more"]])
  testthat::expect_match(trimws(txt[1]), "^\\+11")          # +11.0, signed, 1 decimal
  testthat::expect_match(trimws(txt[2]), "^-8")

  # a composite blanks the p-value on non-primary tokens; `resid` is DERIVED from it, so without the
  # exception in format() the whole template would fall back to the bare primary.
  comp <- tab(d, race, rincome, pct = "row", color = "contrib", na = "drop",
              display = "{pct} ({resid})")
  ctxt <- format(comp[["4-$25000 or more"]])
  testthat::expect_match(trimws(ctxt[1]), "^59% \\( *\\+11")

  # get_num() reads it (it must NOT fall through to the `n` default)
  testthat::expect_equal(get_num(bare[["4-$25000 or more"]])[1:3], ct$stdres[1:3, 4], tolerance = 1e-8,
                         ignore_attr = TRUE)
})

testthat::test_that("the tooltip carries the standardized residual", {
  d <- gss_r()
  h <- tab(d, race, rincome, pct = "row", color = "contrib", na = "drop") |>
    tab_html(tooltips = TRUE) |> as.character()
  testthat::expect_match(h, "residual", fixed = TRUE)
})

testthat::test_that("a cell with expected count < 1 gets no residual (sparse guard)", {
  # The normal approximation does not hold at e < 1: such a cell would otherwise flag at |z| = 6.
  d <- data.frame(
    r = factor(c(rep("A", 800), rep("B", 800), rep("rare", 4))),
    c = factor(c(sample(c("x", "y", "z", "w"), 1600, TRUE, prob = c(.7, .2, .05, .05)),
                 c("w", "w", "w", "x")))
  )
  t <- tab(d, r, c, color = "contrib", color_signif = "grey_non_signif")
  # the "approximation may be incorrect" warning is the POINT of the fixture
  e <- suppressWarnings(stats::chisq.test(table(d$r, d$c), correct = FALSE))$expected
  sparse <- which(e["rare", ] < 1)
  testthat::expect_gt(length(sparse), 0)                       # the fixture really is sparse
  i <- which(levels(d$r) == "rare")
  for (j in sparse) {
    testthat::expect_true(is.na(get_pvalue(t[[colnames(e)[j]]])[i]))
    testthat::expect_equal(fmt_color_channels(t[[colnames(e)[j]]])$text_slot[i], 0L)
  }
})

testthat::test_that("conf_level_to_z converts and rounds", {
  testthat::expect_equal(conf_level_to_z(0.95), 1.96)
  testthat::expect_equal(conf_level_to_z(c(0.95, 0.99, 0.9999, 1 - 2e-9)), c(1.96, 2.58, 3.89, 6))
  testthat::expect_equal(conf_level_to_z(0.95, digits = Inf), stats::qnorm(0.975))
  # it is a plain converter: the scale stores z, so these two are interchangeable
  testthat::expect_equal(mk_color_scale("zscore", conf_level_to_z(c(0.95, 0.99))),
                         mk_color_scale("zscore", c(1.96, 2.58)))
})

testthat::test_that("the confidence level moves the guaranteed_effect thresholds, keeping spacing", {
  # NOTE the mechanism: the colour engine works per COLUMN, so it cannot read a TABLE attribute --
  # every significance threshold in it (including the pre-z4 contrib gate) comes from the
  # `tabxplor.conf_level` OPTION, not from a per-call `tab(conf_level =)`. Documented in ?tab.
  d <- gss_r()
  th <- function(cl) {
    withr::local_options(list(tabxplor.conf_level = cl))
    t <- tab(d, race, rincome, color = "contrib", color_signif = "guaranteed_effect", na = "drop")
    fmt_color_plan(t[["4-$25000 or more"]])$over_breaks
  }
  b95 <- th(0.95); b99 <- th(0.99)
  testthat::expect_equal(b95, c(1.96, 2.58, 3.89, 6), tolerance = 1e-4)
  # the first threshold IS z(conf_level); the others keep the spacing the break scale declares
  testthat::expect_equal(b99[1], conf_level_to_z(0.99, Inf), tolerance = 1e-8)
  testthat::expect_equal(b99 - b95,
                         rep(conf_level_to_z(0.99, Inf) - conf_level_to_z(0.95, Inf), 4),
                         tolerance = 1e-6)
})
