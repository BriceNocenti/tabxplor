# Phase 14b: the Katz log-RR ratio confidence interval.
#
# The rule: the interval belongs to the measure the READER SEES. When the ratio is the text channel
# of a proportion column it owns the stored bounds (ci_type = "ratio", Katz log-RR, neutral 1) and a
# background diff channel derives from them; otherwise nothing changes.
#
# What is locked here: the trigger rule (incl. that the DEFAULTS are untouched), parity against a
# hand-computed Katz interval, the CI <-> stars duality, that means never take it, and that the
# significance gate reads the stored ci_type rather than the measure.

d <- forcats::gss_cat

katz_hand <- function(p1, n1, p2, n2, conf_level = 0.95) {
  se <- sqrt((1 - p1) / (n1 * p1) + (1 - p2) / (n2 * p2))
  z  <- stats::qnorm(1 - (1 - conf_level) / 2)
  list(inf = exp(log(p1 / p2) - z * se), sup = exp(log(p1 / p2) + z * se),
       pvalue = 2 * stats::pnorm(-abs(log(p1 / p2) / se)))
}


# --- the engine ---------------------------------------------------------------------------

testthat::test_that("ci_katz_rr matches a hand-computed log-RR interval + its Wald dual", {
  p1 <- c(0.30, 0.55, 0.10); n1 <- c(400, 250, 900)
  p2 <- c(0.25, 0.50, 0.12); n2 <- c(800, 800, 800)
  got <- ci_katz_rr(p1, n1, p2, n2, conf_level = 0.95, want_p = TRUE)
  exp <- katz_hand(p1, n1, p2, n2, 0.95)
  testthat::expect_equal(got$inf,    exp$inf)
  testthat::expect_equal(got$sup,    exp$sup)
  testthat::expect_equal(got$pvalue, exp$pvalue)
})

testthat::test_that("ci_katz_rr honours conf_level and widens with it", {
  a <- ci_katz_rr(0.3, 400, 0.25, 800, conf_level = 0.90)
  b <- ci_katz_rr(0.3, 400, 0.25, 800, conf_level = 0.99)
  testthat::expect_equal(a$inf, katz_hand(0.3, 400, 0.25, 800, 0.90)$inf)
  testthat::expect_true(b$inf < a$inf && b$sup > a$sup)
  testthat::expect_equal(a$pvalue, b$pvalue)          # the p does not depend on the level
})

testthat::test_that("ci_katz_rr degrades to NA rather than +/-Inf on an empty cell or reference", {
  got <- ci_katz_rr(c(0, 0.3, 0.3), c(100, 100, 100), c(0.2, 0, 0.2), c(100, 100, 0))
  testthat::expect_true(all(is.na(got$inf)))
  testthat::expect_true(all(is.na(got$sup)))
  testthat::expect_true(all(is.na(got$pvalue)))
})

testthat::test_that("ci_katz_rr skips the p-value when stars are off", {
  testthat::expect_true(all(is.na(ci_katz_rr(0.3, 400, 0.25, 800, want_p = FALSE)$pvalue)))
})


# --- the trigger rule ---------------------------------------------------------------------

testthat::test_that("only a ratio TEXT channel switches the stored interval to Katz", {
  ratio_cases <- list("ratio", c("ratio", "diff"), c(pct = "ratio"),
                      list(pct = c("ratio", "diff")))
  diff_cases  <- list(TRUE, "diff", c("diff", "ratio"), c(pct = "diff"))
  for (cc in ratio_cases) {
    t <- tab(d, race, marital, pct = "row", color = cc, color_signif = "grey_non_signif")
    testthat::expect_identical(get_scale(t$Married), "pct_ratio")
  }
  for (cc in diff_cases) {                       # the defaults must be untouched
    t <- tab(d, race, marital, pct = "row", color = cc, color_signif = "grey_non_signif")
    testthat::expect_identical(get_scale(t$Married), "points")
  }
})

testthat::test_that("ci = 'cell' has no ratio counterpart (a one-proportion interval)", {
  t <- tab(d, race, marital, pct = "row", color = "ratio", ci = "cell")
  testthat::expect_identical(get_scale(t$Married), "level_pct")
})

testthat::test_that("a MEAN now gets a ratio-of-means interval under a ratio colour (14v-ii)", {
  # Was: a mean kept the difference interval whatever the colour (a ratio of means "needed Fieller").
  # 14v-ii ships ci_mean_ratio, so a ratio-coloured mean owns a real ratio interval (ci_type "ratio").
  t <- tab(d, race, c(marital, tvhours), pct = "row", color = "ratio",
           color_signif = "grey_non_signif")
  testthat::expect_identical(get_scale(t$Married), "pct_ratio")
  testthat::expect_identical(get_scale(t$tvhours), "mean_ratio")
})

testthat::test_that("tab_resolve_settings only asks for the ratio scale where a diff CI is built", {
  s <- tab_resolve_settings(color = "diff", OR = "no", ci = c("diff", "cell", "no"),
                            chi2 = FALSE, ref = "tot",
                            pct_vect = list("row", "row", "row"), col_vars_text = TRUE,
                            color_ratio_ci = TRUE)
  testthat::expect_identical(s$ci_scale, c("ratio", "diff", "diff"))
  s0 <- tab_resolve_settings(color = "diff", OR = "no", ci = "diff", chi2 = FALSE, ref = "tot",
                             pct_vect = list("row"), col_vars_text = TRUE)
  testthat::expect_identical(s0$ci_scale, "diff")     # the default is unchanged
})


# --- the stored interval ------------------------------------------------------------------

testthat::test_that("the stored bounds are the Katz ones, centred on the ratio", {
  t   <- tab(d, race, marital, pct = "row", color = "ratio",
             color_signif = "grey_non_signif", stars = TRUE)
  col <- t$Married
  # ref = "tot" marks the reference as the TOTAL row (tab_ci's own ref_mask), not via in_refrow.
  r   <- which(get_reference(col, mode = "cells"))
  i   <- setdiff(seq_along(col), r)
  exp <- katz_hand(get_pct(col)[i], get_tot_n(col)[i],
                   get_pct(col)[r], get_tot_n(col)[r])
  testthat::expect_equal(get_ci_inf(col)[i],  exp$inf)
  testthat::expect_equal(get_ci_sup(col)[i],  exp$sup)
  testthat::expect_equal(get_pvalue(col)[i],  exp$pvalue)
  testthat::expect_equal(ci_center(col), get_ratio(col))    # get_ci() reads back off this
  testthat::expect_true(is.na(get_ci_inf(col)[r]))          # a reference is never self-compared
})

testthat::test_that("CI <-> stars stay exact duals on the ratio scale", {
  checked <- 0L
  for (cl in c(0.90, 0.95, 0.99)) {
    tt <- tab(d, c(race, marital), c(partyid, relig), pct = "row", color = "ratio",
              color_signif = "grey_non_signif", stars = TRUE, conf_level = cl,
              output_list = TRUE)
    for (one in tt) for (cc in one[purrr::map_lgl(one, is_fmt)]) {
      if (!identical(get_scale(cc), "pct_ratio")) next
      p <- get_pvalue(cc); lo <- get_ci_inf(cc); hi <- get_ci_sup(cc)
      k <- !is.na(p) & !is.na(lo)
      checked <- checked + sum(k)
      testthat::expect_identical(p[k] < 1 - cl, lo[k] > 1 | hi[k] < 1)
    }
  }
  testthat::expect_gt(checked, 100L)      # the duality check above must not be vacuous
})

testthat::test_that("the ratio bracket renders on the ratio scale: no x100, no %", {
  t <- tab(d, race, marital, pct = "row", color = "ratio", color_signif = "grey_non_signif")
  b <- format(set_display(t$Married, "ci"))
  b <- b[!is.na(b)]
  testthat::expect_false(any(grepl("%", b, fixed = TRUE)))
  testthat::expect_true(any(grepl("^\\[[0-9]+\\.[0-9]{2};[0-9]+\\.[0-9]{2}\\]$", b)))
})

testthat::test_that("the legend names Katz, not a diff method it was not built with", {
  t <- tab(d, race, marital, pct = "row", color = "ratio", color_signif = "grey_non_signif")
  lg <- tab_color_legend(t, medium = "plain", lang = "en")
  testthat::expect_match(paste(lg, collapse = " "), "Katz")
  testthat::expect_false(grepl("Newcombe", paste(lg, collapse = " ")))
})


# --- the colour engine reads the stored scale ---------------------------------------------

testthat::test_that("grey_non_signif greys exactly the cells whose ratio interval includes 1", {
  t   <- tab(d, race, marital, pct = "row", color = "ratio", color_signif = "grey_non_signif")
  for (cc in t[purrr::map_lgl(t, is_fmt)]) {
    if (!identical(get_scale(cc), "pct_ratio")) next
    sl  <- fmt_color_channels(cc)$text
    sig <- (get_ci_inf(cc) > 1 | get_ci_sup(cc) < 1) %in% TRUE
    testthat::expect_true(all(sl[!sig] == 0L | is.na(sl[!sig])))
  }
})

testthat::test_that("guaranteed_effect colours the right SIDE on a ratio interval", {
  t <- tab(d, c(race, marital), c(partyid, relig), pct = "row", color = "ratio",
           color_signif = "guaranteed_effect", output_list = TRUE)
  n <- 0L
  for (one in t) for (cc in one[purrr::map_lgl(one, is_fmt)]) {
    if (!identical(get_scale(cc), "pct_ratio")) next
    sl <- fmt_color_channels(cc)$text; rr <- get_ratio(cc)
    k  <- !is.na(sl) & sl > 0L & !is.na(rr)
    n  <- n + sum(k)
    testthat::expect_identical(sl[k] <= 4L, rr[k] > 1)     # slots 1-4 over, 5-8 under
  }
  testthat::expect_gt(n, 0L)
})

testthat::test_that("a derived diff background channel agrees with the ratio text channel", {
  t <- tab(d, race, marital, pct = "row", color = c("ratio", "diff"),
           color_signif = "guaranteed_effect")
  for (cc in t[purrr::map_lgl(t, is_fmt)]) {
    if (!identical(get_scale(cc), "pct_ratio")) next
    ch <- fmt_color_channels(cc)
    k  <- !is.na(ch$text) & !is.na(ch$bg) & ch$text > 0L & ch$bg > 0L
    testthat::expect_identical(ch$text[k] <= 4L, ch$bg[k] <= 4L)   # same direction
  }
})

testthat::test_that("the significance gate keys on the stored ci_type, not on the measure", {
  # a diff-measure column riding a ratio interval is still gated (both test p1 = p2)
  t  <- tab(d, race, marital, pct = "row", color = c("ratio", "diff"),
            color_signif = "grey_non_signif")
  cc <- t$Married
  testthat::expect_identical(get_scale(cc), "pct_ratio")
  bg <- fmt_color_channels(cc)$bg
  testthat::expect_true(any(!is.na(bg) & bg > 0L))     # not greyed wholesale by a neutral mismatch
})


# --- 14v-ii: numeric ratio-of-means (the ci_type="diff" bug fix) ---------------------------

testthat::test_that("a ratio-coloured MEAN stores ci_type='ratio' + ratio-scale bounds (14v-ii)", {
  # Regression lock for the §48 bug: tab(mean, color='ratio', ci='diff') used to store the DIFFERENCE
  # bounds mislabelled as a ratio. It must now store a real ratio-of-means interval (centred on the
  # cell/reference ratio, neutral 1).
  d2 <- forcats::gss_cat |> dplyr::mutate(race = forcats::fct_rev(race)) |>
    dplyr::filter(!is.na(tvhours))
  t   <- tab(d2, race, tvhours, ref = 1, color = "ratio", ci = "diff", stars = TRUE)
  col <- t$tvhours
  testthat::expect_identical(get_scale(col), "mean_ratio")
  testthat::expect_equal(ci_center(col), get_ratio(col))          # centred on the ratio, not the diff
  # the stored bounds bracket the ratio, not the difference; a diff CI would bracket get_diff (~1.4)
  k <- !is.na(get_ci_inf(col))
  testthat::expect_true(all(get_ci_inf(col)[k] <= get_ratio(col)[k] &
                            get_ratio(col)[k] <= get_ci_sup(col)[k]))
})

testthat::test_that("the three mean_ratio methods give the three decisions-48 intervals", {
  d2 <- forcats::gss_cat |> dplyr::mutate(race = forcats::fct_rev(race)) |>
    dplyr::filter(!is.na(tvhours))
  g  <- d2 |> dplyr::filter(race %in% c("White", "Black"))
  hand <- function(method, want_p = FALSE) {
    gb <- g$tvhours[g$race == "Black"]; gw <- g$tvhours[g$race == "White"]
    ci_mean_ratio(mean(gb), stats::var(gb), length(gb),
                  mean(gw), stats::var(gw), length(gw), method = method, want_p = want_p)
  }
  for (m in c("robust", "quasipoisson", "poisson")) {
    t   <- tab(g |> dplyr::mutate(race = forcats::fct_drop(race)), race, tvhours, ref = 1,
               color = "ratio", ci = "diff", ci_method = c(mean_ratio = m), stars = TRUE)
    col <- t$tvhours
    k   <- which(as.character(t$race) == "Black")  # Black vs White = ref (Total row also has a CI)
    ref <- hand(m, want_p = TRUE)
    testthat::expect_equal(get_ci_inf(col)[k], ref$inf, tolerance = 1e-6, label = m)
    testthat::expect_equal(get_ci_sup(col)[k], ref$sup, tolerance = 1e-6, label = m)
  }
})

testthat::test_that("the ratio-of-means bracket renders bare (no %, >= 2 digits)", {
  d2 <- forcats::gss_cat |> dplyr::mutate(race = forcats::fct_rev(race)) |>
    dplyr::filter(!is.na(tvhours))
  t <- tab(d2, race, tvhours, ref = 1, color = "ratio", ci = "diff")
  b <- format(set_display(t$tvhours, "ci"))
  b <- b[!is.na(b) & nzchar(trimws(b))]
  testthat::expect_false(any(grepl("%", b, fixed = TRUE)))
  testthat::expect_true(any(grepl("^\\[[0-9]+\\.[0-9]{2};[0-9]+\\.[0-9]{2}\\]$", trimws(b))))
})

testthat::test_that("the legend names the ratio-of-means method (Welch/Student/robust/quasi/Poisson)", {
  d2 <- forcats::gss_cat |> dplyr::mutate(race = forcats::fct_rev(race))
  leg <- function(...) paste(tab_color_legend(
    tab(d2, race, tvhours, ref = 1, ci = "diff", color_signif = "grey_non_signif", ...),
    medium = "plain", lang = "en"), collapse = " ")
  testthat::expect_match(leg(color = "ratio"),                                   "robust-Poisson")
  testthat::expect_match(leg(color = "ratio", ci_method = c(mean_ratio = "quasipoisson")),
                         "quasi-Poisson")
  testthat::expect_match(leg(color = "ratio", ci_method = c(mean_ratio = "poisson")),
                         "Poisson interval")
  testthat::expect_match(leg(color = "diff"),                                    "Welch t interval")
  testthat::expect_match(leg(color = "diff", ci_method = c(mean_diff = "student")),
                         "Student t interval")
})
