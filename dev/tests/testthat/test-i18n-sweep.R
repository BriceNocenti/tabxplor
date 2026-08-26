
# === SECTION: the French catalogue ================================================================

fr_data <- function() {
  set.seed(1)
  tibble::tibble(
    y    = factor(sample(c("no", "yes"), 300, TRUE)),
    race = factor(sample(c("white", "black", "other"), 300, TRUE)),
    inc  = factor(sample(c("low", "mid", "high"), 300, TRUE))
  )
}


footer_txt <- function(x, lang) {
  paste(render_footer(tab_footer_streams(x, lang = lang), "plain"), collapse = " ")
}


test_that("French typography: multiply sign + decimal comma (locale-independent)", {
  # number formatting follows the resolved `lang` directly, NOT the gettext catalog -- so this holds
  # even where translation is impossible. Hence unguarded. (ASCII-escaped per the non-ASCII rule.)
  ct <- tab(fr_data(), race, y, pct = "row", color = TRUE)
  expect_match(footer_txt(ct, "fr"), "\u00d71,5")      # x1,5
})


reg_fit <- function() {
  tab_reg(fr_data(), outcome = "y", predictors = c("race", "inc"), family = "binomial")
}


test_that("summary / GOF row labels stay English under the ambient en locale", {
  # goldens/snapshots must not move.
  expect_equal(test_es_measure("cramer_v"), "Cram\u00e9r's V")
  expect_equal(reg_footer_spec()$sigma$label, "Residual SD")
})


# Phase 18z14-ii replaced z14-i's "Weighted by the survey design." by ruling Q7's sentence, now
# that Route A makes the intervals design-based as well as the tests.
svy_footer_en <- paste("Design-based (survey): weighted estimates, intervals and tests",
                       "account for the sample design.")


test_that("the survey-design weight line stays English under the ambient en locale", {
  expect_equal(with_legend_lang("en", function(lg) gettext(svy_footer_en)), svy_footer_en)
})


# Phase 18z16-i: ONE sentence per inference basis, so the DEFAULT weighted position stops being
# silent. All three must translate -- the second is the one every weighted table prints.
z16_footers <- c(
  n       = "Weighted by %s; confidence intervals and tests use the unweighted sample size.",
  weights = "Weighted by %s; confidence intervals and tests account for the weighting.",
  partial = paste("Design-based (survey) estimates; this table's design variance could not be",
                  "computed, so its intervals account for the weighting only."))


test_that("the per-basis weight lines stay English under the ambient en locale", {
  with_legend_lang("en", function(lg)
    for (m in z16_footers) expect_equal(gettext(m), m))
})


test_that("the per-basis weight lines translate", {
  skip_if_no_gettext()
  # Phase 18z16-iiiii: the degrade REASON left the footer -- it is a build event, named in
  # svy_var_degraded()'s console message where it is actionable, so there is no reason msgid to
  # translate any more. The CLAIM ("design_partial") rides the columns, and its sentence is here.
  with_legend_lang("fr", function(lg) {
    for (m in z16_footers) {
      fr <- gettext(m)
      expect_false(identical(fr, m))
      expect_match(fr, "^(Pond\u00e9r\u00e9|Estimations)")
    }
  })
})


# Phase 18z17: forest_plot()'s axis titles and guide keys are the only strings a CHART adds. They
# go through the same with_legend_lang() seam as the legend, so `lang =` reaches them -- unlike the
# footer nouns, which resolve on the ambient locale (the glibc catalogue-caching limit z2 recorded).
z17_plot_msgids <- c("Odds ratio", "Ratio", "Rate ratio", "Percentage points", "Percentage",
                     "Coefficient (log scale)", "Units of the outcome", "SD of the outcome",
                     "not significant", "not guaranteed", "below the first threshold")


test_that("the chart's words stay English under the ambient en locale", {
  with_legend_lang("en", function(lg)
    for (m in z17_plot_msgids) expect_equal(gettext(m), m))
  expect_equal(with_legend_lang("en", function(lg) gettextf("%s (%s%% CI)", "OR", "95")),
               "OR (95% CI)")
})


test_that("the chart's words translate", {
  skip_if_no_gettext()
  with_legend_lang("fr", function(lg) {
    for (m in z17_plot_msgids) expect_false(identical(gettext(m), m))
    expect_equal(gettext("not significant"), "non significatif")
    expect_match(gettextf("%s (%s%% CI)", "OR", "95"), "IC")
    expect_match(gettextf("vs %s", "la ligne Total"), "^p\\. r\\. \u00e0 ")
  })
})


test_that("a forest plot's axis and guide follow lang =", {
  skip_if_no_gettext()
  skip_if_not_installed("ggplot2")
  grDevices::pdf(tempfile(fileext = ".pdf")); on.exit(grDevices::dev.off())
  d <- fx_gss_fmt()
  t <- tab(d, race, party3, pct = "row", ci = "ref", color = TRUE,
           color_signif = "grey_non_signif")
  p_en <- forest_plot(t, lang = "en")
  p_fr <- forest_plot(t, lang = "fr")
  # the axis title carries the interval's own name too, where there is one axis to say it on
  expect_equal(p_en$labels$x, "Percentage points (95% CI, Newcombe score interval)")
  expect_match(p_fr$labels$x, "^Points de pourcentage \\(IC \u00e0 95")
  nm <- function(p) Filter(function(s) !inherits(s$name, "waiver") && !is.null(s$name),
                           p$scales$scales)[[1]]$name
  expect_match(nm(p_en), "^Difference vs ")
  expect_match(nm(p_fr), "^Diff\u00e9rence p\\. r\\. \u00e0 ")
})


test_that("20h: tab_xl(lang =) reaches the Excel colour legend", {
  skip_if_not_installed("openxlsx2")
  d <- fx_gss_fmt()
  t <- tab(d, race, party3, pct = "row", ci = "ref", color = "difference")
  sub_of <- function(lg) {
    p <- withr::local_tempfile(fileext = ".xlsx")
    tab_xl(t, path = p, open = FALSE, replace = TRUE, lang = lg)
    paste(unlist(openxlsx2::read_xlsx(p, col_names = FALSE, skip_empty_rows = FALSE)),
          collapse = " ")
  }
  expect_match(sub_of("en"), "Percentage points (risk) difference", fixed = TRUE)
  skip_if_no_gettext()
  expect_match(sub_of("fr"), "Diff\u00e9rence de points de pourcentage")
})


# === SECTION: the French catalogue ================================================================

fr_data <- function() {
  set.seed(1)
  tibble::tibble(
    y    = factor(sample(c("no", "yes"), 300, TRUE)),
    race = factor(sample(c("white", "black", "other"), 300, TRUE)),
    inc  = factor(sample(c("low", "mid", "high"), 300, TRUE))
  )
}


footer_txt <- function(x, lang) {
  paste(render_footer(tab_footer_streams(x, lang = lang), "plain"), collapse = " ")
}


reg_fit <- function() {
  tab_reg(fr_data(), outcome = "y", predictors = c("race", "inc"), family = "binomial")
}


test_that("summary / GOF row labels + measure words translate", {
  skip_if_no_gettext()
  with_legend_lang("fr", function(lg) {
    expect_match(test_pvalue_descriptor(c("chi2", "F_welch")), "p-valeur")
    expect_match(test_pvalue_descriptor(c("chi2", "F_welch")), "F de Welch")
    expect_match(test_es_measure(c("cramer_v", "eta2")), "V de Cram\u00e9r")
    expect_equal(reg_footer_spec()$r2_adj$label, "R2 ajust\u00e9")
    # dynamic gettext(m$word) -- kept extractable by the fmt_class.R anchor
    expect_equal(legend_measure_word("difference", FALSE, NA, lg), "diff\u00e9rence")
    expect_equal(legend_measure_word("contrib", FALSE, NA, lg), "contribution au Chi2")
  })
})


# Phase 18z14-ii replaced z14-i's "Weighted by the survey design." by ruling Q7's sentence, now
# that Route A makes the intervals design-based as well as the tests.
svy_footer_en <- paste("Design-based (survey): weighted estimates, intervals and tests",
                       "account for the sample design.")


test_that("the survey-design weight line translates", {
  skip_if_no_gettext()
  with_legend_lang("fr", function(lg) {
    expect_equal(gettext(svy_footer_en),
                 paste("Estimations, intervalles et tests pond\u00e9r\u00e9s tiennent compte",
                       "du plan d'\u00e9chantillonnage (survey-design)."))
  })
})


# Phase 18z16-i: ONE sentence per inference basis, so the DEFAULT weighted position stops being
# silent. All three must translate -- the second is the one every weighted table prints.
z16_footers <- c(
  n       = "Weighted by %s; confidence intervals and tests use the unweighted sample size.",
  weights = "Weighted by %s; confidence intervals and tests account for the weighting.",
  partial = paste("Design-based (survey) estimates; this table's design variance could not be",
                  "computed, so its intervals account for the weighting only."))


# Phase 18z17: forest_plot()'s axis titles and guide keys are the only strings a CHART adds. They
# go through the same with_legend_lang() seam as the legend, so `lang =` reaches them -- unlike the
# footer nouns, which resolve on the ambient locale (the glibc catalogue-caching limit z2 recorded).
z17_plot_msgids <- c("Odds ratio", "Ratio", "Rate ratio", "Percentage points", "Percentage",
                     "Coefficient (log scale)", "Units of the outcome", "SD of the outcome",
                     "not significant", "not guaranteed", "below the first threshold")
