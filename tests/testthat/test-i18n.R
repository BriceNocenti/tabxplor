# PURPOSE: the French catalogue: each feature in English (everywhere) and in French (gettext-guarded).
# ROLE: the shipped CONTRACT for po/, inst/i18n/ -- a failure here is a user-visible change.
#   The exhaustive sweep around it lives in dev/tests/testthat/.
# See: CLAUDE.md section "Testing" (what belongs in which suite).

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



test_that("crosstab colour legend stays English when asked in English", {
  ct <- tab(fr_data(), race, y, pct = "row", color = TRUE)
  en <- footer_txt(ct, "en")

  # English legend words present, French ones absent (guards against an accidental global switch).
  expect_match(en, "Percentage points (risk) difference", fixed = TRUE)
  expect_no_match(en, "Diff\u00e9rence de points de pourcentage")
})



test_that("crosstab colour legend translates to French", {
  skip_if_no_gettext()
  ct <- tab(fr_data(), race, y, pct = "row", color = TRUE)
  fr <- footer_txt(ct, "fr")

  # French legend words present, English ones gone.
  expect_match(fr, "Diff\u00e9rence de points de pourcentage")
  expect_match(fr, "Couleur de fond")
  expect_no_match(fr, "Percentage points")
})



reg_fit <- function() {
  tab_reg(fr_data(), outcome = "y", predictors = c("race", "inc"), family = "binomial")
}



test_that("regression 'Model:' footer + estimand stay English when asked in English", {
  en <- reg_model_lines(reg_fit(), "en")
  expect_match(en, "^Model: logistic regression")
  expect_match(en, "OR: odds ratio \\(vs the reference category\\)")
})



test_that("regression 'Model:' footer + estimand translate", {
  skip_if_no_gettext()
  fr <- reg_model_lines(reg_fit(), "fr")
  expect_match(fr, "^Mod\u00e8le : r\u00e9gression logistique")
  expect_match(fr, "OR : rapport de cotes \\(par rapport \u00e0 la modalit\u00e9 de r\u00e9f\u00e9rence\\)")
  expect_no_match(fr, "logistic regression")
})



# Phase 18z14-ii replaced z14-i's "Weighted by the survey design." by ruling Q7's sentence, now
# that Route A makes the intervals design-based as well as the tests.
svy_footer_en <- paste("Design-based (survey): weighted estimates, intervals and tests",
                       "account for the sample design.")



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




# === Phase 20h: `lang` was INERT on two exporters ============================
# `lang` is documented on tab_html / forest_plot / tab_md / tab_xl and rd_footer() takes
# it, but only the first three ever passed it: tab_md() handed it to md_render_one(), which dropped
# it, and tab_xl() never read it at all. Both fixtures fail on the pre-20h tree.

test_that("20h: tab_md(lang =) reaches the colour legend (English, runs everywhere)", {
  d <- fx_gss_fmt()
  t <- tab(d, race, party3, pct = "row", ci = "ref", color = "difference")
  en <- tab_md(t, lang = "en")
  # the legend is rendered, in English, whatever the ambient locale
  expect_match(en, "Percentage points (risk) difference", fixed = TRUE, all = FALSE)
})



test_that("20h: tab_md(lang = 'fr') renders the French legend", {
  skip_if_no_gettext()
  d <- fx_gss_fmt()
  t <- tab(d, race, party3, pct = "row", ci = "ref", color = "difference")
  fr <- tab_md(t, lang = "fr")
  expect_match(fr, "Diff\u00e9rence de points de pourcentage", all = FALSE)
  # ...and it is genuinely the argument, not the ambient locale
  expect_false(identical(fr, tab_md(t, lang = "en")))
})


# === the shape table ==============================================================================
# Its strings are gettext()'d at RENDER, under with_legend_lang()'s language -- so both readings of
# each are asserted, the English one unguarded (it must hold on the CRAN farm too).

shape_txt <- function(lang) {
  d <- fr_data(); d$n1 <- as.numeric(seq_len(nrow(d)) %% 17)
  t <- suppressMessages(tab_reg(d, outcome = "y", predictors = c("race", "n1"),
                                family = "binomial"))
  st <- with_legend_lang(lang, function(lg) tabxplor:::reg_shape_table(t))
  paste(c(attr(st, "headers"), unlist(lapply(st, as.character))), collapse = " | ")
}

test_that("the shape table stays English when asked in English", {
  en <- shape_txt("en")
  expect_match(en, "outcome", fixed = TRUE)
  expect_match(en, "numeric predictor", fixed = TRUE)
  expect_no_match(en, "variable expliqu\u00e9e")
})

test_that("the shape table translates to French", {
  skip_if_no_gettext()
  fr <- shape_txt("fr")
  expect_match(fr, "variable expliqu\u00e9e")
  expect_match(fr, "pr\u00e9dicteur num\\.")
  expect_no_match(fr, "numeric predictor")
})
