# Last Phase w -- French runtime-string translation. These fixtures FAIL without the filled po/R-fr.po
# (compiled to inst/po/fr/LC_MESSAGES/R-tabxplor.mo) AND the gettext-wrapping of the reg / test / measure
# labels. They also guard the English path: a table asked for in English must stay byte-English (so the
# goldens never move). gettext resolves against the installed .mo via bindtextdomain (.onLoad); the render
# helpers set LANGUAGE through with_legend_lang(), so these are deterministic regardless of the CI locale.

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

test_that("crosstab colour legend translates to French (and English is untouched)", {
  ct <- tab(fr_data(), race, y, pct = "row", color = TRUE)
  en <- footer_txt(ct, "en")
  fr <- footer_txt(ct, "fr")

  # English legend words present in EN, absent in FR (guards against an accidental global switch).
  expect_match(en, "Shades of blue")
  expect_no_match(en, "Nuances de bleu")

  # French legend words present, English ones gone.
  expect_match(fr, "Nuances de bleu")
  expect_match(fr, "Couleur de fond")
  expect_no_match(fr, "Shades of blue")
  # French typography: multiply sign + decimal comma (ASCII-escaped per the non-ASCII-source rule).
  expect_match(fr, "\u00d71,5")      # x1,5
})

test_that("regression 'Model:' footer + estimand translate", {
  t  <- tab_reg(fr_data(), dependent = "y", predictors = c("race", "inc"), family = "binomial")
  en <- reg_model_lines(t, "en")
  fr <- reg_model_lines(t, "fr")

  expect_match(en, "^Model: logistic regression")
  expect_match(en, "odds ratios \\(vs the reference category\\)")

  expect_match(fr, "^Mod\u00e8le : r\u00e9gression logistique")
  expect_match(fr, "rapports de cotes")
  expect_no_match(fr, "logistic regression")
})

test_that("summary / GOF row labels + measure words translate", {
  with_legend_lang("fr", function(lg) {
    expect_match(test_pvalue_descriptor(c("chi2", "F_welch")), "p-valeur")
    expect_match(test_pvalue_descriptor(c("chi2", "F_welch")), "F de Welch")
    expect_match(test_es_measure(c("cramer_v", "eta2")), "V de Cram\u00e9r")
    expect_equal(reg_footer_spec()$r2_adj$label, "R2 ajust\u00e9")
    # dynamic gettext(m$word) -- kept extractable by the fmt_class.R anchor
    expect_equal(legend_measure_word("diff", FALSE, NA, lg), "diff\u00e9rence")
    expect_equal(legend_measure_word("contrib", FALSE, NA, lg), "contribution au Chi2")
  })

  # English (ambient en locale) stays English -> goldens/snapshots do not move.
  expect_equal(test_es_measure("cramer_v"), "Cram\u00e9r's V")
  expect_equal(reg_footer_spec()$sigma$label, "Residual SD")
})
