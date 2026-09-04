# The colour legend's SENTENCE (R/tab-legend.R): what it calls a measure, and the one layer a table
# may re-state. Where that sentence is PRINTED is test-tab-footer.R's.

testthat::test_that("a table may re-state a measure's words, and it reaches every medium", {
  t  <- tab(fx_gss(), race, marital, pct = "row", color = "contrib")
  t2 <- set_legend_words(t, contrib = list(word     = "contribution to the axis variance",
                                           ref_word = "vs the mean contribution"))

  # the console's terse form IS "<word> (<ref_word>): <breaks>"
  terse <- tab_footer_text(t2, style = "terse")[[1]]
  testthat::expect_true(grepl("contribution to the axis variance", terse, fixed = TRUE))
  testthat::expect_true(grepl("(vs the mean contribution)", terse, fixed = TRUE))
  testthat::expect_false(grepl("Chi2", terse, fixed = TRUE))

  # ...and the exports' prose head is the same word, capitalised
  testthat::expect_true(grepl("Contribution to the axis variance",
                              tab_footer_text(t2, style = "prose")[[1]], fixed = TRUE))

  # every medium, the console included -- which no exporter argument can reach
  testthat::expect_true(grepl("axis variance", tab_html(t2), fixed = TRUE))
  testthat::expect_true(grepl("axis variance", tab_md(t2, css = FALSE, print = FALSE), fixed = TRUE))
  testthat::expect_true(any(grepl("axis variance", cli::ansi_strip(print(t2, get_text = TRUE)),
                                  fixed = TRUE)))

  # a bare string is the short word; NULL removes the override
  testthat::expect_true(grepl("share of the variance",
                              tab_footer_text(set_legend_words(t, contrib = "share of the variance"),
                                              style = "terse")[[1]], fixed = TRUE))
  testthat::expect_null(get_legend_words(set_legend_words(t2, contrib = NULL)))
})


testthat::test_that("only NAMING may be re-stated -- never a number", {
  t <- tab(fx_gss(), race, marital, pct = "row", color = "contrib")
  testthat::expect_error(set_legend_words(t, contrib = list(raw = function(x) 1)),
                         "not .*legend word")
  testthat::expect_error(set_legend_words(t, contrib = list(break_over = "!")),
                         "not .*legend word")
  testthat::expect_error(set_legend_words(t, nonsense = "x"), "not .*colour measure")

  # ...and the cells are untouched by an override that IS allowed
  t2 <- set_legend_words(t, contrib = "share of the variance")
  testthat::expect_identical(format(t$Total), format(t2$Total))
  testthat::expect_identical(get_color(t$Total), get_color(t2$Total))
})


testthat::test_that("the override survives a dplyr pipeline and a saveRDS round trip", {
  t <- set_legend_words(tab(fx_gss(), race, marital, pct = "row", color = "contrib"),
                        contrib = "share of the variance")
  testthat::expect_identical(get_legend_words(dplyr::filter(t, TRUE)), get_legend_words(t))
  f <- withr::local_tempfile(fileext = ".rds")
  saveRDS(t, f)
  testthat::expect_identical(get_legend_words(readRDS(f)), get_legend_words(t))
})


testthat::test_that("contrib names its baseline per READING, from its declared field", {
  # `ignore` / `grey_non_signif` grade a share of this table's chi2, read against the mean
  # contribution; `guaranteed_effect` grades the standardized residual, read against independence.
  t <- tab(fx_gss(), race, marital, pct = "row", color = "contrib")
  testthat::expect_true(grepl("vs the mean", tab_footer_text(t, style = "terse")[[1]], fixed = TRUE))

  g <- tab(fx_gss(), race, marital, pct = "row", color = "contrib",
           color_signif = "guaranteed_effect")
  testthat::expect_true(grepl("vs independence", tab_footer_text(g, style = "terse")[[1]],
                              fixed = TRUE))
})


testthat::test_that("a re-stated baseline reaches BOTH registers", {
  # `ref_word` carries its preposition and is what the terse form brackets; `ref_phrase` is the bare
  # noun a prose lead points at. Re-stating only the first left the exports saying the old word.
  t <- set_legend_words(tab(fx_gss(), race, marital, pct = "row", color = "contrib"),
                        contrib = list(word       = "contribution to the axis variance",
                                       ref_word   = "vs the mean contribution",
                                       ref_phrase = "the mean contribution"))
  testthat::expect_match(tab_footer_text(t, style = "terse")[[1]], "(vs the mean contribution)",
                         fixed = TRUE)
  testthat::expect_match(tab_footer_text(t, style = "prose")[[1]], "vs the mean contribution",
                         fixed = TRUE)
  testthat::expect_false(grepl("independence", tab_footer_text(t, style = "prose")[[1]],
                               fixed = TRUE))
})
