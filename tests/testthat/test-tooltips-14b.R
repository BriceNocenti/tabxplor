# Phase 14b: tooltips + the numeric-diff display.
#
# What is locked here:
#   - a mean `diff` renders as a SIGNED DIFFERENCE, never with the legacy multiply sign (the field
#     has been a real difference since Phase 2; only the display lagged), and the Excel numFmt
#     bypass follows it;
#   - diff + ratio are one comparison group: same gate, ONE "ref" token, no vacuous line on a
#     Total column / 100% cell;
#   - values are trimmed (column padding is table alignment, noise in prose);
#   - both html engines emit the SAME bootstrap attributes, from one builder.

mult_glyph <- stringi::stri_unescape_unicode("\\u00d7")
div_glyph  <- stringi::stri_unescape_unicode("\\u00f7")

tip_of <- function(t, col) tab_kable_print_tooltip(t[[col]])


# --- the numeric-diff display -------------------------------------------------------------

testthat::test_that("a mean diff displays as a signed difference, not with a multiply sign", {
  t  <- tab(forcats::gss_cat, race, tvhours, pct = "row", color = "diff")
  d  <- format(set_display(t$tvhours, "diff"))
  testthat::expect_false(any(grepl(mult_glyph, d, fixed = TRUE)))
  testthat::expect_true(all(grepl("^[+-]", d[!is.na(d)])))
  # the rendered number IS the field (no standardization, no rescaling)
  raw <- get_diff(t$tvhours)
  testthat::expect_equal(as.numeric(sub("^\\+", "", d)), round(raw, 1), tolerance = 1e-8)
})

testthat::test_that("a pct diff keeps its sign and its %", {
  t <- tab(forcats::gss_cat, race, marital, pct = "row", color = "diff")
  d <- format(set_display(t$Married, "diff"))
  testthat::expect_true(all(grepl("^[+-].*%$", d[!is.na(d)])))
})

testthat::test_that("the Excel numFmt follows the text display for BOTH diff kinds", {
  t <- tab(forcats::gss_cat, race, c(marital, tvhours), pct = "row", color = "diff")
  # signed (+x;-x), because format() now writes "+1.2" / "-0.2" for means too
  testthat::expect_true(all(grepl("^\\+.*;-",
                                  format(set_display(t$tvhours, "diff"), syntax = "excel"))))
  testthat::expect_true(all(grepl("^\\+.*;-",
                                  format(set_display(t$Married, "diff"), syntax = "excel"))))
  # a mean diff carries no "%" (it is in the variable's own units); a pct diff does
  testthat::expect_false(any(grepl("%", format(set_display(t$tvhours, "diff"), syntax = "excel"))))
  testthat::expect_true(all(grepl("%", format(set_display(t$Married, "diff"), syntax = "excel"))))
})


# --- the tooltip --------------------------------------------------------------------------

testthat::test_that("no doubled multiply sign on a mean tooltip", {
  t  <- tab(forcats::gss_cat, race, tvhours, pct = "row", color = "diff")
  tt <- tip_of(t, "tvhours")
  testthat::expect_false(any(grepl(paste0(mult_glyph, mult_glyph), tt, fixed = TRUE)))
  testthat::expect_true(any(grepl("diff: -", tt, fixed = TRUE)))
})

testthat::test_that("a mean column shows the ratio it is coloured by, plus the standardized diff", {
  t  <- tab(forcats::gss_cat, race, tvhours, pct = "row", color = "ratio")
  tt <- tip_of(t, "tvhours")
  testthat::expect_true(any(grepl("ratio: ", tt, fixed = TRUE)))     # was excluded by type
  testthat::expect_true(any(grepl("std diff: [+-][0-9.]+sd", tt)))   # the Glass's delta the legend names
})

testthat::test_that("the standardized diff on hover equals diff / sd_ref", {
  t   <- tab(forcats::gss_cat, race, tvhours, pct = "row", color = "diff")
  col <- t$tvhours
  std <- get_diff(col) / sqrt(get_ref_var(col))
  tt  <- tip_of(t, "tvhours")
  i   <- which(!is.na(std) & std != 0)[1]
  testthat::expect_true(grepl(sprintf("std diff: %+.2fsd", std[i]), tt[i], fixed = TRUE))
})

testthat::test_that("a reference cell says `ref` ONCE for the whole diff+ratio group, keeping n", {
  t  <- tab(forcats::gss_cat, race, marital, pct = "row", color = c("diff", "ratio"))
  tt <- tip_of(t, "Married")
  # NB the default ref = "tot" marks the reference as the TOTAL row, not via in_refrow -- so the
  # reference cells are get_reference()'s (what the tooltip itself reads), not is_refrow()'s.
  r  <- which(get_reference(t$Married, mode = "cells"))
  testthat::expect_length(r, 1L)
  testthat::expect_match(tt[r], "^ref ; n: ")                      # one token, and n survives
  testthat::expect_false(grepl("diff: ref", tt[r], fixed = TRUE))  # not the old double
  testthat::expect_false(grepl("ratio:",    tt[r], fixed = TRUE))
})

testthat::test_that("a Total column gets no vacuous diff/ratio line", {
  t  <- tab(forcats::gss_cat, race, marital, pct = "row", color = c("diff", "ratio"))
  tt <- tip_of(t, "Total")
  testthat::expect_false(any(grepl("ratio:", tt, fixed = TRUE)))   # every cell is its own base
  testthat::expect_false(any(grepl("diff:",  tt, fixed = TRUE)))
  testthat::expect_true(all(grepl("^n: ", tt)))
})

testthat::test_that("tooltip values carry no column-alignment padding", {
  t  <- tab(forcats::gss_cat, race, marital, pct = "row", color = c("diff", "ratio"))
  tt <- unlist(lapply(names(t)[purrr::map_lgl(t, is_fmt)], function(nm) tip_of(t, nm)))
  testthat::expect_false(any(grepl(":  ", tt, fixed = TRUE)))   # never "ratio:   x1"
  testthat::expect_false(any(grepl(" ;  ", tt, fixed = TRUE)))
  testthat::expect_false(any(grepl("^ | $|;$|^;", tt)))
})

testthat::test_that("fragment joining survives a cell with only one field", {
  # the old fixed-separator + collapse chain left a dangling "f1: 5 ;" past 4 adjacent empties;
  # a Total column (n: only) is exactly that shape now that a 10th fragment exists.
  t  <- tab(forcats::gss_cat, race, marital, pct = "row", color = "diff")
  tt <- tip_of(t, "Total")
  testthat::expect_true(all(grepl("^n: [0-9 ]+$", tt)))
})

testthat::test_that("a contrib table's NA-pct Total column does not crash the tooltip", {
  t <- tab(forcats::gss_cat, race, marital, color = "contrib", comp = "tab")
  testthat::expect_no_error(lapply(names(t)[purrr::map_lgl(t, is_fmt)], function(nm) tip_of(t, nm)))
})


# --- the shared bootstrap attribute builder ------------------------------------------------

testthat::test_that("both engines emit the same tooltip attributes, reoriented on overflow", {
  t <- tab(forcats::gss_cat, race, marital, pct = "row", color = "diff")
  for (eng in c("kableExtra", "html")) {
    for (pop in c(FALSE, TRUE)) {
      h <- as.character(tab_kable(t, engine = eng, popover = pop, tooltips = TRUE, css = FALSE))
      testthat::expect_true(grepl('data-placement="auto right"', h, fixed = TRUE),
                            info = paste(eng, pop))
      testthat::expect_false(grepl('data-placement="right"', h, fixed = TRUE),
                             info = paste(eng, pop))
    }
  }
})

testthat::test_that("a popover carries the tooltip TEXT as its content, not its own attributes", {
  # regression: tab_kable_print_tooltip(popover = TRUE) used to return spec_popover()'s ATTRIBUTE
  # string, which the html engine then wrapped again -> data-content="data-toggle=&quot;popover..."
  t <- tab(forcats::gss_cat, race, marital, pct = "row", color = "diff")
  for (eng in c("kableExtra", "html")) {
    h <- as.character(tab_kable(t, engine = eng, popover = TRUE, tooltips = TRUE, css = FALSE))
    testthat::expect_false(grepl('data-content="data-toggle', h, fixed = TRUE), info = eng)
    testthat::expect_true(grepl('data-content="diff:', h, fixed = TRUE), info = eng)
    testthat::expect_true(grepl('data-trigger="hover"', h, fixed = TRUE), info = eng)
  }
})

testthat::test_that("the one-line tooltip rule ships with the chrome, never with tab_md's CSS", {
  testthat::expect_match(tab_css(chrome = TRUE,  style_tag = FALSE), "\\.tooltip-inner\\{")
  testthat::expect_false(grepl("tooltip", tab_css(chrome = FALSE, style_tag = FALSE)))
})
