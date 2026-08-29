# PURPOSE: the hover tooltip: one line per declared token, in reading order.
# ROLE: the shipped CONTRACT for R/tab-tooltip.R -- a failure here is a user-visible change.
#   The exhaustive sweep around it lives in dev/tests/testthat/.
# See: CLAUDE.md section "Testing" (what belongs in which suite).

# === SECTION: the hover tooltip ===================================================================

mult_glyph <- "\u00d7"


div_glyph  <- "\u00f7"



tip_of <- function(t, col) tabxplor:::tab_tooltip_text(t[[col]])



reg_data <- function() {
  fx_gss() |>
    dplyr::mutate(married = factor(dplyr::if_else(marital == "Married", "Married", "Not married")))
}




# --- the numeric-diff display -------------------------------------------------------------

testthat::test_that("a mean diff displays as a signed difference, not with a multiply sign", {
  t  <- tab(fx_gss(), race, tvhours, pct = "row", color = "diff")
  d  <- format(set_display(t$tvhours, "diff"))
  testthat::expect_false(any(grepl(mult_glyph, d, fixed = TRUE)))
  # signed, except on the reference cell, which prints the bare neutral "0"
  testthat::expect_true(all(grepl("^[+-]", d[!is.na(d) & d != "0"])))
  testthat::expect_true(any(d %in% "0"))
  # the rendered number IS the field (no standardization, no rescaling)
  raw <- get_diff(t$tvhours)
  testthat::expect_equal(as.numeric(sub("^\\+", "", d)), round(raw, 1), tolerance = 1e-8)
})



testthat::test_that("the Excel numFmt follows the text display for BOTH diff kinds", {
  t <- tab(fx_gss(), race, c(marital, tvhours), pct = "row", color = "diff")
  # signed (+x;-x), because format() now writes "+1.2" / "-0.2" for means too
  testthat::expect_true(all(grepl("^\\+.*;-",
                                  format(set_display(t$tvhours, "diff"), syntax = "excel"))))
  testthat::expect_true(all(grepl("^\\+.*;-",
                                  format(set_display(t$Married, "diff"), syntax = "excel"))))
  # a mean diff carries no "%" (it is in the variable's own units); a pct diff does
  testthat::expect_false(any(grepl("%", format(set_display(t$tvhours, "diff"), syntax = "excel"))))
  testthat::expect_true(all(grepl("%", format(set_display(t$Married, "diff"), syntax = "excel"))))
})



testthat::test_that("a reference cell says `ref` ONCE for the whole diff+ratio group, keeping n", {
  t  <- tab(fx_gss(), race, marital, pct = "row", color = c("diff", "ratio"))
  tt <- tip_of(t, "Married")
  # NB the default ref = "tot" marks the reference as the TOTAL row, not via in_refrow -- so the
  # reference cells are get_reference()'s (what the tooltip itself reads), not is_refrow()'s.
  r  <- which(get_reference(t$Married, mode = "cells"))
  testthat::expect_length(r, 1L)
  testthat::expect_match(tt[r], "^ref ; n: ")                      # one token, and n survives
  testthat::expect_false(grepl("diff: ref", tt[r], fixed = TRUE))  # not the old double
  testthat::expect_false(grepl("ratio:",    tt[r], fixed = TRUE))
})




# --- the gates, on the two producers -------------------------------------------------------

testthat::test_that("a coefficient is never printed as a percentage, on hover or in Excel", {
  t   <- tab_reg(reg_data(), outcome = "age", predictors = "race", family = "gaussian",
                 empirical = FALSE)
  col <- t[[reg_fmt_cols(t)[[1]]]]
  testthat::expect_identical(get_scale(col), "raw_diff")
  d   <- format(set_display(col, "diff"))
  testthat::expect_false(any(grepl("%", d, fixed = TRUE)))
  testthat::expect_false(any(grepl("%", format(set_display(col, "diff"), syntax = "excel"),
                                   fixed = TRUE)))
  tt <- tabxplor:::tab_tooltip_text(col)
  # the line is the estimate's own, so it is named by the token the scale renders `est` as
  testthat::expect_true(any(grepl("diff: ", tt, fixed = TRUE)))
  testthat::expect_false(any(grepl("%", sub(", p = .*$", "", tt), fixed = TRUE)))
})



testthat::test_that("the odds ratio is shown on every percentage column, its baseline included", {
  t  <- tab(fx_gss(), race, marital, pct = "row", color = "diff")
  tt <- tip_of(t, "Separated")
  testthat::expect_true(any(grepl("OR: ", tt, fixed = TRUE)))
  # the FIRST column is the odds ratio's complementary category -- a whole column of 1s, and that
  # IS the point: "OR: 1" is how a reader finds the column the ratio is read against.
  first <- names(t)[purrr::map_lgl(t, is_fmt)][[1]]
  ft <- tip_of(t, first)
  testthat::expect_true(all(grepl("OR: 1", ft[!grepl("^ref ", ft)], fixed = TRUE)))
  # and a reference cell still collapses to one "ref"
  r <- which(get_reference(t$Separated, mode = "cells"))
  testthat::expect_match(tt[r], "^ref ; n: ")
})



# --- Phase 22g-ii: the tooltip has TWO rows --------------------------------------------------------
# The cell's own numbers, then the observed comparison -- the crude effect and the gap to it, which
# is a statement about ANOTHER column and reads as its own sentence. TOOLTIP_LINES$group declares it.

testthat::test_that("obs and gap take a line of their own, in that order", {
  g <- fx_reg_fmt()
  g$married <- factor(ifelse(g$marital == "Married", "yes", "no"))
  t  <- suppressMessages(tab_reg(g, "married", c("race", "rincome"), family = "binomial",
                                 measure = "difference"))
  mc <- t[[names(t)[vapply(t, function(x)
    is_fmt(x) && identical(get_role(x), "model"), logical(1))][[1]]]]
  tt <- tabxplor:::tab_tooltip_text(mc)
  hit <- grep("obs: ", tt, fixed = TRUE)
  testthat::expect_gt(length(hit), 0L)
  for (k in hit) {
    parts <- strsplit(tt[[k]], "\n", fixed = TRUE)[[1]]
    testthat::expect_length(parts, 2L)                       # exactly two rows
    testthat::expect_false(grepl("obs: ", parts[[1]], fixed = TRUE))
    testthat::expect_lt(regexpr("obs: ", parts[[2]], fixed = TRUE),
                        regexpr("gap: ", parts[[2]], fixed = TRUE))
  }
})
