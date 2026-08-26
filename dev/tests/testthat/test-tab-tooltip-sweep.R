
# === SECTION: the hover tooltip ===================================================================

mult_glyph <- "\u00d7"

div_glyph  <- "\u00f7"


tip_of <- function(t, col) tabxplor:::tab_tooltip_text(t[[col]])


reg_data <- function() {
  fx_gss() |>
    dplyr::mutate(married = factor(dplyr::if_else(marital == "Married", "Married", "Not married")))
}


testthat::test_that("a pct diff keeps its sign and its %", {
  t <- tab(fx_gss(), race, marital, pct = "row", color = "diff")
  d <- format(set_display(t$Married, "diff"))
  # signed, except the reference cell, which prints the bare neutral "0%"
  testthat::expect_true(all(grepl("^[+-].*%$", d[!is.na(d) & d != "0%"])))
  testthat::expect_true(any(d %in% "0%"))
})


# --- the tooltip --------------------------------------------------------------------------

testthat::test_that("no doubled multiply sign on a mean tooltip", {
  t  <- tab(fx_gss(), race, tvhours, pct = "row", color = "diff")
  tt <- tip_of(t, "tvhours")
  testthat::expect_false(any(grepl(paste0(mult_glyph, mult_glyph), tt, fixed = TRUE)))
  testthat::expect_true(any(grepl("diff: -", tt, fixed = TRUE)))
})


testthat::test_that("the standardized diff on hover equals diff / sd_ref", {
  t   <- tab(fx_gss(), race, tvhours, pct = "row", color = "diff")
  col <- t$tvhours
  std <- get_diff(col) / sqrt(get_ref_var(col))
  tt  <- tip_of(t, "tvhours")
  i   <- which(!is.na(std) & std != 0)[1]
  testthat::expect_true(grepl(sprintf("std diff: %+.2fsd", std[i]), tt[i], fixed = TRUE))
})


testthat::test_that("tooltip values carry no column-alignment padding", {
  t  <- tab(fx_gss(), race, marital, pct = "row", color = c("diff", "ratio"))
  tt <- unlist(lapply(names(t)[purrr::map_lgl(t, is_fmt)], function(nm) tip_of(t, nm)))
  testthat::expect_false(any(grepl(":  ", tt, fixed = TRUE)))   # never "ratio:   x1"
  testthat::expect_false(any(grepl(" ;  ", tt, fixed = TRUE)))
  testthat::expect_false(any(grepl("^ | $|;$|^;", tt)))
})


testthat::test_that("fragment joining survives a cell with only one field", {
  # the old fixed-separator + collapse chain left a dangling "f1: 5 ;" past 4 adjacent empties;
  # a Total column (n: only) is exactly that shape now that a 10th fragment exists.
  t  <- tab(fx_gss(), race, marital, pct = "row", color = "diff")
  tt <- tip_of(t, "Total")
  testthat::expect_true(all(grepl("^n: [0-9 ]+$", tt)))
})


testthat::test_that("a contrib table's NA-pct Total column does not crash the tooltip", {
  t <- tab(fx_gss(), race, marital, color = "contrib", comp = "tab")
  testthat::expect_no_error(lapply(names(t)[purrr::map_lgl(t, is_fmt)], function(nm) tip_of(t, nm)))
})


testthat::test_that("a popover carries the tooltip TEXT as its content, not its own attributes", {
  # regression: the builder called with popover = TRUE used to return spec_popover()'s ATTRIBUTE
  # string, which the html engine then wrapped again -> data-content="data-toggle=&quot;popover..."
  t <- tab(fx_gss(), race, marital, pct = "row", color = "diff")
  h <- as.character(tab_kable(t, popover = TRUE, tooltips = TRUE, css = FALSE))
  testthat::expect_false(grepl('data-content="data-toggle', h, fixed = TRUE))
  testthat::expect_true(grepl('data-content="diff:', h, fixed = TRUE))
  testthat::expect_true(grepl('data-trigger="hover"', h, fixed = TRUE))
})


testthat::test_that("the one-line tooltip rule ships with the chrome, never with tab_md's CSS", {
  testthat::expect_match(tab_css(format = "html", style_tag = FALSE), "\\.tooltip-inner\\{")
  testthat::expect_false(grepl("tooltip", tab_css(format = "md",   style_tag = FALSE)))
})


testthat::test_that("a crude column and its model twin agree about the reference row", {
  t <- tab_reg(reg_data(), outcome = "married", predictors = "race", empirical = TRUE)
  tips <- lapply(reg_fmt_cols(t), function(nm) tabxplor:::tab_tooltip_text(t[[nm]]))
  testthat::expect_length(tips, 2L)                       # the crude column and the model one
  r <- which(is_refrow(t[[reg_fmt_cols(t)[[1]]]]) & as.character(t$var) != "Constant")[1]
  for (tt in tips) testthat::expect_match(tt[r], "^ref\\b")
})


testthat::test_that("the baseline row is the reference for nothing, and names nothing", {
  t   <- tab_reg(reg_data(), outcome = "married", predictors = "race", empirical = TRUE)
  nm  <- reg_fmt_cols(t)
  col <- t[[nm[[length(nm)]]]]                            # the MODEL column: a crude one has no baseline
  i   <- which(as.character(t$var) == "Constant")
  tt  <- tabxplor:::tab_tooltip_text(col)[i]
  testthat::expect_false(grepl("ref", tt, fixed = TRUE))     # it is not one
  testthat::expect_false(grepl("OR:", tt, fixed = TRUE))     # a baseline odds is not an odds RATIO
  testthat::expect_match(tt, "n: ")                          # its own base still names itself
})


testthat::test_that("no line repeats an interval the cell already prints", {
  t  <- tab(fx_gss(), race, marital, pct = "row", ci = "cell")
  tt <- tip_of(t, "Married")
  testthat::expect_false(any(grepl("[", tt, fixed = TRUE)))
  testthat::expect_true(any(grepl("row%: ", tt, fixed = TRUE)))
})


testthat::test_that("a total row's contribution is named as the MEAN it is", {
  t  <- tab(fx_gss(), race, marital, pct = "row", color = "contrib")
  tt <- tip_of(t, "Married")
  r  <- which(is_totrow(t$Married))
  testthat::expect_true(any(grepl("mean ctr: ", tt[r], fixed = TRUE)))
  testthat::expect_true(any(grepl("ctr: ", tt[-r], fixed = TRUE)))
  # under `guaranteed_effect` the measure IS the standardized residual: no mean to compare to
  t2 <- tab(fx_gss(), race, marital, pct = "row", color = "contrib",
            color_signif = "guaranteed_effect")
  testthat::expect_false(any(grepl("mean ctr", tip_of(t2, "Married"), fixed = TRUE)))
})


# === SECTION: the hover tooltip ===================================================================

mult_glyph <- "\u00d7"


div_glyph  <- "\u00f7"


tip_of <- function(t, col) tabxplor:::tab_tooltip_text(t[[col]])


reg_data <- function() {
  fx_gss() |>
    dplyr::mutate(married = factor(dplyr::if_else(marital == "Married", "Married", "Not married")))
}


testthat::test_that("a mean column shows the ratio it is coloured by, plus the standardized diff", {
  t  <- tab(fx_gss(), race, tvhours, pct = "row", color = "ratio")
  tt <- tip_of(t, "tvhours")
  testthat::expect_true(any(grepl("ratio: ", tt, fixed = TRUE)))     # was excluded by type
  testthat::expect_true(any(grepl("std diff: [+-][0-9.]+sd", tt)))   # the Glass's delta the legend names
})


testthat::test_that("a Total column gets no vacuous diff/ratio line", {
  t  <- tab(fx_gss(), race, marital, pct = "row", color = c("diff", "ratio"))
  tt <- tip_of(t, "Total")
  testthat::expect_false(any(grepl("ratio:", tt, fixed = TRUE)))   # every cell is its own base
  testthat::expect_false(any(grepl("diff:",  tt, fixed = TRUE)))
  testthat::expect_true(all(grepl("^n: ", tt)))
})


# --- the shared bootstrap attribute builder ------------------------------------------------

testthat::test_that("tooltip attributes carry the auto placement (reoriented on overflow)", {
  t <- tab(fx_gss(), race, marital, pct = "row", color = "diff")
  for (pop in c(FALSE, TRUE)) {
    h <- as.character(tab_kable(t, popover = pop, tooltips = TRUE, css = FALSE))
    testthat::expect_true(grepl('data-placement="auto right"', h, fixed = TRUE), info = pop)
    testthat::expect_false(grepl('data-placement="right"', h, fixed = TRUE), info = pop)
  }
})


testthat::test_that("a cell with no observed comparison stays one line", {
  t  <- tab(fx_gss(), race, marital, pct = "row")
  tt <- tabxplor:::tab_tooltip_text(t[["Married"]])
  testthat::expect_false(any(grepl("\n", tt, fixed = TRUE)))
  # and the declared invariant the html appender rests on: group 2 is the LAST row
  testthat::expect_identical(
    max(vapply(tabxplor:::TOOLTIP_LINES, function(l) l$group, integer(1))),
    tabxplor:::TOOLTIP_GROUP_OBS)
})
