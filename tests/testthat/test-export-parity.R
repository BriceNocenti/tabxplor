# PURPOSE: Lock the parity between tabxplor's two non-unified display paths.
# ROLE: Guardrail for the 1.4.0 "unify the exporter prep function" workstream (#4).
# KEY CONSTRAINTS:
#   - format.tabxplor_fmt() is the source of truth for tab_md() / tab_kable() / console.
#   - tab_xl() BYPASSES it and reads get_num() / get_display() / get_digits() directly.
#   - These two paths must show the SAME number. This test checks the format() string
#     round-trips to the get_num()/get_digits() value tab_xl writes, for simple display
#     types (n, pct, mean). If the exporter-prep refactor makes them drift, this fails.
# See: CLAUDE.md > Design Decisions > Export Parity.

# Extract the leading numeric value from a format.tabxplor_fmt() cell string.
# Drops the "(sd)" parenthetical of means, "%" sign, and thousands separators (any space).
parity_num_from_str <- function(s) {
  s <- sub("\\(.*$", "", s)          # drop " (sigma ...)" for means
  s <- gsub("[^0-9.+-]", "", s)      # keep digits, decimal point, sign; drop %, spaces, etc.
  suppressWarnings(as.numeric(s))
}

# For every "simple" fmt cell of a table, assert the format-path number equals the
# get_num()/get_digits() value tab_xl would write.
expect_export_parity <- function(tabs) {
  fmt_cols <- tabs[purrr::map_lgl(tabs, is_fmt)]
  for (col in fmt_cols) {
    disp   <- get_display(col)
    num    <- get_num(col)
    dg     <- get_digits(col)
    strs   <- format(col)
    simple <- !is.na(disp) & disp %in% c("n", "wn", "pct", "mean") & !is.na(num)
    for (i in which(simple)) {
      got <- parity_num_from_str(strs[[i]])
      testthat::skip_if(is.na(got))                     # blank/dash cell: nothing to compare
      expected <- if (disp[[i]] == "pct") num[[i]] * 100 else num[[i]]
      testthat::expect_equal(
        round(got, dg[[i]]), round(expected, dg[[i]]),
        tolerance = 1e-6,
        info = paste0("display '", disp[[i]], "' cell '", strs[[i]], "'")
      )
    }
  }
}

gss <- forcats::gss_cat

testthat::test_that("format path matches get_num/get_digits (tab_xl path) for counts", {
  expect_export_parity(tab(gss, marital, race, pct = "no"))
})

testthat::test_that("format path matches get_num/get_digits (tab_xl path) for row pct, 0 digits", {
  expect_export_parity(tab(gss, marital, race, pct = "row"))
})

testthat::test_that("format path matches get_num/get_digits (tab_xl path) for row pct, 1 digit", {
  expect_export_parity(tab(gss, marital, race, pct = "row", digits = 1L))
})

testthat::test_that("format path matches get_num/get_digits (tab_xl path) for means", {
  expect_export_parity(tab_num(gss, race, c(age, tvhours), digits = 1L))
})
