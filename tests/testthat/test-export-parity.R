# PURPOSE: Lock the parity between tabxplor's two non-unified display paths.
# ROLE: Guardrail for the 1.4.0 "unify the exporter prep function" workstream (#4 / Phase 10g).
# KEY CONSTRAINTS:
#   - format.tabxplor_fmt() is the source of truth for tab_md() / tab_kable() / console.
#   - tab_xl() writes the RAW get_num() value and lets Excel format it via the codes from
#     format(x, syntax = "excel") (fmt_class.R excel_numfmt_code).
#   - Both paths must show the SAME number. Excel scales by 100 exactly when its numfmt code
#     carries a "%", so the check ties the code's %-scaling to format()'s displayed scaling.
#   - pct_ci / mean_ci cells are written as TEXT (a documented bypass limitation, the raw value is
#     shown, not the value+CI string) -> skipped here.
# See: CLAUDE.md > Design Decisions > Export Parity.

# Extract the leading numeric value from a format.tabxplor_fmt() cell string.
# Drops the "(sd)" parenthetical of means, "%"/"±"/"*" markers, and thousands separators.
parity_num_from_str <- function(s) {
  s <- sub("\\(.*$", "", s)          # drop " (sigma ...)" for means
  s <- gsub("[^0-9.+-]", "", s)      # keep digits, decimal point, sign; drop %, spaces, *, etc.
  suppressWarnings(as.numeric(s))
}

# For every fmt cell the Excel bypass renders as a real NUMBER, assert the format-path number
# equals get_num() scaled by the Excel numfmt code (x100 iff the code is a percentage), rounded
# to get_digits(). This is the invariant the whole raw-write bypass depends on.
expect_export_parity <- function(tabs) {
  fmt_cols <- tabs[purrr::map_lgl(tabs, is_fmt)]
  for (col in fmt_cols) {
    disp <- get_display(col)
    num  <- get_num(col)
    dg   <- get_digits(col)
    code <- format(col, syntax = "excel")   # the Excel numfmt codes tab_xl writes
    strs <- format(col)                      # the text display (source of truth)
    numeric_cell <- !is.na(disp) & !is.na(num) & !is.na(code) & code != "TEXT"
    # threshold displays ("<0.01%", ">...") show a bound, not the exact stored value -> skip
    numeric_cell <- numeric_cell & !grepl("[<>]", strs)
    for (i in which(numeric_cell)) {
      got <- parity_num_from_str(strs[[i]])
      testthat::skip_if(is.na(got))                     # blank/dash cell: nothing to compare
      scale    <- if (grepl("%", code[[i]], fixed = TRUE)) 100 else 1
      expected <- num[[i]] * scale
      testthat::expect_equal(
        round(got, dg[[i]]), round(expected, dg[[i]]),
        tolerance = 1e-6,
        info = paste0("display '", disp[[i]], "' code '", code[[i]], "' cell '", strs[[i]], "'")
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

# Phase 10g: extend the oracle to diff / ctr / or displays (the ones whose Excel digits now follow
# format()'s adjusted digits). "diff" is not a default tab() display, so set it explicitly.
testthat::test_that("format path matches tab_xl path for diff display (pct)", {
  tb <- tab(gss, marital, race, pct = "row", digits = 1L) |>
    dplyr::mutate(dplyr::across(dplyr::where(is_fmt), ~ set_display(., "diff")))
  expect_export_parity(tb)
})

testthat::test_that("format path matches tab_xl path for diff display (mean)", {
  tb <- tab_num(gss, race, age, digits = 1L) |>
    dplyr::mutate(dplyr::across(dplyr::where(is_fmt), ~ set_display(., "diff")))
  expect_export_parity(tb)
})

testthat::test_that("format path matches tab_xl path for a contrib table", {
  expect_export_parity(tab(gss, marital, race, color = "contrib"))
})

testthat::test_that("format path matches tab_xl path for an empirical OR table", {
  testthat::skip_if(inherits(try(tab(gss, marital, race, pct = "row", OR = TRUE),
                                 silent = TRUE), "try-error"))
  expect_export_parity(tab(gss, marital, race, pct = "row", OR = TRUE))
})

# Phase 10g: lock the Excel number-format codes format(syntax = "excel") emits (the fold's anchor,
# since the old inline numfmt() is deleted). One representative cell of each column.
first_codes <- function(tabs) {
  fmt_cols <- names(tabs)[purrr::map_lgl(tabs, is_fmt)]
  purrr::map_chr(fmt_cols, ~ format(tabs[[.x]], syntax = "excel")[[1]]) |>
    stats::setNames(fmt_cols)
}

testthat::test_that("format(syntax = 'excel') emits the expected numFmt codes", {
  # counts -> integer thousands
  cc <- first_codes(tab(gss, marital, race, pct = "no"))
  testthat::expect_true(all(cc == "#,##0"))

  # row pct: percentage columns "0%" / "0.0%"; the count column stays "#,##0"
  c0 <- first_codes(tab(gss, marital, race, pct = "row"))
  testthat::expect_true(all(c0 %in% c("0%", "#,##0")) && "0%" %in% c0)
  c1 <- first_codes(tab(gss, marital, race, pct = "row", digits = 1L))
  testthat::expect_true(all(c1 %in% c("0.0%", "#,##0")) && "0.0%" %in% c1)

  # means: 1 digit -> "#,##0.0"
  testthat::expect_true(all(
    first_codes(tab_num(gss, race, age, digits = 1L)) == "#,##0.0"))

  # a single fmt column, direct: mean 2 digits, and a diff (pct) display -> signed percentage code
  m <- tab_num(gss, race, age, digits = 2L)[["age"]]
  testthat::expect_equal(format(set_display(m, "mean"), syntax = "excel")[[1]], "#,##0.00")
  dcol <- tab(gss, marital, race, pct = "row", digits = 1L)[["Black"]]
  # Phase 13c-v: a pct diff gets an explicit +/- sign; contrib too; a ratio gets a leading x.
  testthat::expect_equal(format(set_display(dcol, "diff"), syntax = "excel")[[1]], "+0.0%;-0.0%")
  ccol <- fmt(n = 1L, ctr = 0.05, type = "row", display = "ctr", digits = 1L)
  testthat::expect_equal(format(ccol, syntax = "excel")[[1]], "+0.0%;-0.0%")
  rcol <- set_digits(set_ratio(set_display(dcol, "rr"), 1.5), 1L)
  # Phase q: the leading multiply sign is BACKSLASH-escaped (\×#,##0.0), not double-quote-wrapped -- a raw
  # " in a formatCode crashes the older jamovi-bundled openxlsx2 ("xml import unsuccessful").
  rr <- format(rcol, syntax = "excel")[[1]]
  testthat::expect_false(grepl('"', rr, fixed = TRUE))
  testthat::expect_match(rr, "^\\\\.#,##0\\.0$")

  # pct_ci (ci = "cell") -> TEXT (the value+CI string is pre-formatted; a documented limitation)
  ci <- tab(gss, marital, race, pct = "row", ci = "cell")
  ci_fmt <- ci[[which(purrr::map_lgl(ci, is_fmt))[[1]]]]
  testthat::expect_true(any(format(ci_fmt, syntax = "excel") == "TEXT"))
})
