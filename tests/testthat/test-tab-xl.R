# PURPOSE: the Excel workbook a user opens: numbers stay numbers, and the format code carries the rest.
# ROLE: the shipped CONTRACT for R/tab_xl.R, R/tab-xl-backend.R -- a failure here is a user-visible change.
#   The exhaustive sweep around it lives in dev/tests/testthat/.
# See: CLAUDE.md section "Testing" (what belongs in which suite).

# === SECTION: the Excel workbook a user opens =====================================================

xl_numfmt_codes <- function(f) {
  st <- openxlsx2::wb_load(f)$styles_mgr$styles$numFmts
  if (!length(st)) return(character(0))
  sub('".*$', "", sub('^.*formatCode="', "", st))
}



# the merged ranges, as plain A1 strings
xl_merges <- function(wb) sub('".*$', "", sub('^.*ref="', "", unlist(wb$worksheets[[1]]$mergeCells)))




testthat::test_that("tab_xl creates an Excel file", {
  testthat::skip_if_not_installed("openxlsx2")
  tabs <-
    purrr::pmap(
      tibble::tribble(
        ~row_vars, ~col_vars      , ~pct , ~filter              , ~subtext               ,
        "race"  , "marital"       , "row", NULL                 , "Source: GSS 2000-2014",
        "relig" , c("race", "age"), "row", "year %in% 2000:2010", "Source: GSS 2000-2010",
        NA_character_, "race"     , "no" , NULL                 , "Source: GSS 2000-2014",
      ),
      .f = tab,
      data = fx_gss(), color = "auto", test = TRUE)

  test_path <- file.path(tempdir(), "tab_xl_test.xlsx")

  tabs |>
    tab_xl(path = test_path, sheets = "unique",
           replace = TRUE, open = FALSE) |>
    testthat::expect_invisible()

 testthat::expect_true(file.exists(test_path))

 file.remove(test_path)
})




testthat::test_that("OR exports as 1/x text by default, numbers with or_numeric = TRUE", {
  testthat::skip_if_not_installed("openxlsx2")
  d  <- fx_gss()
  d$married <- factor(ifelse(d$marital == "Married", "yes", "no"))
  # ⚠ `empirical = FALSE`: what is under test is the multiplicative READING VALUE, and it is read
  # off the sheet's last column -- which the default crude companion turns into the `adj%` aside.
  tl <- tab_reg(d, "married", c("race", "relig"), empirical = FALSE)
  tmp <- tempfile(fileext = ".xlsx"); tab_xl(tl, path = tmp, open = FALSE, replace = TRUE)
  # Phase 18z13: column 2 is the per-level `n` (add_n = TRUE by default); the OR column follows it.
  xl_col <- function(f) {
    df <- openxlsx2::wb_to_df(openxlsx2::wb_load(f), col_names = FALSE)
    df[[ncol(df)]]
  }
  # THE CELL IS A NUMBER AND STILL READS "1/x": it holds the signed fold, and a two-section number
  # format prints the "1/" on the negative side (Excel drops the minus).
  or_col <- suppressWarnings(as.numeric(xl_col(tmp)))
  testthat::expect_true(any(!is.na(or_col) & or_col >  0))
  testthat::expect_true(any(!is.na(or_col) & or_col < -1))                 # a fold below the neutral
  codes <- xl_numfmt_codes(tmp)
  testthat::expect_true(any(grepl("1\\/", codes, fixed = TRUE)))            # ... printed by the code
  # `ratio_cells = "raw"` keeps the untransformed ratio instead: every value strictly positive
  tmp2 <- tempfile(fileext = ".xlsx")
  tab_xl(tl, path = tmp2, open = FALSE, replace = TRUE, ratio_cells = "raw")
  raw <- suppressWarnings(as.numeric(xl_col(tmp2)))
  testthat::expect_true(any(!is.na(raw) & raw > 0))
  testthat::expect_false(any(!is.na(raw) & raw < 0))
  # ... and "text" restores the exact console string
  tmp3 <- tempfile(fileext = ".xlsx")
  tab_xl(tl, path = tmp3, open = FALSE, replace = TRUE, ratio_cells = "text")
  testthat::expect_true(any(grepl("1/", xl_col(tmp3), fixed = TRUE)))
})





# === Phase 22g-vii: widths measured from the content, per SHEET ====================================

testthat::test_that("tab_xl fits each column to what its cells show, and per sheet", {
  testthat::skip_if_not_installed("openxlsx2")
  wids <- function(f) {
    cols <- openxlsx2::wb_load(f)$worksheets[[1]]$cols_attr
    lo <- as.integer(sub('.*min="(\\d+)".*', "\\1", cols))
    hi <- as.integer(sub('.*max="(\\d+)".*', "\\1", cols))
    w  <- as.double(sub('.*width="([0-9.]+)".*', "\\1", cols))
    vapply(seq_len(max(hi)), function(i) w[which(lo <= i & hi >= i)][1], double(1))
  }
  a <- car_arrests
  t <- tab(a, colour, released, pct = "row", ref = "first") |>
    dplyr::mutate(odds_ratio = set_display(.data$Yes, "odds_ratio"))
  p <- withr::local_tempfile(fileext = ".xlsx")
  suppressMessages(tab_xl(t, path = p, open = FALSE, replace = TRUE))
  w <- wids(p)
  # the row-label column used to be a hard-coded 30 whatever it held ("colour" is six characters)
  testthat::expect_lt(w[[1]], 10)
  # ... and a column showing "1/2.11" is wider than one showing "26%"
  testthat::expect_gt(w[[length(w) - 1L]], w[[2]])

  # A COLUMN BELONGS TO THE SHEET: two tables stacked must both fit, where the last used to win
  narrow <- tab(a, colour, released, pct = "row")
  wide   <- tab(dplyr::rename(a, a_deliberately_long_row_variable = "colour"),
                a_deliberately_long_row_variable, released, pct = "row")
  p2 <- withr::local_tempfile(fileext = ".xlsx")
  suppressMessages(tab_xl(list(wide, narrow), path = p2, sheets = "unique", open = FALSE,
                          replace = TRUE))
  p3 <- withr::local_tempfile(fileext = ".xlsx")
  suppressMessages(tab_xl(wide, path = p3, open = FALSE, replace = TRUE))
  testthat::expect_equal(wids(p2)[[1]], wids(p3)[[1]])
})





testthat::test_that("the shape table lies over the main grid, and a check picture is not crowded", {
  testthat::skip_if_not_installed("openxlsx2")
  d <- fx_reg_df(); d$married <- as.integer(d$marital == "Married")
  m <- tab_reg(d, "married", c("race", "age", "tvhours"), family = "binomial")
  testthat::skip_if(is.null(tabxplor:::reg_shape_table(m)))
  f <- withr::local_tempfile(fileext = ".xlsx")
  suppressMessages(tab_xl(m, path = f, replace = TRUE, open = FALSE, check = "auto"))
  wb <- openxlsx2::wb_load(f)
  mg <- xl_merges(wb)

  # THE FIRST SHAPE COLUMN TAKES THE INDEX BLOCK (it holds a formula, and lands under the row
  # labels); the middle ones take two data columns, a data column being one number wide; the LAST --
  # the curve -- takes three, a twenty-glyph run not fitting in two.
  df  <- openxlsx2::wb_to_df(wb, sheet = 1, col_names = FALSE)
  hdr <- which(apply(df, 1, function(r) any(!is.na(r) & grepl("numeric predictor", r))))
  testthat::expect_length(hdr, 1L)
  row <- as.integer(rownames(df)[hdr])
  testthat::expect_true(paste0("A", row, ":B", row) %in% mg)     # the 2-column index block
  testthat::expect_true(paste0("C", row, ":D", row) %in% mg)     # a middle column: two
  testthat::expect_true(paste0("G", row, ":I", row) %in% mg)     # the curve: three, past the edge

  # the picture carries its own title, so nothing is written above it -- and the gap under it is
  # the one constant both the budget and the writer read.
  testthat::expect_gte(tabxplor:::XL_CHECK_GAP, 4L)
  testthat::expect_equal(
    tabxplor:::xl_check_block(list(height = 5.8)),
    as.integer(ceiling(5.8 * 72 / 15)) + tabxplor:::XL_CHECK_GAP)
})


# === SECTION: the A1 ranges and the numFmt escaping ===============================================

test_that("int_to_col / xl_cell produce A1 references", {
  expect_identical(int_to_col(c(1L, 26L, 27L, 28L, 52L, 53L)),
                   c("A", "Z", "AA", "AB", "AZ", "BA"))
  expect_identical(xl_cell(3L, 2L), "B3")
  expect_identical(xl_cell(1L, 1L), "A1")
})




test_that("xl_runs compresses to contiguous runs", {
  expect_identical(xl_runs(c(2, 3, 4, 7, 8)), list(c(2L, 4L), c(7L, 8L)))
  expect_identical(xl_runs(5L), list(c(5L, 5L)))
  expect_identical(xl_runs(c(4, 2, 3, 2)), list(c(2L, 4L)))  # unsorted + duplicate
  expect_identical(xl_runs(integer(0)), list())
})




test_that("xl_coalesce merges same-row columns into blocks", {
  expect_identical(xl_coalesce(c(3L, 4L, 5L), c(2L, 2L, 2L)), "C2:E2")   # row-run shared -> block
  expect_identical(xl_coalesce(rep(2L, 3), 3:5), "B3:B5")                # a full column run
  expect_identical(xl_coalesce(c(3L, 4L), c(2L, 5L)), "C2,D5")          # distinct runs -> separate
  expect_identical(xl_coalesce(integer(0), integer(0)), NA_character_)
})




test_that("numFmt literals are backslash-escaped, never double-quote-wrapped (Phase q)", {
  # A raw " inside <numFmt formatCode="..."/> is left unescaped by the older jamovi-bundled openxlsx2,
  # so its own read_xml round-trip rejects the fragment ("xml import unsuccessful") -- the Windows-side
  # Excel-export crash. xl_numfmt_literal() escapes each character with a backslash instead (XML-safe on
  # every openxlsx2 version, identical Excel rendering).
  mult <- "\u00d7"                                             # the multiply sign
  expect_identical(xl_numfmt_literal("***"), "\\*\\*\\*")
  expect_identical(xl_numfmt_literal(mult), paste0("\\", mult))
  expect_identical(xl_numfmt_literal(""), "")                  # empty passes through
  expect_false(grepl('"', xl_numfmt_literal(" (Chi2)")))       # never emits a quote

  # the multiplicative code path folds BOTH glyphs -> one section per side of the neutral, no raw quote
  code <- excel_numfmt_code(digits = 1L, pct = FALSE, ci = FALSE, text = FALSE,
                            mult = TRUE, mult_over = mult, mult_under = "\u00f7")
  expect_false(grepl('"', code))
  expect_true(grepl(mult, code, fixed = TRUE))
  expect_identical(length(strsplit(code, ";", fixed = TRUE)[[1]]), 2L)
  # ... and an affix lands on EVERY section, which is what puts the stars on both signs
  expect_identical(xl_numfmt_affix("+0.0%;-0.0%", suffix = "*"), "+0.0%\\*;-0.0%\\*")
})





# === SECTION: format() and the Excel numFmt show the same number ==================================

parity_num_from_str <- function(s) {
  s <- sub("\\(.*$", "", s)          # drop " (sigma ...)" for means
  # a multiplicative cell prints its DISTANCE from the neutral: below it, the inverse ("1/2.67",
  # "\u00f72.67"). Excel keeps the raw number, so undo the inversion before comparing.
  inv <- grepl("^\\s*(1/|\u00f7)", s)
  s <- sub("^\\s*(1/|\u00f7)", "", s)   # drop the inverse marker BEFORE the digit scrub
  s <- gsub("[^0-9.+-]", "", s)      # keep digits, decimal point, sign; drop %, spaces, *, etc.
  v <- suppressWarnings(as.numeric(s))
  if (isTRUE(inv) && !is.na(v) && v != 0) 1 / v else v
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




gss <- fx_gss()




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
  ccol <- fmt(n = 1L, ctr = 0.05, scale = "level_pct", pct_type = "row", display = "ctr", digits = 1L)
  testthat::expect_equal(format(ccol, syntax = "excel")[[1]], "+0.0%;-0.0%")
  rcol <- set_digits(set_ratio(set_display(dcol, "rr"), 1.5), 2L)
  # A MULTIPLICATIVE CELL HOLDS ITS READING VALUE, so the code has one section per side of the
  # neutral: the positive one prints the over glyph, the negative one the under glyph, and Excel drops
  # the minus it was not asked for. The glyphs are BACKSLASH-escaped (\×#,##0.0), never
  # double-quote-wrapped -- a raw " in a formatCode crashes the older jamovi-bundled openxlsx2.
  rr <- format(rcol, syntax = "excel")[[1]]
  testthat::expect_false(grepl('"', rr, fixed = TRUE))
  testthat::expect_match(rr, "^\\\\.#,##0\\.00;\\\\.#,##0\\.00$")
  # the value the workbook holds: above the neutral as it stands, below it the signed fold
  testthat::expect_equal(fmt_excel_value(rcol)[[1]], 1.5)
  testthat::expect_equal(fmt_excel_value(set_ratio(rcol, 0.5))[[1]], -2)
  testthat::expect_equal(fmt_excel_value(set_ratio(rcol, 0.5), fold = FALSE)[[1]], 0.5)

  # pct_ci (ci = "cell") -> TEXT (the value+CI string is pre-formatted; a documented limitation)
  ci <- tab(gss, marital, race, pct = "row", ci = "cell")
  ci_fmt <- ci[[which(purrr::map_lgl(ci, is_fmt))[[1]]]]
  testthat::expect_true(any(format(ci_fmt, syntax = "excel") == "TEXT"))
})


test_that("a figure column is wide enough for its own BOLD ink", {
  testthat::skip_if_not_installed("openxlsx2")
  g <- fx_gss()
  g$income25k <- forcats::fct_lump_n(g$rincome, 3)
  g$party3    <- forcats::fct_lump_n(g$partyid, 3)
  t <- tab(g, income25k, party3, pct = "row")
  f <- withr::local_tempfile(fileext = ".xlsx")
  suppressMessages(tab_xl(t, path = f, replace = TRUE, open = FALSE))
  wb <- openxlsx2::wb_load(f)
  # `cols_attr` is one entry per RANGE, not per column
  ca  <- wb$worksheets[[1]]$cols_attr
  num <- function(a, x) as.numeric(sub(paste0('.*', a, '="([0-9.]+)".*'), "\\1", x))
  w   <- rep(NA_real_, max(num("max", ca)))
  for (k in seq_along(ca)) w[num("min", ca[[k]]):num("max", ca[[k]])] <- num("width", ca[[k]])

  rd   <- tabxplor:::tab_export_prep(t, backend = "xl")$tables[[1]]
  bold <- seq_len(nrow(rd$tab)) %in% (rd$bold_rows %||% integer(0))
  # ⚠ asserted against the ratios, never a hard-coded width: the constants may move, the invariant
  # ("every figure fits, bold included") may not. The base font's digit is the width unit; the
  # number font's is XL_NUM_RATIO of it, and a bold one XL_BOLD_RATIO more.
  for (j in which(purrr::map_lgl(rd$tab, is_fmt))) {
    body <- format(rd$tab[[j]], special_formatting = FALSE, na = "", stars = TRUE)
    need <- max(nchar(body) * tabxplor:::XL_NUM_RATIO *
                  ifelse(bold[seq_along(body)], tabxplor:::XL_BOLD_RATIO, 1))
    expect_gte(w[[j]], need)
  }
})


# *An Excel bar that scaled itself would say a different length from the html one under the same
# figure: Excel's own min/max would read the Total rows the html bar excludes.*
testthat::test_that("set_bars() writes a dataBar over the data rows, on pinned bounds", {
  testthat::skip_if_not_installed("openxlsx2")
  t <- set_bars(tab(fx_gss(), race, marital, pct = "row"), "Married")
  f <- withr::local_tempfile(fileext = ".xlsx")
  tab_xl(t, path = f, open = FALSE, replace = TRUE)
  cf <- openxlsx2::wb_load(f)$worksheets[[1]]$conditionalFormatting
  testthat::expect_equal(nrow(cf), 1L)
  testthat::expect_match(cf$cf, 'type="dataBar"')
  # the bounds are PINNED to the ceiling the prep resolved, never left to Excel
  testthat::expect_match(cf$cf, '<cfvo type="num" val="0"/>', fixed = TRUE)
  testthat::expect_match(cf$cf, paste0('val="', max(get_num(t$Married)[!is_totrow(t$Married)])),
                         fixed = TRUE)
  # ...over the DATA rows only: three of the four rows of that one column
  rng <- strsplit(cf$sqref, ":", fixed = TRUE)[[1]]
  testthat::expect_identical(sub("[0-9]+$", "", rng[[1]]), sub("[0-9]+$", "", rng[[2]]))
  testthat::expect_identical(diff(as.integer(sub("^[A-Z]+", "", rng))), 2L)
  # the ink is the chrome's accent, the same one the stylesheet gives an uncoloured bar
  testthat::expect_match(cf$cf, toupper(sub("#", "", tx_chrome_hex("light")$accent)))

  # THE GATE: Excel draws the value it HOLDS, so a magnitude bar over signed values is refused
  # rather than drawn at another length than the html one.
  tn <- set_bars(set_display(tab(fx_gss(), race, marital, pct = "row"), "diff"), "Married")
  f2 <- withr::local_tempfile(fileext = ".xlsx")
  tx_reset_messages()
  testthat::expect_message(tab_xl(tn, path = f2, open = FALSE, replace = TRUE), "No data bar")
  testthat::expect_length(openxlsx2::wb_load(f2)$worksheets[[1]]$conditionalFormatting, 0L)
})
