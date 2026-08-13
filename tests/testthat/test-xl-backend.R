# Phase 10h: pure unit tests for the openxlsx2 range coalescers (no openxlsx2 needed).
# These functions turn per-cell style targets into the fewest multi-area A1 `dims`, the shared-style
# performance lever. They are base-R only (A1 math reimplemented) so they test in isolation.

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

test_that("xl_coalesce covers exactly the target cells", {
  # a 5-col numeric block, all sharing one numFmt over data rows 3:10 -> a single rectangle
  cols <- rep(3:7, each = 8L); rows <- rep(3:10, times = 5L)
  expect_identical(xl_coalesce(cols, rows), "C3:G10")
})

test_that("xlb_dims_each splits a multi-area dims into single ranges", {
  # Phase o: xl_coalesce emits comma-joined multi-area dims (e.g. "C7:E8,F4:F8"); the older jamovi-
  # bundled openxlsx2 rejects those. xlb_dims_each fans them out to single A1:B2 ranges at the emit.
  got <- character(0)
  xlb_dims_each("A1:B2,C3:D4", function(d) got <<- c(got, d))
  expect_identical(got, c("A1:B2", "C3:D4"))

  # a single range passes through as one call
  got <- character(0)
  xlb_dims_each("C3:G10", function(d) got <<- c(got, d))
  expect_identical(got, "C3:G10")

  # NA / empty / non-scalar -> no call at all (the "nothing to style" path)
  got <- character(0)
  xlb_dims_each(NA_character_, function(d) got <<- c(got, d))
  xlb_dims_each("",            function(d) got <<- c(got, d))
  xlb_dims_each(character(0),  function(d) got <<- c(got, d))
  expect_identical(got, character(0))
})

test_that("xlb_set_cell_style / xlb_numfmt survive the OLDER openxlsx2 single-range dims validator", {
  # Reproduce the jamovi-bundled openxlsx2 (the Excel-export crash): a `dims` with a comma raises
  # "dims must be something like A1 or A1:B2.". A stub wb whose engine methods enforce that rule fails
  # WITHOUT the xlb_dims_each split and passes WITH it (each single range applied in turn).
  seen <- list(style = character(0), numfmt = character(0))
  reject_multi <- function(dims) if (grepl(",", dims, fixed = TRUE))
    stop("Invalid input: dims must be something like A1 or A1:B2.")
  wb <- new.env()
  wb$set_cell_style <- function(sheet, dims, style) { reject_multi(dims); seen$style  <<- c(seen$style,  dims) }
  wb$add_numfmt     <- function(sheet, dims, numfmt) { reject_multi(dims); seen$numfmt <<- c(seen$numfmt, dims) }

  expect_no_error(xlb_set_cell_style(wb, "s", "A1:B2,C3:D4", 7L))
  expect_no_error(xlb_numfmt(wb, "s", "C7:E8,F4:F8", "0.0%"))
  expect_identical(seen$style,  c("A1:B2", "C3:D4"))
  expect_identical(seen$numfmt, c("C7:E8", "F4:F8"))
})

test_that("xlb_na_argname resolves the exact NA formal across openxlsx2 versions (Phase q)", {
  # The older jamovi-bundled openxlsx2's add_data NA formal is `na.strings` (dot). Guessing `na_strings`
  # made the arg UNUSED -> the default #N/A error filled empty summary-stat / p-value cells. Read the real
  # name off the method's own formals: na (current) / na_strings / na.strings (oldest).
  mk <- function(f) { e <- new.env(); e$add_data <- f; e }
  expect_identical(xlb_na_argname(mk(function(name, na.strings) NULL)), "na.strings")
  expect_identical(xlb_na_argname(mk(function(x, na_strings) NULL)),    "na_strings")
  expect_identical(xlb_na_argname(mk(function(x, na) NULL)),            "na")
})

test_that("xl_materialize_data blanks NaN so Excel shows an empty cell, not #VALUE! (Phase q)", {
  # openxlsx2 renders a NaN numeric cell as an Excel error even when NA is blanked (the na arg only covers
  # NA), so an empty summary cell that computes to NaN must be coerced to NA before the write.
  x  <- fmt(n = 1L, mean = NaN, scale = "level_mean", display = "mean", digits = 1L)
  tb <- tibble::tibble(v = x)
  out <- xl_materialize_data(tb, fmt_cols = 1L, text_fmt_cols = integer(0))
  expect_true(is.na(out$v))
  expect_false(is.nan(out$v))
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

  # the ratio code path (excel_numfmt_code ratio = TRUE) folds the multiply sign -> no raw quote
  code <- excel_numfmt_code(digits = 1L, pct = FALSE, ci = FALSE, text = FALSE, ratio = TRUE)
  expect_false(grepl('"', code))
  expect_true(grepl(mult, code, fixed = TRUE))
})


# ---- sheet-name sanitisation -----------------------------------------------------------------

test_that("xl_clean_sheet_name replaces every Excel-illegal character with a space", {
  expect_identical(xl_clean_sheet_name("a/b"),   "a b")
  expect_identical(xl_clean_sheet_name("a?b"),   "a b")
  expect_identical(xl_clean_sheet_name("a*b"),   "a b")
  expect_identical(xl_clean_sheet_name("a:b"),   "a b")
  expect_identical(xl_clean_sheet_name("a[b]c"), "a b c")
  # the real driver: Phase 12d names OR columns "<level> vs <ref>: OR"
  expect_identical(xl_clean_sheet_name("Married vs Never: OR"), "Married vs Never  OR")
  # a legal name is untouched (so ordinary sheet titles are byte-identical)
  expect_identical(xl_clean_sheet_name("marital x race"), "marital x race")
  expect_identical(xl_clean_sheet_name(c("a/b", "ok")), c("a b", "ok"))
})

test_that("xl_clean_sheet_name leaves openxlsx2 nothing to fix (no 'illegal characters' warning)", {
  skip_if_not_installed("openxlsx2")
  # The point of the helper: openxlsx2 would silently apply this same substitution and warn.
  # Feeding it a pre-cleaned name must therefore be both silent AND a no-op.
  for (nm in c("Married vs Never: OR", "a/b", "a[b]c", "marital x race")) {
    clean <- xl_clean_sheet_name(nm)
    wb    <- openxlsx2::wb_workbook()
    expect_no_warning(wb$add_worksheet(sheet = clean))
    expect_identical(unname(utils::tail(wb$get_sheet_names(), 1)), clean)
  }
})
