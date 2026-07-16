# PURPOSE: Lock tab_transpose() (Phase 10d Part 2) -- the row%/col% axis flip for the
#          column-percentage inversion workflow.
# ROLE: Phase 10d Part 2. tab_transpose() is a NEW function (no byte-identity contract); these tests
#        pin the flag swaps, the value/colour transpose, the test re-keying, and the guards.

gss <- forcats::gss_cat

# === SECTION: structure matches a native pct = "col" table ===================

testthat::test_that("transpose of a row% table == a native col% table (structure + render)", {
  orig   <- tab(gss, marital, race, pct = "row", color = "diff")
  tr     <- tab_transpose(orig)
  native <- tab(gss, race, marital, pct = "col", color = "diff")

  testthat::expect_identical(names(tr), names(native))
  testthat::expect_identical(tab_get_vars(tr)$row_var, "race")
  testthat::expect_identical(tab_get_vars(tr)$col_vars, "marital")
  # axis flags per fmt column
  for (nm in names(tr)[purrr::map_lgl(tr, is_fmt)]) {
    testthat::expect_identical(get_type(tr[[nm]]),   get_type(native[[nm]]))
    testthat::expect_identical(get_col_var(tr[[nm]]), get_col_var(native[[nm]]))
    testthat::expect_identical(is_totcol(tr[[nm]]),  is_totcol(native[[nm]]))
    testthat::expect_identical(is_refcol(tr[[nm]]),  is_refcol(native[[nm]]))
    testthat::expect_identical(vctrs::field(tr[[nm]], "in_totrow"),
                               vctrs::field(native[[nm]], "in_totrow"))
  }
  # rendered markdown is identical
  testthat::expect_identical(tab_md(tr, print = FALSE), tab_md(native, print = FALSE))
})

# === SECTION: values + colours ride along (per-cell fields) ==================

testthat::test_that("cell values transpose: orig[i, j] == transposed[j, i]", {
  orig <- tab(gss, marital, race, pct = "row", color = "diff")
  tr   <- tab_transpose(orig)
  # orig column "White" over marital rows  <->  tr column <marital level> over race rows (White = 3rd)
  white_row <- which(as.character(dplyr::pull(tr, 1)) == "White")
  marital_lv <- "Never married"
  testthat::expect_equal(get_pct(tr[[marital_lv]])[white_row],
                         get_pct(orig[["White"]])[which(as.character(dplyr::pull(orig, 1)) == marital_lv)])
  testthat::expect_equal(get_diff(tr[[marital_lv]])[white_row],
                         get_diff(orig[["White"]])[which(as.character(dplyr::pull(orig, 1)) == marital_lv)])
})

testthat::test_that("per-cell colour codes transpose (same colours as the built row% table)", {
  orig <- tab(gss, marital, race, pct = "row", color = "diff")
  tr   <- tab_transpose(orig)
  # the colour of tr cell (White, Never married) == orig cell (Never married, White)
  o_row <- which(as.character(dplyr::pull(orig, 1)) == "Never married")
  t_row <- which(as.character(dplyr::pull(tr, 1)) == "White")
  o_code <- fmt_channel_codes(orig[["White"]])$text[o_row]
  t_code <- fmt_channel_codes(tr[["Never married"]])$text[t_row]
  testthat::expect_identical(o_code, t_code)
})

# === SECTION: round-trip =====================================================

testthat::test_that("transpose(transpose(x)) restores the original", {
  orig <- tab(gss, marital, race, pct = "row", color = "diff")
  tr2  <- tab_transpose(tab_transpose(orig))
  testthat::expect_identical(names(tr2), names(orig))
  testthat::expect_equal(get_pct(tr2[["White"]]), get_pct(orig[["White"]]))
  testthat::expect_identical(get_type(tr2[["White"]]), get_type(orig[["White"]]))
  testthat::expect_identical(is_totcol(tr2[["Total"]]), is_totcol(orig[["Total"]]))
})

# === SECTION: reference row <-> reference column =============================

testthat::test_that("a reference row becomes a reference column", {
  orig <- tab(gss, marital, race, pct = "row", ref = "first", color = "diff")
  # ref = "first" marks the first race... actually the reference is a row here; find it
  refrow <- which(is_refrow(orig[[names(orig)[purrr::map_lgl(orig, is_fmt)][1]]]))
  tr <- tab_transpose(orig)
  if (length(refrow) == 1) {
    reflab <- as.character(dplyr::pull(orig, 1))[refrow]
    testthat::expect_true(is_refcol(tr[[reflab]]))
  }
  testthat::expect_s3_class(tr, "tabxplor_tab")
})

# === SECTION: test attribute re-keyed ========================================

testthat::test_that("the whole-table test is re-keyed by the new col_var", {
  orig <- tab(gss, marital, race, pct = "row", test = TRUE)
  tr   <- tab_transpose(orig)
  to <- get_test(orig); tt <- get_test(tr)
  if (nrow(to) > 0) {
    testthat::expect_identical(unique(tt$row_var), unique(to$col_var))  # swapped
    testthat::expect_identical(unique(tt$col_var), unique(to$row_var))
  }
  testthat::expect_s3_class(tr, "tabxplor_tab")
})

# === SECTION: guards =========================================================

testthat::test_that("tab_transpose errors on tab_vars / non-tabxplor input", {
  testthat::expect_error(tab_transpose(tab(gss, marital, race, year, pct = "row")),
                         "tab_vars")
  testthat::expect_error(tab_transpose(42), "tabxplor")
})

testthat::test_that("tab_transpose works on a plain table (no totals)", {
  pl <- tab_plain(gss, marital, race, pct = "row")
  tr <- tab_transpose(pl)
  testthat::expect_s3_class(tr, "tabxplor_tab")
  testthat::expect_identical(tab_get_vars(tr)$row_var, "race")
})

# Phase 13c-vi: transpose at export keeps colours (both channels) + numeric means/sd.

testthat::test_that("transpose at export keeps both colour channels (diff + ratio)", {
  t <- tab(forcats::gss_cat, marital, race, pct = "row", color = c("diff", "ratio"))
  h <- as.character(tab_kable(t, engine = "html", transpose = TRUE))
  testthat::expect_true(grepl("color:#", h))            # text-channel colour survives
  testthat::expect_true(grepl("background-color", h))   # background-channel colour survives
})

testthat::test_that("transpose at export keeps numeric means + inline sd + colour", {
  tn <- tab_num(forcats::gss_cat, race, c(age, tvhours), color = "diff")
  md <- tab_md(tn, transpose = TRUE, print = FALSE)
  testthat::expect_true(grepl("]{.", md, fixed = TRUE))       # colour spans present
  testthat::expect_true(grepl(intToUtf8(0x03c3), md))         # inline sigma sd survives
})
