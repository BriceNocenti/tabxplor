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


# === SECTION: several row_vars (Phase 14d) ==================================

testthat::test_that("transpose works with several row_vars: each becomes a col_var", {
  # Before Phase 14d this aborted with a message about `tab_vars` the table did not have: after
  # tab_compact() the roles were guessed from the columns, and the merge's own `row_var` meta column
  # read as a tab_var. The roles are recorded now, so the guard tells the truth.
  t  <- tab(gss, c(marital, relig), race, pct = "row")
  tr <- tab_transpose(t)

  v <- tab_get_vars(tr)
  testthat::expect_setequal(v$col_vars, c("marital", "relig"))   # old row_vars -> col_vars
  testthat::expect_equal(v$row_var, "race")                      # old col_var  -> row_var
  testthat::expect_length(v$tab_vars, 0L)
  testthat::expect_false(isTRUE(get_vars_attr(tr)$compacted))    # the merged shape is undone

  # every ROW of the merged table survives as its own column (`levels(t$levels)` would be the union of
  # both row_vars' levels, which can include one with no rows at all)
  testthat::expect_length(setdiff(names(tr), "race"), nrow(t))
  # ONE total + reference column per sub-table (not one per table)
  totc <- names(tr)[purrr::map_lgl(tr, ~ is_fmt(.) && is_totcol(.))]
  testthat::expect_setequal(totc, c("Total_marital", "Total_relig"))
  testthat::expect_setequal(names(tr)[purrr::map_lgl(tr, ~ is_fmt(.) && is_refcol(.))], totc)
  # each column's col_var is the variable its rows came from
  testthat::expect_equal(get_col_var(tr[["Total_marital"]]), "marital")
  testthat::expect_equal(get_col_var(tr[["Total_relig"]]),   "relig")
  # values: the transposed cell equals the original one
  testthat::expect_equal(get_num(tr[["Divorced"]])[match("Black", as.character(tr$race))],
                         get_num(t[["Black"]])[which(as.character(t$levels) == "Divorced" &
                                                       as.character(t$row_var) == "marital")])
})

testthat::test_that("transpose suffixes ONLY levels shared by two row_vars", {
  t  <- tab(gss, c(marital, relig), race, pct = "row")
  tr <- tab_transpose(t)
  # "Total" (and "No answer") exist under both -> suffixed; "Divorced" is marital's alone -> bare
  testthat::expect_true(all(c("Total_marital", "Total_relig", "No answer_marital") %in% names(tr)))
  testthat::expect_true("Divorced" %in% names(tr))
  testthat::expect_false("Divorced_marital" %in% names(tr))
})

testthat::test_that("a REAL tab_var still aborts, and says so truthfully", {
  testthat::expect_error(tab_transpose(tab(gss, marital, race, year, pct = "row")), "tab_vars")
})

testthat::test_that("transposed row% renders exactly like a native col% table (Phase 14d)", {
  # The extras are ORIENTED: add_n is a column under row%, a ROW under col%. Materialising before the
  # transpose baked the wrong one in ("100% (n=849)" in-cell instead of an `n` row).
  transposed <- tab_md(tab(gss, marital, race, pct = "row"), transpose = TRUE,
                       print = FALSE, color = FALSE)
  native     <- tab_md(tab(gss, race, marital, pct = "col"), print = FALSE, color = FALSE)
  testthat::expect_identical(transposed, native)
})
