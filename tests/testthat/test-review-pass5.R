# Phase 18m -- fixtures for the maintainer's manual review pass 5. Each asserts a specific fix so it
# fails on the pre-fix code. gss_simple = the maintainer's fixture (merged levels, first-level refs).

gss5 <- suppressWarnings(gss_cat_data_formatting())

# ---- Item 1: common_totrow (opt-in shared Total in its own group, bold when it is a reference) --------
testthat::test_that("common_totrow: default keeps one Total per row_var; TRUE shares one in its own group", {
  # default FALSE -> one Total row per row_var
  t0 <- tab(gss5, c(race, relig), party3, pct = "row", na = "drop_all")
  testthat::expect_equal(sum(is_totrow(dplyr::ungroup(t0))), 2L)

  # TRUE -> the collapse fires at display; the shared Total lands in a blank-row_var group
  t1 <- tab(gss5, c(race, relig), party3, pct = "row", na = "drop_all", common_totrow = TRUE)
  m  <- suppressMessages(tabxplor:::tab_materialize_extras(t1, backend = "text", pvalue = FALSE))
  testthat::expect_equal(sum(is_totrow(m)), 1L)
  testthat::expect_equal(as.character(m$row_var)[is_totrow(m)], "")
})

testthat::test_that("common_totrow: the shared Total is bold when ref = 'tot' for some row_var", {
  # race uses ref = "tot" (the total is its reference); the shared Total must render bold
  t <- tab(gss5, c(race, relig), party3, pct = "row", na = "drop_all",
           color = "diff", ref = c("tot", 1), common_totrow = TRUE)
  tb <- tabxplor:::tab_export_prep(t, backend = "md")$tables[[1]]
  testthat::expect_true(nrow(tb$tab) %in% tb$bold_rows)          # last row (the shared Total) is bold
})

# ---- Item 2: ref positional over col_vars under pct = "col" -------------------------------------------
testthat::test_that("pct='col' ref is positional over col_vars (factor->column, numeric->row)", {
  t <- tab(gss5, race, c(party3, marital, tvhours), pct = "col", na = "drop",
           color = "diff", ref = c("tot", 1, 1))
  # each col_var takes ITS OWN `ref`, and the axis follows the kind: party3 (factor) -> the total
  # COLUMN; marital (factor) -> its column 1 "Married"; tvhours (numeric, orthogonal) -> row 1
  # "White". Asserted on the stored facts rather than on the legend prose, which since Phase 22f-i
  # names a non-total reference generically ("the reference category (in bold)").
  f  <- purrr::keep(t, is_fmt)
  cv <- purrr::map_chr(f, get_col_var)
  testthat::expect_true(all(purrr::map_chr(f[cv == "party3"],
                                           ~ as.character(get_ref_type(.))[1]) == "tot"))
  testthat::expect_identical(
    names(f)[cv == "marital" & purrr::map_lgl(f, ~ isTRUE(is_refcol(.)))], "Married")
  testthat::expect_identical(which(is_refrow(f[[which(cv == "tvhours")[1]]])), 1L)
  testthat::expect_identical(as.character(t[[1]])[1], "White")
})

# ---- Item 2b: the pct='col' "n" row renders plain, not bold ------------------------------------------
testthat::test_that("pct='col' n row is plain (not an anchor)", {
  t  <- tab(gss5, race, party3, pct = "col", color = "diff")
  tb <- tabxplor:::tab_export_prep(t, backend = "md")$tables[[1]]
  n_row <- which(tabxplor:::tab_row_roles(tb$tab) == "n")
  testthat::expect_length(n_row, 1L)
  testthat::expect_false(n_row %in% tb$bold_rows)               # the n row is NOT bold
})

# ---- Item 3: md->HTML borders -- a styled table's blanks are nbsp, not stray-bordered :empty ----------
testthat::test_that("styled md: only real spacer columns are :empty (no ragged left / span borders)", {
  t  <- tab(gss5, c(race, relig), c(party3, marital), pct = "row", na = "drop_all", color = TRUE)
  md <- tab_md(t, css = TRUE, print = FALSE)
  # the CSS carries the whole-table top/bottom/right edges (div-aware, md-only)
  testthat::expect_true(grepl("table > thead > tr:first-child", md))
  testthat::expect_true(grepl("table > tbody > tr:last-child", md))
  # a blanked continuation label / span-row cell is a non-breaking space, NOT an empty pipe cell -- so
  # the styled md carries U+00A0 in its body (the pre-fix code left those cells ASCII-blank => :empty).
  body <- grep("^\\|", strsplit(md, "\n")[[1]], value = TRUE)
  testthat::expect_true(any(grepl("\u00a0", body)))             # nbsp present
})

# ---- Item 4: the reg colour legend strips the [dep] disambiguation bracket ---------------------------
testthat::test_that("multi-dependent reg legend drops the [dep] bracket (kept only in console headers)", {
  skip_if_not_installed("broom")
  t  <- suppressWarnings(tab_reg(gss5, outcome = c("married", "tvhours"),
                                 predictors = c("race", "age"),
                                 family = c("binomial", "poisson"), empirical = TRUE))
  md <- tab_md(t, print = FALSE)
  testthat::expect_false(grepl("\\[married\\]", md))            # no bracket in the export legend
  testthat::expect_false(grepl("\\[tvhours\\]", md))
})

# ---- Item 5: binomial measure = "log" + empirical does NOT bold every row -------------------------
testthat::test_that("binomial measure = log empirical bolds only reference rows + footer", {
  skip_if_not_installed("broom")
  t  <- suppressWarnings(tab_reg(gss5, outcome = "married", predictors = c("race", "rincome", "age"),
                                 family = "binomial", empirical = TRUE, measure = "log"))
  tb <- tabxplor:::tab_export_prep(t, backend = "md")$tables[[1]]
  # NOT every row is bold (the pre-fix all-FALSE-ref_alltot edge bolded the whole table)
  testthat::expect_lt(length(tb$bold_rows), nrow(tb$tab))
  testthat::expect_gt(length(tb$bold_rows), 0L)                 # reference rows + footer still bold
})

# ---- Item 6: split_var + one dependent auto-spread -> ONE GOF block, non-empty ------------------------
testthat::test_that("split_var single-dependent auto-spread gives one GOF block keyed to spread columns", {
  skip_if_not_installed("broom")
  t  <- suppressWarnings(tab_reg(gss5, outcome = "married", predictors = c("rincome", "party3"),
                                 tab_vars = "race"))
  tst <- get_test(t)
  # one block: the GOF rows are keyed to the spread columns (the split levels), not tripled by group
  # z13: the global-test rows keep the PREDICTOR in `var` (like the interaction ones) -- they are a
  # table-wide footer LINE, deliberately not re-keyed onto a group's column.
  gof <- tst[!tst$test %in% tabxplor:::reg_global_types(), , drop = FALSE]
  # Phase 19g: the split level rides a column NAMED after the split variable
  testthat::expect_true(all(!nzchar(tabxplor:::test_key_col(gof, "race"))))   # collapsed to one block
  testthat::expect_gt(dplyr::n_distinct(tst$col), 1L)          # spread across the subpopulation columns
})
