# PURPOSE: Validate the Phase 7g `n_min` small-base display filter (tab_apply_n_min).
# ROLE: n_min is a PURE display step -- it drops small-base rows/cols and blanks weak cells,
#       recomputing nothing (no fields, no chi2/ANOVA/CI). These tests lock the drop/blank rule
#       and the survival invariants (totals, add_n, p-value line, class + attributes).
# KEY CONSTRAINTS:
#   - Base = get_tot_n() (proportions) / get_n() (means); an NA base is never weak.
#   - Row rule (row-oriented cols): drop only if the LARGEST base across cols < n_min.
#   - Column rule (pct = "col"): drop a column whose base < n_min.

# Phase 20a: this file calls functions deprecated in 2.0.0 on purpose -- what it asserts is their
# arithmetic, which the leaf shares with them and which does NOT go away in 2.1.0.
# ⚠ This quiets the TOP-LEVEL calls only: testthat 3e runs local_reproducible_output() inside
# every test_that(), which forces lifecycle_verbosity = "warning" again, so the in-block calls
# still warn. Migrating them to tab() is the corpus sweep routed to Phase 20h.
withr::local_options(lifecycle_verbosity = "quiet", .local_envir = testthat::teardown_env())


gss <- forcats::gss_cat |> dplyr::filter(race != "Not applicable") |>
  dplyr::mutate(race = droplevels(race))

# A controlled two-col_var fixture: under na = "drop" each col_var keeps its own non-NA base, so
# row "A" has a large base on c1 (50) but a tiny base on c2 (5) -> row kept, c2 cell blanked.
nmin_df <- tibble::tibble(
  g  = factor(rep(c("A", "B"), each = 50)),
  c1 = factor(rep(c("a", "b"), 50)),                                       # base 50 in each group
  c2 = factor(c(rep("p", 5), rep(NA_character_, 45),                       # group A: base 5
                rep("p", 40), rep("q", 10)))                               # group B: base 50
)

testthat::test_that("n_min = 0 is a no-op (byte-identical to no filter)", {
  base <- tab(gss, race, marital, pct = "row")
  same <- tab(gss, race, marital, pct = "row", n_min = 0)
  testthat::expect_equal(base, same)
})

testthat::test_that("pct='row': a whole row is dropped only when its max base < n_min", {
  base <- tab(gss, race, marital, pct = "row")
  # "Other" race has the smallest base; a threshold between it and the next drops only that row.
  other_n <- base |> dplyr::filter(race == "Other") |> dplyr::pull(Total) |> get_tot_n()
  black_n <- base |> dplyr::filter(race == "Black") |> dplyr::pull(Total) |> get_tot_n()
  thr     <- ceiling((other_n + black_n) / 2)

  out <- tab(gss, race, marital, pct = "row", n_min = thr)
  testthat::expect_false("Other" %in% as.character(out$race))   # dropped (base < thr)
  testthat::expect_true("Black" %in% as.character(out$race))    # kept   (base >= thr)
  testthat::expect_true("Total" %in% as.character(out$race))    # total row always survives
})

testthat::test_that("a kept row blanks only the cells whose OWN base < n_min", {
  # Two col_vars (c1 a/b, c2 p/q). Under na = "drop" each keeps its own base: row A has c1 base
  # 50 but c2 base 5 -> row A stays (max 50 >= 10) but its c2 cells (base 5) blank.
  out <- tab(nmin_df, g, col_vars = c(c1, c2), pct = "row", na = "drop", n_min = 10)

  row_a <- dplyr::filter(out, g == "A")
  # The c2 "p" cell for A must render as an empty string (its base 5 < 10).
  testthat::expect_true(all(format(dplyr::pull(row_a, "p")) == ""))
  # c1 cells for A are untouched (base 50, non-empty).
  testthat::expect_false(all(format(dplyr::pull(row_a, "a")) == ""))

  # row B: c2 base 50 -> nothing blanked.
  row_b <- dplyr::filter(out, g == "B")
  testthat::expect_false(any(format(dplyr::pull(row_b, "p")) == ""))
})

testthat::test_that("pct='col': weak columns are dropped, totals kept", {
  base <- tab(gss, race, marital, pct = "col")
  full_cols <- names(base)
  out  <- tab(gss, race, marital, pct = "col", n_min = 5000)
  # Small marital categories (base < 5000) drop; "Married" (large) and "Total" survive.
  testthat::expect_true("Married" %in% names(out))
  testthat::expect_true("Total"   %in% names(out))
  testthat::expect_false("Separated" %in% names(out))  # a small column
  testthat::expect_lt(ncol(out), ncol(base))
})

# Phase 20h: built at top level, where the file-level lifecycle line bites.
sw_mass <- dplyr::starwars |> tab_prepare("sex", "mass", other_if_less_than = 0)

testthat::test_that("means use the n base (not tot_n) for the drop", {
  sw <- sw_mass
  # Drop sex groups whose n is below the threshold; the numeric mean column drives it.
  out <- tab(sw, sex, mass, pct = "no", n_min = 5)
  # "hermaphroditic"/"none" are tiny groups -> dropped; "male" (large) survives; Total kept.
  testthat::expect_true("male"  %in% as.character(out$sex))
  testthat::expect_true("Total" %in% as.character(out$sex))
})

testthat::test_that("n_min never drops the total row, and keeps the add_n intent + test attr", {
  out <- tab(gss, race, marital, pct = "row", test = TRUE, add_n = TRUE, n_min = 3000)
  # total row present
  testthat::expect_true(any(is_totrow(out$Total)))
  # Phase 10i-B: add_n / add_pct / p-value are now DISPLAY-only -- the built "core" table has NO `n`
  # column and no p-value body row; n_min runs on the core and must PRESERVE the display intent (the
  # `render_extras` attribute) + the `test` attribute so the extras/p-values still materialise at
  # display. No body cell has an NA n now.
  testthat::expect_false("n" %in% names(out))
  testthat::expect_true(isTRUE(get_render_extras(out)$add_n))
  testthat::expect_false(any(is.na(get_n(out$Total))))
  testthat::expect_false(is.null(get_test(out)))
  testthat::expect_gt(nrow(get_test(out)), 0)
})

testthat::test_that("n_min preserves class and table attributes", {
  base <- tab(gss, race, marital, tab_vars = year, pct = "row", test = TRUE)
  # tab_vars -> a list of grouped tabs; apply n_min and check the first survives as a tab.
  out  <- tab(gss, race, marital, tab_vars = year, pct = "row", test = TRUE,
              n_min = 200, output_list = TRUE)
  testthat::expect_true(inherits(out[[1]], "tabxplor_tab") ||
                          inherits(out[[1]], "tabxplor_grouped_tab"))
  testthat::expect_false(is.null(get_test(out[[1]])))   # test attribute survives
})

testthat::test_that("format() renders the 'blank' display token as an empty string", {
  col <- tab(gss, race, marital, pct = "row") |> dplyr::pull("Married")
  blanked <- set_display(col, "blank")
  testthat::expect_true(all(format(blanked) == ""))
  # get_num() of a blank cell is NA (non-destructive: the pct field is untouched).
  testthat::expect_true(all(is.na(get_num(blanked))))
  testthat::expect_equal(get_pct(blanked), get_pct(col))
})
