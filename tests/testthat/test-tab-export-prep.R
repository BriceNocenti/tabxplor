# PURPOSE: the ephemeral render model every exporter consumes.
# ROLE: the shipped CONTRACT for R/tab-export-prep.R -- a failure here is a user-visible change.
#   The exhaustive sweep around it lives in dev/tests/testthat/.
# See: CLAUDE.md section "Testing" (what belongs in which suite).

# === SECTION: the shared render model =============================================================

gss <- fx_gss()



t_basic <- tab(gss, race, marital, pct = "row", color = "diff", test = TRUE)


t_multi <- tab(gss, race, c(marital, relig), pct = "row", color = "diff")


t_tv    <- tab(gss, race, marital, year, pct = "row", color = "diff")



# === SECTION: render-model shape =============================================

testthat::test_that("tab_export_prep returns a tabxplor_render with tables/meta", {
  p <- tabxplor:::tab_export_prep(t_basic, backend = "kable", wrap = NULL)
  testthat::expect_s3_class(p, "tabxplor_render")
  # Phase 14j: the `labels` slot is gone. tab_export_labels() harvested every column's `label`
  # attribute on every export and nothing ever read the result -- and the source `label` does not
  # survive tab() building anyway, so it was always NULL.
  testthat::expect_named(p, c("tables", "meta"))
  testthat::expect_length(p$tables, 1L)
  rd <- p$tables[[1]]
  # Phase 16e: the plain footer one-liners (reg_line / weight_line / stars_legend) left the prep -- every
  # backend now builds its whole footer via tab_footer_streams(). reg_title (the caption) stays.
  testthat::expect_named(rd, c("tab", "vars", "roles", "ann", "footer_rows", "bold_rows",
                               "bold_cols", "col_var_header", "subtext",
                               "reg_title", "caption", "empirical_tips"))
  testthat::expect_false(rd$vars$degrade)
})



testthat::test_that("roles: fmt_cols / totcols / row_var_col match the built table", {
  rd <- tabxplor:::tab_export_prep(t_basic, backend = "kable", wrap = NULL)$tables[[1]]
  tab <- rd$tab
  testthat::expect_identical(rd$roles$fmt_cols, which(purrr::map_lgl(tab, is_fmt)))
  testthat::expect_identical(rd$roles$totcols, which(is_totcol(tab)))
  testthat::expect_identical(rd$roles$totrows, which(is_totrow(tab)))
  testthat::expect_identical(unname(rd$roles$row_var_col), which(names(tab) == "race"))
})



# === SECTION: graceful degrade ===============================================

testthat::test_that("degrade path flags non-tabxplor inputs with a reason", {
  for (obj in list(iris, tibble::tibble(a = 1:3), datasets::mtcars)) {
    rd <- tabxplor:::tab_export_prep(obj, backend = "md", wrap = NULL)$tables[[1]]
    testthat::expect_true(isTRUE(rd$vars$degrade))
    testthat::expect_type(rd$vars$reason, "character")
  }
})



# === SECTION: base vs list split =============================================

testthat::test_that("a list is NEVER merged at export, even with matching col_vars (Phase 14d)", {
  # It used to be compacted into one render table. `tab()` already merges what it decides to merge;
  # a list reaching an exporter is one the user asked to keep separate (output_list / tab_many /
  # their own list()), so gluing it back together overrode them.
  lst <- list(tab(gss, race, marital, pct = "row", color = "diff"),
              tab(gss, relig, marital, pct = "row", color = "diff"))
  p <- tabxplor:::tab_export_prep(lst, backend = "md", drop_tab_vars = FALSE, wrap = NULL,
                                  list_method = TRUE)
  testthat::expect_length(p$tables, 2L)
  testthat::expect_false(any(purrr::map_lgl(p$tables, ~ isTRUE(.$vars$degrade))))
  # ... while tab() merging its OWN row_vars is untouched: that is a build-time decision.
  testthat::expect_s3_class(tab(gss, c(race, relig), marital, pct = "row"), "tabxplor_tab")
})



testthat::test_that("the title names the DEPENDENT axis first, decided by pct", {
  # Phase 14l: `pct` survives on a built table ONLY as the fmt columns' `type`, so the order is read
  # from there. Under pct="row" a row is a GROUP and the col_var is what is described.
  ti <- function(tabs) {
    rd <- tabxplor:::tab_export_prep(tabs, backend = "xl", list_method = TRUE,
                                     compute = c("refs", "bold"))$tables[[1]]
    tabxplor:::tab_get_titles(rd$tab, rd$vars$row_vars, rd$vars$col_vars, rd$vars$tab_vars)
  }
  testthat::expect_equal(ti(tab(gss, marital, race, pct = "row")), "race by marital")
  # pct="col" swaps the axes back -- the ONLY case that flips
  testthat::expect_equal(ti(tab(gss, c(race, marital), relig, pct = "col")),
                         "race, marital by relig")
  # a mean is always "Y by group", so it must NOT vote for a flip
  testthat::expect_equal(ti(tab(gss, marital, tvhours)), "tvhours by marital")
  testthat::expect_equal(ti(tab(gss, c(race, marital), c(relig, tvhours), pct = "row")),
                         "relig, tvhours by race, marital")
  # counts: no directional type at all -> the dependent-first default
  testthat::expect_equal(ti(tab(gss, marital, race, pct = "no")), "race by marital")
  testthat::expect_equal(ti(tab(gss, marital, race, tab_vars = year, pct = "row")),
                         "race by marital (tabbed by year)")
})



# === Phase 22c-ii: the unit header row =============================================================

testthat::test_that("the unit row names what each column holds, once per block", {
  t   <- tab(gss, race, c(marital, tvhours), pct = "row", color = "diff")
  rd  <- tabxplor:::tab_export_prep(t, backend = "kable", wrap = NULL)$tables[[1]]
  u   <- stats::setNames(rd$col_var_header$unit, names(rd$tab))
  # once per (BLOCK, unit) RUN, in its LEFTMOST column, in the console type tag's own notation
  testthat::expect_identical(unname(u[["No answer"]]), "<row%>")
  testthat::expect_true(all(!nzchar(u[c("Never married", "Separated", "Married")])))
  # a TOTAL column is a block of its own, so it restates the unit its own cell shows -- the base
  # count the reader had no name for
  testthat::expect_identical(unname(u[["Total"]]), "<row% (n_range)>")
  # a numeric col_var is headed "mean"; its default aside, the coefficient of variation, names itself
  # in the cell, so the header drops it and the unit line states the layout once.
  testthat::expect_identical(unname(rd$col_var_header$clean[[which(names(rd$tab) == "tvhours")]]),
                             "mean")
  testthat::expect_identical(unname(u[["tvhours"]]), "<mean (cv)>")
  # the unit says what the column HOLDS even where the header already names the statistic: the
  # console prints both, a name line and a type line, and the exports now do too
  rd2 <- tabxplor:::tab_export_prep(tab(gss, race, c(marital, tvhours), pct = "row",
                                        color = "diff", display = "mean_sd"),
                                    backend = "kable", wrap = NULL)$tables[[1]]
  u2  <- stats::setNames(rd2$col_var_header$unit, names(rd2$tab))
  testthat::expect_identical(unname(rd2$col_var_header$clean[[which(names(rd2$tab) == "tvhours")]]),
                             "mean (sd)")
  testthat::expect_identical(unname(u2[["tvhours"]]), "<mean (sd)>")
})




# === SECTION: the (col_var, col_group) block identity =============================================

gss_fmt <- fx_gss_fmt()



spread_tab <- function(...) {
  d <- dplyr::filter(gss_fmt, year %in% c(2000, 2014))
  tab(d, marital, race, year, pct = "row", spread_vars = year, test = TRUE, color = "diff", ...)
}



test_that("a spread swaps the header bands: the column is the sub-population, the span the variable", {
  sp <- spread_tab()
  h  <- tab_html(sp)
  # the SPAN names the block -- the variable, and its level because `race` gives 3 columns per year
  expect_match(h, '<th class="tx-span"[^>]*>race<br>White</th>', fixed = FALSE)
  expect_match(h, '<th class="tx-span"[^>]*>race<br>Black</th>', fixed = FALSE)
  # ... and the column header names the sub-population, once per column
  expect_match(h, '<th [^>]*>2000</th>', fixed = FALSE)
  expect_match(h, '<th [^>]*>2014</th>', fixed = FALSE)
  # the old shape said the same thing twice: the level rode the column header under a `race` span
  expect_false(grepl('<th class="tx-span"[^>]*>2000<br>race</th>', h))

  # an UNSPREAD table's span is the bare variable name: no stray separator.
  h0 <- tab_html(tab(gss_fmt, marital, race, pct = "row", color = "diff"))
  expect_match(h0, '<th class="tx-span"[^>]*>race</th>', fixed = FALSE)
  expect_false(grepl('<th class="tx-span"[^>]*><br>', h0))
})




# --- Phase 22c-i: what a spread makes of the totals, the base count and the reference -------------

test_that("every total row merges into ONE, under the plain total name", {
  sp <- spread_tab()
  rv <- as.character(sp[[tab_get_vars(sp)$row_var]])
  expect_equal(sum(is_totrow(sp)), 1L)
  expect_true("Total" %in% rv)
  # the total TABLE's own line is not a row of its own: it joins the others, in its own columns
  expect_false(any(grepl("^TOTAL", rv)))
  expect_false(any(grepl("Total Ensemble", rv, fixed = TRUE)))
})



test_that("the base count takes one column per block, and the per-block Total columns go", {
  sp <- spread_tab()
  m  <- tabxplor:::tab_materialize_extras(sp, backend = "text", pvalue = FALSE)
  n_cols <- names(m)[vapply(m, function(x) is_fmt(x) && get_role(x) == "n", logical(1))]
  expect_setequal(n_cols, c("n_2000", "n_2014", "n_Ensemble"))
  # they sit at the RIGHT, so the estimates stay side by side
  expect_equal(tail(names(m), 3), n_cols)
  # four "100 %" columns say nothing once the count lives elsewhere
  expect_false(any(vapply(m, function(x) is_fmt(x) && is_totcol(x) &&
                            get_pct_type(x) == "row", logical(1))))
})



test_that("`spread_vars` alone makes the variable a tab_var, and promotes a total line", {
  d <- dplyr::filter(gss_fmt, year %in% c(2000, 2014))
  tabxplor:::tx_reset_messages()   # the note is once per session
  expect_message(sp <- tab(d, marital, race, pct = "row", spread_vars = year, color = "diff"),
                 "column block")
  # it became a tab_var, then went to column: its levels are the blocks
  expect_true(all(c("2000", "2014") %in%
                    vapply(sp[vapply(sp, is_fmt, logical(1))], get_col_group, character(1))))
  # a total LINE cannot be a block: the promotion gives the Ensemble columns a full table
  expect_true(any(vapply(sp, function(x) is_fmt(x) && get_col_group(x) == "Ensemble", logical(1))))
  ens <- names(sp)[vapply(sp, function(x) is_fmt(x) && get_col_group(x) == "Ensemble", logical(1))]
  expect_true(all(!is.na(get_pct(sp[[ens[[1]]]]))))
})

test_that("a compacted table keeps its tab_var column; a single-row_var one still drops it", {
  # the LEVEL column alone is a complete index only with one row_var (its Total row names the
  # sub-table); a compacted table nests variable x sub-table, so the column has to stay.
  one  <- tabxplor:::tab_export_prep(t_tv, backend = "kable", wrap = NULL)
  testthat::expect_false("year" %in% names(one$tables[[1]]$tab))

  t_cmp <- tab(gss, c(marital, relig), race, tab_vars = year, pct = "row")
  many  <- tabxplor:::tab_export_prep(t_cmp, backend = "kable", wrap = NULL)
  nms   <- names(many$tables[[1]]$tab)
  testthat::expect_true("year" %in% nms)
  testthat::expect_equal(nms[1:3], c("row_var", "year", "levels"))   # column order IS the nesting
})
