# Phase 19n: `col_group` -- WHICH SUB-POPULATION a column's block belongs to.
#
# Two producers turn a variable into side-by-side blocks: `tab(spread_vars =)` / `tab_spread()` (a
# `tab_var` level) and `tab_reg(tab_vars =)` (a split group). Until 19n both WELDED the level into
# the column's `col_var` as "{level}<br>{col_var}", and three backends recovered it by sniffing for
# that html tag (Excel's two-line span and its wrap flag, the colour legend's name normaliser) while
# a fourth un-escaped it back after htmlEscape(). `tab_wrap_text(brk = "<br>")` produces the very
# same tag for an unrelated reason -- a long level label -- and none of them could tell the two apart.
#
# The two facts are stored apart now, and composed by whichever backend can draw two lines. This file
# is the migration's proof: the block identity is the PAIR, the rendered output is unchanged, and an
# unspread table pays nothing.

gss <- gss_cat_data_formatting()

spread_tab <- function(...) {
  d <- dplyr::filter(gss, year %in% c(2000, 2014))
  tab(d, marital, race, year, pct = "row", spread_vars = year, test = TRUE, color = "diff", ...)
}

test_that("an unspread table carries an empty col_group, and one block per col_var", {
  t <- tab(gss, marital, race, pct = "row", color = "diff")
  fc <- t[vapply(t, is_fmt, logical(1))]
  expect_true(all(get_col_group(fc) == ""))
  # the pair degenerates to the col_var alone: this is what makes the addition free for every table
  # that was never spread -- the 36 structural goldens included.
  b <- tabxplor:::tab_col_blocks(t)
  expect_identical(b$key, b$col)
  expect_identical(b$label, b$col)
})

test_that("a spread stores the level beside the variable, never welded into it", {
  sp <- spread_tab()
  fc <- sp[vapply(sp, is_fmt, logical(1))]
  cv <- get_col_var(fc)
  cg <- get_col_group(fc)
  expect_false(any(grepl("<br>", cv, fixed = TRUE)))   # the weld is gone from the stored name
  expect_setequal(unique(cv), "race")                  # ... and the variable is named ONCE
  expect_true(all(c("2000", "2014") %in% cg))

  # the BLOCK is the pair: two blocks of one variable, distinguished only by sub-population.
  b <- tabxplor:::tab_col_blocks(sp)
  expect_equal(nrow(b), length(unique(cg)))
  expect_equal(anyDuplicated(b$key), 0L)
})

test_that("col_group reconciles like col_var: same value kept, a mismatch neutralised", {
  a <- tabxplor:::set_col_group(fmt(1:2, pct = c(.1, .2)), "g1")
  b <- tabxplor:::set_col_group(fmt(3:4, pct = c(.3, .4)), "g1")
  c2 <- tabxplor:::set_col_group(fmt(5:6, pct = c(.5, .6)), "g2")
  expect_identical(get_col_group(vctrs::vec_c(a, b)), "g1")
  expect_identical(get_col_group(vctrs::vec_c(a, c2)), "")
})

test_that("the test grid keys on the block, so a spread gets one p-value column per level", {
  sp <- spread_tab()
  tt <- tabxplor:::get_test(sp)
  chi2 <- tt$test == "chi2"
  expect_true(any(chi2))
  # the `test` twin of the attribute: `col` names the variable, `col_group` the sub-population.
  expect_setequal(tt$col[chi2], "race")
  expect_true(all(c("2000", "2014") %in% tt$col_group[chi2]))

  g <- tabxplor:::test_grid_crosstab(sp, tt)
  expect_false(is.null(g))
  # THE regression this keying exists to prevent: on `col` alone both blocks match one key and the
  # grid emits ONE column for a table that has two.
  expect_true(all(c("2000 race", "2014 race") %in% g$value_headers))
  expect_equal(length(g$value_headers), length(unique(tt$col_group[chi2])))
})

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
  h0 <- tab_html(tab(gss, marital, race, pct = "row", color = "diff"))
  expect_match(h0, '<th class="tx-span"[^>]*>race</th>', fixed = FALSE)
  expect_false(grepl('<th class="tx-span"[^>]*><br>', h0))
})

test_that("markdown composes on ONE line, being unable to draw two", {
  md <- tab_md(spread_tab())
  txt <- paste(md, collapse = "\n")
  expect_true(grepl("*race White*", txt, fixed = TRUE))
  expect_true(grepl("*race Black*", txt, fixed = TRUE))
})

test_that("a col_var giving ONE column per block is named by the variable alone", {
  # `levels = "first"` leaves one column per block, and the variable name then identifies it:
  # repeating the level under it would say `married` twice.
  sp <- tab(dplyr::filter(gss, year %in% c(2000, 2014)), rincome, married, year,
            pct = "row", spread_vars = year, levels = "first", color = "diff")
  h  <- tab_html(sp)
  expect_match(h, '<th class="tx-span"[^>]*colspan="3"[^>]*>married</th>', fixed = FALSE)
  expect_false(grepl("01-Married", h, fixed = TRUE))
})

test_that("the header runs encode the PAIR, so two blocks of one variable stay two spans", {
  # RLE-ing the label alone would merge the adjacent "race" runs into a single span covering both
  # sub-populations -- the shape of the defect, in the one function that decides it.
  r <- tabxplor:::tab_header_runs(c("race", "race", "race", "race"),
                                  c("2000", "2000", "2014", "2014"))
  expect_equal(r$spans, c(2L, 2L))
  expect_equal(r$groups, c("2000", "2014"))
  # ... and with no sub-population it is the plain RLE it has always been.
  expect_equal(tabxplor:::tab_header_runs(c("race", "race", "", ""))$spans, c(2L, 2L))
})

test_that("the colour legend names the block, not the bare variable", {
  sp <- spread_tab()
  specs <- tabxplor:::legend_specs(sp)
  skip_if(length(specs) == 0)
  cvs <- unique(vapply(specs, function(s) s$col_var, character(1)))
  # the one-line label -- exactly what the welded col_var rendered as, once legend_name_list() had
  # turned its "<br>" into a space.
  expect_true(all(grepl("^(2000|2014|Ensemble) race$", cvs)))
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

test_that("`comp = 'all'` leaves ONE reference cell, in the total-table block, and says so", {
  sp <- tab(dplyr::filter(gss, year %in% c(2000, 2014)), rincome, married, year,
            pct = "row", spread_vars = year, levels = "first", color = "diff",
            ref = "tot", comp = "all", totaltab = "table")
  val <- names(sp)[vapply(sp, function(x) is_fmt(x) && !is_totcol(x), logical(1))]
  ens <- val[vapply(sp[val], function(x) get_col_group(x) == "Ensemble", logical(1))]
  oth <- setdiff(val, ens)
  # `in_tottab` is a fact about a BLOCK now, never broadcast down the row
  expect_true(all(vapply(sp[oth], function(x) !any(is_tottab(x)), logical(1))))
  expect_true(all(vapply(sp[ens], function(x)  all(is_tottab(x)), logical(1))))
  # ... so exactly one cell per variable is the reading anchor, and it is the one compared against
  expect_equal(sum(tabxplor:::get_reference(sp[[ens[[1]]]], "cells")), 1L)
  expect_true(all(vapply(sp[oth], function(x)
    !any(tabxplor:::get_reference(x, "all_totals")), logical(1))))
  # the legend names it, instead of saying "Total" for both kinds of baseline
  expect_true(any(grepl("Total Ensemble", tabxplor:::tab_color_legend(sp), fixed = TRUE)))
})

test_that("`spread_vars` alone makes the variable a tab_var, and promotes a total line", {
  d <- dplyr::filter(gss, year %in% c(2000, 2014))
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

test_that("an ordered row variable survives every leaf and every synthetic row", {
  # both were hard aborts: the numeric leaf minted an "NA" level the factor leaf had not, and the
  # pct = "col" count row minted a PLAIN factor where the index column was ordered.
  expect_no_error(tab(gss, rincome, party3, race, pct = "col", na = "drop", totaltab = "table") |>
                    tab_html())
  expect_no_error(tab(gss, c(race, rincome), c(party3, tvhours), pct = "row", na = "drop_all") |>
                    tab_html())
})
