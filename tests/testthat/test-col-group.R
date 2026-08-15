# Phase 19n: `col_group` -- WHICH SUB-POPULATION a column's block belongs to.
#
# Two producers turn a variable into side-by-side blocks: `tab(spread_vars =)` / `tab_spread()` (a
# `tab_var` level) and `tab_reg(split_var =)` (a split group). Until 19n both WELDED the level into
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

test_that("the html span composes the two facts on two lines, as the weld used to render", {
  sp <- spread_tab()
  h  <- tab_html(sp)
  expect_match(h, '<th class="tx-span"[^>]*>2000<br>race</th>', fixed = FALSE)
  expect_match(h, '<th class="tx-span"[^>]*>2014<br>race</th>', fixed = FALSE)

  # an UNSPREAD table's span is the bare variable name: no stray separator.
  h0 <- tab_html(tab(gss, marital, race, pct = "row", color = "diff"))
  expect_match(h0, '<th class="tx-span"[^>]*>race</th>', fixed = FALSE)
  expect_false(grepl('<th class="tx-span"[^>]*><br>', h0))
})

test_that("markdown composes on ONE line, being unable to draw two", {
  md <- tab_md(spread_tab())
  txt <- paste(md, collapse = "\n")
  expect_true(grepl("*2000 race*", txt, fixed = TRUE))
  expect_true(grepl("*2014 race*", txt, fixed = TRUE))
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
