# The REGION under a table: the subtext template, the placeholders, the three kinds and where each
# medium puts them. The SENTENCE the `<legend>` placeholder builds is test-tab-color.R's.

testthat::test_that("the stored template names what THIS table can say", {
  # A member built from `meta` is named only where its fact exists (the FOOTER_BLOCKS `default`
  # column); one built from the COLUMNS is always named, because set_color() can colour an
  # uncoloured table afterwards. So an unweighted crosstab names neither <weight> nor <model>.
  t <- tab(fx_gss(), race, marital, pct = "row", color = "diff")
  testthat::expect_identical(get_subtext(t), c("<legend>", "<stars>"))
  testthat::expect_identical(get_subtext(tab(fx_gss(), race, marital, pct = "row", wt = tvhours)),
                             c("<weight>", "<legend>", "<stars>"))

  # a user line takes the `user` row's slot -- last, as it always did
  t2 <- tab(fx_gss(), race, marital, pct = "row", color = "diff", subtext = "Field: GSS")
  testthat::expect_identical(utils::tail(get_subtext(t2), 1L), "Field: GSS")
  testthat::expect_identical(utils::tail(tab_footer_text(t2), 1L), "Field: GSS")

  # ...and set_subtext() applies the same normalisation, so what you read back is what prints
  testthat::expect_identical(get_subtext(set_subtext(t, "Field: GSS")),
                             c("<legend>", "<stars>", "Field: GSS"))
})


testthat::test_that("a regression names <model>, its weight and its interaction", {
  # ⚠ NO golden fixture and no snapshot renders a regression footer, so this is the ONLY guard that
  # the `default` predicates do not prune a line a regression genuinely prints. `<model>` gates on
  # tab_is_reg() and NOT on reg_call(), which reg_finalize() has not yet attached.
  d <- fx_reg_df() |>
    dplyr::mutate(
      married = factor(dplyr::if_else(marital == "Married", "Married", "Not married")),
      party3  = forcats::fct_collapse(
        partyid, dem = c("Strong democrat", "Not str democrat"),
        rep = c("Strong republican", "Not str republican"), other_level = "oth"),
      # a strictly positive, NA-free weight: `survey` warns on zero-weight observations
      w = year / 2000)
  t <- suppressMessages(tab_reg(d, "married", c("race", "age")))
  testthat::expect_true("<model>" %in% get_subtext(t))
  testthat::expect_true(any(grepl("Model:", tab_footer_text(t), fixed = TRUE)))

  # a regression stores its weight in the MODEL RECORD, not in `vars` -- the case a naive predicate
  # on meta$spec$vars$wt would have silently dropped.
  tw <- suppressMessages(tab_reg(d, "married", c("race", "age"), wt = "w"))
  testthat::expect_true("<weight>" %in% get_subtext(tw))
  testthat::expect_true(any(grepl("Weighted by", tab_footer_text(tw), fixed = TRUE)))

  ti <- suppressMessages(tab_reg(d, outcome = "married", predictors = "race", tab_vars = "party3",
                                 family = "binomial", color = c(TRUE, "between_groups")))
  testthat::expect_true("<interaction>" %in% get_subtext(ti))
  testthat::expect_true(any(grepl("Interaction", tab_footer_text(ti), fixed = TRUE)))
})


testthat::test_that("a merge keeps every generated line, in the region's order", {
  # `test` is unioned over the members, so what SPEAKS about it must be too: a member-1 template
  # lacking <weight> would silently suppress the weighted member's line.
  plain <- tab(fx_gss(), race, marital, pct = "row", color = "diff")
  wtd   <- tab(fx_gss(), race, marital, pct = "row", color = "diff", wt = tvhours)
  m     <- tab_compact(list(plain, wtd))
  testthat::expect_identical(get_subtext(m), c("<weight>", "<legend>", "<stars>"))
})


testthat::test_that("the template decides the order, and dropping <legend> drops the legend", {
  t <- tab(fx_gss(), race, marital, pct = "row", color = "diff", wt = tvhours)
  has_legend <- function(x) any(grepl("difference", tab_footer_text(x), fixed = TRUE))
  has_weight <- function(x) any(grepl("Weighted by",  tab_footer_text(x), fixed = TRUE))

  testthat::expect_true(has_legend(t) && has_weight(t))

  # re-ordered: the weight line now comes second
  t2 <- set_subtext(t, c("<legend>", "<weight>"))
  testthat::expect_true(grepl("difference",  tab_footer_text(t2)[[1]], fixed = TRUE))
  testthat::expect_true(grepl("Weighted by", tab_footer_text(t2)[[2]], fixed = TRUE))

  # dropped: no legend anywhere, INCLUDING the console, which no argument can do
  t3 <- set_subtext(t, "<weight>")
  testthat::expect_false(has_legend(t3))
  testthat::expect_false(any(grepl("difference", cli::ansi_strip(print(t3, get_text = TRUE)),
                                   fixed = TRUE)))
  testthat::expect_false(any(grepl("difference", tab_md(t3, css = FALSE, print = FALSE),
                                   fixed = TRUE)))
})


testthat::test_that("a subtext naming no placeholder is APPENDED, and unknown <...> claims nothing", {
  t <- tab(fx_gss(), race, marital, pct = "row", color = "diff")
  keeps_footer <- function(x) any(grepl("difference", tab_footer_text(x), fixed = TRUE))

  # the safety net: a raw overwrite (what a hand-built table and jamovi's free-text box do)
  raw <- t; attr(raw, "subtext") <- "my note"
  testthat::expect_true(keeps_footer(raw))
  testthat::expect_identical(utils::tail(tab_footer_text(raw), 1L), "my note")

  # raw html, a comparison and a bracketed level are NOT placeholders: verbatim, and no claim
  odd <- set_subtext(t, c("<b>bold</b>", "n < 30", "<30 ans>"))
  testthat::expect_true(keeps_footer(odd))
  testthat::expect_true(all(c("<b>bold</b>", "n < 30", "<30 ans>") %in% tab_footer_text(odd)))

  # ...and a backslash escapes a literal `<`
  esc <- set_subtext(t, "literal \\<legend>")
  testthat::expect_true("literal <legend>" %in% tab_footer_text(esc))
})


testthat::test_that("the inline placeholders are built from the cells' own facts", {
  t <- tab(fx_gss(), race, marital, pct = "row", color = "diff")

  # <breaks> IS the ladder the cells are painted with -- never a pasted copy of it
  brk  <- get_color_breaks()[["pct_diff"]]
  line <- tab_footer_text(set_subtext(t, "ladder: <breaks>"))
  line <- line[grepl("^ladder", line)]
  for (b in brk) testthat::expect_true(grepl(paste0("+", b * 100), line, fixed = TRUE))
  # one side only
  up <- tab_footer_text(set_subtext(t, "up: <breaks:over>"))
  up <- up[grepl("^up:", up)]
  testthat::expect_true(grepl("+5", up, fixed = TRUE))
  testthat::expect_false(grepl("-5", up, fixed = TRUE))

  # <conf> and <cols>, and an inline token NEVER claims the layout
  cf <- set_subtext(t, "at <conf>")
  testthat::expect_true("at 95%" %in% tab_footer_text(cf))
  testthat::expect_true(any(grepl("difference", tab_footer_text(cf), fixed = TRUE)))
  testthat::expect_true(any(grepl("marital", tab_footer_text(set_subtext(t, "on <cols>")),
                                  fixed = TRUE)))

  # the measure's own nouns and its interval, in the words set_legend_words() gives them
  piece <- function(tpl) {
    ln <- tab_footer_text(set_subtext(t, paste0("X=", tpl)))
    sub("^X=", "", ln[grepl("^X=", ln)])
  }
  testthat::expect_identical(piece("<measure>"), "difference")
  testthat::expect_identical(piece("<ref>"),      "Total")
  testthat::expect_identical(piece("<ref:noun>"), "the Total row")
  testthat::expect_true(grepl("confidence", piece("<method>"), fixed = TRUE))
})


testthat::test_that("the generated terse legend is expressible in the template language", {
  # THE completeness property: a consumer copies this line and edits it. It holds for a single
  # legend group on the text channel with `color_signif = "ignore"` -- and in English, where the
  # colon takes no spaces (French writes " : ", which a literal in the template cannot know).
  t <- tab(fx_gss(), race, marital, pct = "row", color = "diff")
  gen  <- tab_footer_text(set_subtext(t, "<legend:terse>"), style = "terse", lang = "en")
  hand <- tab_footer_text(set_subtext(t, "<measure> (<ref>): <breaks>"), style = "terse",
                          lang = "en")
  testthat::expect_identical(hand[[1]], gen[[1]])
})


testthat::test_that("<legend:terse|prose> pins the register on the table", {
  t <- tab(fx_gss(), race, marital, pct = "row", color = "diff")
  terse <- tab_footer_text(set_subtext(t, "<legend:terse>"))
  prose <- tab_footer_text(set_subtext(t, "<legend:prose>"))
  testthat::expect_true(grepl("^difference", terse[[1]]))
  testthat::expect_true(grepl("^Percentage points", prose[[1]]))
  testthat::expect_gt(nchar(prose[[1]]), nchar(terse[[1]]))
})


testthat::test_that("a table stripped of its attributes keeps what its COLUMNS can say", {
  # the degradation contract: each FOOTER_BLOCKS row is gated on what it `reads`, so a stripped table
  # keeps the column-derived half (the colour legend, the stars key) and drops the table-derived half
  # (weight / Model: / the shape table / the subordinate tables) -- with no exception handling.
  t <- tab(fx_gss(), race, marital, pct = "row", color = "diff", wt = tvhours)
  bare <- t
  for (a in c("subtext", "meta")) attr(bare, a) <- NULL

  testthat::expect_true(any(grepl("difference", tab_footer_text(bare), fixed = TRUE)))
  testthat::expect_false(any(grepl("Weighted by", tab_footer_text(bare), fixed = TRUE)))
  for (m in c("plain", "console", "html", "md"))
    testthat::expect_no_error(tab_footer_text(bare, medium = m))
  testthat::expect_no_error(tab_md(bare, css = FALSE, print = FALSE))
  testthat::expect_no_error(tab_html(bare))
})


testthat::test_that("the pre-2.0.1 carrier surface still works (CRAN ggfacto 0.3.2)", {
  # a table hand-built the old way: new_tab(subtext = <character>), a raw attr()<- , and the three
  # meta setters. Nothing here may need the template to exist.
  n <- 3L
  col <- fmt(n = rep(100L, n), pct = c(0.2, 0.5, 0.3), scale = "level_pct", pct_type = "col",
             col_var = "v", color = "no", digits = 0L)
  df  <- tibble::tibble(lv = new_lvl(factor(c("a", "b", "c")), role = "level"), v = col)
  old <- new_tab(df, subtext = "hand-written legend")
  testthat::expect_true(is_tab(old))
  testthat::expect_true("hand-written legend" %in% tab_footer_text(old))

  attr(old, "subtext") <- c("line one", "line two")
  testthat::expect_true(all(c("line one", "line two") %in% tab_footer_text(old)))

  old <- set_footer_tabs(old, list("Base" = new_tab(df)))
  old <- set_bars(old, "v")
  testthat::expect_no_error(tab_kable(old))
  testthat::expect_no_error(tab_md(old, css = FALSE, print = FALSE))
  testthat::expect_no_error(print(old, get_text = TRUE))
})
