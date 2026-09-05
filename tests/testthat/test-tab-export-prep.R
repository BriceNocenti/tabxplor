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
  # Phase 6: `bars` -- the per-cell data-bar fractions set_bars() asked for, resolved here because
  # the render model is where a per-cell display fact belongs.
  # Phase 7: `subordinate` -- is this table travelling UNDER another one (meta$footer_tabs)? It
  # decides that the table renders what it carries and nothing generated, so a host + subordinate
  # pair shows ONE colour legend. Ephemeral, like `bars`: set on the copy the exporter sees.
  # Phase 7b: `want_legend` -- whether THIS table gets a colour legend, decided once here instead of
  # by three backends with three slightly different expressions.
  testthat::expect_named(rd, c("tab", "vars", "roles", "ann", "footer_rows", "bold_rows",
                               "bold_cols", "col_var_header", "subtext", "want_legend",
                               "subordinate", "bars", "reg_title", "caption", "empirical_tips"))
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


# === the two emphasis rules the render model owns =================================================

test_that("a regression footer row is black but NOT bold", {
  d <- fx_reg_df(); d$m <- as.integer(d$marital == "Married")
  t  <- suppressMessages(tab_reg(d, "m", "race", family = "binomial"))
  # `tables` is one render model per table; a regression table is one.
  rd <- tabxplor:::tab_export_prep(t, backend = "kable")$tables[[1]]
  ft <- rd$footer_rows %||% integer(0)
  testthat::skip_if(!length(ft))
  # a model-fit number is a report card under the table: COLOUR is the only emphasis it keeps, so
  # neither the numbers nor the stat names beside them are bold.
  expect_false(any(ft %in% rd$bold_rows))
  for (a in rd$ann) expect_false(any(a$bold[ft]))
})

test_that("under comp = \"all\" a stacked sub-total is not a reference row", {
  g <- fx_gss()
  g$income25k <- forcats::fct_lump_n(g$rincome, 3)
  g$party3    <- forcats::fct_lump_n(g$partyid, 3)
  g$married   <- forcats::fct_lump_n(g$marital, 3)
  # ⚠ SEVERAL row_vars is the one path that stacks, and the stacking bind used to promote EVERY
  # total row to a reference row -- which bolded them all and, a reference row never being coloured,
  # took the sub-totals' own over/under colours away.
  t <- tab(g, c(income25k, married), party3, race, pct = "row", color = TRUE,
           color_signif = "grey_non_signif", comp = "all")
  col <- purrr::keep(t, is_fmt)[[1]]
  expect_false(any(is_refrow(col)))
  # the sub-totals ARE coloured; the total table's own row is the anchor and is not
  slot <- tabxplor:::fmt_color_slots(col, tabxplor:::fmt_color_plan(col))
  sub  <- which(is_totrow(col) & !is_tottab(col))
  expect_true(any(slot[sub] != 0L))
  expect_true(all(slot[is_totrow(col) & is_tottab(col)] == 0L))
})


# === Phase 24g: the caption fallback chain, with a THIRD way in ==================================

testthat::test_that("rd_caption() still reads exporter > stored > a regression's own title", {
  d <- fx_gss()
  t <- tab(d, race, marital, pct = "row", caption = "Built in")
  testthat::expect_match(as.character(tab_html(t)), "Built in", fixed = TRUE)
  # the exporter's own argument still wins over a stored one, whichever way it was stored
  testthat::expect_match(as.character(tab_html(t, caption = "Exporter")), "Exporter", fixed = TRUE)
  testthat::expect_false(grepl("Built in", as.character(tab_html(t, caption = "Exporter")),
                               fixed = TRUE))
  # ... and a regression's auto-title is the last resort, so a stored caption displaces it
  r <- suppressMessages(tab_reg(fx_reg_df(), "marital", "race", family = "binomial",
                                caption = "My model"))
  testthat::expect_identical(get_caption(r), "My model")
  testthat::expect_match(as.character(tab_html(r)), "My model", fixed = TRUE)
})


# === Phase 10: a column is identified by its POSITION, and a col_var name is compacted ============

# ggfacto's shape: a label column headed by a readable name, over a col_var block.
p10_tab <- function(label_name = "Axe", n_axes = 1L) {
  n   <- 6L
  out <- tibble::tibble(
    x   = new_lvl(forcats::as_factor(rep(c("Axis one", "Axis two"), each = 3L)), role = "var"),
    lev = new_lvl(forcats::as_factor(rep(c("a", "b", "c"), 2L)), role = "level")
  )
  names(out)[1] <- label_name
  for (k in seq_len(n_axes))
    out[[paste0("coord_Axe ", k)]] <- fmt(
      n = rep(100L, n), scale = "level_pct", pct_type = "row",
      pct = seq(0.1, 0.6, length.out = n), col_var = paste("Axe", k), color = "no")
  new_tab(out)
}

testthat::test_that("a label column keeps its rowspan whatever its NAME contains", {
  # The export used to RENAME columns while wrapping (spaces -> U+202F), so every per-column fact
  # keyed by a name went stale -- silently. A name with a space lost its rowspan entirely.
  for (nm in c("Axe", "Axis label", " ", "a very long axis label indeed")) {
    h <- as.character(tab_html(p10_tab(nm)))
    testthat::expect_true(grepl('rowspan="3"', h, fixed = TRUE), info = nm)
  }
})

testthat::test_that("export leaves the tibble's column names alone", {
  rd <- tabxplor:::tab_export_prep(
    list(p10_tab("Axis label")), backend = "kable",
    wrap = list(rows = 35L, cols = 3L, exdent = 2, whitespace_only = TRUE,
                unbreakable_spaces = TRUE, brk = "<br>"))$tables[[1]]
  testthat::expect_identical(names(rd$tab)[c(1L, 2L)], c("Axis label", "lev"))
  testthat::expect_true("coord_Axe 1" %in% names(rd$tab))
  # ... and the wrapping lives in the render model's header bands instead
  testthat::expect_true(any(grepl("<br>", rd$col_var_header$clean, fixed = TRUE)))
})

testthat::test_that("a data bar survives a user-side tab_wrap_text()", {
  d <- fx_gss()
  t <- set_bars(tab(d, race, marital, pct = "row"), "Never married")
  # `bars` is keyed by a column NAME, and the user's wrap rewrote it; the prep matches back through
  # tx_unwrap_text(). (A width narrow enough to break mid-word is not recoverable, by anyone.)
  w <- tab_wrap_text(t, wrap_cols = 10L)
  testthat::expect_match(as.character(tab_html(w)), "--tx-bar:", fixed = TRUE)
})

testthat::test_that("tx_elide_prefix() says a shared prefix once, and restates it when it changes", {
  el <- tabxplor:::tx_elide_prefix
  testthat::expect_identical(
    el(c("MUS_CONCERT_CLASSIQUE", "MUS_CONCERT_ROCK", "MUS_CONCERT_JAZZ",
         "MUS_FREQ", "MUS_SUPPORT_VYNILE")),
    c("MUS_CONCERT_CLASSIQUE", "_ROCK", "_JAZZ", "MUS_FREQ", "_SUPPORT_VYNILE"))
  # the first name is never elided, and an unrelated neighbour resets the prefix
  testthat::expect_identical(el(c("AAA_BBB", "CCC_DDD")), c("AAA_BBB", "CCC_DDD"))
  # a prefix under 3 characters, or one not ending at a separator, buys nothing and is refused
  testthat::expect_identical(el(c("a_one", "a_two")), c("a_one", "a_two"))
  testthat::expect_identical(el(c("musConcertRock", "musConcertJazz")),
                             c("musConcertRock", "musConcertJazz"))
})

testthat::test_that("a col_var name is compacted only when its own columns leave it no room", {
  d <- fx_gss()
  d$MUS_CONCERT_CLASSIQUE <- factor(ifelse(as.integer(d$marital) %% 2L == 0L, "Yes", "No"))
  d$MUS_CONCERT_ROCK      <- factor(ifelse(as.integer(d$race)    %% 2L == 0L, "Yes", "No"))
  d$MUS_FREQ              <- factor(ifelse(d$age %% 2L == 0L, "Yes", "No"))
  vars <- c("MUS_CONCERT_CLASSIQUE", "MUS_CONCERT_ROCK", "MUS_FREQ")

  narrow <- tab(d, race, tidyselect::all_of(vars), levels = "first", pct = "row")
  h <- as.character(tab_html(narrow))
  testthat::expect_match(h, ">_ROCK</th>", fixed = TRUE)
  # the elided span says its full name out of band, since "_ROCK" cannot carry the cut point
  testthat::expect_match(h, 'title="MUS_CONCERT_ROCK"', fixed = TRUE)
  # the FIRST block is never elided -- the reader needs the prefix in view -- but nothing entitles it
  # to widen the table either: with no room in its own block it is held to `wrap_cols`.
  testthat::expect_match(h, ">MUS_CONCERT_<br>CLASSIQUE</th>", fixed = TRUE)
  testthat::expect_false(grepl(">MUS_CONCERT_CLASSIQUE<", h, fixed = TRUE))

  # ... and a col_var merging enough level columns says its name outright: neither wrapped nor elided,
  # which is the other half of "never while there is room".
  d$MUS_CONCERT_CLASSIQUE <- d$marital        # 6 levels -> a 6-column span
  d$MUS_CONCERT_ROCK      <- d$race           # 3 levels
  hm <- as.character(tab_html(tab(d, race, tidyselect::all_of(vars[1:2]), pct = "row")))
  testthat::expect_match(hm, ">MUS_CONCERT_CLASSIQUE</th>", fixed = TRUE)
  testthat::expect_match(hm, ">MUS_CONCERT_ROCK</th>", fixed = TRUE)

  # give the columns width and the full names come back, wrapped at their seams and never elided
  wide <- tab(d, race, tidyselect::all_of(vars), levels = "first", pct = "row",
              display = "{pct} (n={n})")
  hw <- as.character(tab_html(wide))
  testthat::expect_false(grepl(">_ROCK<", hw, fixed = TRUE))
  testthat::expect_match(hw, "MUS_<br>CONCERT_<br>ROCK", fixed = TRUE)

  # markdown cannot hold a line break, so the elision is the one compaction it can use
  testthat::expect_match(tab_md(narrow, color = FALSE), "_ROCK", fixed = TRUE)
})

testthat::test_that("a rotated variable name may take several turned lines", {
  # tab_vname_plan() used to demand the whole name on ONE turned line, which put every heading
  # longer than ~1.75 * span out of reach of a rotation it would clearly have won.
  n <- 10L
  t <- new_tab(tibble::tibble(
    Axe = new_lvl(forcats::as_factor(rep(c("Axe 1: 9.9% of variance (mod. 57%)",
                                           "Axe 2: 7.1% of variance (mod. 31%)"), each = 5L)),
                  role = "var"),
    lev = new_lvl(forcats::as_factor(rep(letters[1:5], 2L)), role = "level"),
    coord_Axe = fmt(n = rep(100L, n), scale = "level_pct", pct_type = "row",
                    pct = seq(0.05, 0.5, length.out = n), col_var = "Axe", color = "no")))
  h <- as.character(tab_html(t))
  testthat::expect_match(h, "tx-vname", fixed = TRUE)
  # ... broken at its seams, with no mid-word cut and no horizontal "continues below" indent
  testthat::expect_match(h, "variance<br>", fixed = TRUE)
  testthat::expect_false(grepl("varian<br>", h, fixed = TRUE))
})
