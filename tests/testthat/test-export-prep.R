# PURPOSE: Lock the Phase 10d shared exporter-prep (tab_export_prep + the render-model + ann +
#          tab_check_same_col_vars + tab_bold_rows + tab_totcol_range). The byte-identity of the
#          kable/md/plot OUTPUT is covered by test-golden.R / test-color-golden.R / test-tab_md.R;
#          this file locks the prep's INTERNAL derive-once quantities and the degrade / base-list split.
# ROLE: Phase 10d.

gss <- forcats::gss_cat

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
  testthat::expect_named(rd, c("tab", "vars", "roles", "ann", "bold_rows",
                               "bold_cols", "range_totcol", "col_var_header", "subtext",
                               "reg_line", "reg_title", "empirical_tips"))
  testthat::expect_false(rd$vars$degrade)
})

testthat::test_that("vars are detected correctly", {
  rd <- tabxplor:::tab_export_prep(t_basic, backend = "kable", wrap = NULL)$tables[[1]]
  testthat::expect_identical(rd$vars$row_var, "race")
  testthat::expect_true("marital" %in% rd$vars$col_vars)  # also carries "all_col_vars" (the Total)
  testthat::expect_length(rd$vars$tab_vars, 0L)
})

testthat::test_that("roles: fmt_cols / totcols / row_var_col match the built table", {
  rd <- tabxplor:::tab_export_prep(t_basic, backend = "kable", wrap = NULL)$tables[[1]]
  tab <- rd$tab
  testthat::expect_identical(rd$roles$fmt_cols, which(purrr::map_lgl(tab, is_fmt)))
  testthat::expect_identical(rd$roles$totcols, which(is_totcol(tab)))
  testthat::expect_identical(rd$roles$totrows, which(is_totrow(tab)))
  testthat::expect_identical(unname(rd$roles$row_var_col), which(names(tab) == "race"))
})

# === SECTION: ann + colours gated by compute =================================

testthat::test_that("ann shape is uniform; compute without 'colors' yields a monochrome column", {
  # Phase 10j: fmt_col_ann() ALWAYS returns the full structure, so every backend reads a consistent
  # shape. `want_colors = FALSE` (compute without "colors", i.e. a color = FALSE export) does not drop
  # fields -- it forces a MONOCHROME column: no colour slots, no colour flag.
  cols <- c("ref_alltot", "ref_cells", "font", "back", "bold", "text_slot", "bg_slot")

  rk <- tabxplor:::tab_export_prep(t_basic, backend = "kable", wrap = NULL)$tables[[1]]
  a1 <- rk$ann[[1]]
  testthat::expect_true(all(cols %in% names(a1)))
  testthat::expect_length(a1$font, nrow(rk$tab))

  rm <- tabxplor:::tab_export_prep(t_basic, backend = "md", drop_tab_vars = FALSE,
                                   wrap = NULL, compute = c("refs", "bold"))$tables[[1]]
  am <- rm$ann[[1]]
  testthat::expect_true(all(cols %in% names(am)))
  testthat::expect_false(am$has_color)
  testthat::expect_true(all(am$text_slot == 0L))
  testthat::expect_true(all(am$bg_slot == 0L))
})

testthat::test_that("bold_rows flags the reference/total row(s), reused by ann$ref_alltot", {
  rd <- tabxplor:::tab_export_prep(t_basic, backend = "kable", wrap = NULL)$tables[[1]]
  # a row is bold iff it is a reference/total cell in EVERY discriminating column
  refref <- as.data.frame(purrr::map(rd$ann, "ref_alltot"))
  keep   <- purrr::map_lgl(refref, ~ any(.) & !all(.))
  refref <- refref[, keep, drop = FALSE]
  expected <- which(rowSums(refref) == ncol(refref))
  testthat::expect_identical(rd$bold_rows, expected)
  testthat::expect_gt(length(rd$bold_rows), 0L)  # there is always a total row
})

# === SECTION: graceful degrade ===============================================

testthat::test_that("degrade path flags non-tabxplor inputs with a reason", {
  for (obj in list(iris, tibble::tibble(a = 1:3), datasets::mtcars)) {
    rd <- tabxplor:::tab_export_prep(obj, backend = "md", wrap = NULL)$tables[[1]]
    testthat::expect_true(isTRUE(rd$vars$degrade))
    testthat::expect_type(rd$vars$reason, "character")
  }
})

# === SECTION: tab_check_same_col_vars (block A) ==============================

testthat::test_that("tab_check_same_col_vars accepts same col_vars, rejects mismatch / tab_vars", {
  ok <- list(tab(gss, race, marital, pct = "row"),
             tab(gss, relig, marital, pct = "row"))
  testthat::expect_identical(tabxplor:::tab_check_same_col_vars(ok), "marital")

  bad <- list(tab(gss, race, marital, pct = "row"),
              tab(gss, race, relig, pct = "row"))
  testthat::expect_error(tabxplor:::tab_check_same_col_vars(bad), "same col_vars")

  withtv <- list(tab(gss, race, marital, year, pct = "row"))
  testthat::expect_error(tabxplor:::tab_check_same_col_vars(withtv), "no tab_vars")
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

testthat::test_that("list_method keeps a non-mergeable list as N tables; else it errors", {
  tv_list <- list(tab(gss, race, marital, year, pct = "row", color = "diff"),
                  tab(gss, relig, marital, year, pct = "row", color = "diff"))
  # list_method = TRUE (tab_md) -> one render table per input, each not degraded
  p <- tabxplor:::tab_export_prep(tv_list, backend = "md", drop_tab_vars = FALSE,
                                  wrap = NULL, list_method = TRUE)
  testthat::expect_length(p$tables, 2L)
  testthat::expect_false(p$tables[[1]]$vars$degrade)
  testthat::expect_false(p$tables[[2]]$vars$degrade)
  # list_method = FALSE (tab_kable / tab_plot) -> historical error
  testthat::expect_error(
    tabxplor:::tab_export_prep(tv_list, backend = "kable", wrap = NULL, list_method = FALSE),
    "no tab_vars"
  )
})

# === SECTION: tab_bold_rows edge (md vs kable style) =========================

testthat::test_that("tab_bold_rows: no discriminating column -> md integer(0), kable all rows", {
  # all-FALSE (no reference) columns -> not discriminating
  none <- list(c(FALSE, FALSE, FALSE), c(FALSE, FALSE, FALSE))
  testthat::expect_identical(tabxplor:::tab_bold_rows(none, md_style = TRUE), integer(0))
  testthat::expect_identical(tabxplor:::tab_bold_rows(none, md_style = FALSE), 1:3)
  # a normal discriminating column -> both agree
  disc <- list(c(FALSE, FALSE, TRUE), c(FALSE, FALSE, TRUE))
  testthat::expect_identical(tabxplor:::tab_bold_rows(disc, md_style = TRUE), 3L)
  testthat::expect_identical(tabxplor:::tab_bold_rows(disc, md_style = FALSE), 3L)
})

# === SECTION: tab_totcol_range (block B, inert in Part 1) ====================

testthat::test_that("tab_totcol_range is scalar when col_var bases are equal (na='keep')", {
  rd <- tabxplor:::tab_export_prep(t_multi, backend = "kable", wrap = NULL)$tables[[1]]
  rng <- rd$range_totcol
  testthat::expect_named(rng, c("col", "text", "differ"))
  testthat::expect_length(rng$text, nrow(rd$tab))
  testthat::expect_false(any(rng$differ))  # na='keep' -> every col_var's base = full N
})

testthat::test_that("tab_totcol_range yields [min;max] when bases differ (na='drop')", {
  set.seed(1)
  d <- tibble::tibble(
    g  = factor(sample(c("A", "B"), 400, TRUE)),
    q1 = factor(sample(c("yes", "no"), 400, TRUE)),
    q2 = factor(sample(c("yes", "no", NA), 400, TRUE, prob = c(0.4, 0.4, 0.2)))
  )
  tt <- tab(d, g, c(q1, q2), pct = "row", na = "drop")
  rd <- tabxplor:::tab_export_prep(tt, backend = "kable", wrap = NULL)$tables[[1]]
  rng <- rd$range_totcol
  testthat::expect_true(any(rng$differ))
  testthat::expect_true(any(grepl("^\\[.*;.*\\]$", rng$text[rng$differ])))
})


# === SECTION: recorded variable roles (Phase 14d) ===========================

testthat::test_that("the `vars` attribute records the roles a merged table cannot show", {
  # tab_compact() renames column 1 to the literal "levels" and keeps the row-variable names only as
  # levels of a synthetic column NAMED "row_var" -- so the column-type heuristic read that meta column
  # as a tab_var. Recording the roles at build time is the fix.
  merged <- tab(gss, c(race, relig), marital, pct = "row")
  v <- tabxplor:::get_vars_attr(merged)
  testthat::expect_equal(v$row_vars, c("race", "relig"))   # the SOURCE names, unrecoverable otherwise
  testthat::expect_true(v$compacted)
  testthat::expect_length(v$tab_vars, 0L)
  # tab_get_vars() keeps its COLUMN-name contract, and now tells the truth about tab_vars
  testthat::expect_equal(tab_get_vars(merged)$row_var, "levels")
  testthat::expect_length(tab_get_vars(merged)$tab_vars, 0L)
  # a real tab_var is still reported
  testthat::expect_equal(tab_get_vars(tab(gss, race, marital, year, pct = "row"))$tab_vars, "year")
})

testthat::test_that("`vars` survives dplyr verbs, and a stale one loses to the real columns", {
  merged <- tab(gss, c(race, relig), marital, pct = "row")
  for (f in list(function(x) dplyr::filter(x, TRUE),
                 function(x) dplyr::mutate(x, zz = 1),
                 function(x) dplyr::ungroup(x),
                 function(x) dplyr::arrange(x))) {
    testthat::expect_length(tab_get_vars(f(merged))$tab_vars, 0L)
  }
  # a `vars` naming columns that are gone must not win over what is actually there
  faked <- tabxplor:::set_vars_attr(tab(gss, race, marital, pct = "row"),
                                    tabxplor:::new_vars_attr(row_vars = "gone_var"))
  testthat::expect_null(tabxplor:::tab_vars_recorded(faked))
  testthat::expect_equal(tab_get_vars(faked)$row_var, "race")   # heuristic fallback
})

testthat::test_that("a table with no recorded roles still detects them (tab_num / hand-built)", {
  leaf <- tab_num(gss, race, age)
  testthat::expect_null(tabxplor:::get_vars_attr(leaf))
  testthat::expect_equal(tab_get_vars(leaf)$row_var, "race")
})

testthat::test_that("tab_compact() on an already-merged table is a no-op", {
  merged <- tab(gss, c(race, relig), marital, pct = "row")
  testthat::expect_identical(tab_compact(merged), merged)
})

# === SECTION: the label columns and their runs (Phase 14i) ===================

testthat::test_that("the prep passes the SOURCE row_vars + compacted through", {
  # Phase 14i: tab_render_vars() has returned both since 14d, but prep_one_table() rebuilt `vars`
  # without them -- so tab_xl's title read "levels by relig" (the merge's own scaffolding column).
  rd <- tabxplor:::tab_export_prep(tab(gss, c(race, marital), relig, pct = "row"),
                                   backend = "kable", wrap = NULL)$tables[[1]]
  testthat::expect_equal(rd$vars$row_vars, c("race", "marital"))
  testthat::expect_true(rd$vars$compacted)
  testthat::expect_equal(rd$vars$row_var, "levels")   # the COLUMN contract is unchanged
  # Phase 14l: the DEPENDENT axis is named first. This is a pct="row" table, so the col_var leads.
  testthat::expect_equal(tabxplor:::tab_get_titles(rd$tab, rd$vars$row_vars, rd$vars$col_vars),
                         "relig by race, marital")
  # a plain table reports itself
  rd2 <- tabxplor:::tab_export_prep(t_basic, backend = "kable", wrap = NULL)$tables[[1]]
  testthat::expect_equal(rd2$vars$row_vars, "race")
  testthat::expect_false(rd2$vars$compacted)
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

testthat::test_that("roles$sd_cols finds the Excel sd siblings, ungated by var_names", {
  # Phase 14l: ONE definition of the "<var>_sd" rule, read by tab_col_var_header() and by tab_xl's
  # column widths. The header rewrite is gated on `var_names`; the ROLE must not be -- a width is not
  # a naming decision.
  pr <- function(vn) tabxplor:::tab_export_prep(
    tab(gss, marital, c(race, tvhours), pct = "row"), backend = "xl", list_method = TRUE,
    var_names = vn, compute = c("refs", "bold"))$tables[[1]]
  for (vn in c("both", "cols", "rows", "none")) {
    testthat::expect_equal(names(pr(vn)$roles$sd_cols), "tvhours_sd", info = vn)
  }
  # the HEADER, by contrast, is gated: no span row -> the level header must name the variable itself
  h <- function(vn) { p <- pr(vn); p$col_var_header$clean[p$roles$fmt_cols] }
  testthat::expect_true(all(c("mean", "sd") %in% h("both")))
  testthat::expect_true("tvhours_sd" %in% h("none"))
  # a table with no numeric col_var has no sd sibling at all
  p0 <- tabxplor:::tab_export_prep(tab(gss, marital, race, pct = "row"), backend = "xl",
                                   list_method = TRUE, compute = c("refs", "bold"))$tables[[1]]
  testthat::expect_length(p0$roles$sd_cols, 0)
  # and neither does a text backend, which never materialises them (the sd is inline there)
  pk <- tabxplor:::tab_export_prep(tab(gss, marital, c(race, tvhours), pct = "row"),
                                   backend = "kable", wrap = NULL)$tables[[1]]
  testthat::expect_length(pk$roles$sd_cols, 0)
})

testthat::test_that("a title elides past `max` = 2 names", {
  testthat::expect_equal(tabxplor:::tab_title_names(c("a", "b")), "a, b")
  testthat::expect_equal(tabxplor:::tab_title_names(c("a", "b", "c")), "a, b +1 more")
  testthat::expect_equal(tabxplor:::tab_title_names(c("a", "b", "c", "d", "e")), "a, b +3 more")
  # placeholders never reach a title
  testthat::expect_equal(tabxplor:::tab_title_names(c("no_row_var", "all_col_vars")), "")
  # the default really is 2 (the formal, not just the helper)
  testthat::expect_equal(formals(tabxplor:::tab_get_titles)$max, 2)
})

testthat::test_that("label_cols / var_name_col: the merged name column vs the kept tab_vars", {
  # The two kinds are mutually exclusive by construction (tab_compact() bails on tab_vars).
  merged <- tabxplor:::tab_export_prep(tab(gss, c(race, marital), relig, pct = "row"),
                                       backend = "kable", wrap = NULL)$tables[[1]]
  testthat::expect_equal(names(merged$roles$label_cols), "row_var")
  testthat::expect_equal(names(merged$roles$var_name_col), "row_var")   # values ARE variable names

  # a kept tab_var is a label column (blank/merge) but NOT a name column (its values are levels)
  tv <- tabxplor:::tab_export_prep(t_tv, backend = "md", drop_tab_vars = FALSE,
                                   wrap = NULL)$tables[[1]]
  testthat::expect_equal(names(tv$roles$label_cols), "year")
  testthat::expect_length(tv$roles$var_name_col, 0L)

  # a dropped tab_var is not a label column at all, and a plain table has none
  tvd <- tabxplor:::tab_export_prep(t_tv, backend = "kable", wrap = NULL)$tables[[1]]
  testthat::expect_length(tvd$roles$label_cols, 0L)
  plain <- tabxplor:::tab_export_prep(t_basic, backend = "kable", wrap = NULL)$tables[[1]]
  testthat::expect_length(plain$roles$label_cols, 0L)
  testthat::expect_length(plain$roles$label_runs, 0L)
})

testthat::test_that("tab_label_runs marks one run per block, and agrees with new_group", {
  rd  <- tabxplor:::tab_export_prep(tab(gss, c(race, marital), relig, pct = "row"),
                                    backend = "kable", wrap = NULL)$tables[[1]]
  run <- rd$roles$label_runs[["row_var"]]
  testthat::expect_equal(as.character(rd$tab[["row_var"]])[run$show], c("race", "marital"))
  # a run start is the row after each group end; the spans tile the table exactly
  testthat::expect_equal(which(run$show), c(1L, utils::head(rd$roles$new_group, -1L) + 1L))
  testthat::expect_equal(sum(run$span), nrow(rd$tab))
  testthat::expect_true(all(run$span[!run$show] == 0L))
})

testthat::test_that("tab_label_runs: NA is a continuation, and the columns nest", {
  lr <- tabxplor:::tab_label_runs
  # NA (a materialised p-value row belongs to the block above it) never starts a run
  d1 <- data.frame(g = c("a", "a", NA, "b", NA))
  testthat::expect_equal(which(lr(d1, "g")$g$show), c(1L, 4L))
  testthat::expect_equal(lr(d1, "g")$g$span[c(1L, 4L)], c(3L, 2L))
  # NESTED: an outer column's new run restarts the inner one, even when the inner value repeats.
  # A naive per-column scan (md's old loop) would merge rows 1-2 of `in_` across the `out` change.
  d2 <- data.frame(out = c("x", "y", "y"), in_ = c("k", "k", "k"))
  testthat::expect_equal(which(lr(d2, c("out", "in_"))$in_$show), c(1L, 2L))
  testthat::expect_equal(lr(d2, c("out", "in_"))$in_$span[c(1L, 2L)], c(1L, 2L))
  # degenerate inputs
  testthat::expect_length(lr(d1, character(0)), 0L)
  testthat::expect_length(lr(d1[0, , drop = FALSE], "g"), 0L)
})

testthat::test_that("var_names drops the row-name column / the col_var span, and nothing else", {
  merged <- tab(gss, c(race, marital), relig, pct = "row")
  prep_of <- function(x, vn) tabxplor:::tab_export_prep(x, backend = "kable", wrap = NULL,
                                                        var_names = vn)$tables[[1]]
  expect <- function(vn, has_name_col, has_span) {
    rd <- prep_of(merged, vn)
    testthat::expect_equal("row_var" %in% names(rd$tab), has_name_col, label = vn)
    testthat::expect_equal(any(nzchar(rd$col_var_header$label)), has_span, label = vn)
    # dropping the name column must not leave a stale role behind
    if (!has_name_col) testthat::expect_length(rd$roles$var_name_col, 0L)
  }
  expect("both", TRUE,  TRUE)
  expect("rows", TRUE,  FALSE)   # row names only -> the col_var span goes
  expect("cols", FALSE, TRUE)    # col names only -> the row-name column goes
  expect("none", FALSE, FALSE)
  testthat::expect_error(prep_of(merged, "nope"))

  # It never touches a LEVEL column's header: `race` on a single-row_var table, `year` on a tab_var.
  # That header identifies the column and costs no width (the maintainer's call, Phase 14i).
  rd <- tabxplor:::tab_export_prep(t_basic, backend = "kable", wrap = NULL,
                                   var_names = "none")$tables[[1]]
  testthat::expect_equal(rd$col_var_header$clean[[1]], "race")
  rd_tv <- tabxplor:::tab_export_prep(t_tv, backend = "md", drop_tab_vars = FALSE, wrap = NULL,
                                      var_names = "none")$tables[[1]]
  testthat::expect_true("year" %in% names(rd_tv$tab))
})

testthat::test_that("a numeric col_var names the VARIABLE once, and the statistic below it", {
  # Pass 2: "tvhours is still repeating for the variable name + the normal header (factor level),
  # better keep the variable name + just write 'mean (sd)' in the normal header". A numeric col_var
  # contributes a column named after the variable, so under its own span the name was said twice --
  # three times in Excel, which splits off a `<var>_sd` sibling too.
  t_num <- tab(gss, marital, c(race, tvhours), pct = "row", color = "diff")
  hdr <- function(backend, vn = "both") {
    cvh <- tabxplor:::tab_export_prep(t_num, backend = backend, wrap = NULL,
                                      var_names = vn)$tables[[1]]$col_var_header
    stats::setNames(cvh$clean, cvh$label)
  }
  # text backends fold the sd into the cell ("1.7 (s2.1)"), so the header says so
  testthat::expect_equal(unname(hdr("kable")[names(hdr("kable")) == "tvhours"]), "mean (sd)")
  # Excel splits it into a real `_sd` column: one header each
  testthat::expect_equal(unname(hdr("xl")[names(hdr("xl")) == "tvhours"]), c("mean", "sd"))

  # ... but the level header may only name the STATISTIC while the span names the VARIABLE. Blanking
  # the span after the fact (as Phase 14i did) left `var_names = "none"` with a column headed "mean"
  # and the variable's name NOWHERE -- which is why the decision moved into tab_col_var_header().
  testthat::expect_equal(unname(hdr("kable", "none")), c("marital", "Other", "Black", "White",
                                                         "Total", "tvhours"))
  testthat::expect_true(all(c("tvhours", "tvhours_sd") %in% hdr("xl", "none")))

  # A mean column that shows no sd (ci = "cell" prints an interval) is just "mean" -- the header reads
  # the same predicate format() does, so the two cannot disagree.
  cvh <- tabxplor:::tab_export_prep(tab_num(gss, marital, tvhours, ci = "cell"),
                                    backend = "kable", wrap = NULL)$tables[[1]]$col_var_header
  testthat::expect_equal(cvh$clean[cvh$label == "tvhours"], "mean")
})

testthat::test_that("the literal `row_var` header is always dropped (a bug fix, not a setting)", {
  # It is an internal name, never informative -- tab_col_var_header()'s suffix loop only visits
  # LABELLED columns, so it survived. One blank there and md / kableExtra / html / xl all follow.
  for (vn in c("both", "rows")) {
    rd <- tabxplor:::tab_export_prep(tab(gss, c(race, marital), relig, pct = "row"),
                                     backend = "kable", wrap = NULL, var_names = vn)$tables[[1]]
    testthat::expect_equal(rd$col_var_header$clean[[1]], "", label = vn)
    testthat::expect_equal(names(rd$tab)[[1]], "row_var", label = vn)   # the COLUMN still exists
  }
})

# Phase 14s (L3): the col_var spanning-name row is dropped when every level column's DISPLAYED header
# already equals its col_var (a reg table named after the model). A crosstab (level != col_var) keeps it.
testthat::test_that("L3: a redundant col_var span (name == col_var for all cols) is dropped", {
  span <- function(t) {
    rd <- tabxplor:::tab_export_prep(t, backend = "kable", wrap = NULL)$tables[[1]]
    any(nzchar(rd$col_var_header$label))
  }
  # crosstab: level "Black" != col_var "race" -> span kept
  testthat::expect_true(span(tab(gss, marital, race, pct = "row")))
  # numeric col_var: header "mean (sd)" != col_var "tvhours" -> span kept
  testthat::expect_true(span(tab(gss, marital, tvhours, pct = "row")))
  testthat::skip_if_not_installed("broom")
  d <- forcats::gss_cat |>
    dplyr::mutate(married = factor(dplyr::if_else(marital == "Married", "Married", "Not married")))
  # Phase 14w (item 3): a single-model reg column is now named "Model OR" while its col_var names the
  # OUTCOME ("married: Married") -> name != col_var -> the span is KEPT (it labels the outcome; with
  # empirical companions it spans them all with no border between).
  testthat::expect_true(span(tab_reg(d, "married", c("race", "age"), family = "binomial")))
})
