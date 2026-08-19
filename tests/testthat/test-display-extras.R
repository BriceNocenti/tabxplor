# PURPOSE: Phase 10i-B -- the base count / add_pct / p-value rows are DISPLAY-only. The built tab() is the
#          "core" table (no `n` / `col_pct` column, no p-value rows) carrying the intent (the
#          `render_extras` attribute + the kept `test` attribute); tab_materialize_extras() re-creates
#          the extras at display, byte-identically to the pre-migration built table.
# See: CLAUDE.md Phase 10i-B ; dev/tabxplor_2.0.0_decisions.md §34.

# Phase 20a: this file calls functions deprecated in 2.0.0 on purpose -- what it asserts is their
# arithmetic, which the leaf shares with them and which does NOT go away in 2.1.0.
# ⚠ This quiets the TOP-LEVEL calls only: testthat 3e runs local_reproducible_output() inside
# every test_that(), which forces lifecycle_verbosity = "warning" again, so the in-block calls
# still warn. Migrating them to tab() is the corpus sweep routed to Phase 20h.
withr::local_options(lifecycle_verbosity = "quiet", .local_envir = testthat::teardown_env())


gss <- forcats::gss_cat

# --- the built "core" table carries the intent, not the extras --------------------------------
testthat::test_that("built tab() is the core table: no n/col_pct column, no p-value rows, intent kept", {
  t <- tab(gss, marital, race, pct = "row", n = "range", test = TRUE)
  testthat::expect_false("n" %in% names(t))
  testthat::expect_false("col_pct" %in% names(t))
  testthat::expect_identical(get_render_extras(t), list(n = "range", add_pct = FALSE))
  testthat::expect_false(is.null(get_test(t)))                 # test attribute KEPT (was dropped pre-10i-B)
  rv <- tab_get_vars(t)$row_var
  testthat::expect_false(any(as.character(t[[rv]]) == "pvalue"))
})

# --- render_extras survives dplyr verbs (carried like subtext/test) ----------------------------
testthat::test_that("render_extras is carried through dplyr verbs", {
  t  <- tab(gss, marital, race, pct = "row", n = "range", add_pct = TRUE)
  re <- list(n = "range", add_pct = TRUE)
  testthat::expect_identical(get_render_extras(dplyr::filter(t, TRUE)), re)
  testthat::expect_identical(get_render_extras(dplyr::arrange(t, Total)), re)
  testthat::expect_identical(get_render_extras(dplyr::mutate(t, .z = 1L)), re)
  testthat::expect_identical(get_render_extras(dplyr::select(t, marital, Total)), re)
  tg <- tab(gss, marital, race, tab_vars = year, pct = "row", n = "range", output_list = TRUE)[[1]]
  testthat::expect_identical(get_render_extras(dplyr::slice(tg, 1)), list(n = "range", add_pct = FALSE))
  testthat::expect_identical(get_render_extras(dplyr::ungroup(tg)), list(n = "range", add_pct = FALSE))
})

# --- materialiser (xl backend) reproduces the extras as real columns/rows ----------------------
testthat::test_that("tab_materialize_extras('xl') re-creates the base-count `n` column", {
  t   <- tab(gss, marital, race, pct = "row", n = "range")
  hyd <- tabxplor:::tab_materialize_extras(t, backend = "xl", pvalue = FALSE)
  testthat::expect_true("n" %in% names(hyd))
  testthat::expect_identical(tabxplor:::fmt_var_kind(hyd$n), "count")
  testthat::expect_equal(get_n(hyd$n), get_n(t$Total))          # the base counts of the Total column
})

testthat::test_that("materialiser is idempotent (clears render_extras after consuming)", {
  t   <- tab(gss, marital, race, pct = "row", n = "range")
  h1  <- tabxplor:::tab_materialize_extras(t, backend = "xl", pvalue = FALSE)
  h2  <- tabxplor:::tab_materialize_extras(h1, backend = "xl", pvalue = FALSE)   # no-op
  testthat::expect_identical(names(h1), names(h2))
  testthat::expect_null(get_render_extras(h1))
})

# --- text backend folds the base count IN-CELL on the Total column (no separate `n` column) -------------
testthat::test_that("tab_materialize_extras('text') folds the base count into the Total cell", {
  t   <- tab(gss, marital, race, pct = "row", n = "range")
  hyd <- tabxplor:::tab_materialize_extras(t, backend = "text", pvalue = FALSE)
  testthat::expect_false("n" %in% names(hyd))
  # the Total cell shows the {pct} ({n_range}) composite
  testthat::expect_match(format(hyd$Total)[1], "100% *\\(", perl = TRUE)
})

# --- Phase 22b-i: the base is a RANGE when the blocks do not rest on the same people --------------
testthat::test_that("the Total cell prints a range when the col_vars have different bases", {
  d <- gss
  d$partyid[1:3000] <- NA
  t   <- tab(d, marital, c(race, partyid), pct = "row", na = "drop")
  hyd <- tabxplor:::tab_materialize_extras(t, backend = "text", pvalue = FALSE)
  # ONE Total column, and it speaks for BOTH blocks: the smallest base and the largest
  tot  <- names(hyd)[is_totcol(hyd)]
  testthat::expect_length(tot, 1L)
  race_base  <- get_tot_n(hyd[["White"]])
  party_base <- get_tot_n(hyd[[names(hyd)[startsWith(names(hyd), "Ind")][1]]])
  testthat::expect_equal(get_n(hyd[[tot]]),    as.integer(pmin(race_base, party_base)))
  testthat::expect_equal(get_tot_n(hyd[[tot]]),         pmax(race_base, party_base))
  testthat::expect_match(format(hyd[[tot]])[1], "-", fixed = TRUE)   # a genuine min-max
  # ... and one number again under n = "min"
  hyd2 <- tabxplor:::tab_materialize_extras(
    tab(d, marital, c(race, partyid), pct = "row", na = "drop", n = "min"),
    backend = "text", pvalue = FALSE)
  testthat::expect_false(any(grepl("-", format(hyd2[[tot]]), fixed = TRUE)))
})

testthat::test_that("levels = 'first' drops the misleading 100%, keeping the base alone", {
  t   <- tab(gss, marital, race, pct = "row", levels = "first")
  hyd <- tabxplor:::tab_materialize_extras(t, backend = "text", pvalue = FALSE)
  tot <- names(hyd)[is_totcol(hyd)]
  testthat::expect_length(tot, 1L)                      # the column stays: the base is worth seeing
  testthat::expect_false(any(grepl("100%", format(hyd[[tot]]), fixed = TRUE)))
  testthat::expect_match(format(hyd[[tot]])[1], "[0-9]")
  # with every level shown, the same column DOES total what the reader sees
  h2 <- tabxplor:::tab_materialize_extras(tab(gss, marital, race, pct = "row"),
                                          backend = "text", pvalue = FALSE)
  testthat::expect_match(format(h2$Total)[1], "100%", fixed = TRUE)
})

# --- transpose carries the intent: transpose(row% n) renders like a native col% n ------
# Phase 14o: the render-level `transpose = TRUE` materialises xl-style so the base count's `n` COLUMN flips into
# an `n` ROW -- byte-identical to a native pct = "col" add_n table.
testthat::test_that("transpose = TRUE carries render_extras (transpose == native col% n)", {
  transposed <- tab_md(tab(gss, marital, race, pct = "row", n = "range"), transpose = TRUE, print = FALSE)
  native     <- tab_md(tab(gss, race, marital, pct = "col", n = "range"), print = FALSE)
  testthat::expect_identical(transposed, native)
})

# --- back-compat shim: $n / [[ / pull reconstruct the deprecated column ------------------------
testthat::test_that("$n / [[ / pull reconstruct the display-only `n` column with a deprecation", {
  t  <- tab(gss, marital, race, pct = "row", n = "range")
  xl <- tabxplor:::tab_materialize_extras(t, backend = "xl", pvalue = FALSE)$n

  testthat::expect_warning(n1 <- t$n, class = "lifecycle_warning_deprecated")
  testthat::expect_identical(n1, xl)
  testthat::expect_identical(suppressWarnings(t[["n"]]),         xl)
  testthat::expect_identical(suppressWarnings(dplyr::pull(t, n)), xl)
  testthat::expect_identical(suppressWarnings(dplyr::pull(t, "n")), xl)

  # fast path: an existing column is returned with NO deprecation warning
  testthat::expect_no_warning(tot <- t$Total)
  testthat::expect_true(is_fmt(tot))
  # n = "no" -> never had an `n` column -> NULL (no reconstruction)
  testthat::expect_null(suppressWarnings(tab(gss, marital, race, pct = "row", n = "no")$n))
  # a genuinely unknown column -> NULL (base tbl_df behaviour)
  testthat::expect_null(suppressWarnings(t$zzz_unknown))
  # pct = "col": the base count was a ROW, so `$n` must NOT reconstruct a column
  testthat::expect_null(suppressWarnings(tab(gss, marital, race, pct = "col", n = "range")$n))
})

# --- pull() of a normal (existing) column is untouched by the shim -----------------------------
testthat::test_that("pull() of an existing column keeps tidy-select NSE (shim does not break it)", {
  tabs <- tab(gss, race, c(age, tvhours), comp = "all")
  testthat::expect_true(is_fmt(dplyr::pull(dplyr::filter(tabs, race == "White"), tvhours)))
  testthat::expect_true(is_fmt(dplyr::pull(tabs, age)))
})


# --- Phase 14a: the pct = "col" add_n / add_pct ROW on a merged multi-row_var table --------------
# `last_totrow` is a GLOBAL index (is_totrow.data.frame is not group-aware), but a merged
# multi-row_var tab is a grouped_df where dplyr::slice() indexes WITHIN each group -- no group had
# that many rows, so slice() returned 0 rows and bind_rows() silently dropped the extra.

row_labels <- function(tt) {
  m <- tabxplor:::tab_materialize_extras(tt, backend = "text", pvalue = FALSE)
  as.character(m[[tab_get_vars(m)$row_var]])
}

# A deterministic fixture whose row_vars have DIFFERENT missing counts, so under na = "drop" each
# variable's Total base genuinely differs -> the Phase 14n collapse keeps every sub-table's total.
gss_uneven <- function() {
  g <- forcats::gss_cat
  g$marital[1:800] <- NA
  g$race[1:40]     <- NA
  g
}

testthat::test_that("pct = 'col' add_n: one n row per sub-table, collapsed to one when bases match (14n)", {
  g <- forcats::gss_cat
  testthat::expect_equal(sum(row_labels(tab(g, race, marital, pct = "col")) == "n"), 1L)
  testthat::expect_equal(sum(row_labels(tab(g, race, c(marital, relig), pct = "col")) == "n"), 1L)
  # 2+ row_vars: the Phase 14a regression lost the row entirely (0). Under na = "keep" the sub-tables
  # share one base, so the Phase 14n collapse (Phase 18m: opt-in via common_totrow = TRUE) leaves a
  # SINGLE n row (still catches the 0-row regression).
  testthat::expect_equal(
    sum(row_labels(tab(g, c(race, marital), relig, pct = "col", common_totrow = TRUE)) == "n"), 1L)
  testthat::expect_equal(
    sum(row_labels(tab(g, c(race, marital), c(relig, partyid), pct = "col", common_totrow = TRUE)) == "n"), 1L)
  testthat::expect_equal(
    sum(row_labels(tab(g, c(race, marital, partyid), relig, pct = "col", common_totrow = TRUE)) == "n"), 1L)
  # Phase 18m: the DEFAULT (common_totrow = FALSE) keeps one n row PER row_var.
  testthat::expect_equal(sum(row_labels(tab(g, c(race, marital), relig, pct = "col")) == "n"), 2L)
  # DIFFERENT bases (na = "drop" on an uneven fixture) do NOT collapse -> one n row per sub-table.
  gu <- gss_uneven()
  testthat::expect_equal(
    sum(row_labels(tab(gu, c(race, marital), relig, pct = "col", na = "drop")) == "n"), 2L)
  testthat::expect_equal(
    sum(row_labels(tab(gu, c(race, marital, partyid), relig, pct = "col", na = "drop")) == "n"), 3L)
})

testthat::test_that("each n row sits in its OWN sub-table, right after that sub-table's Total", {
  # DIFFERENT bases (na = "drop" on the uneven fixture) so the 14n collapse keeps both sub-tables' totals
  m <- suppressMessages(tabxplor:::tab_materialize_extras(
    tab(gss_uneven(), c(race, marital), relig, pct = "col", na = "drop"),
    backend = "text", pvalue = FALSE))
  lv <- as.character(m$levels)
  # the n row directly follows its Total row, and stays inside its group
  testthat::expect_equal(lv[which(lv == "n") - 1L], c("Total", "Total"))
  testthat::expect_equal(as.character(m$row_var)[lv == "n"], c("race", "marital"))
})

testthat::test_that("add_pct keeps the historical Total | row_pct | n order", {
  lv <- row_labels(tab(forcats::gss_cat, c(race, marital), relig, pct = "col", add_pct = TRUE))
  i  <- which(lv == "Total")[1]
  testthat::expect_equal(lv[i:(i + 2L)], c("Total", "row_pct", "n"))
})

testthat::test_that("the n row carries the column's real unweighted base", {
  m <- tabxplor:::tab_materialize_extras(
    tab(forcats::gss_cat, c(race, marital), relig, pct = "col"),
    backend = "text", pvalue = FALSE)
  lv <- as.character(m$levels)
  testthat::expect_equal(get_num(m[["Protestant"]])[lv == "n"],
                         get_n(m[["Protestant"]])[lv == "Total"])
})


# ---- Phase 14n: one Total row for several row_vars (display-only collapse) --------------------------

# materialise for a backend, count the visible Total rows
n_totrows <- function(tt, backend = "text") {
  m <- suppressMessages(tabxplor:::tab_materialize_extras(tt, backend = backend, pvalue = FALSE))
  sum(tabxplor:::is_totrow(m))
}

testthat::test_that("compacted several-row_vars table collapses its identical Total rows (common_totrow)", {
  g <- forcats::gss_cat
  t <- tab(g, c(marital, race), relig, pct = "row", common_totrow = TRUE)
  # the CORE object keeps every per-block Total row (collapse is display-only) ...
  testthat::expect_equal(sum(tabxplor:::is_totrow(t)), 2L)
  # ... the displayed table shows exactly one, and the core nrow is unchanged
  testthat::expect_equal(n_totrows(t), 1L)
  testthat::expect_equal(nrow(t), 11L)
  # Phase 18m: the kept shared Total sits in its OWN group (a blank row_var, not the last block's)
  m  <- suppressMessages(tabxplor:::tab_materialize_extras(t, backend = "text", pvalue = FALSE))
  rv <- tab_get_vars(m)$row_var
  testthat::expect_equal(as.character(m$row_var)[tabxplor:::is_totrow(m)], "")
  keep_marital <- which(as.character(m$row_var) == "marital")
  testthat::expect_identical(format(m[["Protestant"]][keep_marital]),
                             format(t[["Protestant"]][keep_marital]))
})

testthat::test_that("collapse reaches the render model: one Total row, bold, at the bottom", {
  t  <- tab(forcats::gss_cat, c(marital, race), relig, pct = "row", color = "diff", common_totrow = TRUE)
  tb <- tabxplor:::tab_export_prep(t, backend = "kable")$tables[[1]]
  testthat::expect_length(tb$roles$totrows, 1L)
  testthat::expect_true(nrow(tb$tab) %in% tb$bold_rows)      # the kept Total is bold
})

testthat::test_that("pct = 'col' collapse drops each redundant Total AND its base n row", {
  m  <- suppressMessages(tabxplor:::tab_materialize_extras(
    tab(forcats::gss_cat, c(marital, race), relig, pct = "col", common_totrow = TRUE),
    backend = "text", pvalue = FALSE))
  lv <- as.character(m$levels)
  testthat::expect_equal(sum(lv == "Total"), 1L)            # one Total block, not two
  testthat::expect_equal(sum(lv == "n"), 1L)
  # the survivors keep the Total | n order, in their own (blank row_var) group (Phase 18m)
  i <- which(lv == "Total")
  testthat::expect_equal(lv[i:(i + 1L)], c("Total", "n"))
  testthat::expect_equal(as.character(m$row_var)[i], "")
})

testthat::test_that("genuinely different totals (na='drop') are all kept, with one message", {
  gu <- gss_uneven()
  # pct='row': the folded (n=...) in the Total cell already differs (common_totrow tries to collapse,
  # the differing bases keep both + one message)
  t1 <- tab(gu, c(marital, race), relig, pct = "row", na = "drop", common_totrow = TRUE)
  testthat::expect_equal(n_totrows(t1), 2L)
  # pct='col': the Total row is always 100%, but the block comparison catches the differing base n row
  t2 <- tab(gu, c(marital, race), relig, pct = "col", na = "drop", common_totrow = TRUE)
  testthat::expect_equal(n_totrows(t2), 2L)
  # the message fires (force it past .frequency = "once")
  withr::local_options(rlib_message_verbosity = "verbose")
  testthat::expect_message(
    tabxplor:::tab_materialize_extras(t1, backend = "text", pvalue = FALSE),
    "na")
})

testthat::test_that("collapse leaves tab_vars and single-row_var tables untouched", {
  g <- forcats::gss_cat
  # a tab_vars table is never compacted; its per-subtable totals are real, not duplicates
  t_tv <- tab(g, marital, relig, year, pct = "row")
  testthat::expect_false(isTRUE(tabxplor:::get_vars_attr(t_tv)$compacted))
  testthat::expect_equal(n_totrows(t_tv), sum(tabxplor:::is_totrow(t_tv)))
  testthat::expect_gt(n_totrows(t_tv), 1L)
  # a single-row_var table has one Total, untouched
  testthat::expect_equal(n_totrows(tab(g, marital, relig, pct = "row")), 1L)
})

testthat::test_that("comp='all' and mean (numeric col_var) compacted tables collapse", {
  g <- forcats::gss_cat
  testthat::expect_equal(
    n_totrows(tab(g, c(marital, race), relig, pct = "row", comp = "all", common_totrow = TRUE)), 1L)
  testthat::expect_equal(
    n_totrows(tab(g, c(marital, race), tvhours, pct = "row", common_totrow = TRUE)), 1L)
})

testthat::test_that("Phase 14n Part B: compacted table gets one p-value row PER block, no list-col warning", {
  g <- forcats::gss_cat
  t <- tab(g, c(marital, race), relig, pct = "row", test = TRUE, common_totrow = TRUE)
  testthat::expect_no_warning(
    m <- tabxplor:::tab_materialize_extras(t, backend = "text", pvalue = TRUE))
  lv <- as.character(m[[tab_get_vars(m)$row_var]])
  # Phase 18m: the p-value row label now states the test ("pvalue (Chi2)"); match its prefix.
  is_pv <- grepl("^pvalue", lv)
  testthat::expect_equal(sum(is_pv), 2L)                                # one per block
  # each p-value row carries its own block's row_var, and sits after that block's rows
  testthat::expect_equal(as.character(m$row_var)[is_pv], c("marital", "race"))
  # the redundant Total collapsed but each block keeps its own (different) p-value
  testthat::expect_equal(sum(tabxplor:::is_totrow(m)), 1L)
})

testthat::test_that("Phase 14n Part B: tab_vars and plain p-value placement unchanged", {
  g <- forcats::gss_cat
  m_tv <- tabxplor:::tab_materialize_extras(
    tab(g, marital, relig, year, pct = "row", test = TRUE), backend = "text", pvalue = TRUE)
  lv_tv <- as.character(m_tv[[tab_get_vars(m_tv)$row_var]])
  # Phase 18m: the p-value row label now states the test ("pvalue (Chi2)"); match its prefix.
  testthat::expect_equal(sum(grepl("^pvalue", lv_tv)), dplyr::n_distinct(forcats::gss_cat$year))  # one per year
  m_pl <- tabxplor:::tab_materialize_extras(
    tab(g, marital, relig, pct = "row", test = TRUE), backend = "text", pvalue = TRUE)
  lv_pl <- as.character(m_pl[[tab_get_vars(m_pl)$row_var]])
  testthat::expect_equal(sum(grepl("^pvalue", lv_pl)), 1L)              # one bottom row
})


# ---- Phase 17c: honest p-value cells -- stored in the `pvalue` field, coloured by an explicit rule ----
testthat::test_that("p-value cell stores p in the pvalue field, not pct/var/diff, and shows no star", {
  m   <- tabxplor:::tab_materialize_extras(
    tab(forcats::gss_cat, marital, race, pct = "row", color = "diff", test = TRUE),
    backend = "text", pvalue = TRUE)
  col <- m[[names(m)[purrr::map_lgl(m, is_fmt)][1]]]
  pv  <- which(tabxplor:::display_primary(get_display(col)) == "pvalue")
  testthat::expect_length(pv, 1L)
  testthat::expect_false(is.na(get_pvalue(col)[pv]))                    # honest: p in the pvalue field
  testthat::expect_true(is.na(get_pct(col)[pv]))                       # no more pct/var double-write
  testthat::expect_true(is.na(get_diff(col)[pv]))                      # no more diff = -0.5 magic
  testthat::expect_identical(get_stars(col)[pv], "")                   # a test row never prints a star
  testthat::expect_false(any(get_col_var(col) == "chi2_cols"))         # the write-only marker is gone
})

testthat::test_that("a NON-significant p-value cell fires red under EVERY color_signif policy (defect 5)", {
  set.seed(1)                                                          # independent data -> non-significant chi2
  n <- 400
  d <- data.frame(a = factor(sample(c("x", "y", "z"), n, TRUE)),
                  b = factor(sample(c("p", "q"),      n, TRUE)))
  slot_pv <- function(signif) {
    t   <- tab(d, a, b, pct = "row", color = "diff", color_signif = signif, test = TRUE)
    m   <- tabxplor:::tab_materialize_extras(t, backend = "text", pvalue = TRUE)
    col <- m[[names(m)[purrr::map_lgl(m, is_fmt)][1]]]
    pv  <- which(tabxplor:::display_primary(get_display(col)) == "pvalue")
    testthat::expect_gt(get_pvalue(col)[pv], 0.05)                     # the fixture really is non-significant
    tabxplor:::fmt_color_slots(col, tabxplor:::fmt_color_plan(col))[pv]
  }
  # deepest UNDER slot (deep red) in all three -- before 17c only "ignore" fired (the fake ci_inf=0 bug)
  testthat::expect_gte(slot_pv("ignore"),            5L)
  testthat::expect_gte(slot_pv("grey_non_signif"),   5L)
  testthat::expect_gte(slot_pv("guaranteed_effect"), 5L)
})


# ---- Phase 17c: the row-role model -- stored kind, not the English label ----------------------------
testthat::test_that("materialised synthetic rows carry a STORED role aligned to the rows", {
  m  <- tabxplor:::tab_materialize_extras(
    tab(forcats::gss_cat, c(race, marital), relig, pct = "col", n = "range", add_pct = TRUE, test = TRUE),
    backend = "text", pvalue = TRUE)
  rr  <- tabxplor:::tab_row_roles(m)
  testthat::expect_length(rr, nrow(m))
  lab <- as.character(m[[tabxplor:::tab_render_vars(m)$row_var]])
  # the stored role agrees with the (English) label on every synthetic row
  testthat::expect_true(all(rr[lab == "n"]       == "n"))
  testthat::expect_true(all(rr[lab == "row_pct"] == "pct"))
  testthat::expect_true(all(rr[lab == "pvalue"]  == "pvalue"))
  testthat::expect_true(all(rr[lab == "Total"]   == "total"))
  testthat::expect_true(any(rr == "n") && any(rr == "pct") && any(rr == "pvalue"))
})

testthat::test_that("the stored role WINS over a relabelled row (jamovi-gettext robustness)", {
  m   <- tabxplor:::tab_materialize_extras(
    tab(forcats::gss_cat, race, relig, pct = "col", n = "range", add_pct = TRUE, test = TRUE),
    backend = "text", pvalue = TRUE)
  rvc <- tabxplor:::tab_render_vars(m)$row_var
  # simulate a translated UI: rename the n / row_pct / pvalue labels away from English. Phase 18m: the
  # p-value label now states the test ("pvalue (Chi2)"), so find each synthetic row's actual label by role.
  lv  <- levels(m[[rvc]])
  rr0 <- tabxplor:::tab_row_roles(m)
  cur <- vapply(c("n", "pct", "pvalue"),
                function(role) as.character(m[[rvc]])[which(rr0 == role)[1]], character(1))
  levels(m[[rvc]])[match(cur, lv)] <- c("effectif", "%_ligne", "p")
  # the STORED role vector is unchanged -> the synthetic rows are still known
  rr  <- tabxplor:::tab_row_roles(m)
  testthat::expect_true(any(rr == "n") && any(rr == "pct") && any(rr == "pvalue"))
  testthat::expect_true(all(tabxplor:::tab_row_roles(m)[!rr %in% "data"] != "data"))
  # Phase 19f: there is no vector left to strip -- the kind is IN the record, so a relabelled row
  # cannot lose it. The fallback fires only for a frame with no fmt columns at all.
  testthat::expect_true(all(tabxplor:::tab_row_roles(tibble::tibble(a = 1:3)) == "data"))
})

# --- Phase 19l: the helper columns DECLARE what they are --------------------------------------
# Before 19l the add_n `n` column and the add_pct `col_pct` column both carried the string
# "all_col_vars" as their `col_var` -- a tag that lies (they belong to NO col_var, not to all of
# them) and whose other, opposite meaning is the legacy tab_tot() grand-total column. Nothing
# asserted it either way: no `_golden/` fixture uses add_n at all. They declare a `role` now.
testthat::test_that("add_n / add_pct helper columns declare a role and carry no col_var", {
  t  <- tab(gss, marital, race, pct = "row", n = "range", add_pct = TRUE)
  mt <- tab_materialize_extras(t, backend = "xl")

  testthat::expect_true(all(c("n", "col_pct") %in% names(mt)))
  testthat::expect_identical(get_role(mt$n), "n")
  testthat::expect_identical(get_role(mt$col_pct), "pct")
  # they belong to no col_var -- and never again to the string the grand total uses
  testthat::expect_identical(unname(get_col_var(mt$n)), "")
  testthat::expect_identical(unname(get_col_var(mt$col_pct)), "")
  testthat::expect_false(any(get_col_var(mt) == "all_col_vars"))

  # the ONE predicate every consumer reads, and it must not sweep up a real level column
  testthat::expect_identical(names(mt)[fmt_is_helper_col(mt)], c("col_pct", "n"))
  testthat::expect_false(fmt_is_helper_col(mt[["Total"]]))

  # no extras asked for -> no helper column at all
  t0 <- tab(gss, marital, race, pct = "row", n = "no")
  testthat::expect_false(any(fmt_is_helper_col(tab_materialize_extras(t0, backend = "xl"))))
  # and the `n` one is xl-only: 17g folds the base into the Total cell on the text backends instead
  # of building a column to throw away, so only the add_pct helper survives there
  mtxt <- tab_materialize_extras(t, backend = "text")
  testthat::expect_identical(names(mtxt)[fmt_is_helper_col(mtxt)], "col_pct")
})
