
# === SECTION: the shared render model =============================================================

gss <- fx_gss()


t_basic <- tab(gss, race, marital, pct = "row", color = "diff", test = TRUE)

t_multi <- tab(gss, race, c(marital, relig), pct = "row", color = "diff")

t_tv    <- tab(gss, race, marital, year, pct = "row", color = "diff")


testthat::test_that("vars are detected correctly", {
  rd <- tabxplor:::tab_export_prep(t_basic, backend = "kable", wrap = NULL)$tables[[1]]
  testthat::expect_identical(rd$vars$row_var, "race")
  testthat::expect_true("marital" %in% rd$vars$col_vars)  # also carries "all_col_vars" (the Total)
  testthat::expect_length(rd$vars$tab_vars, 0L)
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


testthat::test_that("list_method keeps a non-mergeable list as N tables; else it errors", {
  tv_list <- list(tab(gss, race, marital, year, pct = "row", color = "diff"),
                  tab(gss, relig, marital, year, pct = "row", color = "diff"))
  # list_method = TRUE (tab_md) -> one render table per input, each not degraded
  p <- tabxplor:::tab_export_prep(tv_list, backend = "md", drop_tab_vars = FALSE,
                                  wrap = NULL, list_method = TRUE)
  testthat::expect_length(p$tables, 2L)
  testthat::expect_false(p$tables[[1]]$vars$degrade)
  testthat::expect_false(p$tables[[2]]$vars$degrade)
  # list_method = FALSE (tab_kable) -> historical error
  testthat::expect_error(
    tabxplor:::tab_export_prep(tv_list, backend = "kable", wrap = NULL, list_method = FALSE),
    "no tab_vars"
  )
})


# === SECTION: tab_bold_rows edge (md vs kable style) =========================

testthat::test_that("tab_bold_rows: no discriminating column -> integer(0) (no anchor rows)", {
  # Phase 18m: the anchor signal is `ref_alltot | is_refrow`; zero discriminating columns -> NO anchor
  # rows (universally, was: md integer(0) but kable ALL rows -- the binomial exp=FALSE all-bold bug).
  none <- list(c(FALSE, FALSE, FALSE), c(FALSE, FALSE, FALSE))
  testthat::expect_identical(tabxplor:::tab_bold_rows(none), integer(0))
  # a normal discriminating column -> the anchor row
  disc <- list(c(FALSE, FALSE, TRUE), c(FALSE, FALSE, TRUE))
  testthat::expect_identical(tabxplor:::tab_bold_rows(disc), 3L)
})


testthat::test_that("`vars` survives dplyr verbs, and a stale one loses to the real columns", {
  merged <- tab(gss, c(race, relig), marital, pct = "row")
  for (f in list(function(x) dplyr::filter(x, TRUE),
                 function(x) dplyr::mutate(x, zz = 1),
                 function(x) dplyr::ungroup(x),
                 function(x) dplyr::arrange(x))) {
    testthat::expect_length(tab_get_vars(f(merged))$tab_vars, 0L)
  }
  # Phase 19f degraded mode: strip the declaration off the label column (what
  # `mutate(levels = as.character(levels))` does) -> the heuristic fallback, clearly marked as such.
  bare <- tab(gss, race, marital, pct = "row")
  bare$race <- factor(as.character(bare$race), levels = levels(bare$race))
  testthat::expect_null(tabxplor:::tab_declared_vars(bare))
  testthat::expect_equal(tab_get_vars(bare)$row_var, "race")   # heuristic fallback
})


testthat::test_that("a table with no recorded roles still detects them (tab_num / hand-built)", {
  leaf <- tab_num(gss, race, age)
  # Phase 19i: the numeric leaf records its identity like the factor one (they share leaf_finish()).
  # It used to record NO `meta` at all -- no `spec$kind`, and no `vars$wt` for the weight footer.
  testthat::expect_equal(tab_kind(leaf), "crosstab")
  testthat::expect_null(tabxplor:::get_vars_attr(leaf)$wt)          # unweighted -> nothing to record
  testthat::expect_equal(tabxplor:::get_vars_attr(tab_num(gss, race, age, wt = tvhours))$wt,
                         "tvhours")
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


testthat::test_that("roles$sd_cols finds the Excel sd aside column, ungated by var_names", {
  # The sd is an ordinary `{sd}` aside now, so mat_aside_cols() splits it off like any other and the
  # role is "aside". `sd_cols` survives as the ONE definition tab_xl's narrow column width reads --
  # a width is not a naming decision, so it is not gated on `var_names`.
  pr <- function(vn) tabxplor:::tab_export_prep(
    tab(gss, marital, c(race, tvhours), pct = "row", display = "mean_sd"), backend = "xl",
    list_method = TRUE, var_names = vn, compute = c("refs", "bold"))$tables[[1]]
  for (vn in c("both", "cols", "rows", "none")) {
    testthat::expect_equal(names(pr(vn)$roles$sd_cols), "tvhours_sd", info = vn)
  }
  # the HEADER, by contrast, is gated: no span row -> the level header must name the variable itself
  # ... and the sd column, which the RENDER carved out, is named by its unit rather than by a level
  # header: it has no level of the column variable to name (see "which row a column says its name in")
  h <- function(vn) { p <- pr(vn); p$col_var_header$clean[p$roles$fmt_cols] }
  un <- function(vn) { p <- pr(vn); p$col_var_header$unit[p$roles$fmt_cols] }
  testthat::expect_true("mean" %in% h("both"))
  testthat::expect_false("sd" %in% h("both"))
  testthat::expect_true("<sd>" %in% un("both"))
  testthat::expect_false("tvhours_sd" %in% h("none"))
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
  d <- fx_gss() |>
    dplyr::mutate(married = factor(dplyr::if_else(marital == "Married", "Married", "Not married")))
  # Phase 14w (item 3): a single-model reg column is now named "Model_OR" while its col_var names the
  # OUTCOME ("married: Married") -> name != col_var -> the span is KEPT (it labels the outcome; with
  # empirical companions it spans them all with no border between).
  testthat::expect_true(span(tab_reg(d, "married", c("race", "age"), family = "binomial")))
})


# --- Phase 22b-iv: the [outcome] bracket never reaches an exported header -------------------------

test_that("the outcome bracket is stripped in EVERY backend, wrapped names included", {
  d <- suppressWarnings(fx_gss_fmt())
  t <- suppressWarnings(suppressMessages(
    tab_reg(d, c("married", "tvhours"), c("race", "relig"), empirical = TRUE)))
  testthat::expect_true(any(grepl("[married]", names(t), fixed = TRUE)))   # the console keeps it
  for (b in c("kable", "md", "xl")) {
    cl <- tabxplor:::tab_export_prep(t, backend = b)$tables[[1]]$col_var_header$clean
    testthat::expect_false(any(grepl("[", cl, fixed = TRUE)), info = b)
  }
  # html WRAPS the header names first, so the separator before the bracket is not a plain space
  h <- as.character(tab_html(t, print = FALSE))
  testthat::expect_false(grepl("[married]", substr(h, 1L, regexpr("</thead>", h)), fixed = TRUE))
})


testthat::test_that("a whole-table helper column takes no variable name", {
  # tab_reg()'s base-count column was headed "n" on the variable-name row AND on its own header.
  d <- dplyr::mutate(gss, married = factor(.data$marital == "Married"))
  m <- tab_reg(d, outcome = "married", predictors = "race", family = "binomial")
  rd  <- tabxplor:::tab_export_prep(m, backend = "kable", wrap = NULL)$tables[[1]]
  cvh <- rd$col_var_header
  j   <- which(names(rd$tab) == "n")
  testthat::expect_identical(cvh$clean[[j]], "n")
  testthat::expect_identical(cvh$label[[j]], "")      # not a variable
  # a helper THE TABLE ALREADY HAD (a regression's own `n`, col_var "n") keeps both lines, exactly as
  # the console prints a name over a type tag. Only a render-carved one drops its header.
  testthat::expect_identical(cvh$unit[[j]],  "<n>")
})


# === Phase 22g-v: a name is printed once ==========================================================

testthat::test_that("a predictor-subset comparison names its models once, in every backend", {
  d <- suppressWarnings(fx_gss_fmt())
  t <- suppressMessages(tab_reg(
    d, "married",
    list("+ who they are" = c("race", "relig"), "+ and where" = c("race", "relig", "partyid")),
    measure = "difference", display = "est", stats = "no"))
  # the model label IS the col_var here, so the span row above the level row would print it twice
  for (b in c("kable", "md", "xl")) {
    cvh <- tabxplor:::tab_export_prep(t, backend = b)$tables[[1]]$col_var_header
    testthat::expect_false(any(nzchar(cvh$label)), info = b)
  }
  # ⚠ html WRAPS the column NAMES before the header is built and leaves the col_var attribute raw,
  # so the guard compared a wrapped string to an unwrapped one and silently stopped firing there
  h    <- as.character(tab_html(t, print = FALSE))
  head <- substr(h, 1L, regexpr("</thead>", h))
  testthat::expect_identical(lengths(regmatches(head, gregexpr("who", head, fixed = TRUE)))[[1]], 1L)
  # ...and the two models remain two BLOCKS: the vertical rule reads the col_var attribute, which
  # the dropped span row never touched
  testthat::expect_identical(unname(get_col_var(t)[c("+ who they are", "+ and where")]),
                             c("+ who they are", "+ and where"))
})


# === SECTION: the (col_var, col_group) block identity =============================================

gss_fmt <- fx_gss_fmt()


spread_tab <- function(...) {
  d <- dplyr::filter(gss_fmt, year %in% c(2000, 2014))
  tab(d, marital, race, year, pct = "row", spread_vars = year, test = TRUE, color = "diff", ...)
}


test_that("col_group reconciles like col_var: same value kept, a mismatch neutralised", {
  a <- tabxplor:::set_col_group(fmt(1:2, pct = c(.1, .2)), "g1")
  b <- tabxplor:::set_col_group(fmt(3:4, pct = c(.3, .4)), "g1")
  c2 <- tabxplor:::set_col_group(fmt(5:6, pct = c(.5, .6)), "g2")
  expect_identical(get_col_group(vctrs::vec_c(a, b)), "g1")
  expect_identical(get_col_group(vctrs::vec_c(a, c2)), "")
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
  sp <- tab(dplyr::filter(gss_fmt, year %in% c(2000, 2014)), rincome, married, year,
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


test_that("an ordered row variable survives every leaf and every synthetic row", {
  # both were hard aborts: the numeric leaf minted an "NA" level the factor leaf had not, and the
  # pct = "col" count row minted a PLAIN factor where the index column was ordered.
  expect_no_error(tab(gss_fmt, rincome, party3, race, pct = "col", na = "drop", totaltab = "table") |>
                    tab_html())
  expect_no_error(tab(gss_fmt, c(race, rincome), c(party3, tvhours), pct = "row", na = "drop_all") |>
                    tab_html())
})


# === SECTION: the shared render model =============================================================

gss <- fx_gss()


t_basic <- tab(gss, race, marital, pct = "row", color = "diff", test = TRUE)


t_multi <- tab(gss, race, c(marital, relig), pct = "row", color = "diff")


t_tv    <- tab(gss, race, marital, year, pct = "row", color = "diff")


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


# === SECTION: recorded variable roles (Phase 14d) ===========================

testthat::test_that("the DECLARED index columns say what a merged table cannot show", {
  # tab_compact() renames column 1 to the literal "levels" and keeps the row-variable names only as
  # values of a synthetic column named "row_var" -- so the column-type heuristic read that column as a
  # tab_var. Phase 19f: the columns DECLARE their role, so it is read, not recorded and not guessed.
  merged <- tab(gss, c(race, relig), marital, pct = "row")
  v <- tabxplor:::tab_declared_vars(merged)
  testthat::expect_equal(v$row_vars, c("race", "relig"))   # the SOURCE names, unrecoverable otherwise
  testthat::expect_true(v$compacted)
  testthat::expect_equal(v$var_col, "row_var")
  testthat::expect_length(v$tab_vars, 0L)
  # tab_get_vars() keeps its COLUMN-name contract, and now tells the truth about tab_vars
  testthat::expect_equal(tab_get_vars(merged)$row_var, "levels")
  testthat::expect_length(tab_get_vars(merged)$tab_vars, 0L)
  # a real tab_var is still reported
  testthat::expect_equal(tab_get_vars(tab(gss, race, marital, year, pct = "row"))$tab_vars, "year")
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
  t_num <- tab(gss, marital, c(race, tvhours), pct = "row", color = "diff", display = "mean_sd")
  hdr <- function(backend, vn = "both") {
    cvh <- tabxplor:::tab_export_prep(t_num, backend = backend, wrap = NULL,
                                      var_names = vn)$tables[[1]]$col_var_header
    stats::setNames(cvh$clean, cvh$label)
  }
  # text backends print the sd inside the cell ("49 (s17)"), so the header says so
  testthat::expect_equal(unname(hdr("kable")[names(hdr("kable")) == "tvhours"]), "mean (sd)")
  # Excel splits it into a real `_sd` column, named by its unit (the render carved it out)
  testthat::expect_equal(unname(hdr("xl")[names(hdr("xl")) == "tvhours"]), c("mean", ""))
  # ⚠ the DEFAULT aside is the coefficient of variation, and it NAMES ITSELF in the cell
  # ("49 (cv 36%)"), so the level header must not say it twice.
  t_cv <- tab(gss, marital, c(race, tvhours), pct = "row")
  cvh0 <- tabxplor:::tab_export_prep(t_cv, backend = "kable",
                                     wrap = NULL)$tables[[1]]$col_var_header
  testthat::expect_equal(cvh0$clean[cvh0$label == "tvhours"], "mean")
  testthat::expect_equal(cvh0$unit [cvh0$label == "tvhours"], "<mean (cv)>")

  # ... but the level header may only name the STATISTIC while the span names the VARIABLE. Blanking
  # the span after the fact (as Phase 14i did) left `var_names = "none"` with a column headed "mean"
  # and the variable's name NOWHERE -- which is why the decision moved into tab_col_var_header().
  testthat::expect_equal(unname(hdr("kable", "none")), c("marital", "Other", "Black", "White",
                                                         "Total", "tvhours"))
  testthat::expect_true("tvhours" %in% hdr("xl", "none"))

  # A mean column showing an interval and nothing else says so: the header is built from the column's
  # OWN template (fmt_header_label), so it cannot promise a number the cell does not print.
  cvh <- tabxplor:::tab_export_prep(tab_num(gss, marital, tvhours, ci = "cell"),
                                    backend = "kable", wrap = NULL)$tables[[1]]$col_var_header
  testthat::expect_equal(cvh$clean[cvh$label == "tvhours"], "mean-ci")
})


# === SECTION: the (col_var, col_group) block identity =============================================

gss_fmt <- fx_gss_fmt()


spread_tab <- function(...) {
  d <- dplyr::filter(gss_fmt, year %in% c(2000, 2014))
  tab(d, marital, race, year, pct = "row", spread_vars = year, test = TRUE, color = "diff", ...)
}


test_that("an unspread table carries an empty col_group, and one block per col_var", {
  t <- tab(gss_fmt, marital, race, pct = "row", color = "diff")
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


test_that("`comp = 'all'` leaves ONE reference cell, in the total-table block, and says so", {
  sp <- tab(dplyr::filter(gss_fmt, year %in% c(2000, 2014)), rincome, married, year,
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
