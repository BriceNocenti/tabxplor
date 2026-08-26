
# === format() rendering of general + expert templates =============================

testthat::test_that("an expert {diff} [{ci}] template renders both fields", {
  x <- fmt(n = c(100L, 100L), scale = "points", pct_type = "row", pct = c(0.4, 0.6), diff = c(0.1, -0.1),
           ci_inf = c(0.02, -0.2), ci_sup = c(0.18, -0.02), display = "diff")
  out <- format(set_display(x, "{diff} [{ci}]"))
  testthat::expect_true(all(grepl("\\[", out)))          # the [ci] literal + bracket present
  testthat::expect_true(all(grepl("^[+-]", out)))        # the diff sign leads (primary)
})


# === consumer safety: a hand-injected bad template must not crash any consumer =====

testthat::test_that("every display consumer survives a hand-injected malformed template", {
  # ⚠ written through the RAW setter: since 22g-vii the PUBLIC set_display() refuses this at the door
  # (asserted below). What is locked here is that a field already holding one -- a hand-mutate(), an
  # object saved by an older version -- still renders rather than erroring.
  x <- tabxplor:::fmt_set_display(
    fmt(n = c(10L, 20L), scale = "level_pct", pct_type = "row", pct = c(0.4, 0.6), display = "pct"),
    "{bad")
  testthat::expect_error(set_display(x, "{bad"), "Malformed")
  testthat::expect_no_error(format(x))
  testthat::expect_no_error(get_num(x))
  testthat::expect_no_error(set_num(x, c(1, 2)))
  testthat::expect_no_error(vctrs::vec_ptype_abbr(x))
  testthat::expect_no_error(vctrs::vec_ptype_full(x))
  testthat::expect_no_error(c(x, x))               # vec_ptype2 / arithmetic path
})


# === tab() integration: grouped tabs, list output, pct = "col" ====================

testthat::test_that("tab(display = ) works on grouped tabs, lists and pct = 'col'", {
  # grouped (tab_vars) -> a grouped_tab; the {} template survives dplyr reconstruction
  tg <- tab(fx_gss(), marital, race, year, pct = "row", display = "{pct} ({n})")
  fg <- if (is.data.frame(tg)) tg else tg[[1]]
  fcol <- fg[[which(purrr::map_lgl(fg, is_fmt))[1]]]
  testthat::expect_true(any(grepl("{", get_display(fcol), fixed = TRUE)))
  testthat::expect_no_error(invisible(capture.output(print(tg))))

  # pct = "col": composite renders (the primary is the col%), no crash
  tc <- tab(fx_gss(), marital, race, pct = "col", display = "{pct} ({n})")
  fc <- tc[[which(purrr::map_lgl(tc, is_fmt))[1]]]
  testthat::expect_match(format(fc)[1], "\\([0-9 ]+\\)$")
})


testthat::test_that("every exporter renders a composite table without error", {
  t1 <- tab(fx_gss(), marital, race, pct = "row", display = "{pct} ({n})")
  testthat::expect_no_error(tab_md(t1))
  testthat::expect_no_error(invisible(capture.output(print(t1))))
  testthat::expect_no_error(tab_kable(t1))   # dependency-free engine
  if (requireNamespace("openxlsx2", quietly = TRUE)) {
    p <- withr::local_tempfile(fileext = ".xlsx")
    testthat::expect_no_error(tab_xl(t1, path = p, replace = TRUE))
    # Excel shows the PRIMARY field (no in-cell composite) -- the file is a real workbook.
    testthat::expect_true(file.exists(p))
  }
})


# === display = "base_ci": the type-adaptive "{base} {ci}" alias =====================

testthat::test_that("tab(display = 'base_ci') == '{pct} {ci}' / '{mean} {ci}' per column type", {
  gss <- fx_gss()

  # factors, showing the DIFFERENCE CI a significance colour computes: byte-identical to the explicit
  # "{pct} {ci}" template, and every eligible value cell renders a [lo;hi] bracket.
  t_num <- gss |>
    tab(race, marital, pct = "row", color = TRUE, color_signif = "guaranteed_effect",
        display = "base_ci")
  t_tpl <- gss |>
    tab(race, marital, pct = "row", color = TRUE, color_signif = "guaranteed_effect",
        display = "{pct} {ci}")
  testthat::expect_identical(format(t_num), format(t_tpl))
  fcol <- t_num[[which(purrr::map_lgl(t_num, is_fmt))[2]]]
  testthat::expect_gt(sum(grepl("\\[.*;.*\\]", format(fcol))), 0L)      # rule 7: the CI really renders

  # numeric means: "{mean} {ci}", byte-identical to the explicit template
  testthat::expect_identical(
    format(tab(gss, race, age, ci = "cell", display = "base_ci")),
    format(tab(gss, race, age, ci = "cell", display = "{mean} {ci}"))
  )

  # mixed factor + numeric in ONE call: each column resolves by its own type. Compared COLUMN by
  # column -- the whole-tibble print truncates, and `{pct}` is explicit where `{base}` is relative,
  # so they agree on the factor columns and deliberately not on the numeric one.
  t_mix <- tab(gss, race, c(marital, age), pct = "row", ci = "cell", display = "base_ci")
  t_tpl <- tab(gss, race, c(marital, age), pct = "row", ci = "cell", display = "{pct} {ci}")
  testthat::expect_identical(format(t_mix$Married), format(t_tpl$Married))
  testthat::expect_identical(tabxplor:::fmt_var_kind(t_mix[["age"]]), "mean")
  testthat::expect_match(format(t_mix$age)[1], "^ *[0-9.]+ +\\[")      # {base} -> the MEAN there
})


testthat::test_that("tab_counts(display = 'base_ci') does not abort", {
  tc <- tibble::tibble(r = c("a", "a", "b"), c = c("x", "y", "x"), n = c(3, 2, 5))
  testthat::expect_no_error(tab_counts(tc, r, c, counts = n, display = "base_ci"))
})


# === micro-benchmark: the no-composite gate must be cheap (informational) ==========

testthat::test_that("display_primary() no-composite gate is negligible (informational)", {
  d  <- rep(c("pct", "n", "diff"), length.out = 1e6)
  tt <- system.time(for (i in 1:20) display_primary(d))[["elapsed"]]
  message(sprintf("display_primary() x20 on 1e6 no-composite cells: %.3fs", tt))
  testthat::succeed()
})


testthat::test_that("format(bold_split =) reports the primary's character RANGE", {
  x <- fmt(pct = c(.5), n = c(10L), scale = "level_pct", pct_type = "row", digits = 0L)
  rng <- function(tmpl) {
    o <- format(set_display(x, tmpl), bold_split = TRUE, special_formatting = TRUE)
    c(attr(o, "primary_from"), attr(o, "primary_nchar"), nchar(o))
  }
  testthat::expect_identical(rng("{pct} (n={n})"), c(1L, 3L, 10L))   # a prefix
  testthat::expect_identical(rng("({n}) {pct}"),   c(6L, 3L,  8L))   # ...and a suffix
})


testthat::test_that("the aside's colour is the theme's own chrome, resolved PER THEME", {
  # it IS grey2, the slot an uncoloured cell already takes -- one literal, and it must differ
  # between a light and a dark background, or the aside is baked for the wrong one.
  hex <- tabxplor:::color_secondary_hex
  testthat::expect_identical(hex("light"), tabxplor:::tx_chrome_hex("light")$grey2)
  testthat::expect_identical(hex("dark"),  tabxplor:::tx_chrome_hex("dark")$grey2)
  testthat::expect_false(identical(hex("light"), hex("dark")))
  # the stylesheet carries both, never one hex for every theme
  css <- tab_css(theme = "auto")
  testthat::expect_true(grepl(tabxplor:::tx_chrome_hex("dark")$grey2, css, fixed = TRUE))
  # the expert opt-out emits no rule at all: the aside then inherits the cell's own shade
  withr::local_options(tabxplor.color_whole_cell = TRUE)
  testthat::expect_false(grepl("tx-sec", tab_css(theme = "light"), fixed = TRUE))
})


testthat::test_that("the `coef` token names the quantity, not the artefact", {
  g <- fx_gss_fmt()
  # ONE name per quantity: the header composes "log(OR)" (reg_word_logged) and the type tag says the
  # same thing, so a reader never meets two words for one number.
  lg <- suppressMessages(tab_reg(g, outcome = "married", predictors = "race",
                                 family = "binomial", measure = "log", empirical = TRUE))
  testthat::expect_identical(vctrs::vec_ptype_abbr(lg[["Model_log(OR)"]]), "log(OR)")
  testthat::expect_identical(vctrs::vec_ptype_abbr(lg[["Obs_log(OR)"]]),   "log(OR)")
  # the acronym is the FAMILY's own measure, derived from REG_FAMILIES -- not a literal
  testthat::expect_identical(reg_own_word("binomial"), "OR")
  testthat::expect_identical(reg_own_word("poisson"),  "IRR")
  testthat::expect_identical(reg_own_word("ordinal"),  "cumOR")
  testthat::expect_identical(reg_own_word("gaussian"), "diff")
  # a `{coef}` asked for by hand on a multiplicative column is log(OR) and says so; on an additive
  # one it IS the difference
  m <- suppressMessages(tab_reg(g, outcome = "married", predictors = "race", family = "binomial"))
  testthat::expect_identical(fmt_coef_label(m$Model_OR), "log(OR)")
  testthat::expect_identical(fmt_coef_label(fmt(diff = 1, scale = "raw_diff")), "diff")
})


# Phase 22g-ii: A COLUMN IS NAMED BY WHAT IT ESTIMATES. A regression's baseline row prints the level
# its column's effects operate on (EST_SCALES$const_display) -- a percentage under a ratio, a mean
# under a beta -- and that second primary token used to name the whole column "mixed".
testthat::test_that("a baseline row does not rename its column, nor make it 'mixed'", {
  g <- fx_gss_fmt()
  g$married <- factor(ifelse(g$marital == "Married", "yes", "no"))
  ab <- function(...) vctrs::vec_ptype_abbr(
    (function(t) t[[names(t)[vapply(t, function(x)
      is_fmt(x) && identical(get_role(x), "model"), logical(1))][[1]]]])(
        suppressMessages(tab_reg(g, ..., empirical = FALSE, stats = FALSE))))
  testthat::expect_identical(ab("age", c("race", "rincome")), "diff")            # const "mean"
  testthat::expect_identical(ab("married", c("race", "rincome"), measure = "ratio"), "ratio")
  testthat::expect_identical(ab("married", c("race", "rincome")), "OR")          # const == est
  # ...and the baseline cell keeps its own number rather than printing an empty primary
  t <- suppressMessages(tab_reg(g, "age", c("race", "rincome"), stats = FALSE))
  mc <- t[[names(t)[vapply(t, function(x)
    is_fmt(x) && identical(get_role(x), "model"), logical(1))][[1]]]]
  testthat::expect_identical(get_display(mc)[[1]], "mean")
  testthat::expect_true(is.finite(get_num(mc)[[1]]))
})


testthat::test_that("a template is stamped wherever it renders ANYTHING, so a column is one layout", {
  g <- fx_gss_fmt()
  # the crude column of a NUMERIC predictor has an odds ratio and no risk difference: gating the
  # stamping on the primary left that one row showing a bare estimate while every other row showed
  # the requested layout -- two quantities in one column, and a "<mixed>" name over them.
  m <- suppressMessages(tab_reg(
    g, outcome = "married", predictors = c("race", "age"), empirical = TRUE, family = "binomial",
    display = "{diff} [{OR}] ({base})"))
  d <- get_display(m$Obs_OR)
  age <- which(!is.na(get_or(m$Obs_OR)) & is.na(get_diff(m$Obs_OR)))
  testthat::expect_gt(length(age), 0L)
  testthat::expect_true(all(d[age] == "{diff} [{OR}] ({base})"))
  testthat::expect_match(format(m$Obs_OR)[age[[1]]], "1/|[0-9]")      # the aside still renders
  # a cell with NOTHING of the template keeps its own token -- and does not make the column "mixed"
  testthat::expect_identical(vctrs::vec_ptype_abbr(m$Obs_OR),   "diff [OR] (obs%)")
  testthat::expect_identical(vctrs::vec_ptype_abbr(m$Model_OR), "diff [OR] (adj%)")
})


testthat::test_that("the name survives a column torn out of its table, and a statistic row", {
  g <- fx_gss_fmt()
  col <- tab(g, race, party3, pct = "row", na = "drop_all", color = TRUE)[[2]]
  # an fmt column keeps its fields and its attributes and nothing else -- and is still named
  testthat::expect_no_error(vctrs::vec_ptype_abbr(col))
  testthat::expect_identical(vctrs::vec_ptype_abbr(col), "row%")
  # a column IS NAMED BY ITS DATA: the chi2 p-value row and the base-count row `pct = "col"` appends
  # carry their own token, and letting them vote turned a percentage column into "mixed".
  x <- fmt(n = c(10L, 20L, 30L), pct = c(.4, .6, .02), scale = "level_pct", pct_type = "col",
           display = c("pct", "pct", "pvalue"), row_kind = c("data", "total", "pvalue"))
  testthat::expect_identical(vctrs::vec_ptype_abbr(x), "col%")
  y <- fmt(n = c(10L, 20L, 30L), pct = c(.4, .6, NA), scale = "level_pct", pct_type = "col",
           display = c("pct", "pct", "n"), row_kind = c("data", "total", "n"))
  testthat::expect_identical(vctrs::vec_ptype_abbr(y), "col%")
})


testthat::test_that("the three new layouts, and the acronym spelling", {
  g <- fx_gss_fmt()
  cell <- function(d) format(suppressMessages(
    tab(g, race, party3, pct = "row", na = "drop_all", color = TRUE, ref = 1,
        display = d))[[3]])[[2]]
  testthat::expect_identical(cell("base_ratio"), cell("{base} ({ratio})"))
  testthat::expect_identical(cell("base_or"),    cell("{base} ({or})"))
  testthat::expect_identical(cell("or_base"),    cell("{or} ({base})"))
  # the acronym is the same token, on its own and inside a template
  testthat::expect_identical(cell("OR"),   cell("or"))
  testthat::expect_identical(cell("{OR}"), cell("{or}"))
  # the 1.x layout spelling still resolves, to the preset that replaced it
  testthat::expect_identical(cell("or_pct"), cell("or_base"))
  testthat::expect_identical(cell("OR_pct"), cell("or_base"))
})


# Phase 22h: a bracket marks an ASIDE, so a template with no token outside brackets has no primary
# at all -- and the Total cell reduced to "({n_range})" is exactly that: a base count, not the
# number the table is about. Its type tag still reads `n`; the CELL reads as an aside.
testthat::test_that("a cell that is nothing but brackets is nothing but aside", {
  g <- fx_gss_fmt()
  t <- suppressMessages(tab(g, race, party3, pct = "row", na = "drop_all", display = "ratio"))
  m <- tabxplor:::tab_materialize_extras(t, backend = "text", pvalue = FALSE)
  testthat::expect_identical(unique(get_display(m$Total)), "({n_range})")
  f <- format(m$Total, bold_split = TRUE)
  testthat::expect_true(all(attr(f, "primary_from")  == 1L))
  testthat::expect_true(all(attr(f, "primary_nchar") == 0L))
  # every backend then paints the whole cell in the aside: html wraps it in one `tx-sec` span,
  # markdown leaves it unbolded on the bold Total row.
  h <- tab_html(t)
  testthat::expect_match(h, '<span class="tx-sec"[^>]*>\\([ 0-9\u2007-]+\\)</span>')
  testthat::expect_false(grepl('tx-sec[^>]*>\\(</span>', h))
  # the column still SAYS it holds a count -- the type tag drops the primary's own brackets
  testthat::expect_identical(tabxplor:::fmt_display_label(m$Total, "tag"), "n")
})



# Phase 22g-vi: the missing sibling of `base_ratio` / `base_or`. It was offered by the jamovi
# display ComboBox and by the generated .h.R, and picking it ABORTED -- there was no such preset.
testthat::test_that("`base_diff` is a preset, beside its ratio and odds-ratio siblings", {
  g <- fx_gss_fmt()
  testthat::expect_identical(tabxplor:::display_resolve("base_diff"), "{base} ({diff})")
  t <- suppressMessages(tab(g, race, party3, pct = "row", na = "drop_all",
                            display = "base_diff"))
  testthat::expect_match(format(t[[2]])[[1]], "%.*\\(.*%\\)")
  # every value the jamovi ComboBox offers resolves -- the rule the crash broke
  testthat::skip_if_not_installed("yaml")
  vals <- yaml::read_yaml(src_path("jamovi", "jmvtab.a.yaml"))$options
  vals <- Filter(function(o) identical(o$name, "display"), vals)[[1]]$options
  for (v in vapply(vals, function(o) as.character(o$name), character(1)))
    testthat::expect_no_error(tabxplor:::display_resolve(v))
})



# === SECTION: the display-only n / add_pct / p-value rows =========================================

withr::local_options(lifecycle_verbosity = "quiet", .local_envir = testthat::teardown_env())



gss <- fx_gss()


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
  g <- fx_gss()
  g$marital[1:800] <- NA
  g$race[1:40]     <- NA
  g
}


testthat::test_that("pct = 'col' add_n: one n row per sub-table, collapsed to one when bases match (14n)", {
  g <- fx_gss()
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
  lv <- row_labels(tab(fx_gss(), c(race, marital), relig, pct = "col", add_pct = TRUE))
  i  <- which(lv == "Total")[1]
  testthat::expect_equal(lv[i:(i + 2L)], c("Total", "row_pct", "n"))
})


testthat::test_that("the n row carries the column's real unweighted base", {
  m <- tabxplor:::tab_materialize_extras(
    tab(fx_gss(), c(race, marital), relig, pct = "col"),
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
  g <- fx_gss()
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
  t  <- tab(fx_gss(), c(marital, race), relig, pct = "row", color = "diff", common_totrow = TRUE)
  tb <- tabxplor:::tab_export_prep(t, backend = "kable")$tables[[1]]
  testthat::expect_length(tb$roles$totrows, 1L)
  testthat::expect_true(nrow(tb$tab) %in% tb$bold_rows)      # the kept Total is bold
})


testthat::test_that("pct = 'col' collapse drops each redundant Total AND its base n row", {
  m  <- suppressMessages(tabxplor:::tab_materialize_extras(
    tab(fx_gss(), c(marital, race), relig, pct = "col", common_totrow = TRUE),
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
  g <- fx_gss()
  # a tab_vars table is never compacted; its per-subtable totals are real, not duplicates
  t_tv <- tab(g, marital, relig, year, pct = "row")
  testthat::expect_false(isTRUE(tabxplor:::get_vars_attr(t_tv)$compacted))
  testthat::expect_equal(n_totrows(t_tv), sum(tabxplor:::is_totrow(t_tv)))
  testthat::expect_gt(n_totrows(t_tv), 1L)
  # a single-row_var table has one Total, untouched
  testthat::expect_equal(n_totrows(tab(g, marital, relig, pct = "row")), 1L)
})


testthat::test_that("comp='all' and mean (numeric col_var) compacted tables collapse", {
  g <- fx_gss()
  testthat::expect_equal(
    n_totrows(tab(g, c(marital, race), relig, pct = "row", comp = "all", common_totrow = TRUE)), 1L)
  testthat::expect_equal(
    n_totrows(tab(g, c(marital, race), tvhours, pct = "row", common_totrow = TRUE)), 1L)
})


testthat::test_that("Phase 14n Part B: compacted table gets one p-value row PER block, no list-col warning", {
  g <- fx_gss()
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
  g <- fx_gss()
  m_tv <- tabxplor:::tab_materialize_extras(
    tab(g, marital, relig, year, pct = "row", test = TRUE), backend = "text", pvalue = TRUE)
  lv_tv <- as.character(m_tv[[tab_get_vars(m_tv)$row_var]])
  # Phase 18m: the p-value row label now states the test ("pvalue (Chi2)"); match its prefix.
  testthat::expect_equal(sum(grepl("^pvalue", lv_tv)), dplyr::n_distinct(fx_gss()$year))  # one per year
  m_pl <- tabxplor:::tab_materialize_extras(
    tab(g, marital, relig, pct = "row", test = TRUE), backend = "text", pvalue = TRUE)
  lv_pl <- as.character(m_pl[[tab_get_vars(m_pl)$row_var]])
  testthat::expect_equal(sum(grepl("^pvalue", lv_pl)), 1L)              # one bottom row
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
    tab(fx_gss(), c(race, marital), relig, pct = "col", n = "range", add_pct = TRUE, test = TRUE),
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
    tab(fx_gss(), race, relig, pct = "col", n = "range", add_pct = TRUE, test = TRUE),
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



# === SECTION: composite width, ratio glyphs, spanning headers =====================================

mult_glyph <- intToUtf8(0x00d7)  # multiply sign

div_glyph  <- intToUtf8(0x00f7)  # divide sign


testthat::test_that("composite {} tokens are padded to a uniform width per column", {
  x <- set_display(
    fmt(n = c(17L, 5416L, 743L), scale = "level_pct", pct_type = "row", pct = c(1, 1, 1), display = "pct"),
    "{pct} (n={n})")
  f <- format(x)
  # the {n} field is right-aligned to the column-max width so numbers line up in a monospace font.
  testthat::expect_identical(f, c("100% (n=   17)", "100% (n=5 416)", "100% (n=  743)"))
  testthat::expect_length(unique(nchar(f)), 1L)
})


testthat::test_that("format() stays byte-identical (no primary_nchar attr) when bold_split is off", {
  x <- set_display(fmt(n = c(10L, 20L), scale = "level_pct", pct_type = "row", pct = c(0.4, 0.6), display = "pct"),
                   "{pct} (n={n})")
  testthat::expect_null(attr(format(x), "primary_nchar"))
  testthat::expect_identical(format(x), c("40% (n=10)", "60% (n=20)"))
})


testthat::test_that("ratio (rr) display shows the multiplicative x / div sign", {
  x <- set_display(
    fmt(n = rep(1L, 4), scale = "level_pct", pct_type = "row", pct = rep(0.5, 4),
        ratio = c(2, 0.5, 1, 0.998), display = "pct"),
    "rr")
  f <- format(x)
  # >= 1 -> "x<r>"; < 1 -> "/<1/r>"; a value ROUNDING to the neutral takes the over glyph, never the
  # confusing "/1.00". Only a REFERENCE cell loses the glyph (see the next test).
  testthat::expect_identical(
    f, c(paste0(mult_glyph, "2.00"), paste0(div_glyph, "2.00"),
         paste0(mult_glyph, "1.00"), paste0(mult_glyph, "1.00")))
})


testthat::test_that("a multiplicative cell keeps the decimals it ASKS for, and 0 takes the floor", {
  # DISPLAY_TOKENS$min_digits overrides ONLY 0: a ratio read against the x1.2 / x1.5 thresholds is
  # meaningless at "1", but a cell asking for 1 or 3 decimals gets exactly that.
  x <- set_display(
    fmt(n = rep(1L, 3), scale = "level_pct", pct_type = "row", pct = rep(0.5, 3),
        ratio = c(1.5, 0.25, 1.0624), display = "pct"),
    "rr")
  m <- mult_glyph; d <- div_glyph
  testthat::expect_identical(format(set_digits(x, 0L)),
                             c(paste0(m, "1.50"), paste0(d, "4.00"), paste0(m, "1.06")))
  testthat::expect_identical(format(set_digits(x, 1L)),
                             c(paste0(m, "1.5"), paste0(d, "4.0"), paste0(m, "1.1")))
  testthat::expect_identical(format(set_digits(x, 3L)),
                             c(paste0(m, "1.500"), paste0(d, "4.000"), paste0(m, "1.062")))
  # the same rule on the odds-ratio glyph, and the same floor
  y <- fmt(n = rep(1L, 3), scale = "odds_ratio", pct_type = "row",
           or = c(2, 0.5, 1.0624), display = "or")
  testthat::expect_identical(format(set_digits(y, 0L), special_formatting = TRUE),
                             c("2.00", "1/2.00", "1.06"))
  testthat::expect_identical(format(set_digits(y, 1L), special_formatting = TRUE),
                             c("2.0", "1/2.0", "1.1"))
})


# Phase 13c-ii: composite partial bold (first token bold, rest plain).

testthat::test_that("md bolds only the primary field of a composite cell in a bold row", {
  t  <- tab(fx_gss(), marital, race, pct = "row", n = "range")
  md <- tab_md(t, color = FALSE, print = FALSE, css = FALSE)
  # the bold Total row's composite: pct bold, "(...)" plain -> "**100%** (...)". Phase g (A6): the
  # join is a non-breaking space (U+00A0) so html does not wrap the composite.
  nbsp <- intToUtf8(160L)
  testthat::expect_match(md, paste0("\\*\\*100%\\*\\*", nbsp, "\\("))
  # NOT whole-cell bold ("**100% (...)**")
  testthat::expect_false(grepl(paste0("\\*\\*100%", nbsp, "\\([0-9 ]+\\)\\*\\*"), md))
})


testthat::test_that("html bolds only the primary field of a composite bold cell", {
  t <- tab(fx_gss(), marital, race, pct = "row", n = "range")
  h <- as.character(tab_kable(t))
  # a normal-weight span wraps the "(...)" suffix of the bold composite cells
  testthat::expect_true(grepl("font-weight: ?normal", h))
})


testthat::test_that("kable tooltip shows the ratio field (not OR) under a 'ratio:' label", {
  t <- tab(fx_gss(), marital, race, pct = "row", color = c("diff", "ratio"))
  tt <- tabxplor:::tab_tooltip_text(t[[2]])
  testthat::expect_true(any(grepl("ratio:", tt, fixed = TRUE)))
  testthat::expect_false(any(grepl("rr:", tt, fixed = TRUE)))
  # the ratio value is present (x or div sign), not an empty field.
  testthat::expect_true(any(grepl(paste0(mult_glyph, "|", div_glyph), tt)))
})


# Phase 13c-iii: col_var spanning headers + level-name suffix stripping.

testthat::test_that("shared header model: spanning labels + suffix-stripped clean names", {
  d <- fx_gss()
  d$grp <- factor(ifelse(d$age < 40, "Young", "Other"))   # "Other" collides with race "Other"
  t <- tab(d, row_vars = marital, col_vars = c(race, grp), pct = "row")
  cvh <- tabxplor:::tab_export_prep(t, backend = "md", compute = "refs",
                                    list_method = TRUE)$tables[[1]]$col_var_header
  testthat::expect_true("Other_race" %in% names(t))        # stored uniquely
  testthat::expect_false(any(cvh$clean == "Other_race"))   # but shown clean
  testthat::expect_equal(sum(cvh$clean == "Other"), 2L)    # both collisions -> "Other"
  testthat::expect_identical(cvh$label[names(t) == "Total"], "")  # Total stands alone
})


testthat::test_that("md/kable/html show the col_var name spanning header (single col_var too)", {
  t  <- tab(fx_gss(), marital, race, pct = "row")
  md <- tab_md(t, color = FALSE, print = FALSE, css = FALSE)   # css = FALSE: line 3 is the name row
  # Phase 14f: in markdown the name is the first BODY row (line 3: header, delimiter, then it). Above
  # the delimiter it was a second header row, which pandoc does not accept -- it rejected the whole
  # table. Locked by "tab_md() output is valid pandoc" in test-tab_md.R.
  lines <- strsplit(md, "\n")[[1]]
  testthat::expect_match(lines[3], "race")
  testthat::expect_match(lines[3], "[*]race[*]", perl = TRUE)   # italic: it reads as a sub-heading
  testthat::expect_no_match(lines[1], "race")
  hh <- as.character(tab_kable(t))
  testthat::expect_match(hh, 'colspan="3"[^>]*>race<')         # html engine colspan cell
})


# Phase 13c-iv: tabxplor_tabs list class + Viewer routing.

testthat::test_that("tab(output_list = TRUE) returns a tabxplor_tabs that behaves like a list", {
  t <- tab(fx_gss(), c(marital, relig), race, pct = "row", output_list = TRUE)
  testthat::expect_s3_class(t, "tabxplor_tabs")
  testthat::expect_true(is.list(t))
  testthat::expect_length(t, 2L)
  testthat::expect_s3_class(t[[1]], "tabxplor_tab")            # [[ -> bare tab
  testthat::expect_false(inherits(t[[1]], "tabxplor_tabs"))
  testthat::expect_s3_class(t[1], "tabxplor_tabs")             # [ keeps the class
  testthat::expect_length(purrr::map(t, nrow), 2L)            # map / lapply work
})


testthat::test_that("a single tab is returned bare (not wrapped in tabxplor_tabs)", {
  t <- tab(fx_gss(), marital, race, pct = "row")
  testthat::expect_false(inherits(t, "tabxplor_tabs"))
  testthat::expect_s3_class(t, "tabxplor_tab")
})


testthat::test_that("tab_kable(list) routes to the Viewer (kableExtra class) with joined tables", {
  t <- tab(fx_gss(), c(marital, relig), race, pct = "row", output_list = TRUE)
  k <- tab_kable(t)
  testthat::expect_s3_class(k, "kableExtra")                  # print.kableExtra -> Viewer
  testthat::expect_true(grepl("<table", as.character(k)))
})


testthat::test_that("print.tabxplor_tabs honours options(tabxplor.print)", {
  t <- tab(fx_gss(), c(marital, relig), race, pct = "row", output_list = TRUE)
  withr::local_options(tabxplor.print = "kable")
  out <- capture.output(print(t))
  testthat::expect_true(any(grepl("<table", out)))           # kable mode -> html tables
})



# === SECTION: stars ride the primary token ========================================================

gss <- fx_gss()


# a synthetic diff column with three significance levels stored in `pvalue`
star_col <- function() {
  fmt(n = rep(100L, 3), scale = "points", pct_type = "row", pct = c(0.4, 0.5, 0.6), diff = c(0.1, 0, -0.1),
      ci_inf = c(0.05, -0.10, -0.20), ci_sup = c(0.15, 0.10, -0.05),
      pvalue = c(0.0005, 0.5, 0.07), display = "pct")
}


testthat::test_that("stars are right-padded so numbers stay aligned (monospace)", {
  out <- format(star_col(), stars = TRUE)
  testthat::expect_equal(length(unique(nchar(out))), 1L)        # every value cell equal width
  # a cell with fewer stars is space-padded to the column-max star width
  testthat::expect_true(grepl("\\*  $", out[3]))                # one star + two pad spaces
})


testthat::test_that("secondary-field re-render (tooltip path) shows no stars by default", {
  x <- star_col()
  # tooltips re-render alternate fields via format(set_display(x, ...)) at the DEFAULT stars = FALSE
  testthat::expect_false(any(grepl("\\*", format(set_display(x, "n")))))
  testthat::expect_false(any(grepl("\\*", format(set_display(x, "diff")))))
})


testthat::test_that("stars = TRUE forces ci = 'diff' when ci is unset (Phase 16f)", {
  # The reported bug: `stars = TRUE` with no `ci` silently showed nothing, because no difference CI
  # (hence no pvalue) was computed. stars must now surface on its own, on both factor and mean columns.
  tf <- tab(gss, marital, race, pct = "row", stars = TRUE)             # factor, NO ci set
  testthat::expect_true(any(!is.na(get_pvalue(tf$White))))
  testthat::expect_true(any(grepl("\\*", format(tf$White, stars = TRUE))))

  tn <- tab(gss, marital, tvhours, stars = TRUE)                       # numeric mean, NO ci set
  ncol <- reg_fmt_cols(tn)[[1]]
  testthat::expect_true(any(!is.na(get_pvalue(tn[[ncol]]))))

  # byte-safety: the default (no stars) still stores no pvalue, so ordinary tables are unchanged
  t0 <- tab(gss, marital, race, pct = "row")
  testthat::expect_true(all(is.na(get_pvalue(t0$White))))
  # and it also folds with a {ci} display: the bracket AND the stars appear (stars opt-in in format())
  tb <- tab(gss, marital, race, pct = "row", display = "{ci}", stars = TRUE)
  out <- format(tb$White, stars = TRUE)
  testthat::expect_true(any(grepl("\\[", out)))                        # the [ci] bracket renders
  testthat::expect_true(any(grepl("\\*", out)))                        # and stars ride the cell
})


testthat::test_that("tab_reg() shows stars by default; stars = FALSE strips the pvalue", {
  d <- gss |> dplyr::mutate(m = factor(dplyr::if_else(marital == "Married", "Married", "No")))
  r1  <- tab_reg(d, "m", c("race", "rincome"), family = "binomial")
  orc <- reg_fmt_cols(r1)[[1]]
  testthat::expect_true(any(!is.na(get_pvalue(r1[[orc]]))))
  testthat::expect_true(any(grepl("\\*", format(r1[[orc]], stars = TRUE))))

  r0 <- tab_reg(d, "m", c("race", "rincome"), family = "binomial", stars = FALSE)
  testthat::expect_true(all(is.na(get_pvalue(r0[[orc]]))))
})


testthat::test_that("tab_kable main cells carry stars (opt-in) but tooltips do not leak them", {
  testthat::skip_if_not_installed("kableExtra")
  t1  <- tab(gss, marital, race, pct = "row", ci = "ref", stars = TRUE)
  html <- as.character(tab_kable(t1, tooltip = TRUE))
  testthat::expect_true(grepl("\\*", html))                    # main cells show stars
  # every data-toggle tooltip title is star-free (secondary fields do not leak stars)
  titles <- unlist(regmatches(html, gregexpr('title="[^"]*"', html)))
  testthat::expect_false(any(grepl("\\*", titles)))
})



# === SECTION: figure-space padding per medium =====================================================

fig <- "\u2007"   # FIGURE SPACE, one digit wide

sig <- "\u03c3"   # sigma

nbs <- "\u202f"   # Phase 14x: the OLD mean/sd joiner -- must be GONE


testthat::test_that("format(): a composite's mark and its padding are the SAME glyph", {
  # the reported bug: "100% (n=  849)" was padded with figure spaces while "(n=1 811)" separated
  # with an ASCII space -- so the digits the padding had just aligned fell out of line again.
  x <- fmt(n = c(849L, 3648L), pct = c(1, 1), scale = "level_pct", pct_type = "row", display = "{pct} (n={n})")
  h <- format(x, html = TRUE)
  # Phase g (A6): the html/nbsp medium joins the template literal " (n=" with a NON-BREAKING space so
  # the composite does not wrap; the inner digits keep the figure-space pad.
  nb <- intToUtf8(160L)
  testthat::expect_identical(h, c(paste0("100%", nb, "(n=", fig, fig, "849)"),
                                  paste0("100%", nb, "(n=3", fig, "648)")))
  testthat::expect_identical(length(unique(nchar(h))), 1L)   # one width -> aligned
  # markdown keeps ASCII on both counts
  testthat::expect_identical(format(x), c("100% (n=  849)", "100% (n=3 648)"))
})


# === mean (sigma sd): the sd-less cell is padded ==================================
# Phase 22c-iii: the sd tail is an ORDINARY COMPOSITE now (`{mean} (sigma{sd})`, the `mean_sd`
# preset) -- format() has no mean-specific branch left, and the generic per-token padding does the
# work. It aligns the MEANS too, which the hand-rolled tail never did.

mean_col <- function(digits = 1L) {
  fmt(mean = c(1.0, 1.7, 10.25), var = c(NA, 2.1^2, 3^2), n = rep(5L, 3),
      display = DISPLAY_PRESETS$mean_sd$template, scale = "level_mean", digits = digits)
}


testthat::test_that("format(): a mean with no sd is padded to the tail, so the means align", {
  f <- format(mean_col(), special_formatting = TRUE)
  testthat::expect_identical(f, c(paste0(" 1.0", strrep(" ", 7)),
                                  paste0(" 1.7 (", sig, "2.1)"),
                                  paste0("10.2 (", sig, "3.0)")))
  testthat::expect_false(any(grepl(nbs, f, fixed = TRUE)))
  # what alignment MEANS here: every cell is the same width AND the means occupy the same columns,
  # so the decimal points line up whether or not a cell has an sd.
  testthat::expect_identical(length(unique(nchar(f))), 1L)
  testthat::expect_identical(unique(substr(f, 5, 5)), " ")
})


testthat::test_that("format(): the sd-less pad follows `pad` (figure space in html)", {
  h <- format(mean_col(), special_formatting = TRUE, html = TRUE)
  testthat::expect_identical(h[1], paste0(fig, "1.0\u00a0", strrep(fig, 6)))
  testthat::expect_false(grepl(" ", h[1], fixed = TRUE))
  testthat::expect_identical(h[2], paste0(fig, "1.7\u00a0(", sig, "2.1)"))
  testthat::expect_false(any(grepl(nbs, h, fixed = TRUE)))
})


testthat::test_that("format(): an EMPTY mean cell stays NA -- it is not padded", {
  # REGRESSION: an empty cell also has an NA var, so the sd-less mask caught it and pasted onto the
  # NA -> the literal string "NA" + spaces. Only `na` (which kable/md pass as "") hid it; the
  # console, which keeps NA, printed "NA       ".
  x <- fmt(mean = c(1.0, NA, 2.5), var = c(NA, NA, 4), n = c(5L, 0L, 5L),
           display = DISPLAY_PRESETS$mean_sd$template, scale = "level_mean", digits = 1L)
  f <- format(x, special_formatting = TRUE)
  testthat::expect_true(is.na(f[2]))
  testthat::expect_false(any(grepl("NA", f[!is.na(f)], fixed = TRUE)))
  testthat::expect_identical(format(x, special_formatting = TRUE, na = "")[2], "")
})


testthat::test_that("format(): a mean column with no sd at all is untouched", {
  # the whole `(sigma{sd})` group is void down the column, so it leaves the template entirely
  x <- fmt(mean = c(1.0, 2.0), var = c(NA, NA), n = c(5L, 5L),
           display = DISPLAY_PRESETS$mean_sd$template, scale = "level_mean", digits = 1L)
  testthat::expect_identical(format(x, special_formatting = TRUE), c("1.0", "2.0"))
})


# === bold_split reaches the mean/sd cell ==========================================

testthat::test_that("format(bold_split): only the MEAN of a mean (sd) cell is the bold prefix", {
  b  <- format(mean_col(), special_formatting = TRUE, bold_split = TRUE)
  pn <- attr(b, "primary_nchar")
  testthat::expect_identical(pn, c(4L, 4L, 4L))
  # the prefix is exactly the mean -> the "(sigma sd)" tail stays plain in a bold row
  testthat::expect_identical(trimws(as.character(substr(b, 1, pn))), c("1.0", "1.7", "10.2"))
})


testthat::test_that("format(): primary_nchar is attached only when something splits", {
  # the contract: off by default -> attribute-free output; and no bare all-NA attribute either
  testthat::expect_null(attr(format(mean_col(), special_formatting = TRUE), "primary_nchar"))
  plain <- fmt(pct = c(0.4, 0.6), n = c(10L, 10L), scale = "level_pct", pct_type = "row")
  testthat::expect_null(attr(format(plain, bold_split = TRUE), "primary_nchar"))
})


testthat::test_that("tab_md(): a bold row bolds the mean, not the sd", {
  t <- tab(fx_gss(), marital, tvhours, pct = "row", color = FALSE, display = "mean_sd")
  md <- tab_md(t, color = FALSE, css = FALSE, color_legend = FALSE)
  # bold closes BEFORE the joiner: "**3.0**<figsp>(sigma2.6)", never "**3.0 (sigma2.6)**".
  # Phase 14x: the joiner is now the FIGURE space (markdown renders in a proportional host font).
  jn <- "\u00a0"   # the template literal's own space
  testthat::expect_match(md, paste0("\\*\\*[0-9.]+\\*\\*", jn, "\\(", sig), all = FALSE)
  testthat::expect_no_match(md, paste0("\\*\\*[0-9.]+", jn, "\\(", sig, "[0-9. ]+\\)\\*\\*"),
                            all = TRUE)
  testthat::expect_false(any(grepl(nbs, md, fixed = TRUE)))   # the narrow no-break space is gone
})


# === Phase 14m-ii: markdown value-internal padding is figure space ================

testthat::test_that("tab_md() pads a composite's (n=...) with figure space, not ASCII", {
  t  <- tab(fx_gss(), marital, race, pct = "row", display = "{pct} (n={n})")
  md <- tab_md(t, print = FALSE, color = FALSE, css = FALSE)
  # the (n=...) alignment inside a value is a figure space now (survives pandoc + the host font)
  testthat::expect_match(md, paste0("(n=", fig), fixed = TRUE)
  # but the raw layout is byte-for-byte the old one bar the pad glyph: normalise the figure spaces
  # back to ASCII and it equals what format()'s ASCII pad produces at the same widths (nchar-stable).
  testthat::expect_false(grepl(fig, gsub(fig, " ", md, fixed = TRUE), fixed = TRUE))
})


testthat::test_that("format()'s DEFAULT pad (the console) stays ASCII", {
  # the console must NOT move to figure space -- a monospace ASCII space is already one digit wide.
  x <- fmt(n = c(849L, 3648L), pct = c(1, 1), scale = "level_pct", pct_type = "row", display = "{pct} (n={n})")
  testthat::expect_identical(format(x), c("100% (n=  849)", "100% (n=3 648)"))
})


# === footer summary stats are not star-padded (Phase 14m-ii, L5) ==================

testthat::test_that("format(): a gof / pvalue footer cell reaches the column edge (no star pad)", {
  # in a starred column, a "gof" (N/AIC) or "pvalue" summary cell reserves NO star column, so a
  # right-aligned summary number reaches the edge instead of lining up under the starred data.
  x <- fmt(scale = "points", n = c(100L, 100L, NA, NA),
           pct    = c(0.4, 0.6, NA, 0.03),          # pvalue cell stores its p in pct
           diff   = c(0.1, -0.1, 21483, NA),        # gof cell stores its stat in diff
           ci_inf = c(0.05, -0.2, NA, NA), ci_sup = c(0.15, -0.05, NA, NA),
           pvalue = c(0.0005, 0.5, NA, NA),
           display = c("diff", "diff", "gof", "pvalue"), digits = c(0L, 0L, 0L, 2L))
  f <- format(x, special_formatting = TRUE, na = "", stars = TRUE, html = TRUE)
  # the diff data cell IS star-padded to width 3 (stars left, fig pad right)
  testthat::expect_true(endsWith(f[1], paste0("***")) || endsWith(f[2], strrep(fig, 3)))
  # the gof + pvalue cells carry NO trailing figure-space star pad
  testthat::expect_false(endsWith(f[3], fig))
  testthat::expect_false(endsWith(f[4], fig))
})


# === the Excel star pad ===========================================================

testthat::test_that("tab_xl(): the star literal is padded with figure spaces", {
  testthat::skip_if_not_installed("openxlsx2")
  x <- fmt(n = rep(100L, 3), scale = "points", pct_type = "row", pct = c(0.4, 0.5, 0.6), diff = c(0.1, 0, -0.1),
           ci_inf = c(0.05, -0.10, -0.20), ci_sup = c(0.15, 0.10, -0.05),
           pvalue = c(0.0005, 0.5, 0.07), display = "pct")
  st <- get_stars(x)
  testthat::expect_identical(st, c("***", "", "*"))
  # the width every cell's star field is padded to = the column max ("" counts 0)
  w  <- max(nchar(st))
  padded <- tx_pad(st, w, "right", pad = fig)
  testthat::expect_identical(nchar(padded), rep(w, 3L))
  testthat::expect_false(any(grepl(" ", padded, fixed = TRUE)))
  # and format()'s own star pad agrees, glyph for glyph
  testthat::expect_identical(format(x, html = TRUE, stars = TRUE),
                             paste0(format(x, html = TRUE), padded))
})



# === SECTION: the same grammar on a regression ====================================================

reg_data <- function() {
  fx_gss() |>
    dplyr::mutate(married = factor(dplyr::if_else(marital == "Married", "Married", "Not married")))
}

# Phase 18z13: skip the per-level `n` column (add_n = TRUE by default) -- see helper-reg.R.
first_fmt <- function(t) reg_first_fmt(t)


# ---- display = "est_ci" : visible confidence-interval bracket ---------------------------

test_that("display='est_ci' shows a visible CI bracket for OR and beta", {
  d  <- reg_data()
  oc <- first_fmt(tab_reg(d, "married", c("race", "age"), display = "est_ci"))
  txt <- format(oc, special_formatting = TRUE)
  expect_true(any(grepl("\\[.*;.*\\]", txt)))            # "<or> [<lo>;<hi>]"
  expect_equal(get_num(oc), get_or(oc))                  # primary value = the odds ratio

  bc <- first_fmt(tab_reg(d, "tvhours", c("race", "age"), family = "gaussian",
                          display = "est_ci"))
  expect_true(any(grepl("\\[.*;.*\\]", format(bc, special_formatting = TRUE))))
  # row 1 is the Constant, a baseline shown as a mean; everywhere else the beta is the primary
  expect_equal(get_num(bc)[-1], get_diff(bc)[-1])        # beta point estimate
})


test_that("est_ci bracket reads the stored asymmetric bounds, on the cell's own scale", {
  oc  <- first_fmt(tab_reg(reg_data(), "married", "age", display = "est_ci"))
  txt <- format(oc, special_formatting = TRUE)
  i   <- which(!is.na(get_ci_inf(oc)))[1]
  # a multiplicative bound below 1 prints as its inverse, exactly like the estimate it surrounds.
  lo  <- get_ci_inf(oc)[i]
  lo_s <- if (lo > 0 && lo < 1) paste0("1/", formatC(1 / lo, format = "f", digits = 2))
          else formatC(lo, format = "f", digits = 2)
  expect_match(txt[i], lo_s, fixed = TRUE)
})


test_that("options(tabxplor.ratio_print = 'raw') restores the plain bracket", {
  withr::local_options(tabxplor.ratio_print = "raw")
  oc  <- first_fmt(tab_reg(reg_data(), "married", "age", display = "est_ci"))
  txt <- format(oc, special_formatting = TRUE)
  expect_false(any(grepl("1/", txt, fixed = TRUE)))
})


# ---- the folds: the estimate beside the level it sits on -------------------------------

test_that("display='est_base' folds the adjusted probability into the OR cell", {
  skip_if_not_installed("marginaleffects")
  oc  <- first_fmt(tab_reg(reg_data(), "married", "race", display = "est_base"))
  txt <- format(oc)
  expect_true(any(grepl("\\([0-9]", txt)))               # "(16%)" prediction
  expect_equal(get_num(oc), get_or(oc))                  # OR is still the primary field
  expect_true(any(!is.na(get_pct(oc))))                  # the prediction is stored in `pct`
  # the composite KEEPS the inverse form -- it is the one rule, in every rendering path
  expect_true(any(grepl("1/", txt, fixed = TRUE)))
})


test_that("display='base_est' shows the adjusted probability, graded by the effect", {
  skip_if_not_installed("marginaleffects")
  oc  <- first_fmt(tab_reg(reg_data(), "married", "race", display = "base_est"))
  expect_true(any(grepl("%", format(oc), fixed = TRUE)))
  # on the templated rows the LEVEL is now the primary field (the Constant keeps its own token)
  tpl <- get_display(oc) == "{base} ({est})"
  expect_equal(get_num(oc)[tpl], get_pct(oc)[tpl])
})


test_that("a {diff} fold puts the marginal effect beside the odds ratio", {
  skip_if_not_installed("marginaleffects")
  oc  <- first_fmt(tab_reg(reg_data(), "married", "race", display = "{est} ({diff})"))
  expect_true(any(grepl("\\([-+][0-9]", format(oc))))    # "(-21%)" / "(+1%)" marginal effect
  expect_equal(get_num(oc), get_or(oc))
})


test_that("the folds reach EVERY family: a gaussian cell folds an adjusted MEAN", {
  skip_if_not_installed("marginaleffects")
  bc <- first_fmt(tab_reg(reg_data(), "tvhours", "race", family = "gaussian",
                          display = "est_base"))
  expect_true(any(grepl("\\([0-9]", format(bc))))        # "(2.37)" the adjusted mean
  expect_true(any(!is.na(get_mean(bc))))                 # written into `mean`, not `pct`
  expect_equal(get_num(bc)[-1], get_diff(bc)[-1])        # the coefficient stays primary
})


test_that("display reaches the marginal path too -- one grammar, both builders", {
  skip_if_not_installed("marginaleffects")
  t <- suppressMessages(
    tab_reg(reg_data(), "married", "race", family = "binomial", effect = "marginal", measure = "difference",
            display = "est_ci"))
  col <- t[[grep("^Model", names(t))[[1]]]]
  expect_true(any(get_display(col) == tabxplor:::DISPLAY_PRESETS[["est_ci"]]$template))
})


# ---- Excel in-cell test label ----------------------------------------------------------------

test_that("Excel names the test in the p-value row label, not the numFmt (Phase 18m)", {
  skip_if_not_installed("openxlsx2")
  d  <- reg_data()
  ct <- suppressWarnings(tab(d, race, marital, pct = "row", test = TRUE))
  f  <- tempfile(fileext = ".xlsx")
  tab_xl(ct, path = f, replace = TRUE)
  tmp <- tempfile(); dir.create(tmp); utils::unzip(f, exdir = tmp)
  # the test type moved OUT of the cell numFmt and INTO the row label (a text cell)
  sx  <- paste(readLines(file.path(tmp, "xl", "styles.xml"), warn = FALSE), collapse = "")
  expect_no_match(sx, "Chi2")                            # no longer folded into a numFmt literal
  xmls <- list.files(tmp, pattern = "\\.xml$", recursive = TRUE, full.names = TRUE)
  all_xml <- paste(unlist(lapply(xmls, readLines, warn = FALSE)), collapse = "")
  expect_match(all_xml, "pvalue \\(Chi2")               # the p-value row name (text) states the test
})


# ---- split_var export footer -----------------------------------------------------------------

test_that("split_var tables get a per-group export footer; plain tables one footer at the end", {
  d <- reg_data()
  # a models list keeps the STACKED per-group footer (the auto-spread side-by-side shape is tested
  # separately). The two models share ONE "Model fit" block per group, keyed on the group.
  t_split <- tab_reg(d, "married", list(m1 = "age", m2 = "age"), family = "binomial",
                     tab_vars = "race")
  # Phase g (A7): a styled md table's label cells use non-breaking spaces; normalise for text greps.
  md_s <- gsub(intToUtf8(160L), " ", tab_md(t_split, print = FALSE), fixed = TRUE)
  expect_true(grepl("Model fit", md_s))
  # one "Model fit" block per split group -> the footer labels repeat once per group
  n_groups <- nlevels(forcats::fct_drop(as.factor(d$race)))
  expect_equal(length(gregexpr("McFadden R2", md_s)[[1]]), n_groups)

  t_plain <- tab_reg(d, "married", "age")
  md_p <- gsub(intToUtf8(160L), " ", tab_md(t_plain, print = FALSE), fixed = TRUE)
  expect_true(grepl("Model fit", md_p))
  expect_equal(length(gregexpr("McFadden R2", md_p)[[1]]), 1L)  # a single block

  # split export renders through kable too
  expect_s3_class(suppressWarnings(tab_kable(t_split)), "kableExtra")
})


# ---- Phase 14r: tooltips + the AME NA bug ----------------------------------------------------

# Data with an ORDERED-factor income predictor whose levels contain " - " ($20000 - 24999, ...).
ame_data <- function() {
  fx_gss() |>
    dplyr::mutate(
      married = factor(dplyr::if_else(marital == "Married", "01-Married", "02-Not married")),
      rincome = forcats::fct_recode(rincome, NULL = "No answer", NULL = "Refused",
                                    NULL = "Don't know", NULL = "Not applicable") |>
        forcats::fct_relevel(sort) |> as.ordered()
    )
}


test_that("an ordered-factor predictor's AME is non-NA on every non-reference level (Item E)", {
  skip_if_not_installed("marginaleffects")
  d <- ame_data()
  suppressWarnings(t <- tab_reg(d, "married", c("race", "rincome"), family = "binomial",
                                effect = "marginal", measure = "difference", cleannames = FALSE))
  col  <- first_fmt(t)
  rin  <- as.character(t[[2]]) %in% levels(d$rincome)   # rincome level rows
  # the '-' levels ($20000 - 24999, $15000 - 19999, $10000 - 14999) used to be NA; only the reference is
  ame  <- get_diff(col)
  expect_false(any(is.na(ame[rin]) & !is_refrow(col)[rin]))
  # the AME tooltip carries the model OR too (Item E)
  tips <- tabxplor:::tab_tooltip_text(col)
  expect_true(any(grepl("OR: ", tips)))
})


test_that("an ordered-factor predictor's coefficient OR is non-NA (was all-NA)", {
  d <- ame_data()
  suppressWarnings(t <- tab_reg(d, "married", c("race", "rincome"), family = "binomial",
                                cleannames = FALSE))
  col <- first_fmt(t)
  rin <- as.character(t[[2]]) %in% levels(d$rincome)
  or  <- get_or(col)
  expect_true(all(!is.na(or[rin] | is_refrow(col)[rin])))     # every rincome level keyed
  expect_gt(sum(!is.na(or[rin])), 0)
})


test_that("model effect columns carry their level's n; footer cells have no tooltip (Items D/L6)", {
  d <- reg_data()
  t <- tab_reg(d, "married", c("race", "age"), family = "binomial")
  # each level's own base is on hover, and the Constant row carries the model N (Phase 22b-i)
  tips0 <- tabxplor:::tab_tooltip_text(first_fmt(t))
  expect_true(any(grepl("n: ", tips0)))
  # the GOF footer rows are materialised at display -> materialise, then check they carry no tooltip
  # (no nonsense "diff: +6378526%" on an AIC stored in the diff field)
  tm   <- tabxplor:::tab_materialize_extras(t, backend = "text", pvalue = TRUE)
  col  <- first_fmt(tm)
  tips <- tabxplor:::tab_tooltip_text(col)
  disp <- tabxplor:::display_primary(get_display(col))
  foot <- disp %in% c("gof", "blank")
  expect_true(any(foot))
  expect_true(all(!nzchar(tips[foot])))
})


test_that("empirical columns keep the per-LEVEL n in the tooltip (Item D)", {
  d <- reg_data()
  t <- tab_reg(d, "married", c("race", "rincome"), family = "binomial", empirical = TRUE)
  tips <- tabxplor:::tab_tooltip_text(t[["Obs_OR"]])
  expect_true(any(grepl("n: ", tips)))          # per-level counts survive
})


# ---- Phase 22b-iii: every geometry of one comparison, on both roles ----------------------

test_that("a regression cell carries both geometries of its own comparison", {
  d <- reg_data()
  # the pair is (level, reference level), so `diff` and `ratio` are filled on both roles and on
  # every measure -- only the column's OWN estimate field keeps what was fitted.
  for (meas in c("odds_ratio", "ratio", "difference")) {
    t <- suppressMessages(tab_reg(d, "married", "race", family = "binomial",
                                  measure = meas, empirical = TRUE))
    for (nm in c("Obs_OR", "Obs_RR", "Obs_RD", "Model_OR", "Model_RR", "Model_RD")) {
      if (!nm %in% names(t)) next
      col <- t[[nm]]
      i   <- which(!is_refrow(col) & as.character(t$var) == "race")
      expect_true(all(!is.na(get_diff(col)[i])),  label = paste(meas, nm, "diff"))
      expect_true(all(!is.na(get_ratio(col)[i])), label = paste(meas, nm, "ratio"))
    }
  }
})


test_that("the crude column's derived geometries ARE the observed ones", {
  d   <- reg_data()
  t   <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", empirical = TRUE))
  col <- t[["Obs_OR"]]
  # computed from the data, not from the grid: a parity check, not a tautology
  p   <- tapply(d$married == "Married", d$race, mean)
  p   <- stats::setNames(as.vector(p), names(p))          # tapply() returns a 1-d array
  ref <- levels(d$race)[1]
  lv  <- setdiff(as.character(t$levels)[as.character(t$var) == "race"], ref)
  i   <- match(lv, as.character(t$levels))
  expect_equal(get_diff(col)[i],  unname(p[lv] - p[[ref]]), tolerance = 1e-12)
  expect_equal(get_ratio(col)[i], unname(p[lv] / p[[ref]]), tolerance = 1e-12)
  expect_equal(get_diff(col)[match(ref, as.character(t$levels))], 0)   # the reference's neutral
  expect_equal(get_ratio(col)[match(ref, as.character(t$levels))], 1)
})


test_that("`base_est_mdiff` gives each ROLE its own arm, and never prints one field twice", {
  d <- reg_data()
  t <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", empirical = TRUE,
                                display = "base_est_mdiff"))
  expect_identical(unique(get_display(t[["Model_OR"]])[!is_refrow(t[["Model_OR"]])]),
                   "{est} ({diff})")
  expect_identical(unique(get_display(t[["Obs_OR"]])[!is_refrow(t[["Obs_OR"]])]),
                   "({base}) {est}")
  # on a risk-DIFFERENCE column `{est}` IS `{diff}`: the aside collapses instead of doubling
  rd <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", link = "difference",
                                 display = "base_est_mdiff"))
  # ... and what is left is the pipeline's own bare token, `{est}` resolved by the column's scale
  expect_identical(unique(get_display(rd[["Model_RD"]])[!is_refrow(rd[["Model_RD"]])]), "est")
})


test_that("the Model: footer names the aside the cell actually prints", {
  d <- reg_data()
  line <- function(...) tabxplor:::reg_model_lines(
    suppressMessages(tab_reg(d, "married", "race", family = "binomial", empirical = TRUE, ...)))
  # Phase 22f-i: the aside is glossed under the abbreviation the table itself prints, per ROLE --
  # so the crude column's own share is named too, and the model's is no longer claimed for both.
  expect_match(line(display = "est_base"),        "adj%: adjusted/predicted proportion", fixed = TRUE)
  expect_match(line(display = "est_base"),        "obs%: observed proportion", fixed = TRUE)
  expect_match(line(display = "base_est_mdiff"),  "as a difference")
  expect_match(line(display = "base_est_mratio"), "as a ratio")
  expect_false(grepl("adjusted predicted", line(display = "est")))
  # a gaussian outcome answers `{base}` with a mean, so the same clause words itself
  expect_match(tabxplor:::reg_model_lines(
    suppressMessages(tab_reg(d, "tvhours", "race", family = "gaussian", empirical = TRUE))),
    "adj mean: adjusted/predicted mean", fixed = TRUE)
})


test_that("the outcome is named once above the table, and only where there is one", {
  d <- reg_data()
  one <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", stats = FALSE))
  expect_identical(unname(pillar::tbl_sum(one)[["Outcome"]]), "married")
  # ⚠ the grouped method calls NextMethod(), so the line must not be appended twice
  expect_equal(sum(names(pillar::tbl_sum(one)) == "Outcome"), 1L)
  # several outcomes: no line -- each column already carries its own "[outcome]" suffix
  two <- suppressMessages(tab_reg(d, c("married", "tvhours"), "race", stats = FALSE))
  expect_false("Outcome" %in% names(pillar::tbl_sum(two)))
  expect_true(any(grepl("[married]", names(two), fixed = TRUE)))
  # a crosstab is untouched
  expect_false("Outcome" %in% names(pillar::tbl_sum(tab(d, race, married))))
})


# ---- Phase 22g-v: `digits` -- a floor on the cell, a suffix on one token ------------------------

test_that("digits sets every cell, and a named field sets just that one", {
  d  <- reg_data()
  f  <- function(...) format(first_fmt(
    tab_reg(d, "married", "race", stats = "no", empirical = FALSE, ...)))[-1]
  # unset, an odds ratio takes the `or` token's own default of two decimals...
  expect_true(all(grepl("\\.[0-9]{2}$|^ *1 *$", trimws(f()))))
  # ...which a request overrides in EITHER direction, because that default is a default
  expect_true(any(grepl("\\.[0-9]{4}", f(digits = 4))))
  expect_true(all(grepl("\\.[0-9]$|^ *1 *$", trimws(f(digits = 1)))))
  # a SCALE's own statement (EST_SCALES$est_digits) is a floor instead: a per-item odds ratio reads
  # at two decimals beside a mean score at one, and a coarser request cannot flatten the pair
  sc <- format(first_fmt(tab_reg(d, "tvhours", "race", family = "binomial", trials = 24,
                                 stats = "no", digits = 1)))
  expect_true(any(grepl("\\.[0-9]{2} \\([0-9]+\\.[0-9]\\)", sc)))
  # a name targets one display field -- including an ASIDE, which no scalar can reach
  g <- format(first_fmt(tab_reg(d, "married", "race", stats = "no",
                                display = "est_base", digits = c(base = 3))))
  expect_true(any(grepl("\\([0-9]+\\.[0-9]{3}%\\)", g)))
  expect_error(tab_reg(d, "married", "race", digits = c(nope = 2)), "no cell can print")
  expect_error(tab_reg(d, "married", "race", digits = 9), "between 0 and 6")
})


# Phase 22h: comparing predictor subsets, the level is stated ONCE -- by the single observed column
# beside them -- so the model columns sit side by side with nothing between the numbers being
# compared.
test_that("several predictor subsets: the model columns drop the level, the observed keeps it", {
  d <- reg_data()
  t <- suppressMessages(tab_reg(d, "married", list(a = "race", b = c("race", "rincome")),
                                stats = "no"))
  mods <- reg_model_cols(t)
  expect_gt(length(mods), 1L)
  for (m in mods) expect_identical(unique(get_display(t[[m]])), "est")
  emp <- names(t)[purrr::map_lgl(t, ~ is_fmt(.) && identical(as.character(get_role(.))[1], "emp"))]
  expect_true(any(grepl("{base}", get_display(t[[emp[[1]]]]), fixed = TRUE)))
  # the footer's `Model:` gloss follows the cells: it names no aside the model columns never print
  expect_false(grepl("adjusted/predicted", paste(get_subtext(t), collapse = " "), fixed = TRUE))
  # ONE subset is not a comparison: the ordinary layout stands
  t1 <- suppressMessages(tab_reg(d, "married", "race", stats = "no"))
  expect_true(any(grepl("{base}", get_display(t1[[reg_model_cols(t1)[[1]]]]), fixed = TRUE)))
})



testthat::test_that("display_primary() never errors on malformed templates", {
  # unbalanced / empty braces: best-effort, no crash (fall through to get_num()'s default `n`)
  testthat::expect_no_error(out <- display_primary(c("{pct", "{}", "a {b", "pct}")))
  testthat::expect_length(out, 4L)
})



testthat::test_that("parse_display_template() yields no field tokens for a degenerate template", {
  testthat::expect_length(parse_display_template("abc")$fields, 0L)   # literal only
  testthat::expect_length(parse_display_template("{pct")$fields, 0L)  # malformed -> literal
  testthat::expect_false(any(parse_display_template("plain text")$is_tok))
})



testthat::test_that("validate_display_template() rejects non-template input (no curated sugar)", {
  # Composites use the {} grammar only -- the old recipe strings are no longer accepted.
  testthat::expect_error(validate_display_template("pct (n)"), "[Cc]omposite|template")
  testthat::expect_error(validate_display_template("n (pct)"), "[Cc]omposite|template")
  testthat::expect_error(validate_display_template("pct_n"),   "[Cc]omposite|template")
  testthat::expect_error(validate_display_template("wibble"),  "[Cc]omposite|template")
})



testthat::test_that("a void aside renders blank AND keeps its width, so the column stays aligned", {
  x <- fmt(n = c(10L, NA_integer_), scale = "level_pct", pct_type = "row", pct = c(0.4, 0.6), display = "pct")
  out <- format(set_display(x, "{pct} ({n})"))
  testthat::expect_identical(out[1], "40% (10)")
  testthat::expect_identical(out[2], "60%     ")   # no "(NA)", and the same width as its neighbour
  testthat::expect_identical(nchar(out[1]), nchar(out[2]))
  testthat::expect_false(grepl("(", out[2], fixed = TRUE))
})



testthat::test_that("an aside void in the WHOLE column is dropped, padding included", {
  # the raw-template path (set_display writes it verbatim) prunes in format() ...
  x <- fmt(n = c(NA_integer_, NA_integer_), scale = "level_pct", pct_type = "row",
           pct = c(0.4, 0.6), display = "pct")
  testthat::expect_identical(format(set_display(x, "{pct} ({n})")), c("40%", "60%"))
  # ... and the preset path prunes the TEMPLATE itself, collapsing to the bare token
  r <- tabxplor:::display_write_col(x, "{pct} ({n})")
  testthat::expect_identical(unique(get_display(r$col)), "pct")
  testthat::expect_identical(r$missing, "n")
})



testthat::test_that("the primary's own bracket group is never dropped", {
  # "({n_range})" -- the base-count fold's template, whose only token is bracketed
  x <- fmt(n = c(10L, 20L), tot_n = c(NA_real_, NA_real_), scale = "level_n", display = "n_range")
  seg <- tabxplor:::parse_display_template("({n_range})")
  testthat::expect_true(all(tabxplor:::display_template_keep(seg, TRUE)))
  testthat::expect_identical(format(set_display(x, "({n_range})")), c("(10)", "(20)"))
})



testthat::test_that("a top-level literal renders even when the primary token is void", {
  # the base-count cell of a regression's numeric-predictor row: no count, but the row sparkline
  x <- fmt(n = c(100L, NA_integer_), tot_n = c(NA_real_, NA_real_), scale = "level_n",
           display = "n_range", digits = 0L)
  d <- set_display(x, c("n_range", "{n_range}\u2581\u2586\u2588"))
  testthat::expect_identical(format(d, na = ""), c("100", "\u2581\u2586\u2588"))
  testthat::expect_identical(format(d), c("100", "\u2581\u2586\u2588"))     # na = NA (the console)
  # ... and the cell is ONE plain piece: no primary range, so no aside span / partial colouring
  f  <- format(d, bold_split = TRUE)
  pn <- attr(f, "primary_nchar")
  testthat::expect_true(is.null(pn) || is.na(pn[[2]]))
  # the pillar type tag still reads the TOKEN, never the literal
  testthat::expect_identical(vctrs::vec_ptype_abbr(d), vctrs::vec_ptype_abbr(x))
  # a whitespace-only top-level literal is NOT content: a void primary still blanks the cell
  testthat::expect_identical(format(set_display(x, "{n_range} ({n})"), na = "")[[2]], "")
})



# === the ONE multiplicative rendering ===============================================

testthat::test_that("a multiplicative cell prints its inverse below the neutral, in EVERY path", {
  gss <- fx_gss()
  t <- tab(gss, race, marital, pct = "row", color = "OR", ci = "ref", display = "{or}")
  co <- t[[which(purrr::map_lgl(t, is_fmt))[3]]]
  testthat::expect_true(any(grepl("1/", format(co, special_formatting = TRUE), fixed = TRUE)))
  # the COMPOSITE keeps it -- the defect the one rule exists to close
  testthat::expect_true(any(grepl("1/", format(set_display(co, "{or} ({pct})")), fixed = TRUE)))
  # so does the est_ci bracket, bounds included, and the bounds are NOT reordered
  ec <- set_display(co, "est_ci")
  txt <- trimws(format(ec, special_formatting = TRUE), whitespace = "[\\h\\v]")
  i   <- which(!is.na(get_ci_inf(ec)) & get_or(ec) < 1)[1]
  testthat::skip_if(is.na(i))
  testthat::expect_match(txt[i], "^1/[0-9.]+ +\\[1/[0-9.]+;", perl = TRUE)
  # the option restores the journal convention everywhere at once, cell and ladder alike
  withr::local_options(tabxplor.ratio_print = "raw")
  testthat::expect_false(any(grepl("1/", format(co, special_formatting = TRUE), fixed = TRUE)))
  testthat::expect_false(any(grepl("1/", tab_color_legend(t, medium = "plain", lang = "en"),
                                   fixed = TRUE)))
})



testthat::test_that("only the primary token is coloured, and the option says what the rest gets", {
  gss <- fx_gss()
  t  <- tab(gss, marital, race, pct = "row", color = "diff")
  co <- t[["White"]]
  paint <- function(...) {
    withr::local_options(...)
    withr::local_options(cli.num_colors = 256)
    as.character(format(pillar::pillar_shaft(t[["White"]]), width = 30))
  }
  d <- paint(tabxplor.color_whole_cell = FALSE)
  s <- paint(tabxplor.color_whole_cell = TRUE)
  # a simple cell has no aside, so both paint it identically
  testthat::expect_identical(d, s)
  # a composite one does: the aside is set back by default, and inherits the cell's own colour
  # under the expert whole-cell opt-out
  co2 <- set_display(co, "{pct} (n={n})")
  hit <- function(opt) {
    withr::local_options(tabxplor.color_whole_cell = opt, cli.num_colors = 256)
    as.character(format(pillar::pillar_shaft(co2), width = 30))
  }
  testthat::expect_false(identical(hit(FALSE), hit(TRUE)))
})



testthat::test_that("{ci} and {moe} are one interval in two notations, both column-driven", {
  gss <- fx_gss()
  # ONE preset on a MIXED table: the "%" and the x100 come from the COLUMN's declared scale, never
  # from the template, so `base_moe` reads right on a percentage column and a mean column alike.
  t <- tab(gss, race, c(marital, age), pct = "row", ci = "cell", display = "base_moe")
  testthat::expect_match(format(t$Married)[1], "^ *[0-9]+% *\u00b1[0-9]+%$")
  testthat::expect_match(format(t$age)[1],     "^ *[0-9.]+ *\u00b1[0-9.]+$")
  # the same two cells in the bracket notation
  b <- tab(gss, race, c(marital, age), pct = "row", ci = "cell", display = "base_ci")
  testthat::expect_match(format(b$Married)[1], "\\[[0-9]+;[0-9]+\\]%$")
  testthat::expect_match(format(b$age)[1],     "\\[[0-9.]+;[0-9.]+\\]$")
  # `ci = "cell"` writes the ordinary `{ci}` token -- no pipeline-only composite left in the grammar
  testthat::expect_identical(unique(get_display(tab(gss, race, marital, pct = "row",
                                                    ci = "cell")[[2]])), "ci")
  # a RATIO has no half-width -- `{moe}` is declared void there, `{ci}` still renders
  co <- tab(gss, race, marital, pct = "row", color = "OR", ci = "ref")[[3]]
  testthat::expect_true(all(is.na(get_num(set_display(co, "moe")))))
  testthat::expect_true(any(!is.na(get_num(set_display(co, "ci")))))
  # Excel: `{moe}` IS the +/- number format, the bracket form exports as the rendered string
  testthat::expect_true(all(format(set_display(b$Married, "moe"), syntax = "excel") != "TEXT"))
  testthat::expect_true(all(format(set_display(b$Married, "ci"),  syntax = "excel") == "TEXT"))
})



# === Phase 22c-ii: naming what a column holds ======================================================

testthat::test_that("a column's NAME is its own template, token by token", {
  g <- fx_reg_fmt()
  tag <- function(...) {
    t <- suppressMessages(tab(g, race, party3, pct = "row", na = "drop_all", color = TRUE, ...))
    c(vctrs::vec_ptype_abbr(t[[2]]), vctrs::vec_ptype_abbr(t$Total))
  }
  # the aside is NAMED, in the bracket form the cell itself uses
  testthat::expect_identical(tag(display = "pct"),           c("row%", "row%"))
  testthat::expect_identical(tag(display = "{pct} ({n})"),   c("row% (n)", "row% (n)"))
  # ... and it drops where the field is void: the row-% Total has no odds ratio (degenerate 2x2)
  testthat::expect_identical(tag(display = "{pct} ({or})"),  c("row% (OR)", "row%"))
  # a percentage names its own direction; the prefix is only for a column that prints NO level
  testthat::expect_identical(tag(display = "{or} ({pct})")[[1]], "OR (row%)")
  testthat::expect_identical(tag(display = "or")[[1]],          "row%-OR")
  testthat::expect_identical(tag(display = "ratio")[[1]],       "row%-ratio")
  # a numeric column names its aside: the coefficient of variation by default, the sd on request
  testthat::expect_identical(
    vctrs::vec_ptype_abbr(tab(g, race, tvhours, na = "drop_all", color = TRUE)[[2]]),
    "mean (cv)")
  testthat::expect_identical(
    vctrs::vec_ptype_abbr(tab(g, race, tvhours, na = "drop_all", color = TRUE,
                              display = "mean_sd")[[2]]),
    "mean (sd)")
  # a REGRESSION column never takes the pct-type prefix: its estimate is not a percentage. What its
  # LEVEL says instead is WHOSE it is -- the observed one, or the model's adjusted prediction.
  m <- tab_reg(g, outcome = "married", predictors = "race", empirical = TRUE, family = "binomial")
  testthat::expect_identical(vctrs::vec_ptype_abbr(m$Obs_OR),   "(obs%) OR")
  testthat::expect_identical(vctrs::vec_ptype_abbr(m$Model_OR), "OR (adj%)")
  # ... on a mean as much as on a percentage, and the fallback is the crosstab reading. The tag says
  # `diff`, the same word the COLUMN NAME does: an identity-link beta is a mean difference, and only
  # a logged scale earns the word "coefficient" (next block).
  gm <- suppressMessages(tab_reg(g, outcome = "tvhours", predictors = "race", empirical = TRUE))
  testthat::expect_identical(vctrs::vec_ptype_abbr(gm$Obs_diff),   "(obs mean) diff")
  testthat::expect_identical(vctrs::vec_ptype_abbr(gm$Model_diff), "diff (adj mean)")
})




# === Phase 22g-vii: set_display() goes through THE display boundary ================================

testthat::test_that("set_display() takes a measure's own name, and refuses an unknown word", {
  g <- fx_reg_fmt()
  t <- suppressMessages(tab(g, race, party3, pct = "row", ref = "first", na = "drop_all"))
  col <- t[[2]]
  # a colour MEASURE's name reaches the token that renders it: one spelling, one quantity, whichever
  # argument it is typed in (`display = "difference"` used to write the COUNT, silently)
  testthat::expect_identical(get_display(set_display(col, "difference"))[[1]], "diff")
  testthat::expect_identical(get_display(set_display(col, "odds_ratio"))[[1]], "or")
  testthat::expect_identical(get_display(set_display(col, "ratio"))[[1]], "ratio")
  testthat::expect_identical(format(set_display(col, "difference")),
                             format(set_display(col, "diff")))
  # an alias is stored canonically, so a column carries one spelling of its token
  testthat::expect_identical(get_display(set_display(col, "OR"))[[1]], "or")
  # ... and a word that names nothing aborts instead of rendering some other field
  testthat::expect_error(set_display(col, "od_ratio"), "display")
  testthat::expect_error(set_display(col, "{nonsense}"), "Unknown field")
  # a {} template still passes through, validated
  testthat::expect_identical(get_display(set_display(col, "{pct} (n={n})"))[[1]], "{pct} (n={n})")
})




# === SECTION: the display-only n / add_pct / p-value rows =========================================

withr::local_options(lifecycle_verbosity = "quiet", .local_envir = testthat::teardown_env())




gss <- fx_gss()



# --- text backend folds the base count IN-CELL on the Total column (no separate `n` column) -------------
testthat::test_that("tab_materialize_extras('text') folds the base count into the Total cell", {
  t   <- tab(gss, marital, race, pct = "row", n = "range")
  hyd <- tabxplor:::tab_materialize_extras(t, backend = "text", pvalue = FALSE)
  testthat::expect_false("n" %in% names(hyd))
  # the Total cell shows the {pct} ({n_range}) composite
  testthat::expect_match(format(hyd$Total)[1], "100% *\\(", perl = TRUE)
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
  g <- fx_gss()
  g$marital[1:800] <- NA
  g$race[1:40]     <- NA
  g
}




# ---- Phase 14n: one Total row for several row_vars (display-only collapse) --------------------------

# materialise for a backend, count the visible Total rows
n_totrows <- function(tt, backend = "text") {
  m <- suppressMessages(tabxplor:::tab_materialize_extras(tt, backend = backend, pvalue = FALSE))
  sum(tabxplor:::is_totrow(m))
}




# ---- Phase 17c: honest p-value cells -- stored in the `pvalue` field, coloured by an explicit rule ----
testthat::test_that("p-value cell stores p in the pvalue field, not pct/var/diff, and shows no star", {
  m   <- tabxplor:::tab_materialize_extras(
    tab(fx_gss(), marital, race, pct = "row", color = "diff", test = TRUE),
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




# === SECTION: composite width, ratio glyphs, spanning headers =====================================

mult_glyph <- intToUtf8(0x00d7)  # multiply sign


div_glyph  <- intToUtf8(0x00f7)  # divide sign



testthat::test_that("a REFERENCE cell at the neutral prints a bare 1, a cell that merely equals it does not", {
  t <- tab(fx_gss(), race, marital, pct = "row", color = "ratio", display = "ratio")
  col <- t[["Divorced"]]
  f   <- format(col, special_formatting = TRUE)
  # the Total row IS the reference: no glyph, no decimals, so its row stands out
  testthat::expect_identical(f[is_totrow(col)], "1")
  # a non-reference cell rounding to the neutral keeps the glyph and the decimals
  testthat::expect_true(any(f[!is_totrow(col)] == paste0(mult_glyph, "1.00")))
  # and a regression Constant IS a reference row, but its odds ratio is a real baseline value
  reg <- suppressMessages(tab_reg(fx_reg_fmt(), "married", "race",
                                  family = "binomial"))
  cst <- format(reg[["Model_OR"]], special_formatting = TRUE)[as.character(reg$var) == "Constant"]
  testthat::expect_false(cst == "1")
})




# === SECTION: stars ride the primary token ========================================================

gss <- fx_gss()



# a synthetic diff column with three significance levels stored in `pvalue`
star_col <- function() {
  fmt(n = rep(100L, 3), scale = "points", pct_type = "row", pct = c(0.4, 0.5, 0.6), diff = c(0.1, 0, -0.1),
      ci_inf = c(0.05, -0.10, -0.20), ci_sup = c(0.15, 0.10, -0.05),
      pvalue = c(0.0005, 0.5, 0.07), display = "pct")
}



testthat::test_that("format(): stars are opt-in (default off), appended only with stars = TRUE", {
  x <- star_col()
  testthat::expect_false(any(grepl("\\*", format(x))))          # default: none
  out <- format(x, stars = TRUE)
  testthat::expect_equal(lengths(regmatches(out, gregexpr("\\*", out, perl = TRUE))), c(3L, 0L, 1L))  # ***, none, *
})



testthat::test_that("tab() stores no pvalue and shows no stars by default; stars = TRUE does", {
  t0 <- tab(gss, marital, race, pct = "row", ci = "ref")
  testthat::expect_true(all(is.na(get_pvalue(t0$White))))
  testthat::expect_false(any(grepl("\\*", format(t0$White, stars = TRUE))))  # no pvalue -> none

  t1 <- tab(gss, marital, race, pct = "row", ci = "ref", stars = TRUE)
  testthat::expect_true(any(!is.na(get_pvalue(t1$White))))
  testthat::expect_true(any(grepl("\\*", format(t1$White, stars = TRUE))))
})




# === SECTION: figure-space padding per medium =====================================================

fig <- "\u2007"   # FIGURE SPACE, one digit wide


sig <- "\u03c3"   # sigma


nbs <- "\u202f"   # Phase 14x: the OLD mean/sd joiner -- must be GONE


                                                   # now (replaced by `pad`: ASCII in console, fig in md/html)

# === the thousands mark follows `pad` =============================================

testthat::test_that("format(): the thousands mark is the pad glyph, per medium", {
  x <- fmt(n = c(849L, 3648L, 1811L), display = "n")

  # console / markdown: ASCII, as before
  con <- format(x)
  testthat::expect_identical(con, c("849", "3 648", "1 811"))
  testthat::expect_false(any(grepl(fig, con, fixed = TRUE)))

  # html: the figure space, and NO ascii space left to be collapsed by CSS
  h <- format(x, html = TRUE)
  testthat::expect_identical(h, c("849", paste0("3", fig, "648"), paste0("1", fig, "811")))
  testthat::expect_false(any(grepl(" ", h, fixed = TRUE)))

  # an explicit pad wins over both (the lever tab_xl() uses)
  testthat::expect_identical(format(x, pad = fig), h)
})



# === mean (sigma sd): the sd-less cell is padded ==================================
# Phase 22c-iii: the sd tail is an ORDINARY COMPOSITE now (`{mean} (sigma{sd})`, the `mean_sd`
# preset) -- format() has no mean-specific branch left, and the generic per-token padding does the
# work. It aligns the MEANS too, which the hand-rolled tail never did.

mean_col <- function(digits = 1L) {
  fmt(mean = c(1.0, 1.7, 10.25), var = c(NA, 2.1^2, 3^2), n = rep(5L, 3),
      display = DISPLAY_PRESETS$mean_sd$template, scale = "level_mean", digits = digits)
}




# === SECTION: the same grammar on a regression ====================================================

reg_data <- function() {
  fx_gss() |>
    dplyr::mutate(married = factor(dplyr::if_else(marital == "Married", "Married", "Not married")))
}


# Phase 18z13: skip the per-level `n` column (add_n = TRUE by default) -- see helper-reg.R.
first_fmt <- function(t) reg_first_fmt(t)



# ---- Phase 14r: tooltips + the AME NA bug ----------------------------------------------------

# Data with an ORDERED-factor income predictor whose levels contain " - " ($20000 - 24999, ...).
ame_data <- function() {
  fx_gss() |>
    dplyr::mutate(
      married = factor(dplyr::if_else(marital == "Married", "01-Married", "02-Not married")),
      rincome = forcats::fct_recode(rincome, NULL = "No answer", NULL = "Refused",
                                    NULL = "Don't know", NULL = "Not applicable") |>
        forcats::fct_relevel(sort) |> as.ordered()
    )
}



# ---- Phase 22b-xviii: the LEVEL has its own precision, and the outcome is named once -------------

test_that("a `{base}` aside prints its own decimals, never the estimate's", {
  d <- reg_data()
  # `measure = "difference"` puts the column on the `points` scale, whose ESTIMATE wants a decimal
  # (a 2.4-point risk difference is not a 2-point one) while its LEVEL is an ordinary percentage.
  rd <- suppressMessages(tab_reg(d, "married", c("race", "age"), family = "binomial",
                                 measure = "difference", empirical = TRUE, stats = FALSE))
  txt <- format(rd[["Model_mRD"]], na = "")
  expect_true(any(grepl("[0-9]\\.[0-9]%", txt)))          # the estimate keeps its decimal
  expect_false(any(grepl("\\([0-9]+\\.[0-9]+%\\)", txt))) # the base does not
  expect_true(any(grepl("\\([0-9]+%\\)", txt)))
  # and the observed twin, whose base is the same percentage, agrees with it
  expect_false(any(grepl("\\([0-9]+\\.[0-9]+%\\)", format(rd[["Obs_RD"]], na = ""))))
  # a scale that declares no base_digits is untouched: an odds-ratio column already printed 0
  or <- suppressMessages(tab_reg(d, "married", "race", family = "binomial", stats = FALSE))
  expect_false(any(grepl("\\([0-9]+\\.[0-9]+%\\)", format(or[["Model_OR"]], na = ""))))
})




testthat::test_that("display_primary() returns the FIRST {field}, alias-resolved", {
  testthat::expect_identical(display_primary("{pct} (n={n})"), "pct")
  testthat::expect_identical(display_primary("{n} ({pct})"),   "n")
  testthat::expect_identical(display_primary("{ratio} x"),     "ratio") # Phase 17d: ratio is canonical
  testthat::expect_identical(display_primary("{rr} x"),        "ratio") # rr is the read-side legacy alias
  testthat::expect_identical(display_primary("{ pct }"),       "pct")  # trims inside braces
  testthat::expect_identical(display_primary("{diff} [{ci}]"), "diff")
  # a mixed vector: composites resolve, simple/NA pass through, position preserved
  testthat::expect_identical(
    display_primary(c("{pct} ({n})", "pvalue", NA, "diff", "{mean} (sd)")),
    c("pct", "pvalue", NA, "diff", "mean")
  )
})




# === validate_display_template(): write-time validation ({}-only, no curated sugar) ===

testthat::test_that("validate_display_template() passes a valid {} template through", {
  testthat::expect_identical(validate_display_template("{pct} (n={n})"), "{pct} (n={n})")
  testthat::expect_identical(validate_display_template("{n} ({pct})"),   "{n} ({pct})")
  testthat::expect_identical(validate_display_template("{diff} [{ci}]"), "{diff} [{ci}]")
  testthat::expect_identical(validate_display_template("{ratio}"), "{ratio}")   # alias field ok
})




testthat::test_that("validate_display_template() wraps a bare known field (display = 'ci' == '{ci}')", {
  # Phase 16f ergonomics: a bare field name (no braces) is treated as the single-field template, so the
  # historical display = "ci" (and "diff"/"pct"/...) keeps working, mapping to display = "{ci}".
  testthat::expect_identical(validate_display_template("ci"),    "{ci}")
  testthat::expect_identical(validate_display_template("diff"),  "{diff}")
  testthat::expect_identical(validate_display_template("pct"),   "{pct}")
  testthat::expect_identical(validate_display_template("ratio"), "{ratio}")   # alias name wraps too
})




testthat::test_that("no template the package writes has TOP-LEVEL literal content", {
  # THE guard on the "a literal is content" rule: a void primary blanks a cell UNLESS the template
  # says something outside its tokens. Every template the package itself writes separates its tokens
  # with whitespace only -- "(Chi2)" and "(n=" sit inside a bracket GROUP, not at the top level -- so
  # the rule cannot reach an existing column. A new template with a bare literal must be deliberate.
  rfiles <- list.files(src_path("R"), "\\.R$", full.names = TRUE)
  src <- unlist(lapply(rfiles, readLines, warn = FALSE))
  src <- src[!grepl("^\\s*#", src)]                    # a comment may QUOTE a template it does not write
  lit <- gsub('^"|"$', "", unlist(regmatches(src, gregexpr('"[^"\\\\]*\\{[a-z_]+\\}[^"\\\\]*"', src))))
  cand <- unique(c(unlist(lapply(DISPLAY_PRESETS, function(r) r$template), use.names = FALSE), lit,
                   "{pvalue} (Chi2)", "{pvalue} (F, Welch)", "{pvalue} (Rao-Scott Chi2)"))
  cand <- cand[!is.na(cand)]
  keep <- vapply(cand, function(t) {
    # cli inline markup is never a display template, and `{.val {var}}` would otherwise read as one
    if (nchar(t) > 40 || grepl("[.](code|val|arg|field|fn|or)|cli::|\\\\", t)) return(FALSE)
    f <- trimws(gsub("[{}]", "", regmatches(t, gregexpr("\\{[^{}]+\\}", t))[[1]]))
    length(f) > 0 && all(f %in% names(DISPLAY_TOKENS))
  }, logical(1))
  cand <- unique(cand[keep])
  testthat::expect_gt(length(cand), 15L)                       # the scan really found them
  bare <- vapply(cand, function(t) {
    seg <- parse_display_template(t)
    any(fmt_rendered(seg$pieces[!seg$is_tok & seg$group == 0L]))
  }, logical(1))
  testthat::expect_identical(cand[bare], character(0))
})




testthat::test_that("set_display(x, 'base_ci') == tab(display = 'base_ci') (same overlay, post-hoc)", {
  gss <- fx_gss()
  built <- gss |>
    tab(race, marital, pct = "row", color = TRUE, color_signif = "guaranteed_effect")
  post  <- set_display(built, "base_ci")
  live  <- gss |>
    tab(race, marital, pct = "row", color = TRUE, color_signif = "guaranteed_effect",
        display = "base_ci")
  testthat::expect_identical(format(post), format(live))
  # single fmt column resolves too
  col <- built[[which(purrr::map_lgl(built, is_fmt))[2]]]
  testthat::expect_gt(sum(grepl("\\[.*;.*\\]", format(set_display(col, "base_ci")))), 0L)
})




# === the scale-relative tokens and the shared preset table ==========================

testthat::test_that("{est} / {base} resolve to the token each COLUMN renders them as", {
  gss <- fx_gss()
  # a plain percentage column estimates a percentage; asked for a difference CI it estimates the
  # DIFFERENCE, and `{est}` follows -- that is what "whatever this column estimates" means.
  lvl <- tab(gss, race, marital, pct = "row")
  eff <- tab(gss, race, marital, pct = "row", ci = "ref")
  lv  <- lvl[[which(purrr::map_lgl(lvl, is_fmt))[1]]]
  ef  <- eff[[which(purrr::map_lgl(eff, is_fmt))[1]]]
  testthat::expect_identical(get_num(set_display(lv, "est")),  get_pct(lv))
  testthat::expect_identical(get_num(set_display(ef, "est")),  get_diff(ef))
  testthat::expect_identical(get_num(set_display(ef, "base")), get_pct(ef))
  # a numeric column answers `{base}` with its mean
  num <- tab(gss, race, age)
  nc  <- num[[which(purrr::map_lgl(num, is_fmt))[1]]]
  testthat::expect_identical(get_num(set_display(nc, "base")), get_mean(nc))
})




testthat::test_that("stars, get_num() and Excel follow the primary, not the word order", {
  x <- fmt(pct = c(.5, .3), n = c(10L, 20L), diff = c(0, -.2), pvalue = c(NA, 0.001),
           scale = "level_pct", pct_type = "row", digits = 0L)
  a <- set_display(x, "{diff} ({pct})")
  b <- set_display(x, "({pct}) {diff}")
  testthat::expect_equal(get_num(a), get_num(b))                    # the same primary field
  testthat::expect_equal(get_num(b), get_diff(x))
  # the stars ride the DIFFERENCE in both, though `b` prints the percentage first: they land at the
  # end of the cell there, not after the percentage
  txt <- format(b, stars = TRUE, special_formatting = TRUE)[2]
  testthat::expect_match(txt, "\\*")
  testthat::expect_match(txt, "%\\) *[-+][0-9.]+%\\*+ *$")
})





# === SECTION: the display-only n / add_pct / p-value rows =========================================

withr::local_options(lifecycle_verbosity = "quiet", .local_envir = testthat::teardown_env())





gss <- fx_gss()





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
  g <- fx_gss()
  g$marital[1:800] <- NA
  g$race[1:40]     <- NA
  g
}





# ---- Phase 14n: one Total row for several row_vars (display-only collapse) --------------------------

# materialise for a backend, count the visible Total rows
n_totrows <- function(tt, backend = "text") {
  m <- suppressMessages(tabxplor:::tab_materialize_extras(tt, backend = backend, pvalue = FALSE))
  sum(tabxplor:::is_totrow(m))
}





# === SECTION: composite width, ratio glyphs, spanning headers =====================================

mult_glyph <- intToUtf8(0x00d7)  # multiply sign



div_glyph  <- intToUtf8(0x00f7)  # divide sign





# === SECTION: stars ride the primary token ========================================================

gss <- fx_gss()




# a synthetic diff column with three significance levels stored in `pvalue`
star_col <- function() {
  fmt(n = rep(100L, 3), scale = "points", pct_type = "row", pct = c(0.4, 0.5, 0.6), diff = c(0.1, 0, -0.1),
      ci_inf = c(0.05, -0.10, -0.20), ci_sup = c(0.15, 0.10, -0.05),
      pvalue = c(0.0005, 0.5, 0.07), display = "pct")
}




testthat::test_that("star presence is the dual of the CI excluding neutral (no contradiction)", {
  col <- tab(gss, marital, race, pct = "row", ci = "ref", stars = TRUE)$White
  st  <- get_stars(col)
  sig <- get_ci_inf(col) > 0 | get_ci_sup(col) < 0
  val <- !is.na(get_pvalue(col))
  testthat::expect_equal(nzchar(st)[val], sig[val])            # starred <=> CI excludes 0
})





# === SECTION: figure-space padding per medium =====================================================

fig <- "\u2007"   # FIGURE SPACE, one digit wide



sig <- "\u03c3"   # sigma



nbs <- "\u202f"   # Phase 14x: the OLD mean/sd joiner -- must be GONE




# === mean (sigma sd): the sd-less cell is padded ==================================
# Phase 22c-iii: the sd tail is an ORDINARY COMPOSITE now (`{mean} (sigma{sd})`, the `mean_sd`
# preset) -- format() has no mean-specific branch left, and the generic per-token padding does the
# work. It aligns the MEANS too, which the hand-rolled tail never did.

mean_col <- function(digits = 1L) {
  fmt(mean = c(1.0, 1.7, 10.25), var = c(NA, 2.1^2, 3^2), n = rep(5L, 3),
      display = DISPLAY_PRESETS$mean_sd$template, scale = "level_mean", digits = digits)
}





# === SECTION: the same grammar on a regression ====================================================

reg_data <- function() {
  fx_gss() |>
    dplyr::mutate(married = factor(dplyr::if_else(marital == "Married", "Married", "Not married")))
}



# Phase 18z13: skip the per-level `n` column (add_n = TRUE by default) -- see helper-reg.R.
first_fmt <- function(t) reg_first_fmt(t)




# ---- Phase 14r: tooltips + the AME NA bug ----------------------------------------------------

# Data with an ORDERED-factor income predictor whose levels contain " - " ($20000 - 24999, ...).
ame_data <- function() {
  fx_gss() |>
    dplyr::mutate(
      married = factor(dplyr::if_else(marital == "Married", "01-Married", "02-Not married")),
      rincome = forcats::fct_recode(rincome, NULL = "No answer", NULL = "Refused",
                                    NULL = "Don't know", NULL = "Not applicable") |>
        forcats::fct_relevel(sort) |> as.ordered()
    )
}
