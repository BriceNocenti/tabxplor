# Phase 10i-A: the display {} grammar -- shared helpers, edge cases, malformed input, and the
# safety of every display-token consumer when a template is present (or hand-injected). The fmt-level
# and tab()-level rendering of composites lives in test-fmt_class.R (Phase 10i-A section).

# === display_primary(): the hot-path resolver ======================================

testthat::test_that("display_primary() leaves simple tokens and NA untouched (fast path)", {
  d <- c("pct", "n", "diff", "wn", "mean", "ci", "blank", "pvalue")
  testthat::expect_identical(display_primary(d), d)
  testthat::expect_identical(display_primary(c("pct", NA, "n")), c("pct", NA, "n"))
  testthat::expect_identical(display_primary(character(0)), character(0))
  # no "{" anywhere -> the SAME vector is returned (no allocation of a rewritten copy)
  testthat::expect_identical(display_primary(NA_character_), NA_character_)
})

testthat::test_that("display_primary() returns the FIRST {field}, alias-resolved", {
  testthat::expect_identical(display_primary("{pct} (n={n})"), "pct")
  testthat::expect_identical(display_primary("{n} ({pct})"),   "n")
  testthat::expect_identical(display_primary("{ratio} x"),     "rr")   # ratio -> rr alias
  testthat::expect_identical(display_primary("{ pct }"),       "pct")  # trims inside braces
  testthat::expect_identical(display_primary("{diff} [{ci}]"), "diff")
  # a mixed vector: composites resolve, simple/NA pass through, position preserved
  testthat::expect_identical(
    display_primary(c("{pct} ({n})", "pvalue", NA, "diff", "{mean} (sd)")),
    c("pct", "pvalue", NA, "diff", "mean")
  )
})

testthat::test_that("display_primary() never errors on malformed templates", {
  # unbalanced / empty braces: best-effort, no crash (fall through to get_num()'s default `n`)
  testthat::expect_no_error(out <- display_primary(c("{pct", "{}", "a {b", "pct}")))
  testthat::expect_length(out, 4L)
})

# === parse_display_template(): the segment parser ==================================

testthat::test_that("parse_display_template() splits literals and {tokens} in order", {
  p <- parse_display_template("{pct} (n={n})")
  testthat::expect_identical(p$pieces, c("{pct}", " (n=", "{n}", ")"))
  testthat::expect_identical(p$is_tok, c(TRUE, FALSE, TRUE, FALSE))
  testthat::expect_identical(p$fields, c("pct", "n"))

  testthat::expect_identical(parse_display_template("{n} ({pct})")$fields, c("n", "pct"))
  testthat::expect_identical(parse_display_template("{ratio}")$fields, "rr")        # alias
  testthat::expect_identical(parse_display_template("{ pct } x")$fields, "pct")     # trim
  testthat::expect_identical(parse_display_template("{diff} [{ci}]")$fields, c("diff", "ci"))
})

testthat::test_that("parse_display_template() yields no field tokens for a degenerate template", {
  testthat::expect_length(parse_display_template("abc")$fields, 0L)   # literal only
  testthat::expect_length(parse_display_template("{pct")$fields, 0L)  # malformed -> literal
  testthat::expect_false(any(parse_display_template("plain text")$is_tok))
})

# === validate_display_template(): write-time validation ({}-only, no curated sugar) ===

testthat::test_that("validate_display_template() passes a valid {} template through", {
  testthat::expect_identical(validate_display_template("{pct} (n={n})"), "{pct} (n={n})")
  testthat::expect_identical(validate_display_template("{n} ({pct})"),   "{n} ({pct})")
  testthat::expect_identical(validate_display_template("{diff} [{ci}]"), "{diff} [{ci}]")
  testthat::expect_identical(validate_display_template("{ratio}"), "{ratio}")   # alias field ok
})

testthat::test_that("validate_display_template() rejects non-template input (no curated sugar)", {
  # Composites use the {} grammar only -- the old recipe strings are no longer accepted.
  testthat::expect_error(validate_display_template("pct (n)"), "[Cc]omposite|template")
  testthat::expect_error(validate_display_template("n (pct)"), "[Cc]omposite|template")
  testthat::expect_error(validate_display_template("pct_n"),   "[Cc]omposite|template")
  testthat::expect_error(validate_display_template("wibble"),  "[Cc]omposite|template")
})

testthat::test_that("validate_display_template() aborts on malformed / unknown {} input", {
  testthat::expect_error(validate_display_template("{foo}"),   "field")           # unknown field
  testthat::expect_error(validate_display_template("{}"),      "[Mm]alformed")     # empty token
  testthat::expect_error(validate_display_template("{pct"),    "[Mm]alformed")     # unbalanced
  testthat::expect_error(validate_display_template("pct}"),    "[Mm]alformed")     # unbalanced
  testthat::expect_error(validate_display_template("{pct}{"),  "[Mm]alformed")     # stray brace
})

# === format() rendering of general + expert templates =============================

testthat::test_that("an expert {diff} [{ci}] template renders both fields", {
  x <- fmt(n = c(100L, 100L), type = "row", pct = c(0.4, 0.6), diff = c(0.1, -0.1),
           ci_inf = c(0.02, -0.2), ci_sup = c(0.18, -0.02), ci_type = "diff", display = "diff")
  out <- format(set_display(x, "{diff} [{ci}]"))
  testthat::expect_true(all(grepl("\\[", out)))          # the [ci] literal + bracket present
  testthat::expect_true(all(grepl("^[+-]", out)))        # the diff sign leads (primary)
})

testthat::test_that("stars ride the PRIMARY token, not the secondary (not doubled)", {
  x <- fmt(n = c(100L, 100L), type = "row", pct = c(0.4, 0.6),
           diff = c(0.1, -0.1), ci_inf = c(0.02, -0.2), ci_sup = c(0.18, -0.02),
           pvalue = c(0.0005, 0.03), ci_type = "diff", display = "pct")
  plain <- format(x)                              # "40%***", "60%*" (stars on the pct primary)
  comp  <- format(set_display(x, "{pct} ({n})"))  # "40%*** (100)", "60%* (100)"
  testthat::expect_identical(stringr::str_count(plain, "\\*"),
                             stringr::str_count(comp,  "\\*"))   # same star count -> not doubled
  testthat::expect_true(any(stringr::str_count(plain, "\\*") > 0))  # the test is meaningful
})

testthat::test_that("a composite cell missing a field is left as the plain primary", {
  x <- fmt(n = c(10L, NA_integer_), type = "row", pct = c(0.4, 0.6), display = "pct")
  out <- format(set_display(x, "{pct} ({n})"))
  testthat::expect_identical(out[1], "40% (10)")
  testthat::expect_identical(out[2], "60%")        # n is NA -> plain primary kept, no "(NA)"
})

# === consumer safety: a hand-injected bad template must not crash any consumer =====

testthat::test_that("every display consumer survives a hand-injected malformed template", {
  x <- set_display(fmt(n = c(10L, 20L), type = "row", pct = c(0.4, 0.6), display = "pct"), "{bad")
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
  tg <- tab(forcats::gss_cat, marital, race, year, pct = "row", display = "{pct} ({n})")
  fg <- if (is.data.frame(tg)) tg else tg[[1]]
  fcol <- fg[[which(purrr::map_lgl(fg, is_fmt))[1]]]
  testthat::expect_true(any(grepl("{", get_display(fcol), fixed = TRUE)))
  testthat::expect_no_error(invisible(capture.output(print(tg))))

  # pct = "col": composite renders (the primary is the col%), no crash
  tc <- tab(forcats::gss_cat, marital, race, pct = "col", display = "{pct} ({n})")
  fc <- tc[[which(purrr::map_lgl(tc, is_fmt))[1]]]
  testthat::expect_match(format(fc)[1], "\\([0-9 ]+\\)$")
})

testthat::test_that("every exporter renders a composite table without error", {
  t1 <- tab(forcats::gss_cat, marital, race, pct = "row", display = "{pct} ({n})")
  testthat::expect_no_error(tab_md(t1))
  testthat::expect_no_error(invisible(capture.output(print(t1))))
  if (requireNamespace("kableExtra", quietly = TRUE)) {
    testthat::expect_no_error(tab_kable(t1, engine = "kableExtra"))
  }
  testthat::expect_no_error(tab_kable(t1, engine = "html"))   # dependency-free engine
  if (requireNamespace("openxlsx2", quietly = TRUE)) {
    p <- withr::local_tempfile(fileext = ".xlsx")
    testthat::expect_no_error(tab_xl(t1, path = p, replace = TRUE))
    # Excel shows the PRIMARY field (no in-cell composite) -- the file is a real workbook.
    testthat::expect_true(file.exists(p))
  }
})

# === micro-benchmark: the no-composite gate must be cheap (informational) ==========

testthat::test_that("display_primary() no-composite gate is negligible (informational)", {
  d  <- rep(c("pct", "n", "diff"), length.out = 1e6)
  tt <- system.time(for (i in 1:20) display_primary(d))[["elapsed"]]
  message(sprintf("display_primary() x20 on 1e6 no-composite cells: %.3fs", tt))
  testthat::succeed()
})
