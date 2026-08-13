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
  testthat::expect_identical(parse_display_template("{ratio}")$fields, "ratio")     # canonical
  testthat::expect_identical(parse_display_template("{rr}")$fields, "ratio")        # rr -> ratio alias
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

testthat::test_that("validate_display_template() wraps a bare known field (display = 'ci' == '{ci}')", {
  # Phase 16f ergonomics: a bare field name (no braces) is treated as the single-field template, so the
  # historical display = "ci" (and "diff"/"pct"/...) keeps working, mapping to display = "{ci}".
  testthat::expect_identical(validate_display_template("ci"),    "{ci}")
  testthat::expect_identical(validate_display_template("diff"),  "{diff}")
  testthat::expect_identical(validate_display_template("pct"),   "{pct}")
  testthat::expect_identical(validate_display_template("ratio"), "{ratio}")   # alias name wraps too
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
  x <- fmt(n = c(100L, 100L), scale = "points", pct_base = "row", pct = c(0.4, 0.6), diff = c(0.1, -0.1),
           ci_inf = c(0.02, -0.2), ci_sup = c(0.18, -0.02), display = "diff")
  out <- format(set_display(x, "{diff} [{ci}]"))
  testthat::expect_true(all(grepl("\\[", out)))          # the [ci] literal + bracket present
  testthat::expect_true(all(grepl("^[+-]", out)))        # the diff sign leads (primary)
})

testthat::test_that("stars ride the PRIMARY token, not the secondary (not doubled)", {
  x <- fmt(n = c(100L, 100L), scale = "points", pct_base = "row", pct = c(0.4, 0.6),
           diff = c(0.1, -0.1), ci_inf = c(0.02, -0.2), ci_sup = c(0.18, -0.02),
           pvalue = c(0.0005, 0.03), display = "pct")
  # stars are opt-in in format(): request them explicitly (they show at the main display).
  plain <- format(x, stars = TRUE)                            # "40%***", "60%*"
  comp  <- format(set_display(x, "{pct} ({n})"), stars = TRUE)  # "40%*** (100)", "60%* (100)"
  testthat::expect_identical(stringi::stri_count_regex(plain, "\\*"),
                             stringi::stri_count_regex(comp,  "\\*"))   # same star count -> not doubled
  testthat::expect_true(any(stringi::stri_count_regex(plain, "\\*") > 0))  # the test is meaningful
})

testthat::test_that("a composite cell missing a field is left as the plain primary", {
  x <- fmt(n = c(10L, NA_integer_), scale = "level_pct", pct_base = "row", pct = c(0.4, 0.6), display = "pct")
  out <- format(set_display(x, "{pct} ({n})"))
  testthat::expect_identical(out[1], "40% (10)")
  testthat::expect_identical(out[2], "60%")        # n is NA -> plain primary kept, no "(NA)"
})

# === consumer safety: a hand-injected bad template must not crash any consumer =====

testthat::test_that("every display consumer survives a hand-injected malformed template", {
  x <- set_display(fmt(n = c(10L, 20L), scale = "level_pct", pct_base = "row", pct = c(0.4, 0.6), display = "pct"), "{bad")
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

testthat::test_that("tab(display = 'ci') is the bare-field form of display = '{ci}' (Phase 16f)", {
  gss <- forcats::gss_cat
  t_ci    <- tab(gss, marital, race, pct = "row", ci = "diff", display = "ci")
  t_brace <- tab(gss, marital, race, pct = "row", ci = "diff", display = "{ci}")
  fcol_ci    <- t_ci[[which(purrr::map_lgl(t_ci, is_fmt))[1]]]
  fcol_brace <- t_brace[[which(purrr::map_lgl(t_brace, is_fmt))[1]]]
  testthat::expect_identical(get_display(fcol_ci), get_display(fcol_brace))   # same per-cell display
  testthat::expect_identical(format(fcol_ci), format(fcol_brace))             # same rendered cells
  # a genuinely unknown bare display value still aborts (not silently wrapped)
  testthat::expect_error(tab(gss, marital, race, pct = "row", display = "wibble"),
                         "[Cc]omposite|template")
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

# === display = "num_ci": the type-adaptive "{base} {ci}" alias =====================

testthat::test_that("tab(display = 'num_ci') == '{pct} {ci}' / '{mean} {ci}' per column type", {
  gss <- forcats::gss_cat

  # factors, showing the DIFFERENCE CI a significance colour computes: byte-identical to the explicit
  # "{pct} {ci}" template, and every eligible value cell renders a [lo;hi] bracket.
  t_num <- gss |>
    tab(race, marital, pct = "row", color = TRUE, color_signif = "guaranteed_effect",
        display = "num_ci")
  t_tpl <- gss |>
    tab(race, marital, pct = "row", color = TRUE, color_signif = "guaranteed_effect",
        display = "{pct} {ci}")
  testthat::expect_identical(format(t_num), format(t_tpl))
  fcol <- t_num[[which(purrr::map_lgl(t_num, is_fmt))[2]]]
  testthat::expect_gt(sum(grepl("\\[.*;.*\\]", format(fcol))), 0L)      # rule 7: the CI really renders

  # numeric means: "{mean} {ci}", byte-identical to the explicit template
  testthat::expect_identical(
    format(tab(gss, race, age, ci = "cell", display = "num_ci")),
    format(tab(gss, race, age, ci = "cell", display = "{mean} {ci}"))
  )

  # mixed factor + numeric in ONE call: each column resolves by its own type
  t_mix <- tab(gss, race, c(marital, age), pct = "row", ci = "cell", display = "num_ci")
  testthat::expect_identical(
    format(t_mix),
    format(tab(gss, race, c(marital, age), pct = "row", ci = "cell", display = "{pct} {ci}"))
  )                                                                     # factors -> {pct}; age col type stays mean
  testthat::expect_identical(tabxplor:::fmt_var_kind(t_mix[["age"]]), "mean")
})

testthat::test_that("set_display(x, 'num_ci') == tab(display = 'num_ci') (same overlay, post-hoc)", {
  gss <- forcats::gss_cat
  built <- gss |>
    tab(race, marital, pct = "row", color = TRUE, color_signif = "guaranteed_effect")
  post  <- set_display(built, "num_ci")
  live  <- gss |>
    tab(race, marital, pct = "row", color = TRUE, color_signif = "guaranteed_effect",
        display = "num_ci")
  testthat::expect_identical(format(post), format(live))
  # single fmt column resolves too
  col <- built[[which(purrr::map_lgl(built, is_fmt))[2]]]
  testthat::expect_gt(sum(grepl("\\[.*;.*\\]", format(set_display(col, "num_ci")))), 0L)
})

testthat::test_that("tab_counts(display = 'num_ci') does not abort", {
  tc <- tibble::tibble(r = c("a", "a", "b"), c = c("x", "y", "x"), n = c(3, 2, 5))
  testthat::expect_no_error(tab_counts(tc, r, c, counts = n, display = "num_ci"))
})

# === micro-benchmark: the no-composite gate must be cheap (informational) ==========

testthat::test_that("display_primary() no-composite gate is negligible (informational)", {
  d  <- rep(c("pct", "n", "diff"), length.out = 1e6)
  tt <- system.time(for (i in 1:20) display_primary(d))[["elapsed"]]
  message(sprintf("display_primary() x20 on 1e6 no-composite cells: %.3fs", tt))
  testthat::succeed()
})
