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
  # the literals are SPLIT at the top-level bracket boundaries, so every piece belongs to one group
  p <- parse_display_template("{pct} (n={n})")
  testthat::expect_identical(p$pieces, c("{pct}", " ", "(n=", "{n}", ")"))
  testthat::expect_identical(p$is_tok, c(TRUE, FALSE, FALSE, TRUE, FALSE))
  testthat::expect_identical(p$group,  c(0L, 0L, 1L, 1L, 1L))
  testthat::expect_identical(p$fields, c("pct", "n"))
  testthat::expect_identical(p$field_group, c(0L, 1L))

  # ") (" closes one group and opens the next -- the split is what keeps them separable
  p2 <- parse_display_template("{a} ({diff}) ({ratio})")
  testthat::expect_identical(p2$group, c(0L, 0L, 1L, 1L, 1L, 0L, 2L, 2L, 2L))

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
  x <- fmt(n = c(100L, 100L), scale = "points", pct_type = "row", pct = c(0.4, 0.6), diff = c(0.1, -0.1),
           ci_inf = c(0.02, -0.2), ci_sup = c(0.18, -0.02), display = "diff")
  out <- format(set_display(x, "{diff} [{ci}]"))
  testthat::expect_true(all(grepl("\\[", out)))          # the [ci] literal + bracket present
  testthat::expect_true(all(grepl("^[+-]", out)))        # the diff sign leads (primary)
})

testthat::test_that("stars ride the PRIMARY token, not the secondary (not doubled)", {
  x <- fmt(n = c(100L, 100L), scale = "points", pct_type = "row", pct = c(0.4, 0.6),
           diff = c(0.1, -0.1), ci_inf = c(0.02, -0.2), ci_sup = c(0.18, -0.02),
           pvalue = c(0.0005, 0.03), display = "pct")
  # stars are opt-in in format(): request them explicitly (they show at the main display).
  plain <- format(x, stars = TRUE)                            # "40%***", "60%*"
  comp  <- format(set_display(x, "{pct} ({n})"), stars = TRUE)  # "40%*** (100)", "60%* (100)"
  testthat::expect_identical(stringi::stri_count_regex(plain, "\\*"),
                             stringi::stri_count_regex(comp,  "\\*"))   # same star count -> not doubled
  testthat::expect_true(any(stringi::stri_count_regex(plain, "\\*") > 0))  # the test is meaningful
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

testthat::test_that("no template the package writes has TOP-LEVEL literal content", {
  # THE guard on the "a literal is content" rule: a void primary blanks a cell UNLESS the template
  # says something outside its tokens. Every template the package itself writes separates its tokens
  # with whitespace only -- "(Chi2)" and "(n=" sit inside a bracket GROUP, not at the top level -- so
  # the rule cannot reach an existing column. A new template with a bare literal must be deliberate.
  rfiles <- list.files(testthat::test_path("..", "..", "R"), "\\.R$", full.names = TRUE)
  testthat::skip_if(length(rfiles) == 0, "package sources not available (installed check)")
  src <- unlist(lapply(rfiles, readLines, warn = FALSE))
  src <- src[!grepl("^\\s*#", src)]                    # a comment may QUOTE a template it does not write
  lit <- gsub('^"|"$', "", unlist(regmatches(src, gregexpr('"[^"\\\\]*\\{[a-z_]+\\}[^"\\\\]*"', src))))
  cand <- unique(c(unlist(DISPLAY_PRESETS, use.names = FALSE), lit,
                   "{pvalue} (Chi2)", "{pvalue} (F, Welch)", "{pvalue} (Rao-Scott Chi2)"))
  keep <- vapply(cand, function(t) {
    if (nchar(t) > 40 || grepl("[.]code|cli::|\\\\", t)) return(FALSE)
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

# === consumer safety: a hand-injected bad template must not crash any consumer =====

testthat::test_that("every display consumer survives a hand-injected malformed template", {
  x <- set_display(fmt(n = c(10L, 20L), scale = "level_pct", pct_type = "row", pct = c(0.4, 0.6), display = "pct"), "{bad")
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
  t_ci    <- tab(gss, marital, race, pct = "row", ci = "ref", display = "ci")
  t_brace <- tab(gss, marital, race, pct = "row", ci = "ref", display = "{ci}")
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
  gss <- forcats::gss_cat

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

testthat::test_that("set_display(x, 'base_ci') == tab(display = 'base_ci') (same overlay, post-hoc)", {
  gss <- forcats::gss_cat
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

# === the scale-relative tokens and the shared preset table ==========================

testthat::test_that("{est} / {base} resolve to the token each COLUMN renders them as", {
  gss <- forcats::gss_cat
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

testthat::test_that("the preset table is ONE table, resolved the same way by both producers", {
  testthat::expect_identical(
    names(tabxplor:::DISPLAY_PRESETS),
    c("est", "est_ci", "est_base", "est_coef", "base_est_mdiff", "base_est_mratio",
      "base_est", "base", "base_ci", "base_moe"))
  # a preset may declare one arm per column ROLE; an unknown role takes `default`.
  testthat::expect_identical(tabxplor:::display_resolve("est_base"), "{est} ({base})")
  testthat::expect_identical(tabxplor:::display_resolve("est_base", "model"), "{est} ({base})")
  testthat::expect_identical(tabxplor:::display_resolve("est_base", "emp"), "({base}) {est}")
  testthat::expect_identical(tabxplor:::display_resolve("base_est_mdiff", "emp"), "({base}) {est}")
  # idle values leave every cell's own token alone
  for (d in list(NULL, NA_character_, "", "no", "auto"))
    testthat::expect_null(tabxplor:::display_resolve(d))
  testthat::expect_identical(tabxplor:::display_resolve("base_ci"), "{base} {ci}")
  testthat::expect_identical(tabxplor:::display_resolve("est_ci"),  "{est} {ci}")
  testthat::expect_identical(tabxplor:::display_resolve("diff"),    "{diff}")   # a bare token
  testthat::expect_error(tabxplor:::display_resolve("wibble"), "Unknown|Invalid")
  # post-hoc set_display() by preset NAME == the build-time request
  gss <- forcats::gss_cat
  t1  <- tab(gss, race, marital, pct = "row", ci = "ref", display = "base_ci")
  t2  <- tab(gss, race, marital, pct = "row", ci = "ref")
  t2  <- dplyr::mutate(t2, dplyr::across(dplyr::where(is_fmt), ~ set_display(., "base_ci")))
  testthat::expect_identical(format(t1), format(t2))
})

# === the ONE multiplicative rendering ===============================================

testthat::test_that("a multiplicative cell prints its inverse below the neutral, in EVERY path", {
  gss <- forcats::gss_cat
  t <- tab(gss, race, marital, pct = "row", color = "OR", ci = "ref", display = "{or}")
  co <- t[[which(purrr::map_lgl(t, is_fmt))[3]]]
  testthat::expect_true(any(grepl("1/", format(co, special_formatting = TRUE), fixed = TRUE)))
  # the COMPOSITE keeps it -- the defect the one rule exists to close
  testthat::expect_true(any(grepl("1/", format(set_display(co, "{or} ({pct})")), fixed = TRUE)))
  # so does the est_ci bracket, bounds included, and the bounds are NOT reordered
  ec <- set_display(co, "est_ci")
  txt <- stringi::stri_trim(format(ec, special_formatting = TRUE))
  i   <- which(!is.na(get_ci_inf(ec)) & get_or(ec) < 1)[1]
  testthat::skip_if(is.na(i))
  testthat::expect_match(txt[i], "^1/[0-9.]+ +\\[1/[0-9.]+;", perl = TRUE)
  # the option restores the journal convention everywhere at once, cell and ladder alike
  withr::local_options(tabxplor.ratio_print = "raw")
  testthat::expect_false(any(grepl("1/", format(co, special_formatting = TRUE), fixed = TRUE)))
  testthat::expect_false(any(grepl("1/", tab_color_legend(t, medium = "plain", lang = "en"),
                                   fixed = TRUE)))
})


# === the PRIMARY of a composite, and the paint split ===============================================

testthat::test_that("the primary token is the first one outside brackets", {
  prim <- function(tmpl) {
    p <- tabxplor:::parse_display_template(tmpl)
    p$fields[[p$primary]]
  }
  # every template the package writes keeps the token it has always centred on...
  testthat::expect_identical(prim("{est} ({base})"), "est")
  testthat::expect_identical(prim("{pct} (n={n})"),  "pct")
  testthat::expect_identical(prim("{base} {ci}"),    "base")
  testthat::expect_identical(prim("{or} ({obs})"),   "or")
  # ...a bracketed token is an ASIDE, wherever it sits, which is what lets a crude column print its
  # level first and keep the ESTIMATE as the number the cell is about
  testthat::expect_identical(prim("({base}) {est}"), "est")
  testthat::expect_identical(prim("[{n}] {pct}"),    "pct")
  # all of them bracketed -> the first, so a template can still be all-aside
  testthat::expect_identical(prim("({pct})"),        "pct")
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

testthat::test_that("format(bold_split =) reports the primary's character RANGE", {
  x <- fmt(pct = c(.5), n = c(10L), scale = "level_pct", pct_type = "row", digits = 0L)
  rng <- function(tmpl) {
    o <- format(set_display(x, tmpl), bold_split = TRUE, special_formatting = TRUE)
    c(attr(o, "primary_from"), attr(o, "primary_nchar"), nchar(o))
  }
  testthat::expect_identical(rng("{pct} (n={n})"), c(1L, 3L, 10L))   # a prefix
  testthat::expect_identical(rng("({n}) {pct}"),   c(6L, 3L,  8L))   # ...and a suffix
})

testthat::test_that("only the primary token is coloured, and the option says what the rest gets", {
  gss <- forcats::gss_cat
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
  gss <- forcats::gss_cat
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
