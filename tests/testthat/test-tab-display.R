# PURPOSE: the {} display grammar -- the parser, the primary token, and the presets both producers share.
# ROLE: the shipped CONTRACT for R/tab-display.R -- a failure here is a user-visible change.
#   The exhaustive sweep around it lives in dev/tests/testthat/.
# See: CLAUDE.md section "Testing" (what belongs in which suite).

# === SECTION: the {} grammar: parse, validate, primary token ======================================

testthat::test_that("display_primary() leaves simple tokens and NA untouched (fast path)", {
  d <- c("pct", "n", "diff", "wn", "mean", "ci", "blank", "pvalue")
  testthat::expect_identical(display_primary(d), d)
  testthat::expect_identical(display_primary(c("pct", NA, "n")), c("pct", NA, "n"))
  testthat::expect_identical(display_primary(character(0)), character(0))
  # no "{" anywhere -> the SAME vector is returned (no allocation of a rewritten copy)
  testthat::expect_identical(display_primary(NA_character_), NA_character_)
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




testthat::test_that("validate_display_template() aborts on malformed / unknown {} input", {
  testthat::expect_error(validate_display_template("{foo}"),   "field")           # unknown field
  testthat::expect_error(validate_display_template("{}"),      "[Mm]alformed")     # empty token
  testthat::expect_error(validate_display_template("{pct"),    "[Mm]alformed")     # unbalanced
  testthat::expect_error(validate_display_template("pct}"),    "[Mm]alformed")     # unbalanced
  testthat::expect_error(validate_display_template("{pct}{"),  "[Mm]alformed")     # stray brace
})




testthat::test_that("stars ride the PRIMARY token, not the secondary (not doubled)", {
  x <- fmt(n = c(100L, 100L), scale = "points", pct_type = "row", pct = c(0.4, 0.6),
           diff = c(0.1, -0.1), ci_inf = c(0.02, -0.2), ci_sup = c(0.18, -0.02),
           pvalue = c(0.0005, 0.03), display = "pct")
  # stars are opt-in in format(): request them explicitly (they show at the main display).
  plain <- format(x, stars = TRUE)                            # "40%***", "60%*"
  comp  <- format(set_display(x, "{pct} ({n})"), stars = TRUE)  # "40%*** (100)", "60%* (100)"
  testthat::expect_identical(lengths(regmatches(plain, gregexpr("\\*", plain, perl = TRUE))),
                             lengths(regmatches(comp, gregexpr("\\*", comp, perl = TRUE))))   # same star count -> not doubled
  testthat::expect_true(any(lengths(regmatches(plain, gregexpr("\\*", plain, perl = TRUE))) > 0))  # the test is meaningful
})




testthat::test_that("tab(display = 'ci') is the bare-field form of display = '{ci}' (Phase 16f)", {
  gss <- fx_gss()
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




testthat::test_that("the preset table is ONE table, resolved the same way by both producers", {
  testthat::expect_identical(
    names(tabxplor:::DISPLAY_PRESETS),
    c("est", "est_ci", "est_base", "est_base_once", "est_coef", "base_est_mdiff",
      "base_est_mratio", "est_obs", "base_est", "base", "base_ci", "base_moe", "base_diff",
      "base_ratio", "base_or", "or_base", "mean_sd", "mean_cv", "or_pct", "OR_pct", "estimate"))
  # `est_base` with the level stated once, by the observed column: the comparison default.
  testthat::expect_identical(tabxplor:::display_resolve("est_base_once", "model"), "{est}")
  testthat::expect_identical(tabxplor:::display_resolve("est_base_once", "emp"), "({base}) {est}")
  # the word spelt out is an ALIAS: it resolves before anything is stored.
  testthat::expect_identical(tabxplor:::display_resolve("estimate"),
                             tabxplor:::display_resolve("est"))
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
  gss <- fx_gss()
  t1  <- tab(gss, race, marital, pct = "row", ci = "ref", display = "base_ci")
  t2  <- tab(gss, race, marital, pct = "row", ci = "ref")
  t2  <- dplyr::mutate(t2, dplyr::across(dplyr::where(is_fmt), ~ set_display(., "base_ci")))
  testthat::expect_identical(format(t1), format(t2))
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




testthat::test_that("the Total's 100 % goes when the cells stop showing a level", {
  g   <- fx_reg_fmt()
  tot <- function(...) {
    t <- suppressMessages(tab(g, race, party3, pct = "row", na = "drop_all", color = TRUE, ...))
    format(tabxplor:::tab_materialize_extras(t, backend = "text", pvalue = FALSE)$Total)[[1]]
  }
  # a level is shown -> the block sums to the Total, and says so
  testthat::expect_match(tot(display = "pct"),      "^100%")
  testthat::expect_match(tot(ci = "cell"),          "^100%")   # an interval is still ABOUT its share
  testthat::expect_match(tot(ci = "ref"),           "^100%")
  testthat::expect_match(tot(display = "base_ratio"), "^100%") # the primary is the level
  # a deviation is shown -> nothing sums, and only the base count is printed
  testthat::expect_false(grepl("100%", tot(display = "ratio")))
  testthat::expect_false(grepl("100%", tot(display = "{ratio} ({pct})")))
  testthat::expect_false(grepl("100%", tot(display = "or")))
  testthat::expect_false(grepl("100%", tot(display = "or_base")))
  # ... and a COLOUR never removes it. `color = "ratio"` plus a reference interval stamps the
  # interval's own scale (`pct_ratio`) on every column, the Total included; the sum test asks its
  # question on the LEVEL twin, so a ratio and a difference behave alike.
  totc <- function(...) {
    t <- suppressMessages(tab(g, race, party3, pct = "row", na = "drop_all", ...))
    format(tabxplor:::tab_materialize_extras(t, backend = "text", pvalue = FALSE)$Total)[[1]]
  }
  testthat::expect_match(totc(color = "ratio", ci = "ref"),                       "^100%")
  testthat::expect_match(totc(color = "ratio", color_signif = "grey_non_signif"), "^100%")
  testthat::expect_match(totc(color = "ratio", stars = TRUE),                     "^100%")
  testthat::expect_match(totc(color = "difference", ci = "ref"),                  "^100%")
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




test_that("choosing a display changes no number (D11), presets included", {
  d <- reg_data()
  t <- suppressMessages(tab_reg(d, "married", c("race", "age"), family = "binomial",
                                empirical = TRUE))
  for (p in c("est", "est_base", "base_est_mdiff", "base_est_mratio", "est_coef")) {
    for (nm in names(t)[purrr::map_lgl(t, is_fmt)]) {
      before <- t[[nm]]; after <- set_display(before, p)
      for (f in setdiff(tabxplor:::fmt_field_names, "display"))
        expect_identical(vctrs::field(after, f), vctrs::field(before, f),
                         label = paste(p, nm, f))
    }
  }
})




test_that("a display token may carry its own precision, and Excel follows it", {
  d <- reg_data()
  # the grammar is the same one `digits = c(base = 1)` writes
  oc <- first_fmt(tab_reg(d, "married", "race", stats = "no", empirical = FALSE,
                          display = "{est:4} ({base:1})"))
  expect_true(any(grepl("\\.[0-9]{4} \\([0-9]+\\.[0-9]%\\)", format(oc))))
  # ⚠ the Excel number format is finalized BEFORE the composite expander, so the PRIMARY's own
  # suffix has to be read there too -- it used to be dropped, silently
  expect_true(any(grepl("0\\.0000", format(oc, syntax = "excel"))))
  expect_error(tab_reg(d, "married", "race", display = "{est:9}"), "Invalid precision")
})
