# Phase k: graceful degradation when TABLE-LEVEL attributes (subtext / test / meta) are missing, or
# when the tabxplor_tab class was dropped in a pipeline but the tabxplor_fmt columns remain. Contract:
#   - cell fmt FIELDS + per-column ATTRIBUTES stay required (the solid foundation, always travel);
#   - the three table-level attributes are optional and NULL-safe -- losing one removes only the
#     behaviour that needs it (test -> the summary block; subtext -> the note; reg meta -> the title
#     and effect wording), never errors;
#   - a class-stripped tibble that still holds fmt columns exports fully coloured (exporters are
#     class-agnostic; they detect fmt columns via is_fmt).
# These tests LOCK that contract: a future unguarded table-attr read would make them error.

# Phase 20a: this file calls functions deprecated in 2.0.0 on purpose -- what it asserts is their
# arithmetic, which the leaf shares with them and which does NOT go away in 2.1.0.
# ⚠ This quiets the TOP-LEVEL calls only: testthat 3e runs local_reproducible_output() inside
# every test_that(), which forces lifecycle_verbosity = "warning" again, so the in-block calls
# still warn. Migrating them to tab() is the corpus sweep routed to Phase 20h.
withr::local_options(lifecycle_verbosity = "quiet", .local_envir = testthat::teardown_env())


df <- forcats::gss_cat |> dplyr::filter(!is.na(rincome), rincome != "No answer")

tc <- tab(df, race, marital, pct = "row", color = TRUE, test = TRUE, stars = TRUE,
          subtext = "A note")
# suppressWarnings: tvhours is over-dispersed (~2.04); the dispersion warning is expected and
# unrelated to what these tests exercise.
tr <- suppressWarnings(
  tab_reg(df, dependent = "tvhours", predictors = c("race", "marital"),
          family = "poisson", empirical = TRUE))

strip_attr  <- function(x, a) { attr(x, a) <- NULL; x }
strip_class <- function(x) { class(x) <- c("tbl_df", "tbl", "data.frame"); x }
md          <- function(x) as.character(suppressMessages(tab_export(x, "md", css = FALSE)))

# Exercise print + every export backend, asserting no error. Suggests-guarded backends skip cleanly.
expect_all_backends_ok <- function(x) {
  quiet <- function(expr) capture.output(suppressMessages(expr))  # swallow cat()/message noise
  expect_no_error(quiet(print(x)))
  expect_no_error(quiet(tab_export(x, "md")))
  expect_no_error(quiet(tab_export(x, "html")))
  # build the plot object; route any draw to a throwaway device (as test-tab_reg-plots.R does) so
  # it neither warns on a missing font nor leaves an Rplots.pdf behind.
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    grDevices::pdf(tempfile(fileext = ".pdf"))
    on.exit(grDevices::dev.off(), add = TRUE)
    expect_no_error(suppressWarnings(suppressMessages(tab_export(x, "plot"))))
  }
  if (requireNamespace("openxlsx2", quietly = TRUE))
    expect_no_error(quiet(
      tab_xl(x, path = withr::local_tempfile(fileext = ".xlsx"),
             open = FALSE, replace = TRUE)))
}


test_that("stripping any single table-level attribute never errors (crosstab, mean, reg)", {
  tm <- tab(df, race, tvhours, color = TRUE, test = TRUE)
  for (base in list(tc, tm, tr)) {
    for (a in c("test", "meta", "subtext")) {
      expect_all_backends_ok(strip_attr(base, a))
    }
  }
})

test_that("dropping the tabxplor_tab class keeps a fully coloured export", {
  dropped <- strip_class(tc)
  expect_false(is_tab(dropped))
  expect_all_backends_ok(dropped)

  # class-agnostic: same coloured markdown body as the classed table (the attrs still ride along).
  expect_identical(md(dropped), md(tc))
  expect_true(any(grepl("\\{\\.[pmou][1-4]", md(dropped))))  # pandoc colour spans present
})

test_that("as_tibble() keeps the table-level attributes and the coloured output", {
  at <- tibble::as_tibble(tc)
  expect_false(is_tab(at))
  expect_false(is.null(attr(at, "test")))
  expect_false(is.null(attr(at, "meta")))
  expect_identical(md(at), md(tc))
})

test_that("losing `test` drops the summary block and nothing else", {
  expect_true(any(grepl("pvalue", md(tc))))              # summary present when test is
  no_test <- md(strip_attr(tc, "test"))
  expect_false(any(grepl("pvalue", no_test)))            # summary gone
  expect_true(any(grepl("\\{\\.[pmou][1-4]", no_test)))  # colours still there
  expect_true(any(grepl("A note", no_test)))             # subtext still there
})

test_that("losing `subtext` drops the note and nothing else", {
  expect_true(any(grepl("A note", md(tc))))
  no_sub <- md(strip_attr(tc, "subtext"))
  expect_false(any(grepl("A note", no_sub)))
  expect_true(any(grepl("pvalue", no_sub)))              # summary still there
})

test_that("a regression losing `meta` drops its title/effect wording, keeps the cells", {
  full <- md(tr)
  expect_true(any(grepl("Poisson regression", full)))    # caption + Model: line
  no_meta <- md(strip_attr(tr, "meta"))
  expect_false(any(grepl("^: Poisson regression", no_meta)))  # caption gone
  expect_true(any(grepl("Model_IRR", no_meta)))          # the estimate columns remain
  expect_true(any(grepl("\\{\\.[pm][1-4]", no_meta)))    # colours still there
})

# Phase 19m-i: "is this a regression" (the STORED kind) and "does it still carry its model recipe"
# are two questions, and a meta-stripped table is exactly where they diverge. Three sites asked the
# second while claiming to ask the first; one of them said so out loud in its abort message.
test_that("a meta-stripped regression keeps its KIND, and each consumer says which question it asks", {
  no_meta <- strip_attr(tr, "meta")
  expect_true(tab_is_reg(no_meta))                       # the kind survives (it rides `test`)
  expect_null(reg_call(no_meta))                         # the recipe does not

  # the plot axis word comes from the COLUMN (model_family + scale), so it survives the strip
  mcol <- grep("^Model_", names(no_meta), value = TRUE)[[1]]
  expect_identical(reg_eff_word_of(no_meta, mcol), reg_eff_word_of(tr, mcol))
  expect_false(is.na(reg_eff_word_of(no_meta, mcol)))

  # reg_check_plots() refits, so it genuinely needs the recipe -- but it must say THAT, not "this is
  # not a tab_reg() table". A crosstab still gets the other message.
  expect_error(reg_check_plots(no_meta, df), "model record")
  expect_error(reg_check_plots(tc, df), "not a")

  # the "Model:" line has nothing to describe, and correctly writes none
  expect_identical(reg_model_lines(no_meta), character(0))
})

test_that("a standalone extracted tabxplor_fmt column formats and colours on its own", {
  # a column known to be coloured in-table
  slot_of  <- function(col) fmt_color_channels(col)$text_slot
  coloured <- which(vapply(tc, function(col)
    is_fmt(col) && any(slot_of(col) != 0), logical(1)))
  expect_gt(length(coloured), 0L)                        # sanity: the table has colour

  col  <- tc[[coloured[[1]]]]
  bare <- tibble::tibble(v = col)                        # no table context whatsoever
  expect_no_error(format(bare$v))
  expect_type(format(bare$v), "character")
  # colour is read from the column's own attributes/fields -> identical detached vs in-table
  expect_identical(slot_of(bare$v), slot_of(col))
  expect_true(any(slot_of(bare$v) != 0))
})

test_that("tab_render_vars / tab_get_vars degrade without error on degenerate frames", {
  fmt_less <- tibble::tibble(a = 1:3)
  fct_less <- tibble::tibble(a = tc[[2]])                # an fmt column, no factor

  expect_no_error(rv <- tabxplor:::tab_render_vars(fmt_less))
  expect_true(isTRUE(rv$degrade))
  expect_no_error(tab_get_vars(fmt_less))

  expect_no_error(rv2 <- tabxplor:::tab_render_vars(fct_less))
  expect_true(isTRUE(rv2$degrade))
  expect_no_error(gv <- tab_get_vars(fct_less))
  expect_length(gv$row_var, 0L)                          # no stray NULL row_var
})

test_that("binding tables tolerates a missing `test` attribute", {
  a <- tab(df, race, marital, pct = "row", test = TRUE)
  b <- strip_attr(a, "test")
  expect_no_error(dplyr::bind_rows(a, b))
  expect_no_error(vctrs::vec_rbind(a, b))
})

test_that("a table stripped of `meta` still refers its intervals to the design df", {
  # Phase 18z16-iiiii: THE reason `degf` and `basis` left meta$inference for the fmt columns.
  # A number must not depend on a table attribute: `meta` is dropped by any rebuild that does not
  # carry it (two such sites were found in this very phase) and by plenty of ordinary data-frame
  # handling, and tab_ci() then silently fell back to z -- measured 9 % too narrow at 13 PSUs.
  skip_if_not_installed("survey")
  set.seed(41)
  n  <- 320L
  dd <- tibble::tibble(
    psu   = rep(1:8, each = n / 8L),
    strat = rep(1:2, each = n / 2L),
    w     = stats::rgamma(n, 2, 2),
    g     = factor(sample(c("a", "b"), n, TRUE)),
    y     = factor(sample(c("no", "yes"), n, TRUE))
  )
  des <- survey::svydesign(ids = ~psu, strata = ~strat, weights = ~w, data = dd, nest = TRUE)
  t   <- suppressMessages(tab_plain(des, g, y, pct = "row"))
  bare <- strip_attr(t, "meta")
  expect_null(tabxplor:::get_meta(bare))                          # non-vacuous: the metadata is gone
  # ... and the fact survives anyway, because it rides the columns
  expect_identical(tabxplor:::tab_inference_basis(bare), "design")
  expect_identical(tabxplor:::tab_inference_degf(bare), as.double(survey::degf(des)))
  ci_full <- suppressMessages(tab_ci(t,    ci = "cell"))
  ci_bare <- suppressMessages(tab_ci(bare, ci = "cell"))
  expect_identical(get_ci_inf(ci_bare[["yes"]]), get_ci_inf(ci_full[["yes"]]))
  # and t(degf) is genuinely WIDER than the z the stripped table used to fall back to
  ci_z <- suppressMessages(tab_ci(bare, ci = "cell", degf = Inf))
  hw   <- function(x) (get_ci_sup(x) - get_ci_inf(x)) / 2
  ok   <- is.finite(hw(ci_bare[["yes"]])) & is.finite(hw(ci_z[["yes"]]))
  expect_gt(sum(hw(ci_bare[["yes"]])[ok] > hw(ci_z[["yes"]])[ok] + 1e-9), 0L)
})
