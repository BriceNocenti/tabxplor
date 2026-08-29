# PURPOSE: the tibble subclass: every dplyr verb preserves the class, and the table's attribute bag survives.
# ROLE: the shipped CONTRACT for R/tab_classes.R -- a failure here is a user-visible change.
#   The exhaustive sweep around it lives in dev/tests/testthat/.
# See: CLAUDE.md section "Testing" (what belongs in which suite).

# === SECTION: the dplyr wall: every verb preserves the class ======================================

withr::local_options(lifecycle_verbosity = "quiet", .local_envir = testthat::teardown_env())






tabs <- tab(fx_gss(), race, marital)





testthat::test_that("dplyr::rowwise preserves class tabxplor_tab", {
  testthat::expect_s3_class(dplyr::rowwise(tabs), "tabxplor_tab")
})





testthat::test_that("dplyr::mutate preserves class tabxplor_tab", {
  testthat::expect_s3_class(dplyr::mutate(tabs, Married = sum(Married)), "tabxplor_tab")
})





testthat::test_that("dplyr::transmute preserves class tabxplor_tab", {
  testthat::expect_s3_class(dplyr::transmute(tabs, race = race, Married = sum(Married)),
                  "tabxplor_tab")
})





testthat::test_that("dplyr::filter preserves class tabxplor_tab", {
  testthat::expect_s3_class(dplyr::filter(tabs, is_totrow(Married)), "tabxplor_tab")
})





testthat::test_that("dplyr::slice preserves class tabxplor_tab", {
  testthat::expect_s3_class(dplyr::slice(tabs, 1:2), "tabxplor_tab")
})





testthat::test_that("dplyr::arrange preserves class tabxplor_tab", {
  testthat::expect_s3_class(dplyr::arrange(tabs, Married), "tabxplor_tab")
})





testthat::test_that("dplyr::distinct preserves class tabxplor_tab", {
  testthat::expect_s3_class(dplyr::distinct(tabs), "tabxplor_tab")
})





testthat::test_that("dplyr::select preserves class tabxplor_tab", {
  testthat::expect_s3_class(dplyr::select(tabs, race, Married), "tabxplor_tab")
})





testthat::test_that("dplyr::rename, rename_with and relocate preserves class tabxplor_tab", {
  testthat::expect_s3_class(dplyr::relocate   (tabs, Divorced , .after = Married),
                            "tabxplor_tab")
  testthat::expect_s3_class(dplyr::rename     (tabs, new_name = race), "tabxplor_tab")
  testthat::expect_s3_class(dplyr::rename_with(tabs, toupper), "tabxplor_tab")
})





testthat::test_that("[<- and [[<- preserves class tabxplor_tab", {
  tabs[4]     <- dplyr::mutate(tabs[4], dplyr::across(.cols = dplyr::everything(), .fns = ~ set_display(., "ctr")))
  tabs[[2]]   <- tabs[[2]] |> set_digits(3)
  tabs[[2, 1]] <- factor("White")
  testthat::expect_s3_class(tabs, "tabxplor_tab")
})







grouped_tabs <- fx_gss() |>
  dplyr::filter(year %in% c(2000, 2014)) |>
  tab(race, marital, year)





testthat::test_that("dplyr::ungroup preserves class tabxplor_tab", {
  testthat::expect_s3_class(dplyr::ungroup(grouped_tabs), "tabxplor_tab")
})





testthat::test_that("dplyr::summarise, preserves class tabxplor_tab", {
  testthat::expect_s3_class(dplyr::summarise (grouped_tabs, Married = sum(Married)),
                            "tabxplor_tab")
})






testthat::test_that("dplyr::rowwise preserves class tabxplor_grouped_tab", {
  testthat::expect_s3_class(dplyr::rowwise(grouped_tabs), "tabxplor_grouped_tab")
})





testthat::test_that("dplyr::mutate preserves class tabxplor_grouped_tab", {
  testthat::expect_s3_class(dplyr::mutate(grouped_tabs, Married = sum(Married)),
                  "tabxplor_grouped_tab")
})





testthat::test_that("dplyr::transmute preserves class tabxplor_grouped_tab", {
  testthat::expect_s3_class(dplyr::transmute(grouped_tabs, year = year, race = race,
                                   Married = sum(Married)), "tabxplor_grouped_tab")
})





testthat::test_that("dplyr::filter preserves class tabxplor_grouped_tab", {
  testthat::expect_s3_class(dplyr::filter(grouped_tabs, is_totrow(Married)),
                            "tabxplor_grouped_tab")
})





testthat::test_that("dplyr::slice preserves class tabxplor_grouped_tab", {
  testthat::expect_s3_class(dplyr::slice(grouped_tabs, 1:2), "tabxplor_grouped_tab")
})





testthat::test_that("dplyr::arrange preserves class tabxplor_grouped_tab", {
  testthat::expect_s3_class(dplyr::arrange(grouped_tabs, Married), "tabxplor_grouped_tab")
})





testthat::test_that("dplyr::distinct preserves class tabxplor_grouped_tab", {
  testthat::expect_s3_class(dplyr::distinct(grouped_tabs), "tabxplor_grouped_tab")
})





testthat::test_that("dplyr::select preserves class tabxplor_grouped_tab", {
  testthat::expect_s3_class(dplyr::select(grouped_tabs, year, race, Married),
                            "tabxplor_grouped_tab")
})





testthat::test_that("dplyr::rename, rename_with and relocate preserves class tabxplor_grouped_tab", {
  testthat::expect_s3_class(dplyr::relocate   (grouped_tabs, Divorced , .after = Married),
                  "tabxplor_grouped_tab")
  testthat::expect_s3_class(dplyr::rename     (grouped_tabs, new_name = year),
                  "tabxplor_grouped_tab")
  testthat::expect_s3_class(dplyr::rename_with(grouped_tabs, toupper), "tabxplor_grouped_tab")
})





testthat::test_that("[<- and [[<- preserves class tabxplor_grouped_tab", {
  grouped_tabs[4]     <- dplyr::mutate(grouped_tabs[4],
                                       dplyr::across(.cols = dplyr::everything(), .fns = ~ set_display(., "ctr")))
  grouped_tabs[[2]]   <- grouped_tabs[[2]] |> forcats::fct_recode("k\u00e9k\u00e9" = "Black")
  grouped_tabs[[2,2]] <- factor("White")
  testthat::expect_s3_class(grouped_tabs, "tabxplor_grouped_tab")
})






# --- Data-driven verb-coverage registry ----------------------------------------------------
# Extensible guardrail for the 2.0.0 refactors (esp. the tab()/tab_many() merge): each verb is
# checked to preserve BOTH tab classes. A failure names the exact verb whose class-preserving
# S3 method is missing/broken. To add a new verb, append one closure here (works identically
# for a flat and a grouped tab) -- see the `/dplyr-method` skill. Complements the explicit
# per-verb tests above.
verb_coverage <- list(
  mutate      = function(x) dplyr::mutate(x, Married = sum(Married)),
  filter      = function(x) dplyr::filter(x, is_totrow(Married)),
  slice       = function(x) dplyr::slice(x, 1:2),
  arrange     = function(x) dplyr::arrange(x, Married),
  distinct    = function(x) dplyr::distinct(x),
  select      = function(x) dplyr::select(x, dplyr::everything()),
  relocate    = function(x) dplyr::relocate(x, Divorced, .after = Married),
  rename_with = function(x) dplyr::rename_with(x, toupper),
  rowwise     = function(x) dplyr::rowwise(x)
)





cov_flat    <- tab(fx_gss(), race, marital)




cov_grouped <- fx_gss() |>
  dplyr::filter(year %in% c(2000, 2014)) |>
  tab(race, marital, year)





for (vname in names(verb_coverage)) {
  local({
    v <- vname
    testthat::test_that(paste0("verb-coverage keeps tabxplor_tab: ", v), {
      testthat::expect_s3_class(verb_coverage[[v]](cov_flat), "tabxplor_tab")
    })
    testthat::test_that(paste0("verb-coverage keeps tabxplor_grouped_tab: ", v), {
      testthat::expect_s3_class(verb_coverage[[v]](cov_grouped), "tabxplor_grouped_tab")
    })
  })
}






# --- Table-attribute survival + class up/down-grade (2.0.0 tab()/tab_many() merge net) -------
# The blocks above check only that the tab CLASS survives a verb; they do NOT check the two
# table-level attributes: `subtext` (the legend) and `chi2` (the test-results tibble that
# Phase 3 renames to `test`). A verb method could silently reset either to its new_tab()
# default and every test above would still pass. These blocks close that hole -- the most
# valuable coverage before the Phase 6 class-model rewrite touches every reattach site.
#
# tab_plain() |> tab_chi2() is the REAL populator of the chi2 attribute (tab(test = TRUE) does
# NOT fill it for simple tables -- see the DESIGN note in test-calculations.R). subtext has no
# lightweight real populator (the subtext= arg stores whole population data), so a sentinel is
# set directly; that still faithfully exercises the carry path (methods do
# `subtext = get_subtext(.data)`). Both attributes are thus non-default, so "survives" is a
# real assertion, not a vacuous empty == empty.
cov_flat_attr <- tab_plain(fx_gss(), race, marital, pct = "row") |> tab_chi2()




attr(cov_flat_attr, "subtext") <- "phase0 sentinel subtext"





cov_grouped_attr <- dplyr::filter(fx_gss(), year %in% c(2000, 2014)) |>
  tab_plain(race, marital, year, pct = "row") |>
  tab_chi2()




attr(cov_grouped_attr, "subtext") <- "phase0 sentinel subtext"





testthat::test_that("attr fixtures are non-trivial (guards the survival tests below)", {
  testthat::expect_gt(nrow(get_test(cov_flat_attr)),    0L)
  testthat::expect_gt(nrow(get_test(cov_grouped_attr)), 0L)
  testthat::expect_true(any(nzchar(get_subtext(cov_flat_attr))))
  testthat::expect_true(any(nzchar(get_subtext(cov_grouped_attr))))
})





attr_fixtures <- list(tabxplor_tab = cov_flat_attr, tabxplor_grouped_tab = cov_grouped_attr)




for (cls in names(attr_fixtures)) {
  for (vname in names(verb_coverage)) {
    local({
      fx    <- attr_fixtures[[cls]]
      klass <- cls
      v     <- vname
      testthat::test_that(paste0("verb keeps subtext + chi2 (", klass, "): ", v), {
        out <- verb_coverage[[v]](fx)
        testthat::expect_identical(get_subtext(out), get_subtext(fx))
        testthat::expect_identical(get_test(out),    get_test(fx))
      })
    })
  }
}






# === SECTION: meta: the table's attribute bag =====================================================

test_that("meta gathers the attrs and every legacy getter reads into it", {
  t <- tab(fx_gss(), marital, race, ci = "auto")
  m <- attr(t, "meta", exact = TRUE)
  expect_type(m, "list")
  expect_true(!is.null(get_vars_attr(t)))
  expect_true(!is.null(get_render_extras(t)))
  # the getters read the SAME objects the meta list holds
  expect_identical(get_vars_attr(t), m$spec$vars)      # Phase 19g: `vars` is a slot of meta$spec
  expect_identical(tab_kind(t), "crosstab")            # ...beside the STORED table kind
  # Phase 19b: which interval METHOD was used is a per-COLUMN fact, not a meta sub-field. A count
  # column carries no interval, so it names none -- which is the point: the method describes THIS
  # column's bounds, not a table-wide setting the legend then indexes by measure (D8).
  expect_true(all(get_ci_method(t)[purrr::map_lgl(t, is_fmt)] == ""))
  tp <- tab(fx_gss(), marital, race, pct = "row", ci = "ref")
  expect_true(all(get_ci_method(tp)[purrr::map_lgl(tp, is_fmt)] == "newcombe"))
})






# === SECTION: a stripped table still renders ======================================================

withr::local_options(lifecycle_verbosity = "quiet", .local_envir = testthat::teardown_env())






df <- fx_gss() |> dplyr::filter(!is.na(rincome), rincome != "No answer")





tc <- tab(df, race, marital, pct = "row", color = TRUE, test = TRUE, stars = TRUE,
          subtext = "A note")




# suppressWarnings: tvhours is over-dispersed (~2.04); the dispersion warning is expected and
# unrelated to what these tests exercise.
tr <- suppressWarnings(
  tab_reg(df, outcome = "tvhours", predictors = c("race", "marital"),
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
  if (requireNamespace("openxlsx2", quietly = TRUE))
    expect_no_error(quiet(
      tab_xl(x, path = withr::local_tempfile(fileext = ".xlsx"),
             open = FALSE, replace = TRUE)))
}





test_that("dropping the tabxplor_tab class keeps a fully coloured export", {
  dropped <- strip_class(tc)
  expect_false(is_tab(dropped))
  expect_all_backends_ok(dropped)

  # class-agnostic: same coloured markdown body as the classed table (the attrs still ride along).
  expect_identical(md(dropped), md(tc))
  expect_true(any(grepl("\\{\\.[pmou][1-4]", md(dropped))))  # pandoc colour spans present
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


# === SECTION: handing a table to base R ===========================================================
# as.matrix() / as.table() drop what is not data -- the totals and the display-time rows -- because a
# CA or a chi-squared run on a table's own margins is wrong.

testthat::test_that("as.matrix() gives the data cells, with the labels as rownames", {
  gss <- fx_gss()
  m <- as.matrix(tab(gss, race, marital))
  testthat::expect_true(is.matrix(m) && is.numeric(m))
  testthat::expect_identical(rownames(m), c("Other", "Black", "White"))
  testthat::expect_false("Total" %in% colnames(m))
  testthat::expect_false("Total" %in% rownames(m))
  # the numbers are the ones the cells SHOW
  testthat::expect_identical(unname(m[, "Married"]),
                             get_num(dplyr::filter(tab(gss, race, marital),
                                                   !is_totrow(tab(gss, race, marital)))[["Married"]]))
})

testthat::test_that("as.matrix(totals = TRUE) keeps them", {
  m <- as.matrix(tab(fx_gss(), race, marital), totals = TRUE)
  testthat::expect_true("Total" %in% colnames(m))
  testthat::expect_true("Total" %in% rownames(m))
})

testthat::test_that("as.matrix() drops the display-time rows and the total table", {
  gss <- fx_gss()
  m <- as.matrix(tab(gss, race, marital, pct = "col", add_pct = TRUE))
  testthat::expect_identical(rownames(m), c("Other", "Black", "White"))
  m2 <- as.matrix(tab(dplyr::filter(gss, year %in% c(2000, 2014)),
                      race, marital, tab_vars = year, totaltab = "table"))
  testthat::expect_false(any(grepl("Ensemble", rownames(m2))))
})

testthat::test_that("several label columns fold into one rowname", {
  m <- as.matrix(tab(fx_gss(), c(race, partyid), marital))
  testthat::expect_true(all(grepl("_", rownames(m))))
})

testthat::test_that("as.table() names the dimnames after the variables", {
  tt <- as.table(tab(fx_gss(), race, marital))
  testthat::expect_s3_class(tt, "table")
  testthat::expect_identical(names(dimnames(tt)), c("race", "marital"))
})

testthat::test_that("a table with no fmt column is refused", {
  testthat::expect_error(as.matrix(new_tab(tibble::tibble(a = 1:2))), "no .*column")
})


# === Phase 24g: get_test() is public ==============================================================

testthat::test_that("get_test() is exported and reads the tests off a built table", {
  testthat::expect_true("get_test" %in% getNamespaceExports("tabxplor"))
  t <- tab(fx_gss(), race, marital, pct = "row", test = TRUE)
  x <- get_test(t)
  testthat::expect_s3_class(x, "tbl_df")
  # the KEY is the contract: a new kind of test is new rows, never new columns
  testthat::expect_true(all(c("var", "col", "test", "statistic", "df1", "pvalue") %in% names(x)))
  testthat::expect_gt(nrow(x), 0L)
  # a table that ran none carries the EMPTY tibble, same columns -- the schema is stable, so a
  # consumer never branches on absence; only a table stripped of its attributes gives NULL.
  none <- get_test(tab(fx_gss(), race, marital, pct = "row"))
  testthat::expect_identical(nrow(none), 0L)
  testthat::expect_true(all(c("var", "col", "test", "statistic", "pvalue") %in% names(none)))
  testthat::expect_null(get_test(tibble::tibble(a = 1)))
})
