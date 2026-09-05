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


# === Phase 25: meta$footer_tabs -- subordinate tables ============================================
# The CONTRACT is "one table renders as several, in every medium": a producer attaches a fact that
# belongs to the table without being a row of it, and no exporter needs to know what it holds.
# *Silent failure guarded: the block renders in the console the author checked and in no export.*

testthat::test_that("set_footer_tabs() attaches, names caption, and survives a dplyr verb", {
  main <- tab(fx_gss(), race, marital, pct = "row")
  side <- tab(fx_gss(), race)

  testthat::expect_null(get_footer_tabs(main))
  x <- set_footer_tabs(main, list("Base" = side))
  testthat::expect_length(get_footer_tabs(x), 1L)
  # a NAME is the subordinate table's caption -- the mechanism that already exists
  testthat::expect_identical(get_caption(get_footer_tabs(x)[[1]]), "Base")
  # a bare table is accepted, and nothing is captioned
  testthat::expect_null(get_caption(get_footer_tabs(set_footer_tabs(main, side))[[1]]))
  # it rides `meta`, so every dplyr verb carries it (tab_attrs)
  testthat::expect_length(get_footer_tabs(dplyr::mutate(x, dummy = 1L)), 1L)
  testthat::expect_null(get_footer_tabs(set_footer_tabs(x, NULL)))
  testthat::expect_error(set_footer_tabs(main, "not a table"), "list of tables")
})


testthat::test_that("a footer table renders beside its host in every medium, and above it in console", {
  main <- set_footer_tabs(tab(fx_gss(), race, marital, pct = "row"),
                          list("Base" = tab(fx_gss(), race)))

  # console: the host stays a pillar grid, the subordinate one prints as a pipe table (phase 6) --
  # ABOVE it, because the LAST thing printed is the R object you can go on to pipe (phase 7).
  txt  <- cli::ansi_strip(print(main, get_text = TRUE))
  testthat::expect_identical(sum(grepl("A tabxplor tab", txt)), 1L)
  testthat::expect_true(any(grepl("^\\|:-", txt)))
  testthat::expect_lt(which(grepl("^\\|:-", txt))[[1]], which(grepl("A tabxplor tab", txt))[[1]])

  # markdown: a second pipe table, carrying its caption line, BELOW the host
  md <- tab_md(main, css = FALSE, print = FALSE)
  testthat::expect_true(grepl(": Base", md, fixed = TRUE))

  # html: two <table> elements from one call
  h <- tab_html(main)
  testthat::expect_identical(lengths(regmatches(h, gregexpr("<table", h, fixed = TRUE))), 2L)
})


testthat::test_that("a host and its footer table share ONE generated footer", {
  # the generated blocks belong to the HOST (FOOTER_BLOCKS' `carried` column): a subordinate renders
  # what it carries and nothing generated, so two coloured tables show one colour legend, not two.
  side <- tab(fx_gss(), race, marital, pct = "row", color = "diff")
  main <- set_footer_tabs(tab(fx_gss(), race, marital, pct = "row", color = "diff"), list(side))

  md <- tab_md(main, css = FALSE, print = FALSE)
  testthat::expect_identical(
    lengths(regmatches(md, gregexpr("difference", md, fixed = TRUE))), 1L)

  h <- tab_html(main)
  testthat::expect_identical(
    lengths(regmatches(h, gregexpr("(risk) difference", h, fixed = TRUE))), 1L)
})


testthat::test_that("a footer table's own footer tables are never rendered", {
  side  <- tab(fx_gss(), race)
  deep  <- set_footer_tabs(side, list(side))
  main  <- set_footer_tabs(tab(fx_gss(), race, marital, pct = "row"), list(deep))

  testthat::expect_identical(
    lengths(regmatches(tab_html(main), gregexpr("<table", tab_html(main), fixed = TRUE))), 2L)
  txt <- cli::ansi_strip(print(main, get_text = TRUE))
  testthat::expect_identical(sum(grepl("A tabxplor tab", txt)), 1L)
  testthat::expect_identical(sum(grepl("^\\|:-", txt)), 1L)   # ONE pipe table, not two
})


# === Phase 6: the console pipe table, the data bar, and the `var` tag ============================

testthat::test_that("a subordinate table prints as a pipe table, not a second pillar grid", {
  # One grid is the table; what travels under it is a note, and the two must not look like peers.
  main <- set_footer_tabs(tab(fx_gss(), race, marital, pct = "row"),
                          list("Base" = tab(fx_gss(), race)))
  txt <- print(main, get_text = TRUE)
  testthat::expect_true(any(grepl("A tabxplor tab", txt)))        # the host, still pillar
  testthat::expect_true(any(grepl("^\\|:-", cli::ansi_strip(txt))))  # the note, a pipe table
  testthat::expect_identical(sum(grepl("A tabxplor tab", txt)), 1L)
})


testthat::test_that("tab_pipe() is tab_md() with three arguments fixed", {
  t <- tab(fx_gss(), race, marital, pct = "row", color = "diff")
  p <- tab_pipe(t)
  testthat::expect_type(p, "character")
  testthat::expect_gt(length(p), 3L)
  testthat::expect_true(any(grepl("<row%>", p, fixed = TRUE)))    # the unit line is kept
  testthat::expect_false(any(grepl("]{.", p, fixed = TRUE)))      # no colour span
  testthat::expect_false(any(grepl("<style>", p, fixed = TRUE)))  # no stylesheet
  testthat::expect_false(any(grepl("difference (Total)", p, fixed = TRUE)))  # no footer
  # ... and it cannot drift from the markdown export: `...` reaches tab_md()
  testthat::expect_true(any(grepl("]{.", tab_pipe(t, color = TRUE), fixed = TRUE)))
})


# *Silent failure guarded: a bar drawn from a total row, or a colour frozen inline where the theme
# should decide -- both look right and are wrong the moment the reader switches to dark.*
testthat::test_that("set_bars() draws a length inline and leaves every colour to the stylesheet", {
  t <- set_bars(tab(fx_gss(), race, marital, pct = "row"), "Married")
  # the stored field is the CEILING of each barred column, NA = "the column's own largest"
  testthat::expect_identical(get_bars(t), c(Married = NA_real_))
  testthat::expect_null(get_bars(set_bars(t, NULL)))
  testthat::expect_length(get_bars(dplyr::mutate(t, dummy = 1L)), 1L)   # rides `meta`
  # the house grammar of a per-variable argument: unnamed = the fallback, named = that column
  testthat::expect_identical(get_bars(set_bars(t, c("Married", "Divorced"), max = 1)),
                             c(Married = 1, Divorced = 1))
  testthat::expect_identical(
    get_bars(set_bars(t, c("Married", "Divorced"), max = c("Divorced" = 0.5))),
    c(Married = NA_real_, Divorced = 0.5))
  testthat::expect_error(set_bars(t, "Married", max = 0), "positive")
  # a table stored before the ceilings existed carried the NAMES alone, and still draws its bars
  old_shape <- set_meta_field(t, "bars", "Married")
  testthat::expect_true(grepl("--tx-bar:100%", as.character(tab_html(old_shape)), fixed = TRUE))

  h <- as.character(tab_html(t))
  # one bar per DATA row -- a total is not on the same scale as what it totals
  testthat::expect_identical(
    lengths(regmatches(h, gregexpr("--tx-bar:", h, fixed = TRUE))), 3L)
  # the tallest fills its cell (the share is of the column's own maximum)
  testthat::expect_true(grepl("--tx-bar:100%", h, fixed = TRUE))
  # the INK is in the stylesheet, never inline: no hex, no colour keyword in a style attribute
  sty <- unlist(regmatches(h, gregexpr('style="[^"]*"', h)))
  testthat::expect_false(any(grepl("#|rgb|oklch", sty)))
  testthat::expect_true(grepl("td.tx-bar", h, fixed = TRUE))
  testthat::expect_true(grepl("currentColor", h, fixed = TRUE))

  # ONE reference per column, or two bars could not be compared: the ceiling is the largest data cell
  # of the WHOLE column, never of a sub-table, and `max` states it instead.
  tg <- set_bars(tab(fx_gss(), race, marital, tab_vars = "year", filter = "year %in% 2000:2002",
                     pct = "row"), "Married")
  hg <- as.character(tab_html(tg))
  testthat::expect_identical(
    lengths(regmatches(hg, gregexpr("--tx-bar:100%", hg, fixed = TRUE))), 1L)
  # a stated ceiling: nothing reaches the full width any more
  hm <- as.character(tab_html(set_bars(t, "Married", max = 1)))
  wm <- as.numeric(sub("%$", "", sub("^--tx-bar:", "",
                       unlist(regmatches(hm, gregexpr("--tx-bar:[0-9.]+%", hm))))))
  testthat::expect_length(wm, 3L)
  testthat::expect_true(max(wm) < 100 && max(wm) > 40)   # a share of 100 %, not of the largest

  # a bar of length zero is not a bar: no class, no style -- its border would draw a tick on nothing
  t0 <- t; t0$Married <- set_num(t0$Married, c(0, get_num(t0$Married)[-1]))
  h0 <- as.character(tab_html(t0))
  testthat::expect_identical(
    lengths(regmatches(h0, gregexpr("--tx-bar:", h0, fixed = TRUE))), 2L)

  # ⚠ A NAME WITH A SPACE IS A DIFFERENT NAME BY THE TIME THE HTML BACKEND LOOKS IT UP:
  # tab_wrap_text() rewrites every space to U+202F and renames the column, so a `bars` list keyed
  # before the wrap stops matching -- and the bar then vanishes with no error. The whole feature's
  # real caller (ggfacto's "% variance") has a space in it.
  h2 <- as.character(tab_html(set_bars(tab(fx_gss(), race, marital, pct = "row"), "Never married")))
  testthat::expect_true(grepl("--tx-bar:100%", h2, fixed = TRUE))
  testthat::expect_identical(
    lengths(regmatches(h2, gregexpr("--tx-bar:", h2, fixed = TRUE))), 3L)

  # ...and the SAME trap on the other seam: a transposed column IS a row level, so a `bars` list kept
  # across the flip would name nothing. A bar is a per-COLUMN scale and has nothing left to be a share
  # of -- it goes, rather than mis-drawing or silently matching none.
  h3 <- as.character(tab_html(set_bars(tab(fx_gss(), race, marital, pct = "row"), "Married"),
                              transpose = TRUE))
  testthat::expect_false(grepl("--tx-bar:", h3, fixed = TRUE))
})

# *`getOption()` of an option nobody set is NULL, and `NULL %in% x` is `logical(0)`, which stops an
# `if`. .onLoad() seeds it -- but a package reaching tabxplor only through `tabxplor::` never loads
# its namespace, so a user of THAT package could not print a table at all.*
testthat::test_that("printing works with no tabxplor.print option set", {
  t <- tab(fx_gss(), race, marital, pct = "row")
  withr::local_options(tabxplor.print = NULL)
  testthat::expect_null(getOption("tabxplor.print"))
  testthat::expect_no_error(print(t, get_text = TRUE))
})


# *Silent failure guarded: a column of variances headed "the variance of the mean".*
testthat::test_that("a variance names itself, and the prefix rule is asked rather than derived", {
  v <- fmt(n = rep(100, 3), scale = "level_mean", mean = c(2, 3, 4), var = c(4, 9, 16),
           display = "var")
  testthat::expect_identical(tabxplor:::fmt_display_label(v), "var")
  # its own square root already did, and the two must agree
  testthat::expect_identical(
    tabxplor:::fmt_display_label(tabxplor::set_display(v, "sd")), "sd")
  # ⚠ `prefix` is NOT `geometry`: `var` still names no effect geometry, which the mismatch refusal reads
  testthat::expect_true(is.na(tabxplor:::DISPLAY_TOKENS$var$geometry))
  testthat::expect_false(tabxplor:::DISPLAY_TOKENS$var$prefix)
  # a deviation still takes the prefix -- that is what the rule is for
  d <- fmt(n = rep(100, 3), scale = "level_pct", pct_type = "row", pct = c(.1, .2, .3),
           diff = c(.01, .02, .03), display = "diff")
  testthat::expect_identical(tabxplor:::fmt_display_label(d), "row%-diff")
})


# === Phase 10: tab_wrap_text() is unchanged, and the exporters no longer use half of it ===========

testthat::test_that("tab_wrap_text() wraps NAMES and VALUES, tx_wrap_labels() only the values", {
  t <- tab(fx_gss(), marital, race, pct = "row")
  w <- tab_wrap_text(t, wrap_rows = 5L, wrap_cols = 4L, brk = "<br>")
  testthat::expect_true(any(grepl("<br>", names(w), fixed = TRUE)))
  testthat::expect_true(any(grepl("<br>", levels(w[[1]]), fixed = TRUE)))
  # the exporters run only the second half, which is what keeps every per-column fact keyed by a
  # name from going stale mid-export.
  v <- tabxplor:::tx_wrap_labels(t, wrap_rows = 5L, brk = "<br>")
  testthat::expect_identical(names(v), names(t))
  testthat::expect_identical(levels(v[[1]]), levels(w[[1]]))
})
