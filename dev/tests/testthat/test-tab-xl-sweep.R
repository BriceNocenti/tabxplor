
# === SECTION: the Excel workbook a user opens =====================================================

xl_numfmt_codes <- function(f) {
  st <- openxlsx2::wb_load(f)$styles_mgr$styles$numFmts
  if (!length(st)) return(character(0))
  sub('".*$', "", sub('^.*formatCode="', "", st))
}

# the merged ranges, as plain A1 strings
xl_merges <- function(wb) sub('".*$', "", sub('^.*ref="', "", unlist(wb$worksheets[[1]]$mergeCells)))


testthat::test_that("Phase h: xlb_add_data resolves the openxlsx2 NA arg name (no partial-match crash)", {
  testthat::skip_if_not_installed("openxlsx2")
  wb <- openxlsx2::wb_workbook()$add_worksheet("s")
  # The NA arg is `na` (new openxlsx2) or `na_strings` (older jamovi-bundled build); either way the
  # resolved name MUST be a real formal so it never partial-matches `name` ("argument matches multiple
  # formal arguments" = the Excel-export crash).
  nm <- xlb_na_argname(wb)
  testthat::expect_true(nm %in% names(formals(wb$add_data)))
  testthat::expect_no_error(xlb_write_data(wb, "s", data.frame(a = c(1, NA)), 1L, 1L))
})


testthat::test_that("tab_xl work with  after_ci", {
  testthat::skip_if_not_installed("openxlsx2")
  withr::local_options(lifecycle_verbosity = "quiet")
  tabs <-tab(fx_gss(), race, marital, pct = "row", color = "after_ci")

  test_path <- file.path(tempdir(), "tab_xl_test.xlsx")

  tabs |>
    tab_xl(path = test_path, sheets = "unique",
           replace = TRUE, open = FALSE) |>
    testthat::expect_invisible()

  testthat::expect_true(file.exists(test_path))

  file.remove(test_path)
})


# Phase 10g: read the written workbook back and confirm the raw get_num() values reached the file
# (Excel stores the raw value; the "%" is a display-only numFmt). Closes the "no test inspects the
# written file" gap.
testthat::test_that("tab_xl writes get_num() values that round-trip from the file", {
  testthat::skip_if_not_installed("openxlsx2")
  tb <- tab(fx_gss(), marital, race, pct = "row")
  p  <- withr::local_tempfile(fileext = ".xlsx")
  suppressMessages(tab_xl(tb, path = p, sheets = "unique", replace = TRUE, open = FALSE))
  testthat::expect_true(file.exists(p))

  wb <- openxlsx2::wb_load(p)
  testthat::expect_gte(length(openxlsx2::wb_get_sheet_names(wb)), 1)

  d    <- openxlsx2::wb_to_df(p, sheet = 1, col_names = FALSE, convert = TRUE)
  vals <- round(suppressWarnings(as.numeric(unlist(d, use.names = FALSE))), 6)
  vals <- vals[!is.na(vals)]

  fmt_names <- names(tb)[purrr::map_lgl(tb, is_fmt)]
  want <- round(get_num(tb[[fmt_names[[1]]]]), 6)
  want <- want[!is.na(want)]
  testthat::expect_true(all(want %in% vals))
})


# Phase 10h / bug-fix: significance stars are folded into the Excel numFmt code (0.0%"***"), keeping
# the cell a real number. STORAGE-driven (like the console): a table built with stars = TRUE carries
# a per-cell pvalue -> star literals; the opt-out default (stars = FALSE) writes none.
testthat::test_that("tab_xl folds significance stars into the numFmt code", {
  testthat::skip_if_not_installed("openxlsx2")
  tb <- tab(fx_gss(), marital, race, pct = "row", color = "diff",
            color_signif = "guaranteed_effect", stars = TRUE)
  p  <- withr::local_tempfile(fileext = ".xlsx")
  suppressMessages(tab_xl(tb, path = p, sheets = "unique", replace = TRUE, open = FALSE))
  codes <- openxlsx2::wb_load(p)$styles_mgr$styles$numFmts
  testthat::expect_true(any(grepl("\\*", codes)))                 # a code carries the star literal
  # Phase q: the star literal is backslash-escaped (0.0%\*\*\*), NEVER double-quote-wrapped. A raw " inside
  # formatCode="..." is unescaped by the older jamovi-bundled openxlsx2 -> its read_xml round-trip fails
  # with "xml import unsuccessful" (the Windows-side Excel-export crash). Assert on the SOURCE codes (the
  # reloaded XML has attribute-delimiter quotes): no numFmt code carries a raw ".
  src <- unlist(lapply(tb, function(col) if (is_fmt(col)) format(col, syntax = "excel")))
  testthat::expect_false(any(grepl('"', src, fixed = TRUE)))

  # a table built without stars stores no pvalue -> no star literal
  tb2 <- tab(fx_gss(), marital, race, pct = "row", color = "diff",
             color_signif = "guaranteed_effect", stars = FALSE)
  p2 <- withr::local_tempfile(fileext = ".xlsx")
  suppressMessages(tab_xl(tb2, path = p2, sheets = "unique", replace = TRUE, open = FALSE))
  codes2 <- openxlsx2::wb_load(p2)$styles_mgr$styles$numFmts
  testthat::expect_false(any(grepl("\\*", codes2)))
})


# Phase 14l: the fonts we emit must carry NO `scheme`. openxlsx2::create_font() defaults
# scheme = "minor" = "this IS the theme's body font", and Excel then resolves the font from the THEME,
# ignoring our explicit `name` -- so every number, correctly named in the XML, was drawn in the theme's
# minor font ("DejaVu Sans Condensed", written by xlb_base_font). Invisible to any assertion on values
# or on `name`: only the raw <font> XML shows it.
# Phase 14m-ii (rework): numbers use proportional DejaVu Sans in a PLAIN table, and switch to the
# monospace "Cascadia Mono" only when the table SHOWS significance stars (so the stars align).
testthat::test_that("tab_xl emits no font `scheme` (numbers really render in font_num)", {
  testthat::skip_if_not_installed("openxlsx2")
  tb <- tab(fx_gss(), marital, c(race, tvhours), pct = "row", color = TRUE)  # no stars
  p  <- withr::local_tempfile(fileext = ".xlsx")
  suppressMessages(tab_xl(tb, path = p, sheets = "unique", replace = TRUE, open = FALSE))
  fonts <- openxlsx2::wb_load(p)$styles_mgr$styles$fonts

  # openxlsx2's own base font is the ONLY one that may carry a scheme -- "minor" is semantically
  # right there (it IS the theme's body font, and what Excel measures column widths in). Whether it
  # writes one at all is openxlsx2's business: 1.28 does, 1.29 does not. What must hold is that no
  # font WE register carries one, since a scheme sends Excel back to the theme for the face.
  scheme <- grepl("<scheme", fonts)
  testthat::expect_lte(sum(scheme), 1L)
  if (any(scheme)) testthat::expect_true(all(grepl("DejaVu Sans Condensed", fonts[scheme])))
  # a plain table: numbers in the PROPORTIONAL DejaVu Sans, text in Condensed, no Cascadia
  testthat::expect_true(any(grepl('name val="DejaVu Sans"', fonts, fixed = TRUE)))
  testthat::expect_true(any(grepl('name val="DejaVu Sans Condensed"', fonts, fixed = TRUE)))
  testthat::expect_false(any(grepl("Cascadia", fonts)))
  # a STARRED table: numbers switch to the monospace Cascadia Mono
  d  <- fx_gss(); d$married <- as.integer(d$marital == "Married")
  tr <- suppressWarnings(tab_reg(d, "married", c("race", "relig")))
  p2 <- withr::local_tempfile(fileext = ".xlsx")
  suppressMessages(tab_xl(tr, path = p2, sheets = "unique", replace = TRUE, open = FALSE))
  fonts2 <- openxlsx2::wb_load(p2)$styles_mgr$styles$fonts
  testthat::expect_true(any(grepl('name val="Cascadia Mono"', fonts2, fixed = TRUE)))
})


# Phase 14l: an "<var>_sd" sibling holds "s2.1" under a header of "sd" -- it never needs a mean's width.
testthat::test_that("tab_xl narrows the sd column", {
  testthat::skip_if_not_installed("openxlsx2")
  # `mean_sd`: the sd aside is what gets a column of its own here, and the narrow-width rule is
  # declared for it alone (its numbers are short under a two-letter header).
  tb <- tab(fx_gss(), marital, c(race, tvhours), pct = "row", display = "mean_sd")
  p  <- withr::local_tempfile(fileext = ".xlsx")
  # `colwidth` as a NUMBER is what the narrow-sd rule qualifies; the default "auto" measures each
  # column's own content instead (asserted below).
  suppressMessages(tab_xl(tb, path = p, sheets = "unique", replace = TRUE, open = FALSE,
                          colwidth = 10))
  cols <- openxlsx2::wb_load(p)$worksheets[[1]]$cols_attr
  wid  <- function(i) {                      # width of the col_attr entry covering column i
    lo <- as.integer(sub('.*min="(\\d+)".*', "\\1", cols))
    hi <- as.integer(sub('.*max="(\\d+)".*', "\\1", cols))
    as.double(sub('.*width="([0-9.]+)".*', "\\1", cols))[which(lo <= i & hi >= i)][1]
  }
  # geometry: A = row labels, B:D = race levels, E = Total, F = mean, G = sd, H = n
  testthat::expect_lt(wid(7), wid(6))                      # the sd column is narrower than its mean
  testthat::expect_equal(wid(7), wid(6) - 4)               # max(5, colwidth * 0.6) at colwidth = 10
  # it scales with `colwidth` rather than being a fixed number
  p2 <- withr::local_tempfile(fileext = ".xlsx")
  suppressMessages(tab_xl(tb, path = p2, sheets = "unique", replace = TRUE, open = FALSE,
                          colwidth = 20))
  cols <- openxlsx2::wb_load(p2)$worksheets[[1]]$cols_attr
  testthat::expect_equal(wid(7), wid(6) - 8)               # 20 -> 12
  # ... and under the default "auto" there is no per-column rule at all: every column is measured
  # from what its own cells will show, which is strictly more compact than one width for all of them.
  p3 <- withr::local_tempfile(fileext = ".xlsx")
  suppressMessages(tab_xl(tb, path = p3, sheets = "unique", replace = TRUE, open = FALSE))
  cols <- openxlsx2::wb_load(p3)$worksheets[[1]]$cols_attr
  auto_total <- sum(vapply(1:8, wid, double(1)))
  suppressMessages(tab_xl(tb, path = p, sheets = "unique", replace = TRUE, open = FALSE,
                          colwidth = 10))
  cols <- openxlsx2::wb_load(p)$worksheets[[1]]$cols_attr
  testthat::expect_lt(auto_total, sum(vapply(1:8, wid, double(1))))
})


testthat::test_that("tab_xl fonts are settable by option (plain vs starred)", {
  testthat::skip_if_not_installed("openxlsx2")
  withr::local_options(tabxplor.xl_font_num = "Courier New", tabxplor.xl_font_text = "Georgia",
                       tabxplor.xl_font_num_stars = "Consolas")
  # plain table -> the NO-stars number font
  tb <- tab(fx_gss(), marital, race, pct = "row", color = TRUE)
  p  <- withr::local_tempfile(fileext = ".xlsx")
  suppressMessages(tab_xl(tb, path = p, sheets = "unique", replace = TRUE, open = FALSE))
  fonts <- openxlsx2::wb_load(p)$styles_mgr$styles$fonts
  testthat::expect_true(any(grepl('name val="Courier New"', fonts, fixed = TRUE)))
  testthat::expect_true(any(grepl('name val="Georgia"',     fonts, fixed = TRUE)))
  testthat::expect_false(any(grepl("DejaVu|Consolas", fonts)))   # nothing hardcoded; stars font unused
  # starred table -> the STARS number font
  d  <- fx_gss(); d$married <- as.integer(d$marital == "Married")
  tr <- suppressWarnings(tab_reg(d, "married", c("race", "relig")))
  p2 <- withr::local_tempfile(fileext = ".xlsx")
  suppressMessages(tab_xl(tr, path = p2, sheets = "unique", replace = TRUE, open = FALSE))
  fonts2 <- openxlsx2::wb_load(p2)$styles_mgr$styles$fonts
  testthat::expect_true(any(grepl('name val="Consolas"', fonts2, fixed = TRUE)))
})


# Phase 14m-ii: a text-SHAPED fmt cell (ci = "cell" bracket, or the "1/x" OR string) must render in the
# NUMBER font, not the text font. Those columns are already in roles$fmt_cols (so mk_src(fmt_cols,
# font_num) covers them); this traces one bracket cell's style -> font to prove it, distinguishing the
# number font from the text font. (A plain ci = "cell" crosstab has no stars -> the proportional font_num.)
testthat::test_that("tab_xl draws text-shaped fmt cells (ci = 'cell') in the number font", {
  testthat::skip_if_not_installed("openxlsx2")
  withr::local_options(tabxplor.xl_font_num = "Courier New", tabxplor.xl_font_text = "Georgia")
  tb <- tab(fx_gss(), marital, race, pct = "row", ci = "cell")
  p  <- withr::local_tempfile(fileext = ".xlsx")
  suppressMessages(tab_xl(tb, path = p, sheets = "unique", replace = TRUE, open = FALSE))
  wb    <- openxlsx2::wb_load(p)
  cc    <- wb$worksheets[[1]]$sheet_data$cc
  xf    <- wb$styles_mgr$styles$cellXfs
  fonts <- wb$styles_mgr$styles$fonts
  font_of <- function(cell_style) {                        # c_s (0-based) -> xf -> fontId -> <font>
    fid <- as.integer(sub('.*fontId="(\\d+)".*', "\\1", xf[as.integer(cell_style) + 1L])) + 1L
    fonts[fid]
  }
  br <- cc[grepl("[", cc$is, fixed = TRUE) | grepl("[", cc$v, fixed = TRUE), , drop = FALSE]
  testthat::expect_gt(nrow(br), 0)                         # the ci brackets ARE text-shaped cells
  testthat::expect_true(grepl('name val="Courier New"', font_of(br$c_s[1]), fixed = TRUE))
  testthat::expect_false(grepl("Georgia", font_of(br$c_s[1]), fixed = TRUE))
})


# Phase 10h: transpose = TRUE exports the transposed table (still a valid, readable workbook).
testthat::test_that("tab_xl(transpose = TRUE) writes a valid workbook", {
  testthat::skip_if_not_installed("openxlsx2")
  tb <- tab(fx_gss(), marital, race, pct = "row", color = "diff")
  p  <- withr::local_tempfile(fileext = ".xlsx")
  suppressMessages(tab_xl(tb, path = p, replace = TRUE, open = FALSE, transpose = TRUE))
  testthat::expect_true(file.exists(p) && file.size(p) > 0)
})


testthat::test_that("tab_xl: a one-row block falls back to horizontal, with no merge", {
  testthat::skip_if_not_installed("openxlsx2")
  # Excel rejects a 1-cell "merge", and a rotated 1-row cell would only force a tall row. This also
  # drives label_merges' empty-tibble path (a size-1 `col` recycled to zero rows). Keep one DATA level
  # per block (not the Totals, which the Phase 14n collapse would reduce to a single row).
  one <- tab(fx_gss(), c(race, marital), relig, pct = "row") |>
    dplyr::filter(!!rlang::sym("levels") %in% c("White", "Married"))
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  suppressMessages(tab_xl(one, path = tmp, open = FALSE, replace = TRUE))
  wb <- openxlsx2::wb_load(tmp)
  # no DATA merge in column A (a 1-cell "merge" is rejected by Excel); the header/unit merge above the
  # data is a different thing and is expected -- see "an index column's header takes both rows".
  # ⚠ the index column's HEADER is merged over the unit row below it, and that is also a
  # within-column merge -- so what this asserts is that no DATA block was merged: exactly one.
  testthat::expect_length(grep("^A[0-9]+:A", xl_merges(wb), value = TRUE), 1L)
  testthat::expect_false(any(grepl('textRotation="90"', wb$styles_mgr$styles$cellXfs)))
  # each block is its own run, so both names are still written
  testthat::expect_equal(as.character(openxlsx2::wb_to_df(wb, col_names = FALSE)[5:6, 1]),
                         c("race", "marital"))
})


testthat::test_that("tab_xl: var_names = 'none' drops both name annotations", {
  testthat::skip_if_not_installed("openxlsx2")
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  suppressMessages(
    tab_xl(tab(fx_gss(), c(race, marital), relig, pct = "row"), path = tmp, open = FALSE,
           replace = TRUE, var_names = "none")
  )
  wb <- openxlsx2::wb_load(tmp)
  d  <- openxlsx2::wb_to_df(wb, col_names = FALSE)
  # no name column -> no DATA merge; and no span row, so the header climbs to row 2. The one merge
  # left is the index column's header over the unit row below it.
  testthat::expect_length(grep("^A[0-9]+:A", xl_merges(wb), value = TRUE), 1L)
  testthat::expect_equal(as.character(d[2, 1]), "levels")
  testthat::expect_false(any(grepl('textRotation="90"', wb$styles_mgr$styles$cellXfs)))
})


testthat::test_that("tab_xl(open = TRUE) never errors when no spreadsheet app is found", {
  testthat::skip_if_not_installed("openxlsx2")
  # On a machine with no Excel/spreadsheet app (e.g. WSL2), openxlsx2::xl_open() aborts inside
  # chooseExcelApp(). A failed open after a successful write must degrade to an info message, not
  # propagate. Mock the opener to reproduce that failure deterministically (headless CI never opens).
  testthat::local_mocked_bindings(
    xlb_open = function(path) cli::cli_abort("No applications (detected) available.")
  )
  p <- withr::local_tempfile(fileext = ".xlsx")
  t <- tab(fx_gss(), race, marital, pct = "row")
  testthat::expect_no_error(
    testthat::expect_message(tab_xl(t, path = p, open = TRUE), "[Cc]ould not open")
  )
  testthat::expect_true(file.exists(p))   # the write still succeeded
})


testthat::test_that("a split-off aside keeps the CELL's reading order, so the estimates pair up", {
  testthat::skip_if_not_installed("openxlsx2")
  d <- dplyr::mutate(fx_gss(), married = factor(.data$marital == "Married"))
  t <- suppressMessages(tab_reg(d, outcome = "married", predictors = "race",
                                family = "binomial", empirical = TRUE))
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  suppressMessages(tab_xl(t, path = tmp, open = FALSE, replace = TRUE))
  df  <- openxlsx2::wb_to_df(openxlsx2::wb_load(tmp), col_names = FALSE)
  hdr <- as.character(unlist(df[3, ])); unt <- as.character(unlist(df[4, ]))
  # the crude column's template is "({base}) {est}" and the model's "{est} ({base})", so the aside
  # goes BEFORE one and AFTER the other -- which is what puts the two ODDS RATIOS side by side, the
  # whole point of printing a crude column beside its model. The console reads the same way.
  i_obs <- which(hdr == "Obs_OR"); i_mod <- which(hdr == "Model_OR")
  testthat::expect_length(i_obs, 1L)
  testthat::expect_identical(i_mod, i_obs + 1L)
  # ... and each aside still says WHOSE level it is: the split keeps the role it was carved from
  testthat::expect_identical(unt[[i_obs - 1L]], "<obs%>")   # the crude level, before its estimate
  testthat::expect_identical(unt[[i_mod + 1L]], "<adj%>")   # the adjusted one, after its estimate
})


testthat::test_that("the variable-name column is thin when rotated, and fits a horizontal name", {
  testthat::skip_if_not_installed("openxlsx2")
  w_of <- function(f) {
    a <- paste(unlist(openxlsx2::wb_load(f)$worksheets[[1]]$cols_attr), collapse = " ")
    as.numeric(sub('^.*<col min="1" max="1"[^/]*width="([0-9.]+)".*$', "\\1", a))
  }
  # every name rotated (each block is tall, and no name is forced horizontal) -> the narrow width
  # the rotation buys
  t1 <- tab(fx_gss(), c(marital, partyid), relig, pct = "row")
  p1 <- withr::local_tempfile(fileext = ".xlsx")
  suppressMessages(tab_xl(t1, path = p1, open = FALSE, replace = TRUE))
  # a regression writes "Constant" horizontally (a one-row block): the column widens to fit it,
  # deterministically and within the cap
  d  <- dplyr::mutate(fx_gss(), married = factor(.data$marital == "Married"))
  t2 <- suppressMessages(tab_reg(d, outcome = "married", predictors = "race", family = "binomial"))
  p2 <- withr::local_tempfile(fileext = ".xlsx")
  suppressMessages(tab_xl(t2, path = p2, open = FALSE, replace = TRUE))
  testthat::expect_gt(w_of(p2), nchar("Constant"))
  testthat::expect_lte(w_of(p2), TX_VNAME_MAX + XL_PAD)
})


# === Phase 22f-ii: model-check plots under the model they check ====================================

testthat::test_that("tab_xl(check =) writes one picture per model, and none for a crosstab", {
  testthat::skip_if_not_installed("openxlsx2")
  testthat::skip_if_not_installed("ggplot2")
  testthat::skip_if_not_installed("gridExtra")
  d <- dplyr::mutate(fx_gss(), married = factor(.data$marital == "Married"))
  t <- suppressMessages(tab_reg(d, outcome = "married", predictors = c("race", "age"),
                                family = "binomial"))
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  suppressMessages(tab_xl(t, path = tmp, open = FALSE, replace = TRUE, check = "auto", data = d))
  # the picture reached the workbook (⚠ `unzip(list=)` lists the DIRECTORY entry too)
  png_of <- function(f) grep("^xl/media/.+[.]png$", utils::unzip(f, list = TRUE)$Name, value = TRUE)
  testthat::expect_length(png_of(tmp), 1L)
  # ... sized from its own panel grid rather than a fixed guess, and anchored BELOW the table so the
  # sheet's stacking offsets counted it
  drw <- paste(readLines(unz(tmp, "xl/drawings/drawing1.xml"), warn = FALSE), collapse = "")
  testthat::expect_match(drw, 'cx="[0-9]+" cy="[0-9]+"')
  row <- as.integer(sub('^.*<xdr:row>([0-9]+)</xdr:row>.*$', "\\1", drw))
  testthat::expect_gt(row, nrow(t))
  # a crosstab has no model to check, and takes no picture without complaining
  tmp2 <- withr::local_tempfile(fileext = ".xlsx")
  suppressMessages(tab_xl(tab(fx_gss(), race, marital, pct = "row"), path = tmp2,
                          open = FALSE, replace = TRUE, check = "auto"))
  testthat::expect_length(png_of(tmp2), 0L)
})


# ⚠ Phase 22i: TWO models, which is the case the single-model test above cannot reach.
# reg_check_plots() returns a bare gtable for one model and a list NAMED by model for two or more --
# and purrr::imap() then hands the NAME, not the index. Indexing a parallel unnamed `labs` vector by
# it ERRORED ("subscript out of bounds"), so every multi-model check export aborted.
testthat::test_that("tab_xl(check =) labels one picture per model when there are several", {
  testthat::skip_if_not_installed("openxlsx2")
  testthat::skip_if_not_installed("ggplot2")
  testthat::skip_if_not_installed("gridExtra")
  d <- dplyr::mutate(fx_gss(), married = factor(.data$marital == "Married"))
  t <- suppressMessages(tab_reg(d, outcome = c("age", "married"),
                                predictors = c("race", "tvhours")))
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  suppressMessages(tab_xl(t, path = tmp, open = FALSE, replace = TRUE, check = "auto", data = d))
  png_of <- function(f) grep("^xl/media/.+[.]png$", utils::unzip(f, list = TRUE)$Name, value = TRUE)
  testthat::expect_length(png_of(tmp), 2L)
  # each picture is captioned by the model it checks -- the NAME reg_check_plots() gave it
  sheet <- paste(readLines(unz(tmp, "xl/worksheets/sheet1.xml"), warn = FALSE), collapse = "")
  testthat::expect_match(sheet, "age", fixed = TRUE)
  testthat::expect_match(sheet, "married", fixed = TRUE)
})


# Phase 22h: Excel alone splits a composite cell into its primary and its asides, which puts an
# observed column and its model twin side by side with the SAME tag. The run key carries the role,
# so the model column keeps its own -- it used to be swallowed, silently.
testthat::test_that("an observed column and its model twin each say their unit", {
  d <- fx_gss()
  d$married <- d$marital == "Married"
  r <- suppressMessages(tab_reg(d, "married", c("race", "age"), stats = "no"))
  u <- tab_export_prep(r, backend = "xl")$tables[[1]]$col_var_header$unit
  rl <- purrr::map_chr(tab_export_prep(r, backend = "xl")$tables[[1]]$tab,
                       ~ if (is_fmt(.)) as.character(get_role(.))[1] else "")
  testthat::expect_true(all(nzchar(u[rl %in% c("emp", "model")])))
  testthat::expect_identical(u[rl == "emp"], u[rl == "model"])   # same quantity, said twice
})


test_that("xlb_dims_each splits a multi-area dims into single ranges", {
  # Phase o: xl_coalesce emits comma-joined multi-area dims (e.g. "C7:E8,F4:F8"); the older jamovi-
  # bundled openxlsx2 rejects those. xlb_dims_each fans them out to single A1:B2 ranges at the emit.
  got <- character(0)
  xlb_dims_each("A1:B2,C3:D4", function(d) got <<- c(got, d))
  expect_identical(got, c("A1:B2", "C3:D4"))

  # a single range passes through as one call
  got <- character(0)
  xlb_dims_each("C3:G10", function(d) got <<- c(got, d))
  expect_identical(got, "C3:G10")

  # NA / empty / non-scalar -> no call at all (the "nothing to style" path)
  got <- character(0)
  xlb_dims_each(NA_character_, function(d) got <<- c(got, d))
  xlb_dims_each("",            function(d) got <<- c(got, d))
  xlb_dims_each(character(0),  function(d) got <<- c(got, d))
  expect_identical(got, character(0))
})


test_that("xlb_set_cell_style / xlb_numfmt survive the OLDER openxlsx2 single-range dims validator", {
  # Reproduce the jamovi-bundled openxlsx2 (the Excel-export crash): a `dims` with a comma raises
  # "dims must be something like A1 or A1:B2.". A stub wb whose engine methods enforce that rule fails
  # WITHOUT the xlb_dims_each split and passes WITH it (each single range applied in turn).
  seen <- list(style = character(0), numfmt = character(0))
  reject_multi <- function(dims) if (grepl(",", dims, fixed = TRUE))
    stop("Invalid input: dims must be something like A1 or A1:B2.")
  wb <- new.env()
  wb$set_cell_style <- function(sheet, dims, style) { reject_multi(dims); seen$style  <<- c(seen$style,  dims) }
  wb$add_numfmt     <- function(sheet, dims, numfmt) { reject_multi(dims); seen$numfmt <<- c(seen$numfmt, dims) }

  expect_no_error(xlb_set_cell_style(wb, "s", "A1:B2,C3:D4", 7L))
  expect_no_error(xlb_numfmt(wb, "s", "C7:E8,F4:F8", "0.0%"))
  expect_identical(seen$style,  c("A1:B2", "C3:D4"))
  expect_identical(seen$numfmt, c("C7:E8", "F4:F8"))
})


test_that("xlb_na_argname resolves the exact NA formal across openxlsx2 versions (Phase q)", {
  # The older jamovi-bundled openxlsx2's add_data NA formal is `na.strings` (dot). Guessing `na_strings`
  # made the arg UNUSED -> the default #N/A error filled empty summary-stat / p-value cells. Read the real
  # name off the method's own formals: na (current) / na_strings / na.strings (oldest).
  mk <- function(f) { e <- new.env(); e$add_data <- f; e }
  expect_identical(xlb_na_argname(mk(function(name, na.strings) NULL)), "na.strings")
  expect_identical(xlb_na_argname(mk(function(x, na_strings) NULL)),    "na_strings")
  expect_identical(xlb_na_argname(mk(function(x, na) NULL)),            "na")
})


test_that("xl_materialize_data blanks NaN so Excel shows an empty cell, not #VALUE! (Phase q)", {
  # openxlsx2 renders a NaN numeric cell as an Excel error even when NA is blanked (the na arg only covers
  # NA), so an empty summary cell that computes to NaN must be coerced to NA before the write.
  x  <- fmt(n = 1L, mean = NaN, scale = "level_mean", display = "mean", digits = 1L)
  tb <- tibble::tibble(v = x)
  out <- xl_materialize_data(tb, fmt_cols = 1L, text_fmt_cols = integer(0))
  expect_true(is.na(out$v))
  expect_false(is.nan(out$v))
})


test_that("xl_clean_sheet_name leaves openxlsx2 nothing to fix (no 'illegal characters' warning)", {
  skip_if_not_installed("openxlsx2")
  # The point of the helper: openxlsx2 would silently apply this same substitution and warn.
  # Feeding it a pre-cleaned name must therefore be both silent AND a no-op.
  for (nm in c("Married vs Never: OR", "a/b", "a[b]c", "marital x race")) {
    clean <- xl_clean_sheet_name(nm)
    wb    <- openxlsx2::wb_workbook()
    expect_no_warning(wb$add_worksheet(sheet = clean))
    expect_identical(unname(utils::tail(wb$get_sheet_names(), 1)), clean)
  }
})


# === SECTION: format() and the Excel numFmt show the same number ==================================

parity_num_from_str <- function(s) {
  s <- sub("\\(.*$", "", s)          # drop " (sigma ...)" for means
  # a multiplicative cell prints its DISTANCE from the neutral: below it, the inverse ("1/2.67",
  # "\u00f72.67"). Excel keeps the raw number, so undo the inversion before comparing.
  inv <- grepl("^\\s*(1/|\u00f7)", s)
  s <- sub("^\\s*(1/|\u00f7)", "", s)   # drop the inverse marker BEFORE the digit scrub
  s <- gsub("[^0-9.+-]", "", s)      # keep digits, decimal point, sign; drop %, spaces, *, etc.
  v <- suppressWarnings(as.numeric(s))
  if (isTRUE(inv) && !is.na(v) && v != 0) 1 / v else v
}


# For every fmt cell the Excel bypass renders as a real NUMBER, assert the format-path number
# equals get_num() scaled by the Excel numfmt code (x100 iff the code is a percentage), rounded
# to get_digits(). This is the invariant the whole raw-write bypass depends on.
expect_export_parity <- function(tabs) {
  fmt_cols <- tabs[purrr::map_lgl(tabs, is_fmt)]
  for (col in fmt_cols) {
    disp <- get_display(col)
    num  <- get_num(col)
    dg   <- get_digits(col)
    code <- format(col, syntax = "excel")   # the Excel numfmt codes tab_xl writes
    strs <- format(col)                      # the text display (source of truth)
    numeric_cell <- !is.na(disp) & !is.na(num) & !is.na(code) & code != "TEXT"
    # threshold displays ("<0.01%", ">...") show a bound, not the exact stored value -> skip
    numeric_cell <- numeric_cell & !grepl("[<>]", strs)
    for (i in which(numeric_cell)) {
      got <- parity_num_from_str(strs[[i]])
      testthat::skip_if(is.na(got))                     # blank/dash cell: nothing to compare
      scale    <- if (grepl("%", code[[i]], fixed = TRUE)) 100 else 1
      expected <- num[[i]] * scale
      testthat::expect_equal(
        round(got, dg[[i]]), round(expected, dg[[i]]),
        tolerance = 1e-6,
        info = paste0("display '", disp[[i]], "' code '", code[[i]], "' cell '", strs[[i]], "'")
      )
    }
  }
}


gss <- fx_gss()


# Phase 10g: extend the oracle to diff / ctr / or displays (the ones whose Excel digits now follow
# format()'s adjusted digits). "diff" is not a default tab() display, so set it explicitly.
testthat::test_that("format path matches tab_xl path for diff display (pct)", {
  tb <- tab(gss, marital, race, pct = "row", digits = 1L) |>
    dplyr::mutate(dplyr::across(dplyr::where(is_fmt), ~ set_display(., "diff")))
  expect_export_parity(tb)
})


# Phase 10g: lock the Excel number-format codes format(syntax = "excel") emits (the fold's anchor,
# since the old inline numfmt() is deleted). One representative cell of each column.
first_codes <- function(tabs) {
  fmt_cols <- names(tabs)[purrr::map_lgl(tabs, is_fmt)]
  purrr::map_chr(fmt_cols, ~ format(tabs[[.x]], syntax = "excel")[[1]]) |>
    stats::setNames(fmt_cols)
}


# === SECTION: the Excel workbook a user opens =====================================================

xl_numfmt_codes <- function(f) {
  st <- openxlsx2::wb_load(f)$styles_mgr$styles$numFmts
  if (!length(st)) return(character(0))
  sub('".*$', "", sub('^.*formatCode="', "", st))
}


# the merged ranges, as plain A1 strings
xl_merges <- function(wb) sub('".*$', "", sub('^.*ref="', "", unlist(wb$worksheets[[1]]$mergeCells)))


# Phase 13c-v: Excel value/format + col_var spanning header.

testthat::test_that("ci = 'cell' exports the CI text (not the raw proportion)", {
  testthat::skip_if_not_installed("openxlsx2")
  t   <- tab(fx_gss(), marital, race, pct = "row", ci = "cell")
  tmp <- tempfile(fileext = ".xlsx")
  tab_xl(set_display(t, "mean_sd"), path = tmp, open = FALSE, replace = TRUE)
  df  <- openxlsx2::wb_to_df(openxlsx2::wb_load(tmp), col_names = FALSE)
  testthat::expect_true(any(grepl("\\[[0-9]+;[0-9]+\\]", as.matrix(df))))   # a "[lo;hi]" bracket
})


testthat::test_that("numeric vars export a mean + separate sd column, named by the statistic", {
  testthat::skip_if_not_installed("openxlsx2")
  t   <- tab_num(fx_gss(), race, c(age, tvhours), digits = 1L)
  tmp <- tempfile(fileext = ".xlsx")
  tab_xl(set_display(t, "mean_sd"), path = tmp, open = FALSE, replace = TRUE)
  df   <- openxlsx2::wb_to_df(openxlsx2::wb_load(tmp), col_names = FALSE)
  span <- as.character(df[2, ])   # Phase 13c-iii col_var spanning-name row (row 1 = the title)
  hdr  <- as.character(df[3, ])   # level-header row
  unt  <- as.character(df[4, ])   # the unit row
  # Phase 14d: the variable name is said ONCE, by the span; the level headers say which statistic
  # (they used to repeat it: "age" / "age_sd" under an "age" span).
  testthat::expect_true(all(c("age", "tvhours") %in% span))
  testthat::expect_equal(sum(hdr == "mean", na.rm = TRUE), 2L)
  # THE sd COLUMN IS CARVED OUT BY THE RENDER, so it has no level to name: it is named once, by its
  # unit, and its cells carry the console's own sigma folded into the number format.
  testthat::expect_equal(sum(hdr == "sd", na.rm = TRUE), 0L)
  testthat::expect_equal(sum(unt == "<sd>", na.rm = TRUE), 2L)
  testthat::expect_false(any(c("age_sd", "tvhours_sd") %in% hdr))
  testthat::expect_true(any(grepl("\u03c3", xl_numfmt_codes(tmp), fixed = TRUE)))
})


testthat::test_that("Excel gets a col_var spanning-name row + suffix-stripped level labels", {
  testthat::skip_if_not_installed("openxlsx2")
  d <- fx_gss()
  d$grp <- factor(ifelse(d$age < 40, "Young", "Other"))
  t   <- tab(d, row_vars = marital, col_vars = c(race, grp), pct = "row")
  tmp <- tempfile(fileext = ".xlsx")
  tab_xl(set_display(t, "mean_sd"), path = tmp, open = FALSE, replace = TRUE)
  df  <- openxlsx2::wb_to_df(openxlsx2::wb_load(tmp), col_names = FALSE)
  span_row <- as.character(df[2, ]); hdr_row <- as.character(df[3, ])
  testthat::expect_true(all(c("race", "grp") %in% span_row))               # spanning names row
  testthat::expect_true("Other" %in% hdr_row)                              # suffix stripped
  testthat::expect_false(any(hdr_row == "Other_race", na.rm = TRUE))
})


# Phase 22h: the unit tag names its column; it must never be what sets the column's width, and it
# must not wrap (a compound word broken mid-name reads as two tags).
testthat::test_that("the unit row is small, unwrapped, and does not widen its column", {
  testthat::skip_if_not_installed("openxlsx2")
  a <- car_arrests
  t <- tab(a, colour, released, pct = "row")
  p <- withr::local_tempfile(fileext = ".xlsx")
  suppressMessages(tab_xl(t, path = p, open = FALSE, replace = TRUE))
  wb  <- openxlsx2::wb_load(p)
  cc  <- wb$worksheets[[1]]$sheet_data$cc
  xf  <- wb$styles_mgr$styles$cellXfs
  fnt <- wb$styles_mgr$styles$fonts
  sty <- function(row) {
    k <- cc$c_s[cc$row_r == row & nzchar(cc$c_s)]
    xf[as.integer(unique(k)) + 1L]
  }
  fnt_of <- function(x) fnt[as.integer(sub('.*fontId="([0-9]+)".*', "\\1", x)) + 1L]
  urow <- as.character(min(as.integer(cc$row_r[grepl("&lt;", cc$is)])))
  ustyle <- sty(urow)
  testthat::expect_gt(length(ustyle), 0L)
  # 8pt, against a body of 10 and headers of 9 -- XL_UNIT_SIZE
  testthat::expect_true(all(grepl(paste0('sz val="', tabxplor:::XL_UNIT_SIZE, '"'),
                                  fnt_of(ustyle), fixed = TRUE)))
  # ... and it does NOT wrap, while the level header above it still does
  testthat::expect_false(any(grepl("wrapText", ustyle, fixed = TRUE)))
  testthat::expect_true(any(grepl("wrapText", sty(as.character(as.integer(urow) - 1L)),
                                  fixed = TRUE)))

  # the tag is measured on ONE line, at its own size, brackets excluded -- so a column whose figures
  # are narrow is not stretched by a long tag
  o  <- list(text_size = 10, text_size_headers = 9, wrap_rows = 35)
  rd <- tab_export_prep(t, backend = "xl")$tables[[1]]
  w  <- tabxplor:::xl_col_widths(rd$tab, rd$roles, rd$col_var_header, o, "auto")
  j  <- which(nzchar(rd$col_var_header$unit))[[1]]
  tag <- rd$col_var_header$unit[[j]]
  testthat::expect_gt(nchar(tag), 4L)
  testthat::expect_lt(w[[j]], nchar(tag) + tabxplor:::XL_PAD)   # never the tag's own full width
})


test_that("xl_coalesce covers exactly the target cells", {
  # a 5-col numeric block, all sharing one numFmt over data rows 3:10 -> a single rectangle
  cols <- rep(3:7, each = 8L); rows <- rep(3:10, times = 5L)
  expect_identical(xl_coalesce(cols, rows), "C3:G10")
})


# ---- sheet-name sanitisation -----------------------------------------------------------------

test_that("xl_clean_sheet_name replaces every Excel-illegal character with a space", {
  expect_identical(xl_clean_sheet_name("a/b"),   "a b")
  expect_identical(xl_clean_sheet_name("a?b"),   "a b")
  expect_identical(xl_clean_sheet_name("a*b"),   "a b")
  expect_identical(xl_clean_sheet_name("a:b"),   "a b")
  expect_identical(xl_clean_sheet_name("a[b]c"), "a b c")
  # the real driver: Phase 12d names OR columns "<level> vs <ref>: OR"
  expect_identical(xl_clean_sheet_name("Married vs Never: OR"), "Married vs Never  OR")
  # a legal name is untouched (so ordinary sheet titles are byte-identical)
  expect_identical(xl_clean_sheet_name("marital x race"), "marital x race")
  expect_identical(xl_clean_sheet_name(c("a/b", "ok")), c("a b", "ok"))
})


# === SECTION: format() and the Excel numFmt show the same number ==================================

parity_num_from_str <- function(s) {
  s <- sub("\\(.*$", "", s)          # drop " (sigma ...)" for means
  # a multiplicative cell prints its DISTANCE from the neutral: below it, the inverse ("1/2.67",
  # "\u00f72.67"). Excel keeps the raw number, so undo the inversion before comparing.
  inv <- grepl("^\\s*(1/|\u00f7)", s)
  s <- sub("^\\s*(1/|\u00f7)", "", s)   # drop the inverse marker BEFORE the digit scrub
  s <- gsub("[^0-9.+-]", "", s)      # keep digits, decimal point, sign; drop %, spaces, *, etc.
  v <- suppressWarnings(as.numeric(s))
  if (isTRUE(inv) && !is.na(v) && v != 0) 1 / v else v
}


# For every fmt cell the Excel bypass renders as a real NUMBER, assert the format-path number
# equals get_num() scaled by the Excel numfmt code (x100 iff the code is a percentage), rounded
# to get_digits(). This is the invariant the whole raw-write bypass depends on.
expect_export_parity <- function(tabs) {
  fmt_cols <- tabs[purrr::map_lgl(tabs, is_fmt)]
  for (col in fmt_cols) {
    disp <- get_display(col)
    num  <- get_num(col)
    dg   <- get_digits(col)
    code <- format(col, syntax = "excel")   # the Excel numfmt codes tab_xl writes
    strs <- format(col)                      # the text display (source of truth)
    numeric_cell <- !is.na(disp) & !is.na(num) & !is.na(code) & code != "TEXT"
    # threshold displays ("<0.01%", ">...") show a bound, not the exact stored value -> skip
    numeric_cell <- numeric_cell & !grepl("[<>]", strs)
    for (i in which(numeric_cell)) {
      got <- parity_num_from_str(strs[[i]])
      testthat::skip_if(is.na(got))                     # blank/dash cell: nothing to compare
      scale    <- if (grepl("%", code[[i]], fixed = TRUE)) 100 else 1
      expected <- num[[i]] * scale
      testthat::expect_equal(
        round(got, dg[[i]]), round(expected, dg[[i]]),
        tolerance = 1e-6,
        info = paste0("display '", disp[[i]], "' code '", code[[i]], "' cell '", strs[[i]], "'")
      )
    }
  }
}


gss <- fx_gss()


testthat::test_that("format path matches get_num/get_digits (tab_xl path) for counts", {
  expect_export_parity(tab(gss, marital, race, pct = "no"))
})


testthat::test_that("format path matches get_num/get_digits (tab_xl path) for row pct, 0 digits", {
  expect_export_parity(tab(gss, marital, race, pct = "row"))
})


testthat::test_that("format path matches get_num/get_digits (tab_xl path) for row pct, 1 digit", {
  expect_export_parity(tab(gss, marital, race, pct = "row", digits = 1L))
})


testthat::test_that("format path matches get_num/get_digits (tab_xl path) for means", {
  expect_export_parity(tab_num(gss, race, c(age, tvhours), digits = 1L))
})


testthat::test_that("format path matches tab_xl path for diff display (mean)", {
  tb <- tab_num(gss, race, age, digits = 1L) |>
    dplyr::mutate(dplyr::across(dplyr::where(is_fmt), ~ set_display(., "diff")))
  expect_export_parity(tb)
})


testthat::test_that("format path matches tab_xl path for a contrib table", {
  expect_export_parity(tab(gss, marital, race, color = "contrib"))
})


testthat::test_that("format path matches tab_xl path for an empirical OR table", {
  testthat::skip_if(inherits(try(tab(gss, marital, race, pct = "row", display = "{or}", ref = "first"),
                                 silent = TRUE), "try-error"))
  expect_export_parity(tab(gss, marital, race, pct = "row", display = "{or}", ref = "first"))
})


# Phase 10g: lock the Excel number-format codes format(syntax = "excel") emits (the fold's anchor,
# since the old inline numfmt() is deleted). One representative cell of each column.
first_codes <- function(tabs) {
  fmt_cols <- names(tabs)[purrr::map_lgl(tabs, is_fmt)]
  purrr::map_chr(fmt_cols, ~ format(tabs[[.x]], syntax = "excel")[[1]]) |>
    stats::setNames(fmt_cols)
}


# === SECTION: the Excel workbook a user opens =====================================================

xl_numfmt_codes <- function(f) {
  st <- openxlsx2::wb_load(f)$styles_mgr$styles$numFmts
  if (!length(st)) return(character(0))
  sub('".*$', "", sub('^.*formatCode="', "", st))
}


# the merged ranges, as plain A1 strings
xl_merges <- function(wb) sub('".*$', "", sub('^.*ref="', "", unlist(wb$worksheets[[1]]$mergeCells)))


# Phase 10g: a non-tabxplor data.frame degrades gracefully (plain sheet + message, still writes).
testthat::test_that("tab_xl degrades to a plain sheet for a non-tabxplor data.frame", {
  testthat::skip_if_not_installed("openxlsx2")
  p <- withr::local_tempfile(fileext = ".xlsx")
  testthat::expect_message(
    tab_xl(tibble::tibble(a = 1:3, b = letters[1:3]), path = p, open = FALSE),
    "skipped"
  )
  testthat::expect_true(file.exists(p))
})


# === SECTION: the label column -- merge + rotation + the title (Phase 14i) ===

testthat::test_that("tab_xl: a merged table names each row-variable once, merged and rotated", {
  testthat::skip_if_not_installed("openxlsx2")
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  suppressMessages(
    tab_xl(tab(fx_gss(), c(race, marital), relig, pct = "row"), path = tmp, open = FALSE, replace = TRUE)
  )
  wb <- openxlsx2::wb_load(tmp)
  d  <- openxlsx2::wb_to_df(wb, col_names = FALSE)

  # row 1 = title, row 2 = the col_var span, row 3 = level headers, row 4 = the UNIT row
  # (Phase 22c-ii), data from row 5
  # Phase 14i: the title names the SOURCE row_vars -- it read "levels by relig" (the merge's own
  # scaffolding column) because the prep dropped `vars$row_vars`.
  # Phase 14l: and the DEPENDENT axis leads -- this is pct="row", so the col_var comes first.
  testthat::expect_equal(as.character(d[1, 1]), "relig by race, marital")
  # one merge per block, in column A. Phase 18m: common_totrow defaults FALSE, so each block keeps its
  # OWN Total row -> race spans A5:A8 (3 data + Total) and marital spans A9:A15 (6 data + Total).
  merges <- paste(wb$worksheets[[1]]$mergeCells, collapse = " ")
  testthat::expect_match(merges, 'ref="A5:A8"', fixed = TRUE)
  testthat::expect_match(merges, 'ref="A9:A15"', fixed = TRUE)
  # the name is written once per block, not on every row (Excel keeps only a merge's top-left value,
  # so a repeat below it would be an invisible ghost the user finds again on unmerging)
  testthat::expect_equal(as.character(d[5:14, 1]),
                         c("race", rep(NA, 3), "marital", rep(NA, 5)))
  # rotated 90 degrees, and the column narrowed to match (that is what the rotation buys)
  testthat::expect_true(any(grepl('textRotation="90"', wb$styles_mgr$styles$cellXfs)))
  # the literal "row_var" header is gone
  testthat::expect_true(is.na(d[3, 1]) || !nzchar(as.character(d[3, 1])))
})


# === Phase 22c-ii: Excel cannot print a composite, so every aside gets a column ====================

testthat::test_that("tab_xl: a composite cell's aside becomes its own column, headed by the token", {
  testthat::skip_if_not_installed("openxlsx2")
  t   <- tab(fx_gss(), race, marital, pct = "row", ref = 1, display = "{pct} ({or})")
  tmp <- withr::local_tempfile(fileext = ".xlsx")
  suppressMessages(tab_xl(t, path = tmp, open = FALSE, replace = TRUE))
  d <- openxlsx2::wb_to_df(openxlsx2::wb_load(tmp), col_names = FALSE)
  # row 3 = the level headers, row 4 = the unit row (Phase 22c-ii)
  hdr <- as.character(unlist(d[3, ]))
  unt <- as.character(unlist(d[4, ]))
  # AN ASIDE COLUMN IS NAMED BY ITS UNIT, not by a level header: the render carved it out, so it has
  # no level of the column variable to name.
  testthat::expect_false("OR" %in% hdr)
  testthat::expect_identical(sum(unt == "<OR>", na.rm = TRUE), sum(hdr == "Married", na.rm = TRUE) *
                               length(levels(droplevels(fx_gss()$marital))))
  testthat::expect_true("<row%>" %in% unt)
  # the aside column holds ONE field: no re-composed "1 (39%)" on the reference row
  or_col <- which(unt == "<OR>")[[1]]
  testthat::expect_false(any(grepl("(", as.character(d[5:7, or_col]), fixed = TRUE)))
  # ... and it keeps the console's own brackets, folded into the number format rather than pasted
  # into the value, so the cell is still a number
  testthat::expect_true(any(grepl("\\(", xl_numfmt_codes(tmp), fixed = TRUE)))
  testthat::expect_true(is.numeric(suppressWarnings(as.numeric(d[[or_col]][5]))))
})


# === SECTION: format() and the Excel numFmt show the same number ==================================

parity_num_from_str <- function(s) {
  s <- sub("\\(.*$", "", s)          # drop " (sigma ...)" for means
  # a multiplicative cell prints its DISTANCE from the neutral: below it, the inverse ("1/2.67",
  # "\u00f72.67"). Excel keeps the raw number, so undo the inversion before comparing.
  inv <- grepl("^\\s*(1/|\u00f7)", s)
  s <- sub("^\\s*(1/|\u00f7)", "", s)   # drop the inverse marker BEFORE the digit scrub
  s <- gsub("[^0-9.+-]", "", s)      # keep digits, decimal point, sign; drop %, spaces, *, etc.
  v <- suppressWarnings(as.numeric(s))
  if (isTRUE(inv) && !is.na(v) && v != 0) 1 / v else v
}


# For every fmt cell the Excel bypass renders as a real NUMBER, assert the format-path number
# equals get_num() scaled by the Excel numfmt code (x100 iff the code is a percentage), rounded
# to get_digits(). This is the invariant the whole raw-write bypass depends on.
expect_export_parity <- function(tabs) {
  fmt_cols <- tabs[purrr::map_lgl(tabs, is_fmt)]
  for (col in fmt_cols) {
    disp <- get_display(col)
    num  <- get_num(col)
    dg   <- get_digits(col)
    code <- format(col, syntax = "excel")   # the Excel numfmt codes tab_xl writes
    strs <- format(col)                      # the text display (source of truth)
    numeric_cell <- !is.na(disp) & !is.na(num) & !is.na(code) & code != "TEXT"
    # threshold displays ("<0.01%", ">...") show a bound, not the exact stored value -> skip
    numeric_cell <- numeric_cell & !grepl("[<>]", strs)
    for (i in which(numeric_cell)) {
      got <- parity_num_from_str(strs[[i]])
      testthat::skip_if(is.na(got))                     # blank/dash cell: nothing to compare
      scale    <- if (grepl("%", code[[i]], fixed = TRUE)) 100 else 1
      expected <- num[[i]] * scale
      testthat::expect_equal(
        round(got, dg[[i]]), round(expected, dg[[i]]),
        tolerance = 1e-6,
        info = paste0("display '", disp[[i]], "' code '", code[[i]], "' cell '", strs[[i]], "'")
      )
    }
  }
}


gss <- fx_gss()


# Phase 10g: lock the Excel number-format codes format(syntax = "excel") emits (the fold's anchor,
# since the old inline numfmt() is deleted). One representative cell of each column.
first_codes <- function(tabs) {
  fmt_cols <- names(tabs)[purrr::map_lgl(tabs, is_fmt)]
  purrr::map_chr(fmt_cols, ~ format(tabs[[.x]], syntax = "excel")[[1]]) |>
    stats::setNames(fmt_cols)
}
