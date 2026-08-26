
# === SECTION: the markdown exporter ===============================================================

withr::local_options(lifecycle_verbosity = "quiet", .local_envir = testthat::teardown_env())



gss <- fx_gss()


# === SECTION: Basic output ====================================================

tabs <- tab(gss, race, marital, pct = "row")

md <- tab_md(tabs, print = FALSE)


testthat::test_that("tab_md header contains column names from data", {

  # Check that some marital levels appear in the header
  testthat::expect_true(grepl("Married", md))
  testthat::expect_true(grepl("Divorced", md))
  # Check row_var name
  testthat::expect_true(grepl("race", md))
})


testthat::test_that("tab_md has correct number of data rows", {
  lines <- strsplit(md, "\n")[[1]]

  # Count pipe-delimited lines (excluding header and separator)
  pipe_lines <- lines[grepl("^\\|", lines)]
  # At least: 1 header + 1 separator + nrow(tabs) data lines
  testthat::expect_gte(length(pipe_lines), nrow(tabs) + 2)
})


# === SECTION: Bold references =================================================

md_bold <- tab_md(tabs, bold_references = TRUE, print = FALSE)


testthat::test_that("bold_references=TRUE produces ** markers", {
  testthat::expect_true(grepl("\\*\\*", md_bold))
})


md_no <- tab_md(tabs, bold_references = FALSE, print = FALSE)


testthat::test_that("bold_references=FALSE produces no ** markers", {
  testthat::expect_false(grepl("\\*\\*", md_no))
})


# === SECTION: Subtext =========================================================

testthat::test_that("subtext=TRUE does not error even when subtext is empty", {
  md <- tab_md(tabs, subtext = TRUE, print = FALSE)
  testthat::expect_type(md, "character")
})


testthat::test_that("subtext=FALSE produces output no longer than subtext=TRUE", {
  md_with    <- tab_md(tabs, subtext = TRUE, print = FALSE)
  md_without <- tab_md(tabs, subtext = FALSE, print = FALSE)
  testthat::expect_lte(nchar(md_without), nchar(md_with))
})


# === SECTION: Grouped tables ==================================================
gss_sub <- gss |> dplyr::filter(year %in% c(2000, 2014))

tabs_sub <- tab(gss_sub, race, marital, year, pct = "row")


# === SECTION: List of tables ==================================================

testthat::test_that("tab_md merges a list of tables sharing col_vars (no tab_vars)", {
  t1 <- tabs
  t2 <- tab(gss, relig, marital, pct = "row")
  md <- tab_md(list(t1, t2), print = FALSE)
  testthat::expect_type(md, "character")
  testthat::expect_gt(nchar(md), 0)
})


# Phase 10d list method: a NON-mergeable list (different col_vars, or tab_vars) is no longer an
# error -- each table renders one-after-another, joined by a blank line.
testthat::test_that("tab_md renders a list with different col_vars one-after-another", {
  t1 <- tab(gss, race, marital, pct = "row")
  t2 <- tab(gss, race, relig, pct = "row")
  md <- tab_md(list(t1, t2), print = FALSE)
  testthat::expect_type(md, "character")
  # both tables present: a marital level (table 1) AND a relig level (table 2)
  testthat::expect_true(grepl("Divorced", md))              # marital col header
  testthat::expect_true(grepl("Protestant|Catholic", md))  # relig col header
  # rendered as two separate tables (blank line between)
  testthat::expect_true(grepl("\n\n", md))
})


testthat::test_that("tab_md renders a list of tab_vars tables (each keeps its sub-tables)", {
  t1 <- tab(gss, race, marital, year, pct = "row")
  t2 <- tab(gss, relig, marital, year, pct = "row")
  md <- tab_md(list(t1, t2), print = FALSE)
  testthat::expect_type(md, "character")
  # the shared row_var of the tab() list (2 row_vars + tab_var) also renders both tables
  r  <- tab(gss, c(race, relig), marital, year, pct = "row")
  md_r <- tab_md(r, print = FALSE)
  testthat::expect_type(md_r, "character")
  testthat::expect_gt(nchar(md_r), nchar(tab_md(t1, print = FALSE)))
})


# === SECTION: Display modes ===================================================

testthat::test_that("tab_md works with diff display", {
  tabs <- tab(gss, race, marital, pct = "row", color = "diff") |>
    dplyr::mutate(dplyr::across(where(is_fmt), ~ set_display(., "diff")))
  md <- tab_md(tabs, print = FALSE)
  testthat::expect_type(md, "character")
  testthat::expect_gt(nchar(md), 0)
})


testthat::test_that("tab_md works with counts display", {
  tabs <- tab(gss, race, marital)
  md <- tab_md(tabs, print = FALSE)
  testthat::expect_type(md, "character")
  testthat::expect_gt(nchar(md), 0)
})


# Phase 20h: the prepared starwars fixture, built ONCE at top level -- where the file-level
# lifecycle line above actually bites (testthat re-enables the warning inside every
# test_that()). It was written verbatim in each block below.
sw_prepared <- dplyr::starwars |>
  tab_prepare("sex", "hair_color", "eye_color", "mass", "gender",
              other_if_less_than = 5)


testthat::test_that("tab_md works with numeric tables (tab_num)", {
  sw <- sw_prepared
  tabs <- tab_num(sw, sex, height, na = "drop")
  md <- tab_md(tabs, print = FALSE)
  testthat::expect_type(md, "character")
  testthat::expect_gt(nchar(md), 0)
})


# === SECTION: wrap_rows =======================================================

testthat::test_that("wrap_rows truncates long row labels", {
  tabs <- tab(gss, relig, marital, pct = "row")
  md_short <- tab_md(tabs, wrap_rows = 10, bold_references = FALSE,
                     print = FALSE)
  md_long  <- tab_md(tabs, wrap_rows = 200, bold_references = FALSE,
                     print = FALSE)
  # Short wrap should produce shorter or equal output
  testthat::expect_lte(nchar(md_short), nchar(md_long))
})


# === SECTION: colour spans (Phase 10f) ========================================

tabs_col <- tab(gss, race, marital, pct = "row", color = "diff")


testthat::test_that("uncoloured table never gets spans (byte-identical default)", {
  md_default <- tab_md(tabs, print = FALSE)                      # color = TRUE default
  testthat::expect_false(grepl("]{.", md_default, fixed = TRUE))
})


testthat::test_that("two-channel colour emits a background slot class", {
  # a low ratio break so at least one cell qualifies for the background (ratio) channel
  set_color_breaks(pct_ratio = c(1.2))
  withr::defer(options("tabxplor.color_breaks" = default_color_scales()))
  t2 <- tab(gss, race, marital, pct = "row",
            color = c("diff", "ratio"))               # positional: diff text + ratio background
  md <- tab_md(t2, print = FALSE)
  testthat::expect_true(grepl("[{ ]\\.(o|u)[1-4][} ]", md))     # Phase 13d: bg slots, was ".bg<break>"
})


testthat::test_that("coloured tables keep numbers aligned (equal pipe-line widths)", {
  md    <- tab_md(tabs_col, print = FALSE)
  lines <- strsplit(md, "\n")[[1]]
  pl    <- lines[grepl("^[|]", lines)]
  # Phase 13c-iii: exclude the col_var spanning-name header row (a visual title that merges cells);
  # the pipe GRID (level header + separator + data) stays identical width AND identical pipe count.
  sep  <- which(grepl("^[|][-: |]+$", pl))[1]
  grid <- pl[seq.int(sep - 1L, length(pl))]
  testthat::expect_length(unique(nchar(grid)), 1L)
  pipes <- purrr::map_int(grid, ~ lengths(regmatches(., gregexpr("[|]", ., perl = TRUE))))
  testthat::expect_length(unique(pipes), 1L)
})


testthat::test_that("the deprecated `title` arg still feeds `caption`", {
  lifecycle::expect_deprecated(
    md <- tab_md(tabs_col, title = "My caption", print = FALSE)
  )
  testthat::expect_true(grepl("\n: My caption", md, fixed = TRUE))
})


# === SECTION: tab_css(format = "md") ==========================================

testthat::test_that("tab_css(format = \"md\") emits the slot colour rules, chrome-free", {
  css <- tab_css(format = "md")
  testthat::expect_type(css, "character")
  testthat::expect_true(grepl("\\.p1,", css))                             # a text-slot rule (bare part)
  testthat::expect_true(grepl("\\.o1,", css))                             # a bg-slot rule (bare part)
  # The md contract: colour classes ONLY, no table chrome. The bare selector stays first (the user
  # maps it in their own editor CSS); the scoped `.tabxplor-tab .p1` twin rides along since the
  # Bootstrap-host fix (a Quarto site is Bootstrap too, and styled tab_md wraps tables in the
  # `::: {.tabxplor-tab}` div) -- so assert the ABSENCE OF CHROME, not of the class name.
  testthat::expect_false(grepl("\\.tabxplor-tab\\{", css))                # no table-level chrome block
  testthat::expect_false(grepl("tx-", css, fixed = TRUE))                 # no role/geometry rules
  testthat::expect_false(grepl("border", css, fixed = TRUE))              # no border chrome
})


testthat::test_that("format = 'md' + theme = 'auto' adds the media block; 'light' does not", {
  testthat::expect_true(grepl("@media (prefers-color-scheme: dark)",
                              tab_css(format = "md", theme = "auto"), fixed = TRUE))
  # z11: `@media print` (the publication palette) now rides every stylesheet; what "light" must not
  # emit is the AUTO cascade's colour-scheme query.
  testthat::expect_false(grepl("@media (prefers-color-scheme", tab_css(format = "md", theme = "light"),
                               fixed = TRUE))
})


testthat::test_that("the stylesheet is TABLE-INDEPENDENT", {
  # Phase 13d: this is the property that replaces per-table CSS and scoping. Two tables with DIFFERENT
  # color_breaks -- which used to produce conflicting `.p20` rules -- now share one stylesheet, because
  # a class names a palette slot, not a threshold. If this ever fails, collisions are back.
  withr::local_options(list(tabxplor.color_breaks = default_color_scales()))
  a <- tab_css(format = "md", theme = "auto")
  set_color_breaks(pct_diff = c(1, 2, 3, 4))
  b <- tab_css(format = "md", theme = "auto")
  testthat::expect_identical(a, b)
  # ... and it does not depend on any table at all: the stylesheet cannot be table-specific even
  # by accident, because tab_css() takes no table.
  testthat::expect_false("tabs" %in% names(formals(tab_css)))
  testthat::expect_false("x" %in% names(formals(tab_css)))
})


testthat::test_that("tab_md(css = TRUE) embeds a <style> block", {
  md <- tab_md(tabs_col, css = TRUE, print = FALSE)
  testthat::expect_true(grepl("^<style>", md))
  testthat::expect_true(grepl("</style>", md, fixed = TRUE))
})


testthat::test_that("tab_css(format = \"md\") writes to a file when file is given", {
  tmp <- tempfile(fileext = ".css")
  on.exit(unlink(tmp))
  out <- tab_css(format = "md", file = tmp)
  testthat::expect_true(file.exists(tmp))
  testthat::expect_gt(length(readLines(tmp)), 0)
})



# === SECTION: Phase 14f -- the output must be VALID PANDOC ==============================

# THE test this file was missing. Every assertion here was green while pandoc rejected the table
# outright: tabxplor emitted the col_var name as a SECOND HEADER ROW, which pipe tables do not have,
# so pandoc gave up and rendered a line-block plus a paragraph of pipes. Nothing looked at the render.
md_pandoc_html <- function(md) {
  f <- withr::local_tempfile(fileext = ".md")
  writeLines(md, f)
  out <- suppressWarnings(system2("pandoc", c(shQuote(f), "-t", "html"),
                                  stdout = TRUE, stderr = FALSE))
  paste(out, collapse = "\n")
}


testthat::test_that("the delimiter row's spacer column is dashes, not a blank", {
  # "| |" is not a valid pandoc delimiter cell -- it is what invalidated multi-col_var tables.
  md <- tab_md(tab(gss, marital, c(race, relig), pct = "row"), print = FALSE, color = FALSE,
               css = FALSE)  # css = FALSE: line 2 is the delimiter, not a stylesheet line
  sep <- strsplit(md, "\n")[[1]][2]
  testthat::expect_match(sep, "|-|", fixed = TRUE)
  testthat::expect_no_match(sep, "| |", fixed = TRUE)
})


testthat::test_that("var_names drops the col_var name row", {
  t <- tab(gss, marital, race, pct = "row")
  testthat::expect_match(tab_md(t, print = FALSE, color = FALSE), "[*]race[*]", perl = TRUE)
  for (vn in c("rows", "none")) {
    testthat::expect_no_match(tab_md(t, print = FALSE, color = FALSE, var_names = vn),
                              "[*]race[*]", perl = TRUE)
  }
  testthat::expect_match(tab_md(t, print = FALSE, color = FALSE, var_names = "cols"),
                         "[*]race[*]", perl = TRUE)
})


testthat::test_that("the deprecated `col_var_names` still drops the col_var name row", {
  # Phase 14i: md-only `col_var_names` generalised to the shared `var_names`. FALSE drops the COL side
  # of whatever `var_names` asks for, so the two compose instead of fighting.
  t <- tab(gss, marital, race, pct = "row")
  lifecycle::expect_deprecated(
    md <- tab_md(t, print = FALSE, color = FALSE, col_var_names = FALSE)
  )
  testthat::expect_no_match(md, "[*]race[*]", perl = TRUE)
  # ... and composes with an explicit var_names: "cols" + col_var_names = FALSE -> nothing left
  suppressWarnings(
    md2 <- tab_md(t, print = FALSE, color = FALSE, var_names = "cols", col_var_names = FALSE)
  )
  testthat::expect_no_match(md2, "[*]race[*]", perl = TRUE)
  # TRUE is a no-op (still deprecated, but changes nothing)
  suppressWarnings(md3 <- tab_md(t, print = FALSE, color = FALSE, col_var_names = TRUE))
  testthat::expect_match(md3, "[*]race[*]", perl = TRUE)
})


testthat::test_that("coloured cells align on the NUMBER, with no padding inside the bracket", {
  # the bold rows' `**` used to inflate num_width, padding every coloured cell INSIDE its span
  # ("[    38%]{.p2}") -- spaces pandoc discards, and which push the number out of line.
  md <- tab_md(tab(gss, marital, race, pct = "row", color = "diff"), print = FALSE)
  testthat::expect_no_match(md, "\\[ +[0-9]", perl = TRUE)
  # the numbers of one column share a right edge in the raw text
  lines <- strsplit(md, "\n")[[1]]
  data  <- lines[grepl("^[|] [A-Z]", lines)]
  pos   <- regexpr("[0-9]+%\\]?\\{?[^|]*[|]", data)
  testthat::expect_true(length(unique(vapply(data, function(l) {
    m <- gregexpr("[0-9]+%", l)[[1]]; m[1] + attr(m, "match.length")[1]
  }, numeric(1)))) == 1L)
})


testthat::test_that("css = TRUE wraps the table in a fenced div the stylesheet can reach", {
  md <- tab_md(tab(gss, marital, race, pct = "row", color = "diff"), print = FALSE, css = TRUE)
  testthat::expect_match(md, "::: {.tabxplor-tab}", fixed = TRUE)
  testthat::expect_match(md, "<style>", fixed = TRUE)
  testthat::skip_if(Sys.which("pandoc") == "", "pandoc not on PATH")
  h <- md_pandoc_html(md)
  # pandoc emits a BARE <table> for a pipe table; the div is the only hook tab_css() can style
  testthat::expect_match(h, '<div class="tabxplor-tab">', fixed = TRUE)
  testthat::expect_match(h, "<table", fixed = TRUE)
})


testthat::test_that("a kept tab_var is named once too, but PLAIN (its values are levels)", {
  t  <- tab(gss, marital, race, year, pct = "row") |> dplyr::filter(year %in% c(2000, 2006))
  md <- tab_md(t, print = FALSE, color = FALSE)
  lines <- strsplit(md, "\n")[[1]]
  testthat::expect_length(grep("| 2000 ", lines, fixed = TRUE), 1L)
  testthat::expect_length(grep("| 2006 ", lines, fixed = TRUE), 1L)
  testthat::expect_no_match(md, "*2000*", fixed = TRUE)   # a level is not a variable name
  testthat::expect_no_match(md, "**2000**", fixed = TRUE)
})


testthat::test_that("the label column's de-bolding does not desync the column width", {
  # md_extra() / the bold +4 charge markup width per column: leave them charging `**` the body no
  # longer writes and the label column over-pads, so the pipes stop lining up.
  md    <- tab_md(tab(gss, c(race, marital), relig, pct = "row"), print = FALSE, color = FALSE)
  lines <- grep("^[|]", strsplit(md, "\n")[[1]], value = TRUE)
  testthat::expect_length(unique(nchar(lines)), 1L)
  testthat::expect_length(unique(purrr::map_int(gregexpr("|", lines, fixed = TRUE), length)), 1L)
})


# === SECTION: Phase 14m-iii -- taming the host in rendered markdown ============
# Findings 9/10: in a Bootstrap/Quarto host the host draws a black line under every row and md's dash
# separators / spacer columns leak as ugly cells. A STYLED table (coloured, or css = TRUE) now carries
# the fenced div + blank-row separators tab_css() collapses to 1px rules; a plain table stays byte-clean.

# A fully-blank pipe row: only pipes, spaces and the col_var spacer. It is what pandoc renders as a
# <tr> of :empty <td>s, the hook the css collapse keys on.
md_blank_rows <- function(md) {
  lines <- strsplit(md, "\n")[[1]]
  body  <- grep("^[|]", lines, value = TRUE)              # pipe rows only (skip legend / :::)
  body[grepl("^[| ]+$", body) & !grepl("[-:]", body)]     # all-space cells, not the delimiter
}


testthat::test_that("a coloured table carries the fenced div even with css = FALSE (14m-iii decouple)", {
  md <- tab_md(tab(gss, marital, race, pct = "row", color = "diff"), print = FALSE, css = FALSE)
  testthat::expect_match(md, "::: {.tabxplor-tab}", fixed = TRUE)
  testthat::expect_no_match(md, "<style>", fixed = TRUE)                             # no sheet, just the hook
  # ... a PLAIN uncoloured table (css = FALSE) is byte-clean: no div, no scaffold
  plain <- tab_md(tab(gss, marital, race, pct = "row"), print = FALSE, color = FALSE, css = FALSE)
  testthat::expect_no_match(plain, ":::", fixed = TRUE)
})


testthat::test_that("the styled path draws blank-row separators; the plain path keeps dashes", {
  col <- tab_md(tab(gss, marital, race, pct = "row", color = "diff"), print = FALSE)
  # one blank row (the col_var-name underline; a single row_var has no sub-table boundary)
  testthat::expect_length(md_blank_rows(col), 1L)
  # a coloured tab_vars table adds one blank row per sub-table boundary on top of the name underline
  t_tv <- tab(gss, marital, race, year, pct = "row", color = "diff") |>
    dplyr::filter(year %in% c(2000, 2006))
  testthat::expect_gte(length(md_blank_rows(tab_md(t_tv, print = FALSE))), 2L)
  # the PLAIN counterpart keeps DASH separator rows (byte-clean GFM), not blank ones
  t_tv_plain <- tab(gss, marital, race, year, pct = "row") |>
    dplyr::filter(year %in% c(2000, 2006))
  plain <- strsplit(tab_md(t_tv_plain, print = FALSE, color = FALSE, css = FALSE), "\n")[[1]]
  testthat::expect_true(any(grepl("^[|] +-+", plain)))          # a dash separator row survives
})


testthat::test_that("blank/spacer cells stay ASCII -- a figure space would break the :empty hook", {
  # THE decisive 14m-ii coupling: pandoc renders a figure-space cell as `<td> </td>` (NOT :empty), so
  # the whole css collapse dies. The figure-space swap must stay INSIDE a value; blank rows are ASCII.
  fig <- "\u2007"   # U+2007 FIGURE SPACE (\uXXXX per the non-ascii rule)
  for (t in list(tab(gss, marital, race, pct = "row", color = "diff"),
                 tab(gss, marital, c(race, relig), pct = "row", color = "diff"))) {
    md <- tab_md(t, print = FALSE)
    testthat::expect_false(any(grepl(fig, md_blank_rows(md), fixed = TRUE)))
  }
})


testthat::test_that("a blank separator row renders through pandoc as a <tr> of empty <td>s", {
  testthat::skip_if(Sys.which("pandoc") == "", "pandoc not on PATH")
  md <- tab_md(tab(gss, marital, race, pct = "row", color = "diff"), print = FALSE, css = TRUE)
  h  <- md_pandoc_html(md)                                       # defined earlier in this file
  # an empty <td> (any alignment style) exists -> the blank row survived as :empty cells
  testthat::expect_match(h, "<td[^>]*></td>", perl = TRUE)
  testthat::expect_match(h, '<div class="tabxplor-tab">', fixed = TRUE)
})


# ⚠ Phase 22i: tab_resolve_tables() passes a user's list through untouched, so a NAMED list made
# imap()'s `i` the NAME -- and `i == 1` is silently FALSE for a string (no error), so the caption
# was dropped from every table. Same trap as xl_check_images(); the position is what is meant.
test_that("a caption reaches the first table of a NAMED list", {
  t1 <- tab(fx_gss(), race, marital, pct = "row")
  t2 <- tab(fx_gss(), race, relig,   pct = "row")
  for (tabs in list(list(t1, t2), list(a = t1, b = t2))) {
    md <- suppressMessages(tab_md(tabs, caption = "MY CAPTION"))
    expect_match(paste(md, collapse = "\n"), "MY CAPTION", fixed = TRUE)
  }
  # and only the FIRST one carries it
  md <- suppressMessages(tab_md(list(a = t1, b = t2), caption = "MY CAPTION"))
  expect_equal(lengths(regmatches(paste(md, collapse = "\n"),
                                  gregexpr("MY CAPTION", paste(md, collapse = "\n"),
                                           fixed = TRUE))), 1L)
})
