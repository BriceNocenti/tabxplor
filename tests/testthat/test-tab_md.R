# PURPOSE: Test tab_md() markdown export for structural correctness.
# ROLE: Ensures markdown output is well-formed and options work as documented.
# KEY CONSTRAINTS:
#   - Must run via test_check("tabxplor"), never in isolation.
#   - Uses print = FALSE to capture the returned string for assertions.

gss <- forcats::gss_cat

# === SECTION: Basic output ====================================================

tabs <- tab(gss, race, marital, pct = "row")
md <- tab_md(tabs, print = FALSE)

testthat::test_that("tab_md returns a character string with print=FALSE", {
  testthat::expect_type(md, "character")
  testthat::expect_length(md, 1)
  testthat::expect_gt(nchar(md), 0)
})

testthat::test_that("tab_md output contains pipe-delimited markdown table", {
  lines <- strsplit(md, "\n")[[1]]

  # Must have pipe-delimited lines
  pipe_lines <- lines[grepl("\\|", lines)]
  testthat::expect_gt(length(pipe_lines), 2)  # header + separator + data

  # Must have a separator line with dashes and colons
  separator <- lines[grepl("^\\|[-:| ]+\\|$", lines)]
  testthat::expect_gte(length(separator), 1)
})

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

testthat::test_that("tab_md works with tab_vars (grouped tables)", {
  md <- tab_md(tabs_sub, print = FALSE)

  testthat::expect_type(md, "character")
  testthat::expect_gt(nchar(md), 0)
  # Should contain separator lines between groups
  lines <- strsplit(md, "\n")[[1]]
  testthat::expect_gt(length(lines), nrow(tabs_sub) + 2)
})

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

# === SECTION: File output =====================================================

testthat::test_that("tab_md writes to file when file argument provided", {
  
  tmp <- tempfile(fileext = ".md")
  on.exit(unlink(tmp))

  tab_md(tabs, file = tmp, print = FALSE)
  testthat::expect_true(file.exists(tmp))
  content <- readLines(tmp)
  testthat::expect_gt(length(content), 0)
  testthat::expect_true(any(grepl("\\|", content)))
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

testthat::test_that("tab_md works with numeric tables (tab_num)", {
  sw <- dplyr::starwars |>
    tab_prepare("sex", "hair_color", "eye_color", "mass", "gender",
                other_if_less_than = 5)
  tabs <- tab_num(sw, sex, height, na = "drop")
  md <- tab_md(tabs, print = FALSE)
  testthat::expect_type(md, "character")
  testthat::expect_gt(nchar(md), 0)
})

# === SECTION: Alignment and structure =========================================

testthat::test_that("tab_md alignment separator uses : for right/left alignment", {
  
  md <- tab_md(tabs, print = FALSE)
  lines <- strsplit(md, "\n")[[1]]

  # Find separator line
  sep_idx <- which(grepl("^\\|[-:| ]+\\|$", lines))
  testthat::expect_length(sep_idx, 1)

  sep_line <- lines[sep_idx]
  # Should contain right-alignment markers (---:) for numeric columns
  testthat::expect_true(grepl("-:", sep_line))
  # Should contain left-alignment markers (:-) for text columns
  testthat::expect_true(grepl(":-", sep_line))
})

testthat::test_that("tab_md pipe grid lines have same number of pipes", {

  md <- tab_md(tabs, bold_references = FALSE, print = FALSE)
  lines <- strsplit(md, "\n")[[1]]
  pipe_lines <- lines[grepl("^\\|", lines)]

  # Phase 13c-iii: the col_var spanning-NAME header row (above the level header) is a visual title that
  # merges cells, so it has fewer pipes by design. Check the pipe GRID -- the level header + alignment
  # separator + data rows (from the separator's preceding line onward) -- which stays equal-width.
  sep  <- which(grepl("^\\|[-: |]+$", pipe_lines))[1]
  grid <- pipe_lines[seq.int(sep - 1L, length(pipe_lines))]
  pipe_counts <- purrr::map_int(grid, ~ stringr::str_count(., "\\|"))
  testthat::expect_true(length(unique(pipe_counts)) == 1,
                        label = "all grid pipe lines have same number of pipes")
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

testthat::test_that("coloured table emits pandoc bracketed spans with slot classes", {
  md <- tab_md(tabs_col, print = FALSE)
  testthat::expect_true(grepl("]{.", md, fixed = TRUE))          # a span exists
  testthat::expect_true(grepl("[{ ]\\.(p|m)[1-4][} ]", md))      # a text-channel slot class
  # Phase 13d: classes name a palette SLOT (1-4 over / 1-4 under), never a break value, so the
  # stylesheet is table-independent. Nothing may reintroduce a break-derived name.
  testthat::expect_false(grepl("[{ ]\\.(p|m)[0-9]{2}", md))      # no .p20 / .m10
  testthat::expect_false(grepl("[{ ]\\.(sd|x|d|b)", md))         # no .sd0_2 / .x2 / .d2 / .b1
  # Phase 13d: an uncoloured cell gets NO span (the `.n` neutral is gone) -- alignment is padding's job.
  testthat::expect_false(grepl("[{ ]\\.n[}]", md))
})

testthat::test_that("color = FALSE yields plain markdown (no spans)", {
  md <- tab_md(tabs_col, color = FALSE, print = FALSE)
  testthat::expect_false(grepl("]{.", md, fixed = TRUE))
})

testthat::test_that("uncoloured table never gets spans (byte-identical default)", {
  md_default <- tab_md(tabs, print = FALSE)                      # color = TRUE default
  testthat::expect_false(grepl("]{.", md_default, fixed = TRUE))
})

testthat::test_that("numbers stay aligned when coloured and uncoloured cells mix in a column", {
  # Phase 13d: dropping the `.n` neutral removed the span that used to give EVERY cell of a coloured
  # column the same "[num]{...}" scaffold. An uncoloured cell now uses a bracket-free geometry ("  " in
  # place of " [") so the number's right edge lands at the same offset. The sibling pipe-width test
  # cannot see this: padding to total_width keeps the pipes aligned even if the numbers drift.
  md    <- tab_md(tabs_col, print = FALSE)
  lines <- strsplit(md, "\n")[[1]]
  # the alignment row. NOTE the character class excludes spaces on purpose: the Phase 13c-iii col_var
  # spanning-name row is `|        |   marital   |`, which a `[-: ]+` class matches -- picking it up
  # here makes `right` all-FALSE and the whole test vacuous.
  sep   <- which(grepl("^[|][-:]+[|]", lines))[1]
  testthat::expect_false(is.na(sep))
  split_row <- function(l) strsplit(l, "|", fixed = TRUE)[[1]][-1]
  right <- grepl("-:$", trimws(split_row(lines[sep])))      # right-aligned = the fmt columns
  data  <- lines[seq(sep + 1L, length(lines))]              # data rows only (headers are left-aligned)
  data  <- data[grepl("^[|]", data)]
  cells <- lapply(data, split_row)

  for (j in which(right)) {
    col <- vapply(cells, function(x) if (length(x) >= j) x[[j]] else "", character(1))
    col <- col[nzchar(trimws(col))]
    if (length(col) < 2L) next
    # where the number ENDS inside the cell: just before "]" when wrapped in a span, else at the last
    # non-space character. A coloured and an uncoloured cell of the same column must agree.
    ends <- vapply(col, function(cell) {
      br <- regexpr("]{.", cell, fixed = TRUE)
      if (br > 0) br - 1L else nchar(sub("[ ]+$", "", cell))
    }, integer(1), USE.NAMES = FALSE)
    if (length(unique(ends)) > 1L) {
      testthat::fail(paste0("column ", j, " numbers misaligned at offsets ",
                            paste(unique(ends), collapse = "/"), ":\n",
                            paste0("[", col, "]", collapse = "\n")))
    }
  }
  testthat::succeed()
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
  pipes <- purrr::map_int(grid, ~ stringr::str_count(., "[|]"))
  testthat::expect_length(unique(pipes), 1L)
})

testthat::test_that("caption renders a pandoc caption line", {
  md <- tab_md(tabs_col, caption = "My caption", print = FALSE)
  testthat::expect_true(grepl("\n: My caption", md, fixed = TRUE))
})

testthat::test_that("the deprecated `title` arg still feeds `caption`", {
  lifecycle::expect_deprecated(
    md <- tab_md(tabs_col, title = "My caption", print = FALSE)
  )
  testthat::expect_true(grepl("\n: My caption", md, fixed = TRUE))
})

# === SECTION: tab_md_css() / tab_css() ========================================

testthat::test_that("tab_md_css emits the slot colour rules, bare and chrome-free", {
  css <- tab_md_css()
  testthat::expect_type(css, "character")
  testthat::expect_true(grepl("\\.p1\\{color:", css))                     # a text-slot rule
  testthat::expect_true(grepl("\\.o1\\{background-color:", css))          # a bg-slot rule
  # The md contract: BARE selectors the user maps in their own editor CSS -- no table chrome.
  testthat::expect_false(grepl("tabxplor-tab", css, fixed = TRUE))
})

testthat::test_that("tab_md_css theme = 'auto' adds the media block; 'light' does not", {
  testthat::expect_true(grepl("@media (prefers-color-scheme: dark)",
                              tab_md_css(theme = "auto"), fixed = TRUE))
  testthat::expect_false(grepl("@media", tab_md_css(theme = "light"), fixed = TRUE))
})

testthat::test_that("the stylesheet is TABLE-INDEPENDENT", {
  # Phase 13d: this is the property that replaces per-table CSS and scoping. Two tables with DIFFERENT
  # color_breaks -- which used to produce conflicting `.p20` rules -- now share one stylesheet, because
  # a class names a palette slot, not a threshold. If this ever fails, collisions are back.
  withr::local_options(list(tabxplor.color_breaks = default_color_scales()))
  a <- tab_md_css(theme = "auto")
  set_color_breaks(pct_diff = c(1, 2, 3, 4))
  b <- tab_md_css(theme = "auto")
  testthat::expect_identical(a, b)
  # ... and it does not depend on any table at all (the `tabs` argument is inert).
  testthat::expect_identical(tab_md_css(tabs_col), tab_md_css())
})

testthat::test_that("tab_md(css = TRUE) embeds a <style> block", {
  md <- tab_md(tabs_col, css = TRUE, print = FALSE)
  testthat::expect_true(grepl("^<style>", md))
  testthat::expect_true(grepl("</style>", md, fixed = TRUE))
})

testthat::test_that("tab_md_css writes to a file when file is given", {
  tmp <- tempfile(fileext = ".css")
  on.exit(unlink(tmp))
  out <- tab_md_css(tabs_col, file = tmp)
  testthat::expect_true(file.exists(tmp))
  testthat::expect_gt(length(readLines(tmp)), 0)
})
