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

testthat::test_that("tab_md all pipe lines have same number of pipes", {
  
  md <- tab_md(tabs, bold_references = FALSE, print = FALSE)
  lines <- strsplit(md, "\n")[[1]]
  pipe_lines <- lines[grepl("^\\|", lines)]

  # All lines should have same pipe count
  pipe_counts <- purrr::map_int(pipe_lines,
                                ~ stringr::str_count(., "\\|"))
  testthat::expect_true(length(unique(pipe_counts)) == 1,
                        label = "all pipe lines have same number of pipes")
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

testthat::test_that("coloured table emits pandoc bracketed spans with break-derived classes", {
  md <- tab_md(tabs_col, print = FALSE)
  testthat::expect_true(grepl("]{.", md, fixed = TRUE))          # a span exists
  testthat::expect_true(grepl("[{ ]\\.(p|m)[0-9]", md))          # a diff class (.p5 / .m10 / ...)
  testthat::expect_true(grepl("[{ ]\\.n[}]", md))                # the neutral uniform-span class
})

testthat::test_that("color = FALSE yields plain markdown (no spans)", {
  md <- tab_md(tabs_col, color = FALSE, print = FALSE)
  testthat::expect_false(grepl("]{.", md, fixed = TRUE))
})

testthat::test_that("uncoloured table never gets spans (byte-identical default)", {
  md_default <- tab_md(tabs, print = FALSE)                      # color = TRUE default
  testthat::expect_false(grepl("]{.", md_default, fixed = TRUE))
})

testthat::test_that("two-channel colour emits a background (.bg...) class", {
  t2 <- tab(gss, race, marital, pct = "row",
            color = c(text = "diff", background = "ratio"))
  md <- tab_md(t2, print = FALSE)
  testthat::expect_true(grepl("\\.bg[a-z]", md))
})

testthat::test_that("coloured tables keep numbers aligned (equal pipe-line widths)", {
  md    <- tab_md(tabs_col, print = FALSE)
  lines <- strsplit(md, "\n")[[1]]
  pl    <- lines[grepl("^[|]", lines)]
  # single col_var -> all pipe lines identical width AND identical pipe count
  testthat::expect_length(unique(nchar(pl)), 1L)
  pipes <- purrr::map_int(pl, ~ stringr::str_count(., "[|]"))
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

# === SECTION: tab_md_css() ====================================================

testthat::test_that("tab_md_css emits classes matching the table and a dark @media block", {
  css <- tab_md_css(tabs_col)
  testthat::expect_type(css, "character")
  testthat::expect_true(grepl("\\.(p|m)[0-9]+ \\{ color:", css))          # a diff rule
  testthat::expect_true(grepl("@media (prefers-color-scheme: dark)", css, fixed = TRUE))
})

testthat::test_that("tab_md_css dark_mode = 'none' omits the media block", {
  css <- tab_md_css(tabs_col, dark_mode = "none")
  testthat::expect_false(grepl("@media", css, fixed = TRUE))
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
