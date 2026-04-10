# PURPOSE: Test tab_md() markdown export for structural correctness.
# ROLE: Ensures markdown output is well-formed and options work as documented.
# KEY CONSTRAINTS:
#   - Must run via test_check("tabxplor"), never in isolation.
#   - Uses print = FALSE to capture the returned string for assertions.

gss <- forcats::gss_cat

# === SECTION: Basic output ====================================================

testthat::test_that("tab_md returns a character string with print=FALSE", {
  tabs <- tab(gss, race, marital, pct = "row")
  result <- tab_md(tabs, print = FALSE)
  testthat::expect_type(result, "character")
  testthat::expect_length(result, 1)
  testthat::expect_gt(nchar(result), 0)
})

testthat::test_that("tab_md output contains pipe-delimited markdown table", {
  tabs <- tab(gss, race, marital, pct = "row")
  md <- tab_md(tabs, print = FALSE)
  lines <- strsplit(md, "\n")[[1]]

  # Must have pipe-delimited lines
  pipe_lines <- lines[grepl("\\|", lines)]
  testthat::expect_gt(length(pipe_lines), 2)  # header + separator + data

  # Must have a separator line with dashes and colons
  separator <- lines[grepl("^\\|[-:| ]+\\|$", lines)]
  testthat::expect_gte(length(separator), 1)
})

testthat::test_that("tab_md header contains column names from data", {
  tabs <- tab(gss, race, marital, pct = "row")
  md <- tab_md(tabs, print = FALSE)

  # Check that some marital levels appear in the header
  testthat::expect_true(grepl("Married", md))
  testthat::expect_true(grepl("Divorced", md))
  # Check row_var name
  testthat::expect_true(grepl("race", md))
})

testthat::test_that("tab_md has correct number of data rows", {
  tabs <- tab(gss, race, marital, pct = "row")
  md <- tab_md(tabs, print = FALSE)
  lines <- strsplit(md, "\n")[[1]]

  # Count pipe-delimited lines (excluding header and separator)
  pipe_lines <- lines[grepl("^\\|", lines)]
  # At least: 1 header + 1 separator + nrow(tabs) data lines
  testthat::expect_gte(length(pipe_lines), nrow(tabs) + 2)
})

# === SECTION: Bold references =================================================

testthat::test_that("bold_references=TRUE produces ** markers", {
  tabs <- tab(gss, race, marital, pct = "row")
  md_bold <- tab_md(tabs, bold_references = TRUE, print = FALSE)
  testthat::expect_true(grepl("\\*\\*", md_bold))
})

testthat::test_that("bold_references=FALSE produces no ** markers", {
  tabs <- tab(gss, race, marital, pct = "row")
  md_no <- tab_md(tabs, bold_references = FALSE, print = FALSE)
  testthat::expect_false(grepl("\\*\\*", md_no))
})

# === SECTION: Subtext =========================================================

testthat::test_that("subtext=TRUE does not error even when subtext is empty", {
  tabs <- tab(gss, race, marital, pct = "row")
  md <- tab_md(tabs, subtext = TRUE, print = FALSE)
  testthat::expect_type(md, "character")
})

testthat::test_that("subtext=FALSE produces output no longer than subtext=TRUE", {
  tabs <- tab(gss, race, marital, pct = "row")
  md_with    <- tab_md(tabs, subtext = TRUE, print = FALSE)
  md_without <- tab_md(tabs, subtext = FALSE, print = FALSE)
  testthat::expect_lte(nchar(md_without), nchar(md_with))
})

# === SECTION: Grouped tables ==================================================

testthat::test_that("tab_md works with tab_vars (grouped tables)", {
  gss_sub <- gss |> dplyr::filter(year %in% c(2000, 2014))
  tabs <- tab(gss_sub, race, marital, year, pct = "row")
  md <- tab_md(tabs, print = FALSE)

  testthat::expect_type(md, "character")
  testthat::expect_gt(nchar(md), 0)
  # Should contain separator lines between groups
  lines <- strsplit(md, "\n")[[1]]
  testthat::expect_gt(length(lines), nrow(tabs) + 2)
})

# === SECTION: List of tables ==================================================

testthat::test_that("tab_md works with list of tables sharing col_vars", {
  t1 <- tab(gss, race, marital, pct = "row")
  t2 <- tab(gss, relig, marital, pct = "row")
  md <- tab_md(list(t1, t2), print = FALSE)
  testthat::expect_type(md, "character")
  testthat::expect_gt(nchar(md), 0)
})

testthat::test_that("tab_md errors on list with different col_vars", {
  t1 <- tab(gss, race, marital)
  t2 <- tab(gss, race, relig)
  testthat::expect_error(tab_md(list(t1, t2), print = FALSE),
                         "same col_vars")
})

testthat::test_that("tab_md errors on list with tab_vars", {
  gss_sub <- gss |> dplyr::filter(year %in% c(2000, 2014))
  tabs <- tab(gss_sub, race, marital, year)
  testthat::expect_error(tab_md(list(tabs), print = FALSE),
                         "no tab_vars")
})

# === SECTION: File output =====================================================

testthat::test_that("tab_md writes to file when file argument provided", {
  tabs <- tab(gss, race, marital, pct = "row")
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
  tabs <- tab(gss, race, marital, pct = "row")
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
  tabs <- tab(gss, race, marital, pct = "row")
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
