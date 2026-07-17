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
    col <- col[grepl("[0-9]", col)]     # skip the italic col_var-name row (Phase 14f: a body row)
    if (length(col) < 2L) next
    # Where the VISIBLE number ends, in raw columns. Phase 14f: strip the markup that FOLLOWS it
    # (`]{.p1}`, `**`) -- the old metric took the last non-space character, so a bold cell measured
    # its closing `**` and could only ever agree with a coloured one by accident. What must line up
    # for a human reading the raw file is the number; the markup around it is invisible once rendered.
    ends <- vapply(col, function(cell) {
      v <- sub("\\][{][^}]*[}][ ]*$", "", cell)   # ]{.p1 .o2}
      v <- sub("\\*\\*[ ]*$", "", v)              # a closing **
      nchar(sub("[ ]+$", "", v))
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

testthat::test_that("tab_md() output is valid pandoc: it renders as a real <table>", {
  testthat::skip_if(Sys.which("pandoc") == "", "pandoc not on PATH")
  cases <- list(
    "one col_var"    = tab(gss, marital, race, pct = "row"),
    "two col_vars"   = tab(gss, marital, c(race, relig), pct = "row"),
    "coloured"       = tab(gss, marital, c(race, relig), pct = "row", color = "diff"),
    "tab_vars"       = tab(gss, marital, race, year, pct = "row"),
    "numeric col_var"= tab(gss, marital, c(race, tvhours), pct = "row"),
    "no col_var name"= tab(gss, marital, race, pct = "row")
  )
  for (nm in names(cases)) {
    md <- tab_md(cases[[nm]], print = FALSE,
                 var_names = if (identical(nm, "no col_var name")) "rows" else "both")
    h  <- md_pandoc_html(md)
    testthat::expect_match(h, "<table", label = nm)
    # the two symptoms of a table pandoc refused
    testthat::expect_false(grepl("line-block", h, fixed = TRUE), label = nm)
    testthat::expect_false(grepl("|:--", h, fixed = TRUE), label = nm)
    # every data cell really became a cell
    testthat::expect_gt(lengths(regmatches(h, gregexpr("<td", h)))[[1]], nrow(cases[[nm]]))
  }
})

testthat::test_that("the delimiter row's spacer column is dashes, not a blank", {
  # "| |" is not a valid pandoc delimiter cell -- it is what invalidated multi-col_var tables.
  md <- tab_md(tab(gss, marital, c(race, relig), pct = "row"), print = FALSE, color = FALSE)
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

testthat::test_that("a pipe in a label is escaped, not a spurious cell", {
  d <- gss; levels(d$marital)[1] <- "yes | no"
  md <- tab_md(tab(d, marital, race, pct = "row"), print = FALSE, color = FALSE)
  testthat::expect_match(md, "yes \\| no", fixed = TRUE)
  # every body row still has the same number of cells as the header
  lines <- strsplit(md, "\n")[[1]]
  ncell <- function(l) lengths(regmatches(l, gregexpr("(?<!\\\\)[|]", l, perl = TRUE)))
  testthat::expect_true(all(ncell(lines[grepl("^[|]", lines)]) == ncell(lines[1])))
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

# === SECTION: the label columns -- name once, italic, not bold (Phase 14i) ===

testthat::test_that("a merged table names each row-variable ONCE, italic, not bold", {
  # Phase 14i regression: 14d made tab_compact() correctly record tab_vars = character(0), which
  # SILENCED md's tab_vars-gated blanking loop -- so the row-variable name printed on every row.
  md <- tab_md(tab(gss, c(race, marital), relig, pct = "row"), print = FALSE, color = FALSE)
  lines <- strsplit(md, "\n")[[1]]
  testthat::expect_length(grep("*race*", lines, fixed = TRUE), 1L)
  testthat::expect_length(grep("*marital*", lines, fixed = TRUE), 1L)
  # italic (it marks a NAME, like the col_var row), never bold -- even on a bold reference row
  testthat::expect_no_match(md, "**race**", fixed = TRUE)
  testthat::expect_no_match(md, "**marital**", fixed = TRUE)
  # the literal "row_var" header is gone
  testthat::expect_no_match(lines[[1]], "row_var", fixed = TRUE)
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

testthat::test_that("var_names = 'cols' / 'none' drops the row-variable name column", {
  merged <- tab(gss, c(race, marital), relig, pct = "row")
  for (vn in c("cols", "none")) {
    md <- tab_md(merged, print = FALSE, color = FALSE, var_names = vn)
    testthat::expect_no_match(md, "*race*", fixed = TRUE, label = vn)
    testthat::expect_no_match(md, "*marital*", fixed = TRUE, label = vn)
    testthat::expect_match(md, "Never married", fixed = TRUE, label = vn)   # the levels stay
  }
  testthat::expect_match(tab_md(merged, print = FALSE, color = FALSE, var_names = "rows"),
                         "*race*", fixed = TRUE)
})
