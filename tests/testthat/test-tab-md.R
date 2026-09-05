# PURPOSE: the markdown exporter, and that pandoc reads what it writes.
# ROLE: the shipped CONTRACT for R/tab_md.R -- a failure here is a user-visible change.
#   The exhaustive sweep around it lives in dev/tests/testthat/.
# See: CLAUDE.md section "Testing" (what belongs in which suite).

# === SECTION: the markdown exporter ===============================================================

withr::local_options(lifecycle_verbosity = "quiet", .local_envir = testthat::teardown_env())



gss <- fx_gss()


# === SECTION: Basic output ====================================================

tabs <- tab(gss, race, marital, pct = "row")

md <- tab_md(tabs, print = FALSE)


testthat::test_that("tab_md returns a character string with print=FALSE", {
  testthat::expect_type(md, "character")
  testthat::expect_length(md, 1)
  testthat::expect_gt(nchar(md), 0)
})


# === SECTION: the markdown object =================================================================
# `tabxplor_md` is the twin of tab_html()'s `tabxplor_kable`: a rendered string that knows how to
# present itself in either medium. It is what makes options(tabxplor.print = "md") branch-free.

testthat::test_that("tab_md() returns a tabxplor_md that stays an ordinary character", {
  testthat::expect_s3_class(md, "tabxplor_md")
  testthat::expect_true(is.character(md))
  testthat::expect_type(md, "character")
  # every ordinary use of the string is untouched
  testthat::expect_gt(nchar(md), 0)
  testthat::expect_length(strsplit(md, "\n")[[1]], length(strsplit(unclass(md), "\n")[[1]]))
  testthat::expect_type(tab_pipe(tabs), "character")
})

testthat::test_that("print = NULL cats at the console and hands the object over while knitting", {
  # outside a render: cat(), invisibly
  out <- utils::capture.output(res <- tab_md(tabs, css = FALSE))
  testthat::expect_gt(length(out), 3L)
  testthat::expect_s3_class(res, "tabxplor_md")
  # while knitting: nothing cat, the OBJECT -- a cat() would land in a verbatim block
  withr::local_options(knitr.in.progress = TRUE)
  out2 <- utils::capture.output(res2 <- tab_md(tabs, css = FALSE))
  testthat::expect_length(out2, 0L)
  testthat::expect_s3_class(res2, "tabxplor_md")
  testthat::expect_true(isTRUE(all.equal(unclass(res), unclass(res2))))
})

testthat::test_that("knit_print.tabxplor_md hands the markdown over raw", {
  testthat::skip_if_not_installed("knitr")
  out <- knitr::knit_print(tab_md(tabs, print = FALSE, css = FALSE))
  testthat::expect_s3_class(out, "knit_asis")
  testthat::expect_true(grepl("|:-", out, fixed = TRUE))
})

# *The option's own doc, and EXPORT_ARGS$css, both name tab_md() -- but its `css` was a hard TRUE.*
testthat::test_that("css takes its default from tabxplor.tab_kable_css, as tab_html() does", {
  testthat::expect_true(grepl("<style>", tab_md(tabs, print = FALSE), fixed = TRUE))
  withr::with_options(list(tabxplor.tab_kable_css = FALSE),
    testthat::expect_false(grepl("<style>", tab_md(tabs, print = FALSE), fixed = TRUE)))
  # ...and the alias is read first, exactly as tx_option() promises
  withr::with_options(list(tabxplor.kable_css = FALSE),
    testthat::expect_false(grepl("<style>", tab_md(tabs, print = FALSE), fixed = TRUE)))
  # an explicit argument still wins over both
  withr::with_options(list(tabxplor.tab_kable_css = FALSE),
    testthat::expect_true(grepl("<style>", tab_md(tabs, print = FALSE, css = TRUE), fixed = TRUE)))
})

testthat::test_that("a chunk's tab.cap is the caption when the call gives none", {
  testthat::skip_if_not_installed("knitr")
  withr::local_options(knitr.in.progress = TRUE)
  oc <- knitr::opts_current$get()
  withr::defer(knitr::opts_current$restore(oc))
  knitr::opts_current$set(tab.cap = "From the chunk")
  testthat::expect_true(grepl(": From the chunk",
                              tab_md(tabs, print = FALSE, css = FALSE), fixed = TRUE))
  # an explicit caption wins
  testthat::expect_true(grepl(": Mine",
                              tab_md(tabs, print = FALSE, css = FALSE, caption = "Mine"),
                              fixed = TRUE))
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


# === SECTION: Bold references =================================================

md_bold <- tab_md(tabs, bold_references = TRUE, print = FALSE)


md_no <- tab_md(tabs, bold_references = FALSE, print = FALSE)


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


# Phase 20h: the prepared starwars fixture, built ONCE at top level -- where the file-level
# lifecycle line above actually bites (testthat re-enables the warning inside every
# test_that()). It was written verbatim in each block below.
sw_prepared <- dplyr::starwars |>
  tab_prepare("sex", "hair_color", "eye_color", "mass", "gender",
              other_if_less_than = 5)


# === SECTION: Alignment and structure =========================================

testthat::test_that("tab_md alignment separator uses : for right/left alignment", {
  # css = FALSE: the plain byte-clean grammar (the styled default adds blank-row separators + a
  # stylesheet, which is exercised elsewhere).
  md <- tab_md(tabs, print = FALSE, css = FALSE)
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
  pipe_counts <- purrr::map_int(grid, ~ lengths(regmatches(., gregexpr("\\|", ., perl = TRUE))))
  testthat::expect_true(length(unique(pipe_counts)) == 1,
                        label = "all grid pipe lines have same number of pipes")
})


# === SECTION: colour spans (Phase 10f) ========================================

tabs_col <- tab(gss, race, marital, pct = "row", color = "diff")
# a COMPOSITE display is what puts an aside beside the primary token -- the shape the span must not
# swallow, and the one every width case below is measured on.
tabs_comp <- dplyr::mutate(tabs_col,
                           dplyr::across(dplyr::where(is_fmt), ~ set_display(., "{pct} ({n})")))


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


testthat::test_that("the colour span stops at the primary token, and the aside stays plain", {
  # THE CELL'S RENDERING STOPS AT ITS PRIMARY TOKEN -- the rule html states and md now applies.
  md <- tab_md(tabs_comp, print = FALSE)
  # the span CLOSES before the aside (the separator is a nbsp, so html cannot break the pair)
  testthat::expect_match(md, "\\]\\{\\.[pm][0-9][^}]*\\}[ \u00a0]\\(")
  testthat::expect_false(grepl("\\[[^]]*\\([^)]*\\)[^]]*\\]\\{", md))  # no aside inside a span

  # the stars ride INSIDE the span: they are the cell's own signal, not an aside
  st <- tab_md(tab(gss, race, marital, pct = "row", color = "diff", stars = TRUE), print = FALSE)
  testthat::expect_match(st, "[0-9]%[*]+\\]\\{\\.[pm][0-9]")

  # the documented opt-out reaches md too
  withr::with_options(list(tabxplor.color_whole_cell = TRUE), {
    whole <- tab_md(tabs_comp, print = FALSE)
    testthat::expect_match(whole, "\\([^)]*\\)[ ]*\\]\\{\\.[pm][0-9]")
  })
})


testthat::test_that("numbers stay aligned when coloured and uncoloured cells mix in a column", {
  # Phase 13d: dropping the `.n` neutral removed the span that used to give EVERY cell of a coloured
  # column the same "[num]{...}" scaffold. An uncoloured cell now uses a bracket-free geometry ("  " in
  # place of " [") so the number's right edge lands at the same offset. The sibling pipe-width test
  # cannot see this: padding to total_width keeps the pipes aligned even if the numbers drift.
  for (tb in list(tabs_col, tabs_comp)) {
  md    <- tab_md(tb, print = FALSE)
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
  }
  testthat::succeed()
})


testthat::test_that("caption renders a pandoc caption line", {
  md <- tab_md(tabs_col, caption = "My caption", print = FALSE)
  testthat::expect_true(grepl("\n: My caption", md, fixed = TRUE))
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
    # the two symptoms of a table pandoc refused. ⚠ the marker is pandoc's own ELEMENT, not the bare
    # word: `h` carries our inlined stylesheet, where "line-block" is a substring of `display:inline-block`.
    testthat::expect_false(grepl('class="line-block"', h, fixed = TRUE), label = nm)
    testthat::expect_false(grepl("|:--", h, fixed = TRUE), label = nm)
    # every data cell really became a cell
    testthat::expect_gt(lengths(regmatches(h, gregexpr("<td", h)))[[1]], nrow(cases[[nm]]))
  }
})


testthat::test_that("a pipe in a label is escaped, not a spurious cell", {
  d <- gss; levels(d$marital)[1] <- "yes | no"
  # css = FALSE: the plain grammar with ASCII spaces (the styled default makes label spaces
  # non-breaking, tested separately).
  md <- tab_md(tab(d, marital, race, pct = "row"), print = FALSE, color = FALSE, css = FALSE)
  testthat::expect_match(md, "yes \\| no", fixed = TRUE)
  # every body row still has the same number of cells as the header
  lines <- strsplit(md, "\n")[[1]]
  ncell <- function(l) lengths(regmatches(l, gregexpr("(?<!\\\\)[|]", l, perl = TRUE)))
  testthat::expect_true(all(ncell(lines[grepl("^[|]", lines)]) == ncell(lines[1])))
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


testthat::test_that("var_names = 'cols' / 'none' drops the row-variable name column", {
  merged <- tab(gss, c(race, marital), relig, pct = "row")
  for (vn in c("cols", "none")) {
    # css = FALSE: plain labels keep ASCII spaces ("Never married"); the styled default makes them
    # non-breaking.
    md <- tab_md(merged, print = FALSE, color = FALSE, var_names = vn, css = FALSE)
    testthat::expect_no_match(md, "*race*", fixed = TRUE, label = vn)
    testthat::expect_no_match(md, "*marital*", fixed = TRUE, label = vn)
    testthat::expect_match(md, "Never married", fixed = TRUE, label = vn)   # the levels stay
  }
  testthat::expect_match(tab_md(merged, print = FALSE, color = FALSE, var_names = "rows", css = FALSE),
                         "*race*", fixed = TRUE)
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


testthat::test_that("format = \"html\" carries the host reset + the md-only rules; \"md\" omits them", {
  css <- tab_css(style_tag = FALSE)                              # format = "html"
  # THE HOST RESET is medium-agnostic: it must reach the html engine, where `.tabxplor-tab` IS the
  # <table> and a `.tabxplor-tab table ...` selector never matches. That asymmetry is what left a
  # Bootstrap border under every row of a pkgdown-rendered table.
  reset <- ".tabxplor-tab th,.tabxplor-tab td{border-width:0;}"
  testthat::expect_match(css, reset, fixed = TRUE)
  testthat::expect_no_match(css, ".tabxplor-tab table td,.tabxplor-tab table th{border-width",
                            fixed = TRUE)
  testthat::expect_match(css, "tr:not(:has(td:not(:empty)))", fixed = TRUE)     # blank-row rule
  testthat::expect_match(css, ".tabxplor-tab table td:empty", fixed = TRUE)     # spacer collapse
  # the reset reads first, before every rule that redraws over it (it loses on specificity anyway)
  testthat::expect_lt(regexpr(reset, css, fixed = TRUE),
                      regexpr(".tabxplor-tab thead th{", css, fixed = TRUE))
  # the chrome-free flavour omits all three
  bare <- tab_css(format = "md", style_tag = FALSE)
  testthat::expect_no_match(bare, reset, fixed = TRUE)
  testthat::expect_no_match(bare, "tr:not(:has", fixed = TRUE)
})


# === Phase 22c-ii: the unit header row ==============================================================

testthat::test_that("md carries the unit row, italic and span-free", {
  t  <- tab(fx_gss(), race, marital, pct = "row", color = "diff")
  md <- tab_md(t, print = FALSE)
  ln <- grep("^[|]", strsplit(md, "\n")[[1]], value = TRUE)
  # header, delimiter, the col_var-name row, then the unit row -- all inside the header block
  testthat::expect_match(ln[[3]], "*marital*", fixed = TRUE)
  # ... in the console type tag's own notation, and per BLOCK -- so the Total restates its own
  testthat::expect_match(ln[[4]], "*<row%>*",    fixed = TRUE)
  testthat::expect_match(ln[[4]], "*<row% (n)>*", fixed = TRUE)
  # md styles with emphasis, never a class span: a span costs raw line width the grid cannot absorb
  # (the stylesheet still carries the `.tx-unit` rule -- that is the html render's, not the grid's)
  testthat::expect_false(any(grepl("tx-unit", ln, fixed = TRUE)))
  # ... and the grid stays square
  testthat::expect_length(unique(nchar(ln)), 1L)

  # the same, on the shape that actually stresses the width model: an interior span with an aside
  # after it, mixed with uncoloured and bold cells in one column.
  ln2 <- grep("^[|]", strsplit(tab_md(tabs_comp, print = FALSE), "\n")[[1]], value = TRUE)
  testthat::expect_length(unique(nchar(ln2)), 1L)
})
