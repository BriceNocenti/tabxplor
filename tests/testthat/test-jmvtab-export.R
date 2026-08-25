# PURPOSE: Phase 7g jamovi export helpers -- resolveExportPath(), tab_html_string(), jmvtab_export(),
#          + the jamovi html tooltip default (ON via tabxplor.tab_kable_tooltips since pre-release).
# ROLE: The export logic is engine-free so it is unit-testable without a live jamovi session.
# KEY CONSTRAINTS: no native picker -> a typed path resolved to Documents; ~ expands via USERPROFILE.

gss <- forcats::gss_cat
tabs <- tab(gss, marital, race, pct = "row")

testthat::test_that("resolveExportPath(dir, filename, ext): folder + bare name + format extension", {
  # folder + bare filename -> folder/filename.ext (extension from the format, not typed)
  p1 <- resolveExportPath("/tmp/reports", "My Table", ext = "xlsx")
  testthat::expect_match(p1, "My Table\\.xlsx$", ignore.case = TRUE)
  testthat::expect_match(p1, "reports", fixed = TRUE)

  # blank folder -> Documents; blank filename -> "Table"
  p2 <- resolveExportPath("", "", ext = "html")
  testthat::expect_match(p2, "Documents", fixed = TRUE)
  testthat::expect_match(p2, "Table\\.html$", ignore.case = TRUE)

  # a typed extension (even a WRONG one) is dropped; the format's extension wins
  p3 <- resolveExportPath("/tmp", "report.csv", ext = "md")
  testthat::expect_match(p3, "report\\.md$", ignore.case = TRUE)
  testthat::expect_false(grepl("csv", p3))

  # surrounding quotes / brackets are stripped from BOTH parts
  p4 <- resolveExportPath('"/tmp/out"', "<Report>", ext = "xlsx")
  testthat::expect_false(grepl('["<>]', p4))
  testthat::expect_match(p4, "Report\\.xlsx$")

  # OS-illegal filename characters are removed (fs::path_sanitize or the base-R fallback)
  p5 <- resolveExportPath("/tmp", 'a/b:c*d?e', ext = "md")
  testthat::expect_false(grepl('[/:*?]', basename(p5)))
  testthat::expect_match(p5, "\\.md$")

  # ~ in the folder expands via the OS home (NOT R's Documents-remapped path.expand)
  p6 <- resolveExportPath("~/Desktop", "t", ext = "md")
  testthat::expect_false(grepl("^~", p6))
  testthat::expect_match(p6, "t\\.md$")

  # a directory pasted into the FILENAME box is reduced to its bare base name
  p7 <- resolveExportPath("/tmp", "sub/dir/Name", ext = "xlsx")
  testthat::expect_match(basename(p7), "^Name\\.xlsx$")
})

# --- Export-folder detection & the Documents resolver (Phase 18o) -----------------------
# export_documents_dir() is a robust per-OS known-folder resolver backed by the doc_* detectors (the
# rest of the jmvtest diagnostic toolkit is archived in dev/jamovi/jmvtest.b.R). Detectors must NEVER
# error on any OS (off-platform methods return NA); the resolver must always return one usable dir.

testthat::test_that("every Documents detector returns a single path or NA and never errors", {
  detectors <- list(
    doc_win_reg_shell, doc_win_reg_usershell, doc_win_regexe,
    doc_xdg, doc_xdg_file, doc_home_documents
  )
  for (f in detectors) {
    v <- testthat::expect_no_error(f())
    testthat::expect_true(is.character(v) && length(v) == 1L)     # a single path or NA_character_
  }
  testthat::expect_false(is.na(doc_home_documents()))             # the baseline is always concrete
})

testthat::test_that("export_writable(): existing+writable is TRUE, nonexistent / NA / '' are FALSE", {
  testthat::expect_true(export_writable(tempdir()))
  testthat::expect_false(export_writable(file.path(tempdir(), "no_such_dir_xyz_tabxplor")))
  testthat::expect_false(export_writable(NA_character_))
  testthat::expect_false(export_writable(""))
})

testthat::test_that("export_documents_dir() returns one usable directory and never errors", {
  d <- testthat::expect_no_error(export_documents_dir())
  testthat::expect_true(is.character(d) && length(d) == 1L && !is.na(d) && nzchar(d))
  # usable = exists+writable OR its parent is writable (jmvtab_export creates it)
  testthat::expect_true(export_writable(d) || export_writable(dirname(d)))
})

testthat::test_that("resolveExportPath routes the Documents sentinels through the resolver, real paths not", {
  # Compare like with like on EVERY platform: resolveExportPath returns normalizePath() output
  # (BACKslashes on Windows) but the test reads it through dirname(), which always emits "/". So
  # both sides go through one winslash = "/" normaliser -- otherwise the assertion is unsatisfiable
  # on Windows regardless of what the code does.
  norm_dir <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)

  # blank / "~" / "~/Documents" / "auto" all mean "my Documents" -> the SAME resolved folder
  dirs <- vapply(c("", "~", "~/Documents", "~/documents", "auto"),
                 function(s) dirname(resolveExportPath(s, "x", "md")), character(1))
  testthat::expect_length(unique(dirs), 1L)
  testthat::expect_identical(unique(dirs), norm_dir(export_documents_dir()))

  # a real typed folder is respected, NOT rerouted to Documents. tempdir(), not a "/tmp/..." literal:
  # a leading-slash path is DRIVE-RELATIVE on Windows and resolves under the current drive.
  typed <- file.path(tempdir(), "tabxplor_xyz")
  testthat::expect_identical(dirname(resolveExportPath(typed, "x", "md")), norm_dir(typed))
  # a real ~-path still expands to the OS home (NOT the Documents sentinel)
  p <- resolveExportPath("~/Desktop", "x", "md")
  testthat::expect_false(grepl("^~", p))
  testthat::expect_match(p, "Desktop")
})

testthat::test_that("jmvtab_export gives a friendly error when the folder can't be created", {
  # a path we can't create -> a clear, actionable message (not a raw connection error).
  # A directory UNDER A REGULAR FILE is uncreatable on Windows, macOS and Linux alike -- unlike the
  # old "/proc/..." fixture, which is only unwritable on Linux (on Windows it is a drive-relative
  # D:\proc\... and creation legitimately succeeds, so the friendly error never fired there).
  f <- withr::local_tempfile()
  writeLines("x", f)
  bad <- file.path(f, "sub", "Table.md")
  testthat::expect_error(jmvtab_export(tabs, "md", bad), "folder", ignore.case = TRUE)
})

testthat::test_that("tab_html_string produces self-contained HTML (table + inlined CSS)", {
  testthat::skip_if_not_installed("kableExtra")
  h <- tab_html_string(tabs)
  testthat::expect_true(grepl("<table", h))
  testthat::expect_true(grepl("<style", h))           # CSS inlined, not linked
  testthat::expect_false(grepl("<link", h))           # no external stylesheet
})

testthat::test_that("jamovi html carries hover tooltips by default, and tooltips = FALSE overrides", {
  # Phase 18 (pre-release): the two hard-coded tooltips = FALSE were removed -- both jamovi html
  # paths now follow the option default (tabxplor.tab_kable_tooltips, seeded TRUE). The non-popover
  # attrs ride the native `title=` attribute, so they work with no bootstrap JS in the webview.
  h <- tab_html_string(tabs)
  testthat::expect_match(h, 'data-toggle="tooltip"', fixed = TRUE)
  testthat::expect_match(h, ' title="', fixed = TRUE)
  # the ... override path still works (and is the user's option escape hatch)
  h_off <- tab_html_string(tabs, tooltips = FALSE)
  testthat::expect_no_match(h_off, 'data-toggle="tooltip"', fixed = TRUE)

  # results panel: jmv_backend_render_html only reads wrap_rows/wrap_cols off self$options,
  # so a plain list stands in for the R6 self
  self <- list(options = list(wrap_rows = 35, wrap_cols = 15))
  hr <- as.character(jmv_backend_render_html(self, tabs))
  testthat::expect_match(hr, "tx-scrollbox", fixed = TRUE)               # scroll box intact
  testthat::expect_match(hr, 'data-toggle="tooltip"', fixed = TRUE)      # tooltips on by default
})

testthat::test_that("jmvtab_export writes Markdown", {
  tmp <- withr::local_tempdir()
  p   <- file.path(tmp, "t.md")
  out <- jmvtab_export(tabs, "md", p)
  testthat::expect_true(file.exists(p))
  testthat::expect_identical(out, p)
  lines <- readLines(p)
  testthat::expect_true(any(grepl("\\|", lines)))     # a markdown table row
})

testthat::test_that("jmvtab_export writes self-contained HTML", {
  testthat::skip_if_not_installed("kableExtra")
  tmp <- withr::local_tempdir()
  p   <- file.path(tmp, "t.html")
  jmvtab_export(tabs, "html", p)
  testthat::expect_true(file.exists(p))
  txt <- paste(readLines(p), collapse = "\n")
  testthat::expect_true(grepl("<table", txt))
  testthat::expect_true(grepl("lightable|<style", txt))
})

testthat::test_that("jmvtab_export writes a valid Excel workbook", {
  testthat::skip_if_not_installed("openxlsx2")
  tmp <- withr::local_tempdir()
  p   <- file.path(tmp, "t.xlsx")
  jmvtab_export(tabs, "excel", p, replace = TRUE)
  testthat::expect_true(file.exists(p))
  wb <- openxlsx2::wb_load(p)                          # opens without error
  testthat::expect_true(length(openxlsx2::wb_get_sheet_names(wb)) >= 1)
})

# --- "Replace" rule + honest reported path (this-phase) -----------------------------------

testthat::test_that("export_number_path: replace keeps the name, else auto-numbers past existing files", {
  tmp <- withr::local_tempdir()
  p   <- file.path(tmp, "Tableau.xlsx")
  testthat::expect_identical(export_number_path(p, replace = FALSE), p)  # free -> unchanged
  file.create(p)
  testthat::expect_identical(export_number_path(p, replace = TRUE), p)   # replace -> unchanged
  testthat::expect_identical(export_number_path(p, replace = FALSE), file.path(tmp, "Tableau1.xlsx"))
  file.create(file.path(tmp, "Tableau1.xlsx"))
  testthat::expect_identical(export_number_path(p, replace = FALSE), file.path(tmp, "Tableau2.xlsx"))
  # extension-agnostic + robust to a dotted directory
  d2 <- file.path(tmp, "a.b"); dir.create(d2); q <- file.path(d2, "t.md"); file.create(q)
  testthat::expect_identical(export_number_path(q, replace = FALSE), file.path(d2, "t1.md"))
})

testthat::test_that("jmvtab_export honours replace and RETURNS the path really written (md/html/excel)", {
  for (fmt in c("md", "html", "excel")) {
    if (fmt == "html" && !requireNamespace("kableExtra", quietly = TRUE)) next
    if (fmt == "excel" && !requireNamespace("openxlsx2", quietly = TRUE)) next
    tmp <- withr::local_tempdir()
    ext <- switch(fmt, md = "md", html = "html", excel = "xlsx")
    p   <- file.path(tmp, paste0("Tableau.", ext))
    a1  <- jmvtab_export(tabs, fmt, p, replace = FALSE)          # first write -> the requested path
    testthat::expect_identical(a1, p)
    testthat::expect_true(file.exists(a1))
    a2  <- jmvtab_export(tabs, fmt, p, replace = FALSE)          # not replacing -> a NEW, numbered file
    testthat::expect_identical(a2, file.path(tmp, paste0("Tableau1.", ext)))
    testthat::expect_true(file.exists(a2))
    a3  <- jmvtab_export(tabs, fmt, p, replace = TRUE)           # replacing -> back to the requested path
    testthat::expect_identical(a3, p)
  }
})

testthat::test_that("export_status_html: bold green success with the path, bold red failure, escaped", {
  ok <- export_status_html("D:/Documents/Tableau1.xlsx", ok = TRUE)
  testthat::expect_match(ok, "font-weight:bold")
  testthat::expect_match(ok, "#1a7f37")                         # green
  testthat::expect_match(ok, "Saved to: ", fixed = TRUE)
  testthat::expect_match(ok, "Tableau1.xlsx", fixed = TRUE)     # the REAL (numbered) path
  bad <- export_status_html("boom <x> & <y>", ok = FALSE)
  testthat::expect_match(bad, "#c62828")                        # red
  testthat::expect_match(bad, "Export failed: ", fixed = TRUE)
  testthat::expect_match(bad, "&lt;x&gt; &amp; &lt;y&gt;", fixed = TRUE)   # HTML-escaped
})

# --- Reference-level picker helpers (Phase 7g-iii) ----------------------------------------

testthat::test_that("jmvtab_ref_vector: a chosen level -> named vector; none -> free-text", {
  # one explicit level -> named vector keyed by var (raw label; diff_index matches it exactly),
  # unset entries -> "auto"
  rl <- list(list(var = "marital", ref = "Divorced"), list(var = "race", ref = NULL))
  out <- jmvtab_ref_vector(rl, free_text_ref = "auto")
  testthat::expect_named(out, c("marital", "race"))
  testthat::expect_identical(unname(out[["marital"]]), "Divorced")
  testthat::expect_identical(unname(out[["race"]]), "auto")

  # an explicit "tot" (Total) passes through
  rl_tot <- list(list(var = "marital", ref = "tot"))
  testthat::expect_identical(unname(jmvtab_ref_vector(rl_tot)[["marital"]]), "tot")

  # no explicit level chosen -> fall back to the expert free-text ref
  rl2 <- list(list(var = "marital", ref = NULL), list(var = "race", ref = ""))
  testthat::expect_identical(jmvtab_ref_vector(rl2, "tot"), "tot")

  # empty picker -> free-text
  testthat::expect_identical(jmvtab_ref_vector(list(), "first"), "first")
})

testthat::test_that("a named ref vector drives the reference end-to-end (matches a direct call)", {
  rl  <- list(list(var = "marital", ref = "Divorced"))
  ref <- jmvtab_ref_vector(rl, "auto")
  via_picker <- tab(gss, marital, race, pct = "row", ref = ref)
  direct     <- tab(gss, marital, race, pct = "row", ref = c(marital = "Divorced"))
  testthat::expect_equal(via_picker, direct)
})

testthat::test_that("a metacharacter level label matches exactly (rincome '$25000 or more')", {
  # the reported bug: a raw "$25000 or more" was treated as a (broken) regex, so the reference never
  # shifted. diff_index()'s exact-match-first now selects it literally, and the stored `ref` attribute
  # stays human-readable (no anchored/escaped token leaking into the colour legend).
  gss2    <- dplyr::filter(gss, !is.na(rincome))
  ref     <- jmvtab_ref_vector(list(list(var = "rincome", ref = "$25000 or more")))
  testthat::expect_identical(unname(ref[["rincome"]]), "$25000 or more")   # raw, human-readable
  shifted <- tab(gss2, rincome, race, pct = "row", ref = ref, color = "diff")
  default <- tab(gss2, rincome, race, pct = "row", color = "diff")         # ref = "auto" -> total
  testthat::expect_false(isTRUE(all.equal(shifted, default)))              # the reference moved
})

testthat::test_that("a col_var-named ref drives per-col_var references under pct = 'col'", {
  # .b.R keys the picker by col_var under pct="col"; each col_var gets its OWN reference column.
  ref <- jmvtab_ref_vector(list(list(var = "race",  ref = "Black"),
                                list(var = "relig", ref = "None")))
  testthat::expect_named(ref, c("race", "relig"))
  tc    <- tab(gss, marital, c(race, relig), pct = "col", ref = ref, color = "diff")
  marks <- is_refcol(tc)   # exactly one reference column marked per col_var (Black / None)
  testthat::expect_setequal(names(marks)[marks %in% TRUE], c("Black", "None"))
})

testthat::test_that("the jamovi results content carries the width chrome, once, in front", {
  # ⚠ THE rule the whole results width rests on: jamovi pins an Html result at
  # `.jmv-results-html{width:500px}`, so a table wider than that is reported at the app's 620 px floor
  # and clipped by the iframe. Un-pinning it is what makes the panel size itself from the TABLE.
  # See dev/jamovi_results_width.md.
  out <- jmv_results_content("", jmv_results_scrollbox("<table></table>"))
  testthat::expect_match(out, "^<style>", fixed = FALSE)                     # chrome first
  testthat::expect_match(out, ".jmv-results-html{width:max-content;}", fixed = TRUE)
  testthat::expect_identical(lengths(regmatches(out, gregexpr("<style>", out, fixed = TRUE))), 1L)
  testthat::expect_match(out, "tx-scrollbox", fixed = TRUE)
  # the box hugs the table and is capped only by the runaway guard
  testthat::expect_match(out, "width:max-content;max-width:4000px;overflow-x:auto;", fixed = TRUE)

  # empty / NULL fragments drop out, so a caller passes its status line unconditionally
  testthat::expect_identical(jmv_results_content(NULL, "", "<b>x</b>"),
                             paste0(jmv_results_style(), "<b>x</b>"))
})

testthat::test_that("prose cannot size the panel: every fragment is a tx-note", {
  # a wrapping block's max-content is its WHOLE text on one line, so now that the Html element hugs
  # its content an unconstrained hint would report ~1300 px with no table on screen.
  testthat::expect_match(jmv_results_style(), ".tx-note{max-width:520px;}", fixed = TRUE)
  testthat::expect_match(export_status_html("/a/b.xlsx"), 'class="tx-note"', fixed = TRUE)
  testthat::expect_match(export_status_html("boom", ok = FALSE), 'class="tx-note"', fixed = TRUE)

  # THE gate, read from the source: the two placeholders are private methods of a jmvcore R6 class, so
  # the file is the only reach -- and a backend hand-writing a <div> has bypassed jmv_results_note().
  for (f in c("jmvtab.b.R", "jmvtabreg.b.R")) {
    src <- readLines(testthat::test_path("..", "..", "R", f), warn = FALSE)
    testthat::skip_if(length(src) == 0)
    testthat::expect_length(grep("<div", src, value = TRUE), 0L)
  }
})

testthat::test_that("every backend setContent() goes through the content boundary", {
  # a new code path writing the Html element directly would silently re-pin the panel at 620 px.
  for (f in c("jmvtab.b.R", "jmvtabreg.b.R")) {
    src <- readLines(testthat::test_path("..", "..", "R", f), warn = FALSE)
    testthat::skip_if(length(src) == 0)
    one  <- paste(src, collapse = "\n")
    hits <- regmatches(one, gregexpr("html_table[$]setContent[(][[:space:]]*[A-Za-z_.]*", one))[[1]]
    testthat::expect_true(length(hits) > 0)
    testthat::expect_true(all(endsWith(hits, "jmv_results_content")),
                          info = paste(f, paste(hits, collapse = " | ")))
  }
})

testthat::test_that("a table title cannot size a shrink-to-fit container", {
  # `.tabxplor-caption` is a block sibling of the <table>; inside jamovi's max-content scroll box its
  # own max-content (the whole title on one line) would drive the width. Same idiom as `.tx-foot`.
  css <- tab_css()
  testthat::expect_match(css, "\\.tabxplor-caption\\{[^}]*width:0;min-width:100%;\\}")
  testthat::expect_match(css, ".tabxplor-tab .tx-foot{width:0;min-width:100%;}", fixed = TRUE)
})
