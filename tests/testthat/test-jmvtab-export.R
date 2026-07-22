# PURPOSE: Phase 7g jamovi export helpers -- resolveExportPath(), tab_html_string(), jmvtab_export().
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

# --- Export-folder detection & the Documents resolver (Last Phase o) -----------------------
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
  # blank / "~" / "~/Documents" / "auto" all mean "my Documents" -> the SAME resolved folder
  dirs <- vapply(c("", "~", "~/Documents", "~/documents", "auto"),
                 function(s) dirname(resolveExportPath(s, "x", "md")), character(1))
  testthat::expect_length(unique(dirs), 1L)
  testthat::expect_identical(unique(dirs), normalizePath(export_documents_dir(), mustWork = FALSE))

  # a real typed folder is respected, NOT rerouted to Documents
  testthat::expect_identical(dirname(resolveExportPath("/tmp/tabxplor_xyz", "x", "md")),
                             normalizePath("/tmp/tabxplor_xyz", mustWork = FALSE))
  # a real ~-path still expands to the OS home (NOT the Documents sentinel)
  p <- resolveExportPath("~/Desktop", "x", "md")
  testthat::expect_false(grepl("^~", p))
  testthat::expect_match(p, "Desktop")
})

testthat::test_that("jmvtab_export gives a friendly error when the folder can't be created", {
  # a path under a location we can't write to -> a clear, actionable message (not a raw connection error)
  bad <- "/proc/tabxplor_nope/sub/Table.md"
  testthat::expect_error(jmvtab_export(tabs, "md", bad), "folder", ignore.case = TRUE)
})

testthat::test_that("tab_html_string produces self-contained HTML (table + inlined CSS)", {
  testthat::skip_if_not_installed("kableExtra")
  h <- tab_html_string(tabs)
  testthat::expect_true(grepl("<table", h))
  testthat::expect_true(grepl("<style", h))           # CSS inlined, not linked
  testthat::expect_false(grepl("<link", h))           # no external stylesheet
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
