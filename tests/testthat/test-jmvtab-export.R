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

# --- Export-folder detection & diagnostics (Last Phase o) ---------------------------------
# The doc_* / fb_* detectors + export_writable/export_write_test + the candidate tables are the seed
# of the eventual export_documents_dir() rewrite, so they stay after jmvtest is removed. They must
# NEVER error on any OS (off-platform methods return NA), and the read-only probes never write a file.

testthat::test_that("every Documents / fallback detector returns a single path or NA and never errors", {
  detectors <- list(
    doc_win_powershell, doc_win_reg_shell, doc_win_reg_usershell, doc_win_regexe,
    doc_win_onedrive, doc_xdg, doc_xdg_file, doc_wsl_mnt, doc_home_documents,
    fb_home, fb_desktop, fb_downloads, fb_cwd, fb_tempdir
  )
  for (f in detectors) {
    v <- testthat::expect_no_error(f())
    testthat::expect_true(is.character(v) && length(v) == 1L)     # a single path or NA_character_
  }
  # the baseline home/Documents and the tempdir fallback are always concrete (non-NA)
  testthat::expect_false(is.na(doc_home_documents()))
  testthat::expect_false(is.na(fb_tempdir()))
})

testthat::test_that("export_writable(): existing+writable is TRUE, nonexistent / NA / '' are FALSE", {
  testthat::expect_true(export_writable(tempdir()))
  testthat::expect_false(export_writable(file.path(tempdir(), "no_such_dir_xyz_tabxplor")))
  testthat::expect_false(export_writable(NA_character_))
  testthat::expect_false(export_writable(""))
})

testthat::test_that("export_write_test() writes a .md file and reports the path; a bad folder fails cleanly", {
  d <- file.path(tempdir(), "tabxplor_jmvtest_probe")
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  r <- export_write_test(d, "unit_probe", note = "method: unit test")
  testthat::expect_true(r$ok)
  testthat::expect_true(file.exists(r$path))
  testthat::expect_match(r$path, "unit_probe\\.md$")
  testthat::expect_true(any(grepl("tabxplor export folder test", readLines(r$path))))

  bad <- export_write_test("/proc/tabxplor_nope/sub", "x")   # can't create -> ok=FALSE, no throw
  testthat::expect_false(bad$ok)
  testthat::expect_false(is.na(bad$error))
})

testthat::test_that("candidate tables have the right shape (>= 5 rows, method/dir/exists/writable) + a CURRENT row", {
  docs <- export_doc_candidates()
  testthat::expect_named(docs, c("method", "dir", "exists", "writable"))
  testthat::expect_gte(nrow(docs), 5L)
  testthat::expect_true(is.logical(docs$exists) && is.logical(docs$writable))
  testthat::expect_true(any(startsWith(docs$method, "CURRENT")))     # today's behaviour, side by side
  testthat::expect_true(any(nzchar(docs$dir)))                       # at least the baseline resolves

  fb <- export_fallback_candidates()
  testthat::expect_named(fb, c("method", "dir", "exists", "writable"))
  testthat::expect_gte(nrow(fb), 5L)
  testthat::expect_true(fb$writable[fb$method == "tempdir()"])       # the universal safety net
})

testthat::test_that("export_env_probe() + export_probe_html() build a named report and HTML tables", {
  env <- export_env_probe()
  testthat::expect_true(is.character(env) && !is.null(names(env)))
  testthat::expect_true(all(c("OS.type", "WSL", "export_home_dir()") %in% names(env)))

  testthat::expect_match(export_probe_html(env, "Env"), "<table")
  testthat::expect_match(export_probe_html(export_doc_candidates(), "Docs"), "<table")
  testthat::expect_match(export_probe_html(env, "Env"), "<h3")                 # the title renders
  # HTML-escaping: a "<" in a value never leaks as a raw tag
  testthat::expect_false(grepl("<script", export_probe_html(c(a = "<script>"))))
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
