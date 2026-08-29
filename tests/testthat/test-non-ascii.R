# PURPOSE: Lock the package rules "R code must be ASCII" and "an Rd file carries only what LaTeX can
#   set" before R CMD check has to say them.
# ROLE: Guards R/*.R (and the test suite) against a stray accent / em dash / times sign reaching a
#   string literal, where it breaks portability. Escapes (\uXXXX) are ASCII source and pass. Guards
#   man/*.Rd against a glyph the PDF manual cannot typeset, and against a double-escaped percent.
# KEY CONSTRAINTS:
#   - COMMENTS ARE EXEMPT, deliberately: the rule allows real accents in comments, and R CMD check's
#     own scan (tools:::.check_package_ASCII_code) skips them too. A naive whole-file scan would
#     fail on the many files that legitimately carry French accents in a comment.
#   - The scan therefore PARSES each file and drops COMMENT tokens; it never reads raw lines.
#   - This file must itself obey the rule: every accented character below is written \uXXXX, and
#     only the generated temp files hold the real thing.
#   - Skips under R CMD check, which does not ship the source R/ directory next to tests/. That is
#     the point of the rule's "not only via R CMD check": this fires at devtools::test() time.
#   - The Rd rule is a different rule with a different reason: the manual is LaTeX, and utf8
#     inputenc sets some glyphs and not others. Only the "PDF version of manual" step says so, and
#     devtools::check() does not build it (manual = FALSE), so nothing else here would catch it.

# Non-ASCII in anything the parser calls code (i.e. every terminal token except COMMENT).
non_ascii_code <- function(path) {
  pd <- utils::getParseData(parse(path, keep.source = TRUE, encoding = "UTF-8"))
  if (is.null(pd) || !nrow(pd)) return(character())
  code <- pd[pd$terminal & pd$token != "COMMENT", , drop = FALSE]
  bad  <- grepl("[^\x01-\x7f]", code$text, useBytes = FALSE)
  if (!any(bad)) return(character())
  sprintf("%s:%d: %s", basename(path), code$line1[bad], code$text[bad])
}

# devtools::test() runs with the working directory at tests/testthat; be tolerant of the root too.
r_files <- function(dir, pattern = "\\.R$") {
  for (root in c(file.path("..", ".."), ".")) {
    path <- file.path(root, dir)
    if (dir.exists(path)) return(list.files(path, pattern = pattern, full.names = TRUE))
  }
  character()
}

scan_dir <- function(dir) as.character(unlist(lapply(r_files(dir), non_ascii_code),
                                             use.names = FALSE))

test_that("R/ source is ASCII outside comments (accents must be written as \\uXXXX)", {
  skip_if(length(r_files("R")) == 0, "R/ not reachable (installed-package run)")
  offenders <- scan_dir("R")
  expect_equal(offenders, character(),
               info = paste0("Non-ASCII in code or string literals (comments are exempt):\n",
                             paste(offenders, collapse = "\n")))
})

test_that("tests/testthat/ source is ASCII outside comments", {
  skip_if(length(r_files("tests/testthat")) == 0, "tests/testthat/ not reachable")
  offenders <- scan_dir("tests/testthat")
  expect_equal(offenders, character(),
               info = paste0("Non-ASCII in test code or string literals:\n",
                             paste(offenders, collapse = "\n")))
})

test_that("the scanner distinguishes comments from code", {
  f <- withr::local_tempfile(fileext = ".R")

  # Every accent below is a \uXXXX escape HERE (so this file obeys the rule) and a real character
  # only in the generated temp file -- which is what the scanner must judge.

  # A real accent in a COMMENT is allowed by the rule and ignored by R CMD check.
  writeLines(c("# accent in a comment: \u00e9\u00e0\u2014", "x <- 1"), f)
  expect_equal(non_ascii_code(f), character())

  # The same character in a STRING LITERAL is exactly what must be caught.
  writeLines("x <- \"caf\u00e9\"", f)
  expect_length(non_ascii_code(f), 1L)

  # A non-ASCII SYMBOL is caught too (backtick-quoted names are code, not comments).
  writeLines("`caf\u00e9` <- 1", f)
  expect_length(non_ascii_code(f), 1L)

  # ... but the \uXXXX escape is ASCII source, so it passes: this is the fix we ask for.
  writeLines("x <- \"caf\\u00e9\"", f)
  expect_equal(non_ascii_code(f), character())
})


# --- The manual is LaTeX ---------------------------------------------------------------------

# The glyphs utf8 inputenc can set AND the package deliberately writes in an Rd file. Anything else
# -- a sigma, a warning sign -- aborts "checking PDF version of manual" with "Unicode character not
# set up for use with LaTeX". A generated page states such a glyph twice instead (\ifelse{latex}).
RD_LATEX_GLYPHS <- c("\u2014", "\u2026", "\u00d7", "\u00f7")  # em dash, ellipsis, times, divide

# What LaTeX is actually handed. Asked of the very converter R CMD check uses, never imitated with
# a regex: \ifelse{latex}, \if{html} and \enc then resolve themselves, and a page that states a
# glyph twice (display_presets_rd()) passes because LaTeX genuinely never sees the second reading.
# Measured at 0.9 s for the whole manual.
rd_latex <- function(path) {
  out <- tempfile(fileext = ".tex")
  on.exit(unlink(out), add = TRUE)
  tools::Rd2latex(path, out = out)
  readLines(out, encoding = "UTF-8", warn = FALSE)
}

non_ascii_rd <- function(path) {
  tex  <- rd_latex(path)
  Encoding(tex) <- "UTF-8"
  kept <- gsub(paste0("[", paste(RD_LATEX_GLYPHS, collapse = ""), "]"), "", tex)
  bad  <- grepl("[^\x01-\x7f]", kept, useBytes = FALSE)
  if (!any(bad)) return(character())
  glyph <- vapply(kept[bad], function(l) paste(unique(regmatches(
    l, gregexpr("[^\x01-\x7f]", l, useBytes = FALSE))[[1]]), collapse = " "),
    character(1), USE.NAMES = FALSE)
  sprintf("%s: %s in %s", basename(path), glyph, trimws(substr(tex[bad], 1, 55)))
}

test_that("man/ carries only the glyphs LaTeX can set (the PDF manual must build)", {
  rd <- r_files("man", pattern = "\\.Rd$")
  skip_if(length(rd) == 0, "man/ not reachable (installed-package run)")
  offenders <- as.character(unlist(lapply(rd, non_ascii_rd), use.names = FALSE))
  expect_equal(offenders, character(),
               info = paste0("Rd glyphs the PDF manual cannot typeset:\n",
                             paste(offenders, collapse = "\n")))
})

test_that("man/ has no double-escaped percent", {
  # roxygen escapes a bare `%` itself, so hand-written roxygen must NOT write `\%` -- the backslash
  # is escaped in turn and the help prints "10\% level". The opposite holds inside an @eval doc
  # string (R/tab-args.R), inserted as raw Rd, which correctly writes `\\%`.
  rd <- r_files("man", pattern = "\\.Rd$")
  skip_if(length(rd) == 0, "man/ not reachable (installed-package run)")
  offenders <- unlist(lapply(rd, function(p) {
    lines <- readLines(p, warn = FALSE)
    hit   <- grep("\\\\\\\\%", lines)
    if (!length(hit)) NULL else sprintf("%s:%d", basename(p), hit)
  }), use.names = FALSE)
  expect_null(offenders)
})

test_that("the Rd scanner reads what LaTeX is handed", {
  f <- withr::local_tempfile(fileext = ".Rd")
  rd <- function(...) {
    writeLines(c("\\name{probe}", "\\title{probe}", "\\description{", ..., "}"), f)
    non_ascii_rd(f)
  }
  # utf8 inputenc sets all four: they are the allow-list, and pass.
  expect_equal(rd("allowed \u2014 \u00d7 \u00f7 \u2026 here"), character())

  # A bare glyph LaTeX cannot set is what aborts the PDF.
  expect_length(rd("bare sigma \u03c3 here"), 1L)
  expect_length(rd("warn \u26a0 here"), 1L)

  # ... but the same glyph in the NON-latex arm passes, because LaTeX is handed the other one --
  # which a regex over the Rd source could not tell, and Rd2latex settles.
  expect_equal(rd("wrapped \\ifelse{latex}{\\code{SD}}{\\code{\u03c3}} here"), character())
})
