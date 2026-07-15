# PURPOSE: Lock the package rule "R code must be ASCII" before R CMD check has to say it.
# ROLE: Guards R/*.R (and the test suite) against a stray accent / em dash / times sign reaching a
#   string literal, where it breaks portability. Escapes (\uXXXX) are ASCII source and pass.
# KEY CONSTRAINTS:
#   - COMMENTS ARE EXEMPT, deliberately: the rule allows real accents in comments, and R CMD check's
#     own scan (tools:::.check_package_ASCII_code) skips them too. A naive whole-file scan would
#     fail on the many files that legitimately carry French accents in a comment.
#   - The scan therefore PARSES each file and drops COMMENT tokens; it never reads raw lines.
#   - This file must itself obey the rule: every accented character below is written \uXXXX, and
#     only the generated temp files hold the real thing.
#   - Skips under R CMD check, which does not ship the source R/ directory next to tests/. That is
#     the point of the rule's "not only via R CMD check": this fires at devtools::test() time.

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
r_files <- function(dir) {
  for (root in c(file.path("..", ".."), ".")) {
    path <- file.path(root, dir)
    if (dir.exists(path)) return(list.files(path, pattern = "\\.R$", full.names = TRUE))
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
