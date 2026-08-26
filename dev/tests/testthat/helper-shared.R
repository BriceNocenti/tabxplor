# PURPOSE: give the dev suite the shipped suite's helpers, plus the one that has to differ.
# ROLE: the moved tests are unchanged, so they still call the helpers they were written against.
#   Sourcing rather than copying keeps one definition of each.
# WARNING: `src_path()` is redefined here, and the reason is the DEPTH. The shipped suite sits two
#   directories below the package root, this one sits three, so the shipped definition would resolve
#   to `dev/` instead of the root. It is a different fact, not a duplicate.

local({
  shipped <- testthat::test_path("..", "..", "..", "tests", "testthat")
  for (h in c("helper-reg.R", "helper-fixtures.R", "helper-golden.R", "helper-color-golden.R",
              "helper-i18n.R")) {
    p <- file.path(shipped, h)
    if (file.exists(p)) sys.source(p, envir = globalenv())
  }
})

src_path <- function(...) {
  p <- testthat::test_path("..", "..", "..", ...)
  testthat::skip_if_not(file.exists(p), paste0(file.path(...), " is not in this checkout"))
  p
}
