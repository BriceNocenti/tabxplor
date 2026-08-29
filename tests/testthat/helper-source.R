# PURPOSE: the ONE guard for a test that reads the PACKAGE SOURCE (jamovi/, R/, dev/).
#
# `R CMD check` runs the tests from `<pkg>.Rcheck/tests/testthat`, where `../..` holds only the
# INSTALLED package -- and `jamovi/` and `dev/` are `.Rbuildignore`d, so they are absent from the
# tarball too. So a consistency test that reads the source is a DEVELOPMENT test: it runs under
# devtools::test() and must skip everywhere else.
#
# WARNING: call this INSTEAD of test_path(), never after the read. `readLines()` / `read_yaml()` on
#   a missing file THROWS, so a skip_if() on the result is unreachable -- which is exactly how seven
#   of these reached CI as errors.
src_path <- function(...) {
  p <- testthat::test_path("..", "..", ...)
  testthat::skip_if_not(file.exists(p),
                        paste0(file.path(...), " is not shipped in a built package"))
  p
}
