# The shipped suite's setup IS this suite's setup -- the thread pinning and the pinned console theme
# are properties of running tabxplor's tests, not of which suite is running them.
local({
  p <- testthat::test_path("..", "..", "..", "tests", "testthat", "setup.R")
  if (file.exists(p)) sys.source(p, envir = globalenv())
})
