# Phase 10h: pure unit tests for the openxlsx2 range coalescers (no openxlsx2 needed).
# These functions turn per-cell style targets into the fewest multi-area A1 `dims`, the shared-style
# performance lever. They are base-R only (A1 math reimplemented) so they test in isolation.

test_that("int_to_col / xl_cell produce A1 references", {
  expect_identical(int_to_col(c(1L, 26L, 27L, 28L, 52L, 53L)),
                   c("A", "Z", "AA", "AB", "AZ", "BA"))
  expect_identical(xl_cell(3L, 2L), "B3")
  expect_identical(xl_cell(1L, 1L), "A1")
})

test_that("xl_runs compresses to contiguous runs", {
  expect_identical(xl_runs(c(2, 3, 4, 7, 8)), list(c(2L, 4L), c(7L, 8L)))
  expect_identical(xl_runs(5L), list(c(5L, 5L)))
  expect_identical(xl_runs(c(4, 2, 3, 2)), list(c(2L, 4L)))  # unsorted + duplicate
  expect_identical(xl_runs(integer(0)), list())
})

test_that("xl_rect_dims builds one range per row-run x col-run", {
  expect_identical(xl_rect_dims(2:4, 1:2), "A2:B4")
  expect_identical(xl_rect_dims(2L, c(1L, 3L)), "A2,C2")     # non-contiguous cols
  expect_identical(xl_rect_dims(c(2L, 5L), 1L), "A2,A5")     # non-contiguous rows
  expect_identical(xl_rect_dims(3L, 2L), "B3")               # single cell
  expect_identical(xl_rect_dims(integer(0), 1L), NA_character_)
})

test_that("xl_coalesce merges same-row columns into blocks", {
  expect_identical(xl_coalesce(c(3L, 4L, 5L), c(2L, 2L, 2L)), "C2:E2")   # row-run shared -> block
  expect_identical(xl_coalesce(rep(2L, 3), 3:5), "B3:B5")                # a full column run
  expect_identical(xl_coalesce(c(3L, 4L), c(2L, 5L)), "C2,D5")          # distinct runs -> separate
  expect_identical(xl_coalesce(integer(0), integer(0)), NA_character_)
})

test_that("xl_coalesce covers exactly the target cells", {
  # a 5-col numeric block, all sharing one numFmt over data rows 3:10 -> a single rectangle
  cols <- rep(3:7, each = 8L); rows <- rep(3:10, times = 5L)
  expect_identical(xl_coalesce(cols, rows), "C3:G10")
})
