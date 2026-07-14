# PURPOSE: openxlsx2 backend for tab_xl() -- thin engine wrappers + the range coalescer.
# ROLE: Phase 10h isolates the plumbing openxlsx2 calls behind xlb_* wrappers, so tab_xl.R holds the
#       orchestration + the per-table writer. xl_coalesce turns per-cell style targets into the fewest
#       multi-area `dims`, so each precomposed style is applied ONCE over the largest range.
# KEY CONSTRAINTS:
#   - openxlsx2 is Suggests-only; the ONE requireNamespace() guard lives in tab_xl(), so these
#     wrappers (and the create_*/set_cell_style compose in tab_xl.R xl_apply_styles) are unguarded.
#   - Styling model (Phase 10h): the writer PRECOMPOSES each cell's full style (create_font/create_fill/
#     create_border + create_cell_style) and applies it by id with set_cell_style -- far fewer + cheaper
#     openxlsx2 calls than a wb_add_* per aspect (the openxlsx2 "shared styles" fast path). numFmt is
#     the one exception, applied as a grouped wb_add_numfmt pass that MERGES onto the composed xf.
#   - xl_runs/xl_coalesce are pure base-R (A1 math reimplemented, no openxlsx2), unit-testable alone.

# === SECTION: A1 geometry + range coalescing (pure, testable) ========================

# 1 -> "A", 26 -> "Z", 27 -> "AA". Vectorised.
int_to_col <- function(n) {
  vapply(n, function(x) {
    s <- ""
    while (x > 0) { r <- (x - 1L) %% 26L; s <- paste0(LETTERS[r + 1L], s); x <- (x - 1L) %/% 26L }
    s
  }, character(1))
}

xl_cell <- function(row, col) paste0(int_to_col(col), row)

# contiguous integer runs: c(2,3,4,7,8) -> list(c(2,4), c(7,8))
xl_runs <- function(x) {
  x <- sort(unique(as.integer(x)))
  if (!length(x)) return(list())
  brk <- c(0L, which(diff(x) != 1L), length(x))
  lapply(seq_len(length(brk) - 1L), function(i) c(x[brk[i] + 1L], x[brk[i + 1L]]))
}

# one A1 range for a rectangle (or a single cell)
xl_one_rect <- function(r, c) {
  if (r[1] == r[2] && c[1] == c[2]) xl_cell(r[1], c[1])
  else paste0(xl_cell(r[1], c[1]), ":", xl_cell(r[2], c[2]))
}

# per-cell (col, row) targets sharing ONE style -> the fewest rectangles -> one multi-area dims.
# Rows are compressed to runs per column, then columns with an identical run-set are merged into a
# single wider rectangle block. Returns NA_character_ when there is nothing to style.
xl_coalesce <- function(cols, rows) {
  if (!length(cols)) return(NA_character_)
  cols <- as.integer(cols); rows <- as.integer(rows)
  by_col   <- split(rows, cols)
  cols_i   <- as.integer(names(by_col))
  run_key  <- vapply(by_col, function(r) paste(unlist(xl_runs(r)), collapse = "_"), character(1))
  parts <- character(0)
  for (k in unique(run_key)) {
    sel       <- run_key == k
    these_col <- cols_i[sel]
    row_runs  <- xl_runs(by_col[[which(sel)[1]]])
    col_runs  <- xl_runs(these_col)
    for (r in row_runs) for (c in col_runs) parts <- c(parts, xl_one_rect(r, c))
  }
  paste(parts, collapse = ",")
}

# === SECTION: openxlsx2 engine wrappers (unguarded; openxlsx2 loaded by tab_xl) ======
# openxlsx2 `wbWorkbook` is an R6 object: the `wb$method()` (chain) form MUTATES IN PLACE (the
# `wb_*()` pipe form returns a modified clone that must be reassigned). tab_xl issues hundreds of
# style calls into one workbook, so the wrappers use the in-place `$` methods -- no reassignment.

# tabxplor palette hex ("#rrggbb"/"rrggbb"/"aarrggbb") OR a named colour -> openxlsx2 colour object.
xl_color <- function(x) {
  if (grepl("^#?[0-9A-Fa-f]{6}([0-9A-Fa-f]{2})?$", x))
    openxlsx2::wb_color(hex = toupper(sub("^#", "", x)))
  else
    openxlsx2::wb_color(x)   # named colour (e.g. "black")
}

xlb_new_workbook <- function() openxlsx2::wb_workbook()

xlb_base_font <- function(wb, name, size = 10)
  wb$set_base_font(font_size = size, font_name = name)

# gridlines are turned off at sheet creation (replaces v1 showGridLines)
xlb_add_sheet <- function(wb, title)
  wb$add_worksheet(sheet = title, grid_lines = FALSE)

xlb_freeze <- function(wb, sheet, active_row = 3L)
  wb$freeze_pane(sheet = sheet, first_active_row = active_row, first_col = TRUE)

# raw numbers written; na = NULL -> blank cells (not #N/A); apply_cell_style = FALSE -> no
# openxlsx2 auto-styling (tab_xl controls every style itself).
xlb_write_data <- function(wb, sheet, x, row, col)
  wb$add_data(sheet = sheet, x = x, start_row = row, start_col = col,
              col_names = TRUE, na = NULL, apply_cell_style = FALSE)

xlb_write_cell <- function(wb, sheet, dims, x)
  wb$add_data(sheet = sheet, x = x, dims = dims,
              col_names = FALSE, na = NULL, apply_cell_style = FALSE)

# Phase 13b: write ONE rich-text cell (openxlsx2::fmt_txt) from a run list -- each run
# list(text, color = <hex|NA>, bold). Coloured break-words carry their palette hex + bold; the rest
# stays plain black (the sheet subtext font). Rich text keeps the per-run colour INSIDE the string,
# bypassing the one-font-per-cell xf model, so the colour legend is readable in Excel. `size`/`font`
# match the surrounding subtext cell so the rich cell doesn't jump size.
xlb_write_richtext <- function(wb, sheet, dims, runs, size = NULL, font = NULL) {
  rt <- NULL
  for (r in runs) {
    if (!nzchar(r$text)) next
    col   <- if (!is.na(r$color)) xl_color(r$color) else NULL
    piece <- openxlsx2::fmt_txt(r$text, color = col, bold = isTRUE(r$bold),
                                size = size, font = font)
    rt <- if (is.null(rt)) piece else rt + piece
  }
  if (is.null(rt)) return(invisible(wb))
  wb$add_data(sheet = sheet, x = rt, dims = dims,
              col_names = FALSE, na = NULL, apply_cell_style = FALSE)
  invisible(wb)
}

# numFmt is applied as a grouped merging pass (it merges onto the precomposed xf, cross-aspect).
xlb_numfmt <- function(wb, sheet, dims, code)
  wb$add_numfmt(sheet = sheet, dims = dims, numfmt = code)

# Phase 13c-iii: merge a horizontal cell range (the col_var spanning-name header).
xlb_merge <- function(wb, sheet, dims)
  wb$merge_cells(sheet = sheet, dims = dims)

xlb_col_widths <- function(wb, sheet, cols, widths)
  wb$set_col_widths(sheet = sheet, cols = cols, widths = widths)

xlb_row_heights <- function(wb, sheet, rows, heights)
  wb$set_row_heights(sheet = sheet, rows = rows, heights = heights)

xlb_save <- function(wb, path) wb$save(file = path, overwrite = TRUE)

xlb_open <- function(path) openxlsx2::xl_open(path)

# degrade path: dump a raw frame (or list of frames) to a plain workbook.
xlb_write_xlsx <- function(x, path) openxlsx2::write_xlsx(x, file = path, overwrite = TRUE)
