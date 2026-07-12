# PURPOSE: openxlsx2 backend for tab_xl() -- thin engine wrappers + range coalescers.
# ROLE: Phase 10h isolates every openxlsx2 call behind ~14 xlb_* wrappers, so tab_xl.R holds only
#       orchestration + the per-table writer. The coalescers turn per-cell style targets into the
#       fewest multi-area `dims` so each shared style is applied once over the largest range.
# KEY CONSTRAINTS:
#   - openxlsx2 is Suggests-only; the ONE requireNamespace() guard lives in tab_xl(), so these
#     wrappers are unguarded (never reached without openxlsx2).
#   - Styling model (verified, Phase 10h probe): wb_add_font/fill/border/numfmt/cell_style MERGE
#     across aspects automatically (== v1 addStyle(stack=TRUE)); WITHIN an aspect the default
#     REPLACES, so borders pass update=TRUE (only the drawn sides; others NULL) and overlapping
#     fonts pass update=<attrs>. Fill/numfmt/alignment are single nodes.
#   - The coalescers (xl_runs/xl_rect_dims/xl_coalesce) are pure base-R (A1 math reimplemented, no
#     openxlsx2) so they are unit-testable in isolation.

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

# multi-area dims for a full rows x cols rectangle-set (gridExpand semantics): rows and cols are
# each compressed to contiguous runs, one rectangle per (row-run, col-run) pair.
xl_rect_dims <- function(rows, cols) {
  rr <- xl_runs(rows); cc <- xl_runs(cols)
  if (!length(rr) || !length(cc)) return(NA_character_)
  parts <- character(0)
  for (r in rr) for (c in cc) parts <- c(parts, xl_one_rect(r, c))
  paste(parts, collapse = ",")
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

# font: update = a character vector of the sub-attributes to MERGE (keep the rest); NULL/FALSE
# replaces the whole font. Only the requested attributes are passed.
xlb_font <- function(wb, sheet, dims, color = NULL, bold = NULL, name = NULL, size = NULL,
                     update = NULL) {
  args <- list(sheet = sheet, dims = dims)
  if (!is.null(color)) args$color <- xl_color(color)
  if (!is.null(bold))  args$bold  <- if (isTRUE(bold)) "1" else bold
  if (!is.null(name))  args$name  <- name
  if (!is.null(size))  args$size  <- as.character(size)
  args$update <- if (is.null(update)) FALSE else update
  do.call(wb$add_font, args)
}

xlb_fill <- function(wb, sheet, dims, color)
  wb$add_fill(sheet = sheet, dims = dims, color = xl_color(color))

# borders: draw only `sides` (subset of top/bottom/left/right); update = TRUE MERGES with existing
# borders (probe-verified) so overlapping passes accumulate. Unlike fills, wb_add_border rejects a
# multi-area `dims`, so a comma-separated dims is applied per contiguous rectangle (no-op for one).
xlb_border <- function(wb, sheet, dims, sides, style = "thin", color = "black", update = TRUE) {
  side <- function(s) if (s %in% sides) style else NULL
  col  <- xl_color(color)
  for (d in strsplit(dims, ",", fixed = TRUE)[[1]]) {
    wb$add_border(
      sheet = sheet, dims = d, update = update,
      top_border    = side("top"),    top_color    = col,
      bottom_border = side("bottom"), bottom_color = col,
      left_border   = side("left"),   left_color   = col,
      right_border  = side("right"),  right_color  = col
    )
  }
  invisible(wb)
}

xlb_numfmt <- function(wb, sheet, dims, code)
  wb$add_numfmt(sheet = sheet, dims = dims, numfmt = code)

xlb_align <- function(wb, sheet, dims, h = NULL, v = NULL, wrap = NULL, rotation = NULL)
  wb$add_cell_style(sheet = sheet, dims = dims, horizontal = h, vertical = v,
                    wrap_text = wrap, text_rotation = rotation)

xlb_col_widths <- function(wb, sheet, cols, widths)
  wb$set_col_widths(sheet = sheet, cols = cols, widths = widths)

xlb_row_heights <- function(wb, sheet, rows, heights)
  wb$set_row_heights(sheet = sheet, rows = rows, heights = heights)

xlb_save <- function(wb, path) wb$save(file = path, overwrite = TRUE)

xlb_open <- function(path) openxlsx2::xl_open(path)

# degrade path: dump a raw frame (or list of frames) to a plain workbook.
xlb_write_xlsx <- function(x, path) openxlsx2::write_xlsx(x, file = path, overwrite = TRUE)
