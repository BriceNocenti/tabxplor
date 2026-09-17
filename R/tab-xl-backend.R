# PURPOSE: the openxlsx2 boundary -- thin engine wrappers, plus the pure A1 geometry tab_xl() needs.
# ROLE: every raw openxlsx2 call in the package is here, behind an xlb_* wrapper, so tab_xl.R holds
#   orchestration only. xl_runs() / xl_coalesce() are pure base R (A1 math, no openxlsx2) and so are
#   unit-testable alone: they turn per-cell style targets into the fewest multi-area `dims`, and each
#   precomposed style is then applied ONCE over the largest range.
# KEY CONSTRAINTS:
#   - openxlsx2 is Suggests-only and the ONE requireNamespace() guard lives in tab_xl(), so these
#     wrappers are deliberately unguarded.
#   - THE openxlsx2 TRAPS LIVE HERE, and each one silently produces a wrong workbook:
#     * create_font() defaults `scheme = "minor"`, which means "this IS the theme's body font" --
#       Excel then resolves the font from the WORKBOOK THEME and ignores the explicit `name`. Pass
#       `scheme = ""` so a cell renders in the font it names.
#     * wb_add_font(update = TRUE) is buggy over large ranges with scattered cells. Aggregate a
#       cell's whole font descriptor and apply it with update = FALSE, which the precompose does.
#     * a comma-joined MULTI-AREA dims is rejected by the older openxlsx2 that jamovi bundles, so
#       xlb_dims_each() splits every dims into single ranges at the emit boundary.
#     * a `wb$method()` chain MUTATES THE WORKBOOK IN PLACE; keeping that here is half the point of
#       the wrappers.
# See: CLAUDE.md section "tabxplor architecture" (exports and rendering); R/tab_xl.R (the writer).

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

# strategy: rows -> runs per column, then columns sharing the same run-set merge into one wider
# rectangle -- the fewest rectangles for one multi-area dims. NA_character_ when nothing to style.
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

xl_color <- function(x) {
  if (grepl("^#?[0-9A-Fa-f]{6}([0-9A-Fa-f]{2})?$", x))
    openxlsx2::wb_color(hex = toupper(sub("^#", "", x)))
  else
    openxlsx2::wb_color(x)   # named colour (e.g. "black")
}

xlb_new_workbook <- function() openxlsx2::wb_workbook()

xlb_base_font <- function(wb, name, size = 10)
  wb$set_base_font(font_size = size, font_name = name)

# Excel forbids \ / ? * : [ ] in a worksheet name; doing the substitution ourselves pre-empts
# openxlsx2's own silent rewrite-and-warn. WARNING: fixed = TRUE, one character at a time -- a
# bracket-expression regex treats \ as a literal, not an escape, and silently matches nothing.
xl_clean_sheet_name <- function(x) {
  for (ch in c("\\", "/", "?", "*", ":", "[", "]")) x <- gsub(ch, " ", x, fixed = TRUE)
  x
}

xlb_add_sheet <- function(wb, title)
  wb$add_worksheet(sheet = title, grid_lines = FALSE)

# WARNING: `first_col = TRUE` and `first_active_row` are ALTERNATIVES in openxlsx2, not a pair -- the
# shorthand wins and silently drops the row split. Give both axes as ACTIVE cell coordinates.
xlb_freeze <- function(wb, sheet, active_row = 3L, active_col = 2L)
  wb$freeze_pane(sheet = sheet, first_active_row = active_row, first_active_col = active_col)

# openxlsx2 renamed the NA arg across versions (`na.strings` -> `na_strings` -> `na`); resolve the
# EXACT formal from the method -- a literal `na = NULL` partial-matches several formals and errors.
xlb_na_argname <- function(wb) {
  fmls <- tryCatch(names(formals(wb$add_data)), error = function(e) character())
  cand <- c("na", "na_strings", "na.strings")
  hit  <- cand[cand %in% fmls]
  if (length(hit)) hit[[1]] else "na_strings"
}

# `list(NULL)` (single-bracket assign) keeps a NULL-VALUED element in the arg list, so the resolved
# NA arg reaches the call as NULL (blank cells); `[[<-` with NULL would drop it entirely.
xlb_add_data <- function(wb, ...) {
  args <- list(..., apply_cell_style = FALSE)
  args[xlb_na_argname(wb)] <- list(NULL)
  do.call(wb$add_data, args)
}

# apply_cell_style = FALSE: tab_xl controls every style itself, never openxlsx2's auto-styling.
xlb_write_data <- function(wb, sheet, x, row, col, col_names = TRUE)
  xlb_add_data(wb, sheet = sheet, x = x, start_row = row, start_col = col, col_names = col_names)

xlb_write_cell <- function(wb, sheet, dims, x)
  xlb_add_data(wb, sheet = sheet, x = x, dims = dims, col_names = FALSE)

# one rich-text cell from a run list (text/color/bold/italic/underline each): colour lives INSIDE
# the string, bypassing the one-font-per-cell xf model, so several coloured break-words share a cell.
xlb_write_richtext <- function(wb, sheet, dims, runs, size = NULL, font = NULL) {
  rt <- NULL
  for (r in runs) {
    if (!nzchar(r$text)) next
    col   <- if (!is.na(r$color)) xl_color(r$color) else NULL
    # underline is "" / "single" / "double", written straight into <u val=...>.
    und   <- if (is.character(r$underline)) (if (nzchar(r$underline)) r$underline else FALSE)
             else isTRUE(r$underline)
    piece <- openxlsx2::fmt_txt(r$text, color = col, bold = isTRUE(r$bold),
                                italic = isTRUE(r$italic), underline = und,
                                size = size, font = font)
    rt <- if (is.null(rt)) piece else rt + piece
  }
  if (is.null(rt)) return(invisible(wb))
  xlb_add_data(wb, sheet = sheet, x = rt, dims = dims, col_names = FALSE)
  invisible(wb)
}

# splits a multi-area dims at the call boundary -- see header for why (older openxlsx2 rejects commas).
xlb_dims_each <- function(dims, f) {
  if (length(dims) != 1L || is.na(dims) || !nzchar(dims)) return(invisible(NULL))
  for (part in strsplit(dims, ",", fixed = TRUE)[[1]]) if (nzchar(part)) f(part)
  invisible(NULL)
}

# numFmt is applied as a grouped merging pass (it merges onto the precomposed xf, cross-aspect).
xlb_numfmt <- function(wb, sheet, dims, code)
  xlb_dims_each(dims, function(d) wb$add_numfmt(sheet = sheet, dims = d, numfmt = code))

# THE DATA BAR (set_bars()), as Excel says it: a dataBar conditional format over one column's data
# rows. ⚠ The bounds are PINNED (`rule = c(min, max)` -> `cfvo type="num"`), never Excel's own auto
# min/max, which would read the Total rows the html bar excludes -- so both media scale on the one
# ceiling the prep resolved. `style` must match `rule` in length: its second entry is the bar, its
# first the (gated-out) negative one. Excel holds ONE colour per bar, so the aplat is the accent
# itself where html lays a 30 % tint under a full-strength border -- the same ink, Excel's own idiom.
# ⚠ the R6 METHOD, not wb_add_conditional_formatting(), which clones the workbook and would drop the
# rule on the floor -- every wrapper here writes in place, as wb$add_numfmt() does.
xlb_databar <- function(wb, sheet, dims, color, min, max)
  xlb_dims_each(dims, function(d) wb$add_conditional_formatting(
    sheet = sheet, dims = d, type = "dataBar", rule = c(min, max),
    style = c(color, color),
    params = list(showValue = TRUE, gradient = FALSE, border = TRUE)))

# apply a precomposed style id over a (possibly multi-area) dims -- one set_cell_style per range.
xlb_set_cell_style <- function(wb, sheet, dims, style)
  xlb_dims_each(dims, function(d) wb$set_cell_style(sheet = sheet, dims = d, style = style))

# a picture anchored at one cell, sized in inches (openxlsx2 places it over the grid, not in a cell)
xlb_add_image <- function(wb, sheet, dims, file, width, height)
  wb$add_image(sheet = sheet, dims = dims, file = file, width = width, height = height,
               units = "in")

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
