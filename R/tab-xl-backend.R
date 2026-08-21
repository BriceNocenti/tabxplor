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
#   - xl_coalesce emits a comma-joined MULTI-area dims; the older jamovi-bundled openxlsx2 rejects those,
#     so xlb_dims_each splits every dims to single ranges at the emit boundary (see its DESIGN note).
#   - openxlsx2 TRAPS, each cost a session to diagnose:
#     * create_font() defaults `scheme = "minor"` = "this IS the theme's body font" -> Excel resolves
#       the font from the WORKBOOK THEME (set via set_base_font -> font_text = Condensed), IGNORING the
#       explicit `name`. tab_xl passes `scheme = ""` so a cell renders in the font it names (Phase 14l).
#     * wb_add_font(update=) is buggy over large ranges with scattered cells -> aggregate a cell's whole
#       font descriptor and apply with update=FALSE (the precompose above sidesteps it entirely).

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

# Excel forbids  \ / ? * : [ ]  in a worksheet name. openxlsx2 does not reject such a name: it
# silently rewrites each illegal character to a space and warns ("Fixing: removing illegal
# characters found in sheet name"). Regression tables trip this routinely -- Phase 12d names OR
# columns "<level> vs <ref>: OR", and the colon reaches the sheet title. Applying the substitution
# ourselves leaves openxlsx2 nothing to fix, so the warning is gone rather than merely muffled.
# PURE (no workbook) -- unit-tested against openxlsx2's own output in test-xl-backend.R.
#
# DESIGN: verified identical to openxlsx2 for every illegal character EXCEPT backslash, where
# openxlsx2 emits TWO spaces for one "\" (its own quirk); we emit one. Unreachable here -- titles
# are built from variable names and level labels -- and one space is the correct reading.
# WARNING: fixed = TRUE, one character at a time -- do NOT "simplify" this to a bracket-expression
# regex. In a POSIX bracket expression a backslash is a literal, not an escape, so the obvious
# "[\\\\/?*:\\[\\]]" silently matches nothing of what it looks like it matches.
xl_clean_sheet_name <- function(x) {
  for (ch in c("\\", "/", "?", "*", ":", "[", "]")) x <- gsub(ch, " ", x, fixed = TRUE)
  x
}

# gridlines are turned off at sheet creation (replaces v1 showGridLines)
xlb_add_sheet <- function(wb, title)
  wb$add_worksheet(sheet = title, grid_lines = FALSE)

xlb_freeze <- function(wb, sheet, active_row = 3L)
  wb$freeze_pane(sheet = sheet, first_active_row = active_row, first_col = TRUE)

# DESIGN: openxlsx2 renamed the NA-handling arg across versions: `na.strings` (oldest, dot form) ->
# `na_strings` -> `na` (current). Resolve the EXACT formal name from the method itself and pass by name.
# Two failures this guards, both on the jamovi-bundled openxlsx2 (Windows-side):
#   * a literal `na = NULL` partial-matches BOTH `name` and `na_strings`/`na.strings` -> "argument N
#     matches multiple formal arguments" (an earlier Excel-export crash);
#   * guessing `na_strings` when the real formal is `na.strings` (dot) makes the arg UNUSED, so the
#     default (write the Excel #N/A error for NA cells) applies -> summary-stat / p-value rows showed
#     "#N/A" in empty cells. Reading the real name off the formals fixes both.
xlb_na_argname <- function(wb) {
  fmls <- tryCatch(names(formals(wb$add_data)), error = function(e) character())
  cand <- c("na", "na_strings", "na.strings")
  hit  <- cand[cand %in% fmls]
  if (length(hit)) hit[[1]] else "na_strings"
}

# `list(NULL)` (single-bracket assign) keeps a NULL-VALUED element in the arg list -> passes the
# resolved NA arg as NULL (blank cells, not #N/A); a `[[<-` NULL would drop it entirely.
xlb_add_data <- function(wb, ...) {
  args <- list(..., apply_cell_style = FALSE)
  args[xlb_na_argname(wb)] <- list(NULL)
  do.call(wb$add_data, args)
}

# raw numbers written; blank cells for NA; apply_cell_style = FALSE -> no openxlsx2 auto-styling
# (tab_xl controls every style itself).
xlb_write_data <- function(wb, sheet, x, row, col, col_names = TRUE)
  xlb_add_data(wb, sheet = sheet, x = x, start_row = row, start_col = col, col_names = col_names)

xlb_write_cell <- function(wb, sheet, dims, x)
  xlb_add_data(wb, sheet = sheet, x = x, dims = dims, col_names = FALSE)

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
    # z11: `italic`/`underline` carry the print palette's typography into the Excel legend, so the
    # rich-text break-word wears the same face as the cells. FALSE everywhere under a colour palette.
    # `underline` is the three-value vocabulary ("" / "single" / "double"); openxlsx2 writes the
    # string straight into <u val=...>, so a doubled rule survives into the legend run too.
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

# DESIGN: xl_coalesce() packs non-contiguous same-style cells into ONE multi-area `dims` joined with a
# comma (e.g. "C7:E8,F4:F8") -- efficient, and accepted by a current openxlsx2. But the OLDER openxlsx2
# bundled inside jamovi (same build the xlb_na_argname shim above works around) has a single-range dims
# validator that rejects a comma with exactly "dims must be something like A1 or A1:B2." -- the
# Excel-export crash. So split at the CALL boundary: every engine call gets one contiguous range, which
# is semantically identical (the same code/style over each sub-rectangle) and works on BOTH openxlsx2
# versions. Keep xl_coalesce's packing (fewest calls) upstream; only the emit is per-range.
xlb_dims_each <- function(dims, f) {
  if (length(dims) != 1L || is.na(dims) || !nzchar(dims)) return(invisible(NULL))
  for (part in strsplit(dims, ",", fixed = TRUE)[[1]]) if (nzchar(part)) f(part)
  invisible(NULL)
}

# numFmt is applied as a grouped merging pass (it merges onto the precomposed xf, cross-aspect).
xlb_numfmt <- function(wb, sheet, dims, code)
  xlb_dims_each(dims, function(d) wb$add_numfmt(sheet = sheet, dims = d, numfmt = code))

# apply a precomposed style id over a (possibly multi-area) dims -- one set_cell_style per range.
xlb_set_cell_style <- function(wb, sheet, dims, style)
  xlb_dims_each(dims, function(d) wb$set_cell_style(sheet = sheet, dims = d, style = style))

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
