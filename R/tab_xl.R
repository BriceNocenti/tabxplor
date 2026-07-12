# PURPOSE: Export tabxplor tables to Excel with formatting and colors via openxlsx2.
# ROLE: Primary export format for sharing tables with non-R users. Phase 10h: single-tab-first with a
#       list method; consumes the shared exporter prep (R/tab-export-prep.R) for role detection /
#       references / bold rows, the two-channel colour engine (fmt_color_channels), and the openxlsx2
#       backend wrappers (R/tab-xl-backend.R). tab_xl_plan_one() does the pure per-table CPU (raw
#       values + numFmt codes + colour slots + geometry); xl_write_table() issues the openxlsx2 calls.
# KEY CONSTRAINTS:
#   - openxlsx2 is Suggests-only -- the ONE requireNamespace() guard is in tab_xl(); every engine call
#     goes through the unguarded xlb_* wrappers.
#   - Export-Parity: tab_xl writes the RAW get_num() value; Excel formats it via the per-cell codes
#     from format(x, syntax = "excel") (fmt_class.R excel_numfmt_code) -- the single display source of
#     truth. Significance stars are folded into the numFmt code (0.0%"***"), gated by the SAME option
#     as the text path (getOption("tabxplor.stars")), so the cell stays a real number.
#   - Shared-style + range: colours/numFmt are applied per (style) group over the fewest coalesced
#     multi-area dims (xl_coalesce / xl_rect_dims), never per cell.
#   - Styling layers via openxlsx2's automatic cross-aspect merge + update= within borders/fonts
#     (see R/tab-xl-backend.R). The plan builder is pure; the workbook is assembled serially (the
#     openxlsx2 write dominates and is inherently serial -- parallelising it was measured not worth it).

#' Excel output for tabxplor tables, with formatting and colors
#' @description To modify the colors used into the Excel table, you can change the
#' global options with \code{\link{set_color_style}} and \code{\link{set_color_breaks}}.
#' @param tabs A table made with \code{\link{tab}}, \code{\link{tab_many}} or
#' \code{\link{tab_plain}}, or a list of such tables.
#' @param path,replace,open The name, and possibly the path, of the Excel file to
#' create (possibly without the .xlsx extension). Default path to temporary directory.
#' Set global option \code{"tabxplor.export_dir"} with \code{link[base:options]{options}}
#' to change default directory. By default replace is `TRUE` when `path` is provided,
#' `FALSE` when `path` is not provided.
#' Use \code{replace = TRUE} to overwrite existing files. Use \code{open = FALSE}
#' if you don't want to automatically open the tables in Excel (or another
#' software associated with .xlsx files).
#' @param colnames_rotation Rotate the names of columns to an angle (in degrees).
#' @param remove_tab_vars By default, \code{tab_vars} columns are removed to gain space.
#' Set to \code{FALSE} to keep them.
#' @param colwidth The standard width for numeric columns, as a number.
#' Set to \code{"auto"} to let Excel choose.
#' @param transpose Set to \code{TRUE} to transpose each table before export (rows become
#'   columns). Useful for column percentages tables with several row variables.
#' @param conditional_format `r lifecycle::badge("experimental")` Reserved for a future opt-in
#'   to use Excel conditional formatting instead of hard cell colours. Not yet implemented: setting
#'   it emits a message and falls back to the (fast, exact) hard-style colouring.
#' @param titles The titles of the different tables, as a character vector. When missing
#'   titles are given based on the names of the variables.
#' @param font_text,font_num Font for text and for numbers.
#' @param text_size,text_size_headers,text_size_subtext Font sizes of text elements.
#' @param print_color_legend Should the color legends be printed with the subtexts ?
#' @param sheets The Excel sheets options :
#' \itemize{
#'   \item \code{"tabs"}: a new sheet is created for each table
#'   \item \code{"unique"}: all tables are on the same sheet
#'   \item \code{"auto"}: subsequent tables with the same column vars are printed on the
#'    same sheets
#' }
#' @param n_min `r lifecycle::badge("deprecated")` The small-n greying is removed in 1.4.0. The
#' argument is kept for back-compatibility but no longer does anything; use `tab(n_min = )`, which
#' blanks or drops small-n cells at display and flows into every export.
#' @param hide_near_zero `r lifecycle::badge("deprecated")` Removed in 1.4.0 (a rarely used,
#' slow feature): the argument is kept for back-compatibility but no longer does anything.
#' @param color_type By default, the text is colored. Set to \code{"bg"} to color
#' the background instead.
#'
#' @return  The table(s) with formatting and colors in an Excel file, as a side effect.
#'  Invisibly returns \code{tabs}.
#' @export
#'
#' @examples
#' \donttest{
#' forcats::gss_cat %>%
#'   tab(marital, race, pct = "row", color = "diff") %>%
#'   tab_xl()
#'   }
tab_xl <-
  function(tabs, path = NULL, replace = FALSE, open = rlang::is_interactive(),
           colnames_rotation = 0, remove_tab_vars = TRUE,
           colwidth = 10, print_color_legend = TRUE,
           sheets = "auto", n_min = 0, titles,
           font_text = "DejaVu Sans Condensed", font_num = "DejaVu Sans",
           text_size = 10, text_size_headers = 9, text_size_subtext = 9,
           hide_near_zero = Inf, color_type = "text",
           transpose = FALSE, conditional_format = FALSE) {

    if (!requireNamespace("openxlsx2", quietly = TRUE)) {
      stop(paste0("Package \"openxlsx2\" needed for this function to work. ",
                  "You can install it with : install.packages('openxlsx2')"),
           call. = FALSE)
    }

    if (length(replace) == 0) replace <- length(path) != 0

    # Phase 10g soft-deprecations (kept for back-compat but inert; warn only on a non-default value):
    #   - hide_near_zero: near-zero greying (rarely used, slow).
    #   - n_min: the small-n greying is dropped; use tab(n_min = ), which blanks/drops small-n cells.
    if (!identical(hide_near_zero, Inf)) {
      lifecycle::deprecate_soft("1.4.0", "tab_xl(hide_near_zero)")
    }
    if (!identical(n_min, 0) && !identical(n_min, 0L)) {
      lifecycle::deprecate_soft("1.4.0", "tab_xl(n_min)", "tab(n_min)")
    }
    # Phase 10h: conditional_format is reserved but not implemented (the hard-style path is fast,
    # exact and small; faithful CF would need hidden helper columns). Fall back with a message.
    if (isTRUE(conditional_format)) {
      cli::cli_inform(c("!" = paste0("{.arg conditional_format} is experimental and not yet ",
                                     "implemented; using the (fast, exact) hard cell colours.")))
    }

    tabs_base <- tabs
    # Graceful degrade (single input): write the raw frame (+ a message) instead of crashing when the
    # input can't be read as a tabxplor table.
    rv <- if (is.data.frame(tabs)) tab_render_vars(tabs) else list(degrade = FALSE)
    if (isTRUE(rv$degrade)) {
      tab_degrade_inform(rv$reason)
      xlb_write_xlsx(tibble::as_tibble(tabs), tab_xl_resolve_path(path, replace))
      if (isTRUE(open)) xlb_open(tab_xl_resolve_path(path, replace))
      return(invisible(tabs_base))
    }
    if (is.data.frame(tabs)) tabs <- list(tabs)

    tabs <- purrr::map(tabs, tab_pvalue_lines) # chi2 pvalue to lines
    if (isTRUE(transpose)) tabs <- purrr::map(tabs, tab_transpose)

    colwidth <- vctrs::vec_recycle(colwidth, length(tabs))

    # === Shared exporter prep (Phase 10g) ==========================================
    # Role detection (fmt / other / total columns, total-block borders, references, bold rows) is
    # derived ONCE by the shared framework (R/tab-export-prep.R). compact = FALSE keeps one prep-table
    # per input tab (each -> its own sheet region). Colours stay on tab_xl's own two-channel path
    # (fmt_color_channels), which -- unlike the prep's text-only roles$color_cols -- also catches
    # background-only columns.
    prep <- tab_export_prep(
      tabs, backend = "xl", compact = FALSE, drop_tab_vars = remove_tab_vars,
      list_method = TRUE, compute = c("refs", "bold"),
      color_type = color_type, color_legend = print_color_legend, what = "tab_xl()"
    )
    rd <- prep$tables

    # Graceful degrade: any unreadable list member is written as a plain sheet, with a message.
    if (any(purrr::map_lgl(rd, ~ isTRUE(.$vars$degrade)))) {
      purrr::walk(rd, ~ if (isTRUE(.$vars$degrade)) tab_degrade_inform(.$vars$reason))
      xlb_write_xlsx(purrr::map(rd, ~ tibble::as_tibble(.$tab)), tab_xl_resolve_path(path, replace))
      if (isTRUE(open)) xlb_open(tab_xl_resolve_path(path, replace))
      return(invisible(tabs_base))
    }

    tabs           <- purrr::map(rd, "tab")           # ungrouped, tab_vars dropped when requested
    roles          <- purrr::map(rd, "roles")
    row_vars       <- purrr::map(rd, ~ .$vars$row_var)
    tab_vars       <- purrr::map(rd, ~ .$vars$tab_vars)
    col_vars_plain <- purrr::map(rd, ~ .$vars$col_vars)

    stopifnot(sheets %in% c("tabs", "unique", "auto") |
                (is.integer(sheets) & length(sheets) == length(tabs)))
    sheet <-
      if (is.character(sheets)) {
        switch(sheets,
               "tabs"   = seq_along(tabs),
               "unique" = rep(1L, length(tabs)),
               "auto"   = purrr::map2_lgl(col_vars_plain, dplyr::lag(col_vars_plain),
                                          ~ !identical(sort(.x), sort(.y))) |> cumsum())
      } else if (is.integer(sheets)) {
        sheets
      }

    # subtext (+ colour legend) computed once on the main process.
    subtext <- purrr::map(tabs, get_subtext) |>
      purrr::map(~ stringr::str_replace_all(., "\\\n", " ") |> stringr::str_replace_all(" +", " "))
    if (isTRUE(print_color_legend)) {
      color_legend <- purrr::map(tabs, ~ suppressWarnings(
        tab_color_legend(., colored = FALSE, add_color_and_diff_types = TRUE)))
      subtext <- purrr::map2(subtext, color_legend, ~ c(.y, .x))
    }

    if (missing(titles)) {
      titles <- purrr::pmap_chr(list(tabs, row_vars, col_vars_plain, tab_vars),
                                ~ tab_get_titles(..1, ..2, ..3, ..4))
    } else {
      titles <- vctrs::vec_recycle(titles, length(tabs))
    }

    # Sheet-stacking offsets: within a sheet each stacked table starts below the previous one
    # (rows + subtext + 5 blank). Absolute geometry is derived from `start` in the plan builder.
    newsheet <- sheet != dplyr::lag(sheet, default = -1L)
    start <- tibble::tibble(newsheet, rows = purrr::map_int(tabs, nrow),
                            sub = purrr::map_int(subtext, length)) |>
      dplyr::group_by(gr = cumsum(as.integer(.data$newsheet))) |>
      dplyr::mutate(start = dplyr::lag(cumsum(.data$rows + .data$sub + 5L), default = 0L) + 1L) |>
      dplyr::pull(.data$start)

    sheet_titles <- titles[newsheet] |> stringr::str_sub(1, 25)
    sheet_titles <- dplyr::if_else(duplicated(sheet_titles),
                                   stringr::str_c(sheet_titles, ".2"), sheet_titles)
    nb <- 2
    while (length(unique(sheet_titles)) != length(sheet_titles)) {
      nb <- nb + 1
      sheet_titles <- dplyr::if_else(
        duplicated(sheet_titles),
        stringr::str_c(stringr::str_remove(sheet_titles, "..$"), ".", nb), sheet_titles)
    }

    # Colour palettes built ONCE (Phase 5): TEXT channel -> font colour (in the color_type family),
    # BACKGROUND channel -> cell fill (bg palette). 11 hex per palette, indexed by slot integer.
    opts <- list(
      font_num          = font_num,
      font_text         = font_text,
      text_size         = text_size,
      colnames_rotation = colnames_rotation,
      text_size_headers = text_size_headers,
      text_size_subtext = text_size_subtext,
      text_pal          = get_color_style("color_code", theme = "light", type = color_type),
      bg_pal            = get_color_style("color_code", theme = "light", type = "bg"),
      stars_on          = isTRUE(getOption("tabxplor.stars", TRUE))
    )

    # === Per-table plans (pure: raw values + numFmt codes + colour slots + font plan + geometry) ===
    # tab_xl_plan_one() carries no workbook and is side-effect-free; the workbook is assembled serially
    # from the plans below. (Parallelising the plan build was measured NOT worth it -- the openxlsx2
    # WRITE dominates the time and is inherently serial; see dev/benchmarks/results_1.4.0/phase10h_*.)
    plans <- purrr::pmap(
      list(tab = tabs, roles = roles, bold_rows = purrr::map(rd, "bold_rows"),
           start = start, sheet = sheet, title = titles, subtext = subtext, colwidth = colwidth),
      tab_xl_plan_one, o = opts
    )

    # === Assemble the workbook on the main process (serial) =======================================
    wb <- xlb_new_workbook()
    xlb_base_font(wb, font_text, text_size)
    purrr::walk(sheet_titles, ~ xlb_add_sheet(wb, .))
    purrr::walk(unique(sheet), ~ xlb_freeze(wb, ., 3L))
    purrr::walk(plans, ~ xl_write_table(wb, ., opts))

    path <- tab_xl_resolve_path(path, replace)
    xlb_save(wb, path)
    if (isTRUE(open)) xlb_open(path)

    invisible(tabs_base)
  }


# Resolve the export path: default to options("tabxplor.export_dir") or tempdir()/Tab, ensure the
# directory exists, add the .xlsx extension, and auto-number when replace = FALSE and the file exists.
#' @keywords internal
tab_xl_resolve_path <- function(path, replace) {
  if (is.null(path)) {
    path <- getOption("tabxplor.export_dir")
    if (is.null(path)) path <- file.path(tempdir(), "Tab") else path <- file.path(path)
  } else {
    path <- path[[1]]
  }
  if (stringr::str_detect(path, "\\\\|/")) {
    dir_path <- path |> stringr::str_remove("\\\\[^\\\\]+$|/[^/]+$")
    if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
  }
  path_name <- stringr::str_remove(path, "\\.xlsx$")
  if (!stringr::str_detect(path, "\\.xlsx$")) path <- stringr::str_c(path, ".xlsx")
  if (isFALSE(replace)) {
    i <- 0
    while (file.exists(path)) {
      i <- i + 1
      path <- stringr::str_c(path_name, i, ".xlsx")
    }
  }
  path
}


# Pure per-table plan: raw values to write + numFmt codes (stars folded) + colour slots + absolute
# geometry. Side-effect-free (no workbook), so the workbook is assembled serially from the plans.
# Geometry (given `start`): title row = start; header row = start + 1; data rows = start + 2 ..
# start + 1 + nrow; subtext below. Column role indices come from the shared prep `roles`.
#' @keywords internal
tab_xl_plan_one <- function(tab, roles, bold_rows, start, sheet, title, subtext, colwidth, o) {
  n   <- nrow(tab)
  ncl <- ncol(tab)
  data_row0  <- start + 1L                      # data row i -> i + data_row0
  header_row <- start + 1L
  data_rows  <- seq_len(n) + data_row0
  last_row   <- start + 1L + n

  fmt_cols    <- roles$fmt_cols
  txt_cols    <- roles$other_cols
  row_var_col <- roles$row_var_col
  totcols     <- roles$totcols
  ref_cols    <- which(is_refcol(tab))

  # a column is coloured if it carries a text OR a background colour channel (the prep's text-only
  # roles$color_cols would miss background-only columns).
  color_cols <- which(purrr::map_lgl(tab, function(col) {
    if (!is_fmt(col)) return(FALSE)
    ct <- get_color(col); cb <- get_color_bg(col)
    (length(ct) != 0L && !is.na(ct) && !ct %in% c("", "no")) ||
      (length(cb) != 0L && !is.na(cb) && !cb %in% c("", "no"))
  }))

  cv_names      <- get_col_var(tab)
  start_col_var <- which(cv_names != "" & cv_names != dplyr::lag(cv_names, default = NA_character_))

  # Number formats: format(syntax = "excel") is the single display source of truth. Fold significance
  # stars into the numFmt literal (0.0%"***") when stars are on, keeping the cell a real number; a
  # "TEXT"-coded cell (ci display) maps to Excel's "@" text format; NA codes stay General.
  numfmt <- if (length(fmt_cols)) purrr::map_dfr(fmt_cols, function(ci) {
    col  <- tab[[ci]]
    code <- format(col, syntax = "excel")
    if (o$stars_on) {
      st   <- get_stars(col)
      fold <- !is.na(code) & code != "TEXT" & nzchar(st)
      code[fold] <- paste0(code[fold], '"', st[fold], '"')
    }
    code[!is.na(code) & code == "TEXT"] <- "@"
    tibble::tibble(col = as.integer(ci), row = seq_along(code) + data_row0, code = code)
  }) else tibble::tibble(col = integer(), row = integer(), code = character())
  numfmt <- dplyr::filter(numfmt, !is.na(.data$code))

  # Colour slots (two channels) from the vectorised engine. Text channel -> font (bold + colour,
  # folded into the font plan below); background channel -> cell fill (applied by the writer).
  colour <- if (length(color_cols)) purrr::map_dfr(color_cols, function(ci) {
    ch   <- fmt_color_channels(tab[[ci]])
    rows <- seq_along(ch$text_slot) + data_row0
    dplyr::bind_rows(
      tibble::tibble(col = as.integer(ci), row = rows, slot = ch$text_slot, channel = "text"),
      tibble::tibble(col = as.integer(ci), row = rows, slot = ch$bg_slot,   channel = "bg"))
  }) else tibble::tibble(col = integer(), row = integer(), slot = integer(), channel = character())
  colour <- dplyr::filter(colour, .data$slot > 0L)

  subtext_clean <- subtext[!is.na(subtext) & subtext != ""]
  subtext_rows  <- if (length(subtext_clean)) seq_along(subtext_clean) + n + start + 1L else integer()
  ref_rows      <- bold_rows + start + 1L
  ref_row_cols  <- union(fmt_cols, txt_cols)

  # Unified FONT plan: openxlsx2's wb_add_font(update=) is buggy over large ranges when the sheet has
  # scattered cells (title/subtext), so every font need is aggregated per cell into ONE complete
  # descriptor applied with update = FALSE (a full replace) -- cross-aspect merge keeps numFmt / fill /
  # border / alignment intact. Base name/size are filled by the writer. See R/tab-xl-backend.R.
  mk_src <- function(rows, cols, name = NA_character_, size = NA_real_, bold = FALSE,
                     color = NA_character_) {
    if (!length(rows) || !length(cols)) return(NULL)
    g <- tidyr::expand_grid(row = as.integer(rows), col = as.integer(cols))
    dplyr::mutate(g, name = name, size = size, bold = bold, color = color)
  }
  txt_colour <- dplyr::filter(colour, .data$channel == "text")
  fonts <- dplyr::bind_rows(
    mk_src(data_rows, fmt_cols, name = o$font_num),                              # numeric font
    mk_src(header_row, seq_len(ncl), bold = TRUE, size = o$text_size_headers),   # headers
    mk_src(c(header_row, data_rows), ref_cols, bold = TRUE),                     # reference cols
    mk_src(ref_rows, ref_row_cols, bold = TRUE),                                 # reference rows
    mk_src(start, 1L, bold = TRUE, size = 12),                                   # title
    mk_src(subtext_rows, 1L, size = o$text_size_subtext),                        # subtext
    if (nrow(txt_colour)) tibble::tibble(row = txt_colour$row, col = txt_colour$col,
                                         name = NA_character_, size = NA_real_, bold = TRUE,
                                         color = o$text_pal[txt_colour$slot])    # text-channel colour
  )
  if (nrow(fonts)) {
    fonts <- fonts |>
      dplyr::group_by(.data$row, .data$col) |>
      dplyr::summarise(
        name  = c(name[!is.na(name)], NA_character_)[1],
        size  = c(size[!is.na(size)], NA_real_)[1],
        bold  = any(.data$bold),
        color = c(color[!is.na(color)], NA_character_)[1],
        .groups = "drop")
  }

  list(
    sheet = sheet, ncol = ncl,
    title = title, title_row = start,
    subtext = subtext_clean, subtext_row = n + start + 2L,
    data = dplyr::mutate(tab, dplyr::across(where(is_fmt), get_num)) |> tibble::as_tibble(),
    header_row = header_row, data_rows = data_rows, last_row = last_row,
    all_cols = seq_len(ncl),
    fmt_cols = fmt_cols, txt_cols = txt_cols, row_var_col = row_var_col,
    totcols = totcols, ref_cols = ref_cols, start_col_var = start_col_var,
    tot_rows      = roles$totrows        + start + 1L,
    tot_rows_1    = roles$totblock_top   + start + 1L,
    tot_rows_last = roles$totblock_bottom + start + 1L,
    ref_rows      = ref_rows,
    end_group     = utils::head(roles$new_group, -1L) + start + 1L,
    numfmt = numfmt, colour = colour, fonts = fonts, colwidth = colwidth
  )
}


# Per-sheet writer: issues the openxlsx2 calls for ONE table's region. Every style is applied ONCE
# over the fewest coalesced multi-area dims (xl_rect_dims for rectangular passes, xl_coalesce for
# per-cell numFmt/colour passes). Layering relies on openxlsx2's cross-aspect merge + update=.
#' @keywords internal
xl_write_table <- function(wb, plan, o) {
  s      <- plan$sheet
  hdr    <- plan$header_row
  drows  <- plan$data_rows
  hdrows <- c(hdr, drows)                          # header + data rows (the styled block)
  allc   <- plan$all_cols

  # values: raw numbers + header, title, subtext (fonts are applied together below)
  xlb_write_data(wb, s, plan$data, hdr, 1L)
  xlb_write_cell(wb, s, xl_cell(plan$title_row, 1L), plan$title)
  if (length(plan$subtext)) {
    sr <- plan$subtext_row
    xlb_write_cell(wb, s, xl_cell(sr, 1L), plan$subtext)
    xlb_align(wb, s, xl_rect_dims(sr:(sr + length(plan$subtext) - 1L), 1L),
              h = "left", v = "center")
  }

  draw_border <- function(rows, cols, sides, style = "thin") {
    d <- xl_rect_dims(rows, cols)
    if (!is.na(d)) xlb_border(wb, s, d, sides = sides, style = style)
  }
  align <- function(rows, cols, h = NULL, v = NULL, wrap = NULL, rotation = NULL) {
    d <- xl_rect_dims(rows, cols)
    if (!is.na(d)) xlb_align(wb, s, d, h = h, v = v, wrap = wrap, rotation = rotation)
  }

  # --- borders ---
  draw_border(hdr:plan$last_row, allc, c("top", "bottom", "left", "right"))  # surrounding box
  draw_border(plan$last_row, allc, "bottom")                                 # bottom line
  draw_border(hdrows, plan$totcols, c("left", "right"))                      # total columns
  draw_border(hdrows, plan$start_col_var, "left")                            # col_var starts
  draw_border(hdrows, 1L, "left")                                            # first col
  draw_border(hdrows, plan$ncol, "right")                                    # last col
  draw_border(hdr, allc, c("top", "bottom"))                                 # header row
  draw_border(plan$tot_rows_1, allc, "top")                                  # total block top
  draw_border(plan$tot_rows_last, allc, "bottom")                            # total block bottom
  draw_border(plan$end_group, allc, "bottom", style = "double")              # between-group double

  # --- alignment ---
  align(drows, allc, v = "top")                                              # base valign
  if (o$colnames_rotation == 0) {
    align(hdr, allc, h = "center", v = "bottom", wrap = TRUE)
  } else {
    align(hdr, allc, h = "left", v = "bottom", wrap = TRUE, rotation = o$colnames_rotation)
  }
  align(hdrows, plan$totcols, h = "left", v = "top")                         # total cols
  align(plan$tot_rows, plan$fmt_cols, h = "right", v = "top")                # total rows (numbers)
  align(plan$tot_rows, plan$txt_cols, h = "left",  v = "top", wrap = TRUE)   # total rows (text)
  align(plan$tot_rows, plan$totcols,  h = "left",  v = "top")                # total rows (bottom-left)

  # --- fonts: ONE complete descriptor per cell (numeric font + headers + refs + title/subtext +
  #     text-channel colour, aggregated in the plan), applied as a full replace over the fewest
  #     coalesced ranges. update = FALSE sidesteps the openxlsx2 range-update bug; cross-aspect merge
  #     preserves the numFmt / fill / border / alignment already set. ---
  if (nrow(plan$fonts)) {
    plan$fonts |>
      dplyr::mutate(name = dplyr::coalesce(.data$name, o$font_text),
                    size = dplyr::coalesce(.data$size, as.double(o$text_size))) |>
      dplyr::group_by(.data$name, .data$size, .data$bold, .data$color) |>
      dplyr::summarise(dims = xl_coalesce(.data$col, .data$row), .groups = "drop") |>
      purrr::pwalk(function(name, size, bold, color, dims)
        xlb_font(wb, s, dims, name = name, size = size,
                 bold  = if (isTRUE(bold)) TRUE else NULL,
                 color = if (!is.na(color)) color else NULL))
  }

  # --- number formats: one shared code applied over the fewest coalesced ranges ---
  if (nrow(plan$numfmt)) {
    plan$numfmt |>
      dplyr::group_by(.data$code) |>
      dplyr::summarise(dims = xl_coalesce(.data$col, .data$row), .groups = "drop") |>
      purrr::pwalk(function(code, dims) xlb_numfmt(wb, s, dims, code))
  }

  # --- colours: background channel -> cell fill (the text channel rides the font plan above) ---
  if (nrow(plan$colour)) {
    plan$colour |>
      dplyr::filter(.data$channel == "bg") |>
      dplyr::group_by(.data$slot) |>
      dplyr::summarise(dims = xl_coalesce(.data$col, .data$row), .groups = "drop") |>
      purrr::pwalk(function(slot, dims) xlb_fill(wb, s, dims, color = o$bg_pal[[slot]]))
  }

  # --- column widths / row heights ---
  if (length(plan$row_var_col)) xlb_col_widths(wb, s, plan$row_var_col, 30)
  rot <- o$colnames_rotation
  if (length(plan$fmt_cols)) {
    if (identical(plan$colwidth, "auto")) {
      w <- if (rot > 30 && rot < 60) 8
      else if (rot >= 60) 6 + 8 * cos(rot / 90 * pi / 2)
      else "auto"
      xlb_col_widths(wb, s, plan$fmt_cols, w)
    } else {
      xlb_col_widths(wb, s, plan$fmt_cols, as.double(plan$colwidth))
    }
  }
  if (rot > 0) xlb_row_heights(wb, s, plan$header_row, 13.8 + 105 * sin(rot / 90 * pi / 2))

  invisible(wb)
}


#' @keywords internal
tab_get_titles <- function(tabs, row, col, tab, max = 3) {
  res <- dplyr::case_when(
    row ==  "no_row_var" & length(col) <= max ~ paste(col, collapse = ", "),
    row ==  "no_row_var" & length(col) >  max ~ paste(col[1:max], "etc.",
                                                      collapse = ", "),
    all(col ==  "no_col_var")           ~ row,
    length(row) == 1 & length(col) <= max ~ paste(row, "by",
                                                  paste(col, collapse = ", ")),
    length(row) == 1 & length(col) >  max ~ paste(row, "by multi"),
  )
  if (!missing(tab)) {
    if (length(tab) >= 1) res <-
        if (length(tabs) >= 2) {
          paste0(res, " (tabbed by ", paste(tab, collapse = ", "), ")")
        } else {
          paste0(res, " (tabbed by ", tab, ")")
        }
  }
  res
}
