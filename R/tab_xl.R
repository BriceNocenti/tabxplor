# PURPOSE: Export tabxplor tables to Excel with formatting and colors via openxlsx2.
# ROLE: Primary export format for sharing tables with non-R users. Phase 10h: single-tab-first with a
#       list method; consumes the shared exporter prep (R/tab-export-prep.R) for role detection /
#       references / bold rows, the two-channel colour engine (fmt_color_channels), and the openxlsx2
#       backend (R/tab-xl-backend.R). tab_xl_plan_one() does the pure per-table CPU (raw values +
#       numFmt codes + a precomposed per-cell STYLE grid via xl_build_styles); xl_write_table() writes
#       the values, applies the styles by id (xl_apply_styles), then the numFmt merging pass.
# KEY CONSTRAINTS:
#   - openxlsx2 is Suggests-only -- the ONE requireNamespace() guard is in tab_xl(); every engine call
#     goes through the unguarded xlb_* wrappers or xl_apply_styles' create_*/set_cell_style compose.
#   - Export-Parity: tab_xl writes the RAW get_num() value; Excel formats it via the per-cell codes
#     from format(x, syntax = "excel") (fmt_class.R excel_numfmt_code) -- the single display source of
#     truth. Significance stars are folded into the numFmt code (0.0%"***"), gated by the SAME option
#     as the text path (getOption("tabxplor.stars")), so the cell stays a real number.
#   - Shared-style fast path: each cell's FULL style (font+fill+border+alignment) is precomposed and
#     applied ONCE by id (set_cell_style) over the fewest coalesced multi-area dims (xl_coalesce) --
#     far fewer + cheaper openxlsx2 calls than a wb_add_* per aspect. numFmt merges on afterwards.
#   - The plan builder is pure; the workbook is assembled serially (the openxlsx2 write dominates and
#     is inherently serial -- parallelising it was measured not worth it).

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

    # Phase 10i-B: materialise the display extras (p-value rows now; add_n `n` column + add_pct in
    # Increment 2) BEFORE transpose, matching the historical order. backend = "xl" keeps a real `n`
    # column. tab_export_prep()'s later re-materialise (L131) is then a no-op (attrs already consumed).
    tabs <- purrr::map(tabs, tab_materialize_extras, backend = "xl", pvalue = TRUE)
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

  # Background-channel colour -> per-cell fill hex.
  bg <- dplyr::filter(colour, .data$channel == "bg")
  bg_fill <- if (nrow(bg)) tibble::tibble(row = bg$row, col = bg$col, fill = o$bg_pal[bg$slot])
             else tibble::tibble(row = integer(), col = integer(), fill = character())

  # Precompose the ENTIRE per-cell style (font + fill + border + alignment) into the fewest distinct
  # styles, each with its coalesced dims -- the openxlsx2 "shared styles, applied by id" fast path.
  styles <- xl_build_styles(
    header_row = header_row, data_rows = data_rows, last_row = last_row, ncl = ncl,
    fmt_cols = fmt_cols, txt_cols = txt_cols, totcols = totcols, start_col_var = start_col_var,
    tot_rows      = roles$totrows         + start + 1L,
    tot_rows_1    = roles$totblock_top    + start + 1L,
    tot_rows_last = roles$totblock_bottom + start + 1L,
    end_group     = utils::head(roles$new_group, -1L) + start + 1L,
    fonts = fonts, bg_fill = bg_fill, title_row = start, subtext_rows = subtext_rows, o = o
  )

  list(
    sheet = sheet,
    title = title, title_row = start,
    subtext = subtext_clean, subtext_row = n + start + 2L,
    data = dplyr::mutate(tab, dplyr::across(where(is_fmt), get_num)) |> tibble::as_tibble(),
    header_row = header_row,
    fmt_cols = fmt_cols, row_var_col = row_var_col, colwidth = colwidth,
    styles = styles, numfmt = numfmt
  )
}


# Build the per-cell full style grid (font + fill + border + alignment) for one table, grouped into
# the fewest DISTINCT styles, each with a coalesced multi-area dims. numFmt is NOT here (it is applied
# by the writer as a separate merging pass). Borders are painted onto 4 side matrices (0 none / 1 thin
# / 2 double), alignment onto zone matrices (base -> header -> total cols -> total rows, last wins).
#' @keywords internal
xl_build_styles <- function(header_row, data_rows, last_row, ncl, fmt_cols, txt_cols, totcols,
                            start_col_var, tot_rows, tot_rows_1, tot_rows_last, end_group,
                            fonts, bg_fill, title_row, subtext_rows, o) {
  block_rows <- header_row:last_row
  nb  <- length(block_rows)
  idx <- function(r) match(intersect(r, block_rows), block_rows)          # abs row -> block index
  ci  <- function(c) intersect(as.integer(c), seq_len(ncl))

  # borders: 4 side matrices
  bt <- bb <- bl <- br <- matrix(0L, nb, ncl)
  prow <- function(M, rows, v) { i <- idx(rows); if (length(i)) M[i, ] <- v; M }
  pcol <- function(M, cols, v) { c <- ci(cols); if (length(c)) M[, c] <- v; M }
  bt <- prow(bt, c(header_row, tot_rows_1), 1L)                           # surround/header top + block top
  bb <- prow(bb, c(header_row, last_row, tot_rows_last), 1L)             # header/surround/bottomline/block bottom
  bl <- pcol(bl, c(1L, totcols, start_col_var), 1L)                       # first col / total cols / col_var starts
  br <- pcol(br, c(ncl, totcols), 1L)                                     # last col / total cols
  bb <- prow(bb, end_group, 2L)                                           # between-group double (wins)

  # alignment: character/logical matrices, painted general -> specific (last wins)
  ah <- matrix(NA_character_, nb, ncl); av <- matrix("", nb, ncl)
  aw <- matrix(FALSE, nb, ncl);         ar <- matrix(0L, nb, ncl)
  di <- idx(data_rows); if (length(di)) av[di, ] <- "top"                 # data base valign
  hi <- idx(header_row)                                                   # header
  if (o$colnames_rotation == 0) { ah[hi, ] <- "center" } else { ah[hi, ] <- "left"; ar[hi, ] <- o$colnames_rotation }
  av[hi, ] <- "bottom"; aw[hi, ] <- TRUE
  tc <- ci(totcols)                                                       # total cols (header + data): left/top
  if (length(tc)) { ah[, tc] <- "left"; av[, tc] <- "top"; aw[, tc] <- FALSE; ar[, tc] <- 0L }
  tri <- idx(tot_rows)                                                    # total rows
  if (length(tri)) {
    fc <- ci(fmt_cols); if (length(fc)) { ah[tri, fc] <- "right"; av[tri, fc] <- "top"; aw[tri, fc] <- FALSE }
    xc <- ci(txt_cols); if (length(xc)) { ah[tri, xc] <- "left";  av[tri, xc] <- "top"; aw[tri, xc] <- TRUE }
    if (length(tc))    { ah[tri, tc] <- "left";  av[tri, tc] <- "top"; aw[tri, tc] <- FALSE }
  }

  # assemble the per-cell grid
  grid <- tidyr::expand_grid(bi = seq_len(nb), col = seq_len(ncl))
  ix   <- cbind(grid$bi, grid$col)
  cells <- tibble::tibble(
    row = block_rows[grid$bi], col = grid$col,
    bt = bt[ix], bb = bb[ix], bl = bl[ix], br = br[ix],
    ah = ah[ix], av = av[ix], aw = aw[ix], ar = ar[ix])
  # overlay per-cell font (name/size/bold/colour); default to base text font
  bkey <- paste(cells$row, cells$col, sep = ":")
  fm   <- if (nrow(fonts)) match(bkey, paste(fonts$row, fonts$col, sep = ":")) else rep(NA_integer_, nrow(cells))
  cells$fname  <- dplyr::coalesce(fonts$name[fm],  o$font_text)
  cells$fsize  <- dplyr::coalesce(fonts$size[fm],  as.double(o$text_size))
  cells$fbold  <- !is.na(fm) & fonts$bold[fm]
  cells$fcolor <- fonts$color[fm]
  # overlay per-cell fill
  lm <- if (nrow(bg_fill)) match(bkey, paste(bg_fill$row, bg_fill$col, sep = ":")) else rep(NA_integer_, nrow(cells))
  cells$fill <- bg_fill$fill[lm]

  # title + subtext cells (their own simple styles)
  extra <- dplyr::bind_rows(
    tibble::tibble(row = title_row, col = 1L, bt = 0L, bb = 0L, bl = 0L, br = 0L,
                   ah = NA_character_, av = "", aw = FALSE, ar = 0L,
                   fname = o$font_text, fsize = 12, fbold = TRUE, fcolor = NA_character_, fill = NA_character_),
    if (length(subtext_rows)) tibble::tibble(row = subtext_rows, col = 1L, bt = 0L, bb = 0L, bl = 0L, br = 0L,
                   ah = "left", av = "center", aw = FALSE, ar = 0L,
                   fname = o$font_text, fsize = as.double(o$text_size_subtext), fbold = FALSE,
                   fcolor = NA_character_, fill = NA_character_))
  cells <- dplyr::bind_rows(cells, extra)

  # group into distinct styles + coalesce each style's cells to the fewest multi-area dims
  cells |>
    dplyr::group_by(.data$fname, .data$fsize, .data$fbold, .data$fcolor, .data$fill,
                    .data$bt, .data$bb, .data$bl, .data$br,
                    .data$ah, .data$av, .data$aw, .data$ar) |>
    dplyr::summarise(dims = xl_coalesce(.data$col, .data$row), .groups = "drop")
}


# Register each distinct style ONCE (deduped fonts/fills/borders + a composed cell xf) and apply it
# by id with set_cell_style over its coalesced dims -- far fewer + cheaper openxlsx2 calls than a
# separate wb_add_* per aspect. numFmt is applied separately by the writer (it merges cross-aspect).
#' @keywords internal
xl_apply_styles <- function(wb, s, styles) {
  if (!nrow(styles)) return(invisible(wb))
  sm  <- wb$styles_mgr
  fc  <- new.env(parent = emptyenv()); lc <- new.env(parent = emptyenv())
  bc  <- new.env(parent = emptyenv()); ctr <- 0L
  uid <- function() { ctr <<- ctr + 1L; ctr }
  font_id <- function(name, size, bold, color) {
    key <- paste(name, size, bold, color, sep = "\r")
    if (is.null(fc[[key]])) {
      args <- list(name = name, sz = as.character(size))
      if (isTRUE(bold))   args$b     <- "1"
      if (!is.na(color))  args$color <- xl_color(color)
      nm <- paste0("txf", uid()); sm$add(do.call(openxlsx2::create_font, args), nm)
      fc[[key]] <- sm$get_font_id(nm)
    }
    fc[[key]]
  }
  fill_id <- function(color) {
    if (is.na(color)) return("")
    if (is.null(lc[[color]])) {
      nm <- paste0("txl", uid())
      sm$add(openxlsx2::create_fill(pattern_type = "solid", fg_color = xl_color(color)), nm)
      lc[[color]] <- sm$get_fill_id(nm)
    }
    lc[[color]]
  }
  border_id <- function(bt, bb, bl, br) {
    if (bt == 0L && bb == 0L && bl == 0L && br == 0L) return("")
    key <- paste(bt, bb, bl, br, sep = "\r")
    if (is.null(bc[[key]])) {
      sty <- function(v) if (v == 2L) "double" else if (v == 1L) "thin" else NULL
      blk <- xl_color("black"); nm <- paste0("txb", uid())
      sm$add(openxlsx2::create_border(
        top    = sty(bt), top_color    = if (bt > 0L) blk,
        bottom = sty(bb), bottom_color = if (bb > 0L) blk,
        left   = sty(bl), left_color   = if (bl > 0L) blk,
        right  = sty(br), right_color  = if (br > 0L) blk), nm)
      bc[[key]] <- sm$get_border_id(nm)
    }
    bc[[key]]
  }
  for (i in seq_len(nrow(styles))) {
    r <- styles[i, ]
    if (is.na(r$dims)) next
    nm <- paste0("txx", uid())
    sm$add(openxlsx2::create_cell_style(
      font_id       = font_id(r$fname, r$fsize, r$fbold, r$fcolor),
      fill_id       = fill_id(r$fill),
      border_id     = border_id(r$bt, r$bb, r$bl, r$br),
      horizontal    = if (!is.na(r$ah)) r$ah else "",
      vertical      = if (nzchar(r$av)) r$av else "",
      wrap_text     = if (isTRUE(r$aw)) "1" else "",
      text_rotation = if (r$ar != 0L) as.character(r$ar) else ""), nm)
    wb$set_cell_style(sheet = s, dims = r$dims, style = sm$get_xf_id(nm))
  }
  invisible(wb)
}


# Per-sheet writer: write the raw values, then apply the precomposed cell styles by id (font + fill +
# border + alignment in ONE set_cell_style per distinct style), then the numFmt merging pass and the
# column widths / row heights.
#' @keywords internal
xl_write_table <- function(wb, plan, o) {
  s   <- plan$sheet
  hdr <- plan$header_row

  # values: raw numbers + header, title, subtext (styles applied below)
  xlb_write_data(wb, s, plan$data, hdr, 1L)
  xlb_write_cell(wb, s, xl_cell(plan$title_row, 1L), plan$title)
  if (length(plan$subtext)) xlb_write_cell(wb, s, xl_cell(plan$subtext_row, 1L), plan$subtext)

  # --- styles: one composed xf (font + fill + border + alignment) per distinct cell style ---
  xl_apply_styles(wb, s, plan$styles)

  # --- number formats: one shared code over the fewest coalesced ranges (merges onto the xf) ---
  if (nrow(plan$numfmt)) {
    plan$numfmt |>
      dplyr::group_by(.data$code) |>
      dplyr::summarise(dims = xl_coalesce(.data$col, .data$row), .groups = "drop") |>
      purrr::pwalk(function(code, dims) xlb_numfmt(wb, s, dims, code))
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
