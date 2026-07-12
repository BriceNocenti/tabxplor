# PURPOSE: Export tabxplor tables to Excel with formatting and colors via openxlsx.
# ROLE: Primary export format for sharing tables with non-R users. Phase 10g: consumes the shared
#       exporter prep (R/tab-export-prep.R) for role detection / references / bold rows, and the
#       two-channel colour engine (fmt_color_channels) for cell colours.
# KEY CONSTRAINTS:
#   - openxlsx is in Suggests — all calls must be guarded.
#   - Colour codes come from get_color_style(mode = "color_code"); cell colour SLOTS from
#     fmt_color_channels() (the shared Phase 5 findInterval engine), mapped to font/fill styles.
#   - Export-Parity: tab_xl writes the RAW get_num() value and lets Excel format it via the codes
#     from format(x, syntax = "excel") (fmt_class.R excel_numfmt_code) -- so a display/digits change
#     in fmt_class.R can no longer silently desync the export. See CLAUDE.md § Export Parity.
#   - openxlsx v1 (the openxlsx2 engine swap is Phase 11).

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
# @param print_ci Set to \code{TRUE} to print confidence intervals in another table,
# at the left of the base table.
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
# @param pct_breaks The breaks used to color percentages.
# @param mean_breaks The breaks used to color means.
# @param contrib_breaks The breaks used to color contributions of cells to variance.
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
           colnames_rotation = 0, remove_tab_vars = TRUE, # print_ci = FALSE,
           colwidth = 10, print_color_legend = TRUE,
           sheets = "auto", n_min = 0, titles,
           font_text = "DejaVu Sans Condensed", font_num = "DejaVu Sans",
           text_size = 10, text_size_headers = 9, text_size_subtext = 9,
           hide_near_zero = Inf, #, #c("auto", 0.0049, Inf),
           color_type = "text"
           # pct_breaks     = get_color_breaks("pct"),
           # mean_breaks    = get_color_breaks("mean"),
           # contrib_breaks = get_color_breaks("contrib") #c(1, 2, 5, -1,-2, -5)
  ) {
    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      stop(paste0("Package \"openxlsx\" needed for this function to work. ",
                  "You can install it with : install.packages('openxlsx')"),
           call. = FALSE)
    }

    if (length(replace) == 0) replace <- length(path) != 0

    # Phase 10g soft-deprecations (kept for back-compat but inert; warn only on a non-default value):
    #   - hide_near_zero: near-zero greying (rarely used, slow).
    #   - n_min: the small-n greying is dropped; use tab(n_min = ), which blanks/drops small-n cells
    #     at display and flows into every export.
    if (!identical(hide_near_zero, Inf)) {
      lifecycle::deprecate_soft("1.4.0", "tab_xl(hide_near_zero)")
    }
    if (!identical(n_min, 0) && !identical(n_min, 0L)) {
      lifecycle::deprecate_soft("1.4.0", "tab_xl(n_min)", "tab(n_min)")
    }

    tabs_base <- tabs
    # Phase 10c: graceful degrade -- write the raw frame (+ a message) instead of crashing when a
    # single input can't be read as a tabxplor table. Full plain-data.frame support lands in 10g.
    rv <- if (is.data.frame(tabs)) tab_render_vars(tabs) else list(degrade = FALSE)
    if (isTRUE(rv$degrade)) {
      tab_degrade_inform(rv$reason)
      p <- if (length(path)) path[[1]] else {
        d <- getOption("tabxplor.export_dir", "")
        if (nzchar(d)) file.path(d, "Tab") else file.path(tempdir(), "Tab")
      }
      if (!stringr::str_detect(p, "\\.xlsx$")) p <- paste0(p, ".xlsx")
      openxlsx::write.xlsx(tibble::as_tibble(tabs), file = p, overwrite = TRUE)
      if (isTRUE(open)) openxlsx::openXL(p)
      return(invisible(tabs_base))
    }
    if (is.data.frame(tabs)) tabs <- list(tabs)

    tabs <- purrr::map(tabs, tab_pvalue_lines) # chi2 pvalue to lines

    colwidth       <- vctrs::vec_recycle(colwidth,       length(tabs))

    # === Shared exporter prep (Phase 10g) ==========================================
    # Role detection (fmt / other / total columns, total-block borders, references, bold rows) is
    # derived ONCE by the shared framework (R/tab-export-prep.R), replacing tab_xl's two former
    # tab_get_vars() passes and the copy-pasted bold/reference blocks. compact = FALSE keeps one
    # prep-table per input tab (each -> its own sheet region); drop_tab_vars mirrors remove_tab_vars.
    # Colours stay on tab_xl's own two-channel path below (fmt_color_channels + font/fill styles),
    # which -- unlike the prep's text-only roles$color_cols -- also catches background-only columns.
    prep <- tab_export_prep(
      tabs, backend = "xl", compact = FALSE, drop_tab_vars = remove_tab_vars,
      list_method = TRUE, compute = c("refs", "bold"),
      color_type = color_type, color_legend = print_color_legend, what = "tab_xl()"
    )
    rd <- prep$tables

    # Graceful degrade: any list member that is not a readable tabxplor table (no fmt / no factor
    # row variable) is written as a plain sheet, with a message, instead of crashing the export.
    if (any(purrr::map_lgl(rd, ~ isTRUE(.$vars$degrade)))) {
      purrr::walk(rd, ~ if (isTRUE(.$vars$degrade)) tab_degrade_inform(.$vars$reason))
      p <- if (length(path)) path[[1]] else {
        d <- getOption("tabxplor.export_dir", "")
        if (nzchar(d)) file.path(d, "Tab") else file.path(tempdir(), "Tab")
      }
      if (!stringr::str_detect(p, "\\.xlsx$")) p <- paste0(p, ".xlsx")
      openxlsx::write.xlsx(purrr::map(rd, ~ tibble::as_tibble(.$tab)), file = p,
                           overwrite = TRUE)
      if (isTRUE(open)) openxlsx::openXL(p)
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
               "tabs"    = 1L:length(tabs)      ,
               "unique"  = rep(1L, length(tabs)),
               "auto"    = purrr::map2_lgl(
                 col_vars_plain,
                 dplyr::lag(col_vars_plain),
                 # col_vars_levels_no_tot,
                 # dplyr::lag(col_vars_levels_no_tot,
                 #            default = col_vars_levels_no_tot[1]),
                 ~ !identical(sort(.x), sort(.y))
               ) |>
                 cumsum() # + 1L # as.integer() %>%
        )
      } else if (is.integer(sheets)) {
        sheets
      }

    # Phase 5: two style sets built once. TEXT channel -> font colour (in the `color_type` palette
    # family, default "text"); BACKGROUND channel -> cell fill (bg palette). Each is an 11-element
    # list indexed by slot integer (1..5 = pos1..pos5, 6..10 = neg1..neg5, 11 = ratio); the two are
    # stacked per cell downstream (openxlsx addStyle(stack = TRUE)).
    text_pal    <- get_color_style("color_code", theme = "light", type = color_type)
    bg_pal      <- get_color_style("color_code", theme = "light", type = "bg")
    font_styles <- purrr::map(text_pal, ~ openxlsx::createStyle(fontColour = .,
                                                                textDecoration = "bold"))
    fill_styles <- purrr::map(bg_pal,   ~ openxlsx::createStyle(fgFill = .))

    #sign  <- c(rep(">", length(styles)/2L), rep("<", length(styles)/2L))

    # conditional_fmt_styles <- tibble::tibble(
    #   styles,
    #   sign,
    #   pct_breaks     = tabxplor_color_breaks$pct_breaks    ,
    #   mean_breaks    = tabxplor_color_breaks$mean_breaks   ,
    #   contrib_breaks = tabxplor_color_breaks$contrib_breaks,
    #   pct_ci_breaks  = tabxplor_color_breaks$pct_ci_breaks ,
    #   mean_ci_breaks = tabxplor_color_breaks$mean_ci_breaks,
    #   pct_brksup     = tabxplor_color_breaks$pct_brksup    ,
    #   mean_brksup    = tabxplor_color_breaks$mean_brksup   ,
    #   contrib_brksup = tabxplor_color_breaks$contrib_brksup,
    #   pct_ci_brksup  = tabxplor_color_breaks$pct_ci_brksup ,
    #   mean_ci_brksup = tabxplor_color_breaks$mean_ci_brksup,
    # )

    subtext <- purrr::map(tabs, get_subtext) #need breaks calculation first
    subtext <- subtext |>
      purrr::map(~ stringr::str_replace_all(., "\\\n", " ")  |>
                   stringr::str_replace_all(" +", " ")
      )
    if (print_color_legend == TRUE) {
      color_legend <- purrr::map(tabs, ~ suppressWarnings(tab_color_legend(., colored = FALSE,
                                                          add_color_and_diff_types = TRUE)))
      # color_legend <- color_legend %>%
      #   purrr::map_if(purrr::map_lgl(., ~ !is.null(.)),
      #                 ~ purrr::map_if(., 1:length(.) == 1,
      #                                 ~ paste0("Colors: ", .)) %>%
      #                   purrr::flatten_chr())
      subtext      <- purrr::map2(subtext, color_legend, ~ c(.y, .x))
    }


    if (missing(titles)) {
      titles <-
        purrr::pmap_chr(list(tabs, row_vars, col_vars_plain, tab_vars),
                    ~ tab_get_titles(..1, ..2, ..3, ..4)
        )
    } else {
      titles <- vctrs::vec_recycle(titles, length(tabs))
    }

    # === Per-table geometry from the shared prep (Phase 10g) =======================
    # Sheet-stacking offsets first, then role indices sourced from `roles` / `bold_rows` (within-table
    # indices; `+ start + 1L` shifts them to the sheet position of each stacked table).
    newsheet <- sheet != dplyr::lag(sheet, default = -1L)

    start <- tibble::tibble(newsheet, rows = purrr::map_int(tabs, nrow),
                            sub = purrr::map_int(subtext, length)) %>%
      dplyr::group_by(gr = cumsum(as.integer(.data$newsheet))) %>%
      dplyr::mutate(start = dplyr::lag(cumsum(.data$rows + .data$sub + 5L),
                                       default = 0L) + 1L) %>%
      dplyr::pull(.data$start)

    # WARNING: Export-Parity -- tab_xl writes the RAW get_num() value, NOT format()'s string; the
    # display is rebuilt by Excel from the codes returned by format(syntax = "excel") (below). The
    # code path is the single display source of truth now (fmt_class.R excel_numfmt_code).
    tabs_num <- purrr::map(tabs, ~ dplyr::mutate(., dplyr::across(where(is_fmt), get_num)) %>%
                             tibble::as_tibble())

    all_cols <- purrr::map(tabs, ~ seq_len(ncol(.)))
    rows_nb  <- purrr::map2(tabs, start, ~ as.integer(seq_len(nrow(.x)) + .y + 1L))

    fmt_cols      <- purrr::map(roles, "fmt_cols")
    txt_cols      <- purrr::map(roles, "other_cols")
    row_var_col   <- purrr::map(roles, "row_var_col")
    totcols       <- purrr::map(roles, "totcols")
    ref_cols      <- purrr::map(tabs, ~ which(is_refcol(.)))

    # a column is coloured if it carries a text OR a background colour channel (the prep's text-only
    # roles$color_cols would miss background-only columns) -- kept local for two-channel correctness.
    color_cols <- purrr::map(tabs, ~ which(purrr::map_lgl(., function(.col) {
      if (!is_fmt(.col)) return(FALSE)
      ct <- get_color(.col); cb <- get_color_bg(.col)
      (length(ct) != 0L && !is.na(ct) && !ct %in% c("", "no")) ||
        (length(cb) != 0L && !is.na(cb) && !cb %in% c("", "no"))
    })))

    col_vars_names <- purrr::map(tabs, get_col_var)
    start_col_var  <- purrr::map(col_vars_names,
                                 ~ which(. != "" & . != dplyr::lag(., default = NA_character_)))

    tot_rows      <- purrr::map2(roles, start, ~ .x$totrows + .y + 1L)
    tot_rows_1    <- purrr::map2(roles, start, ~ .x$totblock_top + .y + 1L)
    tot_rows_last <- purrr::map2(roles, start, ~ .x$totblock_bottom + .y + 1L)
    ref_rows      <- purrr::map2(rd,    start, ~ .x$bold_rows + .y + 1L)
    # totblock's last (trailing) new_group entry is the table's final row; drop it to keep only the
    # BETWEEN-group double borders.
    end_group     <- purrr::map2(roles, start, ~ utils::head(.x$new_group, -1L) + .y + 1L)

    sheet_titles <-
    # purrr::pmap_chr(list(tabs[newsheet],
    #                      purrr::map(row_var[newsheet], as.character),
    #                      purrr::map(col_vars[newsheet], as.character)),
    #                 ~ tab_get_titles(..1, ..2, ..3, max = 1)
    # ) %>%
    titles[newsheet] |> stringr::str_sub(1, 25)

    sheet_titles <- dplyr::if_else(duplicated(sheet_titles),
                                   stringr::str_c(sheet_titles, ".2"),
                                   sheet_titles)
    nb <- 2
    while (length(unique(sheet_titles)) != length(sheet_titles)) {
      nb <- nb + 1
      sheet_titles <-
        dplyr::if_else(duplicated(sheet_titles),
                       stringr::str_c(stringr::str_remove(sheet_titles, "..$"),
                                      ".", nb),
                       sheet_titles)
    }


    #Create workbook and global formatting -------------------------------------
    wb <- openxlsx::createWorkbook()
    sheet_titles %>% purrr::walk(~ openxlsx::addWorksheet(wb, .))
    purrr::pwalk(list(sheet, start, tabs_num),
                 ~ openxlsx::writeData(wb, sheet = ..1, ..3,
                                       startRow = ..2 + 1, startCol = 1,
                                       borders = "surrounding"))
    # #On a sheet, if colnames are the same, just keep the first :
    # purrr::pwalk(list(sheet[hd_remove], start[hd_remove],  tabs[hd_remove]),
    #              function(.sheet, .start, .tabs)
    #                openxlsx::deleteData(wb, sheet = .sheet, gridExpand = TRUE,
    #                                     rows = .start + 1,
    #                                     cols = 2:ncol(.tabs)))

    openxlsx::modifyBaseFont(wb, fontSize = 10, fontName = font_text) #"Verdana", "DejaVu Sans Condensed"
    purrr::walk(unique(sheet),
                ~ openxlsx::showGridLines(wb, sheet = .x, showGridLines = FALSE))
    purrr::walk(unique(sheet),
                ~ openxlsx::freezePane(wb, sheet = .x, firstActiveRow  = 3L,
                                       firstCol = TRUE))

    st_base_style <- openxlsx::createStyle(valign = "top")

    tibble::tibble(sheet = sheet, rows = rows_nb,cols = all_cols) %>%
      purrr::pwalk(openxlsx::addStyle, wb = wb, style = st_base_style,
                   gridExpand = T, stack = T)

    st_titles <- openxlsx::createStyle(fontSize = 12, textDecoration = "bold")

    tibble::tibble(sheet, startRow = start + 1L - 1L, startCol = 1L, x = titles) %>%
      purrr::pwalk(openxlsx::writeData, wb = wb) %>%
      dplyr::select(tidyselect::all_of(c("sheet", "rows" = "startRow", "cols" = "startCol"))) %>%
      purrr::pwalk(openxlsx::addStyle, wb = wb, stack = TRUE, style = st_titles)

    subtext_style <- openxlsx::createStyle(halign = "left", valign = "center",
                                           fontSize = text_size_subtext)
    tibble::tibble(sheet, x = subtext, startCol = 1L,
                   startRow = purrr::map2_int(start, tabs,
                                              ~ nrow(.y) + .x + 2L)) %>%
      dplyr::filter(purrr::map_lgl(subtext, ~ length(.) != 0)) %>%
      dplyr::filter(purrr::map_lgl(subtext, ~ any(!is.na(.) & . != ""))) %>%
      purrr::pwalk(openxlsx::writeData, wb = wb) %>%
      dplyr::mutate(rows = purrr::map2(.data$startRow, .data$x,
                                       ~ .x:(.x + length(.y) - 1)),
                    cols = .data$startCol) %>%
      dplyr::select(-"startRow", -"startCol", -"x") %>%
      purrr::pwalk(openxlsx::addStyle, wb = wb, stack = TRUE, gridExpand = TRUE,
                   style = subtext_style)

    # Borders and text formating ---------------------------------------------------------
    # Headers and total columns
    st_bottomline <-
      openxlsx::createStyle(border = "bottom", borderStyle = "thin")

    tibble::tibble(sheet, rows = purrr::map2(tabs, start, ~ nrow(.) + .y + 1L),
                   cols = all_cols) %>%
      purrr::pwalk(openxlsx::addStyle, wb = wb, gridExpand = TRUE, stack = T,
                   style = st_bottomline)


    st_totcols <-
      openxlsx::createStyle(halign = "left", valign = "top", border = "LeftRight")

    tibble::tibble(sheet,
                   rows = purrr::map2(rows_nb, start, ~ unique(c(.y + 1L, .x))),
                   cols = totcols) %>%
      dplyr::filter(purrr::map_lgl(.data$cols, ~ length(.) != 0) ) %>%
      purrr::pwalk(openxlsx::addStyle, wb = wb, gridExpand = TRUE, stack = T,
                   style = st_totcols)

    st_refcols <-
      openxlsx::createStyle(textDecoration = "Bold")

    tibble::tibble(sheet,
                   rows = purrr::map2(rows_nb, start, ~ unique(c(.y + 1L, .x))),
                   cols = ref_cols) %>%
      dplyr::filter(purrr::map_lgl(.data$cols, ~ length(.) != 0) ) %>%
      purrr::pwalk(openxlsx::addStyle, wb = wb, gridExpand = TRUE, stack = T,
                   style = st_refcols)

    st_start_col_var <- openxlsx::createStyle(border = "Left")

    tibble::tibble(sheet,
                   rows = purrr::map2(rows_nb, start, ~ unique(c(.y + 1L, .x))),
                   cols = start_col_var) %>%
      dplyr::filter(purrr::map_lgl(.data$cols, ~ length(.) != 0) ) %>%
      purrr::pwalk(openxlsx::addStyle, wb = wb, gridExpand = TRUE, stack = T,
                   style = st_start_col_var)

    st_last_col <- openxlsx::createStyle(border = "Right")

    tibble::tibble(sheet,
                   rows = purrr::map2(rows_nb, start, ~ unique(c(.y + 1L, .x))),
                   cols = purrr::map(all_cols, dplyr::last)) %>%
      dplyr::filter(purrr::map_lgl(.data$cols, ~ length(.) != 0) ) %>%
      purrr::pwalk(openxlsx::addStyle, wb = wb, gridExpand = TRUE, stack = T,
                   style = st_last_col)

    st_first_col <- openxlsx::createStyle(border = "Left")

    tibble::tibble(sheet,
                   rows = purrr::map2(rows_nb, start, ~ unique(c(.y + 1L, .x))),
                   cols = purrr::map(all_cols, dplyr::first)) %>%
      dplyr::filter(purrr::map_lgl(.data$cols, ~ length(.) != 0) ) %>%
      purrr::pwalk(openxlsx::addStyle, wb = wb, gridExpand = TRUE, stack = T,
                   style = st_first_col)

    headers <- if (colnames_rotation == 0) {
      openxlsx::createStyle(halign = "center", valign = "bottom", wrapText = TRUE,
                            textDecoration = "Bold", border = "TopBottom",
                            fontSize = text_size_headers)
    } else {
      openxlsx::createStyle(
        halign = "left", valign = "bottom", wrapText = TRUE,
        textDecoration = "Bold", textRotation = colnames_rotation,
        border = c("bottom", "top"), fontSize = text_size_headers # "left", "right",
      )
    }

    tibble::tibble(sheet, rows = start + 1, cols = all_cols) %>%
      purrr::pwalk(openxlsx::addStyle, wb = wb, gridExpand = TRUE, stack = T,
                   style = headers)

    # Total rows
    st_totrows <-
      openxlsx::createStyle(halign = "right", valign = "top")

    tibble::tibble(sheet, rows = tot_rows, cols = fmt_cols) %>%
      dplyr::filter(purrr::map_lgl(.data$rows, ~ length(.) != 0) ) %>%
      purrr::pwalk(openxlsx::addStyle, wb = wb, gridExpand = TRUE, stack = T,
                   style = st_totrows)

    st_totrows_text <-
      openxlsx::createStyle(halign = "left", valign = "top", wrapText = TRUE)

    tibble::tibble(sheet, rows = tot_rows, cols = txt_cols) %>%
      dplyr::filter(purrr::map_lgl(.data$rows, ~ length(.) != 0) ) %>%
      purrr::pwalk(openxlsx::addStyle, wb = wb, gridExpand = TRUE, stack = T,
                   style = st_totrows_text)

    st_bottom_left_cells <- openxlsx::createStyle(halign = "left", valign = "top")
    tibble::tibble(sheet, rows = tot_rows, cols = totcols) %>%
      purrr::pwalk(openxlsx::addStyle, wb = wb, gridExpand = TRUE, stack = T,
                   style = st_bottom_left_cells)

    st_totrows_1 <-
      openxlsx::createStyle(border = "Top", borderStyle = "thin")

    tibble::tibble(sheet, rows = tot_rows_1, cols = all_cols) %>%
      dplyr::filter(purrr::map_lgl(.data$rows, ~ length(.) != 0) ) %>%
      purrr::pwalk(openxlsx::addStyle, wb = wb, gridExpand = TRUE, stack = T,
                   style = st_totrows_1)

    st_totrows_last <-
      openxlsx::createStyle(border = "Bottom", borderStyle = "thin")

    tibble::tibble(sheet, rows = tot_rows_last, cols = all_cols) %>%
      dplyr::filter(purrr::map_lgl(.data$rows, ~ length(.) != 0) ) %>%
      purrr::pwalk(openxlsx::addStyle, wb = wb, gridExpand = TRUE, stack = T,
                   style = st_totrows_last)

    st_refrows <- openxlsx::createStyle(textDecoration = "Bold")

    tibble::tibble(sheet, rows = ref_rows, cols = fmt_cols) %>%
      dplyr::filter(purrr::map_lgl(.data$rows, ~ length(.) != 0) ) %>%
      purrr::pwalk(openxlsx::addStyle, wb = wb, gridExpand = TRUE, stack = T,
                   style = st_refrows)

    st_refrows_text <- openxlsx::createStyle(textDecoration = "Bold")

    tibble::tibble(sheet, rows = ref_rows, cols = txt_cols) %>%
      dplyr::filter(purrr::map_lgl(.data$rows, ~ length(.) != 0) ) %>%
      purrr::pwalk(openxlsx::addStyle, wb = wb, gridExpand = TRUE, stack = T,
                   style = st_refrows_text)

    st_end_group <-
      openxlsx::createStyle(border = "Bottom", borderStyle = "double")

    tibble::tibble(sheet, rows = end_group, cols = all_cols) %>%
      dplyr::filter(purrr::map_lgl(.data$rows, ~ length(.) != 0) ) %>%
      purrr::pwalk(openxlsx::addStyle, wb = wb, gridExpand = TRUE, stack = T,
                   style = st_end_group)

    #Number formats -------------------------------------------------------------
    # DESIGN: format(col, syntax = "excel") returns the per-cell Excel numFmt code, folding the old
    # inline numfmt() so format() is now the SINGLE display source of truth (fmt_class.R
    # excel_numfmt_code). tab_xl writes the RAW get_num() value; Excel formats it via these codes, so
    # a digits/display change in fmt_class.R can no longer silently desync the export. One
    # createStyle() is built per DISTINCT code (memoised) and applied to its cells per (sheet, code)
    # group. NA codes (e.g. a percentage rounded to a power of ten) get no style (Excel "General").
    numfmt_cells <- purrr::pmap_dfr(
      list(tabs, fmt_cols, sheet, start),
      function(.tab, .fc, .sheet, .start) {
        if (length(.fc) == 0L) return(tibble::tibble())
        purrr::map_dfr(.fc, function(.ci) {
          code <- format(.tab[[.ci]], syntax = "excel")
          tibble::tibble(sheet = .sheet, col = as.integer(.ci),
                         row = seq_along(code) + .start + 1L, code = code)
        })
      })
    numfmt_cells <- dplyr::filter(numfmt_cells, !is.na(.data$code))

    if (nrow(numfmt_cells) != 0L) {
      number_styles <- purrr::map(
        purrr::set_names(unique(numfmt_cells$code)),
        ~ openxlsx::createStyle(fontName = font_num, numFmt = .))
      numfmt_cells %>%
        dplyr::group_by(.data$sheet, .data$code) %>%
        dplyr::summarise(cols = list(.data$col), rows = list(.data$row), .groups = "drop") %>%
        purrr::pwalk(function(sheet, code, cols, rows) openxlsx::addStyle(
          wb, stack = TRUE, sheet = sheet, cols = cols, rows = rows,
          style = number_styles[[code]]))
    }

    # #     digits (and references) for confidence intervals tables
    # if (any(!no_ci)) {
    #   digits_map_ci <-  digits_map %>%
    #     dplyr::ungroup() %>%
    #     dplyr::select(-"num_format", -"num_name") %>%
    #     dplyr::filter(.data$tab_nb %in% which(!no_ci)) %>%
    #     dplyr::mutate(num_format_ci =
    #                     forcats::as_factor(numfmt(.data$digits + 1L, .data$type, "ci")),
    #                   cols = .data$cols + .data$offset) %>%
    #     dplyr::group_by(.data$num_format_ci) %>%
    #     dplyr::mutate(num_name_ci = paste0("st_digits_ci",
    #                                        as.integer(.data$num_format_ci)))
    #
    #   number_ci_styles <- digits_map_ci %>%
    #     dplyr::summarise(num_name_ci = dplyr::last(.data$num_name_ci), .groups = "drop") %>%
    #     dplyr::select("num_name_ci", "num_format_ci") %>% tibble::deframe() %>%
    #     purrr::map(~ openxlsx::createStyle(fontName = "DejaVu Sans",
    #                                        numFmt = as.character(.),
    #                                        fontColour = "#b3b3b3"))
    #
    #   purrr::iwalk(number_ci_styles,
    #                ~ assign(.y, .x, pos = parent.env(rlang::current_env())))
    #
    #   digits_map_ci %>% dplyr::group_by(.data$sheet, .data$num_name_ci) %>%
    #     dplyr::summarise(cols = list(.data$cols), rows = list(.data$rows),
    #                      .groups = "drop") %>%
    #     dplyr::relocate("num_name_ci", .after = -1) %>%
    #     purrr::pwalk(function(sheet, cols, rows, num_name_ci) openxlsx::addStyle(
    #       wb, stack = TRUE,
    #       sheet = sheet, cols = cols, rows = rows,
    #       style = rlang::eval_tidy(rlang::sym(num_name_ci))
    #     ))
    #
    #   ci_ref_map <-
    #     tibble::tibble(sheet = sheet[!no_ci], start, offset,
    #                    x  = purrr::map_depth(ci_refs, 2, ~ .),
    #                    startCol = purrr::map(ci_refs, ~ 1:ncol(.)),
    #                    startRow = purrr::map(ci_refs, ~ 1:nrow(.))) %>%
    #     tidyr::unnest(tidyselect::all_of(c("startCol", "x"))) %>%
    #     tidyr::unnest(tidyselect::all_of(c("startRow", "x"))) %>%
    #     dplyr::filter(!is.na(.data$x) & .data$x != "") %>%
    #     dplyr::mutate(startCol = .data$startCol + .data$offset,
    #                   startRow = .data$startRow + .data$start + 1L)
    #
    #   ci_ref_map %>%
    #     dplyr::select(tidyselect::all_of(c("sheet", "x", "startCol", "startRow"))) %>%
    #     purrr::pwalk(openxlsx::writeData, wb = wb, colNames = FALSE)
    # }


    #Conditional formatting (made with normal color formatting) ----------------
    # color_selections <-
    #   purrr::map2(tabs, color_cols, ~ purrr::map(
    #     .x[.y],
    #     ~ fmt_color_selection(., force_breaks = conditional_fmt_styles) %>%
    #       purrr::map(which)
    #   ) )

    # Phase 5: two-channel conditional formatting from the vectorised engine. For every colored
    # column of every sheet, fmt_color_channels() gives per-cell text_slot (font) and bg_slot
    # (fill); rows are grouped by (sheet, channel, slot) so addStyle is called once per group and
    # both channels are stacked on the cell.
    color_style_map <- purrr::pmap_dfr(
      list(tabs, color_cols, sheet, start),
      function(.tab, .cc, .sheet, .start) {
        if (length(.cc) == 0L) return(tibble::tibble())
        purrr::map_dfr(.cc, function(.ci) {
          ch   <- fmt_color_channels(.tab[[.ci]])
          rows <- seq_along(ch$text_slot) + .start + 1L
          dplyr::bind_rows(
            tibble::tibble(sheet = .sheet, col = .ci, row = rows,
                           slot = ch$text_slot, channel = "text"),
            tibble::tibble(sheet = .sheet, col = .ci, row = rows,
                           slot = ch$bg_slot,   channel = "bg")
          )
        })
      }
    )

    if (nrow(color_style_map) != 0L) {
      color_style_map %>%
        dplyr::filter(.data$slot > 0L) %>%
        dplyr::group_by(.data$sheet, .data$channel, .data$slot) %>%
        dplyr::summarise(cols = list(.data$col), rows = list(.data$row), .groups = "drop") %>%
        purrr::pwalk(function(sheet, channel, slot, cols, rows) {
          style <- if (channel == "text") font_styles[[slot]] else fill_styles[[slot]]
          openxlsx::addStyle(wb = wb, stack = TRUE, sheet = sheet,
                             cols = cols, rows = rows, style = style)
        })
    }

    # if (any(!no_ci)) {
    #   color_selections_ci <-
    #     purrr::map2(tabs_ci[!no_ci], color_cols[!no_ci], ~ purrr::map(
    #       .x[.y],
    #       ~ fmt_color_selection(
    #         ., force_breaks =
    #           conditional_fmt_styles[c(1, nrow(conditional_fmt_styles)/2+1),]) %>%
    #         purrr::map(which)
    #     ) )
    #
    #   conditional_fmt_map <-
    #     tibble::tibble(sheet = sheet[!no_ci],
    #                    cols = purrr::map2(color_cols[!no_ci], offset[!no_ci],
    #                                       ~ .x + .y),
    #                    rows = color_selections_ci, start) %>%
    #     tidyr::unnest(tidyselect::all_of(c("cols", "rows"))) %>%
    #     tibble::add_column(style = list(
    #       paste0(   style[c(1, length(style)/2 + 1)],    "_ci") #.data$ ??
    #     )) %>%
    #     tidyr::unnest(tidyselect::all_of(c("rows", "style"))) %>%
    #     dplyr::filter(purrr::map_lgl(.data$rows, ~ length(.) != 0)) %>%
    #     dplyr::mutate(cols  = purrr::map2(.data$cols, .data$rows,
    #                                       ~ rep(.x, length(.y))),
    #                   rows  = purrr::map2(.data$rows, .data$start, ~ .x + .y + 1L)) %>%
    #     dplyr::group_by(.data$sheet, .data$style) %>%
    #     dplyr::summarise(cols = list(.data$cols), rows = list(.data$rows),
    #                      offset = offset[1],
    #                      .groups = "drop") %>%
    #     dplyr::mutate(dplyr::across(tidyselect::all_of(c("cols", "rows")),
    #                                 ~ purrr::map(., purrr::flatten_int)))
    #
    #   conditional_fmt_map %>%
    #     dplyr::select(tidyselect::all_of(c("sheet", "rows", "cols", "style"))) %>%
    #     purrr::pwalk(function(sheet, cols, rows, style)
    #       openxlsx::addStyle(
    #         wb = wb, stack = TRUE,
    #         sheet = sheet, cols = cols, rows = rows,
    #         style = rlang::eval_tidy(rlang::sym(style))
    #       ))
    #
    #   st_ci_ref <- openxlsx::createStyle(fontColour = "black")
    #
    #   ci_ref_map %>%
    #     dplyr::select(sheet, cols = .data$startCol, rows = .data$startRow) %>%
    #     purrr::pwalk(openxlsx::addStyle, wb = wb, stack = TRUE,
    #                  style = st_ci_ref)
    #
    # }

    # `hide_near_zero` (greying near-zero cells via openxlsx conditional formatting) was a rarely
    # used, slow white-elephant feature. It is soft-deprecated in Phase 10g: the argument is kept for
    # back-compatibility but no longer does anything (see the deprecation note near the top).

    #Colwidths and rowheights --------------------------------------------------
    tibble::tibble(sheet, cols = row_var_col) %>%
      purrr::pwalk(openxlsx::setColWidths, wb = wb, widths = 30)

    # tibble::tibble(sheet, cols = txt_cols) %>%
    #   purrr::pwalk(openxlsx::setColWidths, wb = wb, widths = 30)



    autocw <- purrr::map_lgl(colwidth, ~ . == "auto")

    if (any(!autocw)) {
      tibble::tibble(sheet, cols = fmt_cols, # purrr::map2(fmt_cols, ci_cols, c),
                     widths = colwidth) %>%
        dplyr::filter(!autocw) %>%
        dplyr::group_by(.data$sheet) %>%
        dplyr::mutate(widths = max(as.double(.data$widths))  ) %>%
        dplyr::ungroup() %>%
        purrr::pwalk(openxlsx::setColWidths, wb = wb)
    }

    if (any(autocw)) {
      if (colnames_rotation > 0) {
        if (colnames_rotation > 30 & colnames_rotation < 60) {
          purrr::pwalk(list(sheet[autocw], fmt_cols[autocw]), # ci_cols[autocw]
                       ~ openxlsx::setColWidths(wb, sheet = ..1,
                                                cols = c(..2), # , ..3
                                                widths = 8))
          # purrr::pwalk(list(sheet, tabs, purrr::map(totc, ~ dplyr::if_else(., 13, 8))),
          #              ~ openxlsx::setColWidths(wb, sheet = ..1, cols = ncol(..2), widths = ..3))
        } else if (colnames_rotation > 60) {
          purrr::pwalk(list(sheet[autocw], fmt_cols[autocw]), # ci_cols[autocw]
                       ~ openxlsx::setColWidths(
                         wb, sheet = ..1, cols = c(..2), # , ..3
                         widths = 6 + 8*cos(colnames_rotation/90*pi/2)
                       )) #Entre 6 et 14
          # purrr::pwalk(list(sheet, tabs, purrr::map(totc, ~ dplyr::if_else(., 13, 6 + 8*cos(colnames_rotation/90*pi/2)))),
          #              ~ openxlsx::setColWidths(wb, sheet = ..1, cols = ncol(..2), widths = ..3))
        }

        purrr::walk(sheet[autocw],
                    ~ openxlsx::setRowHeights(
                      wb, sheet = ., rows = 1,
                      heights = 13.8 + 105*sin(colnames_rotation/90*pi/2)
                    ))

        #Enlarge columns if there is confidence intervals
        # if (any(tab_with_CI_on_sheet)) {
        #   purrr::walk2(1:length(tab_with_CI_on_sheet)[tab_with_CI_on_sheet],
        #         purrr::map(tabs_on_same_sheet, ~ ncol(tabs[[.[1]]]))[tab_with_CI_on_sheet],
        #         ~ openxlsx::setColWidths(wb, sheet = .x, cols = 2:(.y-1), widths = 14))
        # }

      } else {
        purrr::pwalk(list(sheet[autocw], fmt_cols[autocw]), # ci_cols[autocw]
                     ~ openxlsx::setColWidths(wb, sheet = ..1,
                                              cols = c(..2), # , ..3
                                              widths = "auto")) #13
      }
    }

    #Save to file --------------------------------------------------------------
    if (is.null(path)) {
      path <- getOption("tabxplor.export_dir")
      if (is.null(path)) {
        path <- file.path(tempdir(), "Tab")
      } else {
        path <- file.path(path) #"Tab"
      }
    }

    if (stringr::str_detect(path, "\\\\|/")) {
      dir_path <- path %>% stringr::str_remove("\\\\[^\\\\]+$|/[^/]+$")
      if (! dir.exists(dir_path))  dir.create(dir_path, recursive = TRUE)
    }
    path_name <- stringr::str_remove(path, "\\.xlsx$")
    if (! stringr::str_detect(path, "\\.xlsx$")) path <-
      stringr::str_c(path, ".xlsx")
    if (replace == FALSE) {
      i <- 0
      file_do_not_exist <- FALSE
      while (file_do_not_exist == FALSE) {
        if (file.exists(path)) {
          i = i+1
          path <- stringr::str_c(path_name, i, ".xlsx")
        } else {
          path <-
            stringr::str_c(path_name, dplyr::if_else(i == 0,
                                                     "",
                                                     stringr::str_c(i)),
                           ".xlsx")
          file_do_not_exist <- TRUE
        }
      }
    }
    print(path)
    openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
    if (open == TRUE) { openxlsx::openXL(path) } #file.show

    invisible(tabs_base)
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






#' @keywords internal
#Calculate excel references of relevant cells
xl_index <- function(cols = "", rows = "", start_row = 0L, offset = 1L,
                     fixedcol = FALSE, fixedrow = FALSE) {

  if (is.list(cols)) cols <- purrr::map_int(cols, ~ .[1])
  if (is.list(rows)) rows <- purrr::map_int(rows, ~ .[1])

  fixc <- if (fixedcol) { "$" } else { "" }
  fixr <- if (fixedrow) { "$" } else { "" }

  paste0(fixc, purrr::map_chr(cols, ~ paste0(LETTERS[.[1] %/% 26],
                                             LETTERS[.[1] %%  26]) ),
         fixr, as.character(rows + start_row + offset)                       )
}
