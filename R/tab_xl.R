# PURPOSE: the Excel exporter -- values, number formats, colours and widths, through openxlsx2.
# ROLE: the format for sharing a table with people who do not use R, and the route into Word.
#   tab_xl_plan_one() does the pure per-table CPU (values + numFmt codes + a precomposed per-cell
#   style grid); xl_write_table() writes the values, applies the styles by id, then merges numFmt on.
#   It reads the shared prep (R/tab-export-prep.R) and reaches the engine only through
#   R/tab-xl-backend.R, which owns the openxlsx2 traps.
# KEY CONSTRAINTS:
#   - EXCEL CANNOT PRINT A COMPOSITE CELL -- one value, one numFmt, so a bracket cannot survive.
#     Every ASIDE therefore becomes a COLUMN of its own (mat_aside_cols) carrying its own segment
#     ("(n={n})"), and the source column keeps its primary alone.
#   - THE CELL IS A NUMBER, and everything the template writes around it lives in the numFmt code:
#     the stars, an aside's brackets, a test label, a sigma. WARNING: xl_fold_literals() folds them
#     into EVERY SECTION (xl_numfmt_affix), or a two-section code wears its stars on the negative
#     half alone.
#   - A MULTIPLICATIVE CELL HOLDS ITS READING VALUE (fmt_excel_value: the signed fold), printed by a
#     two-section code, so "1/2.11" reaches the workbook without costing the cell its numeric type.
#     `ratio_cells = "raw"` / `"text"` opt out; ?tab_xl gives the formula that recovers the ratio.
#   - TEXT IS A PROPERTY OF A CELL, not of a column: a `{ci}` bracket or a real min-max `{n_range}`
#     is written individually into an otherwise numeric column, so a statistic beside it stays a
#     number and takes the reader's own decimal separator.
#   - COLOUR IS READ, NEVER RE-DERIVED: ann$font / ann$back / ann$face_* from the prep, the same
#     fields the html engine consumes, so a greyed cell is grey here too. An aside column is the
#     console's sec(): the secondary grey, no bold, no stars, whatever row it is in.
#   - format(x, syntax = "excel") is the ONE display source of truth (R/fmt_class.R). WARNING: a
#     numFmt literal is backslash-escaped (xl_numfmt_literal), NEVER double-quote-wrapped, which
#     crashes the older openxlsx2 that jamovi bundles.
#   - ONE WORKBOOK-SCOPED STYLE REGISTRAR (xl_style_registrar): openxlsx2's styles_mgr is
#     workbook-global and resolves a name to its FIRST match, so per-table name reuse applies table
#     one's styles to every later table. Each cell's full style is composed once and applied by id
#     over the fewest coalesced ranges.
#   - PROSE IS MERGED AND WRAPPED to about an A4 portrait width (xl_prose_span): a title or a legend
#     left in one narrow column is a paragraph in one cell, and an Excel -> Word paste then sizes
#     that column to the paragraph. WARNING: Excel does not auto-fit a MERGED cell's height, so the
#     row height is computed (xl_prose_height) or the legend is clipped to one line.
#   - COLUMN WIDTHS ARE MEASURED, ONE VECTOR PER SHEET (xl_col_widths -> xl_sheet_widths): a column
#     is as wide as the widest thing in it that CANNOT wrap (a figure), while everything that can (a
#     level header, a unit tag, a long label) contributes its width divided by the lines it may use.
#     Per sheet, because a column index belongs to the sheet and not to the table sitting on it.
#     WARNING: openxlsx2's `bestFit` is not usable -- it blanks merged ranges, ignores wrapText,
#     rotation and the per-cell font, and is sheet-scoped, so stacked tables overwrite each other.
#   - The plan builder is pure; the workbook is assembled serially, the openxlsx2 write being
#     inherently so.
# See: CLAUDE.md section "tabxplor architecture" (exports and rendering); R/tab-export-prep.R (the
#      three header rows, tab_col_block_ids()); R/tab-xl-backend.R (the openxlsx2 traps).

#' Excel output for tabxplor tables, with formatting and colors
#' @description The Excel exporter behind \code{\link{tab_export}}: `tab_export(x, format = "xl")`
#' calls this. Colours follow the same palettes as the console and the HTML output, so a table looks
#' the same wherever it is read; change them with \code{\link{set_color_style}} and
#' \code{\link{set_color_breaks}}.
#' @eval tab_args_rd("tab_xl")
#' @param path,replace,open The name, and possibly the path, of the Excel file to create (the
#' \code{.xlsx} extension is optional). Defaults to a temporary directory; set the global option
#' \code{"tabxplor.export_dir"} with \code{\link[base]{options}} to change it. \code{replace}
#' defaults to \code{TRUE} when \code{path} is given and \code{FALSE} otherwise; set it to
#' \code{TRUE} to overwrite an existing file. Use \code{open = FALSE} not to open the workbook
#' straight away in Excel (or whatever opens \code{.xlsx} files).
#' @param colnames_rotation Rotate the names of columns to an angle (in degrees).
#' @param remove_tab_vars By default, \code{tab_vars} columns are removed to gain space.
#' Set to \code{FALSE} to keep them.
#' @param colwidth Column widths. \code{"auto"} (the default) fits every column to what its cells
#'   actually show, so a number column is exactly as wide as its widest figure and a text column
#'   wraps instead of growing past a cap. Give a number instead to force that fixed width on every
#'   numeric column (a mean's \code{sd} sibling then takes a proportionally narrower one).
#'   Widths are set per \emph{sheet}, so several tables written to one sheet all fit.
#' @param check Model-check plots to draw under each `tab_reg()` table: `FALSE` (the default),
#'   `"auto"`, or a vector of check keys --- the same values \code{\link{reg_check_plots}} takes,
#'   which is what draws them. Each grid is written as a picture below the table it belongs to.
#'   Needs `ggplot2` and `gridExtra`; a crosstab takes none.
#' @param data The data frame the models were fitted on. Only needed when `check` is on AND the
#'   \code{\link{tab_reg}} call cannot be replayed from the name it was written with (a `%>%`
#'   pipeline, a subset expression) --- an ordinary `tab_reg(gss, ...)` recovers it by itself.
#' @param ratio_cells What a ratio / odds-ratio cell holds in the workbook. Excel cannot compute
#'   inside a number format, so a cell storing `0.83` cannot be made to print `÷1.2` the way the
#'   console does. `"fold"` (the default) stores the **reading value** instead --- the fold, signed by
#'   its direction (`x` at or above the neutral, `-1/x` below it) --- which prints as `×1.20` and
#'   `÷1.20`, `2.11` and `1/2.11`. The cell stays a real number: it sorts and filters in the
#'   direction it is read, and takes the reader's own decimal separator. `"raw"` stores the
#'   untransformed ratio (printed `×0.83`); `"text"` writes the exact display string, which reads
#'   perfectly but is no longer a number. Option twin: \code{tabxplor.xl_ratio_cells}.
#' @section Recovering the raw ratio in Excel:
#' A ratio or odds-ratio cell holds its **reading value**: the fold, signed by its direction. The sign
#' IS the marker --- negative means the cell reads `÷` (or `1/`) --- so one formula gives the raw
#' ratio back, with no macro and no add-in:
#'
#' \preformatted{  =IF(A2<0, -1/A2, A2)     the ratio itself
#'   =ABS(A2)                how many times, whichever way it goes
#' }
#'
#' Sorting and filtering need neither: the stored value is monotone in the direction it is read, so
#' "at least twice as likely" is `>2` and "at least twice as unlikely" is `<-2`. Use
#' `ratio_cells = "raw"` when the untransformed ratio matters more than the reading.
#'
#' @param titles The titles of the different tables, as a character vector. When missing
#'   titles are given based on the names of the variables.
#' @param caption A single caption; a shortcut that fills \code{titles} (an explicit \code{titles}
#'   still wins). Unified name across all exporters.
#' @param font_text,font_num,font_num_stars Fonts for text (labels, headers) and for numbers. The
#'   number font is chosen \strong{per table}: \code{font_num} (default \code{"DejaVu Sans"}) when the
#'   table shows no significance stars, and \code{font_num_stars} (default \code{"Cascadia Mono"}, a
#'   \strong{monospace} font) when it does --- monospace aligns the stars and \code{(n=...)}
#'   composites, which a proportional font cannot. Defaults from
#'   \code{options(tabxplor.xl_font_text)} / \code{options(tabxplor.xl_font_num)} /
#'   \code{options(tabxplor.xl_font_num_stars)}. Note that xlsx, unlike CSS, has \strong{no
#'   font-fallback list}: only one name is recorded, so if it is missing on the machine opening the
#'   workbook Excel substitutes by its own rules. Set the options to a font you know is installed.
#' @param text_size,text_size_headers,text_size_subtext Font sizes of text elements.
#' @param theme By default (\code{"light"}) a white table with black text; set to \code{"dark"}
#'   for a black table with white text (the colours follow the theme).
#'   The black-and-white **publication** palettes render a table for a page that has no colour:
#'   \code{"print_ready"} picks the right one per table, or name it yourself --
#'   \code{"print_marks"}, \code{"print_emphasis"}, \code{"print_minimalistic"} (\code{"bw"}).
#'   See \code{\link{tab_css}} for what each of them says.
#' @param print_color_legend `r lifecycle::badge("deprecated")` Renamed to \code{color_legend}.
#' @param sheets The Excel sheets options :
#' \itemize{
#'   \item \code{"tabs"}: a new sheet is created for each table
#'   \item \code{"unique"}: all tables are on the same sheet
#'   \item \code{"auto"}: subsequent tables with the same column vars are printed on the
#'    same sheets
#' }
#' @param ... Retired arguments, accepted and ignored with a deprecation message since 2.0.0
#'   (`color_type`, `html_24_bit`): colour is a channel of `color =`, and Excel is always 24-bit.
#'
#' @return  The table(s) with formatting and colors in an Excel file, as a side effect.
#'  Invisibly returns \code{tabs}.
#' @export
#'
#' @examples
#' \donttest{
#' # openxlsx2 is Suggests-only and tab_xl() stops without it, so guard the example: \donttest{}
#' # does NOT exempt it from R CMD check --as-cran, which CRAN also runs without Suggests.
#' if (requireNamespace("openxlsx2", quietly = TRUE)) {
#'   forcats::gss_cat |>
#'     tab(marital, race, pct = "row", color = "difference") |>
#'     tab_xl()
#' }
#' }
tab_xl <-
  function(tabs, path = NULL, replace = FALSE, open = rlang::is_interactive(),
           lang = NULL,
           colnames_rotation = 0, remove_tab_vars = TRUE,
           colwidth = "auto", color_legend = TRUE,
           sheets = "auto", titles, caption = NULL,
           font_text = NULL, font_num = NULL, font_num_stars = NULL,
           text_size = 10, text_size_headers = 9, text_size_subtext = 9,
           theme = NULL,
           color = TRUE,
           transpose = FALSE, var_names = NULL,
           wrap_rows = 35, wrap_cols = 15,
           ratio_cells = NULL, check = FALSE, data = NULL,
           print_color_legend = lifecycle::deprecated(), ...) {

    tx_deprecate_inert(rlang::list2(...), "tab_xl")

    font_text      <- font_text      %||% tx_option("xl_font_text")
    font_num       <- font_num       %||% tx_option("xl_font_num")
    font_num_stars <- font_num_stars %||% tx_option("xl_font_num_stars")
    ratio_cells    <- match.arg(ratio_cells %||% tx_option("xl_ratio_cells"),
                                c("fold", "raw", "text"))

    # per-table color_breaks override for the render; no-op when tabs carries none.
    .cb <- push_color_breaks(tabs); on.exit(pop_color_breaks(.cb), add = TRUE)

    tx_need_pkg("openxlsx2", "Excel export")

    if (length(replace) == 0) replace <- length(path) != 0

    if (lifecycle::is_present(print_color_legend)) {
      lifecycle::deprecate_soft("2.0.0", "tab_xl(print_color_legend)", "tab_xl(color_legend)")
      color_legend <- print_color_legend
    }
    o <- resolve_export_opts(theme = theme, color = color, color_legend = color_legend,
                             transpose = transpose, caption = caption, var_names = var_names,
                             tabs = tabs)
    theme <- o$theme
    color_legend <- o$color_legend; color <- o$color
    # `caption` (single) is the unified alias; an explicit `titles` (per-sheet) still wins.
    if (!is.null(caption) && missing(titles)) titles <- caption

    tabs_base <- tabs
    # graceful degrade: an unreadable input writes as a raw frame (+ a message) instead of crashing.
    rv <- if (is.data.frame(tabs)) tab_render_vars(tabs) else list(degrade = FALSE)
    if (isTRUE(rv$degrade)) {
      tab_degrade_inform(rv$reason)
      xl_finish(function(p) xlb_write_xlsx(tibble::as_tibble(tabs), p), path, replace, open)
      return(invisible(tabs_base))
    }
    if (is.data.frame(tabs)) tabs <- list(tabs)

    # transpose is a render-model flip AFTER materialise (tx_transpose_render), so a transposed `tab`
    # is a plain character grid: values are written as TEXT here, editable numbers deferred.
    colwidth <- vctrs::vec_recycle(colwidth, length(tabs))

    # === Shared exporter prep ========================================================
    compute <- c("refs", "bold")
    if (color) compute <- c(compute, "colors")
    prep <- tab_export_prep(
      tabs, backend = "xl", drop_tab_vars = remove_tab_vars,
      list_method = TRUE, compute = compute, transpose = transpose,
      theme = theme, var_names = o$var_names,
      # `brk = "\n"` is what a wrapped Excel cell honours; spaces stay ordinary (the U+202F
      # substitution is only there to stop a BROWSER re-breaking what was wrapped -- Excel does not).
      wrap = list(rows = wrap_rows, cols = wrap_cols, exdent = 1,
                  whitespace_only = TRUE, unbreakable_spaces = FALSE, brk = "\n"),
      color_legend = color_legend, what = "tab_xl()"
    )
    rd <- prep$tables

    if (any(purrr::map_lgl(rd, ~ isTRUE(.$vars$degrade)))) {
      purrr::walk(rd, ~ if (isTRUE(.$vars$notify)) tab_degrade_inform(.$vars$reason))
      xl_finish(function(p) xlb_write_xlsx(purrr::map(rd, ~ tibble::as_tibble(.$tab)), p),
                path, replace, open)
      return(invisible(tabs_base))
    }

    tabs           <- purrr::map(rd, "tab")           # ungrouped, tab_vars dropped when requested
    # a transposed table's `tab` is a plain character grid; the subtext / colour legend / title read
    # the MEASURES + variable roles off the original fmt table, kept as `color_src`.
    tabs_src       <- purrr::map(rd, ~ if (is.null(.$color_src)) .$tab else .$color_src)
    transposed     <- purrr::map_lgl(rd, ~ isTRUE(.$transposed))
    roles          <- purrr::map(rd, "roles")
    # `row_var` is the column holding the row labels, which on a merged table is the literal "levels";
    # geometry elsewhere keeps reading `roles` instead.
    row_vars       <- purrr::map(rd, ~ .$vars$row_vars)
    tab_vars       <- purrr::map(rd, ~ .$vars$tab_vars)
    col_vars_plain <- purrr::map(rd, ~ .$vars$col_vars)

    # a `tabxplor_tabs` is an EXPLICIT collection of independent tables (one reg comparison per
    # outcome, or a several-row_vars output_list) -> one sheet each; the col-var "auto" stacking below
    # (which groups tables sharing col_vars onto one sheet) is for a plain manual `list(tab1, tab2)`.
    if (identical(sheets, "auto") && inherits(tabs_base, "tabxplor_tabs")) sheets <- "tabs"
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

    # subtext (+ colour legend) computed once. A workbook cell holds one line, so the render model's
    # `subtext` is newline-flattened here.
    subtext <- purrr::map(prep$tables, "subtext") |>
      purrr::map(~ gsub(" +", " ", gsub("\\\n", " ", ., perl = TRUE), perl = TRUE))
    # the whole footer (weight -> Model: -> colour legend -> stars) as RICH-TEXT run lines from the
    # shared builder, so each break-word carries its palette hex + bold while the rest stays plain
    # black; its plain text (derived from the runs, byte-for-byte) merges into `subtext` for geometry,
    # and the legend occupies the first `length(legend_runs)` subtext rows, overwritten below.
    legend_runs <- purrr::map(tabs_src, function(t)
      rd_footer(t, "runs", theme = theme, want_legend = isTRUE(color_legend), lang = lang))
    if (any(purrr::map_lgl(legend_runs, ~ length(.) > 0L))) {
      legend_plain <- purrr::map(legend_runs, ~ purrr::map_chr(
        ., function(line) paste0(purrr::map_chr(line, "text"), collapse = "")))
      subtext <- purrr::map2(subtext, legend_plain, ~ c(.y, .x))
    }

    if (missing(titles)) {
      # a regression table titles itself from its `meta` (family + outcome + predictors, or outcome +
      # reference + effect for a comparison); a NAMED tabxplor_tabs (several row_vars -> names = the
      # row_vars) uses its element names; a plain table gets the vars-derived "X by Y" title.
      base_nm <- names(tabs_base)
      named_tabs <- inherits(tabs_base, "tabxplor_tabs") && length(base_nm) == length(tabs) &&
        all(nzchar(base_nm))
      # shared rd_caption() (user caption -> set_caption() -> reg auto-title), with xl's own two extra
      # fallbacks passed as the closure -- one caption rule, one place.
      titles <- purrr::pmap_chr(
        list(prep$tables, tabs_src, row_vars, col_vars_plain, tab_vars, seq_along(tabs)),
        function(rd, t, rv, cv, tv, i) {
          cap <- rd_caption(rd, caption, fallback = function()
            if (named_tabs) base_nm[[i]] else tab_get_titles(t, rv, cv, tv))
          if (is.null(cap)) NA_character_ else cap
        })
    } else {
      titles <- vctrs::vec_recycle(titles, length(tabs))
    }
    # a reg table's SHEET name is the compact "<short>_<dep>_<pred>" tag, not the truncated prose
    # title; non-reg tables keep the title (truncated below).
    sheet_base <- purrr::map2_chr(tabs_src, titles, function(t, ti) {
      sn <- reg_sheet_name(reg_call(t)); if (!is.na(sn)) sn else ti
    })

    # sheet-stacking offsets: within a sheet each stacked table starts below the previous one (rows +
    # subtext + 6 blank, +1 for the col_var spanning-name header row); absolute geometry is derived
    # from `start` in the plan builder. Observed-curve shape tables (below) join the same offset, or a
    # second table on the sheet would land on top of one.
    shapes <- purrr::map(tabs_src, function(t)
      if (is_tab(t) && tab_wants_shape_table(t, "xl")) reg_shape_table(t) else NULL)
    shape_n <- purrr::map_int(shapes, function(st)
    if (is.null(st)) 0L else nrow(st) + length(attr(st, "note")) + 2L)
    # model-check pictures are drawn BEFORE the geometry too, so their height joins the same offset.
    check_imgs <- xl_check_images(tabs_src, check, data, theme = theme, lang = lang)
    check_n    <- purrr::map_int(check_imgs, xl_check_rows)
    newsheet <- sheet != dplyr::lag(sheet, default = -1L)
    start <- tibble::tibble(newsheet, rows = purrr::map_int(tabs, nrow),
                            sub = purrr::map_int(subtext, length) + shape_n + check_n) |>
      dplyr::group_by(gr = cumsum(as.integer(.data$newsheet))) |>
      dplyr::mutate(start = dplyr::lag(cumsum(.data$rows + .data$sub + 6L), default = 0L) + 1L) |>
      dplyr::pull(.data$start)

    # Clean AFTER the 25-char cut and BEFORE the de-duplication below: openxlsx2 would otherwise do
    # the identical substitution itself (with a warning) at add_worksheet() time -- i.e. after our
    # de-duplication, which would then have run on names that are not the final ones.
    sheet_titles <- substr(sheet_base[newsheet], 1, 25) |> xl_clean_sheet_name()
    sheet_titles <- dplyr::if_else(duplicated(sheet_titles),
                                   paste0(sheet_titles, ".2"), sheet_titles)
    nb <- 2
    while (length(unique(sheet_titles)) != length(sheet_titles)) {
      nb <- nb + 1
      sheet_titles <- dplyr::if_else(
        duplicated(sheet_titles),
        paste0(sub("..$", "", sheet_titles, perl = TRUE), ".", nb), sheet_titles)
    }

    opts <- list(
      font_num          = font_num,
      font_num_stars    = font_num_stars,
      font_text         = font_text,
      text_size         = text_size,
      colnames_rotation = colnames_rotation,
      wrap_rows         = wrap_rows,            # a text column's width cap: see xl_col_widths()
      text_size_headers = text_size_headers,
      text_size_subtext = text_size_subtext,
      ratio_cells       = ratio_cells,           # what a multiplicative cell holds: fold/raw/text
      theme             = theme                  # format() needs it for a publication palette's marks
    )

    # === Per-table plans (pure: raw values + numFmt codes + colour slots + font plan + geometry) ===
    plans <- purrr::pmap(
      list(tab = tabs, roles = roles, ann = purrr::map(rd, "ann"),
           bold_rows = purrr::map(rd, "bold_rows"),
           col_var_header = purrr::map(rd, "col_var_header"),
           start = start, sheet = sheet, title = titles, subtext = subtext, shape = shapes,
           check_imgs = check_imgs,
           legend_runs = legend_runs, colwidth = colwidth, transposed = transposed),
      tab_xl_plan_one, o = opts
    )

    # === Assemble the workbook on the main process (serial) =======================================
    wb <- xlb_new_workbook()
    xlb_base_font(wb, font_text, text_size)
    purrr::walk(sheet_titles, ~ xlb_add_sheet(wb, .))
    # freeze under the first table's own header block -- title, span row and unit row included, or a
    # fixed row would leave part of a multi-row header scrolling away.
    first_plan <- plans[!duplicated(sheet)]
    purrr::walk2(unique(sheet), first_plan,
                 function(sh, pl) xlb_freeze(wb, sh, pl$data_row0 + 1L, pl$freeze_col))
    reg <- xl_style_registrar(wb)
    # a column belongs to the SHEET, not the table: several tables stacked on one sheet reduce to the
    # pmax of their widths, or a narrow table below a wide one would squeeze it.
    sheet_w <- xl_sheet_widths(plans, sheet)
    purrr::walk2(plans, sheet, ~ xl_write_table(wb, .x, opts, reg, widths = sheet_w[[.y]]))

    xl_finish(function(p) xlb_save(wb, p), path, replace, open)
    invisible(tabs_base)
  }


# resolve the path ONCE: tab_xl_resolve_path() is not pure -- with `replace = FALSE` it auto-numbers
# past any existing file, so a second call would return Tab2.xlsx after writing Tab1.xlsx. The
# message matters because the default path is a tempdir() and `open = FALSE` gives no other way to
# find the file.
xl_finish <- function(write, path, replace, open) {
  path <- tab_xl_resolve_path(path, replace)
  write(path)
  cli::cli_inform(c("v" = "Excel file written to {.file {path}}"))
  # a failed *open* is non-fatal (e.g. WSL2 has no spreadsheet application) -- downgrade to a message.
  if (isTRUE(open)) {
    tryCatch(
      xlb_open(path),
      error = function(e) cli::cli_inform(c(
        "i" = "Could not open the file automatically (no spreadsheet application detected)."
      ))
    )
  }
  invisible(path)
}


tab_xl_resolve_path <- function(path, replace) {
  if (is.null(path)) {
    path <- getOption("tabxplor.export_dir")
    if (is.null(path)) path <- file.path(tempdir(), "Tab") else path <- file.path(path)
  } else {
    path <- path[[1]]
  }
  if (grepl("\\\\|/", path, perl = TRUE)) {
    dir_path <- sub("\\\\[^\\\\]+$|/[^/]+$", "", path, perl = TRUE)
    if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
  }
  if (!grepl("\\.xlsx$", path, perl = TRUE)) path <- paste0(path, ".xlsx")
  # shared with the jamovi exporter (R/jmvtab-export.R), so both number identically.
  export_number_path(path, replace)
}


# a text-mode fmt column (ci = "cell" / OR) becomes its format() display string; every other fmt
# column its raw get_num() number -- mixed column types in one tibble are fine (openxlsx2 writes
# each column by its R type).
xl_materialize_data <- function(tab, fmt_cols, text_fmt_cols, transposed = FALSE, theme = NULL,
                                fold = TRUE) {
  for (ci in fmt_cols) {
    tab[[ci]] <- if (isTRUE(transposed)) {
      as.character(tab[[ci]])                       # already a pre-formatted display string
    } else if (ci %in% text_fmt_cols) {
      format(tab[[ci]], special_formatting = TRUE, na = "", stars = TRUE, theme = theme)
    } else {
      # NaN -> NA so an empty numeric cell (a summary-stat / p-value row the test doesn't apply to)
      # writes BLANK rather than Excel's #VALUE!/#N/A -- openxlsx2 renders NaN as an error even when
      # NA is blanked (the na arg only covers NA). See xlb_na_argname for the NA half.
      v <- fmt_excel_value(tab[[ci]], fold = fold); v[is.nan(v)] <- NA_real_; v
    }
  }
  tibble::as_tibble(tab)
}

# a template's own literals become number-format literals: where a display has exactly one token,
# everything around it folds into the numFmt code ("(n={n})" -> \(#,##0\)), which is what lets an
# aside column (mat_aside_cols) look like the aside it replaced. Runs AFTER the significance stars.
xl_fold_literals <- function(code, disp) {
  val <- !is.na(code) & code != "TEXT" & !is.na(disp)
  for (t in unique(disp[val])) {
    if (!grepl("{", t, fixed = TRUE)) next
    seg <- parse_display_template(t)
    if (sum(seg$is_tok) != 1L) next                 # a composite keeps its Excel primary alone
    pj   <- which(seg$is_tok)
    pre  <- paste0(seg$pieces[seq_len(pj - 1L)], collapse = "")
    post <- paste0(seg$pieces[-seq_len(pj)],     collapse = "")
    if (!nzchar(pre) && !nzchar(post)) next
    hit  <- val & disp == t
    code[hit] <- xl_numfmt_affix(code[hit], prefix = pre, suffix = post)
  }
  code
}

# model-check plots rendered to PNG, one entry per input table (NULL where there is nothing to
# draw). `reg_check_plots()` refits from the stored recipe (meta$spec$call) and recovers the data
# frame from the NAME the call was written with, so `data` is only needed for a piped/subsetted call.
# ⚠ reg_check_plots() DRAWS on the current device as a side effect, so the first pass runs into a
# null device and each returned gtable is then drawn into its own PNG.
xl_check_images <- function(tabs, check, data, theme = NULL, lang = NULL, dpi = 150) {
  none <- vector("list", length(tabs))
  if (is.null(check) || isFALSE(check)) return(none)
  if (!isTRUE(tx_need_pkg(c("ggplot2", "gridExtra"),
                          "The model-check plots of `check`", severity = "inform")))
    return(none)
  purrr::map(tabs, function(t) {
    if (!is.data.frame(t) || !tab_is_reg(t)) return(NULL)
    grids <- tryCatch({
      grDevices::pdf(NULL)
      on.exit(grDevices::dev.off(), add = TRUE)
      g <- reg_check_plots(t, data = data, check = check, theme = theme, lang = lang)
      if (inherits(g, "gtable")) list(g) else g
    }, error = function(e) NULL)
    if (!length(grids)) return(NULL)
    # WARNING: imap() hands the NAME when its input is named and the INDEX when it is not, so a
    # parallel vector indexed by `i` errors the moment the list gains names. reg_check_plots()
    # returns a bare gtable for ONE model (wrapped here, unnamed -> `i` is an integer) and a list
    # NAMED by model for two or more (-> `i` is "age: diff"). The label IS that name: read `i`.
    imgs <- purrr::imap(grids, function(gt, i) {
      # `top` (the model's title) occupies the first layout row and spans every column, so it is
      # excluded from the panel count.
      nc <- max(1L, suppressWarnings(max(gt$layout$r, na.rm = TRUE)))
      nr <- max(1L, length(unique(gt$layout$t)) - 1L)
      # landscape and generous on width: a ggplot draws at a fixed point size, so a wider device gives
      # axis labels more room rather than shrinking them.
      w  <- min(13, 4.6 * nc); h <- min(9, 2.7 * nr + 0.4)
      f  <- tempfile(fileext = ".png")
      grDevices::png(f, width = w, height = h, units = "in", res = dpi)
      on.exit(grDevices::dev.off(), add = TRUE)
      grid::grid.newpage(); grid::grid.draw(gt)
      list(file = f, width = w, height = h, label = if (is.character(i)) i else "")
    })
    imgs
  })
}

# how many sheet ROWS an image block occupies -- one label row per image plus its height at Excel's
# default 15-point row. The stacking offsets must know, or the next table would sit under a picture.
xl_check_rows <- function(imgs)
  if (!length(imgs)) 0L else
    sum(vapply(imgs, function(im) as.integer(ceiling(im$height * 72 / 15)) + 2L, integer(1)))

# === SECTION: column widths -- ONE vector, measured from what the cell will show ===================
# Excel's width unit is one character of the workbook base font (xlb_base_font); format() renders
# the exact strings Excel shows, so nothing beyond the render model is needed to measure a column --
# unlike openxlsx2's own `bestFit` (see the header for why it can't be trusted).

# XL_PAD is the cell's left+right padding, ~5px = 0.7 characters, rounded up to a comfortable 1.
XL_PAD <- 1.0
xl_width_of <- function(nchars, ratio = 1) max(2.5, nchars * ratio + XL_PAD)

# a table showing stars/marks switches numbers to a MONOSPACE stack (tx_num_font) so the suffixes
# align, whose digit is about a tenth wider than the condensed sans the width unit is defined by.
XL_MONO_RATIO <- 1.1

# a wrapped cell (Excel gets the same `\n` breaks html does) is as wide as its longest LINE.
xl_text_width <- function(x) {
  x <- x[!is.na(x) & nzchar(x)]
  if (!length(x)) return(0L)
  max(nchar(unlist(strsplit(x, "[\n\r]"), use.names = FALSE)), 0L)
}

# the width vector for one table, one entry per sheet column: fmt columns -- the widest rendered
# cell, against the level header and the unit tag, never wrapped; the name col -- tab_vname_plan()'s
# own capped `chars`; other text -- the widest value against the header, capped at `wrap_rows` and
# wrapped past it. `colwidth` as a NUMBER keeps the fixed behaviour for fmt columns (and the sd twin).
XL_HEAD_LINES <- 2L

# the unit tag is a compound word ("<obs mean>"), so it must never wrap or size the column: its own
# smaller size (8pt) keeps a long tag inside the figures above it, and its angle brackets -- the
# console's own notation -- are excluded from the count.
XL_UNIT_SIZE <- 8
xl_col_widths <- function(tab, roles, cvh, o, colwidth, theme = "light") {
  ncl    <- ncol(tab)
  ratio  <- if (isTRUE(roles$has_stars)) XL_MONO_RATIO else 1
  hratio <- as.double(o$text_size_headers %||% o$text_size) / as.double(o$text_size)
  cap_text <- if (is.finite(o$wrap_rows %||% Inf)) as.integer(o$wrap_rows) else 40L
  uratio <- XL_UNIT_SIZE / as.double(o$text_size)
  head_w <- function(j) {
    lab <- if (!is.null(cvh)) cvh$clean[[j]] else names(tab)[[j]]
    un  <- if (!is.null(cvh) && !is.null(cvh$unit)) cvh$unit[[j]] else ""
    max(ceiling(xl_text_width(lab) * hratio / XL_HEAD_LINES),
        ceiling(max(0L, xl_text_width(un) - 2L) * uratio))
  }
  vname_chars <- purrr::map(roles$vname_plans %||% list(), ~ .$chars)
  vapply(seq_len(ncl), function(j) {
    col <- tab[[j]]
    nm  <- names(tab)[[j]]
    if (is_fmt(col)) {
      if (!identical(colwidth, "auto")) {
        cw <- as.double(colwidth)
        return(if (j %in% roles$sd_cols) max(5, cw * 0.6) else cw)
      }
      body <- tryCatch(format(col, special_formatting = FALSE, na = "", stars = TRUE, theme = theme),
                       error = function(e) as.character(col))
      return(xl_width_of(max(xl_text_width(body) * ratio, head_w(j))))
    }
    # the name column takes the width tab_vname_plan() already capped (a rotated run costs one
    # vertical line, so only the names that stayed horizontal size it).
    if (nm %in% names(vname_chars)) return(xl_width_of(vname_chars[[nm]]))
    # capped at `wrap_rows`: the values are already broken there, so this only ever holds an
    # unbreakable run, and Excel wraps it (the row is unmerged, so it fits the height itself).
    xl_width_of(max(min(cap_text, xl_text_width(as.character(col))), head_w(j)))
  }, double(1))
}

# The width vector PER SHEET: the element-wise max over every table stacked on it, padded to the
# widest. Named by sheet, so the writer looks its own up.
xl_sheet_widths <- function(plans, sheet) {
  by <- split(purrr::map(plans, ~ .$col_widths %||% double(0)), sheet)
  purrr::map(by, function(ws) {
    n <- max(0L, vapply(ws, length, integer(1)))
    if (!n) return(double(0))
    purrr::reduce(purrr::map(ws, ~ c(., rep(0, n - length(.)))), pmax)
  })
}

# how many wrapped LINES a text needs in a column `w` wide -- the estimator prose rows and merged
# label/span cells share. ⚠ Excel never auto-fits a MERGED cell's height, unlike an ordinary one.
xl_row_lines <- function(text, width) {
  text  <- text %||% ""
  width <- rep_len(pmax(1, as.double(width)), length(text))
  out   <- vapply(seq_along(text), function(i) {
    parts <- strsplit(text[[i]], "[\n\r]")[[1L]]
    if (!length(parts)) return(1L)
    as.integer(sum(pmax(1, ceiling(nchar(parts) / floor(width[[i]])))))
  }, integer(1))
  pmax(1L, out)
}

# how far a line of prose is merged, from column 1 up to roughly an A4 portrait text width (~17 cm =
# ~642 px at 96 dpi; Excel's width unit is ~7 px + 5 px padding). Reads the SAME width vector the
# writer sets on the sheet, so the two can never disagree.
XL_A4_PX <- 642
xl_prose_span <- function(widths, ncl) {
  px <- cumsum(widths * 7 + 5)
  max(1L, min(ncl, sum(px <= XL_A4_PX) + 1L))
}

# the height wrapped prose needs (⚠ a MERGED cell never auto-fits): ~5 px per character at the
# subtext size, one line per 11.5 points.
xl_prose_height <- function(text, span_px, size = 9) {
  per_line <- max(20L, floor(span_px / (size * 0.55)))
  lines    <- pmax(1L, ceiling(nchar(text) / per_line))
  lines * (size * 1.28) + 2
}

# The shape table as (row, col, text) cells: a header row, one row per curve, then the note -- the
# same four columns every other medium prints, in the order reg_shape_table() declares.
xl_shape_cells <- function(shape, row0) {
  if (is.null(shape) || nrow(shape) == 0L) return(NULL)
  hd <- attr(shape, "headers"); nt <- attr(shape, "note")
  purrr::list_rbind(c(
    list(tibble::tibble(row = row0, col = seq_along(hd), text = hd)),
    purrr::map(seq_len(nrow(shape)), function(i)
      tibble::tibble(row = row0 + i, col = seq_along(shape),
                     text = vapply(shape, function(cl) as.character(cl)[[i]], character(1)))),
    list(tibble::tibble(row = row0 + nrow(shape) + seq_along(nt), col = 1L, text = nt))))
}

tab_xl_plan_one <- function(tab, roles, ann, bold_rows, col_var_header, start, sheet, title, subtext,
                            shape = NULL, check_imgs = NULL, legend_runs = list(), colwidth, o,
                            transposed = FALSE) {
  n   <- nrow(tab)
  ncl <- ncol(tab)
  # a col_var spanning-NAME header row sits above the level-name header (whenever the table has a
  # col_var), shifting the header + data + everything below down by one row; `data_row0` / `header_row`
  # / `last_row` derive every absolute position from here.
  cvh        <- col_var_header
  has_span   <- !is.null(cvh) && any(nzchar(cvh$label))
  span_off   <- if (has_span) 1L else 0L
  span_row   <- start + 1L                       # the spanning-name row (used only if has_span)
  header_row <- start + 1L + span_off
  # the UNIT row, directly under the level header, says what each column holds ("row%", "mean (sd)")
  # -- also what a numeric col_var's now-blank level header no longer says. Sits INSIDE the header
  # block, so the header's bottom rule moves down to it.
  has_unit   <- !is.null(cvh) && !is.null(cvh$unit) && any(nzchar(cvh$unit))
  unit_off   <- if (has_unit) 1L else 0L
  unit_row   <- header_row + 1L                  # used only if has_unit
  data_row0  <- header_row + unit_off            # data row i -> i + data_row0
  data_rows  <- seq_len(n) + data_row0
  last_row   <- data_row0 + n

  fmt_cols    <- roles$fmt_cols
  txt_cols    <- roles$other_cols
  # monospace (Cascadia Mono) only for a table that SHOWS stars, per-table since a list export can
  # mix starred (reg) and plain (crosstab) sheets.
  font_num    <- tx_num_font("xl", roles$has_stars, plain = o$font_num, stars = o$font_num_stars)
  row_var_col <- roles$row_var_col
  totcols     <- roles$totcols
  # a transposed table's `tab` is plain character (no fmt columns), so its roles are read from
  # `roles` instead; its reference is a ROW (the Total), carried by bold_rows, not a column.
  ref_cols    <- if (isTRUE(transposed)) integer(0) else which(is_refcol(tab))

  cv_names      <- if (isTRUE(transposed)) unname(roles$col_var_map) else get_col_var(tab)
  # the left edge of each column BLOCK (tab_col_block_ids): Excel draws one rule there and the
  # table's own right edge closes the last one, so no rule can fall INSIDE a block.
  block_start   <- tab_block_starts(roles$col_blocks %||% integer(0))

  # every downstream consumer reads THIS width vector: the writer sets it on the sheet,
  # xl_prose_span() decides how far a footer line merges, merged-cell heights are computed against it.
  col_widths <- xl_col_widths(tab, roles, col_var_header, o, colwidth, theme = o$theme)

  # label runs lifted to ABSOLUTE sheet rows: `label_merges` skips length-1 runs (Excel rejects a
  # 1-cell "merge"); `vname_runs` are the name column's, the only ones that also rotate.
  label_merges <- purrr::imap(roles$label_runs, function(run, cl) {
    at <- which(run$show & run$span > 1L)
    tibble::tibble(col = match(cl, names(tab)),
                   row1 = at + data_row0, row2 = at + run$span[at] - 1L + data_row0)
  })
  label_merges <- if (length(label_merges)) dplyr::bind_rows(label_merges)
                  else tibble::tibble(col = integer(), row1 = integer(), row2 = integer())
  # which names turn is the prep's shared decision (tab_vname_plan), read here and by the html engine
  # so the two media agree.
  vname_runs   <- purrr::imap(roles$vname_plans %||% list(), function(p, cl) {
    j   <- match(cl, names(tab))
    run <- roles$label_runs[[cl]]
    at  <- if (is.null(run) || is.na(j)) integer(0) else which(run$show & run$span > 1L & p$vert)
    tibble::tibble(col = rep(j, length(at)), row1 = at + data_row0,
                   row2 = at + run$span[at] - 1L + data_row0)
  })
  vname_runs   <- if (length(vname_runs)) dplyr::bind_rows(vname_runs)
                  else tibble::tibble(col = integer(), row1 = integer(), row2 = integer())

  # cells are named by the SHARED plan (fmt_mult_plan), never by matching a raw token: a regression
  # cell displays `{est}`, which IS the odds ratio on an odds-ratio column.
  xl_code   <- function(col) {
    code <- format(col, syntax = "excel")
    if (identical(o$ratio_cells, "text")) code[fmt_mult_plan(col)$cells] <- "TEXT"
    code
  }
  # turning the whole column to text took every model-fit statistic in it along with the bracket, so
  # a numeric column is written with a hole at each text cell, filled individually below like a
  # sparkline. A transposed column is heterogeneous character throughout, so there it is all text.
  text_fmt_cols <- if (isTRUE(transposed)) fmt_cols else integer(0)
  text_cells    <- if (isTRUE(transposed)) NULL else
    purrr::list_rbind(purrr::map(fmt_cols, function(ci) {
      cc  <- tab[[ci]]
      hit <- which(!is.na(xl_code(cc)) & xl_code(cc) == "TEXT")
      if (!length(hit)) return(NULL)
      txt <- format(cc, special_formatting = TRUE, na = "", stars = TRUE, theme = o$theme,
                    pad = fig_space)
      keep <- hit[!is.na(txt[hit]) & nzchar(txt[hit])]
      if (!length(keep)) return(NULL)
      tibble::tibble(col = as.integer(ci), row = keep + data_row0, text = txt[keep])
    }))

  # Excel keeps only a merged range's top-left value, so a label's repeats below it become invisible
  # ghosts on unmerging. Blank them at the source, on the WRITTEN copy only (roles read off `tab`).
  xl_data <- xl_materialize_data(tab, fmt_cols, text_fmt_cols, transposed = transposed,
                                 theme = o$theme,
                                 fold = identical(o$ratio_cells, "fold"))
  # a row sparkline lives in a base-count cell holding NO number, so it displaces nothing: the column
  # stays a real editable count and these few cells are written afterwards, individually, as text.
  spark_cells <- if (isTRUE(transposed)) NULL else
    purrr::list_rbind(purrr::map(fmt_cols, function(ci) {
      cc  <- tab[[ci]]
      hit <- which(is.na(get_num(cc)) & tx_has_spark(get_display(cc)))
      if (!length(hit)) return(NULL)
      # ⚠ NOT `col` for the local: tibble() evaluates in a data mask, so the `col =` column would
      # shadow it and format() would dispatch on an integer.
      txt <- format(cc, special_formatting = TRUE, na = "", stars = FALSE, pad = fig_space)
      tibble::tibble(col = as.integer(ci), row = hit + data_row0, text = txt[hit])
    }))
  for (cl in names(roles$label_cols)) {
    if (!cl %in% names(xl_data)) next
    xl_data[[cl]] <- as.character(xl_data[[cl]])
    xl_data[[cl]][!roles$label_runs[[cl]]$show] <- NA_character_
  }

  # fold significance stars into the numFmt literal (0.0%\*\*\*), keeping the cell a real number; a
  # "TEXT"-coded column (ci / OR) is written as a string with Excel's "@" text format; NA codes stay
  # General. Stars are STORAGE-driven (get_stars() is "" when no pvalue was stored); when any cell is
  # starred, pad EVERY value cell's star literal to the column-max width so numbers stay aligned.
  numfmt <- if (length(fmt_cols)) purrr::map_dfr(fmt_cols, function(ci) {
    col <- tab[[ci]]
    if (ci %in% text_fmt_cols) {                        # text-mode column -> "@" per written cell
      # figure-space padding (Excel is a proportional font, unlike ASCII half-digit spaces in html);
      # a transposed column is already a pre-formatted display string.
      val  <- if (isTRUE(transposed)) as.character(col)
              else format(col, special_formatting = TRUE, na = "", stars = TRUE, theme = o$theme,
                          pad = fig_space)
      code <- ifelse(!is.na(val) & nzchar(val), "@", NA_character_)
      return(tibble::tibble(col = as.integer(ci), row = seq_along(code) + data_row0, code = code))
    }
    code <- xl_code(col)
    # the SAME suffix format() writes into the text, so Excel and every other backend annotate a cell
    # identically -- and a `contrib` column, which stars nothing, gets nothing here either.
    st   <- fmt_cell_suffix(col, stars = TRUE, theme = o$theme)
    val  <- !is.na(code) & code != "TEXT"
    if (any(val & nzchar(st))) {
      # an unstarred "" is width 0, so max() over every value cell IS the column-max star width.
      w      <- max(nchar(st[val]))
      st_pad <- tx_pad(st, w, "right", pad = fig_space) # glyphs left, pad right
      code[val] <- xl_numfmt_affix(code[val], suffix = st_pad[val])  # see header: escape, every section
    }
    code <- xl_fold_literals(code, get_display(col))
    code[!is.na(code) & code == "TEXT"] <- "@"
    tibble::tibble(col = as.integer(ci), row = seq_along(code) + data_row0, code = code)
  }) else tibble::tibble(col = integer(), row = integer(), code = character())
  numfmt <- dplyr::filter(numfmt, !is.na(.data$code))

  # text channel -> font (bold + colour, folded into the font plan below); background channel -> cell
  # fill (applied by the writer).
  aside_col <- vapply(seq_len(ncl), function(j)
    is_fmt(tab[[j]]) && fmt_is_aside(tab[[j]]), logical(1))
  sec_hex   <- color_secondary_hex(o$theme)
  colour <- if (length(fmt_cols)) purrr::map_dfr(fmt_cols, function(ci) {
    a <- ann[[names(tab)[ci]]]
    if (is.null(a$font)) return(NULL)
    rows <- seq_along(a$font) + data_row0
    # an aside column wears the console's aside ink and none of its emphasis, even inside a Total or
    # reference row (where `ann$font` would otherwise blacken it).
    ink  <- if (aside_col[[ci]]) rep(sec_hex, length(rows)) else a$font
    face <- !aside_col[[ci]]
    tibble::tibble(col = as.integer(ci), row = rows, hex = ink,
                   bold      = face & a$face_bold,
                   italic    = face & a$face_italic,
                   underline = if (face) a$face_underline else rep("", length(rows)),
                   fill      = if (aside_col[[ci]]) rep(NA_character_, length(rows))
                               else dplyr::if_else(a$back == "none", NA_character_, a$back))
  }) else tibble::tibble(col = integer(), row = integer(), hex = character(),
                         bold = logical(), italic = logical(), underline = character(),
                         fill = character())

  subtext_clean <- subtext[!is.na(subtext) & subtext != ""]
  subtext_rows  <- if (length(subtext_clean)) seq_along(subtext_clean) + last_row else integer()
  ref_rows      <- bold_rows + data_row0
  ref_row_cols  <- union(fmt_cols, txt_cols)

  # every font need is aggregated per cell into ONE complete descriptor -- see R/tab-xl-backend.R for
  # why (wb_add_font(update=) is buggy over scattered ranges). Base name/size are filled by the writer.
  mk_src <- function(rows, cols, name = NA_character_, size = NA_real_, bold = FALSE,
                     color = NA_character_, italic = FALSE) {
    if (!length(rows) || !length(cols)) return(NULL)
    g <- tidyr::expand_grid(row = as.integer(rows), col = as.integer(cols))
    dplyr::mutate(g, name = name, size = size, bold = bold, italic = italic, underline = "",
                  color = color)
  }
  txt_colour <- colour
  # the reference bold rides the PRIMARY: an aside column carries the same number set back, never a
  # second bold one.
  ref_cols     <- setdiff(ref_cols, which(aside_col))
  ref_row_cols <- setdiff(ref_row_cols, which(aside_col))
  fonts <- dplyr::bind_rows(
    mk_src(data_rows, fmt_cols, name = font_num),                                # numeric font
    mk_src(header_row, seq_len(ncl), bold = TRUE, size = o$text_size_headers),   # headers
    # the unit row is the console's own type tag ("<row%>", "<n>"): header size, regular weight,
    # italic like a pillar tag, in the theme's chrome grey (set back from any cell's own ink).
    if (has_unit) mk_src(unit_row, seq_len(ncl), size = XL_UNIT_SIZE, italic = TRUE,
                         color = tx_chrome_hex(o$theme)$grey),
    # a variable name is a heading, so the name column is bold throughout, as html has always done.
    mk_src(c(header_row, data_rows), roles$var_name_col, bold = TRUE),
    mk_src(c(header_row, data_rows), ref_cols, bold = TRUE),                     # reference cols
    mk_src(ref_rows, ref_row_cols, bold = TRUE),                                 # reference rows
    mk_src(start, 1L, bold = TRUE, size = 12),                                   # title
    mk_src(subtext_rows, 1L, size = o$text_size_subtext),                        # subtext
    # a reference row that is also an under-slot cell ends up bold+italic: `any(bold)` below lets the
    # structural bold win over the measure's non-bold, which is the intended reading.
    if (nrow(txt_colour)) tibble::tibble(row = txt_colour$row, col = txt_colour$col,
                                         name = NA_character_, size = NA_real_,
                                         bold = txt_colour$bold, italic = txt_colour$italic,
                                         underline = txt_colour$underline,
                                         color = txt_colour$hex)
  )
  if (nrow(fonts)) {
    fonts <- fonts |>
      dplyr::group_by(.data$row, .data$col) |>
      dplyr::summarise(
        name      = c(name[!is.na(name)], NA_character_)[1],
        size      = c(size[!is.na(size)], NA_real_)[1],
        bold      = any(.data$bold),
        italic    = any(.data$italic),
        # `underline` is "" / "single" / "double": a cell takes the STRONGEST rule any of its sources
        # asks for, which is what `any()` does on the logical aspects beside it.
        underline = face_underline_max(.data$underline),
        color     = c(color[!is.na(color)], NA_character_)[1],
        .groups = "drop")
  }

  bg <- dplyr::filter(colour, !is.na(.data$fill))
  bg_fill <- tibble::tibble(row = bg$row, col = bg$col, fill = bg$fill)

  styles <- xl_build_styles(
    header_row = header_row, unit_row = if (has_unit) unit_row else NA_integer_,
    data_rows = data_rows, last_row = last_row, ncl = ncl,
    fmt_cols = fmt_cols, txt_cols = txt_cols, totcols = totcols, block_start = block_start,
    tot_rows      = roles$totrows         + data_row0,
    tot_rows_1    = roles$totblock_top    + data_row0,
    tot_rows_last = roles$totblock_bottom + data_row0,
    end_group     = utils::head(roles$new_group, -1L) + data_row0,
    vname_col     = unname(roles$var_name_col), vname_runs = vname_runs,
    fonts = fonts, bg_fill = bg_fill, title_row = start, subtext_rows = subtext_rows, o = o
  )

  list(
    sheet = sheet,
    title = title, title_row = start,
    subtext = subtext_clean, subtext_row = last_row + 1L,
    # header row, one row per curve, then the note -- one blank line under the subtext block.
    shape_cells = xl_shape_cells(shape, last_row + length(subtext_clean) + 2L),
    check_imgs = check_imgs,
    check_row = last_row + length(subtext_clean) + 2L +
      (if (is.null(shape)) 0L else nrow(shape) + 3L),
    # the legend runs occupy the FIRST subtext rows (merged above), overwritten with rich text below.
    legend_runs = legend_runs, legend_row = last_row + 1L,
    # a text-mode column (ci = "cell" / OR) is written as its format() display STRING; every other
    # column writes the raw get_num() number, so the tibble mixes character and numeric columns.
    data = xl_data,
    header_row = header_row, ncl = ncl, data_row0 = data_row0,
    freeze_col = max(1L, length(txt_cols)) + 1L,       # index columns stay put beside the header
    prose_cols = xl_prose_span(col_widths, ncl),
    unit_row = if (has_unit) unit_row else NA_integer_,
    unit_names = if (has_unit) cvh$unit else NULL,
    # an index column has no unit, so its header spans both header rows: one merged, bottom-aligned
    # cell, putting "levels" on the same line as the "<row%>" beside it.
    head_merges = if (has_unit) unname(txt_cols) else integer(0),
    clean_names = if (!is.null(cvh)) cvh$clean else names(tab),
    span_row = if (has_span) span_row else NA_integer_,
    header_runs = if (has_span) tab_header_runs(cvh$label, cvh$group) else NULL,
    fmt_cols = fmt_cols, row_var_col = row_var_col, colwidth = colwidth,
    sd_cols = unname(roles$sd_cols),                   # the Excel-only "<var>_sd" siblings, narrower
    # `vname_col` (values ARE variable names) is merged AND rotated 90 degrees, so a long name costs
    # one narrow column; a kept tab_var is merged but never rotated -- its values are levels.
    label_merges = label_merges, vname_col = unname(roles$var_name_col),
    col_widths = col_widths,
    spark_cells = spark_cells,                         # base-count cells holding a sparkline, not a count
    text_cells = text_cells,                           # cells no number can hold (a `{ci}`, an `{n_range}`)
    styles = styles, numfmt = numfmt
  )
}


# Build the per-cell full style grid (font + fill + border + alignment) for one table, grouped into
# the fewest DISTINCT styles, each with a coalesced multi-area dims. numFmt is NOT here (it is applied
# by the writer as a separate merging pass). Borders are painted onto 4 side matrices (0 none / 1 thin
# / 2 double), alignment onto zone matrices (base -> header -> total cols -> total rows, last wins).
xl_build_styles <- function(header_row, unit_row = NA_integer_,
                            data_rows, last_row, ncl, fmt_cols, txt_cols, totcols,
                            block_start, tot_rows, tot_rows_1, tot_rows_last, end_group,
                            vname_col = integer(0), vname_runs = NULL,
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
  # the header's bottom rule closes the UNIT row when there is one: no line separates a column's name
  # from what it holds -- the unit reads as part of the header.
  head_bottom <- if (is.na(unit_row)) header_row else unit_row
  bb <- prow(bb, c(head_bottom, last_row, tot_rows_last), 1L)            # header/surround/bottomline/block bottom
  # one rule per block boundary, drawn on the block's FIRST column; the table's own last column
  # closes the last one -- never both sides, or a Total column boxes away the count carved out of it.
  bl <- pcol(bl, c(1L, block_start), 1L)                                  # first col / block starts
  br <- pcol(br, ncl, 1L)                                                 # the table's right edge
  bb <- prow(bb, end_group, 2L)                                           # between-group double (wins)

  # alignment: character/logical matrices, painted general -> specific (last wins)
  ah <- matrix(NA_character_, nb, ncl); av <- matrix("", nb, ncl)
  aw <- matrix(FALSE, nb, ncl);         ar <- matrix(0L, nb, ncl)
  di <- idx(data_rows); if (length(di)) av[di, ] <- "top"                 # data base valign
  hi <- idx(header_row)                                                   # header
  if (o$colnames_rotation == 0) { ah[hi, ] <- "center" } else { ah[hi, ] <- "left"; ar[hi, ] <- o$colnames_rotation }
  av[hi, ] <- "bottom"; aw[hi, ] <- TRUE
  # the unit row: LEFT (names its column, does not label its numbers), never rotated and NEVER wraps
  # (a compound tag like "<obs mean>" would break mid-word and read as two tags).
  ui <- idx(unit_row)
  if (length(ui)) { ah[ui, ] <- "left"; av[ui, ] <- "bottom"; aw[ui, ] <- FALSE; ar[ui, ] <- 0L }
  # numbers read RIGHT; a TEXT-written cell (a `{ci}` bracket, an `{n_range}`) would otherwise land
  # left, misaligning one column against the next.
  fcd <- ci(fmt_cols); if (length(fcd) && length(di)) ah[di, fcd] <- "right"
  # a LABEL column reads from the left and WRAPS: values are already broken at `wrap_rows`, so Excel
  # just needs leave to show the second line (row unmerged, so it fits the height itself).
  xcd <- ci(txt_cols)
  if (length(xcd)) { ah[, xcd] <- "left"; if (length(di)) aw[di, xcd] <- TRUE }
  # the NAME column wraps: tab_vname_plan() capped its width, and the prep already broke the names at
  # their own seams -- this only lets Excel honour the breaks (and hold a name it did not expect)
  vcw <- ci(vname_col); if (length(vcw)) aw[, vcw] <- TRUE
  # THE TOTAL COLUMN's own zone is the DATA, not the header: painting the whole column made its name
  # float at the top of a header row whose every other cell sits at the bottom.
  tc <- ci(totcols)
  if (length(tc) && length(di)) { ah[di, tc] <- "left"; av[di, tc] <- "top"; aw[di, tc] <- FALSE
                                  ar[di, tc] <- 0L }
  if (length(tc) && length(hi)) ah[hi, tc] <- "left"    # ... its NAME reads with its cells
  tri <- idx(tot_rows)                                                    # total rows
  if (length(tri)) {
    fc <- ci(fmt_cols); if (length(fc)) { ah[tri, fc] <- "right"; av[tri, fc] <- "top"; aw[tri, fc] <- FALSE }
    xc <- ci(txt_cols); if (length(xc)) { ah[tri, xc] <- "left";  av[tri, xc] <- "top"; aw[tri, xc] <- TRUE }
    if (length(tc))    { ah[tri, tc] <- "left";  av[tri, tc] <- "top"; aw[tri, tc] <- FALSE }
  }
  # the row-variable NAME column rotates 90 degrees, centred on the block it merges over, so a long
  # name costs one narrow column; painted LAST so it beats the total-row/total-col zones above. Only
  # the MERGED runs rotate -- a 1-row block stays horizontal, or rotating it just makes the row tall.
  if (length(vname_col) > 0 && !is.null(vname_runs) && nrow(vname_runs) > 0) {
    vc <- ci(vname_col)
    for (k in seq_len(nrow(vname_runs))) {
      vi <- idx(vname_runs$row1[k]:vname_runs$row2[k])
      if (length(vi) && length(vc)) {
        ar[vi, vc] <- 90L; ah[vi, vc] <- "left"; av[vi, vc] <- "center"; aw[vi, vc] <- TRUE
      }
    }
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
  cells$fital  <- !is.na(fm) & fonts$italic[fm]     # a publication palette's face beyond weight
  cells$fund   <- dplyr::coalesce(fonts$underline[fm], "")
  cells$fcolor <- fonts$color[fm]
  # overlay per-cell fill
  lm <- if (nrow(bg_fill)) match(bkey, paste(bg_fill$row, bg_fill$col, sep = ":")) else rep(NA_integer_, nrow(cells))
  cells$fill <- bg_fill$fill[lm]

  # title + subtext cells (their own simple styles)
  extra <- dplyr::bind_rows(
    # the title sits at the BOTTOM of its (merged, wrapped) cell, against the table it names ...
    tibble::tibble(row = title_row, col = 1L, bt = 0L, bb = 0L, bl = 0L, br = 0L,
                   ah = "left", av = "bottom", aw = TRUE, ar = 0L,
                   fname = o$font_text, fsize = 12, fbold = TRUE, fital = FALSE, fund = "",
                   fcolor = NA_character_, fill = NA_character_),
    # ... and a footer line at the TOP of its own, reading down from the table
    if (length(subtext_rows)) tibble::tibble(row = subtext_rows, col = 1L, bt = 0L, bb = 0L, bl = 0L, br = 0L,
                   ah = "left", av = "top", aw = TRUE, ar = 0L,
                   fname = o$font_text, fsize = as.double(o$text_size_subtext), fbold = FALSE,
                   fital = FALSE, fund = "", fcolor = NA_character_, fill = NA_character_))
  cells <- dplyr::bind_rows(cells, extra)

  # group into distinct styles + coalesce each style's cells to the fewest multi-area dims
  cells |>
    dplyr::group_by(.data$fname, .data$fsize, .data$fbold, .data$fital, .data$fund,
                    .data$fcolor, .data$fill,
                    .data$bt, .data$bb, .data$bl, .data$br,
                    .data$ah, .data$av, .data$aw, .data$ar) |>
    dplyr::summarise(dims = xl_coalesce(.data$col, .data$row), .groups = "drop")
}


# deduplicates fonts / fills / borders / composed cell-xfs by CONTENT across ALL tables and hands
# out GLOBALLY-UNIQUE style names -- see header for why (openxlsx2's styles_mgr is workbook-global).
xl_style_registrar <- function(wb) {
  sm  <- wb$styles_mgr
  fc  <- new.env(parent = emptyenv()); lc <- new.env(parent = emptyenv())
  bc  <- new.env(parent = emptyenv()); xc <- new.env(parent = emptyenv())
  ctr <- 0L
  uid <- function() { ctr <<- ctr + 1L; ctr }
  # scheme = "" -- see R/tab-xl-backend.R for why. It is safely absent from the dedup key below only
  # because it is a constant. `italic`/`underline` carry a publication palette's typography (constant
  # FALSE / "" under the colour palettes); `underline` is OOXML's own vocabulary ("single"/"double").
  font_id <- function(name, size, bold, color, italic = FALSE, underline = "") {
    key <- paste(name, size, bold, italic, underline, color, sep = "\r")
    if (is.null(fc[[key]])) {
      args <- list(name = name, sz = as.character(size), scheme = "")
      if (isTRUE(bold))      args$b <- "1"
      if (isTRUE(italic))    args$i <- "1"
      if (nzchar(underline)) args$u <- underline
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
  # composed cell xf: dedup on the full (font, fill, border, alignment) tuple.
  xf_id <- function(fname, fsize, fbold, fcolor, fill, bt, bb, bl, br, ah, av, aw, ar,
                    fital = FALSE, fund = "") {
    fid <- font_id(fname, fsize, fbold, fcolor, fital, fund)
    lid <- fill_id(fill)
    bid <- border_id(bt, bb, bl, br)
    key <- paste(fid, lid, bid, ah, av, aw, ar, sep = "\r")
    if (is.null(xc[[key]])) {
      nm <- paste0("txx", uid())
      sm$add(openxlsx2::create_cell_style(
        font_id = fid, fill_id = lid, border_id = bid,
        horizontal = ah, vertical = av, wrap_text = aw, text_rotation = ar), nm)
      xc[[key]] <- sm$get_xf_id(nm)
    }
    xc[[key]]
  }
  list(xf_id = xf_id)
}


# numFmt is NOT applied here -- it is a separate merging pass in the writer (cross-aspect, per cell).
xl_apply_styles <- function(wb, s, styles, reg) {
  if (!nrow(styles)) return(invisible(wb))
  for (i in seq_len(nrow(styles))) {
    r <- styles[i, ]
    if (is.na(r$dims)) next
    xf <- reg$xf_id(
      r$fname, r$fsize, r$fbold, r$fcolor, r$fill,
      r$bt, r$bb, r$bl, r$br,
      if (!is.na(r$ah)) r$ah else "",
      if (nzchar(r$av)) r$av else "",
      if (isTRUE(r$aw)) "1" else "",
      if (r$ar != 0L) as.character(r$ar) else "",
      isTRUE(r$fital), if (is.na(r$fund)) "" else as.character(r$fund))
    xlb_set_cell_style(wb, s, r$dims, xf)
  }
  invisible(wb)
}


# per-sheet writer: raw values, then the precomposed cell styles by id, then the numFmt merging pass
# and the column widths / row heights. `reg` is the workbook-scoped registrar shared across tables.
xl_write_table <- function(wb, plan, o, reg, widths = NULL) {
  s   <- plan$sheet
  hdr <- plan$header_row

  # values: raw numbers + header, title, subtext (styles applied below). With a UNIT row between the
  # header and the data, the block is written headerless one row lower -- the header cells are
  # overwritten from `clean_names` below in either case.
  if (is.na(plan$unit_row)) xlb_write_data(wb, s, plan$data, hdr, 1L)
  else                      xlb_write_data(wb, s, plan$data, plan$unit_row + 1L, 1L, col_names = FALSE)
  # sparklines/text cells one at a time: openxlsx2 types per CELL, so a text glyph drops into an
  # otherwise numeric column without turning the whole column into text.
  for (cells in list(plan$spark_cells, plan$text_cells))
    if (!is.null(cells) && nrow(cells))
      purrr::pwalk(cells, function(col, row, text) xlb_write_cell(wb, s, xl_cell(row, col), text))
  xlb_write_cell(wb, s, xl_cell(plan$title_row, 1L), plan$title)
  if (length(plan$subtext)) xlb_write_cell(wb, s, xl_cell(plan$subtext_row, 1L), plan$subtext)
  # the title sits at the BOTTOM of its merged cell (against the table it names), a footer line at
  # the TOP (reading down from the table).
  prose_rows <- c(plan$title_row, seq_along(plan$subtext) + plan$subtext_row - 1L)
  prose_txt  <- c(plan$title %||% "", plan$subtext)
  if (plan$prose_cols > 1L)
    for (r in prose_rows)
      xlb_merge(wb, s, paste0(xl_cell(r, 1L), ":", xl_cell(r, plan$prose_cols)))
  keep <- !is.na(prose_txt) & nzchar(prose_txt)
  if (any(keep))
    xlb_row_heights(wb, s, prose_rows[keep],
                    xl_prose_height(prose_txt[keep], XL_A4_PX, o$text_size_subtext))
  if (!is.null(plan$shape_cells) && nrow(plan$shape_cells))
    purrr::pwalk(plan$shape_cells, function(row, col, text)
      xlb_write_cell(wb, s, xl_cell(row, col), text))
  # model-check pictures, each under a plain label naming the model it checks
  r <- plan$check_row
  for (im in plan$check_imgs %||% list()) {
    if (nzchar(im$label)) xlb_write_cell(wb, s, xl_cell(r, 1L), im$label)
    xlb_add_image(wb, s, xl_cell(r + 1L, 1L), im$file, im$width, im$height)
    r <- r + ceiling(im$height * 72 / 15) + 2L
  }

  # overwrite the level-header cells with the suffix-stripped labels (the col_var name is written in
  # the spanning row above), then the merged col_var spanning-name row.
  for (j in seq_len(plan$ncl)) xlb_write_cell(wb, s, xl_cell(hdr, j), plan$clean_names[j])
  # the unit row below it: what each column HOLDS, written once per BLOCK.
  if (!is.na(plan$unit_row)) {
    for (j in which(nzchar(plan$unit_names)))
      xlb_write_cell(wb, s, xl_cell(plan$unit_row, j), plan$unit_names[j])
    for (j in plan$head_merges)
      xlb_merge(wb, s, paste0(xl_cell(hdr, j), ":", xl_cell(plan$unit_row, j)))
  }
  if (!is.na(plan$span_row)) {
    runs <- plan$header_runs
    col0 <- 1L
    for (k in seq_along(runs$labels)) {
      c1 <- col0; c2 <- col0 + runs$spans[k] - 1L
      if (nzchar(runs$labels[k])) {
        # a span belonging to a SUB-POPULATION puts it on its own line above the variable, an in-cell
        # newline with wrap_text set on the span row below.
        xlb_write_cell(wb, s, xl_cell(plan$span_row, c1),
                       if (nzchar(runs$groups[k])) paste0(runs$groups[k], "\n", runs$labels[k])
                       else runs$labels[k])
        if (c2 > c1)
          xlb_merge(wb, s, paste0(xl_cell(plan$span_row, c1), ":", xl_cell(plan$span_row, c2)))
      }
      col0 <- c2 + 1L
    }
  }

  # merge each LABEL run vertically, so a row/tab variable is named once per block. Merged BEFORE the
  # styles: openxlsx2 keeps a merged range's top-left value, and set_cell_style() still reaches every
  # cell of it.
  if (nrow(plan$label_merges)) {
    lm <- plan$label_merges
    for (k in seq_len(nrow(lm))) {
      xlb_merge(wb, s, paste0(xl_cell(lm$row1[k], lm$col[k]), ":", xl_cell(lm$row2[k], lm$col[k])))
    }
  }

  # --- styles: one composed xf (font + fill + border + alignment) per distinct cell style ---
  xl_apply_styles(wb, s, plan$styles, reg)

  # style the col_var spanning-name row (bold + centred); wrap_text when any span carries a
  # sub-population line, so the two lines show.
  if (!is.na(plan$span_row)) {
    span_wrap <- if (any(nzchar(plan$header_runs$groups))) "1" else ""
    xf <- reg$xf_id(o$font_text, o$text_size_headers, TRUE, NA_character_, NA_character_,
                    0L, 0L, 0L, 0L, "center", "", span_wrap, "")
    xlb_set_cell_style(wb, s, paste0(xl_cell(plan$span_row, 1L), ":", xl_cell(plan$span_row, plan$ncl)), xf)
  }

  # --- number formats: one shared code over the fewest coalesced ranges (merges onto the xf) ---
  if (nrow(plan$numfmt)) {
    plan$numfmt |>
      dplyr::group_by(.data$code) |>
      dplyr::summarise(dims = xl_coalesce(.data$col, .data$row), .groups = "drop") |>
      purrr::pwalk(function(code, dims) xlb_numfmt(wb, s, dims, code))
  }

  # --- overwrite the legend rows (first of the subtext block) with coloured rich text ---
  if (length(plan$legend_runs)) {
    for (i in seq_along(plan$legend_runs)) {
      runs <- plan$legend_runs[[i]]
      if (length(runs))
        xlb_write_richtext(wb, s, xl_cell(plan$legend_row + i - 1L, 1L), runs,
                           size = o$text_size_subtext, font = o$font_text)
    }
  }

  # --- column widths / row heights: set per SHEET (pmax over every table stacked on it), not per table ---
  w <- widths %||% plan$col_widths
  # a ROTATED column header needs width for its turned line rather than for its text
  rot <- o$colnames_rotation
  if (rot > 0 && length(plan$fmt_cols))
    w[plan$fmt_cols] <- if (rot < 60) 8 else 6 + 8 * cos(rot / 90 * pi / 2)
  if (length(w)) xlb_col_widths(wb, s, seq_along(w), w)
  if (rot > 0) xlb_row_heights(wb, s, plan$header_row, 13.8 + 105 * sin(rot / 90 * pi / 2))
  # ⚠ Excel never auto-fits a MERGED cell's height: the col_var span row can hold two lines, so it
  # is given the height its content needs.
  if (!is.na(plan$span_row) && !is.null(plan$header_runs)) {
    hr   <- plan$header_runs
    at   <- cumsum(c(1L, utils::head(hr$spans, -1L)))
    span_w <- vapply(seq_along(at), function(k)
      sum(w[at[[k]]:min(length(w), at[[k]] + hr$spans[[k]] - 1L)]), double(1))
    ln <- max(xl_row_lines(hr$labels, pmax(1, span_w)) +
                as.integer(nzchar(hr$groups)))
    if (ln > 1L) xlb_row_heights(wb, s, plan$span_row, ln * (as.double(o$text_size_headers) * 1.35))
  }

  invisible(wb)
}


# which axis holds the dependent variable(s): under pct="row" a row is the group and the column
# distribution is what is described ("race by marital"); under pct="col" the axes swap. `pct` is not
# an argument or a stored attribute -- its only trace is the fmt columns' `pct_type`, so only an
# all-"col" table flips; a mean/coefficient (`none`) cannot vote, and a mixed table falls back
# outcome-first rather than guessing.
tab_title_rows_first <- function(tabs) {
  types <- purrr::map_chr(tabs, ~ if (is_fmt(.)) get_pct_type(.) else NA_character_)
  dir   <- types[!is.na(types) & types %in% c("row", "col")]
  length(dir) > 0 && all(dir == "col")
}

# Name a variable set for a title: every name up to `max`, then "+N more" -- never "multi", which named
# nothing, and never a bare index. Placeholders and empties drop out.
tab_title_names <- function(x, max = 2) {
  x <- as.character(x)
  x <- x[is_real_col_var(x)]
  if (length(x) == 0) return("")
  if (length(x) <= max) return(paste(x, collapse = ", "))
  paste0(paste(x[seq_len(max)], collapse = ", "), " +", length(x) - max, " more")
}

tab_get_titles <- function(tabs, row, col, tab, max = 2) {
  # the DEPENDENT variable is named first ("ROCK, JAZZ by DIPLOM" reads as the thing described, then
  # what it is broken down by), which under pct="row" is the col_vars.
  rows <- tab_title_names(row, max)
  cols <- tab_title_names(col, max)
  swap <- tab_title_rows_first(tabs)
  a    <- if (swap) rows else cols     # the outcome axis, named first
  b    <- if (swap) cols else rows
  res  <- if (!nzchar(a) && !nzchar(b)) "Table"
          else if (!nzchar(a)) b
          else if (!nzchar(b)) a
          else paste(a, "by", b)
  tabn <- if (missing(tab)) "" else tab_title_names(tab, max)
  if (nzchar(tabn)) res <- paste0(res, " (tabbed by ", tabn, ")")
  res
}


