# PURPOSE: Export tabxplor tables to Excel with formatting and colors via openxlsx2.
# ROLE: Primary export format for sharing tables with non-R users. Consumes the shared exporter prep
#       (R/tab-export-prep.R) for roles / references / bold rows / the resolved per-cell colour, and
#       the openxlsx2 backend (R/tab-xl-backend.R). tab_xl_plan_one() does the pure per-table CPU
#       (values + numFmt codes + a precomposed per-cell STYLE grid via xl_build_styles);
#       xl_write_table() writes the values, applies the styles by id, then merges numFmt on.
# KEY CONSTRAINTS:
#   - openxlsx2 is Suggests-only -- the ONE requireNamespace() guard is in tab_xl(); every engine call
#     goes through the unguarded xlb_* wrappers or xl_apply_styles' create_*/set_cell_style compose.
#   - EXCEL CANNOT PRINT A COMPOSITE CELL -- one value + one numFmt, so a bracket cannot survive.
#     Every ASIDE therefore becomes a COLUMN of its own (mat_aside_cols, R/tab_classes.R) carrying
#     its own segment ("(n={n})"), and the source column keeps its primary alone. The header block is
#     three rows: the col_var span, the level names, and the UNIT row -- so the data block is written
#     HEADERLESS one row lower when there is one, and the header's bottom rule moves down to close it.
#   - THE CELL IS A NUMBER, and everything a template writes around it lives in the numFmt code:
#     the significance stars, an aside's brackets, a test label, a sigma. One rule (xl_fold_literals)
#     for all of them, applied to EVERY SECTION of the code (xl_numfmt_affix) -- a two-section code
#     would otherwise wear its stars on the negative half alone.
#   - A MULTIPLICATIVE CELL HOLDS ITS READING VALUE (fmt_excel_value: the signed fold), printed by a
#     two-section code, so "1/2.11" reaches the workbook without costing the cell its numeric type.
#     `ratio_cells = "raw"` / `"text"` are the two opt-outs. See the ?tab_xl section for the one
#     formula that recovers the raw ratio.
#   - TEXT IS A PROPERTY OF A CELL: a `{ci}` bracket and a real min-max `{n_range}` are written
#     individually into an otherwise numeric column, so a model-fit statistic beside them stays a
#     number (and takes the reader's own decimal separator).
#   - COLOUR IS READ, NEVER RE-DERIVED: ann$font / ann$back / ann$face_* (R/tab-export-prep.R), the
#     same fields tab_kable and tab_plot consume -- so a greyed cell is grey here too. An ASIDE
#     column is the console's `sec()`: the secondary grey, no bold, no stars, whatever row it is in.
#   - Export-Parity: the numFmt codes come from format(x, syntax = "excel") (fmt_class.R
#     excel_numfmt_code), the single display source of truth. numFmt literals are backslash-escaped
#     via xl_numfmt_literal() -- NEVER double-quote-wrapped, which crashes the older jamovi-bundled
#     openxlsx2 ("xml import unsuccessful").
#   - Shared-style fast path: each cell's FULL style (font+fill+border+alignment) is precomposed and
#     applied ONCE by id (set_cell_style) over the fewest coalesced multi-area dims (xl_coalesce) --
#     far fewer + cheaper openxlsx2 calls than a wb_add_* per aspect. numFmt merges on afterwards.
#   - ONE workbook-scoped style registrar (xl_style_registrar) dedups styles across ALL tables and
#     keeps style NAMES globally unique: openxlsx2's styles_mgr is workbook-global and resolves a name
#     to its FIRST match, so per-table name reuse mis-applied table 1's styles to every later table.
#   - PROSE IS MERGED AND WRAPPED, to about an A4 portrait width (xl_prose_span): a title or a footer
#     legend left in one narrow column is a paragraph in one cell, and an Excel -> Word paste then
#     sizes that column to the paragraph. ⚠ Excel does not auto-fit a MERGED cell's height, so the
#     row height is computed (xl_prose_height) or the legend is clipped to one line.
#   - The plan builder is pure; the workbook is assembled serially (the openxlsx2 write dominates and
#     is inherently serial -- parallelising it was measured not worth it).

#' Excel output for tabxplor tables, with formatting and colors
#' @description The Excel exporter behind \code{\link{tab_export}}: `tab_export(x, format = "xl")`
#' calls this. To modify the colors used into the Excel table, you can change the
#' global options with \code{\link{set_color_style}} and \code{\link{set_color_breaks}}.
#' @eval tab_args_rd("tab_xl")
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
#' @param check Model-check plots to draw under each `tab_reg()` table: `FALSE` (the default),
#'   `"auto"`, or a vector of check keys --- the same values \code{\link{reg_check_plots}} takes,
#'   which is what draws them. Each grid is written as a picture below the table it belongs to.
#'   Needs `ggplot2` and `gridExtra`; a crosstab takes none.
#' @param data The data frame the models were fitted on. Only needed when `check` is on AND the
#'   \code{\link{tab_reg}} call cannot be replayed from the name it was written with (a `%>%`
#'   pipeline, a subset expression) --- an ordinary `tab_reg(gss, ...)` recovers it by itself.
#' @param ratio_cells What a ratio / odds-ratio cell holds in the workbook. Excel cannot compute
#'   inside a number format, so a cell storing `0.83` cannot be made to print `\u00f71.2` the way the
#'   console does. `"fold"` (the default) stores the **reading value** instead --- the fold, signed by
#'   its direction (`x` at or above the neutral, `-1/x` below it) --- which a two-section number
#'   format prints as `\u00d71.20` and `\u00f71.20`, `2.11` and `1/2.11`. The cell stays a real
#'   number: it sorts and filters in the direction it is read, and takes the reader's own decimal
#'   separator. `"raw"` stores the untransformed ratio (printed `\u00d70.83`); `"text"` writes the
#'   exact display string, which reads perfectly but is no longer a number. Option twin:
#'   \code{tabxplor.xl_ratio_cells}.
#' @section Recovering the raw ratio in Excel:
#' A ratio or odds-ratio cell holds its **reading value**: the fold, signed by its direction. The sign
#' IS the marker --- negative means the cell reads `\u00f7` (or `1/`) --- so one formula gives the raw
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
#' @param font_text,font_num,font_num_stars Fonts for text (labels, headers) and for numbers. The number
#'   font is chosen \strong{per table}: \code{font_num} (default \code{"DejaVu Sans"}) when the table
#'   shows no significance stars, and \code{font_num_stars} (default \code{"Cascadia Mono"}, a
#'   \strong{monospace} font) when it does -- monospace aligns the stars and \code{(n=...)} composites,
#'   which a proportional font cannot. Defaults from \code{options(tabxplor.xl_font_text)} /
#'   \code{options(tabxplor.xl_font_num)} / \code{options(tabxplor.xl_font_num_stars)}. Note that xlsx,
#'   unlike CSS, has \strong{no font-fallback list}: only one name can be recorded per font, so if it is
#'   missing on the machine opening the workbook, Excel substitutes by its own rules and no fallback can
#'   be named here. Set the options to a font you know is installed.
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
           colwidth = 10, color_legend = TRUE,
           sheets = "auto", titles, caption = NULL,
           font_text = NULL, font_num = NULL, font_num_stars = NULL,
           text_size = 10, text_size_headers = 9, text_size_subtext = 9,
           theme = NULL,
           color = TRUE,
           transpose = FALSE, var_names = NULL,
           ratio_cells = NULL, check = FALSE, data = NULL,
           print_color_legend = lifecycle::deprecated(), ...) {

    # Phase 19l: the retired inert arguments (`color_type`, `html_24_bit`, ...) ride `...`.
    tx_deprecate_inert(rlang::list2(...), "tab_xl")

    # 20b: ONE default idiom on the public surface -- an option-backed argument says `NULL` and the
    # value comes from the declared table (TAB_OPTIONS, R/tabxplor-options.R). These four used to
    # spell `getOption("tabxplor.xl_*", <a literal repeated in .onLoad and in ?tabxplor-options>)`
    # in the formal itself, which is the third of the three idioms 20b collapsed.
    font_text      <- font_text      %||% tx_option("xl_font_text")
    font_num       <- font_num       %||% tx_option("xl_font_num")
    font_num_stars <- font_num_stars %||% tx_option("xl_font_num_stars")
    ratio_cells    <- match.arg(ratio_cells %||% tx_option("xl_ratio_cells"),
                                c("fold", "raw", "text"))

    # Phase 13a: install a per-table color_breaks override for the render (no-op otherwise).
    .cb <- push_color_breaks(tabs); on.exit(pop_color_breaks(.cb), add = TRUE)

    if (!requireNamespace("openxlsx2", quietly = TRUE)) {
      stop(paste0("Package \"openxlsx2\" needed for this function to work. ",
                  "You can install it with : install.packages('openxlsx2')"),
           call. = FALSE)
    }

    if (length(replace) == 0) replace <- length(path) != 0

    # Phase 17g: the long-inert `n_min` / `hide_near_zero` / `conditional_format` args were removed
    # before the 2.0.0 CRAN freeze (they never did anything: n_min moved to tab(n_min=), near-zero
    # greying was dropped, and Excel conditional formatting was never implemented). Passing them now
    # errors "unused argument" -- accepted per the Phase 17 §Settled decisions ruling.

    # Phase 10j: `print_color_legend` renamed to `color_legend` (unified across exporters).
    if (lifecycle::is_present(print_color_legend)) {
      lifecycle::deprecate_soft("2.0.0", "tab_xl(print_color_legend)", "tab_xl(color_legend)")
      color_legend <- print_color_legend
    }
    # Shared option resolver (theme/color/color_legend/transpose). Phase 10j makes tab_xl theme-aware:
    # the palettes below now honour `theme` (was hardcoded "light").
    o <- resolve_export_opts(theme = theme, color = color, color_legend = color_legend,
                             transpose = transpose, caption = caption, var_names = var_names,
                             tabs = tabs)
    theme <- o$theme
    color_legend <- o$color_legend; color <- o$color
    # `caption` (single) is the unified alias; an explicit `titles` (per-sheet) still wins.
    if (!is.null(caption) && missing(titles)) titles <- caption

    tabs_base <- tabs
    # Graceful degrade (single input): write the raw frame (+ a message) instead of crashing when the
    # input can't be read as a tabxplor table.
    rv <- if (is.data.frame(tabs)) tab_render_vars(tabs) else list(degrade = FALSE)
    if (isTRUE(rv$degrade)) {
      tab_degrade_inform(rv$reason)
      xl_finish(function(p) xlb_write_xlsx(tibble::as_tibble(tabs), p), path, replace, open)
      return(invisible(tabs_base))
    }
    if (is.data.frame(tabs)) tabs <- list(tabs)

    # Phase 10j: display-extra materialise (backend "xl" keeps a real `n` column) is centralised in
    # tab_export_prep(); tab_xl just passes transpose = transpose below. Phase 14o: transpose is now a
    # render-model flip (tx_transpose_render), AFTER materialise, so its `tab` is a plain character grid
    # (values written as TEXT here; editable numbers deferred -- see tx_transpose_render()).
    colwidth <- vctrs::vec_recycle(colwidth, length(tabs))

    # === Shared exporter prep (Phase 10g/10j/17g) ==================================
    # Role detection (fmt / other / total columns, total-block borders, references, bold rows) AND the
    # two-channel colour are derived ONCE by the shared framework (R/tab-export-prep.R). compact =
    # FALSE keeps one prep-table per input tab (each -> its own sheet region). Phase 10j: `compute`
    # includes "colors" so the per-column `ann` carries the theme-resolved colour. Phase 17g: xl now
    # consumes ann's `text_hex`/`bg_hex` directly (the fmt_channel_codes source the CSS side reads) --
    # no private slot->hex palette, so xl is theme-aware through the ONE shared colour source.
    compute <- c("refs", "bold")
    if (color) compute <- c(compute, "colors")
    prep <- tab_export_prep(
      tabs, backend = "xl", drop_tab_vars = remove_tab_vars,
      list_method = TRUE, compute = compute, transpose = transpose,
      theme = theme, var_names = o$var_names,
      color_legend = color_legend, what = "tab_xl()"
    )
    rd <- prep$tables

    # Graceful degrade: any unreadable list member is written as a plain sheet, with a message.
    if (any(purrr::map_lgl(rd, ~ isTRUE(.$vars$degrade)))) {
      purrr::walk(rd, ~ if (isTRUE(.$vars$notify)) tab_degrade_inform(.$vars$reason))  # batch-aware
      xl_finish(function(p) xlb_write_xlsx(purrr::map(rd, ~ tibble::as_tibble(.$tab)), p),
                path, replace, open)
      return(invisible(tabs_base))
    }

    tabs           <- purrr::map(rd, "tab")           # ungrouped, tab_vars dropped when requested
    # Phase 14o: a transposed table's `tab` is a plain character grid; the subtext / colour legend /
    # title read the MEASURES + variable roles off the original fmt table, kept as `color_src`.
    tabs_src       <- purrr::map(rd, ~ if (is.null(.$color_src)) .$tab else .$color_src)
    transposed     <- purrr::map_lgl(rd, ~ isTRUE(.$transposed))
    roles          <- purrr::map(rd, "roles")
    # Phase 14d: the SOURCE names for the title (`row_var` is the column holding the labels, which on
    # a merged table is the literal "levels"); geometry elsewhere keeps using `roles`.
    # Phase 14i: the prep now always passes `row_vars` through, so the `%||%` fallback that used to
    # guard this line is gone -- it was also a latent bug (base `%||%` is R >= 4.4; the package
    # supports R >= 4.1 and imports it from nowhere), and it silently swallowed the missing field.
    row_vars       <- purrr::map(rd, ~ .$vars$row_vars)
    tab_vars       <- purrr::map(rd, ~ .$vars$tab_vars)
    col_vars_plain <- purrr::map(rd, ~ .$vars$col_vars)

    # Phase 14u: a `tabxplor_tabs` is an EXPLICIT collection of independent tables (one reg comparison
    # per outcome, or a several-row_vars output_list) -> one sheet each by default. The col-var "auto"
    # stacking (below) grouped tables that share col_vars onto one sheet, which is wrong for these; it is
    # kept for a plain manual `list(tab1, tab2)`.
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

    # subtext (+ colour legend) computed once on the main process. Phase 13b: the legend is built as
    # RICH-TEXT runs (medium = "excel") so each break-word is coloured (its palette hex + bold) while
    # the rest stays plain black -- written as fmt_txt cells by the writer. Its plain text (derived from
    # the runs so it matches byte-for-byte) is merged into `subtext` for the geometry / styling; the
    # legend occupies the first `length(legend_runs)` subtext rows, overwritten with rich text below.
    # Phase 19h: the SOURCE is the render model's own `subtext` slot (already discarded of empties);
    # only the newline flattening below is xl's -- a workbook cell holds one line.
    subtext <- purrr::map(prep$tables, "subtext") |>
      purrr::map(~ stringi::stri_replace_all_regex(., "\\\n", " ") |> stringi::stri_replace_all_regex(" +", " "))
    # Phase 16e: the whole footer (weight -> Model: -> colour legend -> stars) as rich-text run lines via the
    # ONE shared builder -- replaces the hand-built plain-line head/tail sandwich around the colour legend.
    # They ride the SAME rich-text block (so the legend_row overwrite stays aligned); the user subtext stays
    # plain black on its own rows below (subtext = character(0) here, merged next).
    # Phase 17g: shared rd_footer(); xl passes the whole run set (no color_cols guard -- legend = the
    # color_legend arg) and no user subtext here (merged plain, below).
    # Phase 20h: `lang` IS threaded now. It was a documented formal of tab_xl() that the body never
    # read at all, so tab_xl(lang = "fr") wrote an English colour legend. Byte-identical when NULL
    # (= follow the ambient locale), which is every golden and every export-parity fixture.
    legend_runs <- purrr::map(tabs_src, function(t)
      rd_footer(t, "runs", theme = theme, want_legend = isTRUE(color_legend), lang = lang))
    if (any(purrr::map_lgl(legend_runs, ~ length(.) > 0L))) {
      legend_plain <- purrr::map(legend_runs, ~ purrr::map_chr(
        ., function(line) paste0(purrr::map_chr(line, "text"), collapse = "")))
      subtext <- purrr::map2(subtext, legend_plain, ~ c(.y, .x))
    }

    if (missing(titles)) {
      # Phase 14w (item 1): a regression table titles itself from its `reg_meta` (family + outcome +
      # predictors, or outcome + reference + effect for a comparison) -- this is the reg fix for the old
      # "levels by var" mis-title. Otherwise (Phase 14u): a NAMED tabxplor_tabs (a several-row_vars
      # output_list -> names = the row_vars) uses its element names, and a plain table the vars-derived
      # "X by Y" title.
      base_nm <- names(tabs_base)
      named_tabs <- inherits(tabs_base, "tabxplor_tabs") && length(base_nm) == length(tabs) &&
        all(nzchar(base_nm))
      # Phase 19h: through the SHARED rd_caption() (user caption -> set_caption() -> reg auto-title),
      # with xl's own two extra fallbacks passed as the closure. One caption rule, one place.
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
    # Phase 14w (item 1): a reg table's SHEET name is the compact "<short>_<dep>_<pred>" tag, not the
    # truncated prose title. Non-reg tables keep the title (truncated below).
    sheet_base <- purrr::map2_chr(tabs_src, titles, function(t, ti) {
      sn <- reg_sheet_name(reg_call(t)); if (!is.na(sn)) sn else ti
    })

    # Sheet-stacking offsets: within a sheet each stacked table starts below the previous one
    # (rows + subtext + 6 blank -- Phase 13c-iii: +1 for the col_var spanning-name header row).
    # Absolute geometry is derived from `start` in the plan builder.
    # the observed curves, in a small table of their own below the footer -- taken only where the
    # base-count cell cannot carry them (see tab_wants_shape_table). Its rows join the stacking
    # offset, or a second table on the same sheet would land on top of it.
    shapes <- purrr::map(tabs_src, function(t)
      if (is_tab(t) && tab_wants_shape_table(t, "xl")) reg_shape_table(t) else NULL)
    shape_n <- purrr::map_int(shapes, function(st)
    if (is.null(st)) 0L else nrow(st) + length(attr(st, "note")) + 2L)
    # ... and the model-check pictures, for the same reason: they are drawn BEFORE the geometry so
    # their height joins the stacking offset instead of landing on the next table.
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
    sheet_titles <- sheet_base[newsheet] |> stringi::stri_sub(1, 25) |> xl_clean_sheet_name()
    sheet_titles <- dplyr::if_else(duplicated(sheet_titles),
                                   stringi::stri_c(sheet_titles, ".2"), sheet_titles)
    nb <- 2
    while (length(unique(sheet_titles)) != length(sheet_titles)) {
      nb <- nb + 1
      sheet_titles <- dplyr::if_else(
        duplicated(sheet_titles),
        stringi::stri_c(stringi::stri_replace_first_regex(sheet_titles, "..$", ""), ".", nb), sheet_titles)
    }

    # Colour palettes built ONCE (Phase 5): TEXT channel -> font colour (the text palette),
    # BACKGROUND channel -> cell fill (bg palette). 11 hex per palette, indexed by slot integer.
    # Phase 10j: the palettes honour `theme` (default "light" == the old hardcoded value).
    opts <- list(
      font_num          = font_num,
      font_num_stars    = font_num_stars,
      font_text         = font_text,
      text_size         = text_size,
      colnames_rotation = colnames_rotation,
      text_size_headers = text_size_headers,
      text_size_subtext = text_size_subtext,
      # Phase 17g: no private palette -- slot->hex is single-sourced through ann (fmt_channel_codes),
      # the same source the CSS side reads. tab_xl_plan_one() consumes ann$text_hex / ann$bg_hex.
      ratio_cells       = ratio_cells,           # what a multiplicative cell holds: fold/raw/text
      # the RESOLVED palette: format() needs it to write a publication palette's marks into the cell
      # (and into the numFmt literal), exactly as it writes the significance stars.
      theme             = theme
    )

    # === Per-table plans (pure: raw values + numFmt codes + colour slots + font plan + geometry) ===
    # tab_xl_plan_one() carries no workbook and is side-effect-free; the workbook is assembled serially
    # from the plans below. (Parallelising the plan build was measured NOT worth it -- the openxlsx2
    # WRITE dominates the time and is inherently serial; see dev/benchmarks/results_2.0.0/phase10h_*.)
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
    # freeze under the first table's own header block -- title, span row and unit row included. The
    # hard-coded row 3 froze two rows of a four-row header, so the level names scrolled away.
    first_plan <- plans[!duplicated(sheet)]
    purrr::walk2(unique(sheet), first_plan,
                 function(sh, pl) xlb_freeze(wb, sh, pl$data_row0 + 1L, pl$freeze_col))
    # ONE style registrar for the whole workbook -> globally-unique style names (Phase 11a: per-table
    # name reuse silently applied table 1's styles to every later table).
    reg <- xl_style_registrar(wb)
    purrr::walk(plans, ~ xl_write_table(wb, ., opts, reg))

    xl_finish(function(p) xlb_save(wb, p), path, replace, open)
    invisible(tabs_base)
  }


# Resolve the path ONCE, write through it, tell the user where the file went, open it if asked.
# WHY once: tab_xl_resolve_path() is not pure -- with `replace = FALSE` it auto-numbers PAST any
# existing file, so calling it a second time (as the two degrade paths did) returned Tab2.xlsx after
# writing Tab1.xlsx, and `open` opened a file that had never been written.
# WHY the message: the default path is a tempdir(), and the function returns `tabs` (so a pipe keeps
# flowing), so a user with `open = FALSE` had no way at all to find the file.
xl_finish <- function(write, path, replace, open) {
  path <- tab_xl_resolve_path(path, replace)
  write(path)
  cli::cli_inform(c("v" = "Excel file written to {.file {path}}"))
  # A failed *open* after a successful *write* is non-fatal: on a machine with no spreadsheet
  # application (e.g. WSL2), openxlsx2::xl_open() aborts with "No applications (detected) available."
  # Downgrade any open failure to a friendly info message rather than erroring out.
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
  if (stringi::stri_detect_regex(path, "\\\\|/")) {
    dir_path <- path |> stringi::stri_replace_first_regex("\\\\[^\\\\]+$|/[^/]+$", "")
    if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
  }
  if (!stringi::stri_detect_regex(path, "\\.xlsx$")) path <- stringi::stri_c(path, ".xlsx")
  # THE shared "replace" rule (auto-number past an existing file when replace = FALSE), single-sourced so
  # direct tab_xl() use and the jamovi exporter number identically. See R/jmvtab-export.R.
  export_number_path(path, replace)
}


# Pure per-table plan: raw values to write + numFmt codes (stars folded) + colour slots + absolute
# geometry. Side-effect-free (no workbook), so the workbook is assembled serially from the plans.
# Geometry (given `start`): title row = start; header row = start + 1; data rows = start + 2 ..
# start + 1 + nrow; subtext below. Column role indices come from the shared prep `roles`.
#' @keywords internal
# Phase 13c-v: build the per-cell VALUE tibble Excel writes. A text-mode fmt column (ci = "cell" / OR)
# becomes its format() display string (character); every other fmt column its raw get_num() number.
# Mixed column types in one tibble are fine (openxlsx2 writes each column by its R type).
#' @keywords internal
xl_materialize_data <- function(tab, fmt_cols, text_fmt_cols, transposed = FALSE, theme = NULL,
                                fold = TRUE) {
  for (ci in fmt_cols) {
    tab[[ci]] <- if (isTRUE(transposed)) {
      as.character(tab[[ci]])                       # Phase 14o: already a pre-formatted display string
    } else if (ci %in% text_fmt_cols) {
      format(tab[[ci]], special_formatting = TRUE, na = "", stars = TRUE, theme = theme)
    } else {
      # THE READING VALUE (fmt_excel_value): a multiplicative cell holds its signed fold, every other
      # cell its raw number.
      # NaN -> NA so an empty numeric cell (a summary-stat / p-value row where the test does not apply)
      # writes as a BLANK cell, not the Excel #VALUE!/#N/A error -- openxlsx2 renders NaN as an error even
      # when NA is blanked (the na arg only covers NA). See xlb_na_argname for the NA half.
      v <- fmt_excel_value(tab[[ci]], fold = fold); v[is.nan(v)] <- NA_real_; v
    }
  }
  tibble::as_tibble(tab)
}

# xl_fold_literals() -- A TEMPLATE'S OWN LITERALS BECOME NUMBER-FORMAT LITERALS. Where a display has
# exactly one token, everything around it is text Excel can print beside the number without costing
# the cell its numeric type: "(n={n})" -> \(#,##0\), "(sigma{sd})" -> \(\s#,##0.0\),
# "{pvalue} (Chi2)" -> 0.0%\ \(Chi2\). That is what lets an aside column (mat_aside_cols) look like
# the aside it replaced, and it replaces the two hand-written arms (a test label, a sigma prefix)
# that used to do this one template at a time.
# Runs AFTER the significance stars, which belong to the number, not outside its brackets.
# WARNING: per SECTION -- a two-section code would otherwise take the literal on its last one only.
#' @keywords internal
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

# xl_check_images() -- the model-check plots, rendered to PNG so a workbook can carry them under the
# model they belong to. `reg_check_plots()` refits from the recipe the table stores (meta$spec$call)
# and recovers the data frame from the NAME the call was written with, so an ordinary
# `tab_reg(gss, ...) |> tab_export("xl", check = "auto")` needs no `data`; pass one only where the
# call was piped with %>% or subsetted.
# ⚠ reg_check_plots() DRAWS on the current device as a side effect and returns its gtables
# invisibly, so the first pass runs into a null device and each gtable is then drawn into its own
# PNG. The grid's own layout gives the panel count, which is what sizes the image: big enough for the
# axis text, no bigger.
# Returns one entry per input table: NULL where there is nothing to draw.
#' @keywords internal
xl_check_images <- function(tabs, check, data, theme = NULL, lang = NULL, dpi = 150) {
  none <- vector("list", length(tabs))
  if (is.null(check) || isFALSE(check)) return(none)
  if (!all(vapply(c("ggplot2", "gridExtra", "grid"), requireNamespace, logical(1), quietly = TRUE))) {
    cli::cli_inform(c("!" = paste("{.pkg ggplot2} / {.pkg gridExtra} are needed for",
                                  "{.arg check}; the workbook is written without the plots.")))
    return(none)
  }
  purrr::map(tabs, function(t) {
    if (!is.data.frame(t) || !tab_is_reg(t)) return(NULL)
    grids <- tryCatch({
      grDevices::pdf(NULL)
      on.exit(grDevices::dev.off(), add = TRUE)
      g <- reg_check_plots(t, data = data, check = check, theme = theme, lang = lang)
      if (inherits(g, "gtable")) list(g) else g
    }, error = function(e) { cli::cli_inform(c("!" = "{.arg check}: {conditionMessage(e)}")); NULL })
    if (!length(grids)) return(NULL)
    labs <- names(grids) %||% rep("", length(grids))
    imgs <- purrr::imap(grids, function(gt, i) {
      # the PANEL grid, read off the arrangement: `top` (the model's title) occupies the first
      # layout row and spans every column, so it is one row of the layout and none of the panels.
      nc <- max(1L, suppressWarnings(max(gt$layout$r, na.rm = TRUE)))
      nr <- max(1L, length(unique(gt$layout$t)) - 1L)
      # LANDSCAPE, and generous on the width: a panel's axis labels and its subtitle are what get cut
      # first, and they cost width, not height. The device size IS the text budget -- a ggplot draws
      # at a fixed point size, so a wider device gives every label more room rather than shrinking it.
      w  <- min(13, 4.6 * nc); h <- min(9, 2.7 * nr + 0.4)
      f  <- tempfile(fileext = ".png")
      grDevices::png(f, width = w, height = h, units = "in", res = dpi)
      on.exit(grDevices::dev.off(), add = TRUE)
      grid::grid.newpage(); grid::grid.draw(gt)
      list(file = f, width = w, height = h, label = labs[[i]] %||% "")
    })
    imgs
  })
}

# how many sheet ROWS an image block occupies -- one label row per image plus its height at Excel's
# default 15-point row. The stacking offsets must know, or the next table would sit under a picture.
#' @keywords internal
xl_check_rows <- function(imgs)
  if (!length(imgs)) 0L else
    sum(vapply(imgs, function(im) as.integer(ceiling(im$height * 72 / 15)) + 2L, integer(1)))

# xl_vname_width() -- how wide the variable-NAME column must be. Excel's width unit is one character
# of the default font, so the name's own length is the measure: `nchar * 1.05 + 1.5` is a plain,
# deterministic estimate (no auto-fit, which is what makes it reliable), floored at the narrow 3.5 a
# rotated column wants and capped at XL_VNAME_MAX so one long name cannot eat the sheet -- past that
# the cell wraps, and a one-row block is unmerged so Excel fits its height by itself.
#' @keywords internal
XL_VNAME_MAX <- 13
#' @keywords internal
xl_vname_width <- function(tab, roles) {
  vc <- unname(roles$var_name_col)
  if (length(vc) != 1L || vc > ncol(tab)) return(3.5)
  run  <- roles$label_runs[[names(tab)[[vc]]]]
  vals <- as.character(tab[[vc]])
  # HORIZONTAL = a run of one row: label_merges skips those, so they are the cells that must fit
  horiz <- if (is.null(run)) rep(TRUE, length(vals)) else (run$show & run$span == 1L)
  hv <- vals[horiz & !is.na(vals) & nzchar(vals)]
  if (!length(hv)) return(3.5)
  max(3.5, min(XL_VNAME_MAX, max(nchar(hv)) * 1.05 + 1.5))
}

# xl_prose_span() -- HOW FAR A LINE OF PROSE IS MERGED: from column 1 up to roughly an A4 portrait
# text width. A title or a footer legend left in column A alone is a paragraph in one narrow cell, and
# an Excel -> Word paste then sizes that column to the paragraph, which is what blew the table's
# geometry apart. Merged and wrapped, the prose sizes nothing.
# Excel's width unit is one character of the default font, ~7 px plus 5 px of cell padding; A4
# portrait text width is ~17 cm = ~642 px at 96 dpi. The widths are the ones the writer sets below --
# one definition would be better still, but they are set on the workbook, not computed into the plan.
#' @keywords internal
XL_A4_PX <- 642
#' @keywords internal
xl_prose_span <- function(colwidth, roles, ncl) {
  w <- rep(if (identical(colwidth, "auto")) 10 else as.double(colwidth), ncl)
  if (length(roles$row_var_col))  w[roles$row_var_col]  <- 30
  if (length(roles$var_name_col)) w[roles$var_name_col] <- 3.5
  px <- cumsum(w * 7 + 5)
  max(1L, min(ncl, sum(px <= XL_A4_PX) + 1L))
}

# ... and the height that wrapped prose needs. ⚠ Excel does NOT auto-fit the height of a MERGED cell,
# so a wrapped legend would be clipped to one line without this. ~5 px per character at the subtext
# size, one line per 11.5 points.
#' @keywords internal
xl_prose_height <- function(text, span_px, size = 9) {
  per_line <- max(20L, floor(span_px / (size * 0.55)))
  lines    <- pmax(1L, ceiling(nchar(text) / per_line))
  lines * (size * 1.28) + 2
}

# The shape table as (row, col, text) cells: a header row, one row per curve, then the note -- the
# same four columns every other medium prints, in the order reg_shape_table() declares.
#' @keywords internal
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
  # Phase 13c-iii: a col_var spanning-NAME header row sits above the level-name header (whenever the
  # table has a col_var), shifting the header + data + everything below down by one row. `data_row0`,
  # `header_row`, `last_row` derive every absolute position, so downstream offsets follow automatically.
  cvh        <- col_var_header
  has_span   <- !is.null(cvh) && any(nzchar(cvh$label))
  span_off   <- if (has_span) 1L else 0L
  span_row   <- start + 1L                       # the spanning-name row (used only if has_span)
  header_row <- start + 1L + span_off
  # Phase 22c-ii: the UNIT row, directly UNDER the level header -- what each column holds. Excel gives
  # every aside a column of its own (mat_aside_cols), so this row says the reading direction and the
  # statistic ("row%", "mean (sd)"), which is also what a numeric col_var's now-blank level header no
  # longer says. Sits INSIDE the header block, so the header's bottom rule moves down to it.
  has_unit   <- !is.null(cvh) && !is.null(cvh$unit) && any(nzchar(cvh$unit))
  unit_off   <- if (has_unit) 1L else 0L
  unit_row   <- header_row + 1L                  # used only if has_unit
  data_row0  <- header_row + unit_off            # data row i -> i + data_row0
  data_rows  <- seq_len(n) + data_row0
  last_row   <- data_row0 + n

  fmt_cols    <- roles$fmt_cols
  txt_cols    <- roles$other_cols
  # Phase 14m-ii (rework): monospace numbers (Cascadia Mono) only for a table that SHOWS stars, else the
  # proportional font. Per-table, because a list export can mix starred (reg) and plain (crosstab) sheets.
  font_num    <- tx_num_font("xl", roles$has_stars, plain = o$font_num, stars = o$font_num_stars)
  row_var_col <- roles$row_var_col
  totcols     <- roles$totcols
  # Phase 14o: a transposed table's `tab` is plain character (no fmt columns), so the fmt accessors that
  # re-derive roles from the tab (is_refcol / get_col_var) read from `roles` instead. Its reference is a
  # ROW (the Total), carried by bold_rows, not a column.
  ref_cols    <- if (isTRUE(transposed)) integer(0) else which(is_refcol(tab))

  cv_names      <- if (isTRUE(transposed)) unname(roles$col_var_map) else get_col_var(tab)
  # the LEFT edge of each column BLOCK (tab_col_block_ids): Excel draws one rule there and the table's
  # own right edge closes the last one, so no rule can fall INSIDE a block -- which is what used to
  # box a Total column away from the count carved out of it.
  block_start   <- tab_block_starts(roles$col_blocks %||% integer(0))

  # Phase 14i: the label columns' runs, lifted to ABSOLUTE sheet rows. `label_merges` is one merge per
  # run (skipping length-1 runs -- Excel rejects a 1-cell "merge", and a rotated 1-row cell would only
  # force a tall row); `vname_runs` are the name column's, the only ones that also get the rotation.
  label_merges <- purrr::imap(roles$label_runs, function(run, cl) {
    at <- which(run$show & run$span > 1L)
    tibble::tibble(col = match(cl, names(tab)),
                   row1 = at + data_row0, row2 = at + run$span[at] - 1L + data_row0)
  })
  label_merges <- if (length(label_merges)) dplyr::bind_rows(label_merges)
                  else tibble::tibble(col = integer(), row1 = integer(), row2 = integer())
  vname_runs   <- label_merges[label_merges$col %in% roles$var_name_col, , drop = FALSE]

  # A MULTIPLICATIVE CELL KEEPS A REAL NUMBER. It holds the READING VALUE (fmt_excel_value: the
  # signed fold) and prints through a two-section code, so "1/2.11" and "\u00f71.20" reach the
  # workbook without costing the cell its numeric type -- what `ratio_cells = "text"` used to be the
  # only way to get. `"text"` survives for a reader who wants the exact console string and no
  # arithmetic. The cells are named by the SHARED plan (fmt_mult_plan), never by matching a raw
  # token: a regression cell displays `{est}`, which IS the odds ratio on an odds-ratio column.
  xl_code   <- function(col) {
    code <- format(col, syntax = "excel")
    if (identical(o$ratio_cells, "text")) code[fmt_mult_plan(col)$cells] <- "TEXT"
    code
  }
  # TEXT IS A PROPERTY OF A CELL, NOT OF A COLUMN. A `{ci}` bracket and a genuine min-max `{n_range}`
  # cannot be a number, but the cells beside them can -- and turning the whole column to text took
  # every model-fit statistic in it with them (an "AIC 17 129" written as a string, which Excel then
  # flags as a number stored as text, and which carries a "." decimal into a locale that reads ","). A
  # numeric column is written with a hole at each text cell, and those few cells are then written
  # individually, exactly as a row sparkline already is.
  # Phase 14o: a transposed column is heterogeneous character throughout (pre-formatted display
  # strings, editable numbers deferred -- see tx_transpose_render()), so there the whole column is
  # text. The colours still ride `ann`.
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

  # Phase 14i: Excel keeps only a merged range's top-left value, so the label repeats below one become
  # invisible ghosts a user would find again on unmerging. Blank them at the source -- the display
  # equivalent of md's blanked cells, and on the WRITTEN copy only (every role is read off `tab`).
  xl_data <- xl_materialize_data(tab, fmt_cols, text_fmt_cols, transposed = transposed,
                                 theme = o$theme,
                                 fold = identical(o$ratio_cells, "fold"))
  # A row sparkline lives in a base-count cell that holds NO number, so it displaces nothing: the
  # column stays a real editable count and these few cells are written afterwards, individually, as
  # text. Only where the column is numeric -- a genuine min-max range already makes it a
  # `text_fmt_col`, whose format() string carries the glyphs on its own.
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

  # Number formats: format(syntax = "excel") is the single display source of truth. Fold significance
  # stars into the numFmt literal (0.0%\*\*\*), keeping the cell a real number; a "TEXT"-coded column
  # (ci / OR) is written as a string with Excel's "@" text format; NA codes stay General. Stars are
  # STORAGE-driven (get_stars() is "" when no pvalue was stored). When any cell is starred, pad EVERY
  # value cell's star literal to the column-max width so numbers stay aligned in the column.
  numfmt <- if (length(fmt_cols)) purrr::map_dfr(fmt_cols, function(ci) {
    col <- tab[[ci]]
    if (ci %in% text_fmt_cols) {                        # text-mode column -> "@" per written cell
      # Phase 14e: Excel renders in a proportional font, so the alignment padding must be figure
      # spaces (a digit wide), not ASCII half-digit spaces -- as in html. `html = TRUE` is NOT the
      # lever here: it would also switch on the html-only <sub> markup.
      # Phase 14o: a transposed column is already a pre-formatted display string (character).
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
      # Phase 14h: the same width and the same pad glyph as format()'s star field (fmt_class.R) --
      # an unstarred "" is width 0, so the max over every value cell IS the column-max star width.
      # formatC() padded with ASCII spaces, half a digit wide in the proportional font Excel renders.
      w      <- max(nchar(st[val]))
      st_pad <- stringi::stri_pad(st, w, side = "right", pad = fig_space) # glyphs left, pad right
      # WARNING: backslash-escape the star literal (xl_numfmt_literal), NEVER double-quote-wrap it -- a raw
      # " in a formatCode crashes the older jamovi-bundled openxlsx2 ("xml import unsuccessful").
      # ⚠ EVERY SECTION: a signed or multiplicative code has two, and a suffix on the whole string
      # would star the negative half alone.
      code[val] <- xl_numfmt_affix(code[val], suffix = st_pad[val])
    }
    # ONE RULE for everything a template writes around its number -- a test label ("{pvalue} (Chi2)"),
    # an aside's brackets and sigma ("(sigma{sd})"), an "n=" -- so Excel shows what the console shows
    # and the cell stays a real number. Replaces the two arms that did this one template at a time.
    code <- xl_fold_literals(code, get_display(col))
    code[!is.na(code) & code == "TEXT"] <- "@"
    tibble::tibble(col = as.integer(ci), row = seq_along(code) + data_row0, code = code)
  }) else tibble::tibble(col = integer(), row = integer(), code = character())
  numfmt <- dplyr::filter(numfmt, !is.na(.data$code))

  # Colour (two channels) comes from the shared prep `ann` (Phase 10j / 17g): text channel -> font
  # (bold + colour, folded into the font plan below); background channel -> cell fill (applied by the
  # writer). Phase 17g: consume ann's already theme-resolved HEX (`text_hex`/`bg_hex`, produced by
  # fmt_channel_codes -- the SAME source the CSS side reads) rather than re-index a private palette by
  # slot. The `slot > 0L` filter keeps exactly the coloured cells (slot 0 <=> hex NA); uncoloured
  # columns contribute all-zero slots / NA hex, filtered out.
  # Phase 22f-ii: the ink comes from the prep's RESOLVED per-cell colour (`ann$font` / `ann$back`),
  # the same three fields tab_kable and tab_plot consume -- not from the raw slots. The slot form
  # said nothing about an UNCOLOURED cell, so Excel drew every greyed non-significant cell in pure
  # black while html greyed it: `ann$font` already folds the whole rule (hex -> anchor black ->
  # grey / grey2), so there is nothing left here to get wrong.
  aside_col <- vapply(seq_len(ncl), function(j)
    is_fmt(tab[[j]]) && fmt_is_aside(tab[[j]]), logical(1))
  sec_hex   <- color_secondary_hex(o$theme)
  colour <- if (length(fmt_cols)) purrr::map_dfr(fmt_cols, function(ci) {
    a <- ann[[names(tab)[ci]]]
    if (is.null(a$font)) return(NULL)
    rows <- seq_along(a$font) + data_row0
    # AN ASIDE COLUMN IS AN ASIDE. mat_aside_cols() is Excel's paint_split(): the source column keeps
    # its primary, each secondary token becomes a column. So those columns wear the console's aside
    # ink and none of its emphasis -- including inside a Total or reference row, where `ann$font`
    # would otherwise blacken them (the console's `sec()` never yields to an anchor either).
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

  # Unified FONT plan: openxlsx2's wb_add_font(update=) is buggy over large ranges when the sheet has
  # scattered cells (title/subtext), so every font need is aggregated per cell into ONE complete
  # descriptor applied with update = FALSE (a full replace) -- cross-aspect merge keeps numFmt / fill /
  # border / alignment intact. Base name/size are filled by the writer. See R/tab-xl-backend.R.
  mk_src <- function(rows, cols, name = NA_character_, size = NA_real_, bold = FALSE,
                     color = NA_character_, italic = FALSE) {
    if (!length(rows) || !length(cols)) return(NULL)
    g <- tidyr::expand_grid(row = as.integer(rows), col = as.integer(cols))
    dplyr::mutate(g, name = name, size = size, bold = bold, italic = italic, underline = "",
                  color = color)
  }
  txt_colour <- colour
  # the reference bold rides the PRIMARY, exactly as it does in the console and in html: an aside
  # column carries the same number set back, never a second bold one.
  ref_cols     <- setdiff(ref_cols, which(aside_col))
  ref_row_cols <- setdiff(ref_row_cols, which(aside_col))
  fonts <- dplyr::bind_rows(
    mk_src(data_rows, fmt_cols, name = font_num),                                # numeric font
    mk_src(header_row, seq_len(ncl), bold = TRUE, size = o$text_size_headers),   # headers
    # THE UNIT ROW is the console's own type tag ("<row%>", "<n>") carried into the workbook: the
    # header's size, regular weight, ITALIC like a pillar tag, and in the chrome's `grey` -- set
    # further back than any cell, because it is a line of the header rather than a value.
    # ⚠ theme-aware: this used to hard-code the light grey, so a dark or a publication workbook
    # printed a light-theme ink.
    if (has_unit) mk_src(unit_row, seq_len(ncl), size = o$text_size_headers, italic = TRUE,
                         color = tx_chrome_hex(o$theme)$grey),
    # A VARIABLE NAME IS A HEADING, so the name column is bold throughout -- the rule html has always
    # applied (`tx-b` on a `tx-vname` cell) and Excel never did.
    mk_src(c(header_row, data_rows), roles$var_name_col, bold = TRUE),
    mk_src(c(header_row, data_rows), ref_cols, bold = TRUE),                     # reference cols
    mk_src(ref_rows, ref_row_cols, bold = TRUE),                                 # reference rows
    mk_src(start, 1L, bold = TRUE, size = 12),                                   # title
    mk_src(subtext_rows, 1L, size = o$text_size_subtext),                        # subtext
    # text-channel colour AND face (z11: `bold` was hard-wired TRUE here). A reference row that is also
    # an under-slot cell ends up bold+italic -- `any(bold)` doing its job, the structural bold winning
    # over the measure's non-bold; that union is the intended reading, not a special case.
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

  # Background-channel colour -> per-cell fill hex (ann$back, the html pill's own source).
  bg <- dplyr::filter(colour, !is.na(.data$fill))
  bg_fill <- tibble::tibble(row = bg$row, col = bg$col, fill = bg$fill)

  # Precompose the ENTIRE per-cell style (font + fill + border + alignment) into the fewest distinct
  # styles, each with its coalesced dims -- the openxlsx2 "shared styles, applied by id" fast path.
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
    # the shape table: a header row, its rows, then the note -- one blank line under the subtext
    # block. Plain cells, like the row sparklines already written into the count column.
    shape_cells = xl_shape_cells(shape, last_row + length(subtext_clean) + 2L),
    # the model-check pictures, under everything else the table wrote
    check_imgs = check_imgs,
    check_row = last_row + length(subtext_clean) + 2L +
      (if (is.null(shape)) 0L else nrow(shape) + 3L),
    # Phase 13b: the coloured legend runs occupy the FIRST rows of the subtext block (legend merged
    # first, above), overwritten with rich text by the writer.
    legend_runs = legend_runs, legend_row = last_row + 1L,
    # Phase 13c-v: fmt cell values -- a text-mode column (ci = "cell" / OR) is written as its format()
    # display STRING (the exact console text, "@"-formatted above); every other column writes the raw
    # get_num() number and lets Excel's numFmt code format it. Built per column so the tibble carries a
    # mix of character (text-mode) and numeric columns. Phase 14i blanks the label columns' repeats
    # on it (above), so a merged range holds no ghost value under its top-left cell.
    data = xl_data,
    header_row = header_row, ncl = ncl, data_row0 = data_row0,
    # the index columns stay put beside the header: a table is read by its row labels
    freeze_col = max(1L, length(txt_cols)) + 1L,
    # THE PROSE SPAN -- how far a title / footer line is merged across. A footer legend written into
    # column A alone is what makes an Excel -> Word paste blow the column widths apart: Word sizes a
    # column to its widest cell, and the legend is a paragraph. Merged and wrapped, it sizes nothing.
    # Capped at roughly an A4 PORTRAIT text width rather than the table's own, so a very wide table
    # does not stretch the legend into one unreadable line.
    prose_cols = xl_prose_span(colwidth, roles, ncl),
    unit_row = if (has_unit) unit_row else NA_integer_,
    unit_names = if (has_unit) cvh$unit else NULL,
    # AN INDEX COLUMN HAS NO UNIT, so its header takes both header rows rather than floating above a
    # blank cell: one merged, bottom-aligned cell, which puts "levels" on the same line as the
    # "<row%>" beside it. html does the same with a rowspan.
    head_merges = if (has_unit) unname(txt_cols) else integer(0),
    # Phase 13c-iii: the level header shows the suffix-stripped labels; the writer overwrites the header
    # cells with them and (when has_span) writes the merged col_var spanning-name row above.
    clean_names = if (!is.null(cvh)) cvh$clean else names(tab),
    span_row = if (has_span) span_row else NA_integer_,
    header_runs = if (has_span) tab_header_runs(cvh$label, cvh$group) else NULL,
    fmt_cols = fmt_cols, row_var_col = row_var_col, colwidth = colwidth,
    # Phase 14l: the Excel-only "<var>_sd" siblings (roles$sd_cols, the ONE definition of the rule).
    # They hold "s2.1" under a header of "sd", so the standard numeric width is wasted on them.
    sd_cols = unname(roles$sd_cols),
    # Phase 14i: the label columns' runs at ABSOLUTE sheet rows -- the writer merges each one, so a
    # row/tab variable is named once per block instead of on every row. `vname_col` is the name column
    # (values ARE variable names): merged AND rotated 90 degrees, so a long name costs one narrow
    # column. A kept tab_var is merged but never rotated -- its values are levels the user reads.
    label_merges = label_merges, vname_col = unname(roles$var_name_col),
    # ... and the width that column needs. A MERGED run is rotated, so it costs one line of vertical
    # text whatever the name is; a ONE-ROW block is written horizontally and was cut off at that width
    # ("Constant" in a regression table). The width is COMPUTED from the horizontal names alone --
    # never auto-fitted, which openxlsx2 cannot do reliably -- and capped, the cells wrapping beyond
    # it. A table whose name column is all rotated keeps the narrow 3.5.
    vname_width = xl_vname_width(tab, roles),
    # the few base-count cells that hold a row sparkline instead of a count: written individually,
    # as text, after the numeric column (see xl_materialize_data above).
    spark_cells = spark_cells,
    # ... and the cells no number can hold -- a `{ci}` bracket, a genuine min-max `{n_range}` --
    # written the same way, so the numbers beside them in the same column stay numbers.
    text_cells = text_cells,
    styles = styles, numfmt = numfmt
  )
}


# Build the per-cell full style grid (font + fill + border + alignment) for one table, grouped into
# the fewest DISTINCT styles, each with a coalesced multi-area dims. numFmt is NOT here (it is applied
# by the writer as a separate merging pass). Borders are painted onto 4 side matrices (0 none / 1 thin
# / 2 double), alignment onto zone matrices (base -> header -> total cols -> total rows, last wins).
#' @keywords internal
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
  # the header block's bottom rule closes the UNIT row when there is one: no line separates a column's
  # name from what it holds (the maintainer's rule -- the unit reads as part of the header).
  head_bottom <- if (is.na(unit_row)) header_row else unit_row
  bb <- prow(bb, c(head_bottom, last_row, tot_rows_last), 1L)            # header/surround/bottomline/block bottom
  # ONE RULE PER BLOCK BOUNDARY, drawn on the block's FIRST column; the table's own last column
  # closes the last one. A Total column used to draw its own left AND right rule, which boxed it away
  # from the base count carved out of it -- one fmt column with a line through it.
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
  # the unit row: LEFT (the tag names its column, it does not label its numbers), never rotated
  ui <- idx(unit_row)
  if (length(ui)) { ah[ui, ] <- "left"; av[ui, ] <- "bottom"; aw[ui, ] <- FALSE; ar[ui, ] <- 0L }
  # numbers read RIGHT. Excel lands a numeric cell there by itself, but a TEXT-written one (a `{ci}`
  # bracket, an `{n_range}`) landed left, so one column read against the next.
  fcd <- ci(fmt_cols); if (length(fcd) && length(di)) ah[di, fcd] <- "right"
  # a LABEL column reads from the left, header included -- a variable name is a heading, not a number
  xcd <- ci(txt_cols); if (length(xcd)) ah[, xcd] <- "left"
  # the NAME column wraps: its width is capped (xl_vname_width), so a long name takes a second line
  # instead of widening the whole table
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
  # Phase 14i: the row-variable NAME column reads vertically (90 degrees), centred on the block it
  # merges over -- so a long name costs one narrow column instead of a wide one. `text_rotation` was
  # already a per-cell matrix in the style dedup key (only `colnames_rotation` drove it), so this is a
  # paint, and xl_coalesce() groups it for free. LAST, so it beats the total-row/total-col zones above
  # (the name column is a text column, and a block's total row would otherwise re-align it to "left").
  # Only the MERGED runs: a 1-row block stays horizontal (rotating it would just make the row tall).
  if (length(vname_col) > 0 && !is.null(vname_runs) && nrow(vname_runs) > 0) {
    vc <- ci(vname_col)
    for (k in seq_len(nrow(vname_runs))) {
      vi <- idx(vname_runs$row1[k]:vname_runs$row2[k])
      if (length(vi) && length(vc)) {
        # A variable name reads from the LEFT in both orientations (`horizontal`, which under a 90
        # degree rotation keeps the line against the column's left edge), and a ROTATED one CENTRES
        # on the block it spans -- it names that block, so it belongs at its middle. A horizontal
        # name keeps the data zone's own `top`, against the first row it names.
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
  # z11: the palette's face beyond weight. Constant FALSE under the colour palettes, so the style
  # partition, its ordering and hence the emitted font ids are unchanged there.
  cells$fital  <- !is.na(fm) & fonts$italic[fm]
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


# WORKBOOK-SCOPED style registrar: deduplicates fonts / fills / borders / composed cell-xfs by CONTENT
# across ALL tables and hands out GLOBALLY-UNIQUE style NAMES.
# WARNING (Phase 11a bug): openxlsx2's styles_mgr is workbook-global and its get_*_id(name) resolves a
#   name to the FIRST matching entry (match(name, df$name)). The old xl_apply_styles reset its uid()
#   counter + caches per table, so from the 2nd table on every "txf1"/"txl1"/"txb1"/"txx1" name
#   collided with table 1's -- and get_xf_id("txx<i>") silently returned TABLE 1's i-th xf, applying
#   table 1's styles to every later table (offset borders, wrong font sizes, dead colours). ONE
#   registrar per workbook keeps the names monotonic and shares style objects across tables.
#' @keywords internal
xl_style_registrar <- function(wb) {
  sm  <- wb$styles_mgr
  fc  <- new.env(parent = emptyenv()); lc <- new.env(parent = emptyenv())
  bc  <- new.env(parent = emptyenv()); xc <- new.env(parent = emptyenv())
  ctr <- 0L
  uid <- function() { ctr <<- ctr + 1L; ctr }
  # WARNING (Phase 14l): `scheme = ""` is NOT cosmetic -- it is the whole reason a number cell renders
  # in `font_num`. openxlsx2::create_font() defaults `scheme = "minor"` = "this IS the theme's body
  # font", and Excel then resolves the font from the THEME, ignoring our explicit `name`. Since
  # xlb_base_font(wb, font_text) writes the theme's minor font, every font we emitted -- all correctly
  # named "DejaVu Sans" in the XML -- was drawn in "DejaVu Sans Condensed". Proven by unzipping the
  # workbook: cellXfs -> fontId resolved to a font named DejaVu Sans on every numeric cell, while the
  # font box in Excel read "DejaVu Sans Condensed (Body)". Never let `scheme` back in.
  # WARNING: `scheme` is safely absent from the dedup key below ONLY because it is a constant. A
  # per-font scheme would need `key` to grow a field, or two different fonts would collide onto one id.
  # `italic`/`underline` carry a publication palette's typography (its under-cells are italic, its
  # upper rungs ruled). Constant FALSE / "" for the colour palettes, so the key partition and hence the
  # emitted font ids are unchanged there. `underline` is OOXML's own vocabulary ("single" / "double"),
  # so it is written verbatim -- and it is IN the dedup key, which every font aspect must be.
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


# Apply the precomposed cell styles by id (font + fill + border + alignment in ONE composed xf per
# distinct style, deduped WORKBOOK-WIDE through `reg`) over each style's coalesced dims. numFmt is
# applied separately by the writer (it merges cross-aspect, per cell).
#' @keywords internal
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


# Per-sheet writer: write the raw values, then apply the precomposed cell styles by id (font + fill +
# border + alignment in ONE set_cell_style per distinct style), then the numFmt merging pass and the
# column widths / row heights. `reg` is the workbook-scoped style registrar (Phase 11a) shared across
# every table, so style NAMES never collide across the workbook.
#' @keywords internal
xl_write_table <- function(wb, plan, o, reg) {
  s   <- plan$sheet
  hdr <- plan$header_row

  # values: raw numbers + header, title, subtext (styles applied below). With a UNIT row between the
  # header and the data, the block is written headerless one row lower -- the header cells are
  # overwritten from `clean_names` below in either case.
  if (is.na(plan$unit_row)) xlb_write_data(wb, s, plan$data, hdr, 1L)
  else                      xlb_write_data(wb, s, plan$data, plan$unit_row + 1L, 1L, col_names = FALSE)
  # ... then the row sparklines, one cell at a time: openxlsx2 types per CELL, so a text glyph run
  # drops into an otherwise numeric count column without turning the whole column into text.
  for (cells in list(plan$spark_cells, plan$text_cells))
    if (!is.null(cells) && nrow(cells))
      purrr::pwalk(cells, function(col, row, text) xlb_write_cell(wb, s, xl_cell(row, col), text))
  xlb_write_cell(wb, s, xl_cell(plan$title_row, 1L), plan$title)
  if (length(plan$subtext)) xlb_write_cell(wb, s, xl_cell(plan$subtext_row, 1L), plan$subtext)
  # EVERY LINE OF PROSE IS ONE MERGED, WRAPPED CELL as wide as `prose_cols` -- the title above the
  # table and each footer line below it. That is what keeps an Excel -> Word paste from sizing a
  # column to a paragraph. The title sits at the BOTTOM of its cell (against the table it names), a
  # footer line at the TOP (reading down from the table).
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
  # ... and the model-check pictures, each under a plain label naming the model it checks
  r <- plan$check_row
  for (im in plan$check_imgs %||% list()) {
    if (nzchar(im$label)) xlb_write_cell(wb, s, xl_cell(r, 1L), im$label)
    xlb_add_image(wb, s, xl_cell(r + 1L, 1L), im$file, im$width, im$height)
    r <- r + ceiling(im$height * 72 / 15) + 2L
  }

  # Phase 13c-iii: overwrite the level-header cells with the suffix-stripped labels (the col_var name is
  # written in the spanning row above), then the merged col_var spanning-name row (a variable name over
  # its contiguous level columns; blank over the row var / total / count columns).
  for (j in seq_len(plan$ncl)) xlb_write_cell(wb, s, xl_cell(hdr, j), plan$clean_names[j])
  # ... and the unit row below it: what each column HOLDS, written once per BLOCK (tab_col_units()).
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
        # Phase 19n: a span belonging to a SUB-POPULATION (a spread level, a split-model group) puts
        # it on its own line above the variable -- an in-cell newline, with wrap_text set on the span
        # row below. The two facts arrive stored and apart (`col_group` / `col_var`); before 19n they
        # arrived welded as "White<br>married" and this line had to gsub an html tag out of a name.
        xlb_write_cell(wb, s, xl_cell(plan$span_row, c1),
                       if (nzchar(runs$groups[k])) paste0(runs$groups[k], "\n", runs$labels[k])
                       else runs$labels[k])
        if (c2 > c1)
          xlb_merge(wb, s, paste0(xl_cell(plan$span_row, c1), ":", xl_cell(plan$span_row, c2)))
      }
      col0 <- c2 + 1L
    }
  }

  # Phase 14i: merge each LABEL run vertically, so a row/tab variable is named once per block. The
  # name column's cells are also rotated 90 degrees (painted in xl_build_styles), which is what makes
  # a long name cost one narrow column. Merged BEFORE the styles: openxlsx2 keeps the range's top-left
  # value, and set_cell_style() over a merged range still reaches every cell of it.
  if (nrow(plan$label_merges)) {
    lm <- plan$label_merges
    for (k in seq_len(nrow(lm))) {
      xlb_merge(wb, s, paste0(xl_cell(lm$row1[k], lm$col[k]), ":", xl_cell(lm$row2[k], lm$col[k])))
    }
  }

  # --- styles: one composed xf (font + fill + border + alignment) per distinct cell style ---
  xl_apply_styles(wb, s, plan$styles, reg)

  # Phase 13c-iii: style the col_var spanning-name row (bold + centred, like the level header).
  # Phase 19n: wrap_text when any span carries a sub-population line, so the two lines show.
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

  # --- Phase 13b: overwrite the legend rows (first of the subtext block) with coloured rich text ---
  if (length(plan$legend_runs)) {
    for (i in seq_along(plan$legend_runs)) {
      runs <- plan$legend_runs[[i]]
      if (length(runs))
        xlb_write_richtext(wb, s, xl_cell(plan$legend_row + i - 1L, 1L), runs,
                           size = o$text_size_subtext, font = o$font_text)
    }
  }

  # --- column widths / row heights ---
  if (length(plan$row_var_col)) xlb_col_widths(wb, s, plan$row_var_col, 30)
  # Phase 14i: the rotated name column is one line of vertical text wide -- the whole point of turning
  # it. Left at the sheet default it would waste the width the rotation was meant to save.
  if (length(plan$vname_col)) xlb_col_widths(wb, s, plan$vname_col, plan$vname_width %||% 3.5)
  rot <- o$colnames_rotation
  if (length(plan$fmt_cols)) {
    if (identical(plan$colwidth, "auto")) {
      w <- if (rot > 30 && rot < 60) 8
      else if (rot >= 60) 6 + 8 * cos(rot / 90 * pi / 2)
      else "auto"
      xlb_col_widths(wb, s, plan$fmt_cols, w)   # "auto" already sizes an sd column to its content
    } else {
      # Phase 14l: an sd sibling holds "s2.1" under a header of "sd" -- it never needs the width its
      # mean does. Scaled rather than fixed, so a user who widens `colwidth` for long numbers widens
      # the sd column too; floored so a wide sigma value still fits.
      cw     <- as.double(plan$colwidth)
      sd_cls <- intersect(plan$fmt_cols, plan$sd_cols)
      xlb_col_widths(wb, s, setdiff(plan$fmt_cols, sd_cls), cw)
      if (length(sd_cls)) xlb_col_widths(wb, s, sd_cls, max(5, cw * 0.6))
    }
  }
  if (rot > 0) xlb_row_heights(wb, s, plan$header_row, 13.8 + 105 * sin(rot / 90 * pi / 2))

  invisible(wb)
}


# Which axis holds the DEPENDENT variable(s)? Under pct="row" a row is a GROUP and the column
# distribution is what is being described ("race by marital" = the distribution of race, by marital
# status); under pct="col" the two axes swap. `pct` is not an argument here, not in `vars`, and not in
# the `vars` attribute -- its only surviving trace on a built table is the fmt columns' `pct_type`.
# DESIGN (Phase 14l): only an all-"col" table flips. A mean and a regression coefficient have no
# percentage base at all (`none`), so neither is directional and neither may vote;
# a genuinely mixed row+col table falls back to the outcome-first default rather than guessing.
#' @keywords internal
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

#' @keywords internal
tab_get_titles <- function(tabs, row, col, tab, max = 2) {
  # Phase 14d: was a case_when over `length(row) == 1` with NO fallback, fed the DETECTED roles. On a
  # merged table (several row_vars) those roles were the merge's own scaffolding, so the title read
  # "levels by multi (tabbed by row_var)" -- three words, none of them a variable of the user's -- and
  # any shape the branches missed fell through to a literal "NA". The roles are recorded now, so the
  # real names are available; name them all, eliding past `max` with a count.
  # Phase 14l: the DEPENDENT variable comes first ("ROCK, JAZZ by DIPLOM" reads as the thing described,
  # then the thing it is broken down by), which under pct="row" is the col_vars -- so the old fixed
  # "<rows> by <cols>" was backwards on the common table. `tabs` was already accepted and unused; it is
  # what tab_title_rows_first() needs, so nothing new is threaded in.
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


