# PURPOSE: Export tabxplor tables to Excel with formatting and colors via openxlsx2.
# ROLE: Primary export format for sharing tables with non-R users. Phase 10h: single-tab-first with a
#       list method; consumes the shared exporter prep (R/tab-export-prep.R) for role detection /
#       references / bold rows AND the two-channel colour slots (`ann`, Phase 10j -- the private
#       fmt_color_channels() pass is gone), and the openxlsx2 backend (R/tab-xl-backend.R).
#       tab_xl_plan_one() does the pure per-table CPU (raw values +
#       numFmt codes + a precomposed per-cell STYLE grid via xl_build_styles); xl_write_table() writes
#       the values, applies the styles by id (xl_apply_styles), then the numFmt merging pass.
# KEY CONSTRAINTS:
#   - openxlsx2 is Suggests-only -- the ONE requireNamespace() guard is in tab_xl(); every engine call
#     goes through the unguarded xlb_* wrappers or xl_apply_styles' create_*/set_cell_style compose.
#   - Export-Parity: tab_xl writes the RAW get_num() value; Excel formats it via the per-cell codes
#     from format(x, syntax = "excel") (fmt_class.R excel_numfmt_code) -- the single display source of
#     truth. Significance stars are folded into the numFmt code (0.0%\*\*\*), gated by the SAME option
#     as the text path (getOption("tabxplor.stars")), so the cell stays a real number. numFmt literals
#     (stars / label / sigma / multiply) are backslash-escaped via xl_numfmt_literal() -- NEVER double-
#     quote-wrapped, which crashes the older jamovi-bundled openxlsx2 ("xml import unsuccessful").
#   - Shared-style fast path: each cell's FULL style (font+fill+border+alignment) is precomposed and
#     applied ONCE by id (set_cell_style) over the fewest coalesced multi-area dims (xl_coalesce) --
#     far fewer + cheaper openxlsx2 calls than a wb_add_* per aspect. numFmt merges on afterwards.
#   - ONE workbook-scoped style registrar (xl_style_registrar) dedups styles across ALL tables and
#     keeps style NAMES globally unique: openxlsx2's styles_mgr is workbook-global and resolves a name
#     to its FIRST match, so per-table name reuse mis-applied table 1's styles to every later table
#     (Phase 11a fix).
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
#' @param or_numeric Odds ratios export as text ("1/x" reciprocal for OR < 1) by default so an OR
#'   below 1 reads symmetrically to an OR above 1. Set to \code{TRUE} (or the option
#'   \code{tabxplor.xl_or_numeric}) to keep them as real, editable numbers instead.
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
#' @param html_24_bit Kept for a uniform exporter signature; inert for Excel (always 24-bit).
#' @param color Set to \code{FALSE} to export without colours (monochrome).
#' @param color_legend Should the color legends be printed with the subtexts ?
#' @param lang Colour-legend language: \code{NULL} (auto from the R/OS locale, English fallback),
#'   \code{"en"} or \code{"fr"}.
#' @param print_color_legend `r lifecycle::badge("deprecated")` Renamed to \code{color_legend}.
#' @param var_names Which variable names to write beside the table: `"both"` (the default),
#'   `"rows"`, `"cols"` or `"none"`. The row-variable name is the leading column a table with
#'   several `row_vars` uses to name each block (merged over it and rotated 90 degrees); the
#'   column-variable names are the merged row above their level columns. See \code{\link{tab_kable}}.
#' @param sheets The Excel sheets options :
#' \itemize{
#'   \item \code{"tabs"}: a new sheet is created for each table
#'   \item \code{"unique"}: all tables are on the same sheet
#'   \item \code{"auto"}: subsequent tables with the same column vars are printed on the
#'    same sheets
#' }
#' @param color_type `r lifecycle::badge("deprecated")` Inert since 1.4.0: the text channel always uses
#' the text palette. The colour CHANNEL is chosen by `color = c(text, background)` (see \code{\link{tab}}).
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
#'     tab(marital, race, pct = "row", color = "diff") |>
#'     tab_xl()
#' }
#' }
tab_xl <-
  function(tabs, path = NULL, replace = FALSE, open = rlang::is_interactive(),
           lang = NULL,
           colnames_rotation = 0, remove_tab_vars = TRUE,
           colwidth = 10, color_legend = TRUE,
           sheets = "auto", titles, caption = NULL,
           font_text = getOption("tabxplor.xl_font_text", "DejaVu Sans Condensed"),
           font_num  = getOption("tabxplor.xl_font_num",  "DejaVu Sans"),
           font_num_stars = getOption("tabxplor.xl_font_num_stars", "Cascadia Mono"),
           text_size = 10, text_size_headers = 9, text_size_subtext = 9,
           theme = NULL,
           color_type = lifecycle::deprecated(), html_24_bit = NULL, color = TRUE,
           transpose = FALSE, var_names = NULL,
           or_numeric = getOption("tabxplor.xl_or_numeric", FALSE),
           print_color_legend = lifecycle::deprecated()) {

    # Phase 13a: install a per-table color_breaks override for the render (no-op otherwise).
    .cb <- push_color_breaks(tabs); on.exit(pop_color_breaks(.cb), add = TRUE)

    if (!requireNamespace("openxlsx2", quietly = TRUE)) {
      stop(paste0("Package \"openxlsx2\" needed for this function to work. ",
                  "You can install it with : install.packages('openxlsx2')"),
           call. = FALSE)
    }

    if (length(replace) == 0) replace <- length(path) != 0

    # Phase 17g: the long-inert `n_min` / `hide_near_zero` / `conditional_format` args were removed
    # before the 1.4.0 CRAN freeze (they never did anything: n_min moved to tab(n_min=), near-zero
    # greying was dropped, and Excel conditional formatting was never implemented). Passing them now
    # errors "unused argument" -- accepted per the Phase 17 §Settled decisions ruling.

    # Phase 10j: `print_color_legend` renamed to `color_legend` (unified across exporters).
    if (lifecycle::is_present(print_color_legend)) {
      lifecycle::deprecate_soft("1.4.0", "tab_xl(print_color_legend)", "tab_xl(color_legend)")
      color_legend <- print_color_legend
    }
    if (lifecycle::is_present(color_type)) lifecycle::deprecate_soft("1.4.0", "tab_xl(color_type)")
    # Shared option resolver (theme/color/color_legend/transpose). Phase 10j makes tab_xl theme-aware:
    # the palettes below now honour `theme` (was hardcoded "light"). `html_24_bit` is inert
    # (Phase 13a): Excel is always 24-bit.
    o <- resolve_export_opts(theme = theme, color = color, color_legend = color_legend,
                             transpose = transpose, caption = caption, var_names = var_names)
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
    # per dependent, or a several-row_vars output_list) -> one sheet each by default. The col-var "auto"
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
    subtext <- purrr::map(tabs_src, get_subtext) |>
      purrr::map(~ stringi::stri_replace_all_regex(., "\\\n", " ") |> stringi::stri_replace_all_regex(" +", " "))
    # Phase 16e: the whole footer (weight -> Model: -> colour legend -> stars) as rich-text run lines via the
    # ONE shared builder -- replaces the hand-built plain-line head/tail sandwich around the colour legend.
    # They ride the SAME rich-text block (so the legend_row overwrite stays aligned); the user subtext stays
    # plain black on its own rows below (subtext = character(0) here, merged next).
    # Phase 17g: shared rd_footer(); xl passes the whole run set (no color_cols guard -- legend = the
    # color_legend arg) and no user subtext here (merged plain, below).
    legend_runs <- purrr::map(tabs_src, function(t)
      rd_footer(t, "runs", theme = theme, want_legend = isTRUE(color_legend)))
    if (any(purrr::map_lgl(legend_runs, ~ length(.) > 0L))) {
      legend_plain <- purrr::map(legend_runs, ~ purrr::map_chr(
        ., function(line) paste0(purrr::map_chr(line, "text"), collapse = "")))
      subtext <- purrr::map2(subtext, legend_plain, ~ c(.y, .x))
    }

    if (missing(titles)) {
      # Phase 14w (item 1): a regression table titles itself from its `reg_meta` (family + dependent +
      # predictors, or dependent + reference + effect for a comparison) -- this is the reg fix for the old
      # "levels by var" mis-title. Otherwise (Phase 14u): a NAMED tabxplor_tabs (a several-row_vars
      # output_list -> names = the row_vars) uses its element names, and a plain table the vars-derived
      # "X by Y" title.
      base_nm <- names(tabs_base)
      named_tabs <- inherits(tabs_base, "tabxplor_tabs") && length(base_nm) == length(tabs) &&
        all(nzchar(base_nm))
      titles <- purrr::pmap_chr(
        list(tabs_src, row_vars, col_vars_plain, tab_vars, seq_along(tabs)),
        function(t, rv, cv, tv, i) {
          # Phase 17b: a stored caption (set_caption()) wins over the reg auto-title, then named/auto.
          cap <- get_caption(t)
          rt  <- reg_title(get_reg_meta(t))
          if (!is.null(cap)) cap
          else if (!is.na(rt)) rt
          else if (named_tabs) base_nm[[i]]
          else tab_get_titles(t, rv, cv, tv)
        })
    } else {
      titles <- vctrs::vec_recycle(titles, length(tabs))
    }
    # Phase 14w (item 1): a reg table's SHEET name is the compact "<short>_<dep>_<pred>" tag, not the
    # truncated prose title. Non-reg tables keep the title (truncated below).
    sheet_base <- purrr::map2_chr(tabs_src, titles, function(t, ti) {
      sn <- reg_sheet_name(get_reg_meta(t)); if (!is.na(sn)) sn else ti
    })

    # Sheet-stacking offsets: within a sheet each stacked table starts below the previous one
    # (rows + subtext + 6 blank -- Phase 13c-iii: +1 for the col_var spanning-name header row).
    # Absolute geometry is derived from `start` in the plan builder.
    newsheet <- sheet != dplyr::lag(sheet, default = -1L)
    start <- tibble::tibble(newsheet, rows = purrr::map_int(tabs, nrow),
                            sub = purrr::map_int(subtext, length)) |>
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
      or_numeric        = isTRUE(or_numeric)      # Phase 13c-v: OR as text (1/x) by default
    )

    # === Per-table plans (pure: raw values + numFmt codes + colour slots + font plan + geometry) ===
    # tab_xl_plan_one() carries no workbook and is side-effect-free; the workbook is assembled serially
    # from the plans below. (Parallelising the plan build was measured NOT worth it -- the openxlsx2
    # WRITE dominates the time and is inherently serial; see dev/benchmarks/results_1.4.0/phase10h_*.)
    plans <- purrr::pmap(
      list(tab = tabs, roles = roles, ann = purrr::map(rd, "ann"),
           bold_rows = purrr::map(rd, "bold_rows"),
           col_var_header = purrr::map(rd, "col_var_header"),
           start = start, sheet = sheet, title = titles, subtext = subtext,
           legend_runs = legend_runs, colwidth = colwidth, transposed = transposed),
      tab_xl_plan_one, o = opts
    )

    # === Assemble the workbook on the main process (serial) =======================================
    wb <- xlb_new_workbook()
    xlb_base_font(wb, font_text, text_size)
    purrr::walk(sheet_titles, ~ xlb_add_sheet(wb, .))
    purrr::walk(unique(sheet), ~ xlb_freeze(wb, ., 3L))
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
  if (isTRUE(open)) xlb_open(path)
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
  path_name <- stringi::stri_replace_first_regex(path, "\\.xlsx$", "")
  if (!stringi::stri_detect_regex(path, "\\.xlsx$")) path <- stringi::stri_c(path, ".xlsx")
  if (isFALSE(replace)) {
    i <- 0
    while (file.exists(path)) {
      i <- i + 1
      path <- stringi::stri_c(path_name, i, ".xlsx")
    }
  }
  path
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
xl_materialize_data <- function(tab, fmt_cols, text_fmt_cols, transposed = FALSE) {
  for (ci in fmt_cols) {
    tab[[ci]] <- if (isTRUE(transposed)) {
      as.character(tab[[ci]])                       # Phase 14o: already a pre-formatted display string
    } else if (ci %in% text_fmt_cols) {
      format(tab[[ci]], special_formatting = TRUE, na = "", stars = TRUE)
    } else {
      get_num(tab[[ci]])
    }
  }
  tibble::as_tibble(tab)
}

tab_xl_plan_one <- function(tab, roles, ann, bold_rows, col_var_header, start, sheet, title, subtext,
                            legend_runs = list(), colwidth, o, transposed = FALSE) {
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
  data_row0  <- header_row                       # data row i -> i + data_row0
  data_rows  <- seq_len(n) + data_row0
  last_row   <- data_row0 + n

  fmt_cols    <- roles$fmt_cols
  txt_cols    <- roles$other_cols
  # Phase 14m-ii (rework): monospace numbers (Cascadia Mono) only for a table that SHOWS stars, else the
  # proportional font. Per-table, because a list export can mix starred (reg) and plain (crosstab) sheets.
  font_num    <- if (isTRUE(roles$has_stars)) o$font_num_stars else o$font_num
  row_var_col <- roles$row_var_col
  totcols     <- roles$totcols
  # Phase 14o: a transposed table's `tab` is plain character (no fmt columns), so the fmt accessors that
  # re-derive roles from the tab (is_refcol / get_col_var) read from `roles` instead. Its reference is a
  # ROW (the Total), carried by bold_rows, not a column.
  ref_cols    <- if (isTRUE(transposed)) integer(0) else which(is_refcol(tab))

  cv_names      <- if (isTRUE(transposed)) unname(roles$col_var_map) else get_col_var(tab)
  start_col_var <- which(cv_names != "" & cv_names != dplyr::lag(cv_names, default = NA_character_))

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

  # Phase 13c-v: OR cells export as TEXT (the "1/x" reciprocal string) by DEFAULT so an OR < 1 reads
  # symmetrically to an OR > 1 -- there is no point keeping the < 1 side numeric while the > 1 side is
  # too; opt in to real numbers with tab_xl(or_numeric = TRUE). A column carrying any TEXT-coded cell
  # (ci = "cell" brackets, or OR) is written as the format() display STRING (special_formatting = TRUE,
  # so the 1/x + stars appear) under Excel's "@" text format -- it keeps the exact console display at
  # the cost of a raw editable number (the accepted trade-off; pct/diff/mean/n stay real numbers).
  or_family <- c("or", "OR", "or_pct", "OR_pct", "est_ci")
  xl_code   <- function(col) {
    code <- format(col, syntax = "excel")
    if (!isTRUE(o$or_numeric)) code[get_display(col) %in% or_family] <- "TEXT"
    code
  }
  # Phase 14o: a transposed column is heterogeneous character (pre-formatted display strings, editable
  # numbers deferred -- see tx_transpose_render()), so every fmt column is written as TEXT ("@"). The
  # colours still ride the slot grid from `ann`.
  text_fmt_cols <- if (isTRUE(transposed)) fmt_cols else fmt_cols[vapply(
    fmt_cols, function(ci) { cd <- xl_code(tab[[ci]]); any(!is.na(cd) & cd == "TEXT") }, logical(1))]

  # Phase 14i: Excel keeps only a merged range's top-left value, so the label repeats below one become
  # invisible ghosts a user would find again on unmerging. Blank them at the source -- the display
  # equivalent of md's blanked cells, and on the WRITTEN copy only (every role is read off `tab`).
  xl_data <- xl_materialize_data(tab, fmt_cols, text_fmt_cols, transposed = transposed)
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
              else format(col, special_formatting = TRUE, na = "", stars = TRUE, pad = fig_space)
      code <- ifelse(!is.na(val) & nzchar(val), "@", NA_character_)
      return(tibble::tibble(col = as.integer(ci), row = seq_along(code) + data_row0, code = code))
    }
    code <- xl_code(col)
    st   <- get_stars(col)
    val  <- !is.na(code) & code != "TEXT"
    if (any(val & nzchar(st))) {
      # Phase 14h: the same width and the same pad glyph as format()'s star field (fmt_class.R) --
      # an unstarred "" is width 0, so the max over every value cell IS the column-max star width.
      # formatC() padded with ASCII spaces, half a digit wide in the proportional font Excel renders.
      w      <- max(nchar(st[val]))
      st_pad <- stringi::stri_pad(st, w, side = "right", pad = fig_space) # glyphs left, pad right
      # WARNING: backslash-escape the star literal (xl_numfmt_literal), NEVER double-quote-wrap it -- a raw
      # " in a formatCode crashes the older jamovi-bundled openxlsx2 ("xml import unsuccessful").
      code[val] <- paste0(code[val], xl_numfmt_literal(st_pad[val]))
    }
    # Phase 12h: fold an in-cell TEST LABEL ("{pvalue} (Chi2)") into the numFmt literal so Excel shows
    # "2.9% (Chi2)" (crosstab chi2/F p-value rows + reg-footer p-value rows), instead of the bare number
    # (the label was previously dropped: format(syntax="excel") resolves the composite to its pvalue
    # PRIMARY before the text expansion). Only the pvalue-composite has a pure-literal suffix; other
    # composites ({pct} (n={n})) keep the Excel primary (their annotation lives in a separate column).
    disp <- get_display(col)
    lbl  <- sub("^\\{\\s*pvalue\\s*\\}(.*)$", "\\1", disp)
    has_lbl <- !is.na(disp) & disp != lbl & !grepl("{", lbl, fixed = TRUE) & nzchar(trimws(lbl))
    if (any(has_lbl & val)) {
      m <- has_lbl & val
      code[m] <- paste0(code[m], xl_numfmt_literal(lbl[m]))   # backslash-escaped, not quote-wrapped
    }
    # Phase 13c-v: the mean's sd twin column (display "var") gets a leading sigma so Excel reads "s2.5".
    vmask <- disp == "var" & val
    if (any(vmask)) code[vmask] <- paste0(xl_numfmt_literal(sigma_sign), code[vmask])
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
  colour <- if (length(fmt_cols)) purrr::map_dfr(fmt_cols, function(ci) {
    a <- ann[[names(tab)[ci]]]
    if (is.null(a$text_slot)) return(NULL)
    rows <- seq_along(a$text_slot) + data_row0
    dplyr::bind_rows(
      tibble::tibble(col = as.integer(ci), row = rows, slot = a$text_slot, hex = a$text_hex, channel = "text"),
      tibble::tibble(col = as.integer(ci), row = rows, slot = a$bg_slot,   hex = a$bg_hex,   channel = "bg"))
  }) else tibble::tibble(col = integer(), row = integer(), slot = integer(), hex = character(), channel = character())
  colour <- dplyr::filter(colour, .data$slot > 0L)

  subtext_clean <- subtext[!is.na(subtext) & subtext != ""]
  subtext_rows  <- if (length(subtext_clean)) seq_along(subtext_clean) + last_row else integer()
  ref_rows      <- bold_rows + data_row0
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
    mk_src(data_rows, fmt_cols, name = font_num),                                # numeric font
    mk_src(header_row, seq_len(ncl), bold = TRUE, size = o$text_size_headers),   # headers
    mk_src(c(header_row, data_rows), ref_cols, bold = TRUE),                     # reference cols
    mk_src(ref_rows, ref_row_cols, bold = TRUE),                                 # reference rows
    mk_src(start, 1L, bold = TRUE, size = 12),                                   # title
    mk_src(subtext_rows, 1L, size = o$text_size_subtext),                        # subtext
    if (nrow(txt_colour)) tibble::tibble(row = txt_colour$row, col = txt_colour$col,
                                         name = NA_character_, size = NA_real_, bold = TRUE,
                                         color = txt_colour$hex)                 # text-channel colour
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

  # Background-channel colour -> per-cell fill hex (from ann, Phase 17g).
  bg <- dplyr::filter(colour, .data$channel == "bg")
  bg_fill <- if (nrow(bg)) tibble::tibble(row = bg$row, col = bg$col, fill = bg$hex)
             else tibble::tibble(row = integer(), col = integer(), fill = character())

  # Precompose the ENTIRE per-cell style (font + fill + border + alignment) into the fewest distinct
  # styles, each with its coalesced dims -- the openxlsx2 "shared styles, applied by id" fast path.
  styles <- xl_build_styles(
    header_row = header_row, data_rows = data_rows, last_row = last_row, ncl = ncl,
    fmt_cols = fmt_cols, txt_cols = txt_cols, totcols = totcols, start_col_var = start_col_var,
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
    # Phase 13b: the coloured legend runs occupy the FIRST rows of the subtext block (legend merged
    # first, above), overwritten with rich text by the writer.
    legend_runs = legend_runs, legend_row = last_row + 1L,
    # Phase 13c-v: fmt cell values -- a text-mode column (ci = "cell" / OR) is written as its format()
    # display STRING (the exact console text, "@"-formatted above); every other column writes the raw
    # get_num() number and lets Excel's numFmt code format it. Built per column so the tibble carries a
    # mix of character (text-mode) and numeric columns. Phase 14i blanks the label columns' repeats
    # on it (above), so a merged range holds no ghost value under its top-left cell.
    data = xl_data,
    header_row = header_row, ncl = ncl,
    # Phase 13c-iii: the level header shows the suffix-stripped labels; the writer overwrites the header
    # cells with them and (when has_span) writes the merged col_var spanning-name row above.
    clean_names = if (!is.null(cvh)) cvh$clean else names(tab),
    span_row = if (has_span) span_row else NA_integer_,
    header_runs = if (has_span) tab_header_runs(cvh$label) else NULL,
    fmt_cols = fmt_cols, row_var_col = row_var_col, colwidth = colwidth,
    # Phase 14l: the Excel-only "<var>_sd" siblings (roles$sd_cols, the ONE definition of the rule).
    # They hold "s2.1" under a header of "sd", so the standard numeric width is wasted on them.
    sd_cols = unname(roles$sd_cols),
    # Phase 14i: the label columns' runs at ABSOLUTE sheet rows -- the writer merges each one, so a
    # row/tab variable is named once per block instead of on every row. `vname_col` is the name column
    # (values ARE variable names): merged AND rotated 90 degrees, so a long name costs one narrow
    # column. A kept tab_var is merged but never rotated -- its values are levels the user reads.
    label_merges = label_merges, vname_col = unname(roles$var_name_col),
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
        ar[vi, vc] <- 90L; ah[vi, vc] <- "center"; av[vi, vc] <- "center"; aw[vi, vc] <- TRUE
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
  font_id <- function(name, size, bold, color) {
    key <- paste(name, size, bold, color, sep = "\r")
    if (is.null(fc[[key]])) {
      args <- list(name = name, sz = as.character(size), scheme = "")
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
  # composed cell xf: dedup on the full (font, fill, border, alignment) tuple.
  xf_id <- function(fname, fsize, fbold, fcolor, fill, bt, bb, bl, br, ah, av, aw, ar) {
    fid <- font_id(fname, fsize, fbold, fcolor)
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
      if (r$ar != 0L) as.character(r$ar) else "")
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

  # values: raw numbers + header, title, subtext (styles applied below)
  xlb_write_data(wb, s, plan$data, hdr, 1L)
  xlb_write_cell(wb, s, xl_cell(plan$title_row, 1L), plan$title)
  if (length(plan$subtext)) xlb_write_cell(wb, s, xl_cell(plan$subtext_row, 1L), plan$subtext)

  # Phase 13c-iii: overwrite the level-header cells with the suffix-stripped labels (the col_var name is
  # written in the spanning row above), then the merged col_var spanning-name row (a variable name over
  # its contiguous level columns; blank over the row var / total / count columns).
  for (j in seq_len(plan$ncl)) xlb_write_cell(wb, s, xl_cell(hdr, j), plan$clean_names[j])
  if (!is.na(plan$span_row)) {
    runs <- plan$header_runs
    col0 <- 1L
    for (k in seq_along(runs$labels)) {
      c1 <- col0; c2 <- col0 + runs$spans[k] - 1L
      if (nzchar(runs$labels[k])) {
        # Phase g: a split-model span carries a "<br>" (e.g. "White<br>married: Married") -> an in-cell
        # newline so Excel shows the split level over the outcome (wrap_text set on the span row below).
        xlb_write_cell(wb, s, xl_cell(plan$span_row, c1), gsub("<br>", "\n", runs$labels[k], fixed = TRUE))
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
  # Phase g: wrap_text when a span carries a "<br>" split-model line, so the two lines show.
  if (!is.na(plan$span_row)) {
    span_wrap <- if (any(grepl("<br>", plan$header_runs$labels, fixed = TRUE))) "1" else ""
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
  if (length(plan$vname_col)) xlb_col_widths(wb, s, plan$vname_col, 3.5)
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
# the `vars` attribute -- its only surviving trace on a built table is the fmt columns' `type`.
# DESIGN (Phase 14l): only an all-"col" table flips. A mean is always "Y by group" (type "mean"), and a
# regression coefficient is profile-free (type "coef"), so neither is directional and neither may vote;
# a genuinely mixed row+col table falls back to the dependent-first default rather than guessing.
#' @keywords internal
tab_title_rows_first <- function(tabs) {
  types <- purrr::map_chr(tabs, ~ if (is_fmt(.)) get_type(.) else NA_character_)
  dir   <- types[!is.na(types) & types %in% c("row", "col")]
  length(dir) > 0 && all(dir == "col")
}

# Name a variable set for a title: every name up to `max`, then "+N more" -- never "multi", which named
# nothing, and never a bare index. Placeholders and empties drop out.
tab_title_names <- function(x, max = 2) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x) & !x %in% c("no_row_var", "no_col_var", "all_col_vars")]
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
  a    <- if (swap) rows else cols     # the dependent axis, named first
  b    <- if (swap) cols else rows
  res  <- if (!nzchar(a) && !nzchar(b)) "Table"
          else if (!nzchar(a)) b
          else if (!nzchar(b)) a
          else paste(a, "by", b)
  tabn <- if (missing(tab)) "" else tab_title_names(tab, max)
  if (nzchar(tabn)) res <- paste0(res, " (tabbed by ", tabn, ")")
  res
}


