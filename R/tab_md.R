# PURPOSE: Export tabxplor tables to simple, human-readable markdown, colours as pandoc spans.
# ROLE: Parallel to tab_kable() (HTML) and tab_xl() (Excel); consumes the shared tab_export_prep().
# KEY CONSTRAINTS:
#   - Padding must be monospace-precise: numbers right-aligned, pipes aligned (raw text stays readable).
#   - Bold rows (**...**) can touch pipes; normal cells have 1-space margins.
#   - Phase 10f: a COLOURED table (any fmt column with a colour measure) wraps EVERY fmt cell in a
#     break-derived pandoc span [<num>]{.class} (uncoloured cells get the neutral .n) so numbers stay
#     aligned; an UNCOLOURED table (or color = FALSE) is byte-identical to the plain padded layout.
#     Class names are palette-INDEPENDENT (slot -> break); tab_md_css() maps them to the palette hex.
# See: CLAUDE.md Phase 10f, dev/tabxplor_phase10_exporters.md (Sec 12).

#' Export a tabxplor table to a markdown table
#'
#' @param tabs A table made with \code{\link{tab}} or \code{\link{tab_many}}, or a `list` of tab.
#'   A list of tables sharing the same `col_vars` (and no `tab_vars`) is merged into one; any other
#'   list --- several `row_vars` and/or `tab_vars` (e.g. `tab()` with several row variables and a
#'   `tab_vars`) --- is rendered one table after another, each keeping its own sub-tables.
#' @param bold_references Bold reference/total rows with markdown `**...**`.
#' @param special_formatting Passed to \code{\link[=format.tabxplor_fmt]{format()}}.
#'   When `TRUE`, shows "ref:" prefix on diff reference cells, "mean:" on ctr
#'   totals, sigma on means.
#' @param wrap_rows Max width for row labels before truncation. `NULL` (default) never truncates
#'   (lossless -- the column grows); set a number to cap the label width. A markdown pipe cell cannot
#'   hold a raw newline, so md "wrapping" means "do not truncate".
#' @param subtext Print chi2/footnotes below the table.
#' @param color When `TRUE` (default) and the table carries colours (e.g. built with
#'   `tab(..., color = "diff")`), each fmt cell is wrapped in a short pandoc bracketed span
#'   `[value]{.class}` so the markdown renders coloured in Quarto / RMarkdown / pandoc (and the
#'   companion \code{\link{tab_md_css}} styles the classes). `FALSE` produces plain monochrome
#'   markdown. Uncoloured tables never get spans.
#' @param color_legend When `TRUE` (default) and the table is coloured, prepend a colour-legend prose
#'   line (its break-words in the same pandoc classes as the cells) above the subtext.
#' @param lang Colour-legend language: `NULL` (auto from the R/OS locale, English fallback), `"en"` or `"fr"`.
#' @param theme,html_24_bit Colour palette selectors (as in
#'   \code{\link[=tab_kable]{tab_kable()}}); they only affect the CSS emitted by `css = TRUE` /
#'   \code{\link{tab_css}}, since the span *class names* are palette- and theme-independent. `theme`
#'   accepts `"auto"` (follow the reader's colour scheme).
#' @param color_type `r lifecycle::badge("deprecated")` Inert since 1.4.0: the text channel always uses
#'   the text palette. The colour CHANNEL is chosen by `color = c(text, background)` (see \code{\link{tab}}).
#' @param caption Optional table caption, rendered as a pandoc caption line `: caption` (captions only
#'   the first table of a list).
#' @param transpose Set to `TRUE` to transpose each table before export (rows become columns) --
#'   the col-percentages-with-several-row-variables use case.
#' @param title `r lifecycle::badge("deprecated")` Renamed to `caption`.
#' @param var_names Which variable names to write beside the table: `"both"` (the default),
#'   `"rows"`, `"cols"` or `"none"`. The column variables' names are written as an italic body row
#'   above their level columns; the row-variable name is the leading column a table with several
#'   `row_vars` uses to name each block (written once per block, in italics). See
#'   \code{\link{tab_kable}}.
#' @param col_var_names `r lifecycle::badge("deprecated")` Replaced by `var_names`:
#'   `col_var_names = FALSE` is `var_names = "rows"` (or `"none"`).
#' @param css When `TRUE`, prepend an inline `<style>` block (from \code{\link{tab_css}}), so the
#'   coloured markdown is self-contained. Default `FALSE` (bring the stylesheet via the document's
#'   `css:`, or emit \code{\link{tab_css}} once at the top of the document -- it styles every table).
#'   Any **styled** table (coloured, or `css = TRUE`) is wrapped in a pandoc fenced div
#'   `::: {.tabxplor-tab}`, which pandoc renders as `<div class="tabxplor-tab">` -- the hook
#'   \code{\link{tab_css}}'s table styling needs, since pandoc emits a bare `<table>` it could not
#'   otherwise reach. So the rendered HTML of a markdown table can look like `tab_kable()`'s (compact
#'   layout, thin rules under the variable-name row and between sub-tables, no host borders), not just
#'   be coloured -- even with `css = FALSE`, as long as the stylesheet is brought in some other way. A
#'   plain uncoloured table is left byte-identical (no div).
#' @param clipboard Copy output to clipboard via \code{clipr::write_clip()}.
#'   Requires the \pkg{clipr} package.
#' @param file Path to write the markdown to a file. `NULL` (default) skips.
#' @param print If `TRUE`, print via `cat()` and return invisibly. If `FALSE`,
#'   return the character string.
#'
#' @return A character string (visible or invisible depending on `print`).
#' @export
#'
#' @examples
#' \donttest{
#' tab(forcats::gss_cat, race, marital, pct = "row") |> tab_md()
#' tab(forcats::gss_cat, race, marital, pct = "row", color = "diff") |> tab_md()
#' tab(forcats::gss_cat, race, marital, pct = "row", color = "diff") |>
#'   dplyr::mutate(dplyr::across(dplyr::where(is_fmt), ~set_display(., "diff"))) |>
#'   tab_md()
#' }
tab_md <- function(tabs,
                   bold_references = TRUE,
                   special_formatting = TRUE,
                   wrap_rows = NULL,
                   subtext = TRUE,
                   color = TRUE,
                   color_legend = TRUE,
                   lang = NULL,
                   theme = NULL,
                   color_type = lifecycle::deprecated(),
                   html_24_bit = NULL,
                   caption = NULL,
                   transpose = FALSE,
                   var_names = NULL,
                   css = FALSE,
                   clipboard = FALSE,
                   file = NULL,
                   print = TRUE,
                   title = lifecycle::deprecated(),
                   col_var_names = lifecycle::deprecated()) {
  if (lifecycle::is_present(color_type)) lifecycle::deprecate_soft("1.4.0", "tab_md(color_type)")
  # Phase 13a: install a per-table color_breaks override for the render (no-op otherwise).
  .cb <- push_color_breaks(tabs); on.exit(pop_color_breaks(.cb), add = TRUE)
  # Phase 10j: `title` renamed to `caption` (unified across exporters); `transpose` added.
  if (lifecycle::is_present(title)) {
    lifecycle::deprecate_soft("1.4.0", "tab_md(title)", "tab_md(caption)")
    caption <- title
  }
  # Phase 14i: `col_var_names` (md-only) generalised to the shared `var_names`, which also governs the
  # row-variable name and is honoured by every exporter. FALSE = drop the col side of whatever
  # `var_names` asks for, so the two compose rather than fight.
  if (lifecycle::is_present(col_var_names)) {
    lifecycle::deprecate_soft("1.4.0", "tab_md(col_var_names)", "tab_md(var_names)")
    if (!isTRUE(col_var_names)) {
      var_names <- resolve_export_opts(var_names = var_names)$var_names
      var_names <- if (identical(var_names, "cols")) "none" else
                   if (identical(var_names, "both")) "rows" else var_names
    }
  }
  # `html_24_bit` is inert (Phase 13a): markdown colour spans map to CSS classes, always 24-bit.
  # Phase 13d: `allow_auto` -- markdown carries a stylesheet (css = TRUE / tab_css()), so it can follow
  # the reader's colour scheme. The spans themselves are theme-independent (only the CSS differs).
  o <- resolve_export_opts(theme = theme, color = color, transpose = transpose,
                           var_names = var_names, allow_auto = TRUE)
  theme <- o$theme; color <- o$color

  # --- Phase 10d/10f: shared exporter prep + the base/list split. ---
  # A single tab (or a mergeable same-col_vars / no-tab_vars list) renders as ONE table; a NON-mergeable
  # list (several row_vars and/or tab_vars -> tab() returns a list) renders each table
  # one-after-another (list_method = TRUE), each keeping its own tab_vars sub-tables. md keeps tab_vars
  # (drop_tab_vars = FALSE) and does its own str_trunc (wrap = NULL). 10f: adding "colors" to `compute`
  # fills the per-cell slots fmt_col_ann() carries -> md_render_one() renders pandoc colour spans.
  compute <- "refs"
  if (bold_references) compute <- c(compute, "bold")
  if (color) compute <- c(compute, "colors")
  prep <- tab_export_prep(tabs, backend = "md", drop_tab_vars = FALSE, wrap = NULL,
                          compute = compute, transpose = o$transpose,
                          theme = theme, var_names = o$var_names, list_method = TRUE,
                          what = "tab_md()")

  parts   <- purrr::imap_chr(prep$tables, function(rd, i) {
    # Phase 14w (item 1) / 17b: fall back to a stored caption (set_caption()) then a reg table's
    # auto-title (`reg_title`) when the user gave no caption= .
    cap <- if (i == 1) caption else NULL
    if (is.null(cap)) cap <- rd$caption
    if (is.null(cap) && !is.null(rd$reg_title) && !is.na(rd$reg_title)) cap <- rd$reg_title
    md_render_one(rd, special_formatting = special_formatting, wrap_rows = wrap_rows,
                  subtext = subtext, color = color, css = css,
                  color_legend = color_legend, lang = lang,
                  title = cap,
                  theme = theme)
  })
  md_text <- paste(parts, collapse = "\n\n")

  # Phase 14f/14m-iii: a STYLED table is wrapped in a pandoc fenced div, and (with `css = TRUE`) the
  # stylesheet is prepended. Pandoc emits a BARE `<table>` for a pipe table, which none of tab_css()'s
  # `.tabxplor-tab ...` rules can reach -- so a rendered markdown table got the colours but none of the
  # layout (compact padding, thin spacer columns, the border-taming of 14m-iii). `::: {.tabxplor-tab}`
  # renders as `<div class="tabxplor-tab">`, the hook every selector matches. 14m-iii DECOUPLES the div
  # from `<style>`: a table is styled when it is coloured OR `css = TRUE`, and a styled table always
  # carries the div -- so the "one tab_css() per document" workflow (bring the sheet via the document)
  # reaches a coloured `tab_md(css = FALSE)` too. A plain uncoloured table stays byte-identical (no div).
  # `md_has_color()` is the one definition of "is this table coloured", shared with md_render_one().
  any_color <- any(vapply(prep$tables, md_has_color, logical(1), color = color))
  styled    <- any_color || isTRUE(css)
  if (styled) md_text <- paste0("::: {.tabxplor-tab}\n", md_text, "\n:::")
  if (isTRUE(css)) {
    md_text <- paste0(tab_css(theme = theme, chrome = TRUE, style_tag = TRUE), "\n\n", md_text)
  }

  if (!is.null(file)) writeLines(md_text, file)
  if (clipboard) {
    if (!requireNamespace("clipr", quietly = TRUE)) {
      warning("Package 'clipr' is needed to copy to clipboard. ",
              "Install it with install.packages('clipr').")
    } else {
      clipr::write_clip(md_text)
    }
  }
  if (print) {
    cat(md_text, "\n")
    return(invisible(md_text))
  }
  md_text
}


#' CSS for the colour spans of \code{\link{tab_md}}
#'
#' A thin wrapper around \code{\link[=tab_css]{tab_css(chrome = FALSE)}}, kept for discoverability
#' alongside \code{\link{tab_md}}. The stylesheet does not depend on the table -- classes name a palette
#' **slot**, not a break -- so `tabs` is ignored and one stylesheet styles every table in a document.
#'
#' @param tabs Ignored (the CSS is table-independent). Kept so `tab_md_css(tabs)` still reads naturally.
#' @param ... Passed to \code{\link{tab_css}} (`theme`, `style_tag`, `file`).
#'
#' @return A character string of CSS (invisible when `file` is given).
#' @seealso [tab_css()], which is the generator and also styles `tab_kable(engine = "html")`.
#' @export
#'
#' @examples
#' cat(tab_md_css())
tab_md_css <- function(tabs = NULL, ...) {
  tab_css(..., chrome = FALSE)
}


# Render ONE prepared table (`rd`, from tab_export_prep) to a markdown string (no file/clipboard/print
# -- tab_md() joins the parts and handles those). Holds the md-specific rendering (Steps 4-13).
# Phase 10f: when the table carries colours and `color = TRUE`, every fmt cell is wrapped in a pandoc
# bracketed span `[<num>]{.class}` (break-derived class, uncoloured cells get the neutral `.n`); the
# uniform scaffold keeps the numbers aligned in raw text. Uncoloured tables (or color = FALSE) render
# the byte-identical plain padded table.
md_render_one <- function(rd, special_formatting, wrap_rows, subtext,
                          color = TRUE, css = FALSE, color_legend = TRUE, lang = NULL, title = NULL,
                          theme = NULL) {
  # Graceful degrade -- a table that can't be read as a tabxplor table renders as a plain pipe table.
  if (isTRUE(rd$vars$degrade)) {
    if (isTRUE(rd$vars$notify)) tab_degrade_inform(rd$vars$reason)  # batch-aware (see tab_export_prep)
    return(paste(knitr::kable(tibble::as_tibble(rd$tab), format = "pipe"), collapse = "\n"))
  }

  tabs         <- rd$tab
  subtext_text <- if (subtext) rd$subtext else character(0)

  # Phase 14i: the LABEL columns (the shared blank/merge set: a merged table's synthetic name column,
  # OR the kept tab_vars) and their runs -- see tab_label_runs(). This replaces md's own `tab_vars`
  # gate, which 14d silenced on a merged table: tab_compact() correctly records tab_vars =
  # character(0), so the loop went quiet and the row-variable name printed on EVERY row.
  label_cols   <- rd$roles$label_cols
  label_runs   <- rd$roles$label_runs
  var_name_col <- rd$roles$var_name_col

  # Phase 16e: the whole footer prose block (weight -> Model: -> colour legend -> stars -> user subtext) via
  # the ONE shared builder. The break-words are wrapped in the SAME pandoc span classes the cells use (both
  # call tx_slot_class(), so tab_css() colours them identically). The source is the fmt table -- rd$color_src
  # for a transposed model (whose rd$tab is plain character), so weight/stars/legend all read the right
  # attributes (previously weight/stars read the stripped rd$tab). Legend only when coloured.
  src         <- if (is.null(rd$color_src)) tabs else rd$color_src
  want_legend <- isTRUE(color) && isTRUE(color_legend) && length(rd$roles$color_cols) != 0
  subtext_text <- suppressWarnings(render_footer(
    tab_footer_streams(src, style = legend_export_style(), subtext = subtext_text, legend = want_legend),
    medium = "md", theme = theme))

  # md drops the trailing separator (no line after the last row); the prep's new_group is the base.
  new_group <- rd$roles$new_group
  new_group <- new_group[new_group < nrow(tabs)]

  fmt_mask   <- rd$roles$fmt_mask
  fmt_cols   <- rd$roles$fmt_cols
  other_cols <- rd$roles$other_cols
  col_var_map   <- rd$roles$col_var_map
  real_col_vars <- rd$roles$real_col_vars
  has_multi_col_vars <- length(real_col_vars) > 1
  bold_rows  <- rd$bold_rows
  cvh        <- rd$col_var_header      # Phase 13c-iii: spanning names + suffix-stripped level labels

  # md-local: positions where a REAL col_var changes (span-header separators). Distinct from kable's
  # col-border transition index, so it is not shared.
  new_col_var <- integer(0)
  if (has_multi_col_vars) {
    cv_simplified <- col_var_map
    cv_simplified[names(other_cols)] <- names(other_cols)
    for (k in seq_along(cv_simplified)[-1]) {
      prev_cv <- cv_simplified[k - 1]
      curr_cv <- cv_simplified[k]
      if (prev_cv %in% real_col_vars && curr_cv %in% real_col_vars &&
          prev_cv != curr_cv) {
        new_col_var <- c(new_col_var, k - 1L)
      }
    }
  }

  # --- Step 6: Format all cells to character ---
  # Format fmt columns. The reference masks are reused from the prep's `ann` (.ref) so
  # format() does not re-run get_reference() -- byte-identical (Phase 10c subset-equivalence).
  # stars = TRUE: main display. When a column carries significance stars, format() right-pads the star
  # field so numbers stay aligned; trim ONLY the leading side to preserve that trailing pad
  # (byte-identical when no star is present -- format() emits no trailing space otherwise).
  # Phase 13c-ii: bold_split = TRUE also attaches primary_nchar (the bold-prefix width of a composite
  # "{pct} (n={n})" cell, on the UN-trimmed string) so a bold row bolds only the primary field
  # (the "(n=...)" stays plain). str_trim(left) shifts it by the leading spaces removed -> prim = pn - lead.
  fmt_out <- purrr::imap(tabs, \(col, nm) {
    if (is_fmt(col)) {
      # Phase 14m-ii: pad the VALUE-INTERNAL alignment (thousands mark, "(n=...)", star field, ci
      # brackets, the sd-less mean tail) with a FIGURE SPACE. Markdown sets no font of its own, so when
      # pandoc renders the table to html the number cells fall in the host's PROPORTIONAL font -- where
      # an ASCII space is half a digit wide and CSS collapses runs of them, so "100% (n=  673)" arrived
      # ragged. A figure space is a digit wide and never collapses, so the composites line up. This is
      # `format()`'s pad only: the CELL-EDGE alignment (pad_cell / md_color_cell) and the spacer columns
      # stay ASCII on purpose -- pandoc strips cell-edge whitespace, so an empty cell must render `<td></td>`
      # (`:empty`), the hook Phase 14m's spacer-collapse + blank-row separators key on. nchar is
      # unchanged (a figure space is one codepoint), so the raw-markdown column layout is byte-for-byte
      # the same, only the pad glyph inside a value differs.
      raw     <- format(col, special_formatting = special_formatting, na = "", stars = TRUE,
                        bold_split = TRUE, pad = fig_space, .ref = ann_ref(rd$ann[[nm]]))
      pn      <- attr(raw, "primary_nchar")
      trimmed <- stringi::stri_trim(raw, side = "left")
      lead    <- nchar(raw) - nchar(trimmed)
      trimmed[is.na(trimmed)] <- ""
      list(txt  = trimmed,
           prim = if (is.null(pn)) rep(NA_integer_, length(col)) else pn - lead)
    } else {
      # Phase 14f: a `|` in a level or tab_var label would open a spurious cell and desync the whole
      # row's column count. Escape it -- pandoc renders `\|` as a literal pipe inside a cell. Only the
      # non-fmt (label) columns can contain one; fmt cells are numbers the package formats itself.
      list(txt = gsub("|", "\\|", as.character(col), fixed = TRUE),
           prim = rep(NA_integer_, length(col)))
    }
  })
  cell_data <- as.data.frame(lapply(fmt_out, `[[`, "txt"), stringsAsFactors = FALSE)
  prim_mat  <- do.call(cbind, lapply(fmt_out, `[[`, "prim"))   # per-cell bold-split point (NA = whole)

  # Truncate row labels (10f: only when wrap_rows is set; default NULL = lossless, column grows).
  # A pipe cell cannot hold a raw newline, so md "wrap" means "do not truncate by default".
  if (!is.null(wrap_rows)) {
    for (j in other_cols) {
      cell_data[[j]] <- tx_str_trunc(cell_data[[j]], wrap_rows)
    }
  }

  # Phase 14i: name each block ONCE -- blank every label cell that is not a run start (the run model is
  # the prep's, shared with the html rowspan and the Excel merge). The old loop was a naive per-column
  # `vals[i] == vals[i-1]` gated on `tab_vars`, so a merged table (which has none) named its
  # row-variable on every row; tab_label_runs() also nests the columns, which the naive scan did not.
  # The name column is ITALIC (the maintainer's call): it mirrors the *col_var* name row below and, in
  # a column that otherwise holds level labels, marks the cell as a variable NAME. tab_var cells stay
  # plain -- their values ARE levels ("Male"), not names. Done BEFORE the width pass, so the markup is
  # measured; the column is left-aligned, so the padding needs no arithmetic.
  for (cl in names(label_cols)) {
    idx <- which(names(cell_data) == cl)
    if (length(idx) != 1) next
    show <- label_runs[[cl]]$show
    cell_data[[idx]][!show] <- ""
    if (cl %in% names(var_name_col)) {
      nz <- show & nzchar(cell_data[[idx]]) & !is.na(cell_data[[idx]])
      cell_data[[idx]][nz] <- paste0("*", cell_data[[idx]][nz], "*")
    }
  }

  is_right <- fmt_mask  # named logical: TRUE for fmt (right-aligned) columns

  # Blank out the label columns' header names (they label sub-tables, not real columns). The `""`
  # sentinel in names(cell_data) is what drives `col_names` at Step 7. Phase 14i: `tab_vars` ->
  # `label_cols`, so a merged table's name column loses the literal "row_var" header here too (the
  # prep already blanks it in cvh$clean, for the three backends that read the header model).
  for (cl in names(label_cols)) {
    idx <- which(names(cell_data) == cl)
    if (length(idx) == 1) names(cell_data)[idx] <- ""
  }

  # --- Step 6b (Phase 10f): per-cell pandoc span attributes (colour) ---
  # A table is "coloured" iff some fmt column carries a colour measure. In that mode every COLOURED fmt
  # cell is wrapped in a span; an uncoloured table keeps the byte-identical plain path. attr_mat holds
  # the per-cell "{.class}" string (fmt columns only; "" = no span).
  # Phase 13d: the class is a pure function of the palette slot the engine already assigned, so no
  # per-column plan/palette lookup is needed here at all -- and the names match tab_kable()'s <td>
  # classes, both styled by the one tab_css() stylesheet.
  do_color <- md_has_color(rd, color)
  # Phase 14m-iii: a STYLED table (coloured, or the caller asked for the stylesheet) gets the pandoc
  # chrome -- blank-row separators the stylesheet collapses to 1px rules (Steps 12/13) instead of the
  # raw-text dash rows. A plain table (`!styled`) keeps the dash rows so its GFM output is byte-clean.
  styled   <- do_color || isTRUE(css)
  attr_mat <- NULL
  if (do_color) {
    attr_mat <- matrix("", nrow = nrow(cell_data), ncol = ncol(cell_data))
    for (k in seq_along(fmt_cols)) {
      nm  <- names(fmt_cols)[k]
      j   <- fmt_cols[[k]]
      a   <- rd$ann[[nm]]
      ts  <- if (!is.null(a$text_slot)) a$text_slot else integer(nrow(cell_data))
      bs  <- if (!is.null(a$bg_slot))   a$bg_slot   else integer(nrow(cell_data))
      attr_mat[, j] <- vapply(seq_len(nrow(cell_data)),
                              function(i) md_span_attr(ts[i], bs[i]),
                              character(1))
    }
  }

  # --- Step 7: Compute column widths ---
  n_rows <- nrow(cell_data)
  n_cols <- ncol(cell_data)
  # Phase 13c-iii: the level-header row uses the suffix-stripped labels (the col_var name is now written
  # in the span row above), keeping the tab_var headers blanked (names(cell_data) == "" for tab_vars).
  col_names <- cvh$clean
  col_names[names(cell_data) == ""] <- ""

  # For each cell, compute the raw text width
  cell_widths <- matrix(0L, nrow = n_rows, ncol = n_cols)
  for (j in seq_len(n_cols)) {
    cell_widths[, j] <- nchar(cell_data[[j]])
  }
  header_widths <- nchar(col_names)

  # Phase 14i: `bold_rows` is a pure ROW set, applied to every column -- so a bold row bolded the LABEL
  # cell too (`**DIPLOM**`). The label columns opt out, here at the consumer (the prep cannot know a
  # backend's markup). The LEVEL still bolds on a reference row, which is wanted. This is the ONE
  # definition: the width pass below and the Step-11 body loop must charge the same markup, or the
  # column over-pads by the `**` it no longer writes.
  no_bold      <- seq_len(n_cols) %in% label_cols
  bold_rows_of <- function(j) if (no_bold[j]) integer(0) else bold_rows

  # Column width = max of display widths:
  #   right-aligned normal: nchar + 3 (1 leading + 2 trailing for bold zone)
  #   left-aligned normal:  nchar + 2 (1 space each side)
  #   bold cell:            nchar + 4 (**...**)
  #   header:               nchar + 2
  # Phase 10f: a coloured fmt column is laid out as " [<num>]<attr> " (fixed scaffold), so its width is
  # num_width (numbers, bold-aware) + attr_width ({.class}) + 4, big enough for the header. num_width /
  # attr_width are reused by the Step-11 scaffold so the numbers align (fixed offset) and pipes align.
  col_width  <- integer(n_cols)
  num_width  <- integer(n_cols)
  attr_width <- integer(n_cols)
  for (j in seq_len(n_cols)) {
    if (do_color && is_right[j]) {
      # Phase 14f: `num_width` is the width of the VISIBLE value, so the bold rows' +4 must NOT enter
      # it. `**` is markup, not text: adding it here padded the value INSIDE the bracket, so every
      # coloured cell in a column that has any bold row read "[    38%]{.p2}" -- four spaces pandoc
      # discards, and which in the raw markdown push the number out of line with the bold one. The
      # bold cells' extra 4 is a property of THEIR text, so it belongs in col_width (below) only.
      nonempty <- nzchar(cell_data[[j]])
      # `num_width` is the widest cell measured in the raw columns its content occupies UP TO its last
      # visible character -- the value plus any markup that precedes that character (md_extra()).
      # Padding to it aligns what the reader sees; padding to the value alone (or, worse, adding the
      # bold +4 to the value) does not, because the markup is invisible only once rendered.
      vis <- cell_widths[, j] + md_extra(cell_data[[j]], seq_len(n_rows) %in% bold_rows_of(j),
                                         prim_mat[, j])
      num_width[j]  <- if (any(nonempty)) max(vis[nonempty]) else 0L
      attr_width[j] <- if (any(nonempty)) max(nchar(attr_mat[nonempty, j])) else 0L
      col_width[j]  <- max(num_width[j] + attr_width[j] + 4L, header_widths[j] + 2L)
    } else {
      margin <- if (is_right[j]) 3L else 2L
      widths <- cell_widths[, j] + margin  # normal cells
      bj     <- bold_rows_of(j)
      if (length(bj) > 0) {
        widths[bj] <- cell_widths[bj, j] + 4L  # bold cells
      }
      col_width[j] <- max(c(widths, header_widths[j] + 2L))
    }
  }

  # --- Helper: pad a cell ---
  # is_right: TRUE for fmt (right-aligned), FALSE for text (left-aligned)
  # is_bold: TRUE to wrap with **
  # split_at: Phase 13c-ii -- for a composite cell, the bold-prefix width (NA = bold the whole cell).
  pad_cell <- function(text, width, is_right, is_bold, split_at = NA_integer_) {
    if (is_bold && nchar(text) > 0) {
      bold_text <- md_bold(text, split_at)                # partial (composite) or whole-cell bold
      if (is_right) {
        stringi::stri_pad(bold_text, width, side = "left")
      } else {
        stringi::stri_pad(bold_text, width, side = "right")
      }
    } else {
      # Non-bold, or bold with empty text (just pad normally)
      if (is_right) {
        # Right-align: pad text to (width - 2) then add 2 trailing spaces
        paste0(stringi::stri_pad(text, width - 2L, side = "left"), "  ")
      } else {
        # Left-align: 1 leading space + text padded to (width - 2) + 1 trailing space
        paste0(" ", stringi::stri_pad(text, width - 2L, side = "right"), " ")
      }
    }
  }

  # --- Step 8: the col_var-name row (Phase 13c-iii; re-sited in Phase 14f) ---
  # WARNING: this row is a BODY row -- it is emitted AFTER the delimiter (see Step 13). It used to sit
  # above the level-name header, which made a TWO-ROW HEADER, and **pandoc does not have those**: it
  # silently gave up on the whole table and rendered it as a line-block followed by a paragraph of
  # pipes. Every tab_md() table carrying a col_var name (i.e. every normal one -- 13c-iii shows the
  # name for a single col_var too) was invalid. Verified with pandoc 3.7.
  # Below the delimiter it parses, and it is styled as data: the name in the FIRST cell of its group
  # (a centred span would need a colspan pandoc pipe tables cannot express), italic, so it reads as a
  # sub-heading rather than a value.
  # Phase 14i: `var_names` drops it by blanking `cvh$label` in the prep -- so this gate needs no
  # argument of its own (it is the same gate the html/kableExtra/xl span rows already used).
  col_var_header_line <- NULL
  if (any(nzchar(cvh$label))) {
    header_parts <- character(0)
    j <- 1
    while (j <= n_cols) {
      lbl <- cvh$label[j]
      if (nzchar(lbl)) {
        # Group consecutive columns spanned by the same col_var name
        j_end <- j
        while (j_end < n_cols && cvh$label[j_end + 1] == lbl) j_end <- j_end + 1
        group_cols <- j:j_end
        # The name goes in the FIRST cell of its group, italic; the rest of the group is blank. It is
        # one cell PER COLUMN, never a merged one -- a pipe row must keep the table's cell count or
        # pandoc shifts the data. A long name simply overflows its own cell: that row is deliberately
        # not pipe-ALIGNED (it parses; only a markdown linter minds), because padding to it would
        # widen every column below it.
        nm_cell <- paste0(" *", lbl, "*")
        header_parts <- c(header_parts,
                          stringi::stri_pad(nm_cell, col_width[j], side = "right"))
        if (j_end > j) header_parts <- c(header_parts,
                                         strrep(" ", col_width[(j + 1L):j_end]))
        # Add separator column between real col_var groups (multi col_var only)
        if (j_end %in% new_col_var && j_end < n_cols) {
          header_parts <- c(header_parts, " ")
        }
        j <- j_end + 1
      } else {
        # Non-grouped column (row var / total / count): empty cell matching column width
        header_parts <- c(header_parts, strrep(" ", col_width[j]))
        j <- j + 1
      }
    }
    col_var_header_line <- paste0("|", paste(header_parts, collapse = "|"), "|")
  }

  # --- Step 9: Build level-names header row ---
  header_cells <- character(n_cols)
  for (j in seq_len(n_cols)) {
    header_cells[j] <- if (is_right[j]) {
      # Right-aligned header
      paste0(stringi::stri_pad(col_names[j], col_width[j] - 2L, side = "left"), "  ")
    } else {
      # Left-aligned header
      paste0(" ", stringi::stri_pad(col_names[j], col_width[j] - 2L, side = "right"), " ")
    }
  }

  # Insert separator columns between col_var groups
  header_line <- md_insert_col_sep(header_cells, new_col_var, n_cols, has_multi_col_vars)

  # --- Step 10: Build alignment separator ---
  sep_cells <- character(n_cols)
  for (j in seq_len(n_cols)) {
    dashes <- strrep("-", col_width[j] - 1L)
    sep_cells[j] <- if (is_right[j]) {
      paste0(dashes, ":")
    } else {
      paste0(":", dashes)
    }
  }
  sep_line <- md_insert_col_sep(sep_cells, new_col_var, n_cols, has_multi_col_vars, fill = "-")

  # --- Step 11: Build body rows ---
  body_lines <- character(n_rows)
  for (i in seq_len(n_rows)) {
    is_bold <- i %in% bold_rows
    row_cells <- character(n_cols)
    for (j in seq_len(n_cols)) {
      split_at <- prim_mat[i, j]                          # Phase 13c-ii composite bold-prefix width
      bold_j   <- is_bold && !no_bold[j]   # Phase 14i -- see bold_rows_of() above
      if (do_color && is_right[j]) {
        row_cells[j] <- md_color_cell(cell_data[[j]][i], attr_mat[i, j],
                                      num_width[j], col_width[j], bold_j, split_at,
                                      attr_width = attr_width[j])
      } else {
        row_cells[j] <- pad_cell(cell_data[[j]][i], col_width[j],
                                  is_right[j], bold_j, split_at)
      }
    }
    body_lines[i] <- md_insert_col_sep(row_cells, new_col_var, n_cols,
                                        has_multi_col_vars)
  }

  # --- Step 12: Insert sub-table separators ---
  if (length(new_group) > 0) {
    # Phase 14m-iii: on the STYLED path a sub-table boundary is a fully-blank row (all cells :empty in
    # the render) that tab_css() collapses to a 1px border-top -- a theme-aware rule with NO dash marker
    # in the raw markdown. The PLAIN path keeps the dash row, so its GFM/text output stays byte-clean.
    if (styled) {
      sep_row <- md_blank_row(col_width, new_col_var, n_cols, has_multi_col_vars)
    } else {
      dash_cells <- character(n_cols)
      for (j in seq_len(n_cols)) {
        dash_cells[j] <- paste0(" ", strrep("-", col_width[j] - 2L), " ")
      }
      sep_row <- md_insert_col_sep(dash_cells, new_col_var, n_cols, has_multi_col_vars)
    }

    # Insert separators after the appropriate rows
    result_lines <- character(0)
    prev <- 1
    for (g in new_group) {
      result_lines <- c(result_lines, body_lines[prev:g], sep_row)
      prev <- g + 1
    }
    if (prev <= n_rows) {
      result_lines <- c(result_lines, body_lines[prev:n_rows])
    }
    body_lines <- result_lines
  }

  # --- Step 13: Assemble and output ---
  # Phase 14f: the col_var-name row goes BELOW the delimiter (a body row). Above it, it made a two-row
  # header, which pandoc does not accept -- see Step 8.
  # Phase 14m-iii: on the styled path, follow the col_var-name row with a blank row -> tab_css() draws a
  # 1px border-top under it (the "rule under the name" the maintainer asked for), theme-aware, with no
  # dash in the raw markdown. Only when the name row exists (var_names may have dropped it).
  name_underline <- if (styled && !is.null(col_var_header_line)) {
    md_blank_row(col_width, new_col_var, n_cols, has_multi_col_vars)
  } else NULL
  all_lines <- c(header_line, sep_line, col_var_header_line, name_underline, body_lines)

  # Optional caption -- a pandoc table caption line (numbered/cross-referenceable in Quarto).
  if (!is.null(title)) {
    all_lines <- c(all_lines, "", paste0(": ", title))
  }

  if (length(subtext_text) > 0) {
    all_lines <- c(all_lines, "", subtext_text)
  }

  md_text <- paste(all_lines, collapse = "\n")
  md_text
}


# Helper: insert empty separator columns between col_var groups
# `fill` is what the thin spacer column between col_var groups contains. It MUST be "-" on the
# delimiter row: a pandoc delimiter cell has to be dashes (optionally with `:`), and a blank one ("| |")
# made pandoc reject the table outright. Every other row wants a blank spacer.
md_insert_col_sep <- function(cells, new_col_var, n_cols, has_multi_col_vars, fill = " ") {
  if (!has_multi_col_vars || length(new_col_var) == 0) {
    return(paste0("|", paste(cells, collapse = "|"), "|"))
  }

  parts <- character(0)
  for (j in seq_along(cells)) {
    parts <- c(parts, cells[j])
    if (j %in% new_col_var && j < n_cols) {
      parts <- c(parts, fill)  # the thin separator column
    }
  }
  paste0("|", paste(parts, collapse = "|"), "|")
}


# Phase 14m-iii: a fully-blank pipe row -- every cell is ASCII spaces, so pandoc renders each cell as
# `<td></td>` (`:empty`). tab_css() then selects the row (`tbody tr:not(:has(td:not(:empty)))`) and
# collapses it to a 1px border-top: the sub-table / col_var-name rule, theme-aware, marker-free in the
# raw markdown. WARNING: ASCII spaces ONLY. A cell of a FIGURE space (U+2007) renders `<td> </td>` --
# NOT `:empty` -- and the whole 14m-iii collapse dies. The 14m-ii figure-space swap is confined to a
# value's INTERNAL padding for exactly this reason; the cell-edge pad here must stay ASCII.
#' @keywords internal
md_blank_row <- function(col_width, new_col_var, n_cols, has_multi_col_vars) {
  md_insert_col_sep(strrep(" ", col_width), new_col_var, n_cols, has_multi_col_vars)
}


# The ONE definition of "is this rendered table coloured" -- a table is coloured iff `color` is on and
# some fmt column carries a colour measure (its prep annotation has `has_color`). Shared by tab_md()'s
# fenced-div gate and md_render_one()'s span/styled logic, so the two cannot disagree.
#' @keywords internal
md_has_color <- function(rd, color) {
  isTRUE(color) && length(rd$ann) > 0L &&
    any(vapply(rd$ann, function(a) isTRUE(a$has_color), logical(1)))
}


# === SECTION: Phase 10f colour spans (break-derived pandoc classes) ==================

# The pandoc bracketed-span attribute for ONE cell: "{.p3 .o2}" / "{.p3}" / "{.o2}" / "" (uncoloured).
# Phase 13d: the class names come from the shared slot vocabulary (tx_slot_class, R/tab-css.R), so a
# markdown span and an html <td> name the same class and ONE stylesheet (tab_css()) styles both.
# An uncoloured cell gets NO span at all -- md_color_cell() keeps it aligned instead.
#' @keywords internal
md_span_attr <- function(text_slot, bg_slot) {
  parts <- c(tx_slot_class("text", text_slot), tx_slot_class("bg", bg_slot))
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0L) return("")
  paste0("{", paste0(".", parts, collapse = " "), "}")
}

# One fmt cell of a coloured column: "<pad>[<num>]<attr><pad>", padded to `total_width`.
# Phase 14f: the alignment target is the VISIBLE NUMBER, not the markup. Markup (`[`, `**`) is invisible
# once rendered but occupies columns in the raw file, so the number's right edge is placed at a fixed
# offset and each cell's markup PREFIX grows leftwards into its own pad. That is what lets a bold cell
# `**54%**` and a coloured one `[42%]{.m2}` show their numbers in the same column of the raw markdown.
# Before, `num_width` carried the bold rows' +4 and the value was padded INSIDE the bracket
# ("[    38%]{.p2}") -- four spaces pandoc discards, and which shifted the number the other way.
# The attr is padded to `attr_width` (pandoc ignores spaces inside `{...}`: `{.m2  }` == `{.m2}`), so
# the closing `}` lines up too when classes differ in length. The whole body is then right-padded so
# the next pipe lands at a fixed column.
# DESIGN (Phase 13d): an UNCOLOURED cell carries no span (`attr = ""`) and needs no bracket -- its pad
# absorbs the missing markup, so its number aligns with the others without a do-nothing `.n` class.
#' @keywords internal
md_color_cell <- function(text, attr, num_width, total_width, is_bold, split_at = NA_integer_,
                          attr_width = nchar(attr)) {
  if (!nzchar(text)) return(strrep(" ", total_width))
  content <- if (is_bold) md_bold(text, split_at) else text   # Phase 13c-ii partial/whole bold
  # An uncoloured cell uses " " where a coloured one opens its bracket, so a bracket costs no offset.
  open  <- if (nzchar(attr)) "[" else " "
  attr2 <- if (nzchar(attr) && attr_width > nchar(attr)) {
    sub("[}]$", paste0(strrep(" ", attr_width - nchar(attr)), "}"), attr)
  } else attr
  close <- if (nzchar(attr)) paste0("]", attr2) else ""
  # Pad by the cell's own VISIBLE-END width (value + the markup preceding its last visible character),
  # so every cell's last visible character lands on the same raw column. The markup grows leftwards
  # into the pad instead of pushing the value right.
  vis  <- nchar(text) + md_extra(text, is_bold, split_at)
  body <- paste0(strrep(" ", max(0L, num_width - vis)), open, content, close)
  stringi::stri_pad(paste0(" ", body), total_width, side = "right")
}

# How many RAW columns of markup precede a cell's last visible character. md_bold() adds "**" twice:
# for a whole-cell bold the closing pair sits AFTER the value (so it costs nothing here, 2); for a
# COMPOSITE cell it bolds only the primary field, so the closing pair sits mid-cell, before the
# "(n=...)" tail -- both pairs precede the last visible character (4). Vectorised over a column.
#' @keywords internal
md_extra <- function(text, is_bold, split_at) {
  whole <- is.na(split_at) | split_at < 1L | split_at >= nchar(text)
  ifelse(!is_bold | !nzchar(text), 0L, ifelse(whole, 2L, 4L))
}

# Phase 13c-ii: wrap the bold-prefix of a cell in **...**. For a composite cell (split_at = the primary
# field's width) only the primary token is bold and the rest ("(n=...)") stays plain; a plain cell
# (split_at NA / covering the whole text) is bolded whole. Adds exactly one ** pair either way, so the
# +4 width budget the column-width computation reserves for bold cells is unchanged.
#' @keywords internal
md_bold <- function(text, split_at = NA_integer_) {
  if (is.na(split_at) || split_at < 1L || split_at >= nchar(text)) return(paste0("**", text, "**"))
  paste0("**", substr(text, 1L, split_at), "**", substr(text, split_at + 1L, nchar(text)))
}

# Phase 13d: md_css_rules() / md_css_block() / md_break_class() / md_slot_class_map() are GONE. The
# stylesheet is table-independent (a pure function of palette + theme), so it is generated
# by tab_css() (R/tab-css.R) with no prep walk, no per-column plan and no per-table CSS.
