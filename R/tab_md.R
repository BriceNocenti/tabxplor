# PURPOSE: Export tabxplor tables to simple, human-readable markdown, colours as pandoc spans.
# ROLE: Parallel to tab_kable() (HTML) and tab_xl() (Excel); consumes the shared tab_export_prep().
# KEY CONSTRAINTS:
#   - THE HEADER BLOCK IS THREE ROWS but a pandoc pipe table takes ONE, so the col_var-name row and
#     the UNIT row (what each column holds -- "row%", "row% (n)") are BODY rows under the delimiter,
#     styled with emphasis. ⚠ Emphasis, never a `.tx-unit` class span: a span costs 13 characters of
#     raw line width md's fixed-width grid cannot absorb, and a monochrome table must carry no
#     pandoc span at all.
#   - Padding must be monospace-precise: numbers right-aligned, pipes aligned (raw text stays readable).
#   - Bold rows (**...**) can touch pipes; normal cells have 1-space margins.
#   - Phase 10f: a COLOURED table (any fmt column with a colour measure) wraps EVERY fmt cell in a
#     break-derived pandoc span [<num>]{.class} (uncoloured cells get the neutral .n) so numbers stay
#     aligned; an UNCOLOURED table (or color = FALSE) is byte-identical to the plain padded layout.
#     Class names are palette-INDEPENDENT (slot -> break); tab_css(format = "md") maps them to hex.
# See: CLAUDE.md Phase 10f, dev/tabxplor_phase10_exporters.md (Sec 12).

#' Export a tabxplor table to a markdown table
#'
#' @description
#' The Markdown exporter behind \code{\link{tab_export}}: `tab_export(x, format = "md")` calls this.
#'
#' @eval tab_args_rd("tab_md")
#' @param bold_references Bold reference/total rows with markdown `**...**`.
#' @param special_formatting Passed to \code{\link[=format.tabxplor_fmt]{format()}}.
#'   When `TRUE`, shows "ref:" prefix on diff reference cells, "mean:" on ctr
#'   totals, sigma on means.
#' @param subtext Print chi2/footnotes below the table.
#' @param theme Colour palette selector (as in \code{\link{tab_html}}); it only affects the CSS
#'   emitted by `css = TRUE` / \code{\link{tab_css}}, since the span *class names* are palette- and
#'   theme-independent. Accepts `"auto"` (follow the reader's colour scheme).
#' @param caption Optional table caption, rendered as a pandoc caption line `: caption` (captions only
#'   the first table of a list).
#' @param title `r lifecycle::badge("deprecated")` Renamed to `caption`.
#' @param col_var_names `r lifecycle::badge("deprecated")` Replaced by `var_names`:
#'   `col_var_names = FALSE` is `var_names = "rows"` (or `"none"`).
#' @param css When `TRUE` (the **default**), prepend an inline `<style>` block (from
#'   \code{\link{tab_css}}), so the exported markdown is self-contained -- it renders coloured and
#'   compact on its own. Set `FALSE` inside an `.Rmd`/`.qmd` document (the host page brings the
#'   stylesheet, or emit \code{\link{tab_css}} once at the top -- it styles every table), otherwise the
#'   inline `<style>` block is duplicated per table.
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
#' @param ... Retired arguments, accepted and ignored with a deprecation message since 2.0.0
#'   (`color_type`, `html_24_bit`): colour is a CSS class, and exports are always 24-bit.
#'
#' @return A character string (visible or invisible depending on `print`).
#' @export
#'
#' @examples
#' \donttest{
#' tab(forcats::gss_cat, race, marital, pct = "row") |> tab_md()
#' tab(forcats::gss_cat, race, marital, pct = "row", color = "difference") |> tab_md()
#' tab(forcats::gss_cat, race, marital, pct = "row", color = "difference") |>
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
                   caption = NULL,
                   transpose = FALSE,
                   var_names = NULL,
                   css = TRUE,
                   clipboard = FALSE,
                   file = NULL,
                   print = TRUE,
                   title = lifecycle::deprecated(),
                   col_var_names = lifecycle::deprecated(),
                   ...) {
  # Phase 19l: the retired inert arguments (`color_type`, `html_24_bit`, ...) ride `...`.
  tx_deprecate_inert(rlang::list2(...), "tab_md")
  # Phase 13a: install a per-table color_breaks override for the render (no-op otherwise).
  .cb <- push_color_breaks(tabs); on.exit(pop_color_breaks(.cb), add = TRUE)
  # Phase 10j: `title` renamed to `caption` (unified across exporters); `transpose` added.
  if (lifecycle::is_present(title)) {
    lifecycle::deprecate_soft("2.0.0", "tab_md(title)", "tab_md(caption)")
    caption <- title
  }
  # Phase 14i: `col_var_names` (md-only) generalised to the shared `var_names`, which also governs the
  # row-variable name and is honoured by every exporter. FALSE = drop the col side of whatever
  # `var_names` asks for, so the two compose rather than fight.
  if (lifecycle::is_present(col_var_names)) {
    lifecycle::deprecate_soft("2.0.0", "tab_md(col_var_names)", "tab_md(var_names)")
    if (!isTRUE(col_var_names)) {
      var_names <- resolve_export_opts(var_names = var_names)$var_names
      var_names <- if (identical(var_names, "cols")) "none" else
                   if (identical(var_names, "both")) "rows" else var_names
    }
  }
  # Phase 13d: `allow_auto` -- markdown carries a stylesheet (css = TRUE / tab_css()), so it can follow
  # the reader's colour scheme. The spans themselves are theme-independent (only the CSS differs).
  o <- resolve_export_opts(theme = theme, color = color, transpose = transpose,
                           var_names = var_names, allow_auto = TRUE, tabs = tabs)
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

  # WARNING: the POSITION, never imap()'s `i` -- tab_resolve_tables() passes a user's list through
  # untouched, so a NAMED list makes `i` the name and `i == 1` silently FALSE on every table (no
  # error): the caption was dropped. Same trap as xl_check_images().
  parts   <- purrr::map_chr(seq_along(prep$tables), function(i) {
    rd <- prep$tables[[i]]
    # Phase 14w (item 1) / 17b / 17g: user caption= (FIRST table only) -> stored set_caption() ->
    # reg auto-title, via the shared rd_caption().
    cap <- rd_caption(rd, if (i == 1L) caption else NULL)
    md_render_one(rd, special_formatting = special_formatting, wrap_rows = wrap_rows,
                  subtext = subtext, color = color, css = css,
                  color_legend = color_legend, lang = lang,
                  title = cap,
                  theme = theme)
  })
  md_text <- paste(parts, collapse = "\n\n")

  # the observed curves, in a pipe table of their own below the footer -- the same lines the console
  # prints, since a GFM table IS what tab_md() emits. Taken only where the base-count cell cannot
  # carry them (see tab_wants_shape_table).
  if (is_tab(tabs) && tab_wants_shape_table(tabs, "md")) {
    st <- reg_shape_table(tabs)
    if (!is.null(st)) {
      nt <- attr(st, "note")                       # empty wherever no row wears the "ns" mark
      md_text <- paste(c(md_text, "",
                         tx_pipe_table(st, attr(st, "headers"), attr(st, "align")),
                         if (length(nt)) c("", paste0("*", paste(nt, collapse = " "), "*"))),
                       collapse = "\n")
    }
  }

  # Phase 14f/14m-iii: a STYLED table is wrapped in a pandoc fenced div, and (with `css = TRUE`) the
  # stylesheet is prepended. Pandoc emits a BARE `<table>` for a pipe table, which none of tab_css()'s
  # `.tabxplor-tab ...` rules can reach -- so a rendered markdown table got the colours but none of the
  # layout (compact padding, thin spacer columns, the border-taming of 14m-iii). `::: {.tabxplor-tab}`
  # renders as `<div class="tabxplor-tab">`, the hook every selector matches. 14m-iii DECOUPLES the div
  # from `<style>`: a table is styled when it is coloured OR `css = TRUE`, and a styled table always
  # carries the div -- so the "one tab_css() per document" workflow (bring the sheet via the document)
  # reaches a coloured `tab_md(css = FALSE)` too. A plain uncoloured table stays byte-identical (no div).
  # Phase 19h: `roles$has_color` is THE realised "is this table coloured" flag, produced once by
  # roles_color_flags() for the prep AND the transpose (md used to define it a second time, and the
  # transpose a third). `compute` already gates it on the caller's `color`.
  any_color <- any(vapply(prep$tables, function(x) isTRUE(x$roles$has_color), logical(1)))
  styled    <- any_color || isTRUE(css)
  if (styled) md_text <- paste0("::: {.tabxplor-tab}\n", md_text, "\n:::")
  if (isTRUE(css)) {
    md_text <- paste0(tab_css(theme = theme, format = "html", style_tag = TRUE), "\n\n", md_text)
  }

  if (!is.null(file)) writeLines(md_text, file)
  if (clipboard) {
    if (isTRUE(tx_need_pkg("clipr", "Copying to the clipboard", severity = "inform")))
      clipr::write_clip(md_text)
  }
  if (print) {
    cat(md_text, "\n")
    return(invisible(md_text))
  }
  md_text
}


# Phase 20a: `tab_md_css()` is DELETED -- `tab_css(format = "md")` is the same call, and now says so
# in its own name. It existed because `tab_css(chrome = FALSE)` was unguessable, which is a reason to
# fix the argument, not to add a function. It was never released (absent from CRAN 1.3.1), so there
# is nothing to deprecate.


# The DEGRADE table: a frame that cannot be read as a tabxplor table still has to come out as
# markdown, so it comes out as a plain pipe table -- numbers right, everything else left, which is
# the only alignment convention a pipe table has.
md_plain_pipe <- function(df) {
  df   <- as.data.frame(df, stringsAsFactors = FALSE)
  num  <- vapply(df, is.numeric, logical(1))
  body <- lapply(df, function(x) format(x, trim = TRUE, justify = "none"))
  w    <- pmax(nchar(names(df), type = "width"),
               vapply(body, function(x) max(0L, nchar(x, type = "width")), integer(1)))
  pad  <- function(x, i) tx_pad(x, w[i], side = if (num[i]) "left" else "right")
  row  <- function(cells) paste0("|", paste(cells, collapse = "|"), "|")
  rule <- vapply(seq_along(df), function(i)
    if (num[i]) paste0(strrep("-", w[i] - 1L), ":") else paste0(":", strrep("-", w[i] - 1L)),
    character(1))
  lines <- c(row(vapply(seq_along(df), function(i) pad(names(df)[i], i), character(1))), row(rule))
  if (nrow(df))
    lines <- c(lines, vapply(seq_len(nrow(df)), function(r)
      row(vapply(seq_along(df), function(i) pad(body[[i]][r], i), character(1))), character(1)))
  paste(lines, collapse = "\n")
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
    return(md_plain_pipe(rd$tab))
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
  # Phase 17g: rd_footer() folds the shared render_footer(tab_footer_streams(...)) call.
  # Phase 20h: `lang` IS threaded now. It was documented on tab_md() and dropped here -- so
  # tab_md(lang = "fr") rendered an English colour legend, while tab_html() /
  # forest_plot() (which pass it) honoured it. Byte-identical when lang is NULL, which is every
  # golden and every snapshot: NULL means "follow the ambient locale", the former behaviour.
  src         <- if (is.null(rd$color_src)) tabs else rd$color_src
  want_legend <- isTRUE(color) && isTRUE(color_legend) && length(rd$roles$color_cols) != 0
  subtext_text <- rd_footer(src, "md", theme = theme, want_legend = want_legend,
                            subtext = subtext_text, lang = lang)

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
  # Phase 19h: through the SHARED roles_col_var_edges() (tab-export-prep.R). md's convention is the
  # right edge of each block, counting a transition only between two REAL col_vars -- so a helper
  # column (`n`, a total) never opens a span block. Declared there beside kable's and Excel's.
  new_col_var <- if (has_multi_col_vars)
    roles_col_var_edges(col_var_map, other_cols, real_col_vars,
                        side = "right", real_only = TRUE) else integer(0)

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
                        theme = theme, bold_split = TRUE, pad = fig_space,
                        .ref = ann_ref(rd$ann[[nm]]))
      pn      <- attr(raw, "primary_nchar")
      pf      <- attr(raw, "primary_from")
      trimmed <- trimws(raw, which = "left", whitespace = "[\\h\\v]")
      lead    <- nchar(raw) - nchar(trimmed)
      trimmed[is.na(trimmed)] <- ""
      list(txt  = trimmed,
           from = if (is.null(pn)) rep(NA_integer_, length(col)) else pf - lead,
           to   = if (is.null(pn)) rep(NA_integer_, length(col)) else pf + pn - 1L - lead)
    } else {
      # Phase 14f: a `|` in a level or tab_var label would open a spurious cell and desync the whole
      # row's column count. Escape it -- pandoc renders `\|` as a literal pipe inside a cell. Only the
      # non-fmt (label) columns can contain one; fmt cells are numbers the package formats itself.
      list(txt  = gsub("|", "\\|", as.character(col), fixed = TRUE),
           from = rep(NA_integer_, length(col)),
           to   = rep(NA_integer_, length(col)))
    }
  })
  cell_data <- as.data.frame(lapply(fmt_out, `[[`, "txt"), stringsAsFactors = FALSE)
  # the primary token's character RANGE per cell (NA = the whole cell is the primary)
  from_mat  <- do.call(cbind, lapply(fmt_out, `[[`, "from"))
  to_mat    <- do.call(cbind, lapply(fmt_out, `[[`, "to"))

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
  # `styled` (computed here, ahead of the label blanking that needs it): a coloured table, or the caller
  # asked for the stylesheet. In a styled table (rendered to html) a blanked continuation LABEL cell must
  # be a non-breaking space, NOT "" -- an :empty <td> makes the CSS col_var-separator rule misfire (the
  # "ragged" leftmost border that appears only on continuation rows; Phase 18m). Plain tables keep "".
  do_color <- isTRUE(rd$roles$has_color)
  styled   <- do_color || isTRUE(css)
  blank_lbl <- if (styled) "\u00a0" else ""
  for (cl in names(label_cols)) {
    idx <- which(names(cell_data) == cl)
    if (length(idx) != 1) next
    show <- label_runs[[cl]]$show
    cell_data[[idx]][!show] <- blank_lbl
    if (cl %in% names(var_name_col)) {
      nz <- show & nzchar(cell_data[[idx]]) & !is.na(cell_data[[idx]]) & cell_data[[idx]] != blank_lbl
      cell_data[[idx]][nz] <- paste0("*", cell_data[[idx]][nz], "*")
    }
  }

  is_right <- fmt_mask  # named logical: TRUE for fmt (right-aligned) columns

  # Phase 18m: the spacer-column set. Plain / unstyled tables keep ONLY the col_var-group spacers
  # (new_col_var). A STYLED table adds thin spacer columns at the interior boundaries the other exports
  # draw as vertical rules -- between the levels column and the first number, between the last number and
  # the grand Total column, and to the right of the Total column -- reusing the same :empty spacer ->
  # CSS border-left mechanism (so no per-column class is needed). `md_insert_col_sep` inserts a spacer
  # AFTER each index in `sep_after`; `has_sep` enables it (was `has_multi_col_vars`, now any spacer).
  sep_after <- new_col_var
  if (styled) {
    fmt_idx   <- which(unname(fmt_mask))
    tot_idx   <- rd$roles$totcols
    extra     <- integer(0)
    if (length(fmt_idx)) {
      first_fmt <- min(fmt_idx)
      if (first_fmt > 1L) extra <- c(extra, first_fmt - 1L)        # levels | first number
    }
    if (length(tot_idx)) extra <- c(extra, min(tot_idx) - 1L, max(tot_idx))  # numbers|Total, right of Total
    sep_after <- sort(unique(c(new_col_var, extra[extra >= 1L])))
  }
  has_sep <- length(sep_after) > 0L

  # Blank out the label columns' header names (they label sub-tables, not real columns). The `""`
  # sentinel in names(cell_data) is what drives `col_names` at Step 7. Phase 14i: `tab_vars` ->
  # `label_cols`, so a merged table's name column loses the literal "row_var" header here too.
  # WARNING (Phase 19h): this is NOT the prep's header blanking and must not be folded into it. The
  # prep blanks only the LITERAL "row_var" header, keeping a real variable name (`marital`) in
  # cvh$clean for the backends that show it; md renders every label column's name as a body row
  # instead, so it blanks them all. Two rules, deliberately.
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
  # classes, both styled by the one tab_css() stylesheet. `do_color` / `styled` are computed above (the
  # label blanking needs `styled`); the col_var-name span row's blanks get the same nbsp treatment (Step 8).
  # Phase g (A7): in a styled table (pandoc renders it to html), spaces in a multi-word LEVEL / label
  # name ("Never married", "Strong republican") let the host wrap it mid-name. Replace them with a
  # non-breaking space so the label holds on one line up to the wrap_rows truncation limit. nchar is
  # unchanged (U+00A0 is one codepoint), so the raw-text column layout stays byte-identical; a plain
  # (unstyled) table keeps ASCII spaces so its GFM output stays byte-clean.
  if (styled) for (j in other_cols) {
    nz <- nzchar(cell_data[[j]]) & !is.na(cell_data[[j]])
    cell_data[[j]][nz] <- gsub(" ", "\u00a0", cell_data[[j]][nz], fixed = TRUE)
  }
  attr_mat <- NULL
  if (do_color) {
    attr_mat <- matrix("", nrow = nrow(cell_data), ncol = ncol(cell_data))
    for (k in seq_along(fmt_cols)) {
      nm  <- names(fmt_cols)[k]
      j   <- fmt_cols[[k]]
      a   <- rd$ann[[nm]]
      # Phase 19m-i: this site had the `is.null` half of the guard and NOT the length half its two
      # html siblings carry -- a short slot vector indexed past its end yields NA, which
      # md_span_attr() -> tx_slot_class() absorbs into "", i.e. silently uncoloured cells rather
      # than an error. Absent stays a real state (the neutral); short is now a producer bug.
      slot <- function(v) {
        if (is.null(v)) return(integer(nrow(cell_data)))
        stopifnot(length(v) == nrow(cell_data))
        v
      }
      ts  <- slot(a$text_slot)
      bs  <- slot(a$bg_slot)
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
  col_names[names(cell_data) == ""] <- if (styled) "\u00a0" else ""  # nbsp: the blank row_var/tab_var header
  # cell must not be :empty in a styled table, else the thead col_var-separator rule draws a stray left
  # border on it (part of the "first row has many unwanted borders"). Spacer headers stay ASCII (:empty).

  # For each cell, compute the raw text width
  cell_widths <- matrix(0L, nrow = n_rows, ncol = n_cols)
  for (j in seq_len(n_cols)) {
    cell_widths[, j] <- nchar(cell_data[[j]])
  }
  header_widths <- nchar(col_names)
  # THE UNIT ROW IS A HEADER ROW, so it sizes its column like one: its text is written between two
  # emphasis markers, and a "<row% (n)>" tag is regularly wider than the level name above it. Without
  # this the unit cell simply overflowed its column and every pipe below it stepped right.
  if (!is.null(cvh$unit))
    header_widths <- pmax(header_widths, ifelse(nzchar(cvh$unit), nchar(cvh$unit) + 2L, 0L))

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
                                         from_mat[, j], to_mat[, j])
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
  # from/to: the primary token's range in a composite cell (NA = bold the whole cell).
  pad_cell <- function(text, width, is_right, is_bold, from = NA_integer_, to = NA_integer_) {
    if (is_bold && nchar(text) > 0) {
      bold_text <- md_bold(text, from, to)                # partial (composite) or whole-cell bold
      if (is_right) {
        tx_pad(bold_text, width, "left")
      } else {
        tx_pad(bold_text, width, "right")
      }
    } else {
      # Non-bold, or bold with empty text (just pad normally)
      if (is_right) {
        # Right-align: pad text to (width - 2) then add 2 trailing spaces
        paste0(tx_pad(text, width - 2L, "left"), "  ")
      } else {
        # Left-align: 1 leading space + text padded to (width - 2) + 1 trailing space
        paste0(" ", tx_pad(text, width - 2L, "right"), " ")
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
    # Phase 17g: group the spanning col_var-name row by tab_header_runs() (the shared RLE), not a
    # hand-rolled while-scan. pandoc pipe tables cannot colspan, so the md-specific layout stays: the
    # name sits in the FIRST cell of its run (italic), the rest are width-padded blanks -- one cell PER
    # column (a pipe row must keep the table's cell count or pandoc shifts the data). A long name simply
    # overflows its own cell: the row is deliberately not pipe-ALIGNED, because padding to it would
    # widen every column below it.
    # Phase 18m: build the span row as a PER-COLUMN cell vector (the name in the first cell of its
    # run, nbsp-padded blanks elsewhere), then route it through md_insert_col_sep(sep_after) exactly like
    # the body -- so the spacer columns (col_var groups + the interior levels/Total boundaries) line up
    # across every row. (Was a hand-assembled line that only knew the col_var-group spacers.)
    # Phase 19n: a span belonging to a SUB-POPULATION carries it beside the variable. md is the one
    # backend that cannot draw two lines in a cell (a pipe row IS one line), so it composes on ONE:
    # "*2000 marital*" -- the same one-line form fmt_col_block()$label gives the console.
    runs <- tab_header_runs(cvh$label, cvh$group)
    span_cells <- md_pad_blank(col_width, styled)
    col_start  <- 1L
    for (r in seq_along(runs$labels)) {
      if (nzchar(runs$labels[r]))
        span_cells[col_start] <- tx_pad(paste0(" *", fmt_col_block(runs$labels[r], runs$groups[r])$label, "*"), col_width[col_start], "right")
      col_start <- col_start + runs$spans[r]
    }
    col_var_header_line <- md_insert_col_sep(span_cells, sep_after, n_cols, has_sep)
  }

  # THE UNIT ROW (Phase 22c-ii) -- what each column HOLDS ("row%", "row% (n)", "OR (row%)"), the one
  # place an export names the ASIDE of a composite cell. Like the col_var-name row it is a BODY row
  # (a pandoc pipe table takes one header row only) and italic, which is md's own way of saying
  # "supporting text" -- NOT a `.tx-unit` span: a class span costs 13 characters of raw line width
  # that md's fixed-width grid cannot absorb, and a monochrome table must carry no pandoc span at all.
  # It sits directly under the name row, inside the header block the blank underline closes.
  unit_line <- NULL
  if (!is.null(cvh$unit) && any(nzchar(cvh$unit))) {
    unit_cells <- md_pad_blank(col_width, styled)
    for (j in which(nzchar(cvh$unit))) {
      txt <- paste0("*", cvh$unit[j], "*")
      unit_cells[j] <- if (is_right[j])
        paste0(tx_pad(txt, col_width[j] - 2L, "left"), "  ")
      else
        paste0(" ", tx_pad(txt, col_width[j] - 2L, "right"), " ")
    }
    unit_line <- md_insert_col_sep(unit_cells, sep_after, n_cols, has_sep)
  }

  # --- Step 9: Build level-names header row ---
  header_cells <- character(n_cols)
  for (j in seq_len(n_cols)) {
    header_cells[j] <- if (is_right[j]) {
      # Right-aligned header
      paste0(tx_pad(col_names[j], col_width[j] - 2L, "left"), "  ")
    } else {
      # Left-aligned header
      paste0(" ", tx_pad(col_names[j], col_width[j] - 2L, "right"), " ")
    }
  }

  # Insert separator columns between col_var groups
  header_line <- md_insert_col_sep(header_cells, sep_after, n_cols, has_sep)

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
  sep_line <- md_insert_col_sep(sep_cells, sep_after, n_cols, has_sep, fill = "-")

  # --- Step 11: Build body rows ---
  body_lines <- character(n_rows)
  for (i in seq_len(n_rows)) {
    is_bold <- i %in% bold_rows
    row_cells <- character(n_cols)
    for (j in seq_len(n_cols)) {
      pfrom  <- from_mat[i, j]; pto <- to_mat[i, j]      # the primary token's range
      bold_j <- is_bold && !no_bold[j]     # Phase 14i -- see bold_rows_of() above
      if (do_color && is_right[j]) {
        row_cells[j] <- md_color_cell(cell_data[[j]][i], attr_mat[i, j],
                                      num_width[j], col_width[j], bold_j, pfrom, pto,
                                      attr_width = attr_width[j])
      } else {
        row_cells[j] <- pad_cell(cell_data[[j]][i], col_width[j],
                                  is_right[j], bold_j, pfrom, pto)
      }
    }
    body_lines[i] <- md_insert_col_sep(row_cells, sep_after, n_cols,
                                        has_sep)
  }

  # --- Step 12: Insert sub-table separators ---
  if (length(new_group) > 0) {
    # Phase 14m-iii: on the STYLED path a sub-table boundary is a fully-blank row (all cells :empty in
    # the render) that tab_css() collapses to a 1px border-top -- a theme-aware rule with NO dash marker
    # in the raw markdown. The PLAIN path keeps the dash row, so its GFM/text output stays byte-clean.
    if (styled) {
      sep_row <- md_blank_row(col_width, sep_after, n_cols, has_sep)
    } else {
      dash_cells <- character(n_cols)
      for (j in seq_len(n_cols)) {
        dash_cells[j] <- paste0(" ", strrep("-", col_width[j] - 2L), " ")
      }
      sep_row <- md_insert_col_sep(dash_cells, sep_after, n_cols, has_sep)
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
  name_underline <- if (styled && (!is.null(col_var_header_line) || !is.null(unit_line))) {
    md_blank_row(col_width, sep_after, n_cols, has_sep)
  } else NULL
  all_lines <- c(header_line, sep_line, col_var_header_line, unit_line, name_underline, body_lines)

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

# Phase 18m: a width-padded blank cell that is NOT :empty (a leading non-breaking space) for STYLED
# tables, so the CSS col_var-separator rule fires only on the true ASCII spacer columns -- used for the
# nearly-blank col_var-name span row. Plain tables keep ASCII spaces (byte-clean GFM). Vectorised.
md_pad_blank <- function(widths, styled) {
  if (styled) tx_pad("\u00a0", widths, "right") else strrep(" ", widths)
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
md_color_cell <- function(text, attr, num_width, total_width, is_bold, from = NA_integer_,
                          to = NA_integer_, attr_width = nchar(attr)) {
  if (!nzchar(text)) return(strrep(" ", total_width))
  content <- if (is_bold) md_bold(text, from, to) else text   # partial (composite) / whole-cell bold
  # An uncoloured cell uses " " where a coloured one opens its bracket, so a bracket costs no offset.
  open  <- if (nzchar(attr)) "[" else " "
  attr2 <- if (nzchar(attr) && attr_width > nchar(attr)) {
    sub("[}]$", paste0(strrep(" ", attr_width - nchar(attr)), "}"), attr)
  } else attr
  close <- if (nzchar(attr)) paste0("]", attr2) else ""
  # Pad by the cell's own VISIBLE-END width (value + the markup preceding its last visible character),
  # so every cell's last visible character lands on the same raw column. The markup grows leftwards
  # into the pad instead of pushing the value right.
  vis  <- nchar(text) + md_extra(text, is_bold, from, to)
  body <- paste0(strrep(" ", max(0L, num_width - vis)), open, content, close)
  tx_pad(paste0(" ", body), total_width, "right")
}

# How many RAW columns of markup precede a cell's last visible character. md_bold() adds one "**"
# pair around the primary token: the OPENING pair always precedes that character, and the CLOSING one
# does too whenever the primary ends before the text does ("50% (n=10)" -> 4, "(10) 50%" -> 2).
# Vectorised over a column.
#' @keywords internal
md_extra <- function(text, is_bold, from, to) {
  ends_early <- !is.na(to) & to < nchar(text)
  ifelse(!is_bold | !nzchar(text), 0L, ifelse(ends_early, 4L, 2L))
}

# Wrap a cell's PRIMARY token in **...**. For a composite cell only that token is bold and the asides
# beside it stay plain -- on either side, since the primary may be a suffix ("(10) 50%"); a plain cell
# (no recorded range) is bolded whole. Adds exactly one ** pair either way, so the
# +4 width budget the column-width computation reserves for bold cells is unchanged.
# Phase g (A1): the alignment pad (leading/trailing spaces -- incl. the star-placeholder pad a reference
# cell carries, and the figure-space fill) is kept OUTSIDE the ** markers. `**77%   **` is not valid
# markdown bold (pandoc will not open an emphasis span that ends in whitespace); `**77%**   ` is, and the
# outer pad still holds the raw-text column alignment (pandoc trims it at render). See review pass 4.
#' @keywords internal
md_bold <- function(text, from = NA_integer_, to = NA_integer_) {
  # ws = the alignment fillers: ASCII space, no-break U+00A0, figure U+2007, narrow no-break U+202F.
  ws <- paste0("[", intToUtf8(c(32L, 160L, 8199L, 8239L)), "]")
  bold_span <- function(s) {
    if (!nzchar(s)) return(s)
    lead  <- regmatches(s, regexpr(paste0("^", ws, "*"), s))
    trail <- regmatches(s, regexpr(paste0(ws, "*$"), s))
    core  <- substr(s, nchar(lead) + 1L, nchar(s) - nchar(trail))
    if (!nzchar(core)) return(s)                    # all-whitespace: nothing to bold
    paste0(lead, "**", core, "**", trail)
  }
  if (is.na(from) || is.na(to) || from < 1L || (from <= 1L && to >= nchar(text)))
    return(bold_span(text))
  paste0(substr(text, 1L, from - 1L),
         bold_span(substr(text, from, to)),
         substr(text, to + 1L, nchar(text)))
}

# Phase 13d: md_css_rules() / md_css_block() / md_break_class() / md_slot_class_map() are GONE. The
# stylesheet is table-independent (a pure function of palette + theme), so it is generated
# by tab_css() (R/tab-css.R) with no prep walk, no per-column plan and no per-table CSS.
