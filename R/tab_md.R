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
#' @param theme,color_type,html_24_bit Colour palette selectors (as in
#'   \code{\link[=tab_kable]{tab_kable()}}); they only affect the CSS emitted by `css = TRUE` /
#'   \code{\link{tab_md_css}}, since the span *class names* are palette-independent.
#' @param caption Optional table caption, rendered as a pandoc caption line `: caption` (captions only
#'   the first table of a list).
#' @param transpose Set to `TRUE` to transpose each table before export (rows become columns) --
#'   the col-percentages-with-several-row-variables use case.
#' @param title `r lifecycle::badge("deprecated")` Renamed to `caption`.
#' @param css When `TRUE`, prepend an inline `<style>` block (from \code{\link{tab_md_css}}) matching
#'   this table's breaks and palette, so the coloured markdown is self-contained. Default `FALSE`
#'   (bring the stylesheet via the document's `css:` instead).
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
                   theme = c("light", "dark"),
                   color_type = NULL,
                   html_24_bit = NULL,
                   caption = NULL,
                   transpose = FALSE,
                   css = FALSE,
                   clipboard = FALSE,
                   file = NULL,
                   print = TRUE,
                   title = lifecycle::deprecated()) {
  # Phase 13a: install a per-table color_breaks override for the render (no-op otherwise).
  .cb <- push_color_breaks(tabs); on.exit(pop_color_breaks(.cb), add = TRUE)
  # Phase 10j: `title` renamed to `caption` (unified across exporters); `transpose` added.
  if (lifecycle::is_present(title)) {
    lifecycle::deprecate_soft("1.4.0", "tab_md(title)", "tab_md(caption)")
    caption <- title
  }
  # `html_24_bit` is inert (Phase 13a): markdown colour spans map to CSS classes, always 24-bit.
  o <- resolve_export_opts(theme, color_type, color, transpose = transpose)
  theme <- o$theme; color_type <- o$color_type; color <- o$color

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
                          compute = compute, transpose = o$transpose, color_type = color_type,
                          theme = theme, list_method = TRUE,
                          what = "tab_md()")

  parts   <- purrr::imap_chr(prep$tables, function(rd, i) {
    md_render_one(rd, special_formatting = special_formatting, wrap_rows = wrap_rows,
                  subtext = subtext, color = color,
                  color_legend = color_legend, lang = lang,
                  title = if (i == 1) caption else NULL,
                  color_type = color_type, theme = theme)
  })
  md_text <- paste(parts, collapse = "\n\n")

  if (isTRUE(css)) {
    md_text <- paste0(tab_md_css(tabs, theme = theme, color_type = color_type,
                                 style_tag = TRUE),
                      "\n\n", md_text)
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
#' Generates the CSS that styles the pandoc bracketed-span classes \code{\link{tab_md}} emits for a
#' coloured table (`[value]{.p20}` etc.). The classes are **break-derived** so they are readable and
#' match *this* table: additive percentage-point diffs are `p5`/`p10`/... (over-represented) and
#' `m5`/`m10`/... (under-represented); standardized mean diffs are `sd0_2`/`sdm0_2`/...; ratios / odds
#' ratios / the `x2` rule are `x2`/`x1_5`/... and `d2`/`d1_5`/...; contribution levels fall back to
#' `b1`/`bm1`/.... Background-channel classes are the same names prefixed `bg`. The neutral class `.n`
#' (uncoloured cells) is left unstyled (inherits the normal text colour).
#'
#' @param tabs The same table (or list of tables) passed to \code{\link{tab_md}}.
#' @param theme,color_type,html_24_bit Palette selectors (as in \code{\link[=tab_kable]{tab_kable()}}).
#' @param dark_mode When `"media"` (default) and `theme = "light"`, append a
#'   `@media (prefers-color-scheme: dark)` block from the dark palette so the table follows the
#'   viewer's colour scheme. `"none"` emits the single theme only.
#' @param style_tag Wrap the CSS in a `<style>...</style>` tag (for embedding, as `tab_md(css = TRUE)`
#'   does). Default `FALSE` returns bare CSS.
#' @param file Optional path to write the CSS to; returns the string invisibly when given.
#'
#' @return A character string of CSS (visible, or invisible when `file` is given).
#' @export
#'
#' @examples
#' \donttest{
#' tabs <- tab(forcats::gss_cat, race, marital, pct = "row", color = "diff")
#' cat(tab_md_css(tabs))
#' }
tab_md_css <- function(tabs,
                       theme = c("light", "dark"),
                       color_type = NULL, html_24_bit = NULL,
                       dark_mode = c("media", "none"),
                       style_tag = FALSE, file = NULL) {
  theme       <- match.arg(theme)
  dark_mode   <- match.arg(dark_mode)
  color_type  <- if (is.null(color_type )) getOption("tabxplor.color_style_type" ) else color_type

  css <- md_css_block(md_css_rules(tabs, color_type, theme))

  if (dark_mode == "media" && theme == "light") {
    dark <- md_css_block(md_css_rules(tabs, color_type, "dark"), indent = "  ")
    if (nzchar(dark)) {
      css <- paste0(if (nzchar(css)) paste0(css, "\n") else "",
                    "@media (prefers-color-scheme: dark) {\n", dark, "\n}")
    }
  }

  if (isTRUE(style_tag)) css <- paste0("<style>\n", css, "\n</style>")
  if (!is.null(file)) {
    writeLines(css, file)
    return(invisible(css))
  }
  css
}


# Render ONE prepared table (`rd`, from tab_export_prep) to a markdown string (no file/clipboard/print
# -- tab_md() joins the parts and handles those). Holds the md-specific rendering (Steps 4-13).
# Phase 10f: when the table carries colours and `color = TRUE`, every fmt cell is wrapped in a pandoc
# bracketed span `[<num>]{.class}` (break-derived class, uncoloured cells get the neutral `.n`); the
# uniform scaffold keeps the numbers aligned in raw text. Uncoloured tables (or color = FALSE) render
# the byte-identical plain padded table.
md_render_one <- function(rd, special_formatting, wrap_rows, subtext,
                          color = TRUE, color_legend = TRUE, lang = NULL, title = NULL,
                          color_type = NULL, theme = NULL) {
  # Graceful degrade -- a table that can't be read as a tabxplor table renders as a plain pipe table.
  if (isTRUE(rd$vars$degrade)) {
    tab_degrade_inform(rd$vars$reason)
    return(paste(knitr::kable(tibble::as_tibble(rd$tab), format = "pipe"), collapse = "\n"))
  }

  tabs         <- rd$tab
  tab_vars     <- rd$vars$tab_vars
  subtext_text <- if (subtext) rd$subtext else character(0)

  # Phase 13b: prepend the colour legend as a prose line, its break-words wrapped in the SAME pandoc
  # span classes the cells use (md_break_class-consistent -> tab_md_css() colours them). Only when
  # coloured (a legend describes the colours). Prepended above the user subtext.
  if (isTRUE(color) && isTRUE(color_legend) && length(rd$roles$color_cols) != 0) {
    leg <- suppressWarnings(tab_color_legend(tabs, medium = "md", style = "prose", lang = lang,
                                             color_type = color_type, theme = theme))
    if (length(leg)) subtext_text <- c(leg, subtext_text)
  }

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
  cell_data <- purrr::imap(tabs, \(col, nm) {
    if (is_fmt(col)) {
      # stars = TRUE: main display. When a column carries significance stars, format() right-pads the
      # star field so numbers stay aligned; trim ONLY the leading side to preserve that trailing pad
      # (byte-identical when no star is present -- format() emits no trailing space otherwise).
      out <- format(col, special_formatting = special_formatting, na = "", stars = TRUE,
                    .ref = ann_ref(rd$ann[[nm]])) |>
        stringr::str_trim(side = "left")
      out[is.na(out)] <- ""
      out
    } else if (is.factor(col)) {
      as.character(col)
    } else {
      as.character(col)
    }
  })
  cell_data <- as.data.frame(cell_data, stringsAsFactors = FALSE)

  # Truncate row labels (10f: only when wrap_rows is set; default NULL = lossless, column grows).
  # A pipe cell cannot hold a raw newline, so md "wrap" means "do not truncate by default".
  if (!is.null(wrap_rows)) {
    for (j in other_cols) {
      cell_data[[j]] <- stringr::str_trunc(cell_data[[j]], wrap_rows)
    }
  }

  # For tables with tab_vars or compact tables: blank out grouping columns
  # except first row of each group (show label only once per sub-table)
  if (length(tab_vars) > 0) {
    for (tv in tab_vars) {
      tv_idx <- which(names(cell_data) == tv)
      if (length(tv_idx) == 1) {
        vals <- cell_data[[tv_idx]]
        for (i in seq_along(vals)) {
          # blank a tab_var cell that repeats the previous label OR is NA (a continuation row such as
          # a materialised p-value line, which belongs to the preceding sub-table) -- NA-safe so the
          # `if` never receives a missing value (kable already tolerates these rows).
          if (i > 1 && (is.na(vals[i]) || isTRUE(vals[i] == vals[i - 1]))) {
            cell_data[[tv_idx]][i] <- ""
          }
        }
      }
    }
  }

  is_right <- fmt_mask  # named logical: TRUE for fmt (right-aligned) columns

  # Blank out tab_vars header names (they label sub-tables, not real columns)
  if (length(tab_vars) > 0) {
    for (tv in tab_vars) {
      tv_idx <- which(names(cell_data) == tv)
      if (length(tv_idx) == 1) names(cell_data)[tv_idx] <- ""
    }
  }

  # --- Step 6b (Phase 10f): per-cell pandoc span attributes (colour) ---
  # A table is "coloured" iff some fmt column carries a colour measure. In that mode EVERY fmt cell is
  # wrapped (uncoloured cells get the neutral `.n`) so numbers stay aligned; an uncoloured table keeps
  # the byte-identical plain path. attr_mat holds the per-cell "{.class}" string (fmt columns only).
  do_color <- isTRUE(color) && length(rd$ann) > 0L &&
    any(vapply(rd$ann, function(a) isTRUE(a$has_color), logical(1)))
  attr_mat <- NULL
  if (do_color) {
    attr_mat <- matrix("", nrow = nrow(cell_data), ncol = ncol(cell_data))
    for (k in seq_along(fmt_cols)) {
      nm  <- names(fmt_cols)[k]
      j   <- fmt_cols[[k]]
      a   <- rd$ann[[nm]]
      ts  <- if (!is.null(a$text_slot)) a$text_slot else integer(nrow(cell_data))
      bs  <- if (!is.null(a$bg_slot))   a$bg_slot   else integer(nrow(cell_data))
      tmap <- md_slot_class_map(tabs[[j]], "text", color_type, theme)
      bmap <- md_slot_class_map(tabs[[j]], "bg",   color_type, theme)
      attr_mat[, j] <- vapply(seq_len(nrow(cell_data)),
                              function(i) md_span_attr(ts[i], bs[i], tmap, bmap),
                              character(1))
    }
  }

  # --- Step 7: Compute column widths ---
  n_rows <- nrow(cell_data)
  n_cols <- ncol(cell_data)
  col_names <- names(cell_data)

  # For each cell, compute the raw text width
  cell_widths <- matrix(0L, nrow = n_rows, ncol = n_cols)
  for (j in seq_len(n_cols)) {
    cell_widths[, j] <- nchar(cell_data[[j]])
  }
  header_widths <- nchar(col_names)

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
      w <- cell_widths[, j]
      if (length(bold_rows) > 0) w[bold_rows] <- cell_widths[bold_rows, j] + 4L
      nonempty <- nzchar(cell_data[[j]])
      num_width[j]  <- if (any(nonempty)) max(w[nonempty]) else 0L
      attr_width[j] <- if (any(nonempty)) max(nchar(attr_mat[nonempty, j])) else 0L
      col_width[j]  <- max(num_width[j] + attr_width[j] + 4L, header_widths[j] + 2L)
    } else {
      margin <- if (is_right[j]) 3L else 2L
      widths <- cell_widths[, j] + margin  # normal cells
      if (length(bold_rows) > 0) {
        widths[bold_rows] <- cell_widths[bold_rows, j] + 4L  # bold cells
      }
      col_width[j] <- max(c(widths, header_widths[j] + 2L))
    }
  }

  # --- Helper: pad a cell ---
  # is_right: TRUE for fmt (right-aligned), FALSE for text (left-aligned)
  # is_bold: TRUE to wrap with **
  pad_cell <- function(text, width, is_right, is_bold) {
    if (is_bold && nchar(text) > 0) {
      bold_text <- paste0("**", text, "**")
      if (is_right) {
        stringr::str_pad(bold_text, width, side = "left")
      } else {
        stringr::str_pad(bold_text, width, side = "right")
      }
    } else {
      # Non-bold, or bold with empty text (just pad normally)
      if (is_right) {
        # Right-align: pad text to (width - 2) then add 2 trailing spaces
        paste0(stringr::str_pad(text, width - 2L, side = "left"), "  ")
      } else {
        # Left-align: 1 leading space + text padded to (width - 2) + 1 trailing space
        paste0(" ", stringr::str_pad(text, width - 2L, side = "right"), " ")
      }
    }
  }

  # --- Step 8: Build col_var header row (only if multiple col_vars) ---
  col_var_header_line <- NULL
  if (has_multi_col_vars) {
    header_parts <- character(0)
    j <- 1
    while (j <= n_cols) {
      cv <- col_var_map[j]
      if (cv %in% real_col_vars) {
        # Group consecutive columns with the same real col_var
        j_end <- j
        while (j_end < n_cols && col_var_map[j_end + 1] == cv) {
          j_end <- j_end + 1
        }
        group_cols <- j:j_end
        span <- sum(col_width[group_cols]) + length(group_cols) - 1
        # Center the col_var name over its group
        label <- cv
        header_parts <- c(header_parts,
                          stringr::str_pad(
                            stringr::str_pad(label,
                                             nchar(label) + (span - nchar(label)) %/% 2,
                                             side = "left"),
                            span, side = "right"))
        # Add separator column between real col_var groups
        if (j_end %in% new_col_var && j_end < n_cols) {
          header_parts <- c(header_parts, " ")
        }
        j <- j_end + 1
      } else {
        # Non-grouped column: empty cell matching column width
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
      paste0(stringr::str_pad(col_names[j], col_width[j] - 2L, side = "left"), "  ")
    } else {
      # Left-aligned header
      paste0(" ", stringr::str_pad(col_names[j], col_width[j] - 2L, side = "right"), " ")
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
  sep_line <- md_insert_col_sep(sep_cells, new_col_var, n_cols, has_multi_col_vars)

  # --- Step 11: Build body rows ---
  body_lines <- character(n_rows)
  for (i in seq_len(n_rows)) {
    is_bold <- i %in% bold_rows
    row_cells <- character(n_cols)
    for (j in seq_len(n_cols)) {
      if (do_color && is_right[j]) {
        row_cells[j] <- md_color_cell(cell_data[[j]][i], attr_mat[i, j],
                                      num_width[j], col_width[j], is_bold)
      } else {
        row_cells[j] <- pad_cell(cell_data[[j]][i], col_width[j],
                                  is_right[j], is_bold)
      }
    }
    body_lines[i] <- md_insert_col_sep(row_cells, new_col_var, n_cols,
                                        has_multi_col_vars)
  }

  # --- Step 12: Insert sub-table separators ---
  if (length(new_group) > 0) {
    # Build separator line with dashes matching column widths
    dash_cells <- character(n_cols)
    for (j in seq_len(n_cols)) {
      dash_cells[j] <- paste0(" ", strrep("-", col_width[j] - 2L), " ")
    }
    dash_line <- md_insert_col_sep(dash_cells, new_col_var, n_cols,
                                    has_multi_col_vars)

    # Insert separators after the appropriate rows (in reverse to preserve indices)
    result_lines <- character(0)
    prev <- 1
    for (g in new_group) {
      result_lines <- c(result_lines, body_lines[prev:g], dash_line)
      prev <- g + 1
    }
    if (prev <= n_rows) {
      result_lines <- c(result_lines, body_lines[prev:n_rows])
    }
    body_lines <- result_lines
  }

  # --- Step 13: Assemble and output ---
  all_lines <- c(col_var_header_line, header_line, sep_line, body_lines)

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
md_insert_col_sep <- function(cells, new_col_var, n_cols, has_multi_col_vars) {
  if (!has_multi_col_vars || length(new_col_var) == 0) {
    return(paste0("|", paste(cells, collapse = "|"), "|"))
  }

  parts <- character(0)
  for (j in seq_along(cells)) {
    parts <- c(parts, cells[j])
    if (j %in% new_col_var && j < n_cols) {
      parts <- c(parts, " ")  # empty separator column
    }
  }
  paste0("|", paste(parts, collapse = "|"), "|")
}


# === SECTION: Phase 10f colour spans (break-derived pandoc classes) ==================

# A colour break -> a concise, CSS-legal class name (classes cannot start with a digit / contain
# +,.,/). Encodes direction + the actual break, so a reader knows what threshold a cell crossed:
#   additive pct diff  : p5/p10/p20/p30  (over) , m5/m10/..  (under)      [break*100 pct-points]
#   additive mean diff : sd0_2/sd0_5/..  (over) , sdm0_2/..   (under)      [Glass's delta, sd units]
#   multiplicative     : x2/x1_5/x1_15   (over) , d2/d1_5/..  (under)      [ratio / OR / the x2 rule]
#   contrib (fallback) : b1/b2/b3/b4     (over) , bm1/bm2/..  (under)      [signed break index]
# `dir` is +1 (over-represented) / -1 (under-represented); `level` the break rank (for the fallback).
#' @keywords internal
md_break_class <- function(measure, center, is_mean, brk, dir, level) {
  if (identical(measure, "contrib")) {
    return(paste0(if (dir > 0L) "b" else "bm", level))
  }
  if (center == 1) {                       # multiplicative (ratio / or / x2)
    v <- gsub("[.]", "_", format(brk, trim = TRUE, scientific = FALSE))
    return(paste0(if (dir > 0L) "x" else "d", v))
  }
  if (isTRUE(is_mean)) {                    # additive, sd-standardized (Glass's delta)
    v <- gsub("[.]", "_", format(brk, trim = TRUE, scientific = FALSE))
    return(paste0(if (dir > 0L) "sd" else "sdm", v))
  }
  paste0(if (dir > 0L) "p" else "m", as.character(round(brk * 100)))   # additive pct-points
}

# For one fmt column + one channel, the map slot-integer (0..8) -> {class name, hex}. Reuses the
# engine (fmt_color_plan gives measure/center + the per-side breaks + palette slots), so md never forks
# the colour math -- it only names the slots the engine already assigns. Returns two length-9 vectors
# indexed by `slot + 1L` (slot 0 = uncoloured -> "" / NA; over 1:4, under 5:8). Background-channel
# names are prefixed "bg" (a distinct CSS class -> the background palette). `class` drives the span;
# `hex` drives tab_md_css().
#' @keywords internal
md_slot_class_map <- function(col, channel = c("text", "bg"),
                              color_type = NULL, theme = NULL) {
  channel <- match.arg(channel)
  cls <- rep("",            9L)
  hex <- rep(NA_character_, 9L)

  color <- if (channel == "text") get_color(col) else get_color_bg(col)
  plan  <- fmt_color_plan(col, channel, color = color)
  if (is.null(plan)) return(list(class = cls, hex = hex))

  is_mean <- get_type(col) %in% c("mean", "n")
  palette <- get_color_style("color_code",
                             type = if (channel == "text") color_type else "bg",
                             theme = theme)

  ov_lv     <- seq_along(plan$over_breaks)
  un_lv     <- seq_along(plan$under_breaks)
  pos_names <- vapply(ov_lv, function(l) md_break_class(plan$measure, plan$center, is_mean,
                                                        plan$over_breaks[l], 1L, l), character(1))
  neg_names <- vapply(un_lv, function(l) md_break_class(plan$measure, plan$center, is_mean,
                                                        plan$under_breaks[l], -1L, l), character(1))
  slots  <- c(plan$over_slots[ov_lv + 1L], plan$under_slots[un_lv + 1L])
  names_ <- c(pos_names, neg_names)
  keep   <- slots > 0L
  slots  <- slots[keep]
  names_ <- names_[keep]
  if (channel == "bg") names_ <- paste0("bg", names_)

  cls[slots + 1L] <- names_
  hex[slots + 1L] <- toupper(unname(palette[slots]))
  list(class = cls, hex = hex)
}

# The pandoc bracketed-span attribute for ONE cell: "{.p20 .bgx2}" / "{.p20}" / "{.bgx2}" / "{.n}"
# (both channels uncoloured -> the neutral uniform-span class, so numbers still align in raw text).
#' @keywords internal
md_span_attr <- function(text_slot, bg_slot, tmap, bmap, neutral = "n") {
  parts <- character(0)
  tc <- tmap$class[text_slot + 1L]
  if (nzchar(tc)) parts <- c(parts, paste0(".", tc))
  bc <- bmap$class[bg_slot + 1L]
  if (nzchar(bc)) parts <- c(parts, paste0(".", bc))
  if (length(parts) == 0L) parts <- paste0(".", neutral)
  paste0("{", paste(parts, collapse = " "), "}")
}

# One coloured fmt cell as the fixed scaffold " [<num>]<attr> " padded to `total_width`. The number is
# right-padded to `num_width` INSIDE the brackets, so `[` and the number sit at a fixed offset every row
# (numbers align); the whole body is right-padded so the next pipe lands at a fixed column (pipes align).
# An empty/NA cell renders as blank of the same width (no empty span).
#' @keywords internal
md_color_cell <- function(text, attr, num_width, total_width, is_bold) {
  if (!nzchar(text)) return(strrep(" ", total_width))
  content <- if (is_bold) paste0("**", text, "**") else text
  body    <- paste0("[", stringr::str_pad(content, num_width, side = "left"), "]", attr)
  paste0(" ", stringr::str_pad(body, total_width - 1L, side = "right"))
}

# The distinct span class -> {property, hex} rules for a table (or list), for tab_md_css(). Reuses the
# shared prep + the same per-column slot maps the spans use, so the CSS can never disagree with the
# cells. Text-channel classes -> `color`; background-channel classes -> `background-color`.
#' @keywords internal
md_css_rules <- function(tabs, color_type, theme) {
  prep <- tab_export_prep(tabs, backend = "md", drop_tab_vars = FALSE, wrap = NULL,
                          compute = c("refs", "colors"), color_type = color_type, theme = theme,
                          list_method = TRUE, what = "tab_md_css()")
  class <- character(0)
  prop  <- character(0)
  hex   <- character(0)
  for (rd in prep$tables) {
    if (isTRUE(rd$vars$degrade)) next
    for (j in rd$roles$fmt_cols) {
      col  <- rd$tab[[j]]
      tmap <- md_slot_class_map(col, "text", color_type, theme)
      bmap <- md_slot_class_map(col, "bg",   color_type, theme)
      tsel <- nzchar(tmap$class)
      bsel <- nzchar(bmap$class)
      class <- c(class, tmap$class[tsel], bmap$class[bsel])
      prop  <- c(prop, rep("color", sum(tsel)), rep("background-color", sum(bsel)))
      hex   <- c(hex, tmap$hex[tsel], bmap$hex[bsel])
    }
  }
  keep <- !duplicated(class)
  class <- class[keep]
  prop  <- prop[keep]
  hex   <- hex[keep]
  ord   <- order(class)
  list(class = class[ord], prop = prop[ord], hex = hex[ord])
}

# Assemble CSS rule lines from md_css_rules() output (empty string when no coloured class).
#' @keywords internal
md_css_block <- function(rules, indent = "") {
  if (length(rules$class) == 0L) return("")
  paste0(indent, ".", rules$class, " { ", rules$prop, ": ", rules$hex, "; }", collapse = "\n")
}
