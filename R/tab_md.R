# PURPOSE: Export tabxplor tables to simple, human-readable markdown format.
# ROLE: Parallel to tab_kable() (HTML) and tab_xl() (Excel) as a lightweight export.
# KEY CONSTRAINTS:
#   - Must not modify any existing code; standalone file.
#   - Padding must be monospace-precise: numbers right-aligned, pipes aligned.
#   - Bold rows (**...**) can touch pipes; normal cells have 1-space margins.
# See: CLAUDE.md § Key Design Decisions > tab_md.

#' Export a tabxplor table to a markdown table
#'
#' @param tabs A table made with \code{\link{tab}} or \code{\link{tab_many}},
#'   or a `list` of tab with the same `col_vars` and no `tab_vars`.
#' @param bold_references Bold reference/total rows with markdown `**...**`.
#' @param special_formatting Passed to \code{\link[=format.tabxplor_fmt]{format()}}.
#'   When `TRUE`, shows "ref:" prefix on diff reference cells, "mean:" on ctr
#'   totals, sigma on means.
#' @param wrap_rows Max width for row labels before truncation.
#' @param subtext Print chi2/footnotes below the table.
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
#'   mutate(across(where(is_fmt), ~set_display(., "diff"))) |> 
#'   tab_md()
#' }
tab_md <- function(tabs,
                   bold_references = TRUE,
                   special_formatting = TRUE,
                   wrap_rows = 50,
                   subtext = TRUE,
                   clipboard = FALSE,
                   file = NULL,
                   print = TRUE) {

  # --- Step 1: Handle list input + compact ---
  if (is.list(tabs) & !is.data.frame(tabs)) {
    same_col_vars <- purrr::map(tabs, ~ tab_get_vars(.)$col_vars)
    same_col_vars <- same_col_vars |>
      purrr::map(~ .[!. %in% c("all_col_vars", "", "no") & !is.na(.)])
    longest_col_vars <- purrr::map_int(same_col_vars, length)
    longest_col_vars <-
      dplyr::first(which(longest_col_vars == max(longest_col_vars, na.rm = TRUE)))
    longest_col_vars <- same_col_vars[[longest_col_vars]]
    same_col_vars <- same_col_vars |> purrr::map_lgl(~ all(. %in% longest_col_vars))
    if (!all(same_col_vars)) {
      stop("tab_md() can only be used with a list of tab if they have the same col_vars")
    }
    if (any(purrr::map_lgl(tabs, ~ length(tab_get_vars(.)$tab_vars) > 0))) {
      stop("tab_md() can only be used with a list of tab if they have no tab_vars")
    }
    tabs <- tab_compact(tabs)
  }

  # --- Step 2: Extract metadata ---
  tab_vars <- tab_get_vars(tabs)$tab_vars
  subtext_text <- if (subtext) {
    get_subtext(tabs) |> purrr::discard(\(x) x == "")
  } else {
    character(0)
  }

  # --- Step 3: Compute group boundaries ---
  new_group <- dplyr::group_indices(tabs)
  new_group <- which(new_group != dplyr::lead(new_group, default = max(new_group) + 1L))
  # Remove the very last row from separators (no separator after last row)
  new_group <- new_group[new_group < nrow(tabs)]

  tabs <- tabs |> dplyr::ungroup()

  # --- Step 4: Identify column roles ---
  row_var_name <- tab_get_vars(tabs)$row_var
  fmt_mask  <- purrr::map_lgl(tabs, is_fmt)
  fmt_cols  <- which(fmt_mask)
  other_cols <- which(!fmt_mask)

  # Detect real col_var groups (excluding "all_col_vars", "", "no", NA)
  col_var_map <- get_col_var(tabs)
  real_col_vars <- unique(col_var_map[fmt_mask])
  real_col_vars <- real_col_vars[!real_col_vars %in%
                                   c("all_col_vars", "", "no", NA_character_)]
  has_multi_col_vars <- length(real_col_vars) > 1

  # Positions where real col_var changes (for separator columns)
  new_col_var <- integer(0)
  if (has_multi_col_vars) {
    # Build simplified col_var map: group non-real col_vars with neighbours
    cv_simplified <- col_var_map
    cv_simplified[names(other_cols)] <- names(other_cols)
    # Mark transitions between real col_var groups only
    for (k in seq_along(cv_simplified)[-1]) {
      prev_cv <- cv_simplified[k - 1]
      curr_cv <- cv_simplified[k]
      if (prev_cv %in% real_col_vars && curr_cv %in% real_col_vars &&
          prev_cv != curr_cv) {
        new_col_var <- c(new_col_var, k - 1L)
      }
    }
  }

  # --- Step 5: Identify bold rows ---
  if (bold_references && length(fmt_cols) > 0) {
    refref <- purrr::map_dfr(tabs[fmt_cols],
                             \(x) get_reference(x, mode = "all_totals"))
    # Keep only columns with mixed TRUE/FALSE
    keep <- purrr::map_lgl(refref, \(x) any(x) & !all(x))
    if (any(keep)) {
      refref <- refref[, keep, drop = FALSE]
      bold_rows <- which(rowSums(refref) == ncol(refref))
    } else {
      bold_rows <- integer(0)
    }
  } else {
    bold_rows <- integer(0)
  }

  # --- Step 6: Format all cells to character ---
  # Format fmt columns
  cell_data <- purrr::map(tabs, \(col) {
    if (is_fmt(col)) {
      out <- format(col, special_formatting = special_formatting, na = "") |>
        stringr::str_trim()
      out[is.na(out)] <- ""
      out
    } else if (is.factor(col)) {
      as.character(col)
    } else {
      as.character(col)
    }
  })
  cell_data <- as.data.frame(cell_data, stringsAsFactors = FALSE)

  # Truncate row labels
  for (j in other_cols) {
    cell_data[[j]] <- stringr::str_trunc(cell_data[[j]], wrap_rows)
  }

  # For tables with tab_vars or compact tables: blank out grouping columns
  # except first row of each group (show label only once per sub-table)
  if (length(tab_vars) > 0) {
    for (tv in tab_vars) {
      tv_idx <- which(names(cell_data) == tv)
      if (length(tv_idx) == 1) {
        vals <- cell_data[[tv_idx]]
        for (i in seq_along(vals)) {
          if (i > 1 && vals[i] == vals[i - 1]) {
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
  col_width <- integer(n_cols)
  for (j in seq_len(n_cols)) {
    margin <- if (is_right[j]) 3L else 2L
    widths <- cell_widths[, j] + margin  # normal cells
    if (length(bold_rows) > 0) {
      widths[bold_rows] <- cell_widths[bold_rows, j] + 4L  # bold cells
    }
    col_width[j] <- max(c(widths, header_widths[j] + 2L))
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
      row_cells[j] <- pad_cell(cell_data[[j]][i], col_width[j],
                                is_right[j], is_bold)
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

  if (length(subtext_text) > 0) {
    all_lines <- c(all_lines, "", subtext_text)
  }

  md_text <- paste(all_lines, collapse = "\n")

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
    invisible(md_text)
  } else {
    md_text
  }
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
