# PURPOSE: Render-level transpose -- flip a finished render model (roles + per-cell ann + headers),
#   NOT the tabxplor_fmt fields, so `tab_export(transpose = TRUE)` swaps rows and columns AFTER colours
#   and cell strings are computed per (correct, homogeneous) source column.
# ROLE: The `transpose = TRUE` seam for every exporter. tab_export_prep() builds the normal per-table
#   model, then calls tx_transpose_render() on it; the backends render the flipped model.
# KEY CONSTRAINTS:
#   - A transposed column is HETEROGENEOUS (a %, a mean, an n stacked), so it cannot be a tabxplor_fmt
#     column and cannot be re-format()ted. The cell STRINGS (and, for Excel, values + numFmt) are
#     produced here, per ORIGINAL homogeneous column, then flipped as plain data. This is why the
#     object-level tab_transpose() (which copies one column's attributes onto all) mis-coloured numeric
#     cells -- see tab.R and decisions doc S46.
#   - The result is a SYNTHETIC render model: `$tab` is a plain character tibble (correct names/dims),
#     `$transposed = TRUE`, `$cells` holds the pre-formatted strings, and roles/ann/col_var_header are
#     the flipped versions. Every backend reads `rd$cells` when `rd$transposed` is TRUE and otherwise is
#     untouched (non-transposed path byte-identical).
#   - Runs AFTER tab_materialize_extras(backend = "xl"): that keeps `n` as a COLUMN (which flips to an
#     `n` ROW, matching a native pct = "col" table) and 14n has already collapsed the redundant Total
#     rows to one (-> one Total column, no `Total_<var>` suffix). The Excel-only `<var>_sd` sibling is
#     DROPPED here (the mean cell folds its sigma back in via special_formatting).
# See: dev/tabxplor_1.4.0_decisions.md S46.

# === SECTION: the model flip =========================================================================

# tx_transpose_render() -- flip one prep_one_table() result into a transposed synthetic render model.
# `rd`      : a prep_one_table() result (roles + ann + col_var_header + bold_rows + tab).
# `backend` : "kable" | "md" | "plot" | "xl" -- drives the cell-string production only.
# `meta`    : the tab_export_prep() meta (theme_cols); currently unused beyond passthrough.
#' @keywords internal
#' @noRd
tx_transpose_render <- function(rd, backend, meta = NULL) {
  if (isTRUE(rd$vars$degrade)) return(rd)                       # a malformed table degrades unchanged
  # A real tab_vars table (sub-tabled / grouped) is out of scope -- its two-level structure has no
  # single flip. A SEVERAL-row_var (compacted) table is fine: it is the whole point of this phase.
  if (length(rd$vars$tab_vars) > 0) {
    cli::cli_abort(c(
      "{.code transpose = TRUE} does not support tables with {.arg tab_vars}.",
      "i" = "It flips a single- or several-row_var table (no sub-tables)."
    ))
  }

  tab   <- rd$tab
  roles <- rd$roles
  ann   <- rd$ann
  cvh   <- rd$col_var_header
  onm   <- names(tab)
  n_ocol <- length(onm)
  n_orow <- nrow(tab)                              # original rows  -> new COLUMNS
  cvm    <- roles$col_var_map

  # ---- (A) the ORIGINAL data columns become the new ROWS, reordered ------------------------------
  # Drop the Excel-only <var>_sd siblings (the mean cell re-folds its sigma). Order the survivors as
  # the review asks: factor col_var levels, then Total, then n, then numeric means.
  data_i  <- setdiff(unname(roles$fmt_cols), unname(roles$sd_cols))
  is_tot  <- data_i %in% roles$totcols
  is_n    <- unname(cvm[data_i]) %in% "all_col_vars"
  types   <- vapply(data_i, function(j) get_type(tab[[j]]), character(1))
  is_mean <- types %in% "mean" & !is_tot & !is_n
  is_fac  <- !is_tot & !is_n & !is_mean
  order_i <- c(data_i[is_fac], data_i[is_tot], data_i[is_n], data_i[is_mean])
  n_nrow  <- length(order_i)
  onames  <- onm[order_i]

  # ---- (B) new leading label columns, from the ORIGINAL col_var header --------------------------
  # `label` = the col_var NAME per source column (already "" on Total / n); `clean` = its level label.
  name_vals  <- cvh$label[order_i]                 # relig / "" / tvhours
  level_vals <- cvh$clean[order_i]                 # <relig level> / "Total" / "n" / "mean (sd)"
  # Several col_var groups among the rows (>1 real col_var) -> a NAME column + a LEVEL column, like a
  # compacted table's [row_var, levels]. One group -> a single level column headed by the col_var name.
  compacted2 <- length(roles$real_col_vars) > 1

  # ---- (C) new col_var header, spanning the new COLUMNS (= original rows) ------------------------
  # `label'` = the source row_var NAME per original row (marital / race), blank on the Total row(s);
  # `clean'` = that row's level label. From the original label columns (var_name_col + row_var_col).
  row_lvl <- as.character(tab[[roles$row_var_col]])
  if (isTRUE(rd$vars$compacted) && length(roles$var_name_col) == 1) {
    # merged: tab[["row_var"]] values already carry the opt-in label swap (done in prep_one_table).
    src_name <- as.character(tab[[roles$var_name_col]])
  } else {
    # single row_var: its name spans every level column -- swap it for the label (Phase k, display only).
    src_name <- rep(var_label_display(rd$vars$row_var, tab), n_orow)
  }
  is_totrow_o <- seq_len(n_orow) %in% roles$totrows
  src_name[is_totrow_o] <- ""                      # a Total column is standalone, under no group name

  # ---- (D) pre-format the source data columns, then FLIP ----------------------------------------
  # Each source column order_i[k] -> chr[n_orow] (one per original row); the new data column for
  # original row `c` gathers the k-th element of every source column.
  fmted <- tx_format_source_cols(tab, ann, order_i, backend)   # list over new rows k: chr[n_orow] (+ attrs)
  flip_col <- function(get) lapply(seq_len(n_orow), function(c) {
    vapply(seq_len(n_nrow), function(k) get(k)[[c]], character(1))
  })
  cells_data <- flip_col(function(k) fmted$txt[[k]])

  # slots / bold / refs flip the same way (per source column arrays, length n_orow)
  slot_int <- function(field) lapply(seq_len(n_orow), function(c) {
    vapply(seq_len(n_nrow), function(k) {
      v <- ann[[onames[k]]][[field]]
      if (is.null(v) || length(v) != n_orow) 0L else as.integer(v[[c]])
    }, integer(1))
  })
  slot_lgl <- function(field) lapply(seq_len(n_orow), function(c) {
    vapply(seq_len(n_nrow), function(k) {
      v <- ann[[onames[k]]][[field]]
      if (is.null(v) || length(v) != n_orow) FALSE else isTRUE(v[[c]])
    }, logical(1))
  })
  slot_chr <- function(field, default) lapply(seq_len(n_orow), function(c) {
    vapply(seq_len(n_nrow), function(k) {
      v <- ann[[onames[k]]][[field]]
      if (is.null(v) || length(v) != n_orow) default else as.character(v[[c]])
    }, character(1))
  })
  text_slot_d <- slot_int("text_slot")
  bg_slot_d   <- slot_int("bg_slot")
  bold_d      <- slot_lgl("bold")
  refalltot_d <- slot_lgl("ref_alltot")
  # font / back are the RESOLVED per-cell hex (theme grey folded in) -- tab_plot reads these, not slots.
  font_d      <- slot_chr("font", NA_character_)
  back_d      <- slot_chr("back", "none")
  texthex_d   <- slot_chr("text_hex", NA_character_)
  bghex_d     <- slot_chr("bg_hex", NA_character_)
  # a source column's has_color/has_bgc is scalar; a mixed new column has_color = any source cell's
  hascol_src  <- vapply(onames, function(nm) isTRUE(ann[[nm]]$has_color), logical(1))
  hasbgc_src  <- vapply(onames, function(nm) isTRUE(ann[[nm]]$has_bgc),   logical(1))

  # tooltips (kable/html only): built per source column, then flipped
  tips_data <- NULL
  if (identical(backend, "kable")) {
    tips_src <- lapply(order_i, function(j) {
      tp <- tab_kable_print_tooltip(tab[[j]], .ref = ann[[onm[j]]]$ref_cells)
      tp[is.na(tp)] <- ""
      tp
    })
    tips_data <- lapply(seq_len(n_orow), function(c)
      vapply(seq_len(n_nrow), function(k) tips_src[[k]][[c]], character(1)))
  }

  # ---- (E) assemble the synthetic transposed model ----------------------------------------------
  dnames <- make.unique(row_lvl)                   # unique internal keys for the new data columns
  if (compacted2) {
    lead_names <- c("row_var", "levels")
    lead_vals  <- list(name_vals, level_vals)
    lead_clean <- c("", "")                        # blanked leading headers (mirror the compacted case)
    row_var_col_name  <- "levels"
    var_name_col_name <- "row_var"
  } else {
    cvname     <- roles$real_col_vars[[1]]
    lead_names <- cvname
    lead_vals  <- list(level_vals)
    # single label column headed by the col_var name -- shown as the label (Phase k), key stays raw.
    lead_clean <- var_label_display(cvname, tab)
    row_var_col_name  <- cvname
    var_name_col_name <- NULL
  }
  all_names <- c(lead_names, dnames)
  n_lead    <- length(lead_names)
  data_pos  <- n_lead + seq_len(n_orow)            # new positions of the data columns

  # the synthetic char tibble (label columns + the flipped data columns), correct names + dims
  new_tab <- tibble::as_tibble(
    stats::setNames(c(lead_vals, cells_data), all_names), .name_repair = "minimal")

  cells_all <- stats::setNames(c(lead_vals, cells_data), all_names)  # rd$cells: what backends render

  # roles (indices over the new columns / rows) --------------------------------------------------
  fmt_mask <- stats::setNames(seq_along(all_names) %in% data_pos, all_names)
  fmt_cols <- stats::setNames(which(fmt_mask), all_names[fmt_mask])
  other_cols <- stats::setNames(which(!fmt_mask), all_names[!fmt_mask])
  # each new data column belongs to its source row's row_var group (blank on Total)
  n_acol  <- length(all_names)
  col_grp <- rep("", n_acol)
  col_grp[data_pos] <- src_name
  new_totcols <- data_pos[is_totrow_o]                                   # Total row -> Total column
  new_totrows <- which(order_i %in% roles$totcols)                       # Total column -> Total row
  # vertical borders between row_var column-groups (was the ROW-block boundary `new_group`)
  cg <- col_grp; cg[other_cols] <- names(other_cols)
  new_col_var <- which(cg != dplyr::lead(cg, default = "._end") & seq_along(cg) %in% data_pos)
  # horizontal borders between distinct REAL col_var row-groups (mirror of the original's vertical
  # col_var borders). The Total / n / col_pct rows are absorbed into the preceding group -- no separator,
  # so a single-col_var transpose matches a native pct = "col" table (n right after Total, no rule),
  # while a several-col_var one keeps a rule before each new block (e.g. before the numeric means).
  # Phase 17c: the absorbed synthetic columns-turned-rows are the total (roles$totcols) + the add_n /
  # add_pct columns (col_var "all_col_vars") -- both STRUCTURAL. The old `level_vals %in% c("pvalue",
  # "row_pct")` clause was dead here (level_vals is a COLUMN header, never an original row label) and
  # missed col_pct; `row_grp == "all_col_vars"` covers n AND col_pct.
  col_of  <- unname(cvm[order_i])                  # each row's source col_var (STABLE; row_grp is mutated)
  is_addn <- col_of == "all_col_vars"              # the add_n / add_pct columns-turned-rows
  row_grp <- col_of
  absorb  <- (order_i %in% roles$totcols) | is_addn
  row_grp[absorb] <- NA
  for (i in seq_len(n_nrow)[-1]) if (is.na(row_grp[i])) row_grp[i] <- row_grp[i - 1]
  if (is.na(row_grp[1])) row_grp[1] <- "._start"
  new_group <- if (n_nrow > 1) which(row_grp[-n_nrow] != row_grp[-1]) else integer(0)
  # total block (Total row + n row): recompute on the new rows (shared border formula, Phase 17g)
  tot_blk <- seq_len(n_nrow) %in% new_totrows | is_addn
  tb_edges <- roles_totblock_edges(tot_blk)
  totblock_top    <- tb_edges$top
  totblock_bottom <- tb_edges$bottom
  align <- stats::setNames(dplyr::if_else(fmt_mask, "r", "l"), all_names)

  label_names <- if (is.null(var_name_col_name)) character(0) else var_name_col_name
  label_cols  <- stats::setNames(match(label_names, all_names), label_names)
  var_name_col <- label_cols
  label_runs  <- tab_label_runs(new_tab, label_names)

  # ann, keyed by new data-column name -----------------------------------------------------------
  ann_new <- stats::setNames(lapply(seq_len(n_orow), function(c) {
    list(ref_alltot = refalltot_d[[c]],
         ref_cells  = rep(FALSE, n_nrow),
         text_hex   = texthex_d[[c]], bg_hex = bghex_d[[c]],
         text_slot  = text_slot_d[[c]], bg_slot = bg_slot_d[[c]],
         font = font_d[[c]], back = back_d[[c]],
         bold = bold_d[[c]],
         has_color = any(hascol_src),
         has_bgc   = any(hasbgc_src))
  }), dnames)

  # bold rows/cols swap: original bold COLUMNS -> bold ROWS; original bold ROWS -> bold COLUMNS
  bold_rows <- which(onames %in% rd$bold_cols)
  bold_cols <- dnames[seq_len(n_orow) %in% rd$bold_rows]

  # col_var header over the new columns (spanning row_var names + level labels) -------------------
  cvh_label <- character(length(all_names))
  cvh_clean <- character(length(all_names))
  cvh_label[data_pos] <- src_name
  cvh_clean[data_pos] <- row_lvl
  cvh_clean[seq_len(n_lead)] <- lead_clean
  col_var_header <- list(label = cvh_label, clean = cvh_clean)

  has_stars <- isTRUE(roles$has_stars)

  rd2 <- list(
    tab = new_tab,
    transposed = TRUE,
    # the colour legend describes the MEASURES, which live on the original fmt columns (the synthetic
    # `tab` above is plain character) -- so keep the pre-transpose fmt table for tab_color_legend().
    color_src = tab,
    cells = cells_all,
    tooltips = if (!is.null(tips_data)) stats::setNames(tips_data, dnames) else NULL,
    vars = list(degrade = FALSE, row_var = row_var_col_name, tab_vars = character(0),
                row_vars = rd$vars$col_vars, compacted = compacted2,
                col_vars = rd$vars$row_vars, col_vars_levels = list()),
    roles = list(
      fmt_mask = fmt_mask, fmt_cols = fmt_cols, other_cols = other_cols,
      row_var_col = which(all_names == row_var_col_name),
      totcols = new_totcols, totrows = new_totrows,
      no_totrows = setdiff(seq_len(n_nrow), new_totrows),
      totblock_top = totblock_top, totblock_bottom = totblock_bottom,
      real_col_vars = unique(src_name[nzchar(src_name)]),
      col_var_map = stats::setNames(col_grp, all_names),
      new_col_var = new_col_var, new_group = new_group, align = align,
      label_cols = label_cols, var_name_col = var_name_col, label_runs = label_runs,
      sd_cols = integer(0),
      color_cols = fmt_cols[vapply(dnames, function(nm) any(ann_new[[nm]]$text_slot > 0 |
                                                              ann_new[[nm]]$bg_slot > 0), logical(1))],
      any_bg = any(vapply(ann_new, function(a) any(a$bg_slot > 0), logical(1))),
      has_stars = has_stars),
    ann = ann_new,
    bold_rows = bold_rows,
    bold_cols = bold_cols,
    range_totcol = NULL,
    col_var_header = col_var_header,
    subtext = rd$subtext,
    # Phase 17g: carry the caption/title/tips through the flip (previously dropped -> a transposed
    # regression table lost its reg_title / set_caption() caption and its multinomial crude tooltips).
    # These describe the SOURCE table, not the axes, so they survive a transpose unchanged.
    reg_title = rd$reg_title,
    caption = rd$caption,
    empirical_tips = rd$empirical_tips
  )
  rd2
}

# tx_format_source_cols() -- format the source data columns (in row order) with the BACKEND's own call,
# so the transposed strings are identical to what the non-transposed backend would emit for that column.
# Returns list(txt = <list of chr[n_orow] per new row>).
#' @keywords internal
#' @noRd
tx_format_source_cols <- function(tab, ann, order_i, backend) {
  onm <- names(tab)
  txt <- vector("list", length(order_i))
  for (k in seq_along(order_i)) {
    j   <- order_i[k]
    nm  <- onm[j]
    col <- tab[[j]]
    rf  <- ann_ref(ann[[nm]])
    if (identical(backend, "xl")) {
      # Excel v1: a transposed column mixes types (a %, a mean, an n), and one written column is ONE R
      # type, so real per-cell numbers would need a per-cell writer (a large tab_xl change). Write the
      # DISPLAY string (with the sigma folded into the mean cell); colours ride the slot grid. Editable
      # numbers for transposed sheets are deferred -- see decisions doc S46.
      txt[[k]] <- format(col, special_formatting = TRUE, na = "", stars = TRUE, pad = fig_space,
                         .ref = rf)
    } else if (identical(backend, "md")) {
      raw <- format(col, special_formatting = TRUE, na = "", stars = TRUE, bold_split = TRUE,
                    pad = fig_space, .ref = rf)
      txt[[k]] <- stringi::stri_trim(raw, side = "left")        # strip the source column's own pad
      txt[[k]][is.na(txt[[k]])] <- ""
    } else if (identical(backend, "kable")) {
      txt[[k]] <- format(col, html = TRUE, special_formatting = TRUE, na = "", stars = TRUE,
                         bold_split = TRUE, .ref = rf)
    } else {                                                   # plot
      txt[[k]] <- format(col, na = "", stars = TRUE, .ref = rf)
    }
  }
  list(txt = txt)
}
