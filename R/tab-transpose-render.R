# PURPOSE: Render-level transpose -- flip a finished render model (roles + per-cell ann + headers),
#   NOT the tabxplor_fmt fields, so `tab_export(transpose = TRUE)` swaps rows and columns AFTER colours
#   and cell strings are computed per (correct, homogeneous) source column.
# ROLE: The `transpose = TRUE` seam for every exporter. tab_export_prep() builds the normal per-table
#   model, then calls tx_transpose_render() on it; the backends render the flipped model.
# KEY CONSTRAINTS:
#   - THE UNIT LINE TURNS WITH THE AXES: a transposed data column holds one original ROW across every
#     original column, so it is named only where those columns agreed on one name -- and row% becomes
#     col% (tx_flip_pct_label), which is what keeps a transposed row% table rendering EXACTLY like a
#     native col% one. The original Total columns are excluded: after the flip they are the Total ROW.
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
#     rows to one (-> one Total column, no `Total_<var>` suffix). ⚠ The Excel ASIDE split does NOT run
#     under a transpose: every backend gets a formatted string here, so a composite cell survives the
#     flip whole ("49 (sigma17)") and splitting it would strip the aside off.
# See: dev/tabxplor_2.0.0_decisions.md S46.

# === SECTION: the model flip =========================================================================

# tx_transpose_render() -- flip one prep_one_table() result into a transposed synthetic render model.
# `rd`      : a prep_one_table() result (roles + ann + col_var_header + bold_rows + tab).
# `backend` : "kable" | "md" | "plot" | "xl" -- drives the cell-string production only.
# `meta`    : the tab_export_prep() meta (theme_cols); currently unused beyond passthrough.
#' @keywords internal
#' @noRd
tx_transpose_render <- function(rd, backend) {
  if (isTRUE(rd$vars$degrade)) return(rd)                       # a malformed table degrades unchanged
  # A real tab_vars table (sub-tabled / grouped) is out of scope -- its two-level structure has no
  # single flip. A SEVERAL-row_var (compacted) table is fine: it is the whole point of this phase.
  # Phase 19h (KEY 7): declared in TAB_OPS (R/tab-structure.R), read here through the render model's own
  # variable block, so the rule and its wording live with every other shape rule.
  tab_check_structure(rd_structure(rd), "transpose_render")

  tab   <- rd$tab
  roles <- rd$roles
  ann   <- rd$ann
  cvh   <- rd$col_var_header
  onm   <- names(tab)
  n_ocol <- length(onm)
  n_orow <- nrow(tab)                              # original rows  -> new COLUMNS
  cvm    <- roles$col_var_map

  # ---- (A) the ORIGINAL data columns become the new ROWS, reordered ------------------------------
  # Drop any Excel-only ASIDE column (there is none under a transpose -- mat_aside_cols does not run
  # here, so the composite survives in the cell -- but a stray one would duplicate its source row).
  # Order the survivors as the review asks: factor col_var levels, then Total, then n, then means.
  aside_i <- unname(roles$fmt_cols)[vapply(tab[unname(roles$fmt_cols)],
                                           function(c) identical(get_role(c), "aside"), logical(1))]
  data_i  <- setdiff(unname(roles$fmt_cols), aside_i)
  is_tot  <- data_i %in% roles$totcols
  is_n    <- fmt_is_helper_col(tab[data_i])
  types   <- vapply(data_i, function(j) fmt_var_kind(tab[[j]]), character(1))
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
  # Phase 19m-i: an ABSENT field is a real state -- a future phase may add one this flip does not
  # know, and the neutral is the right answer. A field of the WRONG LENGTH is not: every `ann` entry
  # is built per column from this same table, so it can only mean a producer went out of step, and
  # substituting a neutral silently is what made D1 (a transposed reg footer rendering grey) survive
  # two phases. `ann_get()` states that split once for the three flavours.
  ann_get <- function(k, field) {
    v <- ann[[onames[k]]][[field]]
    if (is.null(v)) return(NULL)
    stopifnot(length(v) == n_orow)
    v
  }
  slot_int <- function(field) lapply(seq_len(n_orow), function(c) {
    vapply(seq_len(n_nrow), function(k) {
      v <- ann_get(k, field); if (is.null(v)) 0L else as.integer(v[[c]])
    }, integer(1))
  })
  slot_lgl <- function(field) lapply(seq_len(n_orow), function(c) {
    vapply(seq_len(n_nrow), function(k) {
      v <- ann_get(k, field); if (is.null(v)) FALSE else isTRUE(v[[c]])
    }, logical(1))
  })
  slot_chr <- function(field, default) lapply(seq_len(n_orow), function(c) {
    vapply(seq_len(n_nrow), function(k) {
      v <- ann_get(k, field); if (is.null(v)) default else as.character(v[[c]])
    }, character(1))
  })
  text_slot_d <- slot_int("text_slot")
  bg_slot_d   <- slot_int("bg_slot")
  bold_d      <- slot_lgl("bold")
  # z11: the palette's typography, flipped like any other per-cell logical (constant FALSE for the
  # colour palettes). Without these a TRANSPOSED table would lose the print scheme in html AND Excel.
  facebold_d  <- slot_lgl("face_bold")
  faceital_d  <- slot_lgl("face_italic")
  # `face_underline` is the three-value vocabulary ("" / "single" / "double"), so it flips as a
  # CHARACTER: slot_lgl() would collapse a doubled rule to TRUE and lose it.
  faceund_d   <- slot_chr("face_underline", "")
  refalltot_d <- slot_lgl("ref_alltot")
  # Phase 19h (D1): `keep_black` is the "do not grey this cell" anchor set -- ref_alltot | is_refrow |
  # a regression's GOF footer rows (prep_one_table). It was NOT flipped, so a transposed table handed
  # the html engine a NULL, whose length-check fell back to `ref_alltot` alone -- silently greying a
  # transposed regression's footer cells. It is a per-cell logical like any other.
  keepblack_d <- slot_lgl("keep_black")
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
      tp <- tab_tooltip_text(tab[[j]], .ref = ann[[onm[j]]]$ref_cells)
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
    # ⚠ Phase 19l: length 0 lands HERE too, not only length 1 -- a table with NO col_var at all
    # (`tab(d, marital)`, whose columns carry the "no_col_var" sentinel that roles$real_col_vars
    # filters out). `roles$real_col_vars[[1]]` then aborted "subscript out of bounds" on any
    # tab_html(transpose = TRUE) of such a table. There is no variable to name the level column
    # after, so it takes the neutral internal key the compacted branch uses for the same job.
    cvname     <- if (length(roles$real_col_vars)) roles$real_col_vars[[1]] else "levels"
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
  # The absorbed synthetic columns-turned-rows are the total (roles$totcols) + the base-count /
  # add_pct columns -- both STRUCTURAL. The old `level_vals %in% c("pvalue", "row_pct")` clause was
  # dead here (level_vals is a COLUMN header, never an original row label) and missed col_pct.
  # Phase 19l: those two are found by their DECLARED role, not by the col_var tag they borrowed.
  col_of  <- unname(cvm[order_i])                  # each row's source col_var (STABLE; row_grp is mutated)
  is_addn <- fmt_is_helper_col(tab[order_i])       # the base-count / add_pct columns-turned-rows
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
         keep_black = keepblack_d[[c]],
         ref_cells  = rep(FALSE, n_nrow),
         text_hex   = texthex_d[[c]], bg_hex = bghex_d[[c]],
         text_slot  = text_slot_d[[c]], bg_slot = bg_slot_d[[c]],
         font = font_d[[c]], back = back_d[[c]],
         bold = bold_d[[c]],
         face_bold = facebold_d[[c]], face_italic = faceital_d[[c]],
         face_underline = faceund_d[[c]],
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
  # THE UNIT LINE AFTER A FLIP. A transposed data column holds one original ROW across every original
  # column, so it has a name only where those columns agreed on one -- and the reading direction turns
  # with the axes, which is what makes a transposed row% table read exactly like a native col% one.
  # The original TOTAL columns are excluded: after the flip they are the Total ROW, and a native col%
  # table's column names do not know about it either.
  cvh_unit <- character(length(all_names))
  src_cols <- names(tab)[purrr::map_lgl(tab, ~ is_fmt(.) && !is_totcol(.) &&
                                          !get_role(.) %in% c("n", "pct", "sd", "aside"))]
  if (length(src_cols)) {
    su <- unique(vapply(tab[src_cols], fmt_display_label, character(1), style = "tag"))
    if (length(su) == 1L && nzchar(su)) {
      cvh_unit[data_pos] <- tx_flip_pct_label(su)
      # ONE label, at the leftmost data column: every transposed data column carries the same `su`
      # by construction (that is the condition above), Total included -- after the flip it is the
      # Total ROW, exactly as in a native table of the same orientation.
      cvh_unit <- tab_units_once(cvh_unit, replace(character(length(all_names)), data_pos, "d"))
    }
  }
  col_var_header <- list(label = cvh_label, clean = cvh_clean, unit = cvh_unit)

  has_stars <- isTRUE(roles$has_stars)

  # Phase 19h (D1): rd2 MODIFIES rd; it is not re-typed. The literal this replaces enumerated ~39
  # slots, had already lost two silently, and was losing `ann$keep_black` when this was written --
  # masked by a length-check fallback in the html engine. Every slot the flip does not touch
  # (`subtext`, `reg_title`, `caption`, `empirical_tips`, and anything a later phase
  # adds) now survives by construction, because it is never mentioned. Only what genuinely changes
  # axes is assigned below.
  rd2 <- rd
  rd2$tab        <- new_tab
  rd2$transposed <- TRUE
  # the colour legend describes the MEASURES, which live on the original fmt columns (the synthetic
  # `tab` above is plain character) -- so keep the pre-transpose fmt table for tab_color_legend().
  rd2$color_src <- tab
  rd2$cells     <- cells_all
  rd2$tooltips  <- if (!is.null(tips_data)) stats::setNames(tips_data, dnames) else NULL
  rd2$vars      <- list(degrade = FALSE, row_var = row_var_col_name, tab_vars = character(0),
                        row_vars = rd$vars$col_vars, compacted = compacted2,
                        col_vars = rd$vars$row_vars)
  rd2$roles <- list(
    fmt_mask = fmt_mask, fmt_cols = fmt_cols, other_cols = other_cols,
    row_var_col = which(all_names == row_var_col_name),
    totcols = new_totcols, totrows = new_totrows,
    totblock_top = totblock_top, totblock_bottom = totblock_bottom,
    real_col_vars = unique(src_name[nzchar(src_name)]),
    col_var_map = stats::setNames(col_grp, all_names),
    new_col_var = new_col_var, new_group = new_group, align = align,
    label_cols = label_cols, var_name_col = var_name_col, label_runs = label_runs,
    sd_cols = integer(0), has_stars = has_stars)
  # the colour flags come from the SAME producer the prep uses, so "is this table coloured" cannot
  # mean one thing before the flip and another after it (it used to: declared vs realised).
  rd2$roles <- c(rd2$roles, roles_color_flags(ann_new, rd$roles$color_cols))
  rd2$ann            <- ann_new
  rd2$bold_rows      <- bold_rows
  # ⚠ a ROW-INDEX fact of the original table means nothing here: the footer block is a set of
  # COLUMNS after the flip, so the html engine must not draw its boundary from stale indices.
  rd2$footer_rows    <- integer(0)
  rd2$bold_cols      <- bold_cols
  rd2$col_var_header <- col_var_header
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

# row% <-> col%: the ONE thing a transposed unit label must say differently, because a percentage
# names the axis it sums on and the flip turns that axis. Everything else in the label ("mean",
# "(n)", "OR") is axis-free.
#' @keywords internal
#' @noRd
tx_flip_pct_label <- function(x) {
  if (grepl("row%", x, fixed = TRUE)) return(gsub("row%", "col%", x, fixed = TRUE))
  gsub("col%", "row%", x, fixed = TRUE)
}
