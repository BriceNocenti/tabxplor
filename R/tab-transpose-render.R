# PURPOSE: the transpose seam -- flip a FINISHED render model, not the fmt fields.
# ROLE: tab_export_prep() builds the normal per-table model and then calls tx_transpose_render() on
#   it, so `tab_export(transpose = TRUE)` swaps the axes AFTER every colour and cell string has been
#   computed on its own homogeneous source column. The backends render the flipped model.
# KEY CONSTRAINTS:
#   - A TRANSPOSED COLUMN IS HETEROGENEOUS -- a percentage, a mean and a count stacked -- so it
#     cannot be a tabxplor_fmt column and cannot be re-format()ted. The cell STRINGS (and, for Excel,
#     the values and their numFmt codes) are produced here per ORIGINAL column, then flipped as plain
#     data, which is what lets this seam colour every cell on its own ladder and accept a structure
#     the object-level tab_transpose() refuses (several row_vars, tab_vars). The object-level flip is
#     the one to use when the RESULT must stay a table; a mixed column there is graded by whichever
#     ladder its `mixed` scale carries (fmt_color_plan()).
#   - THE UNIT LINE TURNS WITH THE AXES: a transposed data column holds one original ROW across every
#     original column, so it is named only where those columns agreed on one name -- and row% becomes
#     col% (tx_flip_pct_label), which is what makes a transposed row% table render exactly like a
#     native col% one. The original Total columns are excluded: after the flip they are the Total ROW.
#   - The result is a SYNTHETIC render model: `$tab` is a plain character tibble, `$transposed` is
#     TRUE, `$cells` holds the pre-formatted strings, and roles and headers are the flipped ones. A
#     backend reads `$cells` when `$transposed` is TRUE and is otherwise untouched.
#   - WARNING: it runs AFTER tab_materialize_extras(backend = "xl"), which keeps `n` a COLUMN -- it
#     flips to an `n` ROW, matching a native col% table -- and has already collapsed the redundant
#     Total rows. The Excel ASIDE split does NOT run under a transpose: every backend gets a
#     formatted string here, so a composite cell survives the flip whole and splitting it would strip
#     the aside off.
# See: CLAUDE.md section "tabxplor architecture" (exports and rendering).


# === SECTION: the model flip =========================================================================

# Flips one prep_one_table() result into a transposed synthetic render model.
#   `rd`      a prep_one_table() result (roles + ann + col_var_header + bold_rows + tab)
#   `backend` "kable" | "md" | "plot" | "xl" -- drives the cell-string production only
#' @noRd
tx_transpose_render <- function(rd, backend) {
  if (isTRUE(rd$vars$degrade)) return(rd)                       # a malformed table degrades unchanged
  # a real tab_vars table has no single flip (its two-level structure is out of scope); a several
  # row_var (compacted) table is fine -- the whole point of this phase.
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
  # no Excel-only ASIDE column exists under a transpose (mat_aside_cols does not run here); a stray
  # one would duplicate its source row.
  aside_i <- unname(roles$fmt_cols)[vapply(tab[unname(roles$fmt_cols)],
                                           function(c) fmt_is_aside(c), logical(1))]
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
  # `label` = the col_var NAME per source column; `clean` = its level label. >1 real col_var -> a
  # NAME column + a LEVEL column; exactly 1 -> a single level column headed by the col_var name.
  name_vals  <- cvh$label[order_i]
  level_vals <- cvh$clean[order_i]
  compacted2 <- length(roles$real_col_vars) > 1

  # ---- (C) new col_var header, spanning the new COLUMNS (= original rows) ------------------------
  row_lvl <- as.character(tab[[roles$row_var_col]])
  if (isTRUE(rd$vars$compacted) && length(roles$var_name_col) == 1) {
    src_name <- as.character(tab[[roles$var_name_col]])
  } else {
    # single row_var: its name spans every level column -- swap it for the label (display only).
    src_name <- rep(var_label_display(rd$vars$row_var, tab), n_orow)
  }
  is_totrow_o <- seq_len(n_orow) %in% roles$totrows
  src_name[is_totrow_o] <- ""                      # a Total column is standalone, under no group name

  # ---- (D) pre-format the source data columns, then FLIP ----------------------------------------
  fmted <- tx_format_source_cols(tab, ann, order_i, backend)
  flip_col <- function(get) lapply(seq_len(n_orow), function(c) {
    vapply(seq_len(n_nrow), function(k) get(k)[[c]], character(1))
  })
  cells_data <- flip_col(function(k) fmted$txt[[k]])

  # An ABSENT ann field is a real state and the neutral default is right; a WRONG-LENGTH field means
  # a producer went out of step and must abort, not silently substitute.
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
  facebold_d  <- slot_lgl("face_bold")
  faceital_d  <- slot_lgl("face_italic")
  # face_underline is a three-value vocabulary, flipped as CHARACTER: slot_lgl() would collapse a
  # doubled rule to TRUE.
  faceund_d   <- slot_chr("face_underline", "")
  refalltot_d <- slot_lgl("ref_alltot")
  keepblack_d <- slot_lgl("keep_black")
  font_d      <- slot_chr("font", NA_character_)
  back_d      <- slot_chr("back", "none")
  texthex_d   <- slot_chr("text_hex", NA_character_)
  bghex_d     <- slot_chr("bg_hex", NA_character_)
  # a mixed new column's has_color/has_bgc is TRUE if any source cell's is.
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
    # a table with NO col_var at all carries the "no_col_var" sentinel real_col_vars filters out, so
    # length 0 lands HERE too, not just length 1 -- it takes the same neutral key as compacted2.
    cvname     <- if (length(roles$real_col_vars)) roles$real_col_vars[[1]] else "levels"
    lead_names <- cvname
    lead_vals  <- list(level_vals)
    lead_clean <- var_label_display(cvname, tab)
    row_var_col_name  <- cvname
    var_name_col_name <- NULL
  }
  all_names <- c(lead_names, dnames)
  n_lead    <- length(lead_names)
  data_pos  <- n_lead + seq_len(n_orow)            # new positions of the data columns

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
  # vertical borders between row_var column-groups
  cg <- col_grp; cg[other_cols] <- names(other_cols)
  new_col_var <- which(cg != dplyr::lead(cg, default = "._end") & seq_along(cg) %in% data_pos)
  # horizontal borders between distinct REAL col_var row-groups. Total/n/col_pct rows are absorbed
  # into the preceding group (no separator), matching a native pct="col" table.
  col_of  <- unname(cvm[order_i])                  # each row's source col_var (STABLE; row_grp mutated)
  is_addn <- fmt_is_helper_col(tab[order_i])       # base-count/add_pct columns-turned-rows, absorbed
  row_grp <- col_of
  absorb  <- (order_i %in% roles$totcols) | is_addn
  row_grp[absorb] <- NA
  for (i in seq_len(n_nrow)[-1]) if (is.na(row_grp[i])) row_grp[i] <- row_grp[i - 1]
  if (is.na(row_grp[1])) row_grp[1] <- "._start"
  new_group <- if (n_nrow > 1) which(row_grp[-n_nrow] != row_grp[-1]) else integer(0)
  # total block (Total row + n row): recompute on the new rows (shared border formula)
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
  # see file header (the unit line turns with the axes); TOTAL columns are excluded, being the Total
  # ROW after the flip.
  cvh_unit <- character(length(all_names))
  src_cols <- names(tab)[purrr::map_lgl(tab, ~ is_fmt(.) && !is_totcol(.) &&
                                          !get_role(.) %in% c("n", "pct", "sd") && !fmt_is_aside(.))]
  if (length(src_cols)) {
    su <- unique(vapply(tab[src_cols], fmt_display_label, character(1), style = "tag"))
    if (length(su) == 1L && nzchar(su)) {
      cvh_unit[union(data_pos, new_totcols)] <- tx_flip_pct_label(su)
      # ONE PER BLOCK: leftmost column of each states it, so the Total restates it rather than every
      # data column repeating it.
      cvh_unit <- tab_units_once(
        cvh_unit, tab_col_block_ids(col_grp, other_cols = other_cols, totcols = new_totcols))
    }
  }
  col_var_header <- list(label = cvh_label, clean = cvh_clean, unit = cvh_unit)

  has_stars <- isTRUE(roles$has_stars)

  # rd2 MODIFIES rd: every slot the flip does not touch survives by construction.
  rd2 <- rd
  rd2$tab        <- new_tab
  rd2$transposed <- TRUE
  # the colour legend describes the MEASURES, which live on the original fmt columns (the synthetic
  # `tab` above is plain character) -- keep the pre-transpose fmt table for tab_color_legend().
  rd2$color_src <- tab
  rd2$cells     <- cells_all
  # ⚠ ANYTHING KEYED BY A COLUMN NAME DIES HERE, silently, because a transposed column IS a row level:
  # `bars` would keep the pre-transpose names and match nothing. A data bar carries ONE reference per
  # column, and a flipped column is a row level -- so it goes, rather than mis-drawing.
  # (Same rule as follow_wrap()'s in tab_export_prep(): a stale key is not an error, it quietly does
  # nothing -- so every such member is re-keyed or dropped at the seam that invalidates it.)
  rd2$bars      <- NULL
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
    col_blocks = tab_col_block_ids(col_grp, other_cols = other_cols, totcols = new_totcols),
    new_col_var = new_col_var, new_group = new_group, align = align,
    label_cols = label_cols, var_name_col = var_name_col, label_runs = label_runs,
    sd_cols = integer(0), has_stars = has_stars)
  # the colour flags come from the SAME producer the prep uses, so "is this table coloured" cannot
  # mean one thing before the flip and another after it.
  rd2$roles <- c(rd2$roles, roles_color_flags(ann_new, rd$roles$color_cols))
  rd2$ann            <- ann_new
  rd2$bold_rows      <- bold_rows
  # a ROW-INDEX fact of the original table means nothing here: the footer block is a set of COLUMNS
  # after the flip, so the html engine must not draw its boundary from stale indices.
  rd2$footer_rows    <- integer(0)
  rd2$bold_cols      <- bold_cols
  rd2$col_var_header <- col_var_header
  rd2
}

# Formats the source data columns with the BACKEND's own call, so the transposed strings are
# identical to what the non-transposed backend would emit for that column.
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
      # a transposed column mixes types (a %, a mean, an n) and one written column is ONE R type, so
      # real per-cell numbers would need a per-cell writer. Writes the DISPLAY STRING instead (sigma
      # folded into the mean cell); colours ride the slot grid separately.
      txt[[k]] <- format(col, special_formatting = TRUE, na = "", stars = TRUE, pad = fig_space,
                         .ref = rf)
    } else if (identical(backend, "md")) {
      raw <- format(col, special_formatting = TRUE, na = "", stars = TRUE, bold_split = TRUE,
                    pad = fig_space, .ref = rf)
      txt[[k]] <- trimws(raw, which = "left", whitespace = "[\\h\\v]")  # strip the source column's own pad
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

# row% <-> col%: the ONE thing a transposed unit label must say differently, since a percentage names
# the axis it sums on and the flip turns that axis. Everything else in the label ("mean", "(n)", "OR")
# is axis-free.
#' @noRd
tx_flip_pct_label <- function(x) {
  if (grepl("row%", x, fixed = TRUE)) return(gsub("row%", "col%", x, fixed = TRUE))
  gsub("col%", "row%", x, fixed = TRUE)
}
