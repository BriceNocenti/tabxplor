# PURPOSE: The ONE shared exporter-prep helper + ephemeral render-model for tab_kable / tab_md /
#          tab_plot (and, from Phase 10g, tab_xl). Kills the 4x-duplicated "canonical col_vars ->
#          validate -> compact" preamble, the per-exporter role detection, and the repeated
#          get_reference()/fmt_channel_codes() derivations by computing each ONCE.
# ROLE: Phase 10d. The exporters become `prep <- tab_export_prep(...); render_<backend>(prep)`.
# KEY CONSTRAINTS:
#   - BYTE-IDENTICAL: the prep reproduces each exporter's exact derivation, in the same order, so
#     rendered output does not change (golden / md-snapshot / color-golden / export-parity locked).
#   - The render-model is an EPHEMERAL S3-tagged list, NOT tab attributes (dplyr rename/select desync
#     bare attributes). Built once, consumed by one backend, discarded.
#   - Genuinely medium-specific quirks stay LOCAL to each exporter (glyph/colour application,
#     NA-hiding, the new_col_var transition index, md's tab_vars keep+blank). The prep factors only
#     the shared, expensive, derive-once quantities.
# See: dev/tabxplor_phase10_exporters.md (Sec 1-2, 5), CLAUDE.md Phase 10d, decisions.md Sec 33.


# === SECTION: canonical col_vars check (block A) =====================================

# The "longest col_vars set = canonical, validate all others match, no tab_vars" selection that was
# duplicated verbatim in tab_kable / tab_md / tab_compact. Returns the canonical col_vars invisibly;
# aborts (in the CALLER's frame) when the list can't be compacted. `what` names the calling exporter
# for the message.
#' @keywords internal
tab_check_same_col_vars <- function(tabs, what = "tab_export_prep()",
                                    call = rlang::caller_env()) {
  same_col_vars <- purrr::map(tabs, ~ tab_get_vars(.)$col_vars)
  same_col_vars <- same_col_vars |>
    purrr::map(~ .[!. %in% c("all_col_vars", "", "no") & !is.na(.)])
  longest_col_vars <- purrr::map_int(same_col_vars, length)
  longest_col_vars <-
    dplyr::first(which(longest_col_vars == max(longest_col_vars, na.rm = TRUE)))
  longest_col_vars <- same_col_vars[[longest_col_vars]]
  all_same <- same_col_vars |> purrr::map_lgl(~ all(. %in% longest_col_vars))
  if (!all(all_same)) {
    cli::cli_abort(
      "{what} can only be used with a list of tab if they have the same col_vars.",
      call = call
    )
  }
  if (any(purrr::map_lgl(tabs, ~ length(tab_get_vars(.)$tab_vars) > 0))) {
    cli::cli_abort(
      "{what} can only be used with a list of tab if they have no tab_vars.",
      call = call
    )
  }
  invisible(longest_col_vars)
}


# === SECTION: per-column derive-once sidecar (`ann`) =================================

# The per-fmt-column quantities every colour backend re-derives today, computed ONCE. `theme_cols` is
# the resolved list(text, grey, grey2). The FULL structure (font/back/bold/slots/refs) is ALWAYS
# returned so every backend reads a consistent shape (Phase 10j); `want_colors = FALSE` (color = FALSE
# export) only forces a MONOCHROME column -- no slots, no hex, the font/back/bold an uncoloured column
# would take -- skipping the fmt_channel_codes() hex-mapping cost.
# BYTE-IDENTICAL (want_colors = TRUE) to the tab_kable colour loop (tab_classes.R) / tab_plot colours.
#' @keywords internal
fmt_col_ann <- function(col, theme_cols, color_type = "text", html_24_bit = NULL,
                        want_colors = TRUE) {
  ref_alltot <- get_reference(col, mode = "all_totals")
  ref_cells  <- get_reference(col, mode = "cells")

  ct <- get_color(col)
  has_col <- want_colors && length(ct) != 0L && !is.na(ct) && !ct %in% c("", "no")
  cb <- get_color_bg(col)
  has_bgc <- want_colors && length(cb) != 0L && !is.na(cb) && !cb %in% c("", "no")
  grey_this <- if (has_col || has_bgc) theme_cols$grey else theme_cols$grey2

  if (has_col || has_bgc) {
    codes     <- fmt_channel_codes(col, color_type, theme_cols$theme, html_24_bit)
    text_hex  <- codes$text
    bg_hex    <- codes$bg
    # Phase 10f: keep the raw slot integers fmt_channel_codes() already produced -- tab_md() maps
    # them to break-derived pandoc span classes (.p20 / .bgx2 / ...). Byte-neutral for kable/plot,
    # which read font/back/bold only.
    text_slot <- codes$text_slot
    bg_slot   <- codes$bg_slot
  } else {
    text_hex  <- rep(NA_character_, length(col))
    bg_hex    <- rep(NA_character_, length(col))
    text_slot <- integer(length(col))
    bg_slot   <- integer(length(col))
  }

  list(
    ref_alltot = ref_alltot,
    ref_cells  = ref_cells,
    text_hex   = text_hex,
    bg_hex     = bg_hex,
    text_slot  = text_slot,
    bg_slot    = bg_slot,
    font = dplyr::case_when(!is.na(text_hex) ~ text_hex,
                            ref_alltot       ~ theme_cols$text,
                            TRUE             ~ grey_this),
    back = dplyr::if_else(is.na(bg_hex), "none", bg_hex),
    bold = !is.na(text_hex) | ref_alltot,
    has_color = has_col || has_bgc,
    has_bgc   = has_bgc
  )
}


# Convert a per-column `ann` entry to the `.ref` list format()/pillar_shaft expect (the precomputed
# reference masks -> the derive-once speed-up). NULL when the column has no ann (non-fmt / not built).
#' @keywords internal
ann_ref <- function(a) {
  if (is.null(a)) NULL else list(cells = a$ref_cells, all_totals = a$ref_alltot)
}


# The shared bold-row set (block D): a row is bold iff it is a reference/total cell in EVERY
# DISCRIMINATING column (a column that is all-reference or all-non-reference says nothing about which
# ROWS are references, so it is dropped first). Reuses the already-computed `ref_alltot` masks.
# `md_style = TRUE` reproduces tab_md's empty-set guard (integer(0) when no discriminating column);
# `FALSE` reproduces tab_kable/tab_plot's `rowSums == ncol` on the reduced frame (which flags ALL
# rows when 0 columns survive -- the historical edge, preserved for byte-identity).
#' @keywords internal
tab_bold_rows <- function(ref_alltot_list, md_style = FALSE) {
  if (length(ref_alltot_list) == 0) return(integer(0))
  refref <- as.data.frame(ref_alltot_list)
  keep   <- purrr::map_lgl(refref, ~ any(.) & !all(.))
  if (md_style && !any(keep)) return(integer(0))
  refref <- refref[, keep, drop = FALSE]
  which(rowSums(refref) == ncol(refref))
}


# === SECTION: total-column base range [min;max] (block B, decisions.md Sec 10) =======

# Each col_var's own percentage base can differ (chiefly na="drop" with different NA rates), so one
# Total column must summarise K row bases. This TABLE-LEVEL pre-pass (format() sees one column at a
# time and cannot) returns, per data row, the scalar base when all col_vars agree, else the range.
#   base per cell = get_tot_n() for row/col/all types, get_n() for means; weighted variants via
#   get_tot_wn()/get_wn() -- the tab_ci()/tab_apply_n_min() rule (tab.R). Built INERT in Phase 10d
#   (nothing consumes `text` yet -> byte-identical); the exporters overwrite the Total cell strings
#   in Phase 10e/10f (a conscious golden change then).
# `style`: "range" -> "[min;max]" (default), "min" -> the smallest (safest) base
# (option "tabxplor.totcol_range").
#' @keywords internal
tab_totcol_range <- function(tab, fmt_cols, col_var_map, totcols,
                             style = getOption("tabxplor.totcol_range", "range")) {
  n_row <- nrow(tab)
  empty <- list(col = totcols, text = character(0), differ = logical(0))
  if (length(fmt_cols) == 0 || length(totcols) == 0) return(empty)

  # One representative base per col_var per row: within a col_var+row the base is constant, so read
  # each col_var's total column (or its first fmt column) once. NA bases (mean cells with no base)
  # are ignored in the min/max.
  cvs <- unique(col_var_map[fmt_cols])
  cvs <- cvs[!cvs %in% c("all_col_vars", "", "no", NA_character_)]
  if (length(cvs) == 0) return(empty)

  base_of <- function(col) {
    if (get_type(col) == "mean") get_n(col) else get_tot_n(col)
  }

  # a matrix of bases: rows = table rows, cols = col_vars
  base_mat <- vapply(cvs, function(cv) {
    cols_cv  <- names(col_var_map)[col_var_map == cv]
    cols_cv  <- intersect(cols_cv, names(fmt_cols))
    if (length(cols_cv) == 0) return(rep(NA_real_, n_row))
    # prefer this col_var's total column, else its first column
    tcol <- intersect(names(totcols), cols_cv)
    pick <- if (length(tcol) > 0) tcol[[1]] else cols_cv[[1]]
    as.numeric(base_of(tab[[pick]]))
  }, numeric(n_row))
  if (is.null(dim(base_mat))) base_mat <- matrix(base_mat, nrow = n_row)

  row_min <- suppressWarnings(apply(base_mat, 1, min, na.rm = TRUE))
  row_max <- suppressWarnings(apply(base_mat, 1, max, na.rm = TRUE))
  fin     <- is.finite(row_min) & is.finite(row_max)
  differ  <- fin & (row_max != row_min)

  text <- rep(NA_character_, n_row)
  text[fin & !differ] <- as.character(round(row_min[fin & !differ]))
  if (identical(style, "min")) {
    text[differ] <- as.character(round(row_min[differ]))
    differ[]     <- FALSE
  } else {
    text[differ] <- paste0("[", round(row_min[differ]), ";", round(row_max[differ]), "]")
  }
  list(col = totcols, text = text, differ = differ)
}


# === SECTION: the render-model builder ==============================================

# Non-erroring twin of tab_check_same_col_vars(): TRUE iff a list of tabs can be merged by
# tab_compact() -- i.e. NO tab_vars anywhere AND all share the same (longest) col_vars set.
#' @keywords internal
tab_list_mergeable <- function(tabs) {
  if (any(purrr::map_lgl(tabs, ~ length(tab_get_vars(.)$tab_vars) > 0))) return(FALSE)
  cvs <- purrr::map(tabs, ~ {
    v <- tab_get_vars(.)$col_vars
    v[!v %in% c("all_col_vars", "", "no") & !is.na(v)]
  })
  lens    <- purrr::map_int(cvs, length)
  longest <- cvs[[dplyr::first(which(lens == max(lens, na.rm = TRUE)))]]
  all(purrr::map_lgl(cvs, ~ all(. %in% longest)))
}

# Resolve the input into the list of tables to render.
#   - a single tab             -> itself.
#   - a MERGEABLE list          -> compact into ONE table (same col_vars, no tab_vars).
#   - a NON-mergeable list      -> `list_method = TRUE` (tab_md): return the list, rendered
#                                  one-after-another (each keeps its own tab_vars sub-tables);
#                                  `list_method = FALSE` (tab_kable / tab_plot, no list renderer yet):
#                                  error with the historical message (block A).
#' @keywords internal
tab_resolve_tables <- function(tabs, compact, list_method = FALSE, what,
                               call = rlang::caller_env()) {
  if (is.data.frame(tabs) || !is.list(tabs)) return(list(tabs))
  if (compact && tab_list_mergeable(tabs)) return(list(tab_compact(tabs)))
  if (list_method) return(tabs)                 # render each separately (the list method)
  tab_check_same_col_vars(tabs, what = what, call = call)  # errors (kable/plot) -- current behaviour
  tabs
}


# Build the render-model for ONE resolved table (already compacted / single). See the file header.
#' @keywords internal
prep_one_table <- function(tab, backend, drop_tab_vars, wrap, compute,
                           theme_cols, color_type, html_24_bit) {
  rv <- tab_render_vars(tab)
  if (isTRUE(rv$degrade)) {
    return(list(tab = tab, vars = list(degrade = TRUE, reason = rv$reason)))
  }

  tab_vars <- rv$tab_vars
  subtext  <- get_subtext(tab) %>% purrr::discard(. == "")

  # group boundaries -- computed BEFORE ungroup (needs the grouping)
  gi        <- dplyr::group_indices(tab)
  new_group <- which(gi != dplyr::lead(gi, default = max(gi) + 1L))

  tab <- dplyr::ungroup(tab)
  if (drop_tab_vars && length(tab_vars) > 0) {
    tab <- dplyr::select(tab, -tidyselect::all_of(tab_vars))
  }
  if (!is.null(wrap)) {
    tab <- tab_wrap_text(tab,
                         wrap_rows          = wrap$rows,
                         wrap_cols          = wrap$cols,
                         exdent             = wrap$exdent,
                         whitespace_only    = wrap$whitespace_only,
                         unbreakable_spaces = wrap$unbreakable_spaces,
                         brk                = wrap$brk)
  }

  # --- role detection on the FINAL (ungrouped / dropped / wrapped) tab ---
  fmt_mask   <- purrr::map_lgl(tab, is_fmt)
  fmt_cols   <- which(fmt_mask)
  other_cols <- which(!fmt_mask)

  col_var_map   <- get_col_var(tab)
  real_col_vars <- unique(col_var_map[fmt_mask])
  real_col_vars <- real_col_vars[!real_col_vars %in%
                                   c("all_col_vars", "", "no", NA_character_)]

  color_cols <- get_color(tab)
  color_cols <- which(!color_cols %in% c("", "no") & !is.na(color_cols))

  totcols    <- which(is_totcol(tab))
  totrows    <- which(is_totrow(tab))
  no_totrows <- which(!is_totrow(tab))

  # row_var re-detected on the FINAL tab (wrap can rename the row-label column, so the index must
  # come from the wrapped names -- matches tab_kable's `which(names(tabs) == tab_get_vars(...)$row_var)`).
  row_var_name <- tab_render_vars(tab)$row_var
  row_var_col  <- which(names(tab) == row_var_name)

  # Total-BLOCK border rows (block D borders), lifted verbatim from tab_kable (derive-once, shared by
  # both render engines). A "total block" is a maximal run of total rows OR the reserved n/pvalue/
  # row_pct label rows; the first row of each run gets a top border, the last a bottom border.
  # WARNING: the c("n","pvalue","row_pct") whitelist is un-translated (English row labels) -> it
  # silently misses jamovi's gettext labels. Kept as-is for byte-identity; a real fix needs a per-row
  # role flag on the add_n/add_pct/pvalue rows rather than a value match on the row-label column.
  tot_block <- is_totrow(tab) |
    (!is_totrow(tab) & tab[[row_var_col]] %in% c("n", "pvalue", "row_pct"))
  totblock_top <-
    which(dplyr::if_else(tot_block, !dplyr::lag(tot_block), FALSE))
  totblock_bottom <-
    which(dplyr::if_else(tot_block, !dplyr::lead(tot_block, default = FALSE), FALSE))

  # kable/plot col_var transition index (one-liner). md keeps its own real-col_var span loop.
  new_col_var <- col_var_map
  new_col_var[names(other_cols)] <- names(other_cols)
  new_col_var <- which(new_col_var != dplyr::lead(new_col_var, default = "._at_the_end"))

  align <- purrr::map_chr(
    tab, ~ dplyr::if_else(is_fmt(.) | is.numeric(.), "r", "l")
  )

  # --- per-column ann (derive-once) ---
  want_colors <- "colors" %in% compute
  ann <- purrr::map(
    stats::setNames(names(fmt_cols), names(fmt_cols)),
    ~ fmt_col_ann(tab[[.x]], theme_cols, color_type, html_24_bit, want_colors)
  )
  any_bg <- if (want_colors) any(purrr::map_lgl(ann, ~ isTRUE(.$has_bgc))) else FALSE

  # --- bold rows + bold cols (block D), reusing ann$ref_alltot ---
  ref_alltot_list <- purrr::map(ann, "ref_alltot")
  bold_rows <- if ("bold" %in% compute) {
    tab_bold_rows(ref_alltot_list, md_style = identical(backend, "md"))
  } else integer(0)
  bold_cols <- if ("bold" %in% compute && length(ref_alltot_list) > 0) {
    names(which(purrr::map_lgl(ref_alltot_list, all)))
  } else character(0)

  range_totcol <- if ("range" %in% compute) {
    tab_totcol_range(tab, fmt_cols, col_var_map, totcols)
  } else NULL

  list(
    tab = tab,
    vars = list(degrade = FALSE, row_var = row_var_name, tab_vars = tab_vars,
                col_vars = rv$col_vars, col_vars_levels = rv$col_vars_levels),
    roles = list(fmt_mask = fmt_mask, fmt_cols = fmt_cols, other_cols = other_cols,
                 row_var_col = row_var_col, totcols = totcols, totrows = totrows,
                 no_totrows = no_totrows, totblock_top = totblock_top,
                 totblock_bottom = totblock_bottom, real_col_vars = real_col_vars,
                 col_var_map = col_var_map, new_col_var = new_col_var,
                 new_group = new_group, align = align,
                 color_cols = color_cols, any_bg = any_bg),
    ann = ann,
    bold_rows = bold_rows,
    bold_cols = bold_cols,
    range_totcol = range_totcol,
    subtext = subtext
  )
}


# === SECTION: shared option resolver (Phase 10j) ====================================

# Resolve the canonical shared export options ONCE (theme / color_type / html_24_bit + the on/off
# toggles), so every exporter AND the tab_export() facade share one set of names, defaults and option
# fallbacks (killing the copy-pasted `match.arg(theme)` + `if (is.null(color_type)) getOption(...)`
# preambles). `color = FALSE` renders monochrome AND disables the colour legend (which would otherwise
# describe colours the cells no longer show). Returns a normalized scalar list.
#' @keywords internal
resolve_export_opts <- function(theme = c("light", "dark"),
                                color_type = NULL, html_24_bit = NULL,
                                color = TRUE, color_legend = TRUE,
                                transpose = FALSE, caption = NULL) {
  theme <- match.arg(theme)
  if (is.null(color_type))  color_type  <- getOption("tabxplor.color_style_type")
  if (is.null(html_24_bit)) html_24_bit <- getOption("tabxplor.color_html_24_bit")
  color <- isTRUE(color)
  list(theme = theme, color_type = color_type, html_24_bit = html_24_bit,
       color = color, color_legend = isTRUE(color_legend) && color,
       transpose = isTRUE(transpose), caption = caption)
}


# The single exporter-prep entry point. Returns a `tabxplor_render` (an ephemeral tagged list; see the
# file header). `compute` gates the expensive derivations so a backend / jamovi live path opts out of
# what it does not use. `transpose = TRUE` (Phase 10j) transposes each table at export (col%-invert
# use case), applied AFTER materialise so the order matches tab_xl's historical materialise->transpose.
#' @keywords internal
tab_export_prep <- function(tabs,
                            backend       = c("kable", "md", "plot", "xl"),
                            compact       = TRUE,
                            drop_tab_vars = TRUE,
                            wrap          = NULL,
                            compute       = NULL,
                            color_type    = NULL,
                            theme         = "light",
                            html_24_bit   = NULL,
                            color_legend  = TRUE,
                            transpose     = FALSE,
                            list_method   = FALSE,
                            what          = NULL) {
  backend <- match.arg(backend)
  if (is.null(what)) what <- paste0("tab_", backend, "()")
  if (is.null(compute)) {
    compute <- if (backend %in% c("kable", "plot")) {
      c("refs", "colors", "bold", "range", "labels")
    } else c("refs", "bold", "labels")  # md / xl
  }
  # base `%||%` is R >= 4.4 only; the package supports R >= 4.1, so use explicit is.null().
  if (is.null(color_type))  color_type  <- getOption("tabxplor.color_style_type")
  if (is.null(html_24_bit)) html_24_bit <- getOption("tabxplor.color_html_24_bit")

  theme_cols <- list(
    theme = theme[1],
    text  = dplyr::if_else(theme[1] == "light", "#000000", "#FFFFFF"),
    grey  = dplyr::if_else(theme[1] == "light", "#989898", "#BBBBBB"),
    grey2 = dplyr::if_else(theme[1] == "light", "#111111", "#EEEEEE")
  )

  resolved <- tab_resolve_tables(tabs, compact = compact, list_method = list_method,
                                 what = what)

  # Phase 10i-B: hydrate the "core" table into its rendered shape ONCE, on the still-grouped resolved
  # tables (before prep_one_table ungroups), so p-value rows + add_n/add_pct are real rows/cols the
  # role detection then sees. "xl" keeps a real `n` column; every other backend folds add_n into the
  # Total cell (backend "text").
  mat_backend <- if (identical(backend, "xl")) "xl" else "text"
  resolved <- purrr::map(resolved, tab_materialize_extras, backend = mat_backend, pvalue = TRUE)

  # Phase 10j: opt-in transpose-at-export, applied AFTER materialise so the order matches tab_xl's
  # historical materialise->transpose (all four exporters now share this seam; console never transposes).
  if (isTRUE(transpose)) resolved <- purrr::map(resolved, tab_transpose)

  tables <- purrr::map(
    resolved,
    ~ prep_one_table(.x, backend = backend, drop_tab_vars = drop_tab_vars,
                     wrap = wrap, compute = compute, theme_cols = theme_cols,
                     color_type = color_type[1], html_24_bit = html_24_bit[1])
  )

  # table-level labels (survey `label` attributes of the source variables), Suggests-guarded; only
  # tab_kable consumes them (Phase 10e). Cheap NULL when unused.
  labels <- if ("labels" %in% compute) tab_export_labels(resolved) else NULL

  structure(
    list(
      tables = tables,
      labels = labels,
      meta = list(backend = backend, compact = compact, theme = theme[1],
                  color_type = color_type[1], html_24_bit = html_24_bit[1],
                  color_legend = color_legend, theme_cols = theme_cols,
                  compute = compute)
    ),
    class = "tabxplor_render"
  )
}


# Capture each variable's `label` attribute (survey question text), when present. Consumed by
# tab_kable only (Phase 10e). Returns a named chr (var -> label) or NULL.
#' @keywords internal
tab_export_labels <- function(resolved) {
  labs <- purrr::map(resolved, function(tab) {
    if (!is.data.frame(tab)) return(NULL)
    out <- purrr::map(tab, ~ attr(.x, "label", exact = TRUE))
    out <- out[!purrr::map_lgl(out, is.null)]
    if (length(out) == 0) return(NULL)
    purrr::map_chr(out, ~ as.character(.x)[[1]])
  })
  labs <- purrr::compact(labs)
  if (length(labs) == 0) return(NULL)
  out <- do.call(c, labs)
  out[!duplicated(names(out))]
}
