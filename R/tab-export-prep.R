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
#     NA-hiding, the new_col_var transition index). The prep factors only the shared, expensive,
#     derive-once quantities -- and, from 14i, the shared MODEL the backends' markup differs over.
#   - Phase 14i: the variable-NAME annotations are decided here, once, for all four backends:
#     `roles$label_cols` / `label_runs` (name each block once: md blanks, html rowspans, Excel
#     merges), `roles$var_name_col` (the merged table's name column), and the `var_names` argument,
#     whose two drops happen in prep_one_table() so no backend needs to know it exists.
# See: dev/tabxplor_phase10_exporters.md (Sec 1-2, 5), CLAUDE.md Phase 10d + 14i, decisions.md Sec 33.


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
fmt_col_ann <- function(col, theme_cols, want_colors = TRUE) {
  ref_alltot <- get_reference(col, mode = "all_totals")
  ref_cells  <- get_reference(col, mode = "cells")
  # Phase 14q: greying makes coloured cells pop, but a reading ANCHOR must stay black. `ref_alltot`
  # catches the crosstab total / reference row; a regression EMPIRICAL column (`Emp. %`) carries
  # ref_type = "tot" yet marks its reference CATEGORY via in_refrow, so ref_alltot misses it -- hence
  # the extra `is_refrow(col)`. For crosstabs is_refrow is a subset of ref_alltot, so this is a no-op
  # there. GOF footer ROWS are un-greyed at the table level in prep_one_table() (a per-column helper
  # cannot tell a footer row from a data row).
  keep_black <- ref_alltot | is_refrow(col)

  ct <- get_color(col)
  has_col <- want_colors && length(ct) != 0L && !is.na(ct) && !ct %in% c("", "no")
  cb <- get_color_bg(col)
  has_bgc <- want_colors && length(cb) != 0L && !is.na(cb) && !cb %in% c("", "no")
  grey_this <- if (has_col || has_bgc) theme_cols$grey else theme_cols$grey2

  if (has_col || has_bgc) {
    codes     <- fmt_channel_codes(col, theme_cols$theme)
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
    keep_black = keep_black,
    font = dplyr::case_when(!is.na(text_hex) ~ text_hex,
                            keep_black       ~ theme_cols$text,
                            TRUE             ~ grey_this),
    back = dplyr::if_else(is.na(bg_hex), "none", bg_hex),
    bold = !is.na(text_hex) | keep_black,
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


# === SECTION: the label columns and their runs (Phase 14i) ===========================

# The shared run model for the LABEL columns -- the leading factor columns whose value repeats down a
# block, and which every backend must therefore render ONCE per block: md blanks the repeats, the html
# engine gives the run a `rowspan`, Excel merges it. One definition, four consumers (the review's "add
# a shared function, be consistent between export types").
#
# Two kinds of label column, and they are MUTUALLY EXCLUSIVE by construction (tab_compact() bails on
# tab_vars, so a merged table has none):
#   - the synthetic `row_var` column of a merged table: its values are variable NAMES and its header is
#     the literal "row_var" -> `roles$var_name_col`, which `var_names` can drop and which renders
#     vertically / italic. It is neither the row_var (that is the literal "levels" column) nor a
#     tab_var, so nothing named it before this phase;
#   - the `tab_vars` when they are kept: their values are LEVELS and their header is a real variable
#     name. Data -- never dropped by `var_names`, never rotated.
#
# Returns, per column, `list(show = lgl(n_row), span = int(n_row))`: `show[i]` marks a run START and
# `span[i]` is its length; a `!show` row is a continuation (blank / omitted / merged over).
#
# DESIGN: runs come from the VALUES, not from the grouping. `roles$new_group` marks the same boundaries
# for a merged table, but for >= 2 tab_vars it marks the full group COMBINATION, so the outer tab_var's
# run (which spans several groups) would be cut. Values also survive a dplyr chain that ungrouped.
# The scan is nested (outer -> inner): an outer column's new run restarts every inner one, so a
# one-row group followed by a group repeating the inner value cannot be merged across. md's own loop
# compared each column naively; that is equivalent under the nested ordering tab() produces, so this
# is a hardening, not a behaviour change.
# NA = a continuation, reproducing md's rule verbatim: a materialised p-value row carries NA in the
# label column and belongs to the block above it.
#' @keywords internal
tab_label_runs <- function(tab, label_names) {
  n <- nrow(tab)
  res <- list()
  if (length(label_names) == 0 || n == 0) return(res)

  force <- rep(FALSE, n)
  force[1] <- TRUE                                    # the first row always starts a run
  for (cl in label_names) {
    # base `[[`, never tidyselect: a merged table HAS a column named "row_var" (the data-mask trap
    # tab_transpose() documents at tab.R ~L2425).
    v    <- as.character(tab[[cl]])
    locf <- v                                         # carry the last real value over the NA rows
    for (i in seq_len(n)[-1]) if (is.na(locf[i])) locf[i] <- locf[i - 1]
    start <- force
    if (n > 1) {
      changed <- !is.na(v[-1]) & (is.na(locf[-n]) | v[-1] != locf[-n])
      start[-1] <- start[-1] | changed
    }
    at   <- which(start)
    span <- rep(0L, n)
    span[at] <- diff(c(at, n + 1L))
    res[[cl]] <- list(show = start, span = span)
    force <- start                                    # nest the next (inner) column inside this one
  }
  res
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

# Resolve the input into the list of tables to render.
#   - a single tab        -> itself.
#   - a list              -> `list_method = TRUE` (tab_md / tab_xl / tab_plot): the list, rendered
#                            one-after-another (each keeps its own tab_vars sub-tables);
#                            `list_method = FALSE` (tab_kable, no list renderer yet): error (block A).
# Phase 14d: a list is NEVER merged here. `tab()` already merges what it decides to merge (a
# `tab_compact()` at build time, recorded as `compacted` in the `vars` attribute); a list reaching an
# exporter is one the user asked to keep separate -- via `output_list = TRUE`, `tab_many()`, or their
# own `list()` -- and silently gluing it back together at render time overrode that. It also removed
# the need for `tab_list_mergeable()`, the non-erroring twin of `tab_check_same_col_vars()` that
# re-ran `tab_get_vars()` over every tab immediately before `tab_compact()` re-ran the identical scan.
#' @keywords internal
tab_resolve_tables <- function(tabs, list_method = FALSE, what,
                               call = rlang::caller_env()) {
  if (is.data.frame(tabs) || !is.list(tabs)) return(list(tabs))
  if (list_method) return(tabs)                 # render each separately (the list method)
  tab_check_same_col_vars(tabs, what = what, call = call)  # errors (kable) -- current behaviour
  tabs
}


# Build the render-model for ONE resolved table (already compacted / single). See the file header.
#' @keywords internal
prep_one_table <- function(tab, backend, drop_tab_vars, wrap, compute,
                           theme_cols, var_names = "both") {
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
  # Phase 14i: `var_names` drops the row-side variable-NAME annotation -- the merged table's synthetic
  # `row_var` column, whose values ARE the names. Done HERE, before the role detection, so every index
  # below is right and every backend (incl. tab_plot, which reads no header model) inherits one rule.
  # The col side is the twin blank of `col_var_header$label` further down.
  # It never touches a level column's header: `marital` on a single-row_var table, `year` on a kept
  # tab_var. That header is the column's only identification and costs no width -- the maintainer's
  # call, and the symmetric one (the col side removes the span row, never the level names).
  # (the local is `name_col`, not `row_var`: with a column of that name in the frame, a same-named
  # local is the tidyselect data-mask trap tab_transpose() documents at tab.R ~L2425.)
  name_col <- if (isTRUE(rv$compacted) && "row_var" %in% names(tab)) "row_var" else character(0)
  if (length(name_col) > 0 && !var_names %in% c("both", "rows")) {
    tab      <- dplyr::select(tab, -tidyselect::all_of(name_col))
    name_col <- character(0)
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

  # Phase 14m-ii (rework): does this table actually SHOW significance stars? The number font switches to
  # a monospace stack (so stars align) ONLY for a starred table; a plain table keeps proportional DejaVu
  # Sans. get_stars() is "" for an absent/NA pvalue, so this is TRUE exactly when a star will render --
  # read by the html engine (adds the `tx-has-stars` class), tab_xl (picks font_num_stars) and tab_plot.
  has_stars <- length(fmt_cols) > 0 &&
    any(vapply(fmt_cols, function(j) any(nzchar(get_stars(tab[[j]]))), logical(1)))

  col_var_map   <- get_col_var(tab)
  real_col_vars <- unique(col_var_map[fmt_mask])
  # Phase 14p: `no_col_var` (the sentinel a no-col_var table's `n`/`pct`/`wn` columns carry) is NOT a
  # real variable name -- rendering it as a spanning col_var header is noise. Excluded here so
  # tab_col_var_header() marks those columns `is_level = FALSE` (no span label). "no_row_var" and the
  # empty/`no` markers are the sibling sentinels.
  real_col_vars <- real_col_vars[!real_col_vars %in%
                                   c("all_col_vars", "no_col_var", "no_row_var",
                                     "", "no", NA_character_)]

  color_cols <- get_color(tab)
  color_cols <- which(!color_cols %in% c("", "no") & !is.na(color_cols))

  totcols    <- which(is_totcol(tab))
  totrows    <- which(is_totrow(tab))
  no_totrows <- which(!is_totrow(tab))

  # row_var re-detected on the FINAL tab (wrap can rename the row-label column, so the index must
  # come from the wrapped names -- matches tab_kable's `which(names(tabs) == tab_get_vars(...)$row_var)`).
  row_var_name <- tab_render_vars(tab)$row_var
  row_var_col  <- which(names(tab) == row_var_name)

  # Phase 14i: the LABEL columns and their runs -- see tab_label_runs(). `label_cols` is the blank /
  # rowspan / merge set (the synthetic name column OR the kept tab_vars, never both); `var_name_col`
  # is the name-valued subset, the only one `var_names` drops, the header always blanks, and the html
  # / Excel backends rotate. Both are named-int, indexed on the FINAL tab like every role above.
  label_names <- intersect(c(name_col, tab_vars), names(tab))
  label_cols  <- stats::setNames(match(label_names, names(tab)), label_names)
  var_name_col <- label_cols[names(label_cols) %in% name_col]
  label_runs  <- tab_label_runs(tab, label_names)

  # Phase 14l: the Excel-only "<var>_sd" siblings tab_materialize_extras(backend = "xl") splits off a
  # numeric mean column into. ONE definition, read by tab_col_var_header() (which heads them "sd") and
  # by tab_xl's column widths -- the rule used to be re-derived at each site.
  # Naturally integer(0) for every other backend: nothing else creates those columns.
  # WARNING: ungated by `var_names`, unlike the header rewrite. A width is not a naming decision, so
  # `var_names = "none"` must still get a narrow sd column.
  sd_cols <- fmt_cols[vapply(names(fmt_cols), function(nm) {
    cv <- col_var_map[[nm]]
    !is.na(cv) && nzchar(cv) && identical(nm, paste0(cv, "_sd"))
  }, logical(1))]

  # Total-BLOCK border rows (block D borders), lifted verbatim from tab_kable (derive-once, shared by
  # both render engines). A "total block" is a maximal run of total rows OR the reserved n/pvalue/
  # row_pct label rows; the first row of each run gets a top border, the last a bottom border.
  # WARNING: the c("n","pvalue","row_pct") whitelist is un-translated (English row labels) -> it
  # silently misses jamovi's gettext labels. Kept as-is for byte-identity; a real fix needs a per-row
  # role flag on the add_n/add_pct/pvalue rows rather than a value match on the row-label column.
  # Phase 12f: the reg GOF footer rows (row-label "N"/"AIC"/... ) form a total-block too -> the box.
  # A crosstab never contains those labels, so its tot_block is byte-identical (render-html unchanged).
  tot_block <- is_totrow(tab) |
    (!is_totrow(tab) & tab[[row_var_col]] %in% c("n", "pvalue", "row_pct", reg_footer_labels()))
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
    ~ fmt_col_ann(tab[[.x]], theme_cols, want_colors)
  )
  any_bg <- if (want_colors) any(purrr::map_lgl(ann, ~ isTRUE(.$has_bgc))) else FALSE

  # Phase 14q: the regression GOF FOOTER rows must read black + bold -- they are model-fit numbers,
  # not data to grey out so colours pop. A footer row is one where EVERY fmt cell is a footer stat
  # (display gof / pvalue / blank); a crosstab chi2 pvalue row is NOT (its non-pvalue cells stay
  # "pct"), so this never touches a crosstab and needs no reg gate. Un-grey the whole row in every
  # column's ann (font + keep_black, which the html engine reads) and mark it bold.
  footer_rows <- if (length(fmt_cols) > 0) {
    purrr::reduce(purrr::map(names(fmt_cols),
      ~ display_primary(get_display(tab[[.x]])) %in% c("gof", "pvalue", "blank")), `&`)
  } else logical(nrow(tab))
  if (any(footer_rows)) {
    ann <- purrr::map(ann, function(a) {
      a$keep_black[footer_rows] <- TRUE
      a$font[footer_rows]       <- theme_cols$text
      a$bold[footer_rows]       <- TRUE
      a
    })
  }

  # --- bold rows + bold cols (block D), reusing ann$ref_alltot ---
  ref_alltot_list <- purrr::map(ann, "ref_alltot")
  bold_rows <- if ("bold" %in% compute) {
    tab_bold_rows(ref_alltot_list, md_style = identical(backend, "md"))
  } else integer(0)
  # Phase 14q: footer rows' LABEL cells (row-var / level columns) bold too, matching the value cells.
  if ("bold" %in% compute && any(footer_rows)) bold_rows <- union(bold_rows, which(footer_rows))
  bold_cols <- if ("bold" %in% compute && length(ref_alltot_list) > 0) {
    names(which(purrr::map_lgl(ref_alltot_list, all)))
  } else character(0)

  range_totcol <- if ("range" %in% compute) {
    tab_totcol_range(tab, fmt_cols, col_var_map, totcols)
  } else NULL

  # Phase 13c-iii: the shared col_var HEADER model (spanning variable-name row + suffix-stripped level
  # labels), consumed by every exporter so the two header rows stay in sync (console is unchanged).
  # Phase 14i: `name_cols` is the col-side twin of the `var_names` row-side drop above. A blank `label`
  # is the WHOLE implementation: every backend already gates its spanning-name row on
  # `any(nzchar(label))` -- md (tab_md.R), kableExtra + the html engine (tab-render-html.R), and
  # tab_xl's `has_span` (which also drives its geometry offset). So no backend knows it exists.
  # Phase 14j moved the decision INTO the header builder, which also owns the level labels: dropping
  # the span changes what the level header must say (see tab_col_var_header()).
  col_var_header <- tab_col_var_header(
    tab, list(col_var_map = col_var_map, real_col_vars = real_col_vars, totcols = totcols,
              var_name_col = var_name_col, sd_cols = sd_cols),
    name_cols = var_names %in% c("both", "cols"))

  list(
    tab = tab,
    # Phase 14i: `row_vars` (the SOURCE names) and `compacted` are passed through, not re-derived.
    # tab_render_vars() has returned both since 14d, but this list dropped them -- so tab_xl's title
    # read "levels by relig" (the merge's own scaffolding column) instead of "race, marital by relig".
    # Both come from the `vars` ATTRIBUTE, so they are unaffected by the ungroup/drop/wrap above.
    vars = list(degrade = FALSE, row_var = row_var_name, tab_vars = tab_vars,
                row_vars = rv$row_vars, compacted = isTRUE(rv$compacted),
                col_vars = rv$col_vars, col_vars_levels = rv$col_vars_levels),
    roles = list(fmt_mask = fmt_mask, fmt_cols = fmt_cols, other_cols = other_cols,
                 row_var_col = row_var_col, totcols = totcols, totrows = totrows,
                 no_totrows = no_totrows, totblock_top = totblock_top,
                 totblock_bottom = totblock_bottom, real_col_vars = real_col_vars,
                 col_var_map = col_var_map, new_col_var = new_col_var,
                 new_group = new_group, align = align,
                 label_cols = label_cols, var_name_col = var_name_col,
                 label_runs = label_runs, sd_cols = sd_cols,
                 color_cols = color_cols, any_bg = any_bg, has_stars = has_stars),
    ann = ann,
    bold_rows = bold_rows,
    bold_cols = bold_cols,
    range_totcol = range_totcol,
    col_var_header = col_var_header,
    subtext = subtext
  )
}

# Phase 13c-iii: the shared col_var HEADER model. Per column, `label` = the spanning col_var NAME, shown
# only for a real-col_var LEVEL column -- blank for the row var, the count / all_col_vars column, and
# total columns (a total column is the row marginal, not a col_var level, so it stands alone). `clean` =
# the level name with its disambiguation "_<col_var>" suffix stripped (two col_vars sharing a level
# "Other" are stored uniquely as "Other_race"/"Other_grp"; exports show the bare "Other" under the
# variable-name span, per the maintainer's rule -- never print the suffix once the name is written).
#
# `name_cols` (Phase 14j) = will the spanning row actually be rendered (the col side of `var_names`)?
# It moved here from prep_one_table(), which blanked `label` after the fact, because the two decisions
# are ONE rule: a level header may name the STATISTIC ("mean") only when the span names the VARIABLE.
# Blanking after the fact left `var_names = "none"` + Excel with a column headed "mean" and the
# variable's name nowhere. Both `var_names` drops still live in the prep, so Phase 14i's property
# holds: no backend knows the argument exists.
#' @keywords internal
tab_col_var_header <- function(tab, roles, name_cols = TRUE) {
  nms   <- names(tab)
  cvm   <- roles$col_var_map
  real  <- roles$real_col_vars
  totc  <- seq_along(nms) %in% roles$totcols
  # a real col_var LEVEL column: not the row var / all_col_vars / "" (no span name), and not a total
  # column (the marginal, not a level). Kept separate from `label` because the rewrites below must run
  # even when nothing is NAMED -- a "_race" suffix is noise whatever `var_names` says.
  is_level <- (unname(cvm) %in% real) & !totc
  label    <- ifelse(is_level & isTRUE(name_cols), unname(cvm), "")
  clean <- nms
  # Phase 14i: the merged table's name column is headed by the literal "row_var" -- an internal name,
  # never informative, and the loop below never reaches it (it only visits LEVEL columns). Blanked
  # unconditionally: this is a bug fix, not a `var_names` setting. One line, and md's `col_names`,
  # kableExtra's `col.names`, the html engine's `head_cells` and tab_xl's `clean_names` all follow.
  clean[roles$var_name_col] <- ""
  for (j in which(is_level)) {
    suff <- paste0("_", cvm[[j]])
    if (endsWith(nms[j], suff)) {
      clean[j] <- substr(nms[j], 1L, nchar(nms[j]) - nchar(suff))
    } else if (isTRUE(name_cols) && identical(nms[j], cvm[[j]]) && is_fmt(tab[[j]]) &&
               identical(get_type(tab[[j]]), "mean")) {
      # A numeric col_var contributes a column bearing the VARIABLE's own name, so under its own span
      # the name was said twice ("tvhours" over "tvhours") -- three times in Excel, which also splits
      # off a "<var>_sd" sibling. The span says which variable; the level header says which STATISTIC.
      # NB a different question from `j %in% roles$sd_cols` below: this asks whether THIS mean has an
      # sd sibling to hand its "(sd)" tail to, not whether j is one.
      clean[j] <- if (paste0(cvm[[j]], "_sd") %in% nms) {
        "mean"                       # Excel: the sd is its own column, headed "sd" below
      } else if (mean_shows_sd(tab[[j]])) {
        "mean (sd)"                  # text backends: format() folds the sd into the cell, "1.7 (s2.1)"
      } else {
        "mean"
      }
    } else if (isTRUE(name_cols) && j %in% roles$sd_cols) {
      clean[j] <- "sd"
    }
  }
  # Phase 14s (L3): if EVERY level column's DISPLAYED header already equals its col_var, the spanning
  # name row would only duplicate the column headers -> drop it. A regression table named after the
  # model / outcome ("Married: OR" over "Married: OR") is the case this targets. Compare the CLEAN
  # header, not the raw name, so a numeric col_var (header "mean (sd)", col_var "tvhours") is NOT
  # dropped; a crosstab (level "Black" != col_var "race") is never affected.
  lvl <- which(is_level)
  if (length(lvl) > 0 && all(clean[lvl] == unname(cvm)[lvl])) label <- rep("", length(nms))
  list(label = label, clean = clean)
}

# Does this mean column actually render a "(sigma sd)" tail? THE SAME predicate format() uses for its
# `disp_mean_sd` mask (R/fmt_class.R), so the header and the cells cannot disagree: a mean cell shows
# its sd exactly when the display is "mean" and the var field is there.
#' @keywords internal
mean_shows_sd <- function(col) {
  any(get_display(col) == "mean" & !is.na(get_var(col)), na.rm = TRUE)
}

# Phase 13c-iii: run-length-encode the header `label` vector into (label, span) runs for the spanning
# header row -- blank runs keep the label "" (each exporter maps it to its own empty-cell form).
#' @keywords internal
tab_header_runs <- function(label) {
  r <- rle(label)
  list(labels = r$values, spans = r$lengths)
}


# === SECTION: shared option resolver (Phase 10j) ====================================

# Resolve the canonical shared export options ONCE (theme + the on/off toggles), so every exporter AND
# the tab_export() facade share one set of names, defaults and option fallbacks (killing the
# copy-pasted `match.arg(theme)` preambles). `color = FALSE` renders monochrome AND disables the colour
# legend (which would otherwise describe colours the cells no longer show). Returns a normalized scalar
# list.
#
# DESIGN: `theme = NULL` -> options("tabxplor.theme") is the package idiom (cf. engine / popover) and
# the only way to wire an option through match.arg().
# WARNING (Phase 13d): "auto" (follow the reader's colour scheme) is a RENDER intent, not a palette --
# only media that emit a stylesheet can honour it (tab_kable(engine = "html"), tab_md/tab_css). Static
# backends pass `allow_auto = FALSE` and get "light". Everything downstream of a palette lookup must go
# through tx_palette_theme() (R/tab-css.R), NOT this value.
# WARNING (Phase 14l): `color_type` is GONE. It was the 2nd positional arg, so every call site was
# converted to NAMED arguments in the same change -- do NOT reintroduce a positional call, it would
# shift every later toggle silently (color -> color_type, color_legend -> color, ...).
#' @keywords internal
resolve_export_opts <- function(theme = NULL,
                                color = TRUE, color_legend = TRUE,
                                transpose = FALSE, caption = NULL,
                                var_names = NULL,
                                allow_auto = FALSE) {
  if (is.null(theme)) theme <- getOption("tabxplor.theme", "light")
  theme <- match.arg(theme[1], c("light", "dark", "auto"))
  if (identical(theme, "auto") && !isTRUE(allow_auto)) theme <- "light"
  if (is.null(var_names)) var_names <- getOption("tabxplor.var_names", "both")
  var_names <- match.arg(var_names[1], c("both", "rows", "cols", "none"))
  color <- isTRUE(color)
  list(theme = theme,
       color = color, color_legend = isTRUE(color_legend) && color,
       transpose = isTRUE(transpose), caption = caption, var_names = var_names)
}


# The single exporter-prep entry point. Returns a `tabxplor_render` (an ephemeral tagged list; see the
# file header). `compute` gates the expensive derivations so a backend / jamovi live path opts out of
# what it does not use. `transpose = TRUE` (Phase 14o) flips the FINISHED render model of each table via
# tx_transpose_render() -- colours and cell strings are computed per (correct, homogeneous) source
# column, then rows and columns swap as plain data (see R/tab-transpose-render.R). Materialise runs
# "xl"-style when transposing so add_n is an `n` COLUMN that flips into an `n` ROW.
#' @keywords internal
tab_export_prep <- function(tabs,
                            backend       = c("kable", "md", "plot", "xl"),
                            drop_tab_vars = TRUE,
                            wrap          = NULL,
                            compute       = NULL,
                            theme         = "light",
                            color_legend  = TRUE,
                            transpose     = FALSE,
                            var_names     = "both",
                            list_method   = FALSE,
                            what          = NULL) {
  backend   <- match.arg(backend)
  var_names <- match.arg(var_names[1], c("both", "rows", "cols", "none"))
  if (is.null(what)) what <- paste0("tab_", backend, "()")
  if (is.null(compute)) {
    compute <- if (backend %in% c("kable", "plot")) {
      c("refs", "colors", "bold", "range")
    } else c("refs", "bold")  # md / xl
  }

  # Phase 13d: `theme` may be the render intent "auto"; a PALETTE is always light/dark. Resolve once
  # here so theme_cols (and fmt_col_ann -> fmt_channel_codes -> get_color_style, which reads
  # theme_cols$theme) can never be handed "auto" -- get_color_style() would build the key "text_auto",
  # find no palette and error. `meta$theme` below keeps the intent for the renderer.
  # WARNING: the hex here MUST stay in sync with tx_chrome_hex() (R/tab-css.R), which emits the same
  # colours as CSS rules for the html engine. Both read tx_chrome_hex(); do not inline literals.
  pal_theme  <- tx_palette_theme(theme[1])
  chrome     <- tx_chrome_hex(pal_theme)
  theme_cols <- list(
    theme = pal_theme,
    text  = chrome$text,
    grey  = chrome$grey,
    grey2 = chrome$grey2
  )

  resolved <- tab_resolve_tables(tabs, list_method = list_method, what = what)

  # Phase 10i-B: hydrate the "core" table into its rendered shape ONCE, on the still-grouped resolved
  # tables (before prep_one_table ungroups), so p-value rows + add_n/add_pct are real rows/cols the
  # role detection then sees. "xl" keeps a real `n` column; every other backend folds add_n into the
  # Total cell (backend "text").
  # Phase 14o: when transposing, materialise "xl"-style FOR EVERY backend, so `add_n` is a real `n`
  # COLUMN that flips into an `n` ROW (matching a native pct = "col" table) instead of a folded
  # "100% (n=849)" cell -- and 14n has already collapsed the redundant per-block Total rows to one, so
  # the single Total row flips to a single Total column. This supersedes 14d's transpose-before-
  # materialise: the flip is now a render-model transform (below), oriented for free.
  mat_backend <- if (identical(backend, "xl") || isTRUE(transpose)) "xl" else "text"
  resolved <- purrr::map(resolved, tab_materialize_extras, backend = mat_backend, pvalue = TRUE)

  tables <- purrr::map(
    resolved,
    ~ prep_one_table(.x, backend = backend, drop_tab_vars = drop_tab_vars,
                     wrap = wrap, compute = compute, theme_cols = theme_cols,
                     var_names = var_names)
  )

  # Phase 14o: opt-in transpose-at-export (all four exporters share this seam; console never transposes).
  # The flip runs on the FINISHED render model -- colours and cell strings are computed per (correct,
  # homogeneous) source column, then rows and columns are swapped as plain data. Doing it on the
  # tabxplor_fmt fields (the old tab_transpose()) mis-coloured numeric cells, because a transposed
  # column is heterogeneous and one fmt column cannot carry two type/color values. See tx_transpose_render().
  if (isTRUE(transpose)) {
    tables <- purrr::map(tables, tx_transpose_render, backend = backend,
                         meta = list(theme_cols = theme_cols))
  }

  structure(
    list(
      tables = tables,
      meta = list(backend = backend, theme = theme[1],
                  color_legend = color_legend, theme_cols = theme_cols,
                  compute = compute)
    ),
    class = "tabxplor_render"
  )
}

# Phase 14j: tab_export_labels() was DELETED here. It walked every column of every table harvesting
# `attr(., "label")` on 100% of export paths, and nothing ever read the `prep$labels` it filled -- in
# practice it returned NULL anyway, because the source `label` attribute does not survive tab()
# building. It was built for a "label -> header tooltip" feature that was never wired for exactly that
# reason; reviving it needs core-pipeline plumbing first, at which point it belongs with that work.
