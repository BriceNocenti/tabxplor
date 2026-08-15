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
    purrr::map(~ .[is_real_col_var(.)])                          # 19m-i: the declared set
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
    face      <- codes$text_face
  } else {
    text_hex  <- rep(NA_character_, length(col))
    bg_hex    <- rep(NA_character_, length(col))
    text_slot <- integer(length(col))
    bg_slot   <- integer(length(col))
    face      <- list(bold = logical(length(col)), italic = logical(length(col)),
                      underline = logical(length(col)))
  }

  list(
    ref_alltot = ref_alltot,
    ref_cells  = ref_cells,
    text_hex   = text_hex,
    bg_hex     = bg_hex,
    text_slot  = text_slot,
    bg_slot    = bg_slot,
    keep_black = keep_black,
    # Phase 18z11: the palette's own TYPOGRAPHY for this cell's text slot, kept FLAT (three vectors,
    # not a nested list) because tx_transpose_render() flips per-cell logicals with a flat helper.
    # These are the MEASURE's face only -- `keep_black` (the structural reference/total bold) is folded
    # into `bold` below and deliberately not into these, since tab_plot's structural bolding is a
    # row/column SET, not a per-cell flag.
    face_bold      = face$bold,
    face_italic    = face$italic,
    face_underline = face$underline,
    font = dplyr::case_when(!is.na(text_hex) ~ text_hex,
                            keep_black       ~ theme_cols$text,
                            TRUE             ~ grey_this),
    back = dplyr::if_else(is.na(bg_hex), "none", bg_hex),
    # z11: was `!is.na(text_hex) | keep_black` -- a HEX heuristic that collapses in a palette whose
    # every text hex is black. The palette declares it now. Byte-identical for light/dark, where
    # face_bold is TRUE at exactly the slots that carry a hex.
    bold = face$bold | keep_black,
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


# The shared bold-row set (block D): a row is bold iff it is a reference/total ANCHOR in EVERY
# DISCRIMINATING column (a column that is all-anchor or all-non-anchor says nothing about which ROWS
# are references, so it is dropped first). The per-column signal is `anchor = ref_alltot | is_refrow`
# (fmt_col_ann) -- the SAME signal the per-cell keep_black uses -- so a totals-free regression table
# (whose ref_alltot is empty but whose reference CATEGORIES carry in_refrow) still bolds its reference
# rows. For crosstabs is_refrow is a subset of ref_alltot, so this is byte-identical there.
# WARNING (Phase 18m): when NO column discriminates the result is integer(0) (no anchor rows) --
# universally, not just for md. The old `rowSums == ncol` edge flagged EVERY row when 0 columns
# survived; that bolted the whole table on a binomial exponentiate=FALSE + empirical reg (all columns
# non-discriminating on ref_alltot). Reference rows now stay bold via the in_refrow signal instead.
#' @keywords internal
tab_bold_rows <- function(anchor_list) {
  if (length(anchor_list) == 0) return(integer(0))
  refref <- as.data.frame(anchor_list)
  keep   <- purrr::map_lgl(refref, ~ any(.) & !all(.))
  if (!any(keep)) return(integer(0))
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


# === SECTION: total-column base range [min;max] -- DORMANT (block B, decisions.md Sec 10) =======

# DORMANT -- possible future implementation. The retired option `tabxplor.totcol_range` once drove
# this: when col_vars have differing percentage bases (chiefly na="drop" with different NA rates),
# one Total column must summarise K row bases. This TABLE-LEVEL pre-pass (format() sees one column
# at a time and cannot) returns, per data row, the scalar base when all col_vars agree, else the
# range. base per cell = get_tot_n() for row/col/all types, get_n() for means.
# `style`: "range" (default) -> "[min;max]", "min" -> the smallest (safest) base, "off" -> uniform
# (no per-row range). NO site reads the option any more: the console fold branch
# (tab.R tab_fold_addn_incell) and the render-model compute (range_totcol, below) are both
# commented out -- the per-row literal templates it emitted defeated the composite-token padding
# (format.tabxplor_fmt aligns per unique template) and no renderer ever consumed range_totcol.
# The helper is kept honest by direct tests in test-export-prep.R.
#' @keywords internal
tab_totcol_range <- function(tab, fmt_cols, col_var_map, totcols,
                             style = "range") {
  n_row <- nrow(tab)
  empty <- list(col = totcols, text = character(0), differ = logical(0))
  if (identical(style, "off")) return(empty)
  if (length(fmt_cols) == 0 || length(totcols) == 0) return(empty)

  # One representative base per col_var per row: within a col_var+row the base is constant, so read
  # each col_var's total column (or its first fmt column) once. NA bases (mean cells with no base)
  # are ignored in the min/max.
  cvs <- unique(col_var_map[fmt_cols])
  cvs <- cvs[is_real_col_var(cvs)]
  if (length(cvs) == 0) return(empty)

  base_of <- function(col) {
    if (fmt_var_kind(col) == "mean") get_n(col) else get_tot_n(col)
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
  # Phase 16a: a regression `split_var` (the population each model was fit on) is a tab_var, but -- unlike
  # a crosstab tab_var whose level rides on a Total row -- it has NO Total row to carry its level, so
  # dropping it loses that information entirely. Keep it (rendered as a vertical/merged name column
  # below, like a merged row_var name) even when the other tab_vars are dropped for html/Excel.
  split_var_col <- intersect(reg_call(tab)$split_var, tab_vars)
  subtext  <- get_subtext(tab) |> purrr::discard(\(s) s == "")

  # Phase 14v: resolve the multinomial crude-companion tooltip fragments to a per-column, per-ROW list
  # NOW -- while the predictor `var` column is still present (drop_tab_vars removes it below). Keyed by
  # (var, level); NULL on a crosstab. ungroup / drop / wrap never reorder rows, so the per-row vectors
  # stay aligned to the final tab. The render then indexes by column name (no `var` needed downstream).
  emp_tips <- NULL
  et_raw   <- get_empirical_tips(tab)
  if (!is.null(et_raw)) {
    lvl0 <- as.character(tab[[rv$row_var]])
    # 19l: the variable column is the DECLARED one (rv$var_col, used again at the label runs below),
    # not a column that happens to be named "var" -- this was the last consumer sniffing for the name.
    vcol <- intersect(rv$var_col, names(tab))
    var0 <- if (length(vcol)) as.character(tab[[vcol[[1]]]]) else rep(NA_character_, nrow(tab))
    key0 <- paste(var0, lvl0, sep = "\r")
    emp_tips <- lapply(split(et_raw, et_raw$col), function(sub)
      sub$tip[match(key0, paste(sub$var, sub$level, sep = "\r"))])
  }

  # group boundaries -- computed BEFORE ungroup (needs the grouping)
  gi        <- dplyr::group_indices(tab)
  new_group <- which(gi != dplyr::lead(gi, default = max(gi) + 1L))

  tab <- dplyr::ungroup(tab)
  if (drop_tab_vars && length(tab_vars) > 0) {
    drop_these <- setdiff(tab_vars, split_var_col)   # Phase 16a: keep a reg split_var
    if (length(drop_these) > 0) tab <- dplyr::select(tab, -tidyselect::all_of(drop_these))
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
  # Phase 19f: the DECLARED "var"-role column, whatever it is called -- `row_var` on a merged
  # crosstab, `var` on a regression (where it used to masquerade as a sub-table variable).
  name_col <- intersect(rv$var_col, names(tab))
  if (length(name_col) > 0 && !var_names %in% c("both", "rows")) {
    tab      <- dplyr::select(tab, -tidyselect::all_of(name_col))
    name_col <- character(0)
  }
  # Phase k: a merged (>=2 row_vars) table shows the SOURCE variable names as the values of the synthetic
  # `row_var` column. Swap those values for variable labels when tabxplor.var_labels is on (display only;
  # the swap happens before wrap so a long label wraps too). No-op off / when no label is recorded.
  if (length(name_col) > 0)
    tab[[name_col]] <- var_label_display(as.character(tab[[name_col]]), tab)
  if (!is.null(wrap)) {
    pre_wrap_names <- names(tab)
    tab <- tab_wrap_text(tab,
                         wrap_rows          = wrap$rows,
                         wrap_cols          = wrap$cols,
                         exdent             = wrap$exdent,
                         whitespace_only    = wrap$whitespace_only,
                         unbreakable_spaces = wrap$unbreakable_spaces,
                         brk                = wrap$brk)
    # Phase 14v: tab_wrap_text RENAMES columns (spaces -> unbreakable U+202F, long names wrapped), so the
    # emp_tips keys (build-time column names) must follow, or the render lookup by the wrapped name fails.
    # Phase 19m-iii: a key absent from `pre_wrap_names` used to become NA SILENTLY, which blanks that
    # column's tooltips with no trace. 19m-i measured the miss as unreachable today (every row- and
    # column-moving step runs strictly before the capture above), so this is not a fix but a degrade:
    # a key the rename cannot follow keeps its OLD name, and at worst its tooltip does not attach.
    if (!is.null(emp_tips)) {
      renamed <- stats::setNames(names(tab), pre_wrap_names)[names(emp_tips)]
      names(emp_tips) <- ifelse(is.na(renamed), names(emp_tips), renamed)
    }
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
    any(vapply(fmt_cols, function(j) fmt_stars_applicable(tab[[j]]) &&
                                     any(nzchar(get_stars(tab[[j]]))), logical(1)))

  col_var_map   <- get_col_var(tab)
  real_col_vars <- unique(col_var_map[fmt_mask])
  # Phase 14p: `no_col_var` (the sentinel a no-col_var table's `n`/`pct`/`wn` columns carry) is NOT a
  # real variable name -- rendering it as a spanning col_var header is noise. Excluded here so
  # tab_col_var_header() marks those columns `is_level = FALSE` (no span label).
  # Phase 19m-i: this site spelled the whole placeholder set and was the only one that did; the set
  # is declared once now, as TAB_PLACEHOLDER_COL_VARS (R/fmt_class.R).
  real_col_vars <- real_col_vars[is_real_col_var(real_col_vars)]

  # DECLARED: which columns name a colour measure at all -- the LEGEND's gate (it describes the
  # scheme, so it shows even if every cell happens to land in slot 0). Its realised twin, "does any
  # cell actually carry a colour", is roles$has_color -- see roles_color_flags() below.
  color_cols <- get_color(tab)
  color_cols <- which(!color_cols %in% c("", "no") & !is.na(color_cols))

  totcols    <- which(is_totcol(tab))
  totrows    <- which(is_totrow(tab))

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
  # Phase 16a: a kept reg split_var also rotates vertical (via var_name_col), but is NOT added to
  # `name_col` so `var_names` never drops it (its levels are data, not a variable name).
  var_name_col <- label_cols[names(label_cols) %in% c(name_col, split_var_col)]
  label_runs  <- tab_label_runs(tab, label_names)

  # Phase 14l: the Excel-only "<var>_sd" siblings tab_materialize_extras(backend = "xl") splits off a
  # numeric mean column into. ONE definition, read by tab_col_var_header() (which heads them "sd") and
  # by tab_xl's column widths -- the rule used to be re-derived at each site.
  # Naturally integer(0) for every other backend: nothing else creates those columns.
  # WARNING: ungated by `var_names`, unlike the header rewrite. A width is not a naming decision, so
  # `var_names = "none"` must still get a narrow sd column.
  sd_cols <- fmt_cols[vapply(tab[fmt_cols], \(col) identical(get_role(col), "sd"), logical(1))]

  # Total-BLOCK border rows (block D borders), lifted verbatim from tab_kable (derive-once, shared by
  # both render engines). A "total block" is a maximal run of total rows OR the synthetic n / pvalue /
  # row_pct / reg-GOF rows; the first row of each run gets a top border, the last a bottom border.
  # Phase 19f: read the row's own `row_kind` (tab_row_roles) instead of matching un-translated English row
  # labels -- so it no longer silently misses jamovi's gettext labels (the role model retires the
  # c("n","pvalue","row_pct", reg_footer_labels()) whitelist), and it now rides every slice of the table.
  tot_block <- tab_row_roles(tab) != "data"
  tb_edges  <- roles_totblock_edges(tot_block)
  totblock_top    <- tb_edges$top
  totblock_bottom <- tb_edges$bottom

  # kable/plot col_var transition index (one-liner). md keeps its own real-col_var span loop.
  new_col_var <- roles_col_var_edges(col_var_map, other_cols, side = "right")

  align <- purrr::map_chr(
    tab, ~ dplyr::if_else(is_fmt(.) | is.numeric(.), "r", "l")
  )

  # --- per-column ann (derive-once) ---
  want_colors <- "colors" %in% compute
  ann <- purrr::map(
    stats::setNames(names(fmt_cols), names(fmt_cols)),
    ~ fmt_col_ann(tab[[.x]], theme_cols, want_colors)
  )
  color_flags <- roles_color_flags(ann, color_cols)
  any_bg      <- color_flags$any_bg
  # The row-ANCHOR signal for the shared bold-row set: a cell anchors a row when it is a
  # reference/total (`ref_alltot`) OR a regression reference CATEGORY (`in_refrow`, which a
  # totals-free reg column carries but ref_alltot misses). For crosstabs is_refrow is a subset of
  # ref_alltot, so this equals ref_alltot there. Captured HERE, BEFORE the footer override below --
  # tab_bold_rows() needs the pure signal. Phase 19h: a LOCAL, not the shipped `ann$anchor` slot it
  # used to be; no backend ever read that slot, and the transpose dropped it silently.
  anchors <- purrr::map(ann, "keep_black")

  # Phase 14q: the regression GOF FOOTER rows must read black + bold -- they are model-fit numbers,
  # not data to grey out so colours pop. A footer row is one where EVERY fmt cell is a footer stat
  # (display gof / pvalue / blank); a crosstab chi2 pvalue row is NOT (its non-pvalue cells stay
  # "pct"), so this never touches a crosstab and needs no reg gate. Un-grey the whole row in every
  # column's ann (font + keep_black, which the html engine reads) and mark it bold.
  footer_rows <- if (length(fmt_cols) > 0) {
    purrr::reduce(purrr::map(names(fmt_cols),
      ~ display_primary(get_display(tab[[.x]])) %in% DISPLAY_FOOTER_TOKENS), `&`)
  } else logical(nrow(tab))
  if (any(footer_rows)) {
    ann <- purrr::map(ann, function(a) {
      a$keep_black[footer_rows] <- TRUE
      a$font[footer_rows]       <- theme_cols$text
      a$bold[footer_rows]       <- TRUE
      a
    })
  }

  # --- bold rows + bold cols (block D), from the pure `anchors` signal / ann$ref_alltot ---
  ref_alltot_list <- purrr::map(ann, "ref_alltot")
  bold_rows <- if ("bold" %in% compute) {
    tab_bold_rows(anchors)
  } else integer(0)
  # Phase 14q: footer rows' LABEL cells (row-var / level columns) bold too, matching the value cells.
  if ("bold" %in% compute && any(footer_rows)) bold_rows <- union(bold_rows, which(footer_rows))
  bold_cols <- if ("bold" %in% compute && length(ref_alltot_list) > 0) {
    names(which(purrr::map_lgl(ref_alltot_list, all)))
  } else character(0)

  # Phase 18m: the pct = "col" "n" (count) ROW is minted from the sub-table Total row, so its cell in
  # the total COLUMN (a column ATTRIBUTE, not clearable per cell) falls into the all_totals anchor and
  # renders bold. It is a base-count row, not a reading anchor -> force plain weight, keyed on the stored
  # `n` role (17c), in both the whole-row set and the per-cell ann the html/md engines read.
  if ("bold" %in% compute) {
    n_rows <- which(tab_row_roles(tab) == "n")
    if (length(n_rows)) {
      ann       <- purrr::map(ann, function(a) { a$bold[n_rows] <- FALSE; a })
      bold_rows <- setdiff(bold_rows, n_rows)
    }
  }

  # DORMANT (possible future implementation, retired tabxplor.totcol_range): no renderer ever
  # consumed range_totcol, so the compute is off. The named NULL slot stays in the model below.
  # range_totcol <- if ("range" %in% compute) {
  #   tab_totcol_range(tab, fmt_cols, col_var_map, totcols)
  # } else NULL
  range_totcol <- NULL

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
  # Phase k: a single-row_var table heads the row-label column with the variable name -- swap it for the
  # variable label (display only) when tabxplor.var_labels is on. The merged case has no such header
  # (blanked at var_name_col); "levels" is not a variable name, so it never matches a recorded label.
  if (length(name_col) == 0 && length(row_var_col) == 1)
    col_var_header$clean[row_var_col] <- var_label_display(col_var_header$clean[row_var_col], tab)

  list(
    tab = tab,
    # Phase 14i: `row_vars` (the SOURCE names) and `compacted` are passed through, not re-derived.
    # tab_render_vars() has returned both since 14d, but this list dropped them -- so tab_xl's title
    # read "levels by relig" (the merge's own scaffolding column) instead of "race, marital by relig".
    # Both come from the `vars` ATTRIBUTE, so they are unaffected by the ungroup/drop/wrap above.
    vars = list(degrade = FALSE, row_var = row_var_name, tab_vars = tab_vars,
                row_vars = rv$row_vars, compacted = isTRUE(rv$compacted),
                var_col = rv$var_col, col_vars = rv$col_vars),
    roles = list(fmt_mask = fmt_mask, fmt_cols = fmt_cols, other_cols = other_cols,
                 row_var_col = row_var_col, totcols = totcols, totrows = totrows,
                 totblock_top = totblock_top,
                 totblock_bottom = totblock_bottom, real_col_vars = real_col_vars,
                 col_var_map = col_var_map, new_col_var = new_col_var,
                 new_group = new_group, align = align,
                 label_cols = label_cols, var_name_col = var_name_col,
                 label_runs = label_runs, sd_cols = sd_cols,
                 color_cols = color_flags$color_cols, any_bg = color_flags$any_bg,
                 has_color = color_flags$has_color, has_stars = has_stars),
    ann = ann,
    bold_rows = bold_rows,
    bold_cols = bold_cols,
    range_totcol = range_totcol,
    col_var_header = col_var_header,
    subtext = subtext,
    # Phase 16e: the plain footer one-liners (weight / Model: / stars) are no longer pre-computed here --
    # every backend now builds its whole footer through tab_footer_streams(), the ONE shared ordered model.
    # Phase 14w (item 1): the regression title/caption stays -- that is the CAPTION (above the table), used
    # when the exporter has no user caption. NA on a crosstab (those keep their own caption / auto-title path).
    reg_title = reg_title(reg_call(tab)),
    # Phase 17b: a stored caption (set_caption(), in meta$vars$caption) survives the pipeline and takes
    # precedence over reg_title when the exporter's own caption= is not supplied. NULL when none stored.
    caption = get_caption(tab),
    # Phase 14v: multinomial crude-companion tooltip fragments, per column -> per-row char vector
    # (resolved above while `var` was present); reg_append_empirical_tip() appends them at html render.
    empirical_tips = emp_tips
  )
}

# Phase k: the opt-in variable-NAME -> variable-LABEL display map. Reads the labels captured at build
# (meta$vars$var_labels), gated by the tabxplor.var_labels option. DISPLAY ONLY -- the tibble structure
# (col_var attr, row_var column values, column names) keeps canonical names, so select()/reference by
# name still work. Returns `x` unchanged when the option is off or no label is recorded for that name;
# any element whose value IS a recorded variable name is swapped for its label. Shared by the col-var
# span header, the single-row_var header, and the merged row_var name column.
#' @keywords internal
var_label_display <- function(x, tab) {
  if (!isTRUE(getOption("tabxplor.var_labels", FALSE))) return(x)
  labs <- get_vars_attr(tab)[["var_labels"]]
  if (is.null(labs) || length(labs) == 0L) return(x)
  hit <- !is.na(x) & x %in% names(labs)
  if (any(hit)) x[hit] <- unname(labs[x[hit]])
  x
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
  # Phase 19l: which col_vars have an Excel sd twin beside their mean, read off the twins' DECLARED
  # role (roles$sd_cols) rather than by re-minting their "<col_var>_sd" name below.
  sd_of <- unname(cvm[roles$sd_cols])
  # a real col_var LEVEL column: not the row var / all_col_vars / "" (no span name), and not a total
  # column (the marginal, not a level). Kept separate from `label` because the rewrites below must run
  # even when nothing is NAMED -- a "_race" suffix is noise whatever `var_names` says.
  is_level <- (unname(cvm) %in% real) & !totc
  # Phase k: the spanning col_var name shows the variable LABEL when tabxplor.var_labels is on (display
  # only -- `cvm` stays the raw name for the suffix-strip + dedup logic below, which is structural).
  label    <- ifelse(is_level & isTRUE(name_cols), var_label_display(unname(cvm), tab), "")
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
               identical(fmt_var_kind(tab[[j]]), "mean")) {
      # A numeric col_var contributes a column bearing the VARIABLE's own name, so under its own span
      # the name was said twice ("tvhours" over "tvhours") -- three times in Excel, which also splits
      # off a "<var>_sd" sibling. The span says which variable; the level header says which STATISTIC.
      # NB a different question from `j %in% roles$sd_cols` below: this asks whether THIS mean has an
      # sd sibling to hand its "(sd)" tail to, not whether j is one.
      clean[j] <- if (cvm[[j]] %in% sd_of) {
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
  # Phase g: a regression column disambiguated across several dependents carries a trailing " [dep]"
  # bracket in its stored name ("Model_OR [married]"), so the console can tell columns apart. The col_var
  # span row already names the outcome, so the exported level header strips it. Role-driven (17c): only
  # "model"/"emp" columns are touched, never a crosstab level that happens to hold brackets.
  for (j in which(is_level)) {
    # Phase 18z13: "n" too -- the per-level count column is disambiguated across dependents by the
    # same bracket, for the same reason (the console needs to tell two outcomes' counts apart).
    if (is_fmt(tab[[j]]) && get_role(tab[[j]]) %in% c("model", "emp", "n"))
      clean[j] <- tx_strip_dep_suffix(clean[j])
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

# Phase 17g: the ONE footer invocation. Every backend built its footer with the identical
# render_footer(tab_footer_streams(...)) sandwich, differing only by medium / colour-legend gate /
# subtext / html-classes. `src` is the fmt SOURCE table (rd$color_src for a transposed model, whose
# rd$tab is plain character; else rd$tab): weight / Model: / stars / legend all read its attributes.
# `want_legend` (caller-computed, since the colour-legend guard differs per backend) gates ONLY the
# colour legend -- the other streams always render. Returns the rendered footer for `medium`.
#' @keywords internal
rd_footer <- function(src, medium, theme = NULL, want_legend = TRUE,
                      subtext = character(0), lang = NULL, classes = FALSE) {
  suppressWarnings(render_footer(
    tab_footer_streams(src, style = legend_export_style(), lang = lang,
                       subtext = subtext, legend = want_legend,
                       # z11: the shade NAMES are a palette fact, decided while the tokens are built --
                       # a print legend says "Bold"/"Italic" where a colour one says "Shades of blue".
                       theme = tx_palette_theme(theme)),
    medium = medium, theme = theme, classes = classes))
}

# Phase 17g: the ONE caption fallback -- user caption=, else a stored set_caption() (rd$caption),
# else a regression table's auto-title (rd$reg_title). Phase 19h: `fallback` is what let tab_xl()
# join it: a workbook sheet has two further fallbacks (a NAMED tabxplor_tabs element, then the
# auto-generated "<row_var> by <col_vars>" title), which are xl's own policy but not a second
# caption RULE. A closure, so the fallback is only computed when the caption is genuinely absent.
#' @keywords internal
rd_caption <- function(rd, user_caption = NULL, fallback = NULL) {
  cap <- user_caption
  if (is.null(cap)) cap <- rd$caption
  if (is.null(cap) && !is.null(rd$reg_title) && !is.na(rd$reg_title)) cap <- rd$reg_title
  if (is.null(cap) && is.function(fallback)) cap <- fallback()
  cap
}

# roles_col_var_edges() -- Phase 19h: THE col_var transition index, in the roles_totblock_edges()
# idiom. Three backends need a boundary between two column-variable blocks and each derived it
# itself, from the SAME seed (`col_var_map`, with the non-col_var columns standing for themselves)
# but with three conventions that were never stated side by side:
#
#   side = "right"  the LAST column of each group   (kable/plot: a right border)      -- lead()
#   side = "left"   the FIRST column of each group  (Excel: a left border)            -- lag()
#   real_only       count a transition only between two REAL col_vars (md's span separators), so a
#                   helper column (`n`, a total) never opens a new block
#
# One derivation, three declared variants; the conventions are now readable in one place.
#' @keywords internal
roles_col_var_edges <- function(col_var_map, other_cols = NULL, real_col_vars = NULL,
                                side = c("right", "left"), real_only = FALSE) {
  side <- match.arg(side)
  cv <- col_var_map
  if (length(other_cols)) cv[names(other_cols)] <- names(other_cols)
  if (length(cv) == 0L) return(integer(0))
  if (real_only) {
    if (length(cv) < 2L) return(integer(0))
    k    <- seq_along(cv)[-1]
    prev <- cv[k - 1]; curr <- cv[k]
    hit  <- prev %in% real_col_vars & curr %in% real_col_vars & prev != curr
    return(unname(if (side == "right") (k - 1L)[hit] else k[hit]))
  }
  if (side == "right") which(cv != dplyr::lead(cv, default = "._at_the_end"))
  else                 which(nzchar(cv) & cv != dplyr::lag(cv, default = NA_character_))
}

# tx_strip_dep_suffix() -- Phase 19h: the trailing " [dependent]" disambiguation bracket, removed in
# ONE place. A regression column built across several outcomes carries it in its stored NAME
# ("Model_OR [married]") so the console can tell two outcomes' columns apart; wherever the outcome is
# already named -- the col_var span row above the level header, the colour legend -- it is noise.
# The regex was written twice, each copy commenting that the other existed. The two GATES stay local:
# the header strip keys on the column's own role, the legend on the group carrying any role at all.
#' @keywords internal
tx_strip_dep_suffix <- function(x) sub(" \\[[^]]*\\]$", "", x)

# tx_num_font() -- Phase 19h: THE number-font rule, which is one DECISION written twice.
#
# Measured, the three options are NOT one option -- they are three incompatible value syntaxes: a CSS
# font stack (html/md), a single xlsx font NAME (Excel has no fallback list, so the option IS the
# fallback), and a graphics family (ggpubr). What IS duplicated is the rule: **switch to a monospace
# font when the table shows significance stars**, so the stars cannot push the digits out of column.
# html/md has been unconditionally monospace since Phase g and so needs no switch; Excel and the plot
# still choose, and both wrote the choice out themselves.
#
# `has_stars` is roles$has_stars -- already in the render model, computed once from the cells.
#' @keywords internal
tx_num_font <- function(medium = c("html", "xl", "plot"), has_stars = FALSE,
                        plain = NULL, stars = NULL) {
  switch(match.arg(medium),
    html = getOption("tabxplor.tab_kable_num_font", tx_num_font_html_stars),
    xl   = if (isTRUE(has_stars)) stars %||% getOption("tabxplor.xl_font_num_stars", "Cascadia Mono")
           else                   plain %||% getOption("tabxplor.xl_font_num", "DejaVu Sans"),
    # "" keeps the ggpubr default: tab_plot() has no per-column font, so a plain table is left alone
    plot = if (isTRUE(has_stars)) getOption("tabxplor.plot_num_font", "Cascadia Mono") else "")
}

# roles_color_flags() -- Phase 19h: THE colour flags of the render model, one producer for the prep
# and the transpose (which used to define both a third way -- realised where the prep was declared,
# so a column whose every cell landed in slot 0 counted as coloured before the flip and not after).
#
# The two questions are genuinely different and now have different NAMES:
#   color_cols  DECLARED -- which columns name a colour measure. The LEGEND's gate: it describes the
#               scheme, so it prints even when no cell happens to reach a break.
#   has_color   REALISED -- does any cell actually carry a colour. The gate for emitting spans / CSS,
#               and it is FALSE whenever the caller did not ask for colours at all (`compute`).
#   any_bg      realised, for the background channel alone (kableExtra's fill argument, plot fills).
#' @keywords internal
roles_color_flags <- function(ann, color_cols) {
  list(color_cols = color_cols,
       any_bg     = any(vapply(ann, function(a) isTRUE(a$has_bgc) , logical(1))),
       has_color  = any(vapply(ann, function(a) isTRUE(a$has_color), logical(1))))
}

# Phase 17g: the top/bottom border rows of each "total block" -- a maximal run of TRUE in `in_block`
# (total rows + the synthetic n / pvalue / row_pct / reg-GOF rows). First row of a run gets a top
# border, last a bottom border. The formula is a fact of the render model, so it lives ONCE: shared by
# prep_one_table() (block from the row_kind field, tab_row_roles) and tx_transpose_render() (block from the
# flipped indices). NOTE the rest of the two role models are DIFFERENT computations -- prep derives
# roles from the fmt table (is_fmt/is_totcol/...), transpose from the flipped positional grid -- so a
# single roles_from(tab) builder does not fit without rewriting the golden-locked transpose path; only
# this genuinely-identical derivation is single-sourced.
#' @keywords internal
roles_totblock_edges <- function(in_block) {
  list(
    top    = which(dplyr::if_else(in_block, !dplyr::lag(in_block), FALSE)),
    bottom = which(dplyr::if_else(in_block, !dplyr::lead(in_block, default = FALSE), FALSE))
  )
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
# only media that emit a stylesheet can honour it (tab_html(), tab_md/tab_css). Static
# backends pass `allow_auto = FALSE` and get "light". Everything downstream of a palette lookup must go
# through tx_palette_theme() (R/tab-css.R), NOT this value.
# WARNING (Phase 14l): `color_type` is GONE. It was the 2nd positional arg, so every call site was
# converted to NAMED arguments in the same change -- do NOT reintroduce a positional call, it would
# shift every later toggle silently (color -> color_type, color_legend -> color, ...).
# NOTE (Phase 18z11): the allow_auto gate below tests "auto" SPECIFICALLY, so the new "print" theme
# reaches every backend including the static ones (tab_xl, tab_plot) -- which is right: "print" is a
# palette, not a render intent, and Excel is exactly where a publication table is wanted.
#' @keywords internal
resolve_export_opts <- function(theme = NULL,
                                color = TRUE, color_legend = TRUE,
                                transpose = FALSE, caption = NULL,
                                var_names = NULL,
                                allow_auto = FALSE) {
  theme <- tx_theme_resolve(theme, allow_auto = allow_auto)
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
      c("refs", "colors", "bold")  # "range" DORMANT (retired totcol_range)
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
    tables <- purrr::map(tables, tx_transpose_render, backend = backend)
  }

  # Phase 18a bug-fix: decide the graceful-degrade NOTICE once for the whole render batch. A degraded
  # (non-tabxplor) sub-table is only worth flagging when the batch holds NO real fmt table -- otherwise
  # the "no tabxplor_fmt columns" message is MISLEADING (a formatted table IS shown alongside it). And
  # then flag it only ONCE, not per degraded table. Each backend gates its tab_degrade_inform() on
  # vars$notify (a single non-tabxplor input still informs: it is the sole, all-degraded table).
  # tx_transpose_render() returns a degraded rd unchanged, so this survives the transpose flip above.
  degraded <- purrr::map_lgl(tables, ~ isTRUE(.x$vars$degrade))
  notify_i <- if (any(degraded) && !any(!degraded)) which(degraded)[1] else 0L
  for (i in seq_along(tables)) {
    if (isTRUE(tables[[i]]$vars$degrade)) tables[[i]]$vars$notify <- (i == notify_i)
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
