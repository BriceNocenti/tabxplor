# PURPOSE: the ONE preparation step every exporter runs, and the ephemeral RENDER MODEL it produces.
# ROLE: an exporter is `prep <- tab_export_prep(...)` then `render_<backend>(prep)`. Everything the
#   four backends would otherwise each derive -- the canonical col_vars, the per-cell colour and
#   face, the roles, the references, the header bands, the variable-name blocks -- is computed HERE,
#   once, so a fix lands in one place and two media cannot disagree. What stays local to a backend is
#   only what its own medium decides: its markup, its escaping, how it hides an NA.
#
# THE HEADER BLOCK IS THREE ROWS, and this file decides it for every backend:
#   1. the variable-NAME span -- which col_var this run of columns belongs to
#   2. the LEVEL names        -- what the table HAS
#   3. the UNIT row           -- what each column HOLDS ("<row%>", "<row% (n)>", "<OR (row%)>"),
#                                built by fmt_display_label() from the column's own display
#                                template, in the console type tag's own angle brackets
#   The unit row exists because a composite cell showed an aside in every row and named it nowhere.
#   Each backend then does what its medium allows with those three -- html gives them <tr>s, md
#   collapses them into body rows under the delimiter, Excel writes the data block one row lower --
#   and none of them re-decides WHICH row says what.
#
# A NAME IS PRINTED ONCE, ON BOTH AXES. Down the rows tab_label_runs() blanks a repeated row_var;
#   along the header a col_var opening a second labelled run is blanked the same way; and where every
#   column already prints its own col_var (a `predictors` comparison, whose columns ARE the models)
#   the whole span row goes.
#
# ONE UNIT PER BLOCK. tab_col_block_ids() is THE definition of a column block -- a col_var run within
#   one sub-population, a Total column its own, every col_var-less helper joined to whatever it was
#   carved from. Two consumers read it: the unit line (so a Total says "<row%>" and the count beside
#   it "<n>") and tab_xl()'s vertical rules (so no line falls between a Total and its own count).
#   Anything needing to know where a block starts asks this function; it is not re-derived.
#
# A TAB_VAR COLUMN IS DROPPED ONLY WHERE THE LEVEL COLUMN IS A COMPLETE ROW INDEX -- one row_var,
#   where a sub-table is one contiguous run of levels and its Total row names it ("Total 2000"). A
#   COMPACTED table nests variable x sub-table, so the column stays whatever the backend asked for.
#
# KEY CONSTRAINTS:
#   - The render model is an EPHEMERAL S3-tagged list, never table attributes: dplyr's rename/select
#     desync bare attributes. Built once, consumed by one backend, discarded.
#   - WHICH ROW A COLUMN SAYS ITS NAME IN: the level header names what the table HAS, the unit line
#     what it HOLDS. A column the RENDER carved out of another -- a split-off aside, the base count
#     taken out of a Total cell -- has no level to name and is named by its unit alone; a helper the
#     table already had (a regression's `n`) keeps both. Not under a transpose, which turns the level
#     header into the row label and carries no unit line to say it instead.
#   - A WHOLE-TABLE HELPER IS NOT A VARIABLE: a base-count or col% column (`role`) takes no name on
#     the span row. The `sd` twin and Excel's `aside` columns are NOT helpers in that sense -- they
#     are the second half of their col_var's block and keep its span.
#   - WARNING: match a column NAME through tx_unwrap_text(). tab_wrap_text() rewrites column names
#     (U+202F for each space) before the header is built and leaves the col_var attribute raw, so a
#     literal == silently stops matching in the html backend alone. For the same reason, any list
#     KEYED BY a column name (`emp_tips`, `bars`) is re-keyed through `pre_wrap_names` right after the
#     wrap: a stale key is not an error, it is a feature that quietly does nothing.
#   - WARNING: block boundaries are read off the label columns' VALUES, never off the dplyr grouping.
#     group_indices() answers 1 for every row of a table that has lost its grouped_df class, and the
#     separators then vanish with no error.
#   - WHICH NAMES ROTATE is one decision too (tab_vname_plan): a rotation must SAVE width, so each
#     name is weighed against its block's height and against the names that CANNOT turn (a one-row
#     block), which set the column's floor. A rotated name gets ONE vertical line and never wraps --
#     neither medium grows a rowspanned or merged block to hold an overrun.
# See: CLAUDE.md section "tabxplor architecture" (exports and rendering).


# === SECTION: canonical col_vars check ================================================

# Canonical col_vars = the longest set found among the tables; every other table's col_vars must be
# a subset. Aborts (in the caller's frame) when they are not, or when any table still carries tab_vars.
tab_check_same_col_vars <- function(tabs, what = "tab_export_prep()",
                                    call = rlang::caller_env()) {
  same_col_vars <- purrr::map(tabs, ~ tab_get_vars(.)$col_vars)
  same_col_vars <- same_col_vars |>
    purrr::map(~ .[is_real_col_var(.)])
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


# === SECTION: per-column derive-once sidecar (`ann`) ==================================

# Per-column colour/face facts every backend reads. Always returns the FULL shape (font/back/bold/
# slots/refs) even when `want_colors = FALSE`, which instead forces a MONOCHROME column (no hex, no
# slots) so every backend can read one consistent structure regardless of whether colour is on.
fmt_col_ann <- function(col, theme_cols, want_colors = TRUE) {
  ref_alltot <- get_reference(col, mode = "all_totals")
  ref_cells  <- get_reference(col, mode = "cells")
  # WARNING: a reference/total anchor (ref_alltot) must stay black even when its column greys
  # non-anchors; a regression EMPIRICAL column marks its reference CATEGORY via in_refrow instead,
  # which ref_alltot alone misses.
  keep_black <- ref_alltot | is_refrow(col)

  ct <- get_color(col)
  has_col <- want_colors && length(ct) != 0L && !is.na(ct) && !ct %in% c("", "no")
  cb <- get_color_bg(col)
  has_bgc <- want_colors && length(cb) != 0L && !is.na(cb) && !cb %in% c("", "no")
  grey_this <- if (has_col || has_bgc) theme_cols$grey else theme_cols$grey2

  if (has_col || has_bgc) {
    codes     <- fmt_channel_codes(col, theme_cols$theme, ink = theme_cols$ink %||% "text")
    text_hex  <- codes$text
    bg_hex    <- codes$bg
    # tab_md() maps these raw slot integers to break-derived pandoc span classes; every other backend
    # reads only font/back/bold below.
    text_slot <- codes$text_slot
    bg_slot   <- codes$bg_slot
    face      <- codes$text_face
  } else {
    text_hex  <- rep(NA_character_, length(col))
    bg_hex    <- rep(NA_character_, length(col))
    text_slot <- integer(length(col))
    bg_slot   <- integer(length(col))
    face      <- list(bold = logical(length(col)), italic = logical(length(col)),
                      underline = rep("", length(col)))
  }

  list(
    ref_alltot = ref_alltot,
    ref_cells  = ref_cells,
    text_hex   = text_hex,
    bg_hex     = bg_hex,
    text_slot  = text_slot,
    bg_slot    = bg_slot,
    keep_black = keep_black,
    # Kept FLAT (three vectors) because tx_transpose_render() flips per-cell logicals with a flat
    # helper. These are the MEASURE's face only -- `keep_black`'s structural bold is folded into
    # `bold` below, never into these, since structural bolding is a row/column SET, not a per-cell flag.
    face_bold      = face$bold,
    face_italic    = face$italic,
    face_underline = face$underline,
    font = dplyr::case_when(!is.na(text_hex) ~ text_hex,
                            keep_black       ~ theme_cols$text,
                            TRUE             ~ grey_this),
    back = dplyr::if_else(is.na(bg_hex), "none", bg_hex),
    bold = face$bold | keep_black,
    has_color = has_col || has_bgc,
    has_bgc   = has_bgc
  )
}


# Precomputed reference masks in the shape format()/pillar_shaft expect. NULL when the column has no
# ann (non-fmt / not built).
ann_ref <- function(a) {
  if (is.null(a)) NULL else list(cells = a$ref_cells, all_totals = a$ref_alltot)
}


# A row is bold iff it is a reference/total anchor in EVERY discriminating column (a column that is
# all-anchor or all-non-anchor says nothing about which rows are references, and is dropped first).
# WARNING: when no column discriminates the result is integer(0), not "every row" -- a column that
# never discriminates must not bold the whole table.
tab_bold_rows <- function(anchor_list) {
  if (length(anchor_list) == 0) return(integer(0))
  refref <- as.data.frame(anchor_list)
  keep   <- purrr::map_lgl(refref, ~ any(.) & !all(.))
  if (!any(keep)) return(integer(0))
  refref <- refref[, keep, drop = FALSE]
  which(rowSums(refref) == ncol(refref))
}


# === SECTION: the label columns and their runs =========================================

# The shared run model for the LABEL columns: marks where a value repeats down a block so each backend
# renders it once (md blanks repeats, html gives a rowspan, Excel merges). The two label-column kinds --
# a merged table's synthetic `row_var` name column, or kept `tab_vars` levels -- are mutually exclusive.
# Returns per column list(show = lgl(n_row), span = int(n_row)); NA is a continuation of the row above.
# WARNING: columns must be listed OUTER -> INNER (the scan nests): for a regression split by tab_vars,
# list the kept tab_var before the predictor-name column, or its run is cut at every predictor change.
#' @noRd
tab_label_order <- function(tab, nms) {
  nms <- intersect(nms, names(tab))
  nms[order(match(nms, names(tab)))]
}

tab_label_runs <- function(tab, label_names) {
  n <- nrow(tab)
  res <- list()
  if (length(label_names) == 0 || n == 0) return(res)

  force <- rep(FALSE, n)
  force[1] <- TRUE
  for (cl in label_names) {
    # base `[[`, never tidyselect -- a merged table can have a column literally named "row_var", which
    # tidyselect would treat as a symbol.
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


# === SECTION: the variable-name column -- rotate, or stay horizontal? ==============================

#' @noRd
TX_VNAME_MAX          <- 13L   # the widest a name column may be before the name wraps instead
#' @noRd
TX_VNAME_MIN          <- 4L    # ... and the narrowest: a shorter name never earns a rotation
# THE TWO NUMBERS BELOW ARE READ OFF THE STYLESHEET (R/tab-css.R), not invented: a data row is
# `line-height:1.1` + `padding:3px` twice (1.1em + 6px tall); a turned character advances by its own
# glyph width, ~0.6em; the name cell's own padding costs the block a fixed 8px, once, not per row. A
# block of `span` rows can then hold (span*(1.1em+6px)-8px)/0.6em turned characters, ~2*span-1 at every
# font size between 12 and 18px -- rounded down, with the rate cut to 1.75 for overrun headroom.
#' @noRd
TX_VERT_CHARS_PER_ROW <- 1.75  # turned characters one table row is tall enough to hold
#' @noRd
TX_VERT_PAD_CHARS     <- 1L    # ... less what the name cell's own padding costs the block, once

#' @noRd
tx_vert_capacity <- function(span)
  pmax(0L, as.integer(floor(span * TX_VERT_CHARS_PER_ROW)) - TX_VERT_PAD_CHARS)

# One name column's plan, per RUN carried down its rows: `vert` (rotate?), `chars` (the column's
# horizontal width), `width` (the wrap width, vertical or horizontal, that run's name gets).
#' @noRd
tab_vname_plan <- function(vals, run, wrap_rows = Inf) {
  nm <- as.character(vals); nm[is.na(nm)] <- ""
  n  <- length(nm)
  if (is.null(run)) run <- list(show = rep(TRUE, n), span = rep(1L, n))
  w    <- nchar(nm)
  cap  <- max(TX_VNAME_MIN,
              min(TX_VNAME_MAX, if (is.finite(wrap_rows)) as.integer(wrap_rows) else TX_VNAME_MAX))
  vcap <- tx_vert_capacity(run$span)
  fits <- run$show & nzchar(nm) & run$span > 1L & w <= vcap
  forced <- run$show & nzchar(nm) & !fits
  # stable without iterating: a name that fits but loses to the floor becomes horizontal at a width
  # already <= the floor, so it can never raise it.
  chars <- max(TX_VNAME_MIN, min(cap, if (any(forced)) max(w[forced]) else 0L))
  vert  <- fits & w > chars
  # a rotated name's wrap width is its block's own capacity, which `fits` has already cleared, so
  # tx_wrap_name() never breaks it.
  width <- ifelse(vert, pmax(1L, vcap), chars)
  # CARRIED DOWN THE RUN, never left per row: a continuation row holds the same name, and a width that
  # differed there would wrap it differently -- splitting one block's name into two spellings.
  at   <- which(run$show)
  if (!length(at)) at <- 1L
  idx  <- pmax(1L, cumsum(run$show))
  list(vert = vert[at][idx], chars = chars, width = width[at][idx])
}

# Puts a per-row rewritten label vector back, keeping the column's type (a tabxplor_lvl factor stays
# one, since its role/var declaration drives variable detection elsewhere). Maps level -> new text so a
# level appearing on several rows cannot end up with two different spellings.
#' @noRd
tx_recolumn_labels <- function(col, new) {
  if (!is.factor(col)) return(new)
  old <- as.character(col)
  map <- new[!duplicated(old)]
  names(map) <- old[!duplicated(old)]
  forcats::fct_relabel(col, function(l) unname(ifelse(l %in% names(map), map[l], l)))
}

# === SECTION: the render-model builder ==============================================

# Resolve input into the list of tables to render. A list is NEVER merged here: `tab()` already merges
# what it decided to merge at build time, so a list reaching an exporter is one the caller asked to
# keep separate, and gluing it back together at render time would override that.
tab_resolve_tables <- function(tabs, list_method = FALSE, what,
                               call = rlang::caller_env()) {
  if (is.data.frame(tabs) || !is.list(tabs)) return(list(tabs))
  if (list_method) return(tabs)                 # render each separately (the list method)
  tab_check_same_col_vars(tabs, what = what, call = call)  # errors (kable) -- current behaviour
  tabs
}


# Build the render-model for ONE resolved table (already compacted / single). See the file header.
prep_one_table <- function(tab, drop_tab_vars, wrap, compute,
                           theme_cols, var_names = "both", transposed = FALSE, lang = NULL,
                           color_legend = TRUE) {
  rv <- tab_render_vars(tab)
  if (isTRUE(rv$degrade)) {
    return(list(tab = tab, vars = list(degrade = TRUE, reason = rv$reason)))
  }

  tab_vars <- rv$tab_vars
  # a regression tab_vars has no Total row to carry its level, so it is kept as a name column even
  # when other tab_vars are dropped for html/Excel.
  reg_grp_col <- intersect(reg_call(tab)$tab_vars, tab_vars)
  subtext  <- get_subtext(tab) |> purrr::discard(\(s) s == "")
  # a table travelling UNDER another one (meta$footer_tabs, expanded by tx_with_footer_tabs()) renders
  # what it carries and nothing generated -- so one host + subordinate pair shows one colour legend.
  subordinate <- tx_is_subordinate(tab)

  # multinomial crude-companion tooltips, resolved NOW while the predictor `var` column is still
  # present (drop_tab_vars removes it below).
  emp_tips <- NULL
  et_raw   <- get_empirical_tips(tab)
  if (!is.null(et_raw)) {
    lvl0 <- as.character(tab[[rv$row_var]])
    vcol <- intersect(rv$var_col, names(tab))
    var0 <- if (length(vcol)) as.character(tab[[vcol[[1L]]]]) else rep(NA_character_, nrow(tab))
    key0 <- paste(var0, lvl0, sep = "\r")
    emp_tips <- lapply(split(et_raw, et_raw$col), function(sub)
      sub$tip[match(key0, paste(sub$var, sub$level, sep = "\r"))])
  }

  # The per-cell DATA-BAR fraction, for the columns set_bars() names: the value as a share of its own
  # column's largest, so the tallest bar fills its cell -- a bar chart inside the table. Data rows
  # only: a total is not on the same scale as what it totals.
  bars <- purrr::map(purrr::set_names(intersect(get_bars(tab) %||% character(0), names(tab))),
                     function(nm) {
    col <- tab[[nm]]
    if (!is_fmt(col)) return(NULL)
    v <- abs(get_num(col))
    v[get_row_kind(col) != "data"] <- NA_real_
    m <- suppressWarnings(max(v, na.rm = TRUE))
    if (!is.finite(m) || m <= 0) return(NULL)
    pmin(v / m, 1)
  })
  bars <- purrr::compact(bars)

  # block-closing rows, read off label VALUES before `var_names` may drop the name column below.
  bound_runs <- tab_label_runs(tab, tab_label_order(tab, c(rv$var_col, tab_vars)))
  new_group  <-
    if (length(bound_runs)) which(dplyr::lead(bound_runs[[length(bound_runs)]]$show, default = TRUE))
    else nrow(tab)

  # from the SAME raw values, before wrap or `var_names` can touch the column: which names rotate
  # and how wide (tab_vname_plan).
  vname_cols  <- tab_label_order(tab, c(rv$var_col, reg_grp_col))
  vname_runs0 <- tab_label_runs(tab, vname_cols)
  vname_plans <- purrr::imap(vname_runs0, function(run, cl)
    tab_vname_plan(tab[[cl]], run, wrap_rows = wrap$rows %||% Inf))

  tab <- dplyr::ungroup(tab)
  # A tab_var column is dropped only where the LEVEL column alone is a complete row index: with one
  # row_var a sub-table is one contiguous run of levels and its Total row carries the level name
  # ("Total 2000"). A COMPACTED table nests two indexes (variable x sub-table), so the level column
  # cannot say which sub-table a row belongs to and the column stays, whatever the backend asked for.
  if (drop_tab_vars && length(tab_vars) > 0 && !isTRUE(rv$compacted)) {
    drop_these <- setdiff(tab_vars, reg_grp_col)
    if (length(drop_these) > 0) tab <- dplyr::select(tab, -tidyselect::all_of(drop_these))
  }
  # the declared "var"-role column (`row_var` on a merged crosstab, `var` on a regression), dropped
  # when `var_names` excludes rows. WARNING: local is `name_col`, not `row_var` -- a real column can
  # be literally named that (a tidyselect trap; see tab_transpose(), tab.R).
  name_col <- intersect(rv$var_col, names(tab))
  if (length(name_col) > 0 && !var_names %in% c("both", "rows")) {
    tab      <- dplyr::select(tab, -tidyselect::all_of(name_col))
    name_col <- character(0)
  }
  # swaps source variable NAMES for LABELS when tabxplor.var_labels is on -- before wrap, so a long
  # label wraps too.
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
    # tab_wrap_text RENAMES columns, so any list KEYED BY A COLUMN NAME must follow via
    # pre_wrap_names or the lookup fails -- silently, and only in the html backend (see file header).
    follow_wrap <- function(x) {
      if (is.null(x) || !length(x)) return(x)
      renamed <- stats::setNames(names(tab), pre_wrap_names)[names(x)]
      names(x) <- ifelse(is.na(renamed), names(x), renamed)
      x
    }
    emp_tips <- follow_wrap(emp_tips)
    bars     <- follow_wrap(bars)
    # NAME columns re-wrap to their OWN width via tx_wrap_name() (block height when rotated, column
    # width otherwise), not `wrap_rows` -- run AFTER the generic wrap, over tx_unwrap_text().
    for (cl in intersect(names(vname_plans), names(tab))) {
      p   <- vname_plans[[cl]]
      raw <- tx_unwrap_text(as.character(tab[[cl]]))
      new <- unlist(purrr::map2(raw, p$width, function(s, w)
        tx_wrap_name(s, width = w, exdent = 1L, brk = wrap$brk %||% "\n")), use.names = FALSE)
      if (isTRUE(wrap$unbreakable_spaces))
        new <- gsub(" ", unbrk, new, perl = TRUE)
      tab[[cl]] <- tx_recolumn_labels(tab[[cl]], new)
    }
  }

  # --- role detection on the FINAL (ungrouped / dropped / wrapped) tab ---
  fmt_mask   <- purrr::map_lgl(tab, is_fmt)
  fmt_cols   <- which(fmt_mask)
  other_cols <- which(!fmt_mask)

  # Whether the table shows a cell suffix (stars / effect-size marks) -- gates the monospace number
  # font (tx_num_font() below) so the suffix stays aligned.
  has_stars <- length(fmt_cols) > 0 &&
    any(vapply(fmt_cols,
               function(j) any(nzchar(fmt_cell_suffix(tab[[j]], stars = TRUE,
                                                      theme = theme_cols$marks))),
               logical(1)))

  col_var_map   <- get_col_var(tab)
  real_col_vars <- unique(col_var_map[fmt_mask])
  # `no_col_var` (a no-col_var table's helper columns) is not a real name; excluded so no span draws.
  real_col_vars <- real_col_vars[is_real_col_var(real_col_vars)]

  # DECLARED (names a colour measure, the legend's gate) vs REALISED (roles$has_color); see
  # roles_color_flags() below.
  color_cols <- get_color(tab)
  color_cols <- which(!color_cols %in% c("", "no") & !is.na(color_cols))

  totcols    <- which(is_totcol(tab))
  totrows    <- which(is_totrow(tab))

  # re-detected on the FINAL tab: wrap can rename the row-label column.
  row_var_name <- tab_render_vars(tab)$row_var
  row_var_col  <- which(names(tab) == row_var_name)

  # label_cols = blank/rowspan/merge set; var_name_col is its name-valued subset (`var_names` drops
  # it) -- a kept regression tab_vars rotates too but stays out of name_col, so is never dropped.
  label_names <- tab_label_order(tab, c(name_col, tab_vars))
  label_cols  <- stats::setNames(match(label_names, names(tab)), label_names)
  var_name_col <- label_cols[names(label_cols) %in% c(name_col, reg_grp_col)]
  label_runs  <- tab_label_runs(tab, label_names)

  # the Excel-only "<var>_sd" siblings (tab_materialize_extras, backend "xl"). Ungated by `var_names`:
  # a width is not a naming decision.
  sd_cols <- fmt_cols[vapply(tab[fmt_cols], \(col)
    fmt_is_aside(col) &&
      identical(display_primary(get_display(col))[[1]], "sd"), logical(1))]

  # A "total block" is a maximal run of total / synthetic n/pvalue/row_pct/GOF rows; its first row
  # gets a top border, its last a bottom one -- from row_kind, not matched labels.
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
  # row-anchor signal for tab_bold_rows(), captured BEFORE the footer override below needs the pure signal.
  anchors <- purrr::map(ann, "keep_black")

  # A REGRESSION GOF FOOTER ROW READS BLACK, AND NOT BOLD: a model-fit number is a report card under
  # the table, so COLOUR is the only emphasis it keeps -- a marked check (`gof_warn`) keeps its own
  # shade, a non-significant p-value its red. Bolding the numbers and the stat names beside them put
  # the footer in front of the table it describes, in dark mode especially. The one thing still bold
  # is the "Model fit" label in the predictor-names column, which each backend bolds through that
  # COLUMN (html `tx-b` on the named cell, xl's var_name_col source) and not through the row.
  # Never touches a crosstab.
  footer_rows <- if (length(fmt_cols) > 0) {
    purrr::reduce(purrr::map(names(fmt_cols),
      ~ display_primary(get_display(tab[[.x]])) %in% DISPLAY_FOOTER_TOKENS), `&`)
  } else logical(nrow(tab))
  if (any(footer_rows)) {
    ann <- purrr::imap(ann, function(a, nm) {
      blk <- footer_rows & display_primary(get_display(tab[[nm]])) != "gof_warn"
      a$keep_black[blk] <- TRUE
      a$font[blk]       <- theme_cols$text
      a$bold[footer_rows] <- FALSE
      a
    })
  }

  # --- bold rows + bold cols (block D), from the pure `anchors` signal / ann$ref_alltot ---
  ref_alltot_list <- purrr::map(ann, "ref_alltot")
  bold_rows <- if ("bold" %in% compute) {
    tab_bold_rows(anchors)
  } else integer(0)
  # ...and a footer row is never a bold ROW either, which is what used to reach its LABEL cells (the
  # stat names in the level column). See the block above.
  if ("bold" %in% compute && any(footer_rows)) bold_rows <- setdiff(bold_rows, which(footer_rows))
  bold_cols <- if ("bold" %in% compute && length(ref_alltot_list) > 0) {
    names(which(purrr::map_lgl(ref_alltot_list, all)))
  } else character(0)

  # a pct="col" "n" row's Total-COLUMN cell falls into the all_totals anchor -- force it plain, a
  # base count is not a reading anchor.
  if ("bold" %in% compute) {
    n_rows <- which(tab_row_roles(tab) == "n")
    if (length(n_rows)) {
      ann       <- purrr::map(ann, function(a) { a$bold[n_rows] <- FALSE; a })
      bold_rows <- setdiff(bold_rows, n_rows)
    }
  }

  # THE shared col_var header model (spanning name row + level labels); see tab_col_var_header() below.
  col_blocks <- tab_col_block_ids(col_var_map, tab_col_groups(tab), other_cols, totcols)
  col_var_header <- tab_col_var_header(
    tab, list(col_var_map = col_var_map, real_col_vars = real_col_vars, totcols = totcols,
              var_name_col = var_name_col, sd_cols = sd_cols, col_blocks = col_blocks),
    name_cols = var_names %in% c("both", "cols"), transposed = transposed)
  # a single-row_var table's row-label header is swapped for its var_labels label too (merged case
  # already blanked above).
  if (length(name_col) == 0 && length(row_var_col) == 1)
    col_var_header$clean[row_var_col] <- var_label_display(col_var_header$clean[row_var_col], tab)

  list(
    tab = tab,
    vars = list(degrade = FALSE, row_var = row_var_name, tab_vars = tab_vars,
                row_vars = rv$row_vars, compacted = isTRUE(rv$compacted),
                var_col = rv$var_col, col_vars = rv$col_vars),
    roles = list(fmt_mask = fmt_mask, fmt_cols = fmt_cols, other_cols = other_cols,
                 row_var_col = row_var_col, totcols = totcols, totrows = totrows,
                 totblock_top = totblock_top,
                 totblock_bottom = totblock_bottom, real_col_vars = real_col_vars,
                 col_var_map = col_var_map, new_col_var = new_col_var, col_blocks = col_blocks,
                 new_group = new_group, align = align,
                 label_cols = label_cols, var_name_col = var_name_col,
                 # rotation decision + width per name column (tab_vname_plan above); read by the html
                 # engine (the `tx-vname` class) and by tab_xl (a 90-degree rotation + that width).
                 vname_plans = vname_plans[intersect(names(vname_plans), names(tab))],
                 label_runs = label_runs, sd_cols = sd_cols,
                 color_cols = color_flags$color_cols, any_bg = color_flags$any_bg,
                 has_color = color_flags$has_color, has_stars = has_stars),
    ann = ann,
    # A different KIND of block from a row_var separator: its boundary is drawn across the whole table
    # (the html engine's 2px top/bottom rule).
    footer_rows = which(footer_rows),
    bold_rows = bold_rows,
    bold_cols = bold_cols,
    col_var_header = col_var_header,
    subtext = subtext,
    # WHETHER THIS TABLE GETS A COLOUR LEGEND, decided ONCE: the call's `color_legend` (already
    # ANDed with `color` by resolve_export_opts()) and whether this table declares a measure at all.
    # ⚠ the three backends each computed it, and the Excel one had no `color_cols` term -- a divergence
    # only legend_specs()' own gate was hiding.
    want_legend = isTRUE(color_legend) && length(color_flags$color_cols) != 0,
    subordinate = subordinate,
    bars = bars,
    # a fallback caption (NA on a crosstab) and a stored one (set_caption()), which takes precedence.
    # ⚠ the exporter's OWN language: a caption built in the ambient locale while the footer below it
    # followed `lang =` was the one place two languages met in one table.
    reg_title = reg_title(reg_call(tab), lang = lang),
    caption = get_caption(tab),
    empirical_tips = emp_tips
  )
}

# The opt-in variable-NAME -> variable-LABEL display swap (tabxplor.var_labels), reading labels stored
# at build (meta$vars$var_labels). Display only: the tibble's structural names (col_var attr, row_var
# column values, column names) stay canonical, so select()/reference by name still works. Shared by the
# col-var span header, the single-row_var header, and the merged row_var name column.
var_label_display <- function(x, tab) {
  if (!isTRUE(tx_option("var_labels"))) return(x)
  labs <- get_vars_attr(tab)[["var_labels"]]
  if (is.null(labs) || length(labs) == 0L) return(x)
  hit <- !is.na(x) & x %in% names(labs)
  if (any(hit)) x[hit] <- unname(labs[x[hit]])
  x
}

# The shared col_var HEADER model. `label` = the spanning col_var NAME (blank for the row var, helper
# and Total columns). `clean` = the level name, its "_<col_var>" disambiguation suffix stripped (two
# col_vars sharing a level "Other" are stored as "Other_race"/"Other_grp"; exports show the bare name).
# `name_cols` (will the span render?) is decided HERE, not after: a level header may name the
# STATISTIC ("mean") only when the span names the VARIABLE.
tab_col_var_header <- function(tab, roles, name_cols = TRUE, transposed = FALSE) {
  nms   <- names(tab)
  cvm   <- roles$col_var_map
  real  <- roles$real_col_vars
  totc  <- seq_along(nms) %in% roles$totcols
  # a real col_var LEVEL column, kept separate from `label`: the suffix-strip below must run even
  # when nothing is named -- a "_race" suffix is noise whatever `var_names` says.
  is_level <- (unname(cvm) %in% real) & !totc
  label    <- ifelse(is_level & isTRUE(name_cols), var_label_display(unname(cvm), tab), "")
  # the SUB-POPULATION each level column belongs to, kept apart from `label` (col_group) so a backend
  # composes the two as its medium allows -- html a <br>, Excel a newline, markdown neither.
  grp <- rep("", length(nms))
  gvec <- vapply(seq_along(nms), function(j) if (is_fmt(tab[[j]])) get_col_group(tab[[j]]) else "",
                 character(1))
  grp[is_level] <- gvec[is_level]
  clean <- nms
  # the merged table's row-var NAME column is headed by the literal "row_var" -- blanked unconditionally.
  clean[roles$var_name_col] <- ""
  for (j in which(is_level)) {
    suff <- paste0("_", cvm[[j]])
    # ⚠ THROUGH tx_unwrap_text(), per the file header's rule: tab_wrap_text() has already rewritten
    #   this name -- U+202F for each space, a "<br>" wherever it broke -- while the col_var attribute
    #   stayed raw. A literal endsWith() therefore stopped matching the moment a col_var held a space
    #   or the compound name was long enough to wrap, and the suffix reached the page.
    raw <- tx_unwrap_text(nms[j])
    if (endsWith(raw, suff)) {
      clean[j] <- substr(raw, 1L, nchar(raw) - nchar(suff))
    } else if (isTRUE(name_cols) && identical(nms[j], cvm[[j]]) && is_fmt(tab[[j]]) &&
               identical(fmt_var_kind(tab[[j]]), "mean")) {
      # A numeric col_var's column bears the VARIABLE's own name (else "tvhours" over "tvhours"), so
      # here it takes the column's STATISTIC instead ("mean", "mean (sd)") -- not the unit line, since
      # a TRANSPOSED render turns this header into the ROW LABEL with no unit line to say it instead.
      clean[j] <- fmt_header_label(tab[[j]])
    }
  }
  for (j in which(is_level)) {
    # A regression column across several outcomes carries a trailing " [dep]" bracket in its stored
    # name; the span already names the outcome, so the level header strips it (never a crosstab level).
    if (is_fmt(tab[[j]]) && get_role(tab[[j]]) %in% c("model", "emp", "n"))
      clean[j] <- tx_strip_outcome_suffix(clean[j])
  }
  # THE SPREAD SWAP: after a spread, a COLUMN is identified by its sub-population and a BLOCK by its
  # variable -- the level heads the block, the sub-population becomes the column header. The level
  # band survives only where a variable contributes several columns per sub-population.
  spread_grp <- ifelse(is_level, gvec, "")
  if (length(unique(spread_grp[nzchar(spread_grp)])) > 1L) {
    cv_lab <- var_label_display(unname(cvm), tab)
    # the level WITHOUT its sub-population suffix (the pivot appends "_<group>"). WARNING: compared
    # via tx_unwrap_text() -- names are wrapped before this runs while `spread_grp` is not, so a
    # literal endsWith() stops matching once a name is long enough to break.
    cl0  <- tx_unwrap_text(clean)
    lvl0 <- ifelse(nzchar(spread_grp) & endsWith(cl0, paste0("_", spread_grp)),
                   substr(cl0, 1L, nchar(cl0) - nchar(spread_grp) - 1L), cl0)
    for (j in which(is_level)) {
      many <- length(unique(lvl0[is_level & unname(cvm) == cvm[[j]]])) > 1L
      clean[j] <- spread_grp[[j]]
      label[j] <- if (isTRUE(name_cols)) { if (many) lvl0[[j]] else cv_lab[[j]] } else ""
      grp[j]   <- if (isTRUE(name_cols) && many) cv_lab[[j]] else ""
    }
    # the per-block base-count columns: their col_var is the "n" placeholder, not a level, and would
    # otherwise render as a bare `n_White`.
    is_n <- vapply(seq_along(nms), function(k) is_fmt(tab[[k]]) &&
                     get_role(tab[[k]]) == "n" && nzchar(gvec[[k]]), logical(1))
    for (j in which(is_n & !is_level)) {
      clean[j] <- gvec[[j]]
      label[j] <- if (isTRUE(name_cols)) gettext("n") else ""
      grp[j]   <- ""
    }
  }

  # base-count / col% columns take no span name (see file header: a whole-table helper is not a
  # variable).
  helper <- vapply(seq_along(nms),
                   function(k) is_fmt(tab[[k]]) && get_role(tab[[k]]) %in% c("n", "pct"),
                   logical(1))
  label[helper] <- ""
  # A column the render CARVED out of another (a split-off aside, a base count out of a Total cell)
  # has no level to name -- named once by its unit instead ("<n>", "<sd>"). NOT under a transpose,
  # which turns this header into the row label with no unit line to say it instead.
  carved <- !transposed & vapply(seq_along(nms), function(k) {
    if (!is_fmt(tab[[k]])) return(FALSE)
    r <- get_role(tab[[k]])
    fmt_is_aside(tab[[k]]) || (identical(r, "n") && !nzchar(unname(cvm)[[k]]))
  }, logical(1))
  clean[carved] <- ""
  # If every level column's CLEAN header already equals its own col_var (a `predictors` comparison
  # whose columns ARE the models), the span would say nothing new -- drop it. Compared via
  # tx_unwrap_text(), since names are wrapped before this runs while the col_var attribute is not.
  lvl <- which(is_level)
  if (length(lvl) > 0 &&
      all(tx_unwrap_text(clean[lvl]) == tx_unwrap_text(unname(cvm)[lvl]))) label <- rep("", length(nms))
  # A COL_VAR IS NAMED ONCE: a Total column would open a second labelled run and repeat the span --
  # blanked here. Keyed on (label, group, gvec) because a spread names a variable once per sub-population.
  key <- ifelse(nzchar(label), paste(label, grp, gvec, sep = "\r"), "")
  rl  <- rle(key)
  at  <- cumsum(c(1L, utils::head(rl$lengths, -1L)))
  seen <- character(0)
  for (k in seq_along(rl$values)) {
    v <- rl$values[[k]]
    if (!nzchar(v)) next
    if (v %in% seen) label[at[[k]] + seq_len(rl$lengths[[k]]) - 1L] <- "" else seen <- c(seen, v)
  }
  # a blanked span row carries no sub-population either: `group` only ever qualifies a `label`.
  grp[!nzchar(label)] <- ""
  unit <- tab_col_units(tab, roles$col_blocks %||% unname(cvm))
  list(label = label, group = grp, clean = clean, unit = unit)
}

# THE UNIT LINE -- what each column HOLDS, in the console type tag's own words: "row%", "row% (n)",
# "OR (row%)", "mean (sd)". Written ONCE per col_var in its LEFTMOST column, again wherever a block's
# columns disagree.
tab_col_units <- function(tab, blocks) {
  # an aside MOVED OUT into a column of its own (mat_aside_cols) already reduced its display to the
  # primary, so its name promises nothing the cell no longer shows.
  u <- vapply(seq_along(tab), function(j) {
    col <- tab[[j]]
    if (!is_fmt(col)) return("")
    fmt_display_label(col, "tag")
  }, character(1))
  # role goes into the run key: an observed/model pair with MIRRORED layouts would otherwise have the
  # model column's tag swallowed into the run, since it repeats the same string.
  r <- vapply(tab, function(col) if (is_fmt(col)) as.character(get_role(col))[1] else "",
              character(1), USE.NAMES = FALSE)
  tab_units_once(u, blocks, r)
}

# Written ONCE per (block, role, unit) RUN, in its leftmost column -- so a Total and its carved-out
# count (one block, two units) each say their own. Shared with the transposed render.
tab_units_once <- function(unit, group, role = NULL) {
  if (is.null(role)) role <- rep("", length(unit))
  r     <- rle(paste0(group, "\r", role, "\r", unit))
  first <- c(1L, utils::head(cumsum(r$lengths), -1L) + 1L)
  out   <- rep("", length(unit))
  out[first] <- unit[first]
  out[nzchar(out)] <- paste0("<", out[nzchar(out)], ">")
  out
}

# RLEs the header `label` vector into (label, span) runs. Encodes the PAIR (group, label): a spread
# table has two adjacent runs of the same variable, and the label alone would merge them into one span.
tab_header_runs <- function(label, group = NULL) {
  if (is.null(group)) group <- rep("", length(label))
  r <- rle(paste0(group, "\r", label))
  ends <- cumsum(r$lengths)
  list(labels = label[ends], groups = group[ends], spans = r$lengths)
}

# The ONE caption fallback: user caption=, else set_caption() (rd$caption), else a regression's
# auto-title. `fallback` is a closure so a further fallback is only computed when genuinely needed.
rd_caption <- function(rd, user_caption = NULL, fallback = NULL) {
  cap <- user_caption
  if (is.null(cap)) cap <- rd$caption
  if (is.null(cap) && !is.null(rd$reg_title) && !is.na(rd$reg_title)) cap <- rd$reg_title
  if (is.null(cap) && is.function(fallback)) cap <- fallback()
  cap
}

# The per-column sub-population ("" on a non-fmt column).
tab_col_groups <- function(tab)
  vapply(tab, function(col) if (is_fmt(col)) get_col_group(col) else "", character(1))

# See file header for the block definition. WARNING: not tab_col_blocks() (fmt_class.R), the DISTINCT
# (col_var, col_group) pairs a test grid needs -- this is the per-COLUMN index, where a Total opens a
# block of its own precisely because it is not a value block there.
tab_col_block_ids <- function(col_var, col_group = NULL, other_cols = integer(0),
                              totcols = integer(0)) {
  n <- length(col_var)
  if (!n) return(integer(0))
  cv  <- unname(col_var)
  grp <- if (is.null(col_group)) rep("", n) else unname(col_group)
  idx <- seq_len(n) %in% unname(other_cols)
  tot <- seq_len(n) %in% unname(totcols)
  key <- ifelse(idx, "\rindex", paste(cv, grp, tot, sep = "\r"))
  out <- integer(n); b <- 0L; prev <- NA_character_
  for (j in seq_len(n)) {
    if (!idx[j] && !tot[j] && !nzchar(cv[j]) && b > 0L) { out[j] <- b; next }
    if (is.na(prev) || !identical(key[[j]], prev)) { b <- b + 1L; prev <- key[[j]] }
    out[j] <- b
  }
  out
}

# The first column of each block -- where a vertical rule is drawn (the next block's left rule
# separates two; the table's own right edge closes the last).
tab_block_starts <- function(blocks) {
  if (!length(blocks)) return(integer(0))
  which(blocks != dplyr::lag(blocks, default = 0L))
}

# THE col_var transition index, in three conventions three backends each re-derived separately:
#   side = "right"   the LAST column of each group  (kable/plot: a right border)
#   side = "left"    the FIRST column of each group (Excel: a left border)
#   real_only        count a transition only BETWEEN two real col_vars (md's span separators), so a
#                    helper column (`n`, a Total) never opens a new block
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

# Strips the trailing " [outcome]" bracket a regression column carries when built across several
# outcomes -- noise wherever the outcome is already named. WARNING: the separator may not be a plain
# space, since tab_wrap_text() runs first and rewrites spaces into U+202F or <br>.
tx_strip_outcome_suffix <- function(x)
  sub("([[:space:]\u202f\u00a0]|<br>)*\\[[^]]*\\]$", "", x)

# The name a col_var LEVEL column is SHOWN under: a producer suffixes "_<col_var>" to keep tibble
# names unique across the axes/levels of a span (ggfacto's "coord_Axe 1"), and tab_col_var_header()
# strips it so the header reads "coord" under an "Axe 1" span. The legend must name the same thing:
# a footer pointing at "coord_Axe 1" names a column the table never shows.
# WARNING: through tx_unwrap_text(), for the reason tab_col_var_header() states -- the NAME has
#   already been rewritten by tab_wrap_text() while the col_var attribute stayed raw.
tx_strip_col_var_suffix <- function(x, col_var) {
  raw  <- tx_unwrap_text(x)
  suff <- paste0("_", col_var)
  keep <- endsWith(raw, suff) & nchar(raw) > nchar(suff)
  ifelse(keep, substr(raw, 1L, nchar(raw) - nchar(suff)), x)
}

# Undoes tab_wrap_text()'s rewriting of a NAME (see file header), for comparison against a stored
# attribute the wrap never touched.
tx_unwrap_text <- function(x) {
  # a break AT A SEAM is removed, not turned into a space: it broke a compound name at a separator the
  # name itself carries (name_<br>suffix), so a space would invent one. Mirrors tx_name_atoms()'s breaks.
  br <- "(?:<br>|\\n)[\u00a0 ]*"
  x <- gsub(paste0("(?<=[_.])", br), "", x, perl = TRUE)                    # broken after a seam
  x <- gsub(paste0(br, "(?=[*])"), "", x, perl = TRUE)                      # ... before the operator
  x <- gsub(paste0("(?<=[a-z0-9])", br, "(?=[A-Z])"), "", x, perl = TRUE)   # ... at a camelCase seam
  gsub("[[:space:]]+", " ", gsub("<br>|\\n", " ", gsub("[\u202f\u00a0]", " ", x)))
}

# ONE rule for two incompatible value syntaxes: html/md take a CSS font stack, Excel a single xlsx
# font NAME. Shared: switch to monospace when the table shows stars, so they cannot push digits out of
# column. html/md are unconditionally monospace already; Excel is the one that chooses.
tx_num_font <- function(medium = c("html", "xl"), has_stars = FALSE,
                        plain = NULL, stars = NULL) {
  switch(match.arg(medium),
    html = tx_option("tab_kable_num_font"),
    xl   = if (isTRUE(has_stars)) stars %||% tx_option("xl_font_num_stars")
           else                   plain %||% tx_option("xl_font_num"))
}

# THE colour flags, one producer shared by the prep and the transpose:
#   color_cols  DECLARED -- names a colour measure; the legend's gate, shown even if no cell breaks.
#   has_color   REALISED -- any cell actually carries one; FALSE if colour was never requested.
#   any_bg      realised, background channel alone.
roles_color_flags <- function(ann, color_cols) {
  list(color_cols = color_cols,
       any_bg     = any(vapply(ann, function(a) isTRUE(a$has_bgc) , logical(1))),
       has_color  = any(vapply(ann, function(a) isTRUE(a$has_color), logical(1))))
}

# Top/bottom border rows of each "total block" -- a maximal run of TRUE in `in_block`. Shared by
# prep_one_table() and tx_transpose_render(), whose otherwise-independent role models agree here.
roles_totblock_edges <- function(in_block) {
  list(
    top    = which(dplyr::if_else(in_block, !dplyr::lag(in_block), FALSE)),
    bottom = which(dplyr::if_else(in_block, !dplyr::lead(in_block, default = FALSE), FALSE))
  )
}


# === SECTION: shared option resolver ================================================

# Resolves the shared export options (theme + toggles) ONCE, so every exporter and tab_export() share
# one set of names, defaults and fallbacks. `color = FALSE` also disables the legend.
# WARNING: every call site takes NAMED arguments -- a positional call would silently shift the toggles.
# WARNING: "auto" (follow the reader's scheme) is a RENDER intent, not a palette: only a stylesheet
# backend can honour it, a static one gets "light". Downstream of a palette lookup, always go through
# tx_palette_theme() (tab-css.R), not this value. "print" IS a palette, so it reaches static backends too.
resolve_export_opts <- function(theme = NULL,
                                color = TRUE, color_legend = TRUE,
                                transpose = FALSE, caption = NULL,
                                var_names = NULL,
                                allow_auto = FALSE, tabs = NULL) {
  # the one place a theme becomes concrete: `tabs` is what lets `print_ready` choose a publication
  # palette (tx_theme_for_table); a caller with no table (tab_css()) takes the declared fallback.
  theme <- tx_theme_for_table(tx_theme_resolve(theme, allow_auto = allow_auto), tabs)
  if (is.null(var_names)) var_names <- tx_option("var_names")
  var_names <- match.arg(var_names[1], c("both", "rows", "cols", "none"))
  color <- isTRUE(color)
  list(theme = theme,
       color = color, color_legend = isTRUE(color_legend) && color,
       transpose = isTRUE(transpose), caption = caption, var_names = var_names)
}


# The single exporter-prep entry point; returns a `tabxplor_render` (see file header). `compute` gates
# the expensive derivations so a backend / jamovi live path can opt out of what it does not use.
# `transpose = TRUE` flips the FINISHED render model of each table via tx_transpose_render() -- colours
# and cell strings are computed per (correct, homogeneous) source column, THEN rows and columns swap as
# plain data (see tab-transpose-render.R). Materialises "xl"-style when transposing, so the base count
# is an `n` COLUMN that flips into an `n` ROW.
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
                            lang          = NULL,
                            what          = NULL) {
  backend   <- match.arg(backend)
  var_names <- match.arg(var_names[1], c("both", "rows", "cols", "none"))
  if (is.null(what)) what <- paste0("tab_", backend, "()")
  if (is.null(compute)) {
    compute <- if (backend %in% c("kable", "plot")) {
      c("refs", "colors", "bold")
    } else c("refs", "bold")  # md / xl
  }

  # theme MAY be the render intent "auto"; a PALETTE is always light/dark. Resolved ONCE here so
  # theme_cols (-> fmt_channel_codes -> get_color_style) never sees "auto", which would build the key
  # "text_auto" and find no palette. `meta$theme` below keeps the intent for the renderer.
  # WARNING: the hex here MUST stay in sync with tx_chrome_hex() (tab-css.R), which emits the same
  # colours as CSS for the html engine -- both read tx_chrome_hex(); never inline a literal.
  pal_theme  <- tx_palette_theme(theme[1])
  chrome     <- tx_chrome_hex(pal_theme)
  theme_cols <- list(
    theme = pal_theme,
    text  = chrome$text,
    grey  = chrome$grey,
    grey2 = chrome$grey2,
    # which family a cell's ink comes from: the plot backend cannot draw a rule, so a publication
    # palette borrows its grey ramp there instead. "text" everywhere else.
    ink   = if (identical(backend, "plot")) tx_plot_ink_family(pal_theme, "text") else "text",
    # THE THEME THE CELL SUFFIX READS, which is not always the table's: a publication palette MARKS
    # its cells (`print_marks`), and a mark is the cell's own visual signal, not an aside -- so
    # `color = FALSE` must take it away with the colour, or the reader gets a sign with no key
    # (fmt_cell_suffix() draws nothing at all on a NULL theme).
    marks = if ("colors" %in% compute) pal_theme else NULL
  )

  resolved <- tab_resolve_tables(tabs, list_method = list_method, what = what)

  # Hydrate the "core" table into its rendered shape ONCE, on the still-grouped resolved tables (before
  # prep_one_table ungroups), so p-value rows + the base count / add_pct become real rows/cols the role
  # detection then sees. "xl" keeps a real `n` column; "text" folds it into the Total cell.
  # Materialise "xl"-style for EVERY backend when transposing, so the base count is a real `n` COLUMN
  # that flips into an `n` ROW (matching a native pct="col" table) instead of a folded "100% (n=849)"
  # cell.
  mat_backend <- if (identical(backend, "xl") || isTRUE(transpose)) "xl" else "text"
  resolved <- purrr::map(resolved, tab_materialize_extras, backend = mat_backend, medium = backend,
                         pvalue = TRUE,
                         transposed = isTRUE(transpose))

  tables <- purrr::map(
    resolved,
    ~ prep_one_table(.x, drop_tab_vars = drop_tab_vars,
                     wrap = wrap, compute = compute, theme_cols = theme_cols,
                     var_names = var_names, transposed = isTRUE(transpose), lang = lang,
                     color_legend = color_legend)
  )

  # Opt-in transpose-at-export, shared by all four exporters (console never transposes). Runs on the
  # FINISHED render model -- colours and cell strings are computed per (correct, homogeneous) source
  # column, then rows/columns are swapped as plain data. Doing it on the tabxplor_fmt fields directly
  # mis-colours numeric cells, because a transposed column is heterogeneous and one fmt column cannot
  # carry two type/colour values.
  if (isTRUE(transpose)) {
    tables <- purrr::map(tables, tx_transpose_render, backend = backend)
  }

  # The graceful-degrade notice is decided ONCE for the whole batch: flagged only when NO table in the
  # batch built a real fmt table (a formatted table shown alongside a degraded one would make the
  # message misleading), and then only once, not per degraded table.
  degraded <- purrr::map_lgl(tables, ~ isTRUE(.x$vars$degrade))
  notify_i <- if (any(degraded) && !any(!degraded)) which(degraded)[1] else 0L
  for (i in seq_along(tables)) {
    if (isTRUE(tables[[i]]$vars$degrade)) tables[[i]]$vars$notify <- (i == notify_i)
  }

  structure(
    list(
      tables = tables,
      # ⚠ `color_legend` is NOT here: whether a table gets a legend is a PER-TABLE fact and lives on
      # each `rd` as `want_legend`, so the backends read one answer instead of computing three.
      meta = list(backend = backend, theme = theme[1], theme_cols = theme_cols,
                  compute = compute)
    ),
    class = "tabxplor_render"
  )
}
