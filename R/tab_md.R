# PURPOSE: the Markdown exporter -- a plain, human-readable pipe table, colours as pandoc spans.
# ROLE: a backend over tab_export_prep(), beside the html engine and tab_xl().
# KEY CONSTRAINTS:
#   - A PANDOC PIPE TABLE HAS ONE HEADER ROW, so the prep's other two (the col_var span and the UNIT
#     row -- R/tab-export-prep.R) become BODY rows under the delimiter, in emphasis. WARNING:
#     emphasis, not a `.tx-unit` span -- a span costs 13 characters of raw line width the fixed-width
#     grid cannot absorb, and a monochrome table must carry no pandoc span at all.
#   - Padding is monospace-precise (numbers right-aligned, pipes aligned), because the raw file is
#     meant to be readable as text. A bold row may touch its pipes; a normal cell keeps a 1-space
#     margin.
#   - A COLOURED table wraps each fmt cell's PRIMARY TOKEN in a break-derived span
#     `[<num>]{.class} (<aside>)`, the same rule html states: the span carries the value and the
#     stars or marks it wears, the aside beside it stays plain. An uncoloured cell gets no span at
#     all, and an uncoloured table is byte-identical to the plain padded layout: no span, no div.
#   - ONE WIDTH INVARIANT: every cell's last text character lands on raw column 1 + num_width, so
#     markup grows leftwards into the cell's own pad. md_cell_markup() is the ONE place that measures
#     it (`pre` / `post`), read by both the width pass and the body loop -- they cannot disagree.
#   - The class names are palette- and theme-INDEPENDENT (a slot, not a hex). tab_css(format = "md")
#     maps them, which is what lets one stylesheet serve a whole document.
# See: CLAUDE.md section "tabxplor architecture" (exports and rendering); R/tab-css.R (the classes).

#' Render a table as Markdown
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
#' @param css When `TRUE` (the default), prepend an inline `<style>` block so the exported markdown is
#'   self-contained and renders coloured and compact on its own. Set `FALSE` inside an `.Rmd`/`.qmd`
#'   document once the host page brings the stylesheet (or call \code{\link{tab_css}} once at the top
#'   for the whole document) -- otherwise the `<style>` block is duplicated per table. A plain
#'   uncoloured table renders byte-identical either way.
#' @param clipboard Copy output to clipboard via \code{clipr::write_clip()} (requires \pkg{clipr}).
#' @param file Path to write the markdown to a file. `NULL` (default) skips.
#' @param print If `TRUE`, print via `cat()` and return invisibly; if `FALSE`, return the string.
#' @param ... Retired arguments, accepted and ignored with a deprecation message since 2.0.0
#'   (`color_type`, `html_24_bit`): colour is a CSS class, and exports are always 24-bit.
#'   Anything else is an error naming the argument you meant, as it already was in [tab()].
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
# === SECTION: the entry point ======================================================================

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
  tx_export_dots(rlang::list2(...), "tab_md", rlang::caller_env())
  .cb <- push_color_breaks(tabs); on.exit(pop_color_breaks(.cb), add = TRUE)
  if (lifecycle::is_present(title)) {
    lifecycle::deprecate_soft("2.0.0", "tab_md(title)", "tab_md(caption)")
    caption <- title
  }
  # `col_var_names` (md-only) generalised to the shared `var_names`, which also governs the row-variable
  # name and is honoured by every exporter. FALSE drops the col side of whatever `var_names` asks for,
  # so the two compose rather than fight.
  if (lifecycle::is_present(col_var_names)) {
    lifecycle::deprecate_soft("2.0.0", "tab_md(col_var_names)", "tab_md(var_names)")
    if (!isTRUE(col_var_names)) {
      var_names <- resolve_export_opts(var_names = var_names)$var_names
      var_names <- if (identical(var_names, "cols")) "none" else
                   if (identical(var_names, "both")) "rows" else var_names
    }
  }
  # `allow_auto`: markdown carries a stylesheet (css = TRUE / tab_css()), so it can follow the reader's
  # colour scheme -- the spans themselves are theme-independent (only the CSS differs).
  o <- resolve_export_opts(theme = theme, color = color, color_legend = color_legend,
                           transpose = transpose,
                           var_names = var_names, allow_auto = TRUE, tabs = tabs)
  theme <- o$theme; color <- o$color; color_legend <- o$color_legend

  # a single tab (or a mergeable list) renders as ONE table; a non-mergeable list renders each table
  # one-after-another (list_method = TRUE), keeping its own tab_vars sub-tables (drop_tab_vars = FALSE).
  # "colors" in `compute` fills the per-cell slots fmt_col_ann() carries -> md_render_one() renders them.
  compute <- "refs"
  if (bold_references) compute <- c(compute, "bold")
  if (color) compute <- c(compute, "colors")
  # a table carrying subordinate tables (meta$footer_tabs) enters as the LIST it means, so the same
  # list path renders them under it -- one pipe table after another (tx_with_footer_tabs).
  tabs_x    <- tx_with_footer_tabs(tabs)
  tabs_list <- if (is.data.frame(tabs_x) || !is.list(tabs_x)) list(tabs_x) else tabs_x
  prep <- tab_export_prep(tabs_x, backend = "md", drop_tab_vars = FALSE, wrap = NULL,
                          compute = compute, transpose = o$transpose,
                          theme = theme, var_names = o$var_names, list_method = TRUE,
                          color_legend = color_legend, lang = lang, what = "tab_md()")

  # WARNING: the POSITION, never imap()'s `i` -- a NAMED list makes `i` the name and `i == 1` silently
  # FALSE on every table, so the caption is dropped with no error. Same trap as xl_check_images().
  parts   <- purrr::map_chr(seq_along(prep$tables), function(i) {
    rd  <- prep$tables[[i]]
    cap <- rd_caption(rd, if (i == 1L) caption else NULL)   # user caption= applies to the FIRST table
    txt <- md_render_one(rd, special_formatting = special_formatting, wrap_rows = wrap_rows,
                         subtext = subtext, color = color, css = css, lang = lang,
                         title = cap,
                         theme = theme)
    # the NOTES this table carries -- a character grid, and the regression's observed curves where the
    # base-count cell cannot hold them -- as pipe tables of their own below its footer.
    for (nt in footer_notes(tabs_list[[i]], "md"))
      txt <- paste(c(txt, "", note_md(nt)), collapse = "\n")
    txt
  })
  md_text <- paste(parts, collapse = "\n\n")

  # a STYLED table is wrapped in a pandoc fenced div: pandoc emits a BARE `<table>` for a pipe table,
  # which none of tab_css()'s `.tabxplor-tab ...` rules can reach, so `::: {.tabxplor-tab}` (rendered
  # as `<div class="tabxplor-tab">`) is the hook every selector needs. The div is DECOUPLED from
  # `<style>`: a table is styled when it is coloured OR `css = TRUE`, so the "one tab_css() per
  # document" workflow reaches a coloured `tab_md(css = FALSE)` too. A plain uncoloured table stays
  # byte-identical (no div).
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


# === SECTION: the pipe-table renderer ==============================================================

# a frame that cannot be read as a tabxplor table still has to come out as markdown, so it comes out
# as a plain pipe table -- numbers right, everything else left, the only alignment a pipe table has.
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

# Renders ONE prepared table (`rd`, from tab_export_prep) to a markdown string -- tab_md() joins the
# parts and handles file/clipboard/print. When the table carries colours, every fmt cell is wrapped in
# a pandoc bracketed span `[<num>]{.class}` (uncoloured cells get the neutral `.n`), keeping the
# numbers aligned in raw text; an uncoloured table renders the byte-identical plain padded table.
md_render_one <- function(rd, special_formatting, wrap_rows, subtext,
                          color = TRUE, css = FALSE, lang = NULL, title = NULL,
                          theme = NULL) {
  if (isTRUE(rd$vars$degrade)) {
    if (isTRUE(rd$vars$notify)) tab_degrade_inform(rd$vars$reason)
    return(md_plain_pipe(rd$tab))
  }

  tabs         <- rd$tab
  subtext_text <- if (subtext) rd$subtext else character(0)

  # the LABEL columns (a merged table's synthetic name column, or the kept tab_vars) and their runs.
  label_cols   <- rd$roles$label_cols
  label_runs   <- rd$roles$label_runs
  var_name_col <- rd$roles$var_name_col

  # the whole footer prose (weight -> Model: -> colour legend -> stars -> user subtext) via the ONE
  # shared builder; the break-words use the same pandoc span classes as the cells (both call
  # tx_slot_class()), so tab_css() colours them identically. The source is `rd$color_src` for a
  # transposed model (whose rd$tab is plain character), so weight/stars/legend read the right
  # attributes. Legend only when coloured.
  src          <- if (is.null(rd$color_src)) tabs else rd$color_src
  subtext_text <- rd_blocks(src, "md", theme = theme, want_legend = isTRUE(rd$want_legend),
                            subtext = subtext_text, lang = lang,
                            host = !isTRUE(rd$subordinate))

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
  cvh        <- rd$col_var_header      # spanning names + suffix-stripped level labels

  # md-local: positions where a REAL col_var changes (span-header separators), through the shared
  # roles_col_var_edges() -- md's convention is the right edge of each block, counting a transition
  # only between two REAL col_vars, so a helper column (`n`, a total) never opens a span block.
  new_col_var <- if (has_multi_col_vars)
    roles_col_var_edges(col_var_map, other_cols, real_col_vars,
                        side = "right", real_only = TRUE) else integer(0)

  # --- Step 6: Format all cells to character ---
  # the reference masks are reused from the prep's `ann` (.ref) so format() does not re-run
  # get_reference(). stars = TRUE right-pads the star field so numbers stay aligned; trim only the
  # leading side to preserve that trailing pad. bold_split = TRUE also attaches the primary token's
  # RANGE on the UN-trimmed string; trimming the left shifts it by the spaces removed.
  # THREE numbers come out of it, and they are what the whole layout reads:
  #   `s` the span's start, `b` the BOLD's end (the value), `e` the SPAN's end (value + its stars or
  #   marks -- a supporting piece that must take the cell's colour, not the aside's plain ink).
  # `s = NA` means "this cell has no primary at all" -> no span, no bold (a `{n_range}` Total cell).
  fmt_out <- purrr::imap(tabs, \(col, nm) {
    if (is_fmt(col)) {
      # pad the VALUE-INTERNAL alignment with a FIGURE SPACE, not ASCII: rendered to html the cells sit
      # in a PROPORTIONAL font, where an ASCII space is half a digit wide and CSS collapses runs of
      # them ("100% (n=  673)" arrives ragged); a figure space is a digit wide and never collapses.
      # `format()`'s internal pad only -- the CELL-EDGE pad (pad_cell / md_color_cell) stays ASCII on
      # purpose, since pandoc must see an empty cell as `<td></td>` (`:empty`) for the spacer/blank-row
      # mechanisms to key on. nchar is unchanged (one codepoint either way).
      raw     <- format(col, special_formatting = special_formatting, na = "", stars = TRUE,
                        theme = if (isTRUE(color)) theme else NULL,
                        bold_split = TRUE, pad = fig_space,
                        .ref = ann_ref(rd$ann[[nm]]))
      pn      <- attr(raw, "primary_nchar")
      pf      <- attr(raw, "primary_from")
      sfx     <- attr(raw, "suffix_nchar") %||% 0L      # the stars / marks run, a column-wide width
      trimmed <- trimws(raw, which = "left", whitespace = "[\\h\\v]")
      lead    <- nchar(raw) - nchar(trimmed)
      trimmed[is.na(trimmed)] <- ""
      n <- nchar(trimmed)
      if (is.null(pn)) {                                # no recorded range: the cell IS its primary
        list(txt = trimmed, s = rep(1L, length(col)), b = n, e = n)
      } else {
        st <- pmax(1L, pf - lead)                       # the trim only ever eats the primary's own pad
        bd <- pf + pn - 1L - lead
        en <- pmin(n, bd + sfx)
        none <- is.na(pn) | pn == 0L | bd < st
        st[none] <- NA_integer_; bd[none] <- NA_integer_; en[none] <- NA_integer_
        if (color_whole_cell_opt()) { st[!none] <- 1L; en[!none] <- n[!none] }
        list(txt = trimmed, s = st, b = bd, e = en)
      }
    } else {
      # a `|` in a level or tab_var label would open a spurious cell and desync the row's column count;
      # pandoc renders `\|` as a literal pipe. Only the non-fmt (label) columns can contain one.
      txt <- gsub("|", "\\|", as.character(col), fixed = TRUE)
      list(txt = txt, s = rep(1L, length(col)), b = nchar(txt), e = nchar(txt))
    }
  })
  cell_data <- as.data.frame(lapply(fmt_out, `[[`, "txt"), stringsAsFactors = FALSE)
  # the primary token's ranges per cell (see Step 6's comment): span start, bold end, span end
  s_mat <- do.call(cbind, lapply(fmt_out, `[[`, "s"))
  b_mat <- do.call(cbind, lapply(fmt_out, `[[`, "b"))
  e_mat <- do.call(cbind, lapply(fmt_out, `[[`, "e"))

  # Truncate row labels (10f: only when wrap_rows is set; default NULL = lossless, column grows).
  # A pipe cell cannot hold a raw newline, so md "wrap" means "do not truncate by default".
  if (!is.null(wrap_rows)) {
    for (j in other_cols) {
      cell_data[[j]] <- tx_str_trunc(cell_data[[j]], wrap_rows)
    }
  }

  # name each block ONCE -- blank every label cell that is not a run start (the run model is the prep's,
  # shared with the html rowspan and the Excel merge). The name column is ITALIC (mirrors the col_var
  # name row below); tab_var cells stay plain, since their values ARE levels ("Male"), not names. Done
  # BEFORE the width pass, so the markup is measured.
  # `styled`: in a styled table (rendered to html) a blanked continuation LABEL cell must be a
  # non-breaking space, not "" -- an :empty <td> makes the CSS col_var-separator rule misfire (a
  # "ragged" leftmost border on continuation rows). Plain tables keep "".
  do_color <- isTRUE(rd$roles$has_color)
  styled   <- do_color || isTRUE(css)
  blank_lbl <- if (styled) "\u00a0" else ""
  for (k in seq_along(label_cols)) {
    idx <- label_cols[[k]]
    if (is.na(idx) || idx > length(cell_data)) next
    show <- label_runs[[k]]$show
    cell_data[[idx]][!show] <- blank_lbl
    if (idx %in% unname(var_name_col)) {
      nz <- show & nzchar(cell_data[[idx]]) & !is.na(cell_data[[idx]]) & cell_data[[idx]] != blank_lbl
      cell_data[[idx]][nz] <- paste0("*", cell_data[[idx]][nz], "*")
    }
  }

  is_right <- fmt_mask  # named logical: TRUE for fmt (right-aligned) columns

  # the spacer-column set. Plain tables keep ONLY the col_var-group spacers; a STYLED table adds thin
  # spacer columns at the interior boundaries other exports draw as vertical rules (levels|first
  # number, numbers|Total, right of Total), reusing the :empty -> CSS border-left mechanism.
  # `md_insert_col_sep` inserts a spacer AFTER each index in `sep_after`; `has_sep` enables it.
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

  # blank out the label columns' header names (they label sub-tables, not real columns): the `""`
  # sentinel drives `col_names` at Step 7. WARNING: this is NOT the prep's header blanking and must not
  # be folded into it -- the prep keeps a real variable name (`marital`) in cvh$clean for the backends
  # that show it, while md renders every label column's name as a body row instead.
  for (idx in unname(label_cols))
    if (!is.na(idx) && idx <= length(cell_data)) names(cell_data)[idx] <- ""

  # --- Step 6b: per-cell pandoc span attributes (colour) ---
  # a table is "coloured" iff some fmt column carries a colour measure; attr_mat holds the per-cell
  # "{.class}" string (fmt columns only; "" = no span), a pure function of the palette slot the engine
  # already assigned -- names match tab_kable()'s <td> classes, both styled by one tab_css() stylesheet.
  # in a styled table (rendered to html), spaces in a multi-word LEVEL/label let the host wrap it
  # mid-name -- replaced with a non-breaking space so it holds on one line. nchar is unchanged (U+00A0
  # is one codepoint), so the raw-text column layout stays byte-identical; an unstyled table keeps
  # ASCII spaces so its GFM output stays byte-clean.
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
      # a wrong length here yields NA absorbed into "" by tx_slot_class() -- silently uncoloured
      # cells instead of an error -- so it is asserted rather than left to that fallback.
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
  # the level-header row uses the suffix-stripped labels (the col_var name is in the span row above).
  col_names <- cvh$clean
  # nbsp: the blank row_var/tab_var header cell must not be :empty in a styled table, or the thead
  # col_var-separator rule draws a stray left border on it. Spacer headers stay ASCII (:empty).
  col_names[names(cell_data) == ""] <- if (styled) "\u00a0" else ""

  # For each cell, compute the raw text width
  cell_widths <- matrix(0L, nrow = n_rows, ncol = n_cols)
  for (j in seq_len(n_cols)) {
    cell_widths[, j] <- nchar(cell_data[[j]])
  }
  header_widths <- nchar(col_names)
  # THE UNIT ROW IS A HEADER ROW, so it sizes its column like one -- a "<row% (n)>" tag is regularly
  # wider than the level name above it, and without this the cell overflowed and every pipe below it
  # stepped right.
  if (!is.null(cvh$unit))
    header_widths <- pmax(header_widths, ifelse(nzchar(cvh$unit), nchar(cvh$unit) + 2L, 0L))

  # `bold_rows` is a pure ROW set, so a bold row would bold the LABEL cell too -- the label columns opt
  # out here, at the consumer (the prep cannot know a backend's markup). This is the ONE definition:
  # the width pass below and the Step-11 body loop must charge the same markup, or the column over-pads.
  no_bold      <- seq_len(n_cols) %in% label_cols
  bold_rows_of <- function(j) if (no_bold[j]) integer(0) else bold_rows

  # --- Step 7b: the markup of every coloured cell, built ONCE -------------------------------------
  # md_cell_markup() is the one definition; the width pass and the Step-11 body loop read the SAME
  # `pre` / `post`, so a column can never over- or under-pad.
  mk_str  <- matrix("", n_rows, n_cols)
  mk_pre  <- matrix(0L, n_rows, n_cols)
  mk_post <- matrix(0L, n_rows, n_cols)
  attr_width <- integer(n_cols)
  if (do_color) for (j in which(is_right)) {
    nz <- nzchar(cell_data[[j]])
    # the attr is padded to the column's widest, so the closing `}` -- and every coloured cell's
    # number -- line up in the raw file (pandoc ignores spaces inside `{...}`).
    attr_width[j] <- if (any(nz)) max(nchar(attr_mat[nz, j])) else 0L
    bj <- seq_len(n_rows) %in% bold_rows_of(j)
    for (i in seq_len(n_rows)) {
      m <- md_cell_markup(cell_data[[j]][i], attr_mat[i, j], bj[i],
                          s_mat[i, j], e_mat[i, j], b_mat[i, j], attr_width[j])
      mk_str[i, j] <- m$str; mk_pre[i, j] <- m$pre; mk_post[i, j] <- m$post
    }
  }

  # Column width = max of display widths:
  #   right-aligned normal: nchar + 3 (1 leading + 2 trailing for bold zone)
  #   left-aligned normal:  nchar + 2 (1 space each side)
  #   bold cell:            nchar + 4 (**...**)
  #   header:               nchar + 2
  # A COLOURED fmt column reads the same 3-column margin, and everything else is measured from the
  # markup actually built: `num_width` is the widest cell UP TO its last text character (value plus
  # the markup preceding it), `post_width` the widest run trailing it. So the invariant holds whatever
  # a cell contains: every cell's last text character lands on raw column 1 + num_width.
  col_width  <- integer(n_cols)
  num_width  <- integer(n_cols)
  post_width <- integer(n_cols)
  for (j in seq_len(n_cols)) {
    if (do_color && is_right[j]) {
      nonempty <- nzchar(cell_data[[j]])
      num_width[j]  <- if (any(nonempty)) max(cell_widths[nonempty, j] + mk_pre[nonempty, j]) else 0L
      post_width[j] <- if (any(nonempty)) max(mk_post[nonempty, j]) else 0L
      col_width[j]  <- max(num_width[j] + post_width[j] + 3L, header_widths[j] + 2L)
    } else {
      margin <- if (is_right[j]) 3L else 2L
      widths <- cell_widths[, j] + margin  # normal cells
      bj     <- bold_rows_of(j)
      # a cell with no primary at all is never bolded, so it is charged nothing for it
      if (length(bj) > 0) widths[bj] <- cell_widths[bj, j] + ifelse(is.na(s_mat[bj, j]), 0L, 4L)
      col_width[j] <- max(c(widths, header_widths[j] + 2L))
    }
  }

  # --- Helper: pad a cell ---
  # is_right: TRUE for fmt (right-aligned), FALSE for text (left-aligned)
  # is_bold: TRUE to wrap with **
  # s/b: the primary token's range (`s = NA` -> the cell has no primary, so nothing is bolded).
  pad_cell <- function(text, width, is_right, is_bold, s = NA_integer_, b = NA_integer_) {
    if (is_bold && !is.na(s) && nchar(text) > 0) {
      bold_text <- md_bold(text, s, b)                    # partial (composite) or whole-cell bold
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

  # --- Step 8: the col_var-name row ---
  # WARNING: this row is a BODY row, emitted AFTER the delimiter (see Step 13) -- pandoc pipe tables
  # have no two-row header: placed above the level-name header it silently rendered the whole table as
  # a line-block, not a table (verified with pandoc 3.7). Below the delimiter it parses, styled as
  # data: the name sits in the FIRST cell of its group (pandoc pipe tables cannot colspan a centred
  # span), italic, so it reads as a sub-heading rather than a value.
  # `var_names` drops it by blanking `cvh$label` in the prep, so this gate needs no argument of its own.
  col_var_header_line <- NULL
  if (any(nzchar(cvh$label))) {
    # grouped by tab_header_runs() (the shared RLE). The name sits in the FIRST cell of its run
    # (italic), the rest are width-padded blanks -- one cell PER column, since a pipe row must keep the
    # table's cell count. A long name simply overflows its own cell: the row is deliberately not
    # pipe-ALIGNED, because padding to it would widen every column below it. Routed through
    # md_insert_col_sep(sep_after) exactly like the body, so the spacer columns line up across rows.
    # a span belonging to a SUB-POPULATION composes on ONE line ("*2000 marital*"): md is the one
    # backend that cannot draw two lines in a cell (a pipe row IS one line).
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

  # THE UNIT ROW -- what each column HOLDS (see the file header): a BODY row, italic, sitting directly
  # under the name row, inside the header block the blank underline closes.
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
      bold_j <- is_bold && !no_bold[j]     # see bold_rows_of() above
      if (do_color && is_right[j]) {
        row_cells[j] <- md_color_cell(mk_str[i, j], mk_pre[i, j], cell_widths[i, j],
                                      num_width[j], col_width[j])
      } else {
        row_cells[j] <- pad_cell(cell_data[[j]][i], col_width[j],
                                  is_right[j], bold_j, s_mat[i, j], b_mat[i, j])
      }
    }
    body_lines[i] <- md_insert_col_sep(row_cells, sep_after, n_cols,
                                        has_sep)
  }

  # --- Step 12: Insert sub-table separators ---
  if (length(new_group) > 0) {
    # on the STYLED path a sub-table boundary is a fully-blank row (all cells :empty) that tab_css()
    # collapses to a 1px border-top, theme-aware, with no dash marker in the raw markdown. The PLAIN
    # path keeps the dash row, so its GFM/text output stays byte-clean.
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
  # on the styled path, follow the col_var-name row with a blank row -> tab_css() draws a 1px
  # border-top under it, theme-aware, with no dash in the raw markdown. Only when the name row exists
  # (var_names may have dropped it).
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


# === SECTION: padding and alignment helpers ========================================================

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


# a fully-blank pipe row -- every cell is ASCII spaces, so pandoc renders each cell `<td></td>`
# (`:empty`), which tab_css() selects and collapses to a 1px border-top: the sub-table / col_var-name
# rule, theme-aware, marker-free in the raw markdown.
# WARNING: ASCII spaces ONLY. A FIGURE space (U+2007) renders `<td> </td>`, not `:empty`, and the
# collapse dies -- the figure-space swap elsewhere is confined to a value's INTERNAL padding for
# exactly this reason; the cell-edge pad here must stay ASCII.
md_blank_row <- function(col_width, new_col_var, n_cols, has_multi_col_vars) {
  md_insert_col_sep(strrep(" ", col_width), new_col_var, n_cols, has_multi_col_vars)
}

# a width-padded blank cell that is NOT :empty (a leading non-breaking space) for STYLED
# tables, so the CSS col_var-separator rule fires only on the true ASCII spacer columns -- used for the
# nearly-blank col_var-name span row. Plain tables keep ASCII spaces (byte-clean GFM). Vectorised.
md_pad_blank <- function(widths, styled) {
  if (styled) tx_pad("\u00a0", widths, "right") else strrep(" ", widths)
}





# === SECTION: the colour spans (break-derived pandoc classes) ======================================

# The pandoc bracketed-span attribute for ONE cell: "{.p3 .o2}" / "{.p3}" / "{.o2}" / "" (uncoloured).
# The class names come from the shared slot vocabulary (tx_slot_class, R/tab-css.R), so a markdown
# span and an html <td> name the same class and ONE stylesheet (tab_css()) styles both. An uncoloured
# cell gets NO span at all -- md_color_cell() keeps it aligned instead.
md_span_attr <- function(text_slot, bg_slot) {
  parts <- c(tx_slot_class("text", text_slot), tx_slot_class("bg", bg_slot))
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0L) return("")
  paste0("{", paste0(".", parts, collapse = " "), "}")
}

# One fmt cell of a coloured column, padded to `total_width`. `str` and `pre` come from
# md_cell_markup(); `n` is the cell's text width.
# the alignment target is the LAST TEXT CHARACTER, not the markup: `[`/`**` are invisible once
# rendered but occupy columns in the raw file, so each cell's markup PREFIX grows leftwards into its
# own pad -- letting a bold cell `**54%**` and a coloured one `[42%]{.m2}` end on the same raw column.
# DESIGN: an UNCOLOURED cell carries no span at all -- its pad absorbs the missing markup, so it
# aligns without a do-nothing `.n` class.
md_color_cell <- function(str, pre, n, num_width, total_width) {
  if (!nzchar(str)) return(strrep(" ", total_width))
  tx_pad(paste0(" ", strrep(" ", max(0L, num_width - n - pre)), str), total_width, "right")
}

# === SECTION: one cell's markup, and the two numbers the layout reads =============================

# THE CELL'S RENDERING STOPS AT ITS PRIMARY TOKEN, exactly as in html (R/tab-render-html.R): the
# colour span wraps `s..e` -- the value plus the stars or marks it wears -- and the aside beside it
# stays plain. The bold, which is a ROW property, stops earlier, at the value (`s..b`).
# Returns the built string plus `pre` / `post`: how many raw columns of markup sit before and after
# the cell's last text character. They are the ONE definition the width pass and the body loop share.
# `**` goes INSIDE the span: the span must be the outer one, since it contains the suffix the bold
# does not (`[**42%**\u207a]{.p1}`).
md_cell_markup <- function(text, attr, is_bold, s, e, b, attr_width) {
  if (!nzchar(text)) return(list(str = "", pre = 0L, post = 0L))
  n <- nchar(text)
  if (is.na(s)) return(list(str = text, pre = 0L, post = 0L))   # no primary: nothing to mark
  before <- substr(text, 1L, s - 1L)
  core   <- substr(text, s, e)
  after  <- substr(text, e + 1L, n)
  trail <- 0L                                        # markup after the last character of `core`
  if (is_bold) {
    bt <- md_bold(core, 1L, b - s + 1L)
    # the closing `**` trails only where the bold reaches the core's end AND no alignment filler
    # follows it (md_bold keeps the filler outside the markers).
    if (!identical(bt, core) && b >= e && !grepl(paste0(MD_FILL, "$"), core)) trail <- 2L
    core <- bt
  }
  if (nzchar(attr)) {
    a2 <- if (attr_width > nchar(attr))
      sub("[}]$", paste0(strrep(" ", attr_width - nchar(attr)), "}"), attr) else attr
    core  <- paste0("[", core, "]", a2)
    trail <- trail + 1L + nchar(a2)
  }
  str  <- paste0(before, core, after)
  post <- if (nzchar(after)) 0L else trail           # an aside after the span pushes it all before
  list(str = str, pre = nchar(str) - n - post, post = post)
}

# the alignment fillers: ASCII space, no-break U+00A0, figure U+2007, narrow no-break U+202F.
MD_FILL <- paste0("[", intToUtf8(c(32L, 160L, 8199L, 8239L)), "]")

# Wraps a cell's PRIMARY token in **...**; asides beside it (on either side) stay plain, a plain cell
# (no recorded range) is bolded whole -- exactly one ** pair either way, matching the +4 width budget
# the column-width computation reserves for bold cells.
# the alignment pad (leading/trailing spaces, incl. the figure-space fill) is kept OUTSIDE the **
# markers: `**77%   **` is not valid markdown bold (pandoc will not open an emphasis span ending in
# whitespace), `**77%**   ` is, and the outer pad still holds the raw-text column alignment.
md_bold <- function(text, from = NA_integer_, to = NA_integer_) {
  ws <- MD_FILL
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


#' Render a table as a plain pipe table
#'
#' @description
#' The markdown grid without the markup: one GFM pipe table, its unit line kept (\verb{<col%>},
#' \verb{<var>}), no colour spans, no footer and no stylesheet. It is what the console prints under a
#' table for each of its subordinate tables (\code{\link{set_footer_tabs}}), the same shape a regression's
#' \emph{shape table} takes there --- a grid one can read as text and paste anywhere.
#'
#' It is \code{\link{tab_md}} with three arguments fixed, not a second renderer: a pipe table that
#' drifted from the markdown export would be a second answer to one question.
#'
#' @param tabs A \code{tabxplor_tab}, or a list of them.
#' @param ... Passed to \code{\link{tab_md}} --- `color = TRUE` brings the colour spans back,
#'   `subtext = TRUE` the footer.
#' @return A character vector, one element per line.
#' @seealso [tab_md()], [set_footer_tabs()].
#' @export
#' @examples
#' cat(tab_pipe(tab(forcats::gss_cat, race, marital, pct = "row")), sep = "\n")
tab_pipe <- function(tabs, ...) {
  # `...` OVERRIDES the three defaults rather than colliding with them: they are a starting point,
  # not a contract, and `tab_pipe(t, color = TRUE)` must reach tab_md() once.
  args <- utils::modifyList(list(css = FALSE, color = FALSE, subtext = FALSE), rlang::list2(...))
  txt  <- do.call(tab_md, c(list(tabs), args, list(print = FALSE)))
  strsplit(txt, "\n", fixed = TRUE)[[1L]]
}
