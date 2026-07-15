# PURPOSE: The final-HTML render seam for tab_kable() + a dependency-free home-built <table> engine.
# ROLE: Phase 10e. tab_kable() = option resolution + tab_export_prep() + map(render_kable_html) + join.
#       render_kable_html() isolates the engine so the render-model (rd, meta) stays engine-agnostic.
# KEY CONSTRAINTS:
#   - engine = "kableExtra" is the DEFAULT and reproduces the pre-10e output BYTE-IDENTICALLY (the
#     legacy row_spec()/column_spec() pipeline, just carved out of tab_kable() and reading the prep's
#     derive-once roles instead of recomputing them). Locked by test-exports.R + the new kable HTML
#     snapshot (test-render-html.R).
#   - engine = "html" is the home-built renderer: geometry inline on <td>, colour as a slot CLASS (no
#     per-cell <span>), and ~O(n_col+n_row) paste0 calls (Phase 9 idiom: base masks, vectorised
#     assembly, NO case_when/if_else over fmt). It reproduces the kableExtra visual (content-identical
#     -- same cell text, colours, tooltips), not byte-identical DOM.
#   - Phase 13d: this engine is THEME-AGNOSTIC. Colour lives in classes (tx_slot_class) resolved by the
#     stylesheet tab_css() builds, which is what makes theme = "auto" possible: an inline `style` beats
#     every stylesheet rule short of `!important`, so inline hex could never follow a dark-mode toggle.
#     Do not reintroduce inline colour here. The <style> is hoisted ONCE by tab_kable_join(); it works
#     in jamovi (its results view injects our HTML through jQuery .html(), which applies <style> nodes,
#     and it has no sanitizer on that path -- what jamovi ignores is htmlDependency, not <style>).
#   - The two engines wire the SAME 10e features (spanning header, [min;max] total column, NA="") off
#     the shared render-model, so they cannot drift.
# See: dev/tabxplor_phase10_exporters.md Sec 10, CLAUDE.md Phase 10e + 13d, R/tab-css.R.


# === SECTION: the seam =================================================================

# Render ONE prepared table (rd = prep$tables[[i]], meta = prep$meta) to HTML. `subtext` already has
# the colour legend prepended by tab_kable() (it is content, not styling). Called once per table; the
# list method maps it over prep$tables and joins with tab_kable_join().
#' @keywords internal
render_kable_html <- function(rd, meta,
                              engine   = c("kableExtra", "html"),
                              subtext  = character(0),
                              caption  = NULL,
                              tooltips = TRUE, popover = FALSE,
                              html_font = NULL, full_width = FALSE,
                              get_data = FALSE, in_knitr = FALSE, ...) {
  engine <- match.arg(engine)

  # per-table graceful degrade (mirrors md_render_one(): a list may hold a malformed table)
  if (isTRUE(rd$vars$degrade)) {
    tab_degrade_inform(rd$vars$reason)
    if (engine == "html") return(render_html_degrade(rd$tab))
    return(kableExtra::kbl(tibble::as_tibble(rd$tab)))
  }

  if (engine == "html") {
    return(render_html_engine(rd, meta, subtext = subtext, caption = caption,
                              tooltips = tooltips, popover = popover, get_data = get_data))
  }

  render_kableExtra_engine(rd, meta, subtext = subtext, caption = caption,
                           tooltips = tooltips, popover = popover, html_font = html_font,
                           full_width = full_width, get_data = get_data, in_knitr = in_knitr, ...)
}


# === SECTION: the legacy kableExtra engine (byte-identical carve of tab_kable() 536-708) ==========

#' @keywords internal
# Phase 13c-ii: build one HTML cell string, keeping only the PRIMARY field of a composite bold cell
# bold. A bold composite cell (`pn` = primary-field width) gets its escaped suffix wrapped in a
# normal-weight <span> so the "(n=...)" part overrides the inherited row/cell bold; each part is
# escaped separately so the offset can't drift. Non-composite / non-bold cells are just escaped.
# `esc`: the HTML-escape fn -- htmltools::htmlEscape for the cell_spec path (escape = FALSE downstream,
# byte-identical to escape = TRUE), identity for the home-built engine (which places cells raw).
#' @keywords internal
html_cell_text <- function(raw, pn, bold, esc = htmltools::htmlEscape) {
  out <- esc(raw)
  if (is.null(pn)) return(out)
  hit <- bold & !is.na(pn) & pn >= 1L & pn < nchar(raw)
  if (any(hit)) {
    out[hit] <- paste0(esc(substr(raw[hit], 1L, pn[hit])),
                       "<span style=\"font-weight:normal;\">",
                       esc(substr(raw[hit], pn[hit] + 1L, nchar(raw[hit]))), "</span>")
  }
  out
}

render_kableExtra_engine <- function(rd, meta, subtext, caption, tooltips, popover,
                                     html_font, full_width, get_data, in_knitr, ...) {
  tabs  <- rd$tab
  # Phase 13d: only ever "light"/"dark" here -- tab_kable() downgrades "auto" before the prep, because
  # kableExtra bakes its theme at render time (kable_classic / kable_material_dark) and its HTML is not
  # ours to restyle. Auto dark mode is an engine = "html" feature.
  theme <- meta$theme
  cvh   <- rd$col_var_header       # Phase 13c-iii: spanning names + suffix-stripped level labels

  # kableExtra-only: escape markdown stars in knitr contexts, else the significance `*` become markdown
  # (byte-identical to the old in-tab_kable escape). NOT done for the html engine (raw HTML fragment).
  if (in_knitr) {
    tabs <- tabs |>
      dplyr::mutate(dplyr::across(dplyr::where(is.character),
                                  ~ stringr::str_replace_all(., "\\*", "\\\\*")))
  }

  new_group   <- rd$roles$new_group
  row_var     <- rd$roles$row_var_col
  fmt_cols    <- rd$roles$fmt_cols
  other_cols  <- rd$roles$other_cols
  totcols     <- rd$roles$totcols
  new_col_var <- rd$roles$new_col_var
  any_bg      <- rd$roles$any_bg

  text_color <- meta$theme_cols$text

  # Per-fmt-column colour vectors (derive-once) from the prep's `ann`.
  color_font <- purrr::map(rd$ann, "font")
  color_back <- purrr::map(rd$ann, "back")
  color_bold <- purrr::map(rd$ann, "bold")

  # Unified fmt-across (was two any_bg branches): background = NULL when the table has no bg channel is
  # identical to omitting it (cell_spec default), so ONE branch reproduces both byte-for-byte.
  # Phase 13c-ii: partial-bold composite cells -- format(bold_split = TRUE) marks the primary-field
  # width; html_cell_text() escapes the value AND wraps a bold cell's composite suffix in a normal
  # <span>, then cell_spec(escape = FALSE) (byte-identical to escape = TRUE for non-composite cells).
  out <- tabs %>%
    dplyr::mutate(dplyr::across(
      where(is_fmt),
      ~ {
        col   <- .
        colnm <- dplyr::cur_column()
        raw   <- format(col, html = TRUE, special_formatting = TRUE, na = "", stars = TRUE,
                        bold_split = TRUE, .ref = ann_ref(rd$ann[[colnm]]))
        boldc <- color_bold[[colnm]]
        txt   <- html_cell_text(raw, attr(raw, "primary_nchar"),
                                (seq_along(raw) %in% rd$bold_rows) | boldc)
        kableExtra::cell_spec(
          txt, escape = FALSE,
          bold       = boldc,
          color      = color_font[[colnm]],
          background = if (any_bg) color_back[[colnm]] else NULL,
          tooltip = if (!popover & tooltips) {
            tab_kable_print_tooltip(col, .ref = rd$ann[[colnm]]$ref_cells)
          } else {NULL},
          popover = if (popover & tooltips) {
            tab_kable_print_tooltip(col, popover = TRUE, .ref = rd$ann[[colnm]]$ref_cells)
          } else {NULL}
        )
      }
    ))

  if (get_data) return(out)

  alignement <- rd$roles$align

  # Phase 13c-iii: level headers use the suffix-stripped labels (the col_var name is written in the
  # spanning header row added below).
  out <- knitr::kable(out, escape = FALSE, format = "html", align = alignement,
                      caption = caption, col.names = cvh$clean)

  # Phase 13c-iii: the col_var spanning-name header row above the level names -- each variable name
  # merged (colspan) over its contiguous level columns; blank (" ") over the row var / total / count
  # columns. Applied on the plain kable (before the theme) so kableExtra emits a clean <div> style.
  runs <- tab_header_runs(cvh$label)
  if (any(nzchar(runs$labels))) {
    header_above <- stats::setNames(runs$spans, ifelse(nzchar(runs$labels), runs$labels, " "))
    out <- kableExtra::add_header_above(out, header_above)
  }

  if (theme == "light") {
    out <- out %>% kableExtra::kable_classic(
      lightable_options = "hover",
      full_width = full_width,
      html_font = html_font,
      ...
    )
  } else {
    out <- out %>% kableExtra::kable_material_dark(
      lightable_options = "hover",
      bootstrap_options = c("hover", "condensed", "responsive"),
      full_width = full_width,
      html_font = html_font,
      ...
    )
  }

  # Bold reference/total rows + total-block borders -- from the prep's derive-once sets (block D).
  tot_or_ref    <- rd$bold_rows
  tot_rows_1    <- rd$roles$totblock_top
  tot_rows_last <- rd$roles$totblock_bottom

  if (length(subtext) != 0) {
    out <- out %>% kableExtra::add_footnote(subtext, notation = "none", escape = FALSE)
  }

  out <- out %>%
    kableExtra::row_spec(
      0, color = text_color, bold = TRUE,
      extra_css = "border-top: 0px solid ; border-bottom: 1px solid ;font-size: 90%;vertical-align: bottom;line-height: 0.9;padding: 3px;text-align: center;"
    ) %>%
    kableExtra::row_spec(tot_or_ref, bold = TRUE) %>%
    kableExtra::row_spec(tot_rows_1, extra_css = "border-top: 1px solid ;") %>%
    kableExtra::row_spec(tot_rows_last, extra_css = "border-bottom: 1px solid ;") %>%
    kableExtra::column_spec(fmt_cols, extra_css = "white-space: nowrap;") %>%
    kableExtra::column_spec(unique(c(new_col_var, ncol(tabs))), border_right = TRUE) %>%
    kableExtra::column_spec(other_cols, border_left = TRUE) %>%
    kableExtra::column_spec(totcols, border_left = TRUE, width_min = 11) %>%
    kableExtra::column_spec(row_var, width_min = 20) %>%
    kableExtra::row_spec(new_group, extra_css = "border-bottom: 2px solid;") %>%
    kableExtra::row_spec(nrow(tabs), extra_css = "border-bottom: 1px solid;") |>
    kableExtra::row_spec(1:nrow(tabs), extra_css = "vertical-align: top; line-height: 0.85;padding: 3px;")

  # Phase 10e: NA cells now render "" at source (format(na="")), so the old post-hoc
  # str_replace_all(">NA</span>", …) is retired; only the tab.css include remains.
  if (getOption("tabxplor.always_add_css_in_tab_kable") | interactive()) {
    out <- paste0(
      htmltools::includeCSS(system.file("tab.css", package = "tabxplor")),
      "\n",
      as.character(out)
    ) |>
      vctrs::vec_restore(out)
  }

  out
}


# === SECTION: the home-built HTML engine =========================================================

# Vectorised, dependency-free <table>: one style string per column, one per row, cells built as
# per-column vectors then concatenated with do.call(paste0, .). Returns the BARE <table> string; the
# <style> block (which carries the whole theme -- see the file header) is hoisted ONCE by
# tab_kable_join(). Nothing here reads `meta$theme`.
#' @keywords internal
render_html_engine <- function(rd, meta, subtext, caption, tooltips, popover, get_data) {
  tab   <- rd$tab
  roles <- rd$roles
  ann   <- rd$ann
  nm    <- names(tab)
  cvh   <- rd$col_var_header       # Phase 13c-iii: spanning names + suffix-stripped level labels
  n_row <- nrow(tab)
  n_col <- ncol(tab)

  # (a) format every column once -> list of chr[n_row] (reuse .ref via the prep's ann). bold_split =
  # TRUE marks the composite primary-field width (Phase 13c-ii) so step (c) can bold only the primary.
  cells <- purrr::imap(tab, function(col, name) {
    if (is_fmt(col)) {
      format(col, html = TRUE, special_formatting = TRUE, na = "", stars = TRUE,
             bold_split = TRUE, .ref = ann_ref(ann[[name]]))
    } else {
      as.character(col)
    }
  })

  if (isTRUE(get_data)) {
    df <- as.data.frame(cells, stringsAsFactors = FALSE, optional = TRUE)
    names(df) <- nm
    return(df)
  }

  # (b) column-CONSTANT style prefix, one string per column
  col_style <- ifelse(roles$align == "r", "text-align:right;", "text-align:left;")
  col_style[roles$fmt_cols]   <- paste0(col_style[roles$fmt_cols], "white-space:nowrap;")
  br_cols                     <- unique(c(roles$new_col_var, n_col))
  col_style[br_cols]          <- paste0(col_style[br_cols], "border-right:1px solid;")
  col_style[roles$other_cols] <- paste0(col_style[roles$other_cols], "border-left:1px solid;")
  col_style[roles$totcols]    <- paste0(col_style[roles$totcols], "border-left:1px solid;min-width:5.5em;")
  col_style[roles$row_var_col]<- paste0(col_style[roles$row_var_col], "min-width:10em;")

  # (c) per-column <td> vectors over rows (colours/bold/tooltips from the prep's ann)
  # Phase 13d: colour is emitted as a slot CLASS, never as inline hex -- an inline `style` beats every
  # stylesheet rule short of `!important`, so inline colour makes dark mode impossible. The class is a
  # pure function of the slot the engine already assigned (tx_slot_class), so cells and tab_css() cannot
  # disagree, and this renderer is THEME-AGNOSTIC: the theme lives only in the stylesheet.
  #   text_slot > 0        -> .p1-.p4 / .m1-.m4      bg_slot > 0 -> .o1-.o4 / .u1-.u4
  #   ref_alltot, slot 0   -> no class: `theme_cols$text` IS the table's colour, so it inherits
  #   otherwise            -> .g1 (column has a colour measure) / .g2 (it has none)
  td_html <- purrr::imap(cells, function(cell, name) {
    a  <- ann[[name]]
    cs <- rep("", n_row)      # inline style: bold only (theme-independent)
    cls <- rep("", n_row)
    if (!is.null(a)) {
      tsl <- if (length(a$text_slot) == n_row) a$text_slot else integer(n_row)
      bsl <- if (length(a$bg_slot)   == n_row) a$bg_slot   else integer(n_row)
      cls <- tx_slot_class("text", tsl)
      bgc <- tx_slot_class("bg",   bsl)
      cls <- paste0(cls, ifelse(nzchar(cls) & nzchar(bgc), " ", ""), bgc)
      grey <- !nzchar(cls) & !a$ref_alltot
      cls[grey] <- if (isTRUE(a$has_color) || isTRUE(a$has_bgc)) "g1" else "g2"
      cs[a$bold] <- "font-weight:bold;"
    }
    tip <- rep("", n_row)
    if (tooltips && is_fmt(tab[[name]])) {
      tp <- tab_kable_print_tooltip(tab[[name]], popover = popover,
                                    .ref = if (is.null(a)) NULL else a$ref_cells)
      nz <- !is.na(tp) & nzchar(tp)
      if (any(nz)) {
        esc <- htmltools::htmlEscape(tp[nz], attribute = TRUE)
        # Match kableExtra's bootstrap tooltip/popover attributes so the JS binds identically in
        # jamovi (data-container/placement) and standalone HTML.
        tip[nz] <- if (popover) {
          paste0(' data-toggle="popover" data-container="body" data-placement="right" title="" data-content="',
                 esc, '"')
        } else {
          paste0(' data-toggle="tooltip" data-container="body" data-placement="right" title="', esc, '"')
        }
      }
    }
    j <- match(name, nm)
    # Phase 13c-ii: in a bold row/cell, bold only the PRIMARY field of a composite "{pct} (n={n})"
    # cell (the "(n=...)" stays plain). Cells are placed raw here, so esc = identity (byte-identical).
    bold_cell <- seq_len(n_row) %in% rd$bold_rows
    if (!is.null(a)) bold_cell <- bold_cell | a$bold
    cell_html <- html_cell_text(cell, attr(cell, "primary_nchar"), bold_cell, esc = identity)
    paste0('<td style="', col_style[j], cs, '"',
           ifelse(nzchar(cls), paste0(' class="', cls, '"'), ""), tip, '>', cell_html, '</td>')
  })

  # (d) rows: paste0 across the LIST of column vectors -> all n_row rows in one call
  row_inner <- do.call(paste0, td_html)

  # (e) row-style vector (index assignment; kable border precedence: last-wins for the final row)
  rs <- rep("vertical-align:top;line-height:0.85;padding:3px;", n_row)
  rs[rd$bold_rows]        <- paste0(rs[rd$bold_rows], "font-weight:bold;")
  rs[roles$totblock_top]  <- paste0(rs[roles$totblock_top], "border-top:1px solid;")
  bottom <- rep("", n_row)
  bottom[roles$totblock_bottom] <- "border-bottom:1px solid;"
  bottom[roles$new_group]       <- "border-bottom:2px solid;"
  bottom[n_row]                 <- "border-bottom:1px solid;"
  rs <- paste0(rs, bottom)

  body <- paste0('<tr style="', rs, '">', row_inner, '</tr>', collapse = "\n")

  # header (level-name row), styled like the legacy row_spec(0)
  # Phase 13d: no `color:` -- the header inherits `.tabxplor-tab{color:}` from the stylesheet, so it
  # flips with the theme. Everything else stays INLINE on purpose: col_style's `text-align:right` is
  # inline, so a stylesheet header rule could not beat it -- the inline-last-wins order below is what
  # centres headers over right-aligned data.
  hdr_style <- paste0(
    "font-weight:bold;border-top:0;border-bottom:1px solid;",
    "font-size:90%;vertical-align:bottom;line-height:0.9;padding:3px;text-align:center;")
  # col_style first (its borders/nowrap/widths apply to the header too), hdr_style last so its
  # text-align:center wins over the column's data alignment (kableExtra centres all headers).
  # Phase 13c-iii: level headers use the suffix-stripped labels (the col_var name is written in the
  # spanning row above).
  head_cells <- paste0('<th style="', col_style, hdr_style, '">',
                       htmltools::htmlEscape(cvh$clean), '</th>')
  thead <- paste0('<tr>', paste0(head_cells, collapse = ""), '</tr>')

  # Phase 13c-iii: the col_var spanning-name header row -- each variable name centred (colspan) over its
  # contiguous level columns; an empty cell over the row var / total / count columns.
  cvh_runs <- tab_header_runs(cvh$label)
  span_thead <- if (any(nzchar(cvh_runs$labels))) {
    span_style <- paste0("font-weight:bold;border-bottom:1px solid;",
                         "text-align:center;padding:3px;")   # colour inherited (Phase 13d)
    span_cells <- paste0('<th colspan="', cvh_runs$spans, '" style="', span_style, '">',
                         ifelse(nzchar(cvh_runs$labels), htmltools::htmlEscape(cvh_runs$labels), ""),
                         '</th>')
    paste0('<tr>', paste0(span_cells, collapse = ""), '</tr>')
  } else ""

  cap <- if (!is.null(caption) && length(caption) && nzchar(caption)) {
    paste0('<caption>', htmltools::htmlEscape(caption), '</caption>')
  } else ""

  tfoot <- if (length(subtext) != 0) {
    paste0('<tfoot><tr><td colspan="', n_col, '">',
           paste0(subtext, collapse = "<br>"), '</td></tr></tfoot>')
  } else ""

  # Phase 13d: no `tabxplor-<theme>` token -- the stylesheet carries the theme, and under "auto" the
  # markup must not commit to one.
  paste0(
    '<table class="tabxplor-tab">', cap,
    '<thead>', span_thead, thead, '</thead>',
    '<tbody>', body, '</tbody>',
    tfoot,
    '</table>'
  )
}


# Minimal escaped <table> for the graceful-degrade path (plain data.frame / no fmt columns).
#' @keywords internal
render_html_degrade <- function(tab) {
  tab <- tibble::as_tibble(tab)
  nm  <- names(tab)
  head_cells <- paste0('<th>', htmltools::htmlEscape(nm), '</th>')
  thead <- paste0('<tr>', paste0(head_cells, collapse = ""), '</tr>')
  cols <- lapply(tab, function(col) paste0('<td>', htmltools::htmlEscape(as.character(col)), '</td>'))
  row_inner <- if (length(cols)) do.call(paste0, cols) else rep("", nrow(tab))
  body <- paste0('<tr>', row_inner, '</tr>', collapse = "\n")
  paste0('<table class="tabxplor-tab"><thead>', thead,
         '</thead><tbody>', body, '</tbody></table>')
}


# === SECTION: join + jamovi helpers ===============================================================

# Join the per-table render parts (single table => length-1 list). kableExtra: concatenate the
# knitr_kable objects (keeping the class). html: hoist ONE <style> block, stack the <table> fragments.
# Phase 13d: `css` is the stylesheet built by tab_kable() (tab_css(); "" when the document supplies it
# itself). It replaced the old static html_style_block() -- the theme now lives entirely in the CSS,
# so this function no longer needs to know it.
#' @keywords internal
tab_kable_join <- function(parts, engine, css = "") {
  if (length(parts) == 1L && engine == "kableExtra") return(parts[[1]])

  if (engine == "html") {
    body <- paste(unlist(parts), collapse = "\n<br>\n")
    out  <- if (nzchar(css)) paste0("<style>", css, "</style>\n", body) else body
    return(structure(out, format = "html", class = "knitr_kable"))
  }

  # kableExtra list: stack the rendered tables one-after-another. Phase 13c-iv: give the joined HTML
  # the `kableExtra` class so print.kableExtra routes it to the Viewer (like a single table does),
  # instead of the bare `knitr_kable` that just cat()s to the console.
  chr <- vapply(parts, as.character, character(1))
  structure(paste(chr, collapse = "\n<br>\n"), format = "html",
            class = c("kableExtra", "knitr_kable"))
}

# Wrap a home-built html fragment in a horizontally-scrollable div (replaces kableExtra::scroll_box
# for the jamovi results iframe; inline-styled so it needs no external CSS).
#' @keywords internal
tab_render_scrollbox <- function(html, width = "100%") {
  paste0('<div style="overflow-x:auto;width:', width,
         ';display:block;">', as.character(html), '</div>')
}
