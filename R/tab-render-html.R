# PURPOSE: The final-HTML render seam for tab_kable() + a dependency-free home-built <table> engine.
# ROLE: Phase 10e. tab_kable() = option resolution + tab_export_prep() + map(render_kable_html) + join.
#       render_kable_html() isolates the engine so the render-model (rd, meta) stays engine-agnostic.
# KEY CONSTRAINTS:
#   - engine = "kableExtra" is the DEFAULT and reproduces the pre-10e output BYTE-IDENTICALLY (the
#     legacy row_spec()/column_spec() pipeline, just carved out of tab_kable() and reading the prep's
#     derive-once roles instead of recomputing them). Locked by test-exports.R + the new kable HTML
#     snapshot (test-render-html.R).
#   - engine = "html" is the home-built renderer: inline styles on <td> (no per-cell <span>), so it is
#     self-contained (jamovi honours only inline CSS, issue #1529) and ~O(n_col+n_row) paste0 calls
#     (Phase 9 idiom: base masks, vectorised assembly, NO case_when/if_else over fmt). It reproduces the
#     kableExtra visual (content-identical -- same cell text, colours, tooltips), not byte-identical DOM.
#   - The two engines wire the SAME 10e features (spanning header, [min;max] total column, NA="") off
#     the shared render-model, so they cannot drift.
# See: dev/tabxplor_phase10_exporters.md Sec 10, CLAUDE.md Phase 10e.


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
render_kableExtra_engine <- function(rd, meta, subtext, caption, tooltips, popover,
                                     html_font, full_width, get_data, in_knitr, ...) {
  tabs  <- rd$tab
  theme <- meta$theme

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
  out <- tabs %>%
    dplyr::mutate(dplyr::across(
      where(is_fmt),
      ~ format(., html = TRUE, special_formatting = TRUE, na = "",
               .ref = ann_ref(rd$ann[[dplyr::cur_column()]])) %>%
        kableExtra::cell_spec(
          bold       = color_bold[[dplyr::cur_column()]],
          color      = color_font[[dplyr::cur_column()]],
          background = if (any_bg) color_back[[dplyr::cur_column()]] else NULL,
          tooltip = if (!popover & tooltips) {
            tab_kable_print_tooltip(., .ref = rd$ann[[dplyr::cur_column()]]$ref_cells)
          } else {NULL},
          popover = if (popover & tooltips) {
            tab_kable_print_tooltip(., popover = TRUE, .ref = rd$ann[[dplyr::cur_column()]]$ref_cells)
          } else {NULL}
        )
    ))

  if (get_data) return(out)

  alignement <- rd$roles$align

  out <- knitr::kable(out, escape = FALSE, format = "html", align = alignement,
                      caption = caption)

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
# per-column vectors then concatenated with do.call(paste0, .). Returns the BARE <table> string (the
# scoped <style> block is hoisted ONCE by tab_kable_join()).
#' @keywords internal
render_html_engine <- function(rd, meta, subtext, caption, tooltips, popover, get_data) {
  tab   <- rd$tab
  roles <- rd$roles
  ann   <- rd$ann
  nm    <- names(tab)
  n_row <- nrow(tab)
  n_col <- ncol(tab)
  tc    <- meta$theme_cols

  # (a) format every column once -> list of chr[n_row] (reuse .ref via the prep's ann)
  cells <- purrr::imap(tab, function(col, name) {
    if (is_fmt(col)) {
      format(col, html = TRUE, special_formatting = TRUE, na = "", .ref = ann_ref(ann[[name]]))
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
  td_html <- purrr::imap(cells, function(cell, name) {
    a  <- ann[[name]]
    cs <- rep("", n_row)
    if (!is.null(a)) {
      cs <- paste0("color:", a$font, ";")
      hasbg <- a$back != "none"
      cs[hasbg] <- paste0(cs[hasbg], "background-color:", a$back[hasbg], ";")
      cs[a$bold] <- paste0(cs[a$bold], "font-weight:bold;")
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
    paste0('<td style="', col_style[j], cs, '"', tip, '>', cell, '</td>')
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
  hdr_style <- paste0(
    "color:", tc$text, ";font-weight:bold;border-top:0;border-bottom:1px solid;",
    "font-size:90%;vertical-align:bottom;line-height:0.9;padding:3px;text-align:center;")
  # col_style first (its borders/nowrap/widths apply to the header too), hdr_style last so its
  # text-align:center wins over the column's data alignment (kableExtra centres all headers).
  head_cells <- paste0('<th style="', col_style, hdr_style, '">',
                       htmltools::htmlEscape(nm), '</th>')
  thead <- paste0('<tr>', paste0(head_cells, collapse = ""), '</tr>')

  cap <- if (!is.null(caption) && length(caption) && nzchar(caption)) {
    paste0('<caption>', htmltools::htmlEscape(caption), '</caption>')
  } else ""

  tfoot <- if (length(subtext) != 0) {
    paste0('<tfoot><tr><td colspan="', n_col, '">',
           paste0(subtext, collapse = "<br>"), '</td></tr></tfoot>')
  } else ""

  paste0(
    '<table class="tabxplor-tab tabxplor-', meta$theme, '">', cap,
    '<thead>', thead, '</thead>',
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
  paste0('<table class="tabxplor-tab tabxplor-light"><thead>', thead,
         '</thead><tbody>', body, '</tbody></table>')
}


# === SECTION: join + jamovi helpers ===============================================================

# The scoped, self-contained <style> block for the html engine (jamovi honours only inline/`<style>`
# CSS, issue #1529). Ports the still-relevant inst/tab.css rules off the kableExtra .lightable class.
#' @keywords internal
html_style_block <- function() {
  paste0(
    "<style>",
    ".tabxplor-tab{border-collapse:collapse;border-top:0;border-bottom:0;margin:0;}",
    ".tabxplor-tab caption{text-align:center;font-weight:bold;font-size:120%;}",
    ".tabxplor-tab tfoot{font-size:80%;text-align:left;}",
    ".tabxplor-tab tbody tr:hover{background:rgba(0,0,0,.045);}",
    ".tabxplor-tab.tabxplor-dark{background:#212121;color:#fff;}",
    "</style>"
  )
}

# Join the per-table render parts (single table => length-1 list). kableExtra: concatenate the
# knitr_kable objects (keeping the class). html: hoist ONE <style> block, stack the <table> fragments.
#' @keywords internal
tab_kable_join <- function(parts, engine, theme = "light") {
  if (length(parts) == 1L && engine == "kableExtra") return(parts[[1]])

  if (engine == "html") {
    body <- paste(unlist(parts), collapse = "\n<br>\n")
    out  <- paste0(html_style_block(), "\n", body)
    return(structure(out, format = "html", class = "knitr_kable"))
  }

  # kableExtra list: stack the rendered tables one-after-another.
  chr <- vapply(parts, as.character, character(1))
  structure(paste(chr, collapse = "\n<br>\n"), format = "html", class = "knitr_kable")
}

# Wrap a home-built html fragment in a horizontally-scrollable div (replaces kableExtra::scroll_box
# for the jamovi results iframe; inline-styled so it needs no external CSS).
#' @keywords internal
tab_render_scrollbox <- function(html, width = "100%") {
  paste0('<div style="overflow-x:auto;width:', width,
         ';display:block;">', as.character(html), '</div>')
}
