# PURPOSE: The final-HTML render seam for tab_kable() + a dependency-free home-built <table> engine.
# ROLE: Phase 10e. tab_kable() = option resolution + tab_export_prep() + map(render_kable_html) + join.
#       render_kable_html() isolates the engine so the render-model (rd, meta) stays engine-agnostic.
# KEY CONSTRAINTS:
#   - engine = "html" is the DEFAULT since Phase 14e (options("tabxplor.tab_kable_engine")): the
#     home-built renderer. Geometry and colour are role CLASSES resolved by tab_css() -- it emits NO
#     inline style at all -- assembled in ~O(n_col+n_row) paste0 calls (Phase 9 idiom: base masks,
#     vectorised assembly, NO case_when/if_else over fmt). It reproduces the kableExtra visual
#     (content-identical -- same cell text, colours, tooltips), not byte-identical DOM.
#   - engine = "kableExtra" is the LEGACY path and reproduces the pre-10e output BYTE-IDENTICALLY (the
#     row_spec()/column_spec() pipeline, just carved out of tab_kable() and reading the prep's
#     derive-once roles instead of recomputing them). It bakes its own theme, so it cannot do
#     theme = "auto", and its cell_spec() HTML is version-unstable -- which is why its tests assert
#     invariants and never snapshot bytes (test-render-html.R).
#   - Phase 13d: this engine is THEME-AGNOSTIC. Colour lives in classes (tx_slot_class) resolved by the
#     stylesheet tab_css() builds, which is what makes theme = "auto" possible: an inline `style` beats
#     every stylesheet rule short of `!important`, so inline hex could never follow a dark-mode toggle.
#     Do not reintroduce inline colour here. The <style> is hoisted ONCE by tab_kable_join(); it works
#     in jamovi (its results view injects our HTML through jQuery .html(), which applies <style> nodes,
#     and it has no sanitizer on that path -- what jamovi ignores is htmlDependency, not <style>).
#   - The two engines wire the SAME 10e features (spanning header, [min;max] total column, NA="") off
#     the shared render-model, so they cannot drift.
# See: dev/tabxplor_phase10_exporters.md Sec 10, CLAUDE.md Phase 10e + 13d, R/tab-css.R.


# === SECTION: tooltips =================================================================

# Phase 14b: ONE builder for the bootstrap tooltip/popover attribute string, shared by both engines --
# the kableExtra path hands it to cell_spec() pre-classed, the home-built path pastes it into the <td>.
# They used to construct it separately ("match kableExtra's attributes so the JS binds identically"),
# which had already drifted: the home-built popover omitted data-trigger, so it needed a CLICK where
# kableExtra's opened on HOVER. One builder, one placement, no drift.
#
# WARNING: kableExtra::spec_tooltip()/spec_popover() CANNOT emit this placement -- their match.arg()
# takes tokens from c("right","bottom","top","left","auto"), so "auto right" errors outright and
# c("auto", "right") silently yields a length-2 attribute that recycles into the title. The string is
# therefore built here and passed through the `ke_tooltip`/`ke_popover` class cell_spec() honours (it
# pastes such an object into the <span> verbatim, without re-calling spec_*()).
#' @keywords internal
tab_tooltip_attrs <- function(text, popover = FALSE, escape = FALSE) {
  esc <- if (escape) htmltools::htmlEscape(text, attribute = TRUE) else text
  # "auto right" is Bootstrap's auto token: prefer right, reorient to left when the tooltip would
  # overflow the viewport. It keeps today's look while fixing the last columns being unreachable in a
  # narrow Viewer pane -- and unlike a "last N columns" rule in R it is measured at render time, so it
  # also covers a horizontally scrolled table and a wide tooltip on a middle column.
  out <- if (popover) {
    paste0('data-toggle="popover" data-container="body" data-trigger="hover"',
           ' data-placement="auto right" title="" data-content="', esc, '"')
  } else {
    paste0('data-toggle="tooltip" data-container="body"',
           ' data-placement="auto right" title="', esc, '"')
  }
  class(out) <- if (popover) "ke_popover" else "ke_tooltip"
  out
}


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
# Escape a label, then put back the ONE tag the package itself injects: tab_wrap_text() wraps long
# header names on "<br>". Escaping the whole string rendered it literally ("Tele:<br>occasionnel");
# not escaping at all would pass a user's own "<" straight into the markup.
#' @keywords internal
html_escape_br <- function(x) {
  gsub("&lt;br&gt;", "<br>", htmltools::htmlEscape(x), fixed = TRUE)
}

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
          # Phase 14b: pre-built (tab_tooltip_attrs) so both engines share one placement; cell_spec()
          # passes a `ke_tooltip`/`ke_popover` through untouched.
          tooltip = if (!popover & tooltips) {
            tab_tooltip_attrs(tab_kable_print_tooltip(col, .ref = rd$ann[[colnm]]$ref_cells))
          } else {NULL},
          popover = if (popover & tooltips) {
            tab_tooltip_attrs(tab_kable_print_tooltip(col, .ref = rd$ann[[colnm]]$ref_cells),
                              popover = TRUE)
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

  # (b) column-CONSTANT CLASSES, one string per column.
  # Phase 14e: these were inline `style=` strings. Three reasons they are classes now:
  #   1. an inline style cannot be overridden by a user's CSS, so the "good default you can restyle"
  #      contract (what kableExtra gives) was impossible;
  #   2. it is half of the coloured-border fix. `border-right:1px solid` is a SHORTHAND -- it resets
  #      border-color to `currentColor`, i.e. the cell's own palette colour, so a +20% cell drew a
  #      blue border. Moving it into a class removed the INLINE precedence; the shorthand itself
  #      survived (and a class still out-specifies the border-color rule), so 14e recorded this as
  #      fixed while it was not. Phase 14j finished it: R/tab-css.R now uses longhands only;
  #   3. the markup shrinks (one short class vs a repeated style string per cell).
  # The names are the ROLE, not the styling, so the stylesheet stays the only place that decides looks.
  cls_col <- ifelse(roles$align == "r", "tx-r", "tx-l")
  add_cls <- function(v, i, k) { v[i] <- paste0(v[i], " ", k); v }
  cls_col <- add_cls(cls_col, roles$fmt_cols,    "tx-num")   # numbers: nowrap + the number font
  cls_col <- add_cls(cls_col, unique(c(roles$new_col_var, n_col)), "tx-br")
  cls_col <- add_cls(cls_col, roles$other_cols,  "tx-bl")
  cls_col <- add_cls(cls_col, roles$totcols,     "tx-bl tx-tot")
  cls_col <- add_cls(cls_col, roles$row_var_col, "tx-rv")

  # (c) per-column <td> vectors over rows (colours/bold/tooltips from the prep's ann)
  # Phase 13d: colour is emitted as a slot CLASS, never as inline hex -- an inline `style` beats every
  # stylesheet rule short of `!important`, so inline colour makes dark mode impossible. The class is a
  # pure function of the slot the engine already assigned (tx_slot_class), so cells and tab_css() cannot
  # disagree, and this renderer is THEME-AGNOSTIC: the theme lives only in the stylesheet.
  #   text_slot > 0        -> .p1-.p4 / .m1-.m4      bg_slot > 0 -> .o1-.o4 / .u1-.u4
  #   ref_alltot, slot 0   -> no class: `theme_cols$text` IS the table's colour, so it inherits
  #   otherwise            -> .g1 (column has a colour measure) / .g2 (it has none)
  td_html <- purrr::imap(cells, function(cell, name) {
    a   <- ann[[name]]
    cls <- rep("", n_row)      # the <td>'s classes: text slot / grey / bold
    bgc <- rep("", n_row)      # the background slot, which rides the PILL span (below), not the <td>
    if (!is.null(a)) {
      tsl <- if (length(a$text_slot) == n_row) a$text_slot else integer(n_row)
      bsl <- if (length(a$bg_slot)   == n_row) a$bg_slot   else integer(n_row)
      cls <- tx_slot_class("text", tsl)
      bgc <- tx_slot_class("bg",   bsl)
      grey <- !nzchar(cls) & !nzchar(bgc) & !a$ref_alltot
      cls[grey] <- if (isTRUE(a$has_color) || isTRUE(a$has_bgc)) "g1" else "g2"
      cls[a$bold] <- trimws(paste(cls[a$bold], "tx-b"))
    }
    bg <- nzchar(bgc)
    tip <- rep("", n_row)
    if (tooltips && is_fmt(tab[[name]])) {
      tp <- tab_kable_print_tooltip(tab[[name]],
                                    .ref = if (is.null(a)) NULL else a$ref_cells)
      nz <- !is.na(tp) & nzchar(tp)
      if (any(nz)) {
        # Phase 14b: the SAME builder the kableExtra engine uses, so the bootstrap JS binds identically
        # here, in jamovi and in standalone HTML (leading space: this is pasted straight after the
        # style attribute's closing quote).
        tip[nz] <- paste0(" ", tab_tooltip_attrs(tp[nz], popover = popover, escape = TRUE))
      }
    }
    j <- match(name, nm)
    # Phase 13c-ii: in a bold row/cell, bold only the PRIMARY field of a composite "{pct} (n={n})"
    # cell (the "(n=...)" stays plain). Cells are placed raw here, so esc = identity (byte-identical).
    bold_cell <- seq_len(n_row) %in% rd$bold_rows
    if (!is.null(a)) bold_cell <- bold_cell | a$bold
    cell_html <- html_cell_text(cell, attr(cell, "primary_nchar"), bold_cell, esc = identity)
    # Phase 14e: a background-coloured cell wraps its text in a PILL (an inline span, rounded, hugging
    # the text) instead of flooding the whole <td>. Full-cell fills read as a heavy blocky grid, and
    # they also swallow the row-hover highlight (a child's background always paints over its row's).
    # The colour class moves onto the span; the text class stays on the <td> so `.p*` still cascades.
    if (any(bg)) {
      cell_html[bg] <- paste0('<span class="tx-pill ', bgc[bg], '">', cell_html[bg], '</span>')
    }
    paste0('<td class="', trimws(paste(cls_col[j], cls)), '"', tip, '>', cell_html, '</td>')
  })

  # (c2) Phase 14i: the LABEL columns are re-emitted as ONE `rowspan` cell per block, so the row/tab
  # variable is named once instead of on every row. A continuation row contributes the empty string --
  # which is exactly what (d)'s column-wise paste0 needs, so the assembly is untouched.
  # The name column additionally gets `tx-vname` (vertical text), but only where the run is longer than
  # one row: a rotated single-row cell just makes that row tall, so it falls back to horizontal.
  # html_escape_br() (not the raw path (c) takes): a label cell carries no markup of ours EXCEPT the
  # `<br>` tab_wrap_text() may have injected, which is exactly what that helper preserves.
  for (cl in names(roles$label_cols)) {
    j    <- match(cl, nm)
    run  <- roles$label_runs[[cl]]
    if (is.null(run) || is.na(j)) next
    vert <- cl %in% names(roles$var_name_col) & run$span > 1L
    cls  <- paste(cls_col[j], "tx-lbl", ifelse(vert, "tx-vname", ""))
    td   <- paste0('<td class="', trimws(cls), '" rowspan="', run$span, '">',
                   html_escape_br(cells[[j]]), '</td>')
    td[!run$show] <- ""
    td_html[[j]]  <- td
  }

  # (d) rows: paste0 across the LIST of column vectors -> all n_row rows in one call
  row_inner <- do.call(paste0, td_html)

  # (e) per-row CLASSES (Phase 14e; was an inline style string -- see (b) for why)
  # `radd` appends, it is not a set union -- so a row must reach each class through exactly ONE call
  # (the last row is normally also a totblock_bottom, which used to emit `class="tx-bb tx-bb"`).
  # WARNING: tx-bb (1px) and tx-bb2 (2px) have identical CSS specificity, so a row carrying both is
  # decided by the stylesheet's source order, where tx-bb2 comes last and wins. That is intended (a
  # thicker rule closes a row_var block); it is load-bearing, and R/tab-css.R says so at the rules.
  rcls <- rep("", n_row)
  radd <- function(i, k) rcls[i] <<- paste0(rcls[i], " ", k)
  radd(rd$bold_rows,          "tx-b")
  radd(roles$totblock_top,    "tx-bt")
  # the table's last row always closes, so it folds into the bottom rule rather than repeating it
  radd(union(roles$totblock_bottom, n_row), "tx-bb")
  radd(roles$new_group,       "tx-bb2")     # a thicker rule between row_var blocks
  rcls <- trimws(rcls)
  # a row with no role gets a bare <tr>, not `<tr class="">`
  rtag <- ifelse(nzchar(rcls), paste0('<tr class="', rcls, '">'), '<tr>')
  body <- paste0(rtag, row_inner, '</tr>', collapse = "\n")

  # Phase 13c-iii: level headers use the suffix-stripped labels (the col_var name is written in the
  # spanning row above). The look is `.tabxplor-tab thead th` in the stylesheet.
  # WARNING: `cvh$clean` may legitimately contain `<br>` -- tab_wrap_text() (via tab_export_prep's
  # `wrap`) wraps long header names with it, so html-escaping the whole label printed a literal
  # "Telе:<br>occasionnel". kableExtra never hit this: it passes col.names through knitr::kable(escape
  # = FALSE). Escape, then restore the tag we ourselves injected -- so a `<` a USER put in a level name
  # is still escaped.
  head_cells <- paste0('<th class="', cls_col, '">', html_escape_br(cvh$clean), '</th>')
  thead <- paste0('<tr>', paste0(head_cells, collapse = ""), '</tr>')

  # Phase 13c-iii: the col_var spanning-name header row -- each variable name centred (colspan) over its
  # contiguous level columns; an empty cell over the row var / total / count columns.
  cvh_runs <- tab_header_runs(cvh$label)
  span_thead <- if (any(nzchar(cvh_runs$labels))) {
    span_cells <- paste0('<th class="tx-span" colspan="', cvh_runs$spans, '">',
                         ifelse(nzchar(cvh_runs$labels), html_escape_br(cvh_runs$labels), ""),
                         '</th>')
    paste0('<tr>', paste0(span_cells, collapse = ""), '</tr>')
  } else ""

  cap <- if (!is.null(caption) && length(caption) && nzchar(caption)) {
    paste0('<caption>', htmltools::htmlEscape(caption), '</caption>')
  } else ""

  # Phase 14j: the footnote goes in a `tx-foot` div, which is what stops it SIZING the table. Its cell
  # spans every column and its prose (subtext + the colour legend) is ~330 characters on one line, so
  # its max-content dwarfed the data's -- and a table is as wide as min(max-content, available), so it
  # took the whole pane and auto layout padded every column with the slack. That, not the min-widths
  # 14e wrote, was the compactness complaint. The div's `width:0` contributes 0 to max-content; its
  # `min-width:100%` fills the cell once the table is sized. See R/tab-css.R (.tx-foot).
  tfoot <- if (length(subtext) != 0) {
    paste0('<tfoot><tr><td colspan="', n_col, '"><div class="tx-foot">',
           paste0(subtext, collapse = "<br>"), '</div></td></tr></tfoot>')
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
    # Phase 14e: the `kableExtra` class is what routes an HTML table to the Viewer (print.kableExtra)
    # and knits it (knit_print.kableExtra). Without it this was a bare `knitr_kable`, whose print just
    # cat()s the markup to the console -- so the maintainer had to re-class it by hand to see a table.
    # We produce the same thing kableExtra does (an HTML fragment, `format = "html"`), so we claim the
    # class rather than duplicate its two methods. Both live in kableExtra, a Suggests: without it the
    # class is inert and printing falls back to knitr_kable's cat(), which is the old behaviour.
    return(structure(out, format = "html", class = c("kableExtra", "knitr_kable")))
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
