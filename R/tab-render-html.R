# PURPOSE: tab_html()'s render seam, and the dependency-free <table> engine it drives.
# ROLE: tab_html() = resolve options -> tab_export_prep() -> render one table per prep -> join.
#   render_kable_html() isolates the engine so the render model stays engine-agnostic. There is ONE
#   engine; `engine =` is accepted and ignored.
# KEY CONSTRAINTS:
#   - EVERY LOOK IS A CLASS, never an inline style. That is what makes theme = "auto" possible: an
#     inline `style` beats every stylesheet rule short of !important, so inline hex could never
#     follow a dark-mode toggle. R/tab-css.R owns what each class looks like; this file only names
#     them. Do not reintroduce inline colour.
#   - The <thead> is the prep's three rows (R/tab-export-prep.R). An INDEX column has no unit, so its
#     header spans both rows and sits bottom-aligned, putting "levels" on the line of the "<row%>"
#     beside it; Excel merges the same two.
#   - The <style> is hoisted ONCE by tab_kable_join(). It works inside jamovi: the results view
#     injects our html through jQuery .html(), which applies <style> nodes, and has no sanitizer on
#     that path (what jamovi ignores is htmlDependency, not <style>).
#   - The `knitr_kable` class on our output IS load-bearing: it is what routes the fragment to
#     knitr's own print / knit_print when tabxplor's do not apply. The Viewer page and the tooltip
#     binding are tabxplor's own, through tx_html_deps() -- jQuery and bootstrap from rmarkdown, the
#     binding from inst/tabxplor-1.0/tabxplor.js.
#   - WARNING: do not replace the tooltip string with a library call. kableExtra::spec_tooltip() /
#     spec_popover() cannot emit this placement -- their match.arg() takes single tokens, so
#     "auto right" errors and c("auto", "right") silently yields a length-2 attribute that recycles
#     into the title. The attribute is therefore built by hand below.
#   - A CELL'S RENDERING STOPS AT ITS PRIMARY TOKEN: the text colour, the background fill and, under
#     a publication palette, the face. html_cell_text() is the one place all three are known, so it
#     applies all three; the aside takes `tx-sec` and the stylesheet sets it back. A background is a
#     colour MEASURE, not the cell's ground -- flooding the asides and the stars with it would say
#     the measure grades those too.
#   - THE FACE IS <b>/<i>/<u> MARKUP, not only CSS: the destinations that matter for a publication
#     table (GitHub, a Word paste) strip class AND style but keep tags.
#   - WARNING: a publication palette's effect-size MARKS are not an aside. They sit where the stars
#     sit but REPLACE the colour, so they carry the deviation itself and take `tx-mark` (the chrome's
#     own ink), not the aside grey. format() hands back their character range as `mark_nchar`.
#   - The result is classed `tabxplor_kable` and carries a `tabxplor_theme` attribute ONLY when our
#     stylesheet ships with it. print.tabxplor_kable() is the one place a theme is resolved in R
#     rather than by the browser -- the Viewer's page is ours and its webview cannot see the editor's
#     theme. Everything else delegates to the reader through the tab_css() cascade.
#   - Assembly is ~O(n_col + n_row) paste0 over base masks: no case_when / if_else over fmt.
# See: CLAUDE.md section "tabxplor architecture" (exports and rendering); R/tab-css.R (the classes).


# === SECTION: tooltips =================================================================

tab_tooltip_attrs <- function(text, popover = FALSE, escape = FALSE) {
  esc <- if (escape) tx_html_escape(text, attribute = TRUE) else text
  # Bootstrap's own auto token: prefers right, reorients left only if it would overflow the viewport.
  out <- if (popover) {
    paste0('data-toggle="popover" data-container="body" data-trigger="hover"',
           ' data-placement="auto right" title="" data-content="', esc, '"')
  } else {
    paste0('data-toggle="tooltip" data-container="body"',
           ' data-placement="auto right" title="', esc, '"')
  }
  out
}

# The multinomial crude-companion tooltip fragment lives outside the fmt fields, so it is appended
# here rather than read by the shared per-column tooltip builder; a plain crosstab has none to add.
reg_append_empirical_tip <- function(tp, rd, col_name) {
  add <- rd$empirical_tips[[col_name]]
  if (is.null(add)) return(tp)
  ifelse(!is.na(add) & nzchar(tp), paste0(tp, " ; ", add),
         ifelse(!is.na(add), add, tp))
}


# === SECTION: the seam =================================================================

# `subtext` already has the colour legend prepended (tab_kable()); it is content, not styling. Called
# once per table -- the list method maps it over prep$tables and joins with tab_kable_join().
render_kable_html <- function(rd, meta,
                              subtext  = character(0),
                              caption  = NULL,
                              tooltips = TRUE, popover = FALSE,
                              get_data = FALSE) {
  # a table that merely lost its class keeps its fmt columns and is not degraded; only
  # tab_export_prep()'s own `degrade` flag (a non-tabxplor input) takes this path.
  if (isTRUE(rd$vars$degrade)) {
    if (isTRUE(rd$vars$notify)) tab_degrade_inform(rd$vars$reason)  # batch-aware (see tab_export_prep)
    return(render_html_degrade(rd$tab))
  }

  render_html_engine(rd, meta, subtext = subtext, caption = caption,
                     tooltips = tooltips, popover = popover, get_data = get_data)
}


# === SECTION: markup helpers (escaping, partial bold, typography) =================================

# escapes the whole label, then restores the ONE tag we ourselves inject (tab_wrap_text()'s "<br>") --
# a user's own "<" in the text stays escaped.
html_escape_br <- function(x) {
  gsub("&lt;br&gt;", "<br>", tx_html_escape(x), fixed = TRUE)
}

# DESIGN: the glyph run IS the data -- parsed straight out of the rendered string, so it survives
# transpose/tab_spread untouched. `inset` (half the stroke) keeps the extreme bins from clipping; the
# run's length is fixed so `dx` alone sets width, keeping every predictor's plot comparable.
tx_spark_svg <- function(x, h = 22L, dx = 5L, lwd = 2.4) {
  gl  <- rd_spark_glyphs()
  pat <- paste0("[", paste(gl, collapse = ""), "]{3,}")
  hit <- grepl(pat, x)
  if (!any(hit)) return(x)
  inset <- lwd / 2
  one <- function(run) {
    v <- match(strsplit(run, "")[[1L]], gl)
    n <- length(v)
    w <- (n - 1L) * dx + 2 * inset
    pts <- paste(sprintf("%.1f,%.1f", inset + (seq_len(n) - 1L) * dx,
                         h - inset - (v - 1) / 7 * (h - 2 * inset)), collapse = " ")
    paste0('<svg class="tx-spark" width="', round(w, 1), '" height="', h,
           '" viewBox="0 0 ', round(w, 1), ' ', h,
           '" aria-hidden="true"><polyline points="', pts,
           '" fill="none" stroke="currentColor" stroke-width="', lwd,
           '" stroke-linejoin="round" stroke-linecap="round"/></svg>')
  }
  x[hit] <- vapply(x[hit], function(s) {
    m <- gregexpr(pat, s)[[1L]]
    if (m[[1L]] == -1L) return(s)
    runs <- regmatches(s, gregexpr(pat, s))[[1L]]
    regmatches(s, gregexpr(pat, s)) <- list(vapply(runs, one, character(1), USE.NAMES = FALSE))
    s
  }, character(1), USE.NAMES = FALSE)
  x
}

# Splits a composite cell into its THREE pieces (aside / primary / aside), so a backend can bold and
# colour the number without touching what sits beside it. `face` and `pill` (the background slot)
# apply ONLY to the primary piece -- neither is what a measure grades outside it (fmt_class.R,
# paint_split). A cell with no recorded range, or that could not be split, returns whole via `pill_left`.
html_cell_text <- function(raw, from, pn, bold, esc = tx_html_escape, face = NULL,
                           pill = NULL, mk = NULL) {
  out  <- esc(raw)
  left <- if (is.null(pill)) rep(FALSE, length(raw)) else nzchar(pill)
  wrap_pill <- function(s, i) {
    if (is.null(pill)) return(s)
    ifelse(nzchar(pill[i]), paste0('<span class="tx-pill ', pill[i], '">', s, "</span>"), s)
  }
  wrap_face <- function(s, i)
    if (is.null(face)) s
    else html_face_wrap(s, bold[i], face$italic[i], face$underline[i])
  if (is.null(pn)) return(structure(wrap_face(out, seq_along(out)), pill_left = left))
  from <- if (is.null(from)) rep(1L, length(raw)) else from
  # `pn == 0` is a template with no token outside brackets: no primary at all, so the whole cell is
  # the aside. NA is the other thing entirely -- one plain piece.
  hit  <- !is.na(pn) & !is.na(from) & pn >= 0L & (from > 1L | pn < nchar(raw))
  if (any(hit)) {
    to  <- from[hit] + pn[hit] - 1L
    # an aside is never bold, kept as an inline style (like html_face_wrap()'s markup) so it survives
    # a stylesheet-less destination.
    wt  <- ifelse(bold[hit], " style=\"font-weight:normal;\"", "")
    wrap <- function(s) ifelse(nzchar(s),
                               paste0("<span class=\"tx-sec\"", wt, ">", esc(s), "</span>"), "")
    # `mk` is format()'s `mark_nchar` (NULL for stars); the run sits immediately AFTER the primary
    # range, not at the cell's end -- under "{est} ({base})" the aside follows the marks.
    mw   <- if (is.null(mk)) rep(0L, length(raw)) else ifelse(is.na(mk), 0L, mk)
    mrun <- ifelse(mw[hit] > 0L, substr(raw[hit], to + 1L, to + mw[hit]), "")
    out[hit] <- paste0(wrap(substr(raw[hit], 1L, from[hit] - 1L)),
                       wrap_pill(wrap_face(esc(substr(raw[hit], from[hit], to)), which(hit)),
                                 which(hit)),
                       ifelse(nzchar(mrun),
                              paste0("<span class=\"tx-mark\"", wt, ">", esc(mrun), "</span>"), ""),
                       wrap(substr(raw[hit], to + mw[hit] + 1L, nchar(raw[hit]))))
    left[hit] <- FALSE
  }
  if (any(!hit)) out[!hit] <- wrap_face(out[!hit], which(!hit))
  structure(out, pill_left = left)
}

# gated on the palette's own `semantic` flag, so the colour themes emit nothing and stay byte-identical.
html_face_wrap <- function(html, bold, italic, underline) {
  n <- length(html)
  # NULL is a real state (a degraded model carries no face flags); a wrong length means a producer
  # went out of step, so it is asserted rather than silently substituted.
  g <- function(v) {
    if (is.null(v)) return(logical(n))
    stopifnot(length(v) == n)
    # underline arrives as "" / "single" / "double"; there is no markup for a doubled rule, so both
    # ruled rungs emit <u> and the CSS class alone carries the doubling.
    if (is.character(v)) nzchar(v) else v %in% TRUE
  }
  bold <- g(bold); italic <- g(italic); underline <- g(underline)
  if (any(underline)) html[underline] <- paste0("<u>", html[underline], "</u>")
  if (any(italic))    html[italic]    <- paste0("<i>", html[italic],    "</i>")
  if (any(bold))      html[bold]      <- paste0("<b>", html[bold],      "</b>")
  html
}


# === SECTION: the home-built HTML engine =========================================================

# Returns the BARE <table> string; the <style> block is hoisted once by tab_kable_join(). Only a
# palette's TYPOGRAPHY reaches this markup, read from the RESOLVED theme, never from `meta$theme`.
render_html_engine <- function(rd, meta, subtext, caption, tooltips, popover, get_data) {
  # the model-fit block's first row draws a boundary across the whole table (2px), where a row_var
  # separator stops at the name column.
  foot_top <- if (length(rd$footer_rows)) min(rd$footer_rows) else NA_integer_
  if (!is.na(foot_top) && foot_top > nrow(rd$tab)) foot_top <- NA_integer_
  tab   <- rd$tab
  roles <- rd$roles
  ann   <- rd$ann
  semantic_face <- fmt_face_semantic(meta$theme_cols$theme %||% "light")
  nm    <- names(tab)
  cvh   <- rd$col_var_header       # spanning names + suffix-stripped level labels
  n_row <- nrow(tab)
  n_col <- ncol(tab)

  # (a) format every column once. A TRANSPOSED model's columns are heterogeneous and cannot be
  # format()ted, so it carries its own pre-formatted strings (tx_transpose_render() built them).
  cells <- if (isTRUE(rd$transposed)) {
    rd$cells
  } else purrr::imap(tab, function(col, name) {
    if (is_fmt(col)) {
      format(col, html = TRUE, special_formatting = TRUE, na = "", stars = TRUE,
             theme = meta$theme_cols$theme %||% "light",
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

  # (b) column-CONSTANT CLASSES, one string per column, never inline `style=`: `border-right:1px solid`
  # as a shorthand would reset border-color to the cell's own text colour (R/tab-css.R uses longhands
  # only for the same reason). Names are the ROLE, not the styling.
  cls_col <- ifelse(roles$align == "r", "tx-r", "tx-l")
  add_cls <- function(v, i, k) { v[i] <- paste0(v[i], " ", k); v }
  cls_col <- add_cls(cls_col, roles$fmt_cols,    "tx-num")   # numbers: nowrap + the number font
  cls_col <- add_cls(cls_col, unique(c(roles$new_col_var, n_col)), "tx-br")
  cls_col <- add_cls(cls_col, roles$other_cols,  "tx-bl")
  cls_col <- add_cls(cls_col, roles$totcols,     "tx-bl tx-tot")
  cls_col <- add_cls(cls_col, roles$row_var_col, "tx-rv")

  # (c) per-column <td> vectors. Colour is a slot CLASS (never inline hex, per the header) -- a pure
  # function of tx_slot_class, so cells and tab_css() cannot disagree:
  #   text_slot > 0      -> .p1-.p4 / .m1-.m4     bg_slot > 0 -> .o1-.o4 / .u1-.u4
  #   ref_alltot, slot 0 -> no class (inherits theme_cols$text)
  #   otherwise          -> .g1 (has a colour measure) / .g2 (none)
  # base-count columns already print the n on each row, so the tooltip does not repeat it.
  base_n <- if (tooltips && !isTRUE(rd$transposed)) tab_base_n_values(tab) else NULL

  td_html <- purrr::imap(cells, function(cell, name) {
    a   <- ann[[name]]
    cls <- rep("", n_row)      # the <td>'s classes: text slot / grey / bold
    bgc <- rep("", n_row)      # the background slot, which rides the PILL span (below), not the <td>
    if (!is.null(a)) {
      # a wrong length here means a producer went out of step -- assert rather than substitute.
      slot <- function(v) { if (is.null(v)) return(integer(n_row)); stopifnot(length(v) == n_row); v }
      tsl <- slot(a$text_slot)
      bsl <- slot(a$bg_slot)
      cls <- tx_slot_class("text", tsl)
      bgc <- tx_slot_class("bg",   bsl)
      # keep_black = ref_alltot | is_refrow | footer -- the cells kept as black reading anchors rather
      # than greyed; falls back to ref_alltot alone (a degraded-model guard) when absent.
      keep <- if (is.null(a$keep_black)) a$ref_alltot else { stopifnot(length(a$keep_black) == n_row)
                                                             a$keep_black }
      grey <- !nzchar(cls) & !nzchar(bgc) & !keep
      cls[grey] <- if (isTRUE(a$has_color) || isTRUE(a$has_bgc)) "g1" else "g2"
      cls[a$bold] <- trimws(paste(cls[a$bold], "tx-b"))
    }
    bg <- nzchar(bgc)
    tip <- rep("", n_row)
    # a transposed model's columns are heterogeneous, so tx_transpose_render() pre-built the tooltips.
    tp <- if (isTRUE(rd$transposed)) rd$tooltips[[name]]
          else if (tooltips && is_fmt(tab[[name]]))
            tab_tooltip_text(tab[[name]], .ref = if (is.null(a)) NULL else a$ref_cells,
                             .note = if (fmt_display_shows(get_display(tab[[name]]), "n_range")[[1]])
                               tab_base_notes(tab, name),
                             .base_n = base_n)
          else NULL
    if (tooltips && !is.null(tp) && !isTRUE(rd$transposed)) tp <- reg_append_empirical_tip(tp, rd, name)
    if (tooltips && !is.null(tp)) {
      nz <- !is.na(tp) & nzchar(tp)
      if (any(nz)) {
        # leading space: pasted straight after the style attribute's closing quote.
        tip[nz] <- paste0(" ", tab_tooltip_attrs(tp[nz], popover = popover, escape = TRUE))
      }
    }
    j <- match(name, nm)
    # `bold_cell`, not a$face_bold alone, so the structural reference/total bold travels too.
    bold_cell <- seq_len(n_row) %in% rd$bold_rows
    if (!is.null(a)) bold_cell <- bold_cell | a$bold
    face <- if (semantic_face && !color_whole_cell_opt() && !is.null(a))
      list(italic = a$face_italic, underline = a$face_underline)
    # an fmt cell is OURS and placed raw; a CHARACTER column is user data and must be escaped, or a
    # level like "Arts & Humanities" emits invalid html.
    esc_cell <- if (is_fmt(tab[[name]]) || isTRUE(rd$transposed)) identity else html_escape_br
    # the PILL rides the primary token unless the cell opted out of the split (color_whole_cell,
    # a degraded model with no `ann`) or has no recorded range -- those come back in `pill_left`.
    split_pill <- if (color_whole_cell_opt() || is.null(a)) NULL else bgc
    cell_html <- html_cell_text(cell, attr(cell, "primary_from"), attr(cell, "primary_nchar"),
                                bold_cell, esc = esc_cell, face = face, pill = split_pill,
                                mk = attr(cell, "mark_nchar"))
    bg_left   <- attr(cell_html, "pill_left") %||% bg
    cell_html <- as.character(cell_html)
    # this is the ONE place a row sparkline becomes an inline <svg> -- it must stay off the rowspanned
    # label path (c2), which escapes through html_escape_br() and would turn the markup back to text.
    sp <- tx_has_spark(cell_html)
    if (any(sp)) cls[sp] <- trimws(paste(cls[sp], "tx-sparkcell"))
    cell_html <- tx_spark_svg(cell_html)
    # color_whole_cell opts out of the split, and a degraded model (no `ann`) has no face flags.
    if (semantic_face && (color_whole_cell_opt() || is.null(a))) {
      cell_html <- html_face_wrap(cell_html, bold_cell,
                                  if (is.null(a)) NULL else a$face_italic,
                                  if (is.null(a)) NULL else a$face_underline)
    }
    # a full-cell fill reads as a heavy grid and swallows the row-hover highlight, so the text wraps
    # in a PILL instead. Only what html_cell_text() could NOT split reaches here.
    if (any(bg_left)) {
      cell_html[bg_left] <- paste0('<span class="tx-pill ', bgc[bg_left], '">',
                                   cell_html[bg_left], '</span>')
    }
    paste0('<td class="', trimws(paste(cls_col[j], cls)), '"', tip, '>', cell_html, '</td>')
  })

  # (c2) LABEL columns are re-emitted as ONE `rowspan` cell per block, so the row/tab variable is
  # named once; a continuation row contributes "", which is what (d)'s paste0 needs. html_escape_br(),
  # not the raw path (c): a label carries no markup of ours except tab_wrap_text()'s own "<br>".
  for (cl in names(roles$label_cols)) {
    j    <- match(cl, nm)
    run  <- roles$label_runs[[cl]]
    if (is.null(run) || is.na(j)) next
    named <- cl %in% names(roles$var_name_col)   # a variable name is bold in its own right
    # rotation is decided by the prep (tab_vname_plan), never re-derived -- Excel reads the same vector.
    vert  <- named & (roles$vname_plans[[cl]]$vert %||% (run$span > 1L))
    # the bottom rule is decided HERE: a rowspanned cell is anchored in its block's FIRST row, so
    # `tr.tx-bb2>*` never reaches it, unlike a one-row block's own closing row.
    bot <- rep("", n_row)
    if (any(run$show)) {
      last_i <- max(which(run$show))
      if (last_i + run$span[last_i] - 1L >= n_row) bot[last_i] <- "tx-bb"
      if (!is.na(foot_top) && foot_top <= n_row && isTRUE(run$show[[foot_top]]))
        bot[foot_top] <- "tx-bb2"
    }
    cls   <- gsub(" +", " ", paste(cls_col[j], "tx-lbl", ifelse(vert, "tx-vname", ""),
                                   ifelse(named, "tx-b", ""),
                                   ifelse(named & !nzchar(bot), "tx-nb", ""), bot))
    td   <- paste0('<td class="', trimws(cls), '" rowspan="', run$span, '">',
                   html_escape_br(cells[[j]]), '</td>')
    td[!run$show] <- ""
    td_html[[j]]  <- td
  }

  # (d) rows: paste0 across the LIST of column vectors -> all n_row rows in one call
  row_inner <- do.call(paste0, td_html)

  # (e) per-row CLASSES. `radd` appends, not a set union, so a row reaches each class through exactly
  # ONE call. WARNING: tx-bb and tx-bb2 share CSS specificity, so a row carrying both is decided by
  # source order -- tx-bb2 comes last in R/tab-css.R and wins; this is load-bearing.
  rcls <- rep("", n_row)
  radd <- function(i, k) rcls[i] <<- paste0(rcls[i], " ", k)
  radd(rd$bold_rows,          "tx-b")
  # the model-fit block opens at 2px, matching the row_var separator that closes the block above it
  radd(setdiff(roles$totblock_top, foot_top),   "tx-bt")
  radd(intersect(roles$totblock_top, foot_top), "tx-bt2")
  radd(union(roles$totblock_bottom, n_row), "tx-bb")  # the last row always closes
  radd(roles$new_group,       "tx-bb2")     # a thicker rule between row_var blocks
  rcls <- trimws(rcls)
  # a row with no role gets a bare <tr>, not `<tr class="">`
  rtag <- ifelse(nzchar(rcls), paste0('<tr class="', rcls, '">'), '<tr>')
  body <- paste0(rtag, row_inner, '</tr>', collapse = "\n")

  # level headers use the suffix-stripped labels; the col_var name is in the spanning row above.
  # WARNING: `cvh$clean` may legitimately contain `<br>` (tab_wrap_text() wraps long names) -- escape,
  # then restore only the tag we ourselves injected, so a user's own `<` stays escaped.
  # the UNIT ROW states what each column HOLDS, in the console type tag's own notation ("<row%>", "<n>")
  # -- `tx-unit` keeps it discreet so it reads as the header's second line, not a second header row.
  has_unit <- !is.null(cvh$unit) && any(nzchar(cvh$unit))
  span2 <- has_unit & seq_along(cls_col) %in% unname(roles$other_cols)
  head_cells <- paste0('<th class="', cls_col, '"', ifelse(span2, ' rowspan="2"', ''), '>',
                       html_escape_br(cvh$clean), '</th>')
  thead <- paste0('<tr>', paste0(head_cells, collapse = ""), '</tr>')

  unit_thead <- if (has_unit) {
    paste0('<tr>', paste0('<th class="', cls_col[!span2], ' tx-unit">',
                          tx_html_escape(cvh$unit[!span2]), '</th>', collapse = ""), '</tr>')
  } else ""

  # the col_var spanning-name header row: each variable name centred (colspan) over its level columns.
  # A span belonging to a SUB-POPULATION gets its own line above the variable name, composed here from
  # two stored facts rather than welded into the name.
  cvh_runs <- tab_header_runs(cvh$label, cvh$group)
  span_thead <- if (any(nzchar(cvh_runs$labels))) {
    span_txt <- ifelse(nzchar(cvh_runs$labels), html_escape_br(cvh_runs$labels), "")
    span_txt <- ifelse(nzchar(cvh_runs$groups) & nzchar(span_txt),
                       paste0(html_escape_br(cvh_runs$groups), "<br>", span_txt), span_txt)
    span_cells <- paste0('<th class="tx-span" colspan="', cvh_runs$spans, '">', span_txt,
                         '</th>')
    paste0('<tr>', paste0(span_cells, collapse = ""), '</tr>')
  } else ""

  # the title is a `<div>` sibling before the <table>, not a `<caption>` child -- a caption
  # participates in the table's width. See R/tab-css.R (.tabxplor-caption).
  cap <- if (!is.null(caption) && length(caption) && nzchar(caption)) {
    paste0('<div class="tabxplor-caption">', tx_html_escape(caption), '</div>')
  } else ""

  # the footnote sits in a `tx-foot` div so its long prose does not SIZE the table -- `width:0`
  # contributes nothing to max-content, `min-width:100%` fills once the table is sized (R/tab-css.R).
  tfoot <- if (length(subtext) != 0) {
    paste0('<tfoot><tr><td colspan="', n_col, '"><div class="tx-foot">',
           paste0(subtext, collapse = "<br>"), '</div></td></tr></tfoot>')
  } else ""

  # no `tabxplor-<theme>` token in the markup -- the stylesheet carries the theme. A table showing
  # significance stars gets `tx-has-stars`, flipping the number cells to the monospace stack in CSS.
  tbl_class <- if (isTRUE(roles$has_stars)) "tabxplor-tab tx-has-stars" else "tabxplor-tab"
  paste0(
    cap,
    '<table class="', tbl_class, '">',
    '<thead>', span_thead, thead, unit_thead, '</thead>',
    '<tbody>', body, '</tbody>',
    tfoot,
    '</table>'
  )
}


# The SHAPE TABLE as html: the same four columns the console prints, in the table's own chrome, with
# the glyph run upgraded to an <svg> at double size -- a table of its own has room a base-count cell
# has not.
shape_html_table <- function(tab) {
  st <- reg_shape_table(tab)
  if (is.null(st)) return(NULL)
  hd <- attr(st, "headers"); al <- attr(st, "align")
  cls <- vapply(al, function(a) if (a == "right") "tx-r tx-num" else "tx-l", character(1))
  thead <- paste0('<tr>', paste0('<th class="', cls, '">', tx_html_escape(hd), '</th>',
                                 collapse = ""), '</tr>')
  # a curve inside its own sampling noise wears the ASIDE ink -- same convention as a non-significant
  # cell. WARNING: the grey goes on a SPAN inside the cell, never the <td>: tab_css() gives `.tx-sec`
  # `display:inline-block` under every publication palette, which on a <td> would break the row layout.
  ns <- attr(st, "noisy") %||% rep(FALSE, nrow(st))
  cells <- lapply(seq_along(st), function(j) {
    v <- tx_html_escape(as.character(st[[j]]))
    k <- cls[[j]]
    if (names(st)[[j]] == "shape") { v <- tx_spark_svg(v, h = 44L, dx = 10L, lwd = 2.6)
                                     k <- paste(k, "tx-sparkcell") }
    v <- ifelse(ns, paste0('<span class="tx-sec">', v, '</span>'), v)
    paste0('<td class="', k, '">', v, '</td>')
  })
  body <- paste0('<tr>', do.call(paste0, cells), '</tr>', collapse = "")
  nt    <- attr(st, "note")                        # empty wherever no row wears the "ns" mark
  tfoot <- if (!length(nt)) "" else
    paste0('<tfoot><tr><td colspan="', length(st), '"><div class="tx-foot">',
           paste(tx_html_escape(nt), collapse = "<br>"), '</div></td></tr></tfoot>')
  paste0('<table class="tabxplor-tab">', '<thead>', thead, '</thead>',
         '<tbody>', body, '</tbody>', tfoot, '</table>')
}


# Minimal escaped <table> for the graceful-degrade path (plain data.frame / no fmt columns).
render_html_degrade <- function(tab) {
  tab <- tibble::as_tibble(tab)
  nm  <- names(tab)
  head_cells <- paste0('<th>', tx_html_escape(nm), '</th>')
  thead <- paste0('<tr>', paste0(head_cells, collapse = ""), '</tr>')
  cols <- lapply(tab, function(col) paste0('<td>', tx_html_escape(as.character(col)), '</td>'))
  row_inner <- if (length(cols)) do.call(paste0, cols) else rep("", nrow(tab))
  body <- paste0('<tr>', row_inner, '</tr>', collapse = "\n")
  paste0('<table class="tabxplor-tab"><thead>', thead,
         '</thead><tbody>', body, '</tbody></table>')
}


# === SECTION: join ================================================================================

# Joins the per-table render parts: hoists ONE <style> block and stacks the <table> fragments.
# `theme` is the render INTENT, carried to print.tabxplor_kable() so the Viewer page can match it.
tab_kable_join <- function(parts, css = "", theme = NULL) {
  body <- paste(unlist(parts), collapse = "\n<br>\n")
  out  <- if (nzchar(css)) paste0("<style>", css, "</style>\n", body) else body
  out <- structure(out, format = "html", class = c("tabxplor_kable", "knitr_kable"))
  # tabxplor paints a Viewer page only when its OWN stylesheet ships (css != ""): otherwise there is
  # no document to paint against, and the Viewer's dark chrome around an unstyled table is unreadable.
  if (nzchar(css)) attr(out, "tabxplor_theme") <- theme
  out
}


# === SECTION: the html dependencies ===============================================================

# THE ONE PRODUCER of the browser-side assets, read by the Viewer print and by the knit path.
#
# tabxplor writes the tooltip ATTRIBUTES itself (html_cell_text(), above); these three bind them:
# jQuery and bootstrap come from rmarkdown, which every Rmd user already has, and the 10-line binding
# is ours (inst/tabxplor-1.0/). lightable.css is deliberately NOT reproduced -- tabxplor ships its own
# stylesheet through tab_css() and uses none of kableExtra's themes.
#
# ⚠ NULL when rmarkdown or htmltools is absent: BOTH are Suggests, and the caller degrades. Nothing
# breaks when it does -- the `title=` attribute is a native browser tooltip on its own, so a table
# without these still hovers, just unstyled. Popovers are the part that genuinely needs the JS.
#' @keywords internal
#' @noRd
tx_html_deps <- function() {
  if (!requireNamespace("rmarkdown", quietly = TRUE) ||
      !requireNamespace("htmltools", quietly = TRUE)) return(NULL)
  src <- system.file("tabxplor-1.0", package = "tabxplor")
  if (!nzchar(src)) return(NULL)
  list(
    rmarkdown::html_dependency_jquery(),
    rmarkdown::html_dependency_bootstrap(theme = "cosmo"),
    htmltools::htmlDependency("tabxplor", utils::packageVersion("tabxplor"),
                              src = src, script = "tabxplor.js")
  )
}


# === SECTION: the Viewer page ======================================================================

# `detected` is the impure theme probe as a DEFAULT ARGUMENT, forced only in the "auto" branch, so a
# test can drive every path with no mocking.
#
# WHY resolved here in R, not left to the CSS cascade: the Viewer is an Electron webview, where
# `@media (prefers-color-scheme)` reports the OPERATING SYSTEM, not the editor's theme -- only R can
# see the editor. A file or knitted document keeps the cascade untouched, since there the READER
# decides. The resolution is expressed as a `data-theme` wrapper (tx_dark_hooks/tx_light_hooks beat
# @media in both directions), emitted only under "auto" -- an explicit theme's stylesheet carries no
# hook rule, so the wrapper would be inert.
tx_kable_page <- function(html, theme = "light", detected = tx_detect_theme()) {
  auto     <- identical(theme, "auto")
  resolved <- if (auto) detected else tx_palette_theme(theme)
  paste0(
    "<style>", tx_page_style(resolved), "</style>\n",
    if (auto) paste0('<div data-theme="', resolved, '">'),
    as.character(html),
    if (auto) "</div>"
  )
}

# Pure predicate (all inputs passed in) so it is testable -- testthat is never interactive(), and the
# branches below are otherwise unreachable. Returns:
#   "next"    : fall through to knitr's own print
#   "degrade" : an interactive themed print wants the Viewer, but the deps are absent -> note + print
#   "viewer"  : paint the themed Viewer page and bind the tooltips
#' @noRd
kable_print_mode <- function(theme, interactive, view_opt, knitting, have_deps) {
  if (is.null(theme) || !interactive || !isTRUE(view_opt) || knitting) return("next")
  if (!have_deps) return("degrade")
  "viewer"
}

#' Print a tabxplor html table
#'
#' Opens the html table \code{\link{tab_kable}} returned in the Viewer, on a page painted to match it
#' -- so a \code{theme = "dark"} table no longer sits in a white pane. Under \code{theme = "auto"} the
#' theme is resolved from **your editor** rather than your operating system: the Viewer is a webview,
#' and its \code{prefers-color-scheme} reports the OS, so it cannot see the editor the table is sitting
#' in. Anything else -- a non-interactive print, a knitted document, or a table tabxplor did not style
#' (\code{css = FALSE}) -- prints the markup exactly as \pkg{knitr} does.
#'
#' The Viewer page carries jQuery and bootstrap (from \pkg{rmarkdown}) plus tabxplor's own binding
#' script, which is what turns the cells' \code{title=} attributes into styled tooltips and makes
#' \code{popover = TRUE} work. Without \pkg{rmarkdown} and \pkg{htmltools} the table still prints,
#' and the tooltips fall back to the browser's own plain ones.
#'
#' @param x A html table returned by \code{\link{tab_kable}}.
#' @param ... Passed to the next print method.
#' @return \code{x}, invisibly.
#' @seealso \code{\link{tab_kable}}, \code{\link{tab_css}}
#' @export
#' @keywords internal
print.tabxplor_kable <- function(x, ...) {
  theme <- attr(x, "tabxplor_theme")
  # everything but an interactive Viewer print falls through to the next method, byte for byte:
  #   - no theme      : we did not ship the stylesheet, so the page is not ours to paint
  #   - !interactive(): there is no page (the only branch the test suite exercises)
  #   - knitting      : the page belongs to the DOCUMENT; knit_print.tabxplor_kable carries the deps
  #   - degrade       : rmarkdown / htmltools are absent, so there is no page to build -- a note,
  #                     then the same fall-through (the browser's own title= tooltips still work)
  deps <- tx_html_deps()
  mode <- kable_print_mode(theme, interactive(),
                           # `kableExtra_view_html` is honoured as the former spelling of the opt-out
                           getOption("tabxplor.view_html", getOption("kableExtra_view_html", TRUE)),
                           !is.null(tx_knitr_opt("out.format", "knit")),
                           !is.null(deps))
  if (identical(mode, "degrade"))
    tx_need_pkg(c("rmarkdown", "htmltools"), "A themed Viewer page with styled tooltips",
                severity = "inform")
  # WARNING: load knitr before falling through -- an S3 method exists only once its own package is
  # loaded, so an unguarded NextMethod() would reach print.default() instead of knitr's.
  if (identical(mode, "next") || identical(mode, "degrade")) {
    if (requireNamespace("knitr", quietly = TRUE)) return(NextMethod())
    cat(as.character(x), sep = "\n")
    return(invisible(x))
  }
  # The Viewer page, with its assets: htmltools' own print method for a shiny.tag.list is what
  # writes the dependencies out and opens the pane.
  page <- htmltools::browsable(htmltools::HTML(tx_kable_page(as.character(x), theme)))
  htmltools::htmlDependencies(page) <- deps
  class(page) <- "shiny.tag.list"
  print(page, ...)
  invisible(x)
}

