# PURPOSE: The final-HTML render seam for tab_html() + the dependency-free <table> engine it drives.
# ROLE: Phase 10e. tab_html() = option resolution + tab_export_prep() + map(render_kable_html) + join.
#       render_kable_html() isolates the engine so the render-model (rd, meta) stays engine-agnostic.
# KEY CONSTRAINTS:
#   - ONE engine since Phase 19l: the home-built renderer. Geometry and colour are role CLASSES
#     resolved by tab_css() -- it emits NO inline style at all -- assembled in ~O(n_col+n_row) paste0
#     calls (Phase 9 idiom: base masks, vectorised assembly, NO case_when/if_else over fmt).
#     The legacy `engine = "kableExtra"` (its row_spec()/column_spec() pipeline, the pre-10e output)
#     is DELETED: it baked its own theme, so it could not do theme = "auto", it could not render a
#     transposed model at all, and by 19l it was reachable only by naming it. `engine =` is accepted
#     and ignored for one release (tx_deprecate_inert, R/utils.R).
#   - Phase 13d: this engine is THEME-AGNOSTIC. Colour lives in classes (tx_slot_class) resolved by the
#     stylesheet tab_css() builds, which is what makes theme = "auto" possible: an inline `style` beats
#     every stylesheet rule short of `!important`, so inline hex could never follow a dark-mode toggle.
#     Do not reintroduce inline colour here. The <style> is hoisted ONCE by tab_kable_join(); it works
#     in jamovi (its results view injects our HTML through jQuery .html(), which applies <style> nodes,
#     and it has no sanitizer on that path -- what jamovi ignores is htmlDependency, not <style>).
#   - WARNING: \pkg{kableExtra} is still a Suggests, and the CLASS on our output is load-bearing --
#     tab_kable_join() stamps `kableExtra` so print.kableExtra()/knit_print.kableExtra() route the
#     fragment to the Viewer and bind the bootstrap tooltips. We produce what they expect (an HTML
#     fragment); do not "clean up" that class because the engine is gone.
#   - Phase 14k: the html result is classed `tabxplor_kable` and carries the render intent in a
#     `tabxplor_theme` attribute -- but ONLY when our stylesheet ships with it (tab_kable_join()).
#     print.tabxplor_kable() is the one place a theme is resolved in R rather than by the browser: the
#     Viewer's page is OURS, and its webview cannot see the editor's theme. Everything else (a file, a
#     knitted document) still delegates to the reader via the tab_css() cascade.
# See: dev/tabxplor_phase10_exporters.md Sec 10, CLAUDE.md Phase 10e + 13d + 14k, R/tab-css.R.


# === SECTION: tooltips =================================================================

# Phase 14b: ONE builder for the bootstrap tooltip/popover attribute string. It was shared by the two
# engines, which had built it separately ("match kableExtra's attributes so the JS binds identically")
# and already drifted: the home-built popover omitted data-trigger, so it needed a CLICK where
# kableExtra's opened on HOVER.
#
# WARNING: do NOT replace this with a library call. kableExtra::spec_tooltip()/spec_popover() CANNOT
# emit this placement -- their match.arg() takes tokens from c("right","bottom","top","left","auto"),
# so "auto right" errors outright and c("auto", "right") silently yields a length-2 attribute that
# recycles into the title. (19l dropped the `ke_tooltip`/`ke_popover` class the string used to carry:
# it existed so cell_spec() would paste the object verbatim instead of re-calling spec_*(), and
# cell_spec() went with the engine. The tooltips are still BOUND by kableExtra's print method -- see
# the file header -- but it never inspects them.)
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
  out
}

# Phase 14v: append the multinomial crude-companion tooltip fragment to a per-column tooltip vector `tp`
# (from tab_kable_print_tooltip). The values are NOT in an fmt field -- so the shared per-column tooltip
# builder never sees them and tab()/other reg tooltips are untouched. prep_one_table() resolved them to
# rd$empirical_tips[[col_name]] (a per-row char vector, or NULL); a plain crosstab has none -> `tp`
# returned unchanged.
reg_append_empirical_tip <- function(tp, rd, col_name) {
  add <- rd$empirical_tips[[col_name]]
  if (is.null(add)) return(tp)
  ifelse(!is.na(add) & nzchar(tp), paste0(tp, " ; ", add),
         ifelse(!is.na(add), add, tp))
}


# === SECTION: the seam =================================================================

# Render ONE prepared table (rd = prep$tables[[i]], meta = prep$meta) to HTML. `subtext` already has
# the colour legend prepended by tab_kable() (it is content, not styling). Called once per table; the
# list method maps it over prep$tables and joins with tab_kable_join().
#' @keywords internal
render_kable_html <- function(rd, meta,
                              subtext  = character(0),
                              caption  = NULL,
                              tooltips = TRUE, popover = FALSE,
                              get_data = FALSE) {
  # per-table graceful degrade (mirrors md_render_one(): a list may hold a malformed table).
  # It is also the whole plain-data.frame path: tab_export_prep() flags any non-tabxplor input
  # `degrade`, so tab_html() / tab_export("html") answer a bare tibble with a bare <table> rather
  # than an error. A table that merely LOST its class keeps its fmt columns and is not degraded --
  # it renders fully coloured (test-degraded-attrs.R).
  if (isTRUE(rd$vars$degrade)) {
    if (isTRUE(rd$vars$notify)) tab_degrade_inform(rd$vars$reason)  # batch-aware (see tab_export_prep)
    return(render_html_degrade(rd$tab))
  }

  render_html_engine(rd, meta, subtext = subtext, caption = caption,
                     tooltips = tooltips, popover = popover, get_data = get_data)
}


# === SECTION: markup helpers (escaping, partial bold, typography) =================================

#' @keywords internal
# Escape a label, then put back the ONE tag the package itself injects: tab_wrap_text() wraps long
# header names on "<br>". Escaping the whole string rendered it literally ("Tele:<br>occasionnel");
# not escaping at all would pass a user's own "<" straight into the markup.
#' @keywords internal
html_escape_br <- function(x) {
  gsub("&lt;br&gt;", "<br>", htmltools::htmlEscape(x), fixed = TRUE)
}

# Phase 18z15 -- upgrade a block-glyph sparkline to an inline <svg> polyline (121 bytes against
# 1084 for svglite and 843 for a base64 PNG). ONE place decides what markup of ours survives escaping,
# which is why it lives inside html_escape_br() rather than beside the label builder.
#
# DESIGN: the GLYPH RUN IS THE DATA. Each of U+2581..U+2588 is one of eight levels, so the polyline is
# read straight out of the rendered string -- no lookup into meta$assumptions, no key to keep in sync,
# and it therefore survives transpose, tab_spread and any dplyr pipeline that moved the label.
# `stroke="currentColor"` makes z11's light / dark / print themes carry it with no new colour rule.
#' @keywords internal
tx_spark_svg <- function(x, h = 12L, dx = 3L) {
  gl  <- rd_spark_glyphs(TRUE)
  pat <- paste0("[", paste(gl, collapse = ""), "]{3,}")
  hit <- grepl(pat, x)
  if (!any(hit)) return(x)
  one <- function(run) {
    v <- match(strsplit(run, "")[[1L]], gl)
    n <- length(v)
    pts <- paste(sprintf("%d,%.1f", (seq_len(n) - 1L) * dx, h - (v - 1) / 7 * (h - 2) - 1),
                 collapse = " ")
    paste0('<svg class="tx-spark" width="', (n - 1L) * dx + 2L, '" height="', h,
           '" viewBox="0 0 ', (n - 1L) * dx + 2L, ' ', h,
           '" style="vertical-align:-2px" aria-hidden="true"><polyline points="', pts,
           '" fill="none" stroke="currentColor" stroke-width="1.2"/></svg>')
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

# Phase 13c-ii: build one HTML cell string, keeping only the PRIMARY field of a composite bold cell
# bold. A bold composite cell (`pn` = primary-field width) gets its escaped suffix wrapped in a
# normal-weight <span> so the "(n=...)" part overrides the inherited row/cell bold; each part is
# escaped separately so the offset can't drift. Non-composite / non-bold cells are just escaped.
# `esc`: the HTML-escape fn -- identity for the engine, which places cells raw and has already
# escaped them; a caller that has not may pass htmltools::htmlEscape.
#' @keywords internal
# A composite cell in THREE pieces -- the aside before the primary, the primary, the aside after --
# so a backend can bold and colour the number without touching what sits beside it. The asides carry
# a CLASS, never an inline colour (the "no inline colour" invariant): the stylesheet decides, from
# the theme's own chrome. A cell with no recorded range is one piece and gets no markup at all.
#' @keywords internal
html_cell_text <- function(raw, from, pn, bold, esc = htmltools::htmlEscape) {
  out <- esc(raw)
  if (is.null(pn)) return(out)
  from <- if (is.null(from)) rep(1L, length(raw)) else from
  hit  <- !is.na(pn) & !is.na(from) & pn >= 1L & (from > 1L | pn < nchar(raw))
  if (any(hit)) {
    to  <- from[hit] + pn[hit] - 1L
    # the primary keeps the cell's own weight; an aside is never bold (bold glyphs are wider, and
    # emphasising an aside defeats the split). That one stays an INLINE style, like html_face_wrap()'s
    # markup, so it survives a stylesheet-less destination -- the colour never does.
    wt  <- ifelse(bold[hit], " style=\"font-weight:normal;\"", "")
    wrap <- function(s) ifelse(nzchar(s),
                               paste0("<span class=\"tx-sec\"", wt, ">", esc(s), "</span>"), "")
    out[hit] <- paste0(wrap(substr(raw[hit], 1L, from[hit] - 1L)),
                       esc(substr(raw[hit], from[hit], to)),
                       wrap(substr(raw[hit], to + 1L, nchar(raw[hit]))))
  }
  out
}

# Wrap a cell's html in the palette's TYPOGRAPHY as real MARKUP, Phase 18z11.
# WHY, when the stylesheet already says the same thing: the two destinations that matter for a
# publication table carry tags and nothing else. GitHub's markdown sanitizer strips `class` AND `style`
# from raw html (which is why README tables are colourless there), and an HTML -> Word paste keeps
# character formatting but no stylesheet. tabxplor has no .docx writer, so the paste IS the Word route.
# `<b>`/`<i>`/`<u>` are not inline STYLES, so the "no inline colour" invariant (test-render-html.R) and
# the "restyle with ordinary CSS" contract both stand -- a user can still unbold with
# `.tabxplor-tab b {font-weight:normal}`.
# Gated on the palette's own `semantic` flag, so the colour themes emit nothing and stay byte-identical.
#' @keywords internal
html_face_wrap <- function(html, bold, italic, underline) {
  n <- length(html)
  # Phase 19m-i: ABSENT is a real state (a degraded model carries no face flags); SHORT is a bug.
  # The three flags are built per column as logical(length(col)) by fmt_col_ann(), so a wrong length
  # can only mean a producer went out of step -- and degrading silently is how D1's grey footer hid
  # for two phases. Assert the length, keep the NULL fallback.
  g <- function(v) {
    if (is.null(v)) return(logical(n))
    stopifnot(length(v) == n)
    v %in% TRUE
  }
  bold <- g(bold); italic <- g(italic); underline <- g(underline)
  # Innermost first, so the nesting reads <b><i><u>x</u></i></b>.
  if (any(underline)) html[underline] <- paste0("<u>", html[underline], "</u>")
  if (any(italic))    html[italic]    <- paste0("<i>", html[italic],    "</i>")
  if (any(bold))      html[bold]      <- paste0("<b>", html[bold],      "</b>")
  html
}


# === SECTION: the home-built HTML engine =========================================================

# Vectorised, dependency-free <table>: one style string per column, one per row, cells built as
# per-column vectors then concatenated with do.call(paste0, .). Returns the BARE <table> string; the
# <style> block (which carries the whole theme -- see the file header) is hoisted ONCE by
# tab_kable_join(). COLOUR never reaches the markup here; the one thing that does is a palette's
# TYPOGRAPHY, and only when the palette declares it must survive without a stylesheet (z11's
# `semantic` flag -- see html_face_wrap). That reads the RESOLVED palette theme, never `meta$theme`.
#' @keywords internal
render_html_engine <- function(rd, meta, subtext, caption, tooltips, popover, get_data) {
  tab   <- rd$tab
  roles <- rd$roles
  ann   <- rd$ann
  semantic_face <- fmt_face_semantic(meta$theme_cols$theme %||% "light")
  nm    <- names(tab)
  cvh   <- rd$col_var_header       # Phase 13c-iii: spanning names + suffix-stripped level labels
  n_row <- nrow(tab)
  n_col <- ncol(tab)

  # (a) format every column once -> list of chr[n_row] (reuse .ref via the prep's ann). bold_split =
  # TRUE marks the composite primary-field width (Phase 13c-ii) so step (c) can bold only the primary.
  # Phase 14o: a TRANSPOSED model carries pre-formatted strings (its columns are heterogeneous and
  # cannot be format()ted); use them directly (tx_transpose_render() built them per source column).
  cells <- if (isTRUE(rd$transposed)) {
    rd$cells
  } else purrr::imap(tab, function(col, name) {
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
      # Phase 19m-i: every `ann` field is built per column, length(col) BY CONSTRUCTION -- both by
      # fmt_col_ann() and by the transpose's rebuild. So ABSENT is a real state (a degraded model),
      # SHORT is a producer bug: assert it rather than silently substituting a neutral. That silent
      # substitution is precisely what hid D1's grey footer for two phases (see keep_black below).
      slot <- function(v) { if (is.null(v)) return(integer(n_row)); stopifnot(length(v) == n_row); v }
      tsl <- slot(a$text_slot)
      bsl <- slot(a$bg_slot)
      cls <- tx_slot_class("text", tsl)
      bgc <- tx_slot_class("bg",   bsl)
      # Phase 14q: keep_black = ref_alltot | is_refrow | footer (the black reading anchors), so reg
      # reference cells and GOF footer cells are no longer greyed. == ref_alltot for a crosstab.
      # Phase 19h (D1): the ref_alltot fallback stays as a degraded-model guard, but it is no longer
      # LOAD-BEARING -- the transpose flips keep_black like every other per-cell logical now. It used
      # to drop it, and the silent fallback is what turned that into "a transposed reg footer renders
      # grey"; 19m-i made the SHORT case an error, so only a genuinely absent field falls back.
      keep <- if (is.null(a$keep_black)) a$ref_alltot else { stopifnot(length(a$keep_black) == n_row)
                                                             a$keep_black }
      grey <- !nzchar(cls) & !nzchar(bgc) & !keep
      cls[grey] <- if (isTRUE(a$has_color) || isTRUE(a$has_bgc)) "g1" else "g2"
      cls[a$bold] <- trimws(paste(cls[a$bold], "tx-b"))
    }
    bg <- nzchar(bgc)
    tip <- rep("", n_row)
    # Phase 14o: a transposed model's columns are heterogeneous -- the per-column tooltip builder cannot
    # run, so tx_transpose_render() pre-built and flipped the tooltips (kable backend only).
    tp <- if (isTRUE(rd$transposed)) rd$tooltips[[name]]
          else if (tooltips && is_fmt(tab[[name]]))
            tab_kable_print_tooltip(tab[[name]], .ref = if (is.null(a)) NULL else a$ref_cells,
                                    .note = if (fmt_display_shows(get_display(tab[[name]]), "n_range")[[1]])
                                      tab_base_notes(tab, name))
          else NULL
    # Phase 14v: append the multinomial crude-companion fragment (no-op on a crosstab / non-reg table)
    if (tooltips && !is.null(tp) && !isTRUE(rd$transposed)) tp <- reg_append_empirical_tip(tp, rd, name)
    if (tooltips && !is.null(tp)) {
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
    cell_html <- html_cell_text(cell, attr(cell, "primary_from"), attr(cell, "primary_nchar"),
                                bold_cell, esc = identity)
    # Phase 18z15: THE one place a row sparkline becomes an inline <svg>. It sits here, at the
    # emission of an ordinary text cell, because that is the only kind of cell a glyph run can be in
    # (a reg table's `levels` column) -- an fmt cell never carries one, and a rowspanned label cell
    # (path c2) goes through html_escape_br(), which would escape the markup back into text.
    cell_html <- tx_spark_svg(cell_html)
    # z11: a palette whose meaning is TYPOGRAPHY writes it as markup too, so it survives a stylesheet-
    # less destination (GitHub, a Word paste). `bold_cell` rather than a$face_bold, so the structural
    # reference/total bold travels as well. No-op under the colour palettes (semantic_face = FALSE).
    if (semantic_face) {
      cell_html <- html_face_wrap(cell_html, bold_cell,
                                  if (is.null(a)) NULL else a$face_italic,
                                  if (is.null(a)) NULL else a$face_underline)
    }
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
    # Phase 18r: a rowspanned label cell is anchored in its block's FIRST row, so the per-row
    # `tr.tx-bb>*` bottom rule never reaches the one covering the table's LAST row -> open bottom-left
    # corner. Tag that single cell `tx-bb` (the cell-scoped 1px rule in R/tab-css.R) to close it.
    if (any(run$show)) {
      last_i <- max(which(run$show))
      if (last_i + run$span[last_i] - 1L >= n_row) cls[last_i] <- paste(cls[last_i], "tx-bb")
    }
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
  # Phase 19n: a span belonging to a SUB-POPULATION (a spread level, a split-model group) puts it on
  # its own line above the variable. The `<br>` is COMPOSED here, from two stored facts, instead of
  # arriving welded into the name -- which is why the escape below is now unambiguous: every `<br>`
  # in `cvh$label` is one tab_wrap_text() injected.
  cvh_runs <- tab_header_runs(cvh$label, cvh$group)
  span_thead <- if (any(nzchar(cvh_runs$labels))) {
    span_txt <- ifelse(nzchar(cvh_runs$labels), html_escape_br(cvh_runs$labels), "")
    span_txt <- ifelse(nzchar(cvh_runs$groups) & nzchar(span_txt),
                       paste0(html_escape_br(cvh_runs$groups), "<br>", span_txt), span_txt)
    span_cells <- paste0('<th class="tx-span" colspan="', cvh_runs$spans, '">', span_txt,
                         '</th>')
    paste0('<tr>', paste0(span_cells, collapse = ""), '</tr>')
  } else ""

  # Phase 15d: the title is a `<div>` sibling BEFORE the <table>, not a `<caption>` child -- a caption
  # participates in the table's width (a long centred title widened / wrapped thin tables). As a left-
  # aligned block it fills the container and wraps only past the table's own width. See R/tab-css.R
  # (.tabxplor-caption).
  cap <- if (!is.null(caption) && length(caption) && nzchar(caption)) {
    paste0('<div class="tabxplor-caption">', htmltools::htmlEscape(caption), '</div>')
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
  # Phase 14m-ii (rework): a table that SHOWS significance stars gets `tx-has-stars`, which flips the
  # number cells to the monospace stack (+ a size bump) in the stylesheet. A plain table stays DejaVu
  # Sans. The class -- not an inline font -- keeps the look restyleable and tab_css() table-independent.
  tbl_class <- if (isTRUE(roles$has_stars)) "tabxplor-tab tx-has-stars" else "tabxplor-tab"
  paste0(
    cap,
    '<table class="', tbl_class, '">',
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
# itself). It replaced the old static html_style_block() -- the theme now lives entirely in the CSS.
# Phase 14k: `theme` is the render INTENT ("light"/"dark"/"auto"), carried to print.tabxplor_kable() so
# the standalone page it opens in the Viewer can paint itself to match. See the attr rule below.
#' @keywords internal
tab_kable_join <- function(parts, css = "", theme = NULL) {
  body <- paste(unlist(parts), collapse = "\n<br>\n")
  out  <- if (nzchar(css)) paste0("<style>", css, "</style>\n", body) else body
  # Phase 14e: the `kableExtra` class is what routes an HTML table to the Viewer (print.kableExtra)
  # and knits it (knit_print.kableExtra). Without it this was a bare `knitr_kable`, whose print just
  # cat()s the markup to the console -- so the maintainer had to re-class it by hand to see a table.
  # We produce the same thing kableExtra does (an HTML fragment, `format = "html"`), so we claim the
  # class rather than duplicate its two methods. (kableExtra is Suggests, not Imports -- when it is
  # absent, print.tabxplor_kable's interactive Viewer path degrades gracefully: no page, tooltips off,
  # a one-time note, fall through to knitr's print -- Phase 17g.) 19l deleted the kableExtra render
  # ENGINE; this class is the reason kableExtra remains a Suggests, and it is not the engine's.
  # Phase 14k prepends `tabxplor_kable`, whose print() paints the Viewer's page (below).
  out <- structure(out, format = "html",
                   class = c("tabxplor_kable", "kableExtra", "knitr_kable"))
  # THE RULE: tabxplor paints a page only when tabxplor's own stylesheet ships with the table -- the
  # same discriminator Phase 13d/14j use for the colour legend ("does our stylesheet ship?"). With
  # css = "" the document supplies it (options("tabxplor.tab_kable_css" = FALSE) + tab_css()) or nothing
  # does, and in the Viewer there is no document: painting the page #222222 around a table we did not
  # style would leave it black-on-#222222, i.e. unreadable. No attr => print does nothing new.
  if (nzchar(css)) attr(out, "tabxplor_theme") <- theme
  out
}


# === SECTION: the Viewer page (Phase 14k) =========================================================

# The standalone page print.tabxplor_kable() opens in the Viewer: the table, plus the chrome around it.
# Pure and self-contained -- `detected` is the impure probe as a DEFAULT ARGUMENT (the idiom
# R/tab-theme-detect.R already established: tx_positron_settings(file=), tx_theme_kind(ext_dir=)), so
# R's lazy evaluation forces it ONLY in the "auto" branch and a test can drive every path with no
# mocking and no dependence on the host IDE.
#
# WHY "auto" is resolved HERE, in R, rather than left to the 4-layer cascade: the Viewer is an Electron
# webview, where `@media (prefers-color-scheme)` reports the OPERATING SYSTEM, not the editor's colour
# theme -- so the cascade cannot see a dark Positron on a light OS (or the reverse), and the table ends
# up fighting the pane around it. Only R can see the editor (tx_detect_theme(), Phase 14g). A file or a
# knitted document keeps the cascade untouched: there the READER decides, and the browser is right.
#
# HOW the resolution is expressed: a `data-theme` wrapper, i.e. this page declares an explicit toggle,
# which is exactly what tx_dark_hooks/tx_light_hooks exist for -- cascade layers 3/4 (0,2,x) then beat
# the @media layer (0,1,x) in BOTH directions. No fifth layer, no second copy of the stylesheet, no new
# mechanism. It is emitted only under "auto": with an explicit theme the stylesheet is a single static
# layer carrying no hook rule at all, so a wrapper would be inert markup -- and its absence is what
# proves the detector never leaks into an explicit theme.
#' @keywords internal
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

# Which print path a tabxplor_kable takes. Pure predicate (all inputs passed in) so it is testable --
# testthat is never interactive(), so the branches below are otherwise unreachable (the tx_kable_page
# precedent). Returns:
#   "next"    : fall through to kableExtra/knitr's own print (no theme, non-interactive, view off, knitting)
#   "degrade" : an interactive themed print the Viewer wants, but kableExtra is absent -> knitr print + note
#   "viewer"  : paint the themed Viewer page and let kableExtra bind the tooltips
#' @keywords internal
#' @noRd
kable_print_mode <- function(theme, interactive, view_opt, knitting, have_ke) {
  if (is.null(theme) || !interactive || !isTRUE(view_opt) || knitting) return("next")
  if (!have_ke) return("degrade")
  "viewer"
}

#' Print a tabxplor html table
#'
#' Opens the html table \code{\link{tab_kable}} returned in the Viewer, on a page painted to match it
#' -- so a \code{theme = "dark"} table no longer sits in a white pane. Under \code{theme = "auto"} the
#' theme is resolved from **your editor** rather than your operating system: the Viewer is a webview,
#' and its \code{prefers-color-scheme} reports the OS, so it cannot see the editor the table is sitting
#' in. Anything else -- a non-interactive print, a knitted document, or a table tabxplor did not style
#' (\code{css = FALSE}, or the kableExtra engine) -- prints exactly as \pkg{kableExtra} does.
#'
#' @param x A html table returned by \code{\link{tab_kable}}.
#' @param ... Passed to \pkg{kableExtra}'s print method.
#' @return \code{x}, invisibly.
#' @seealso \code{\link{tab_kable}}, \code{\link{tab_css}}
#' @export
#' @keywords internal
print.tabxplor_kable <- function(x, ...) {
  theme <- attr(x, "tabxplor_theme")
  # Everything but an interactive Viewer print falls through to kableExtra's own method, byte for byte:
  #   - no theme      : we did not ship the stylesheet, so the page is not ours to paint (see the join)
  #   - !interactive(): kableExtra's print cat()s the markup; there is no page. (This is also the ONLY
  #                     branch the test suite executes -- testthat is never interactive.)
  #   - knitting      : the page belongs to the DOCUMENT. Painting its html,body would repaint Quarto
  #                     around the table. knit_print is likewise NOT overridden: dispatch walks the
  #                     class vector on to knit_print.kableExtra, which is what we want.
  mode <- kable_print_mode(theme, interactive(),
                           getOption("kableExtra_view_html", TRUE),
                           !is.null(knitr::opts_knit$get("out.format")),
                           requireNamespace("kableExtra", quietly = TRUE))
  if (identical(mode, "next")) return(NextMethod())
  # Phase 17g: the themed Viewer page AND the tooltip binding are kableExtra's to attach -- its two
  # UNEXPORTED html deps (html_dependency_kePrint / _lightable) carry the JS. kableExtra is Suggests, so
  # when it is absent we cannot reproduce them; degrade cleanly rather than dispatch into a missing
  # method: a one-time note, then knitr's own print (NextMethod) -- table shows, tooltips simply off.
  if (identical(mode, "degrade")) {
    rlang::inform(
      c("!" = "An interactive Viewer page for tabxplor html tables needs the {kableExtra} package.",
        "i" = "Install it (install.packages(\"kableExtra\")) for a themed Viewer page with tooltips."),
      .frequency = "once", .frequency_id = "tabxplor_kable_viewer_no_kableExtra")
    return(NextMethod())
  }
  # Delegate, never reimplement: kableExtra's print is what attaches jquery + bootstrap + those two
  # dependencies -- the JS that binds our tooltips in the Viewer. Reproducing it would mean kableExtra:::.
  print(structure(tx_kable_page(as.character(x), theme),
                  format = "html", class = c("kableExtra", "knitr_kable")), ...)
  invisible(x)
}

# Wrap a home-built html fragment in a horizontally-scrollable, width-capped box for the jamovi results
# iframe (replaces kableExtra::scroll_box; self-contained -- needs no external CSS). ONLY the two jmvtab
# backends call this.
#
# Phase 15c width fix: jamovi's results `.jmv-results-html` is a fixed ~500px box whose `.content` caps
# at max-width:450px and does NOT clip; the iframe auto-sizes to content and reports width+40 to the
# panel. The old `width:100%` resolved against that 450-500px cap -> a cramped box with a wide blank
# panel beside it. `width:max-content` sizes the box to the TABLE's own intrinsic width (independent of
# the host cap, so a small table shows no blank), `max-width:<CAP>px` bounds how wide the panel may grow,
# and `overflow-x:auto` gives an INTERNAL horizontal scrollbar past the CAP.
#
# OS-scaling-aware CAP (Phase 15c-ii): the box CSS lives in a scoped <style> + a `.tx-scrollbox` class
# (NOT an inline style -- an inline max-width would out-specify the @media rules). `device-width` media
# features are evaluated against the physical SCREEN (not the iframe's content-sized viewport, so no
# feedback loop) and are expressed in CSS pixels, which ALREADY fold in OS display scaling -- a 4K panel
# at Windows 150% reports 2560 CSS px, not 3840. So a cap tiered on device-width fits each machine's
# real, scaled display. `device-width` is deprecated but still honoured by Chromium/Electron; if a
# browser ever drops it, no @media matches and the base `max_width` stands -> identical to a plain fixed
# cap. `@media` order matters (equal specificity, last match wins): the wider 4K@100% tier is placed
# after the QHD tier so it overrides. NB: needs live review; tune the px thresholds/caps to taste.
#' @keywords internal
tab_render_scrollbox <- function(html, max_width = 1600L) {
  base <- if (is.null(max_width)) 1600L else max_width
  css <- paste0(
    "<style>",
    ".tx-scrollbox{overflow-x:auto;width:max-content;display:block;max-width:", base, "px;}",
    "@media (min-device-width:2200px){.tx-scrollbox{max-width:2000px;}}",   # QHD / 4K scaled (e.g. 2560)
    "@media (min-device-width:3200px){.tx-scrollbox{max-width:2600px;}}",   # 4K at 100 % scaling
    "@media (max-device-width:1500px){.tx-scrollbox{max-width:1200px;}}",   # small / high-scaling laptop
    "</style>"
  )
  paste0(css, '<div class="tx-scrollbox">', as.character(html), '</div>')
}
