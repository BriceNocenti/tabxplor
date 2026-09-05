# PURPOSE: the REGION under a table -- what appears there, in what order, in every medium.
# ROLE: the third part of what a table says about itself. R/fmt_class.R declares the colour MEASURES,
#   R/tab-legend.R turns them into a sentence, and this file decides what is printed beneath the grid
#   and where: the weight line, the `Model:` lines, the colour legend, the significance-stars key, the
#   user's own notes, and the tables and notes that travel under the whole block.
# KEY CONSTRAINTS:
#   - FOOTER_BLOCKS is the region: one row per member, ROW ORDER IS READING ORDER, and a row is GATED
#     BY WHAT IT READS -- which is also the degradation contract. A table stripped of its metadata
#     keeps the column-derived half of its footer (the colour legend, the stars key) and drops the
#     rest, with no exception handling anywhere.
#   - THE TEXT IS A TEMPLATE THE TABLE CARRIES: `subtext`, written by the producers, holds one
#     `<placeholder>` per generated line and one line per thing a person wrote, IN PRINTING ORDER.
#     ⚠ A subtext naming no placeholder is APPENDED to the default instead -- the rule that keeps a
#     bare note, a raw `attr(x, "subtext") <-` and jamovi's free-text box working -- and an unknown
#     `<...>` passes through verbatim, claiming nothing.
#   - AND IT NAMES WHAT *THIS* TABLE CAN SAY (the `default` column): a member built from `meta` is
#     named only where its fact exists, one built from the COLUMNS always. So the two gates answer
#     different questions -- `reads` decides what a stripped table still prints, `default` what the
#     producers write down -- and a predicate may over-name but must never under-name.
#     ⚠ Written at each PRODUCER'S TAIL, on the finished object: a regression's model record is
#     attached after the table is assembled (set_reg_call()), so anything earlier prunes <model>.
#   - A generated member belongs to the HOST (the `carried` column): a subordinate table renders what
#     it carries and nothing else, so a host + subordinate pair shows ONE colour legend.
#   - ⚠ In the CONSOLE a note and a subordinate table print ABOVE the grid, because the last thing
#     printed is the R object you can go on to pipe; in an export they read below the footer.
#   - Every footer line is a TOKEN STREAM, so a plain one-liner is a 1-token stream and one renderer
#     (render_footer) covers the whole region. `role` only picks the console's subtle prefix.
#   - Composed at RENDER, like the legend it carries: see R/tab-legend.R's header and
#     dev/legend_and_side_tables.md section 3.
#   - THE EMITTER MATRIX -- three kinds, five media, and every emitter already existed:
#       line   legend_render_line(medium = "console" | "md" | "html" | "runs" | "plain")
#       note   the pipe-table renderer (console + md) / note_html() / note_xl()
#       tab    tab_pipe() (console) / the exporters' `list_method = TRUE` path
# See: dev/legend_and_side_tables.md; CLAUDE.md section "Exports and rendering".

# the legend style for EXPORTS (md / html / Excel). Default "prose"; options(tabxplor.legend_style =
# "terse") gives the compact one-liner. The console always uses "terse". Any value but "terse" -> "prose".
legend_export_style <- function() {
  if (identical(tx_option("legend_style"), "terse")) "terse" else "prose"
}

# === SECTION: the region, declared ==================================================================
#
# DESIGN: every footer line is a TOKEN STREAM -- no plain-vs-legend split, because legend_render_line()
# renders uncoloured tokens too, so a plain one-liner is a 1-token stream. One renderer (render_footer)
# covers the whole region; `role` only picks the console subtle.
#
# The typed context every builder reads. `lang` is resolved ONCE here, so no builder can answer in a
# different language from its neighbour.
#' @keywords internal
#' @noRd
new_footer_ctx <- function(x, style = "prose", lang = NULL, theme = "light",
                           subtext = character(0), legend = TRUE) {
  list(style = style, lang = legend_resolve_lang(lang), theme = theme,
       subtext = subtext, legend = legend)
}

# ---- the builders: (x, ctx) -> a LIST of token streams (0, 1 or several lines) ---------------------
# Each is the only reader of its own fact, and each is GATED BY WHAT IT READS: a table that has lost
# its metadata makes these return nothing, which is the whole degradation contract (see the file
# header and dev/legend_and_side_tables.md section 4).

fb_weight <- function(x, ctx, args = character(0)) {
  wl <- tab_weight_line(x, lang = ctx$lang)
  if (is.null(wl)) list() else list(list(.lg_tok(wl)))
}

fb_model <- function(x, ctx, args = character(0))   # translated per family
  lapply(Filter(nzchar, reg_model_lines(x, ctx$lang)), function(rl) list(.lg_tok(rl)))

# the aggregated effect-modification test (predictor x tab_vars) -- table-wide, so it rides the stream
# footer like the weight / Model: lines. `esc = TRUE`: the p-values carry stars pandoc would read as
# emphasis. (The per-predictor global test is footer ROWS -- reg_test_rows_plan() -- not a line here.)
fb_interaction <- function(x, ctx, args = character(0))
  lapply(Filter(nzchar, reg_interaction_lines(x, ctx$lang)), function(il) list(.lg_tok(il, esc = TRUE)))

# `<legend:terse>` / `<legend:prose>` pins the register ON THE TABLE, which is the only lever the
# console ever had -- `options(tabxplor.legend_style)` says what the EXPORTS do by default.
fb_legend <- function(x, ctx, args = character(0)) {
  if (!isTRUE(ctx$legend)) return(list())
  st <- intersect(args, c("terse", "prose"))
  legend_streams(x, if (length(st)) st[[1L]] else ctx$style, ctx$lang, ctx$theme)
}

# `esc = TRUE` -> the md renderer escapes the `*` glyphs (else pandoc reads them as emphasis).
fb_stars <- function(x, ctx, args = character(0)) {
  sl <- suppressWarnings(tab_stars_legend(x, lang = ctx$lang, theme = ctx$theme))
  if (is.null(sl)) list() else list(list(.lg_tok(sl, esc = TRUE)))
}

# one STREAM per carried line -- footer_text_tokens() already returns the line's token stream.
fb_user <- function(x, ctx, args = character(0))
  lapply(Filter(nzchar, ctx$subtext), function(s) footer_text_tokens(s, x, ctx))

# ---- the INLINE builders: (x, ctx, args) -> a list of TOKENS spliced into one line -----------------
# They are the reason a foreign package never pastes a break value into prose: `<breaks>` is built from
# the very plan the cells are painted with, so a hand-written ladder cannot drift from color_breaks.

# THE spec of the measure an inline token speaks about: the one named in `args`, else the table's own
# when it has a single one. NULL when the table is uncoloured or the name matches nothing.
# `mods` are the token's OWN modifier words, which are not measure names (`over`, `noun`, ...).
# `resolve`: only the tokens that read a per-channel phrase pay for legend_resolve_spec(), and they
# pay INSIDE the tryCatch -- a stripped table must reach the same empty answer, not an error.
fi_spec <- function(x, ctx, args, mods = character(0), resolve = FALSE) {
  tryCatch({
    specs <- legend_specs(x, ctx$theme)
    hit   <- NULL
    if (length(specs)) {
      m <- setdiff(args, mods)
      if (!length(m)) hit <- specs[[1L]]
      else {
        k <- measure_key(m[[1L]])
        f <- Filter(function(s) identical(s$measure_text, k) || identical(s$measure_bg, k), specs)
        if (length(f)) hit <- f[[1L]]
      }
    }
    if (is.null(hit) || !resolve) hit else legend_resolve_spec(hit, ctx$lang)
  }, error = function(e) NULL)
}

fi_breaks <- function(x, ctx, args = character(0)) {
  spec <- fi_spec(x, ctx, args, mods = c("over", "under")); if (is.null(spec)) return(list())
  is_bg <- is.null(spec$plan_txt)
  plan  <- if (is_bg) spec$plan_bg else spec$plan_txt
  bt    <- legend_break_tokens(plan, spec$is_pct, if (is_bg) "bg" else "text", ctx$lang, ctx$theme)
  side  <- intersect(args, c("over", "under"))
  toks  <- if (!length(side))            c(rev(bt$under), bt$over)
           else if (side[[1L]] == "over") bt$over
           else                           bt$under
  legend_join(toks, " ")
}

# The measure's own NAME, built by the very call the terse legend makes -- so `<measure> (<ref>):
# <breaks>` IS the terse line, and a hand-written legend cannot drift from the generated one. No
# resolution: every argument is already on the unresolved spec.
fi_measure <- function(x, ctx, args = character(0)) {
  spec <- fi_spec(x, ctx, args); if (is.null(spec)) return(list())
  plan <- spec$plan_txt %||% spec$plan_bg; if (is.null(plan)) return(list())
  list(.lg_tok(legend_measure_word(plan$measure, spec$is_std, spec$eff_word,
                                   plan$policy, plan$scale_key, words = spec$words)))
}

# ...and its BASELINE, in the two shapes a sentence needs: the compact clause the terse form brackets
# (preposition included), or `<ref:noun>`, the bare noun a lead points at.
fi_ref <- function(x, ctx, args = character(0)) {
  spec <- fi_spec(x, ctx, args, mods = "noun", resolve = TRUE); if (is.null(spec)) return(list())
  w <- if ("noun" %in% args) legend_ref_phrase(spec, "plain") else legend_ref_short(spec)
  if (is.null(w) || is.na(w) || !nzchar(w)) list() else list(.lg_tok(w))
}

# ...and what the interval WAS, through the one reader forest_plot() also uses.
fi_method <- function(x, ctx, args = character(0)) {
  ph <- legend_method_phrases(x, lang = ctx$lang)
  if (!length(ph)) return(list())
  # the joiner branches on the language, like every other one: French spaces its high punctuation.
  list(.lg_tok(paste(ph, collapse = if (identical(ctx$lang, "fr")) " ; " else "; ")))
}

fi_cols <- function(x, ctx, args = character(0)) {
  spec <- fi_spec(x, ctx, args, mods = c("over", "under")); if (is.null(spec)) return(list())
  nm <- if (length(setdiff(args, c("over", "under")))) spec$col_label else spec$col_var
  list(.lg_tok(legend_name_list(nm), esc = TRUE))
}

fi_conf <- function(x, ctx, args = character(0)) {
  cl <- unique(stats::na.omit(get_conf_level(purrr::keep(x, is_fmt))))
  if (!length(cl)) return(list())
  list(.lg_tok(paste0(legend_num(cl[[1L]] * 100, ctx$lang), "%")))
}

# ---- THE GRID -------------------------------------------------------------------------------------
# One row per member of the region below a table, and ROW ORDER IS READING ORDER -- the same shape
# TOOLTIP_LINES gives the hover. Adding a member is one row.
#   key      the row's name.
#   token    the `<placeholder>` a subtext template places it by, or NA for a member that is not text
#            and needs none: carrying its `reads` fact is what prints it, as for the test rows.
#   kind     "line" (a token stream, under the table in every medium) | "note" (a grid of rendered
#            character columns) | "tab" (a tabxplor_tab). See the emitter matrix in the file header.
#   role     render_footer()'s console subtle key; NA where the kind is not a line.
#   reads    WHICH FACTS this member is built from -- `meta$...` fields and `fmt` column attributes.
#            It is the row's gate, its degradation contract, and (through TAB_ATTRS' setters) the
#            source of ?tabxplor-footer's "to change what this says, use..." column.
#   carried  is this member CARRIED by the table (the user's own lines, the subordinate tables) rather
#            than GENERATED from it? A generated member belongs to the HOST: a subordinate table is not
#            a peer, so it renders what it carries and nothing else, which is what keeps one colour
#            legend under a host + subordinate pair instead of two.
#   build    closure(x, ctx) -> a list of token streams. NULL where the kind is not a line.
#   default  closure(x) -> is this member part of THIS TABLE'S OWN template? Absent = always named.
#            It answers a DIFFERENT question from `reads`: `reads` is the RENDER gate (a fact that is
#            gone prints nothing), this is the TEMPLATE gate (what the producers write down). So it
#            tests only what the BUILD settles and no setter can add later -- a weight, a model, an
#            interaction -- while everything derived from the COLUMNS stays named unconditionally,
#            since set_color() can colour an uncoloured table afterwards.
#            ⚠ It may OVER-name (the builder then prints nothing, and the reader deletes a line that
#            says nothing) but must NEVER under-name: a placeholder the producers omit is one nothing
#            brings back. ⚠ The predicates are CLOSURES, not bare symbols: this table is folded at
#            source time, before R/table-spec.R and R/tab_reg.R exist (the TAB_ATTRS `spec` precedent).
FOOTER_BLOCKS <- tx_grid(tibble::tribble(
  ~key,          ~token,        ~kind,  ~role,         ~carried, ~reads,                                                                                            ~build,         ~default,
  "weight",      "weight",      "line", "weight",      FALSE,    c("meta$spec$vars$wt", "basis", "conf_level", "display"),                                          fb_weight,      function(x) !is.null(footer_wt_name(x)),
  "model",       "model",       "line", "reg",         FALSE,    "meta$spec$call",                                                                                  fb_model,       function(x) tab_is_reg(x),
  "interaction", "interaction", "line", "reg",         FALSE,    "test",                                                                                            fb_interaction, function(x) reg_has_interaction(x),
  "legend",      "legend",      "line", "legend",      FALSE,    c("color", "color_signif", "scale", "ref", "col_var", "meta$legend_words", "meta$color_breaks"), fb_legend,      NULL,
  "stars",       "stars",       "line", "stars",       FALSE,    c("pvalue", "conf_level"),                                                                         fb_stars,       NULL,
  "user",        NA_character_, "line", "subtext",     TRUE,     "subtext",                                                                                         fb_user,        NULL,
  "shape",       NA_character_, "note", NA_character_, FALSE,    "meta$assumptions",                                                                                NULL,           NULL,
  "tabs",        NA_character_, "tab",  NA_character_, TRUE,     "meta$footer_tabs",                                                                                NULL,           NULL,
  # the INLINE tokens: they render INSIDE a line rather than as one, so they never claim the layout --
  # a note that quotes the confidence level must not cost its writer the whole generated footer.
  "breaks",      "breaks",      "inline", NA_character_, FALSE,  c("color", "meta$color_breaks"),                                                                   fi_breaks,      NULL,
  "measure",     "measure",     "inline", NA_character_, FALSE,  c("color", "meta$legend_words"),                                                                   fi_measure,     NULL,
  "ref",         "ref",         "inline", NA_character_, FALSE,  c("color", "ref", "meta$legend_words"),                                                            fi_ref,         NULL,
  "method",      "method",      "inline", NA_character_, FALSE,  c("ci_method", "conf_level", "degf"),                                                              fi_method,      NULL,
  "cols",        "cols",        "inline", NA_character_, FALSE,  c("col_var", "col_group"),                                                                         fi_cols,        NULL,
  "conf",        "conf",        "inline", NA_character_, FALSE,  "conf_level",                                                                                      fi_conf,        NULL,
))

# === SECTION: the template ==========================================================================
#
# THE ONE RULE: a `subtext` naming NO block placeholder gets the default template plus its lines
# appended -- which is what tabxplor has always done, so a plain note, a raw `attr(x, "subtext") <-`
# and jamovi's free-text box keep working. The moment one line IS a block placeholder, the subtext
# OWNS the layout: only what it names is printed, in the order it names it.
# ⚠ Only a BLOCK placeholder alone on its line claims the layout. An INLINE one (`<breaks>`, `<conf>`)
#   is substitution, so quoting the confidence level in a note cannot cost its writer the footer.
# ⚠ An unknown `<...>` is not a placeholder: it passes through VERBATIM and claims nothing, so raw
#   html in a subtext behaves as it always did and a typo prints itself instead of hiding a block.
#   `\<` escapes a literal `<` that would otherwise read as one.

# a `<token>` or `<token:arg:arg>`, the token lowercase-alphanumeric. Deliberately narrow: "n < 30"
# and "<30 ans>" do not match, so ordinary prose is never re-read as markup.
FOOTER_TOKEN_RE <- "<([a-z][a-z0-9_]*)((?::[A-Za-z0-9_.]+)*)>"

#' @keywords internal
#' @noRd
footer_token_row <- function(tok) {
  hit <- Filter(function(b) identical(b$token, tok), FOOTER_BLOCKS)
  if (length(hit)) hit[[1L]] else NULL
}

# the BLOCK a line IS, or NULL: the trimmed line must be exactly one placeholder naming a non-inline row.
#' @keywords internal
#' @noRd
footer_block_of <- function(line) {
  m <- regmatches(trimws(line), regexec(paste0("^", FOOTER_TOKEN_RE, "$"), trimws(line), perl = TRUE))[[1L]]
  if (!length(m)) return(NULL)
  b <- footer_token_row(m[[2L]])
  if (is.null(b) || identical(b$kind, "inline")) return(NULL)
  list(block = b, args = footer_args(m[[3L]]))
}

footer_args <- function(s) {
  s <- sub("^:", "", s %||% "")
  if (!nzchar(s)) character(0) else strsplit(s, ":", fixed = TRUE)[[1L]]
}

# ONE text line -> a token stream, with every INLINE placeholder replaced by what its row builds.
#' @keywords internal
#' @noRd
footer_text_tokens <- function(line, x, ctx) {
  unesc <- function(z) gsub("\\\\<", "<", z)
  m <- gregexpr(FOOTER_TOKEN_RE, line, perl = TRUE)[[1L]]
  if (m[[1L]] == -1L) return(list(.lg_tok(unesc(line))))
  starts <- as.integer(m); lens <- attr(m, "match.length")
  out <- list(); pos <- 1L
  for (i in seq_along(starts)) {
    piece <- substr(line, starts[[i]], starts[[i]] + lens[[i]] - 1L)
    mm    <- regmatches(piece, regexec(FOOTER_TOKEN_RE, piece, perl = TRUE))[[1L]]
    b     <- footer_token_row(mm[[2L]])
    # a block token used mid-sentence, or an unknown one: left verbatim, like any other text.
    if (is.null(b) || !identical(b$kind, "inline")) next
    if (starts[[i]] > pos)
      out <- c(out, list(.lg_tok(unesc(substr(line, pos, starts[[i]] - 1L)))))
    out <- c(out, b$build(x, ctx, footer_args(mm[[3L]])))
    pos <- starts[[i]] + lens[[i]]
  }
  if (pos <= nchar(line)) out <- c(out, list(.lg_tok(unesc(substr(line, pos, nchar(line))))))
  if (!length(out)) list(.lg_tok(unesc(line))) else out
}

# The block placeholders, in the order the region reads them -- what a merge sorts by.
#' @keywords internal
#' @noRd
footer_block_order <- function()
  unname(Filter(Negate(is.na), vapply(FOOTER_BLOCKS, function(b)
    if (identical(b$kind, "inline")) NA_character_ else b$token %||% NA_character_,
    character(1))))

# TWO TEMPLATES RECONCILED (the `subtext` row's `bind` rule): every generated line either table names,
# in the region's own order, then every line a person wrote, in the order they arrive. A merge unions
# the `test` rows, so it must union what SPEAKS about them -- else a weighted table bound onto an
# unweighted one loses its weight line, or states it after the stars.
#' @keywords internal
#' @noRd
subtext_bind <- function(sx, sy) {
  s <- c(as.character(sx %||% character(0)), as.character(sy %||% character(0)))
  s <- unique(s[!is.na(s) & nzchar(s)])
  if (!length(s)) return(character(0))
  tok <- vapply(s, function(l) { b <- footer_block_of(l)
                                 if (is.null(b)) NA_character_ else b$block$token },
                character(1), USE.NAMES = FALSE)
  ord <- match(tok, footer_block_order())
  c(s[!is.na(ord)][order(ord[!is.na(ord)])], s[is.na(ord)])
}

# THE LINE ROWS, in reading order. With a table, only the ones ITS OWN template names (the `default`
# column); without one, every row -- a table that never had a template gets the whole default and each
# builder's own gate decides, which is the degradation contract.
#' @keywords internal
#' @noRd
footer_default_rows <- function(x = NULL) {
  rows <- Filter(function(b) identical(b$kind, "line"), FOOTER_BLOCKS)
  if (is.null(x)) return(rows)
  Filter(function(b) is.null(b$default) || isTRUE(tryCatch(b$default(x), error = function(e) TRUE)),
         rows)
}

# Does this subtext CLAIM the layout -- is any line exactly one block placeholder? The vectorised
# pre-filter and the early exit are what keep it cheap on the dplyr path, where every verb re-checks a
# template that already claims it.
#' @keywords internal
#' @noRd
footer_claims_layout <- function(subtext) {
  cand <- grepl(paste0("^", FOOTER_TOKEN_RE, "$"), trimws(subtext), perl = TRUE)
  for (i in which(cand)) if (!is.null(footer_block_of(subtext[[i]]))) return(TRUE)
  FALSE
}

# THE EFFECTIVE TEMPLATE: a list of entries, each either a FOOTER_BLOCKS row (with its args) or one
# line of the user's own text. The default is FOOTER_BLOCKS' own order, the "user" row standing for
# wherever the carried lines go.
#' @keywords internal
#' @noRd
footer_plan <- function(subtext = character(0)) {
  subtext <- Filter(nzchar, as.character(subtext))
  blocks  <- lapply(subtext, footer_block_of)
  if (!any(vapply(blocks, Negate(is.null), logical(1)))) {                       # APPEND mode
    return(lapply(footer_default_rows(),
                  function(b) list(block = b, args = character(0), text = NA_character_)))
  }
  lapply(seq_along(subtext), function(i)                                          # OWN mode
    if (is.null(blocks[[i]])) list(block = NULL, args = character(0), text = subtext[[i]])
    else                      c(blocks[[i]], list(text = NA_character_)))
}

# THE DEFAULT TEMPLATE, as the producers write it into `subtext` AT THEIR TAIL: the tokens of the rows
# this FINISHED table can say something with, then the caller's own lines (the `user` row's slot).
# Stored rather than implied so a reader SEES what the footer is made of and can re-order or drop a
# part of it -- which is why it must name what THIS table says and not what every table might.
# ⚠ ON A FINISHED TABLE, never mid-build: a regression's model record is attached after the object is
#   assembled (set_reg_call()), so a template written earlier would omit <model> and <weight>.
# ⚠ IDEMPOTENT: a `subtext` already naming a block is returned untouched, so a rebuild, a merge or a
#   second producer in the chain cannot stack two templates.
#' @keywords internal
#' @noRd
footer_default_template <- function(x = NULL, subtext = character(0)) {
  subtext <- Filter(nzchar, as.character(subtext))
  if (footer_claims_layout(subtext)) return(subtext)
  toks <- Filter(function(b) !is.na(b$token), footer_default_rows(x))
  c(vapply(toks, function(b) paste0("<", b$token, ">"), character(1), USE.NAMES = FALSE), subtext)
}

# THE ordered below-table footer, as a list of typed token-streams: by default weight -> Model: ->
# colour-legend group(s) -> stars -> the carried lines, which is FOOTER_BLOCKS' own order; or whatever
# order `subtext` names. Each stream carries a `role` so render_footer() can subtle the plain lines
# whole while a legend keeps its colours. `legend = FALSE` drops the colour legend wherever it sits.
tab_footer_streams <- function(x, style = "prose", lang = NULL,
                               subtext = character(0), legend = TRUE, theme = "light",
                               host = TRUE) {
  ctx     <- new_footer_ctx(x, style = style, lang = lang, theme = theme,
                            subtext = subtext, legend = legend)
  streams <- list()
  push    <- function(toks, role) if (length(toks))
    streams[[length(streams) + 1L]] <<- list(tokens = toks, role = role)
  # DESIGN: ONE gettext scope for the whole region. new_footer_ctx() hoists the language VALUE; this
  # hoists the SCOPE, which is what the INLINE builders never had -- `<cols>`' overflow word and the
  # effect words legend_specs() reads used to answer in the ambient LANGUAGE, not in ctx$lang. The
  # line builders' own with_legend_lang() calls become free (it is re-entrant).
  with_legend_lang(ctx$lang, function(lg) {
    for (e in footer_plan(subtext)) {
      if (is.null(e$block)) {                  # a carried line, its inline tokens resolved
        push(footer_text_tokens(e$text, x, ctx), "subtext")
        next
      }
      # a SUBORDINATE renders what it carries and nothing generated (see the `carried` column).
      if (!isTRUE(host) && !isTRUE(e$block$carried)) next
      for (toks in e$block$build(x, ctx, e$args)) push(toks, e$block$role)
    }
  })
  streams
}

#' Read the footer a table would print
#'
#' @description
#' The lines \pkg{tabxplor} prints under a table, resolved: the template's placeholders built, the
#' user's own lines as written. It is what [set_subtext()] edits, after the fact.
#'
#' @param x A \code{tabxplor_tab}.
#' @param medium One of \code{"plain"} (default), \code{"console"}, \code{"html"}, \code{"md"}.
#' @param style \code{"terse"} (the console's compact one-liner) or \code{"prose"} (full sentences,
#'   the exports' default); \code{NULL} follows the medium.
#' @param lang \code{NULL} (from \code{getOption("tabxplor.lang")}), \code{"en"} or \code{"fr"}.
#' @param theme Palette theme; \code{NULL} follows the medium's option.
#' @return A character vector, one element per footer line.
#' @seealso [set_subtext()] for the template, [set_legend_words()] for the words.
#' @export
#' @examples
#' t <- tab(forcats::gss_cat, race, marital, pct = "row", color = "diff")
#' cat(tab_footer_text(t), sep = "\n")
tab_footer_text <- function(x, medium = c("plain", "console", "html", "md"),
                            style = NULL, lang = NULL, theme = NULL) {
  medium <- match.arg(medium)
  if (is.null(style)) style <- if (identical(medium, "console")) "terse" else legend_export_style()
  if (is.null(theme))
    theme <- tx_theme_option(if (identical(medium, "console")) "console" else "export")
  suppressWarnings(render_footer(
    tab_footer_streams(x, style = style, lang = lang, theme = tx_palette_theme(theme),
                       subtext = get_subtext(x) |> purrr::discard(\(s) s == "")),
    medium = medium, theme = theme))
}

# NAMING A SET OF VARIABLES, once, for every line that has to. Two axes, because a name list is doing
# one of two jobs:
#   join      "and" where the list is PROSE a reader reads ("cinema by qualif, sexe and age"), "comma"
#             where it is a LABEL that merely identifies ("Obs_RD, Model_mRD --").
#   overflow  "count" where the reader wants to know how many were left out ("by 4 predictors"),
#             "etc" where a count says nothing they want ("marital, race, etc.").
# ⚠ ONE STRING LITERAL PER gettextf() CALL: potools extracts each literal it sees, so a message built
#   with paste0() INSIDE the call can never be found at run time.
#' @keywords internal
#' @noRd
tx_name_list <- function(x, max = 3L, join = c("comma", "and"),
                         overflow = c("etc", "count"), noun = NULL) {
  join <- match.arg(join); overflow <- match.arg(overflow)
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x)]
  if (!length(x)) return("")
  if (length(x) > max) {
    if (identical(overflow, "count") && !is.null(noun)) return(gettextf("%s %s", length(x), noun))
    return(paste0(paste(utils::head(x, max), collapse = ", "), ", ", gettext("etc.")))
  }
  if (length(x) == 1L || identical(join, "comma")) return(paste(x, collapse = ", "))
  # the last two joined by a word, the rest by commas: "qualif, sexe and age" / "qualif, sexe et age"
  gettextf("%s and %s", paste(utils::head(x, -1L), collapse = ", "), utils::tail(x, 1L))
}

# Name a variable set for a TITLE: up to `max` names, then how many there were -- never "multi",
# which named nothing, and never a bare index. Placeholders and empties drop out.
# ⚠ ONE renderer, TWO doors: this filters placeholder col_vars, legend_name_list() (R/tab-legend.R)
# undoes an html wrap marker instead. The pre-processing is what differs, never the joining.
#' @keywords internal
#' @noRd
tab_title_names <- function(x, max = 3, noun = NULL, join = "comma") {
  x <- as.character(x)
  x <- x[is_real_col_var(x)]
  tx_name_list(x, max = max, join = join,
               overflow = if (is.null(noun)) "etc" else "count", noun = noun)
}

# ...and TRUE when that set overflowed, which a title needs: "cinema, by 4 predictors" takes a comma
# where "cinema by qualif and sexe" does not.
#' @keywords internal
#' @noRd
tx_name_list_counted <- function(x, max = 3L) sum(!is.na(x) & nzchar(as.character(x))) > max

# === SECTION: the note kind =========================================================================

#' A note under a table
#'
#' @description
#' A small grid of **already-rendered** character columns, printed under a table in the aside ink: a
#' glossary, a range, a set of diagnostics --- something that belongs to the table without being a row
#' of it. Attach it with \code{\link{set_footer_tabs}}, beside (or instead of) a real table.
#'
#' A plain \code{data.frame} passed to \code{set_footer_tabs()} already renders as a note, its own
#' names as headers, everything left-aligned. \code{tab_note()} is for when that is not enough.
#'
#' @param df A data.frame of character columns, already formatted.
#' @param headers Column titles; defaults to \code{names(df)}.
#' @param align One of \code{"left"} / \code{"right"} per column; defaults to all left.
#' @param grey One logical per row: a row to render in the dimmer aside ink (a result the note itself
#'   marks as not to be read). \code{NULL} for none.
#' @param note One or more lines printed under the grid, smaller still --- what a cell cannot say for
#'   itself.
#' @param kind Per column, \code{"text"} (default) or \code{"spark"}, a run of block glyphs the html
#'   backend upgrades to an inline \code{<svg>}.
#' @return A \code{tabxplor_note}.
#' @seealso [set_footer_tabs()] to attach one, [set_subtext()] for the footer's text.
#' @export
#' @examples
#' n <- tab_note(data.frame(axis = c("1", "2"), variance = c("9.9%", "7.2%")),
#'               headers = c("Axis", "% variance"), align = c("left", "right"))
#' set_footer_tabs(tab(forcats::gss_cat, race, marital, pct = "row"), list("Axes" = n))
tab_note <- function(df, headers = NULL, align = NULL, grey = NULL, note = character(0),
                     kind = NULL) {
  if (!is.data.frame(df)) cli::cli_abort("{.arg df} must be a data.frame.")
  df <- as.data.frame(lapply(df, as.character), stringsAsFactors = FALSE,
                      optional = TRUE, col.names = names(df))
  n  <- length(df)
  chk <- function(v, what, len) {
    if (is.null(v)) return(NULL)
    if (length(v) != len) cli::cli_abort("{.arg {what}} must have {len} value{?s}, not {length(v)}.")
    v
  }
  structure(
    df,
    headers = chk(headers, "headers", n) %||% names(df),
    align   = chk(align,   "align",   n) %||% rep("left", n),
    kind    = chk(kind,    "kind",    n) %||% rep("text", n),
    noisy   = chk(grey,    "grey",    nrow(df)),
    note    = as.character(note),
    class   = c("tabxplor_note", class(df)))
}

#' @keywords internal
#' @noRd
is_tab_note <- function(x) inherits(x, "tabxplor_note")

# ANY data.frame that is not a tabxplor_tab reads as a note, so the common case needs no constructor.
#' @keywords internal
#' @noRd
as_tab_note <- function(x) if (is_tab_note(x)) x else tab_note(x)

# THE notes a table prints under itself: the ones it carries (set_footer_tabs()), then the regression
# shape table where the option and the data call for one. `syntax = "html"` asks the producers for
# markup in the cells that can carry it.
#' @keywords internal
#' @noRd
footer_notes <- function(x, medium = "console", syntax = c("text", "html")) {
  syntax <- match.arg(syntax)
  if (!is.data.frame(x)) return(list())
  carried <- Filter(Negate(is_tab), get_footer_tabs(x) %||% list())
  out <- lapply(carried, as_tab_note)
  if (is_tab(x) && tab_wants_shape_table(x, medium)) {
    sh <- reg_shape_table(x, syntax = syntax)
    if (!is.null(sh)) out <- c(out, list(sh))
  }
  out
}

# A GFM pipe table from already-rendered character columns -- the console's notes and the Markdown
# exporter's are the same lines, so they are built once. Widths are counted in CHARACTERS, exact in a
# monospace medium and near enough in a proportional one.
# `grey`: one logical per row, styled AFTER the padding (an ANSI sequence has no width, but nchar()
# would count it). NULL in a medium that has no colour, where the note's own marks carry the verdict.
#' @keywords internal
#' @noRd
tx_pipe_table <- function(df, headers = NULL, align = NULL, grey = NULL) {
  headers <- headers %||% attr(df, "headers") %||% names(df)
  align   <- align   %||% attr(df, "align")   %||%
    vapply(df, function(cl) if (is.numeric(cl)) "right" else "left", character(1), USE.NAMES = FALSE)
  cols <- lapply(seq_along(df), function(j) c(headers[[j]], as.character(df[[j]])))
  w    <- vapply(cols, function(c) max(nchar(c, type = "chars")), integer(1))
  pad  <- function(s, j) formatC(s, width = w[[j]], flag = if (align[[j]] == "right") "" else "-")
  emit <- function(cells) paste0("| ", paste(cells, collapse = " | "), " |")
  body <- vapply(seq_len(nrow(df)), function(i)
    emit(vapply(seq_along(df), function(j) pad(as.character(df[[j]])[[i]], j), character(1))),
    character(1))
  if (!is.null(grey) && any(grey)) body[grey] <- cli::col_grey(body[grey])
  c(emit(vapply(seq_along(df), function(j) pad(headers[[j]], j), character(1))),
    paste0("|", paste(vapply(seq_along(df), function(j) mk_align(w[[j]], align[[j]]), character(1)),
                      collapse = "|"), "|"),
    body)
}

# the note as MARKDOWN: the grid, then its own lines in italics.
#' @keywords internal
#' @noRd
note_md <- function(nt) {
  ln <- attr(nt, "note")
  c(tx_pipe_table(nt),
    if (length(ln)) c("", paste0("*", paste(ln, collapse = " "), "*")))
}

# ...and as CONSOLE lines: the same grid in the aside ink, a NOISY row one step dimmer, then the note
# behind the footer's own `# ` prefix.
#' @keywords internal
#' @noRd
note_console <- function(nt) {
  aside <- tryCatch(cli::make_ansi_style(tx_chrome_hex(tx_theme_option("console"))$grey2),
                    error = function(e) identity)
  ln <- attr(nt, "note")
  c(aside(tx_pipe_table(nt, grey = attr(nt, "noisy"))),
    if (length(ln)) cli::col_grey(paste0("# ", ln)))
}

# HOW MANY ROWS a note block occupies on a sheet: its header, its rows, its own lines, and one blank.
# ONE arithmetic, so the geometry that reserves the space and the geometry that writes it agree.
#' @keywords internal
#' @noRd
note_xl_rows <- function(notes) {
  if (!length(notes)) return(0L)
  sum(vapply(notes, function(n) nrow(n) + length(attr(n, "note")) + 2L, integer(1)))
}

# ...and the cells of every note a table carries, stacked from `row0`.
#' @keywords internal
#' @noRd
note_xl_all <- function(notes, row0, index_cols = 1L) {
  if (!length(notes)) return(NULL)
  out <- list(); r <- row0
  for (n in notes) {
    out <- c(out, list(note_xl(n, r, index_cols = index_cols)))
    r   <- r + nrow(n) + length(attr(n, "note")) + 2L
  }
  purrr::list_rbind(purrr::compact(out))
}

# The ONE footer invocation every backend shares. `src` is the fmt SOURCE table (rd$color_src for a
# transposed model, whose rd$tab is plain character; else rd$tab). `want_legend` gates ONLY the colour
# legend; the other streams follow their own `reads`. `host = FALSE` for a subordinate table.
rd_blocks <- function(src, medium, theme = NULL, want_legend = TRUE,
                      subtext = character(0), lang = NULL, classes = FALSE, host = TRUE) {
  suppressWarnings(render_footer(
    tab_footer_streams(src, style = legend_export_style(), lang = lang,
                       subtext = subtext, legend = want_legend,
                       # the direction WORDS are a palette fact, decided while the tokens are built:
                       # a publication legend says "Underlined"/"Italic", a colour one names none.
                       theme = tx_palette_theme(theme), host = host),
    medium = medium, theme = theme, classes = classes))
}

# render the footer streams for one medium. Console applies the "# " subtle prefix per line, role-aware:
# a legend keeps its coloured break-words (only the prefix is subtle), every other line is subtle whole.
# Other media return the rendered character vector (md/html/plain) or run-lists (runs); the caller places them.
render_footer <- function(streams, medium, theme = NULL, colored = TRUE, classes = FALSE) {
  # the theme scope is derived from the MEDIUM (only the console footer belongs to the console palette),
  # read through tx_theme_option() (R/tab-css.R).
  if (is.null(theme))
    theme <- tx_theme_option(if (identical(medium, "console")) "console" else "export")
  if (length(streams) == 0) return(if (identical(medium, "runs")) list() else character(0))
  toks_list <- lapply(streams, function(s) s$tokens)
  out <- render_streams(toks_list, medium, theme, colored, classes)
  if (identical(medium, "console")) {
    roles <- vapply(streams, function(s) s$role, character(1))
    out <- ifelse(roles == "legend",
                  paste0(pillar::style_subtle("# "), out),
                  pillar::style_subtle(paste0("# ", out)))
  }
  out
}

# The null a regression `Constant` row's star is tested against, or NA when no column TESTS that row.
# "Constant" is the SKELETON's own untranslated key, not a label, so this reads a stored fact. NA too
# when the starred model columns disagree about their null -- a mixed table cannot name one number.
#' @keywords internal
tab_constant_null <- function(x, cols) {
  if (!tab_is_reg(x)) return(NA_real_)
  ax <- x[["var"]]
  if (is.null(ax)) return(NA_real_)
  cst <- as.character(ax) == "Constant"
  cst[is.na(cst)] <- FALSE
  if (!any(cst)) return(NA_real_)
  n <- unique(unlist(purrr::map(cols, function(cl) {
    # ⚠ the test is a finite P-VALUE, not a finite estimate: a `marginal` / `at_reference` Constant
    # holds a predicted BASELINE, which carries no p-value and takes no star, so it has no null to
    # name; an ordinal fit has no intercept at all and shows nothing there.
    if (!identical(get_role(cl), "model")) return(NULL)
    if (!any(is.finite(get_pvalue(cl)[cst]))) return(NULL)
    fmt_scale_row(cl)$neutral
  })))
  n <- n[!is.na(n)]
  if (length(n) == 1L) n else NA_real_
}

# the significance-stars legend line, shown when any DISPLAYED, star-applicable fmt column carries a
# star (never on a contrib table -- fmt_stars_applicable). Thresholds/labels come from the same options
# get_stars() reads, so the named confidence levels match the glyphs drawn. Returns one plain string or NULL.
# A publication palette that MARKS its cells prints no star at all (fmt_cell_suffix), so it prints no
# stars legend either -- the marks are explained by the break-words they ride on.
tab_stars_legend <- function(x, lang = NULL, theme = NULL) {
  if (print_palette_marks(print_palette_of(tx_palette_theme(theme)))) return(NULL)
  cols <- purrr::keep(x, ~ is_fmt(.) && fmt_stars_applicable(.))
  if (length(cols) == 0) return(NULL)
  if (!any(vapply(cols, function(cl) any(nzchar(get_stars(cl))), logical(1)))) return(NULL)
  with_legend_lang(lang, function(lg) {
    ladder <- tx_stars_ladder()
    lev  <- sort(unname(ladder))                                              # ascending p
    lab  <- names(ladder)
    lab  <- lab[order(nchar(lab), decreasing = TRUE)]                          # most stars first
    conf <- (1 - lev) * 100                                                    # aligned: *** <-> 99%
    semi <- if (identical(lg, "fr")) " ; " else "; "
    # ONE sentence for every table. A regression's `Constant` row is the exception -- its star tests
    # the baseline value against the measure's own null -- so it is a parenthesis, appended only where such
    # a row exists, naming the null EST_SCALES declares (1 on a ratio, 0 on a difference).
    nul   <- tab_constant_null(x, cols)
    first <- if (is.na(nul)) gettextf(
      "%s: significantly different from the reference category (in bold) at the %s%% confidence level",
      lab[1], legend_num(conf[1], lg))
    else gettextf(
      "%s: significantly different from the reference category (in bold) at the %s%% confidence level (from %s for the Constant)",
      lab[1], legend_num(conf[1], lg), legend_num(nul, lg))
    rest <- if (length(lab) > 1)
      vapply(2:length(lab), function(i) gettextf("%s: at the %s%% level", lab[i],
                                                 legend_num(conf[i], lg)), character(1))
    else character(0)
    none <- gettext("no star: not significant")
    enc2utf8(paste0(paste(c(first, rest, none), collapse = semi), "."))
  })
}

# IS THERE ANY INFERENCE ON THIS TABLE -- an interval, a star, a test, or a colour that only paints
# what is significant? Read from the COLUMNS, so it answers for the table as rendered, and read here
# rather than threaded down from tab_export_prep()'s `roles` (which every caller of rd_blocks() would
# have to carry). The one reader is tab_weight_line(): a caveat about what the intervals rest on has
# nothing to say where the reader can see no interval.
# ⚠ DISPLAYED, not merely computed: `ci` and `moe` are two tokens over one stored field, and a colour
# counts only when its policy actually gates on significance (`color_signif = "ignore"` does not).
tab_shows_inference <- function(x) {
  cols <- purrr::keep(x, is_fmt)
  if (length(cols) == 0) return(FALSE)
  shows_ci <- function(cl) {
    d <- get_display(cl)
    any(fmt_display_shows(d, "ci")) || any(fmt_display_shows(d, "moe"))
  }
  if (any(vapply(cols, shows_ci, logical(1)))) return(TRUE)
  # the stars gate, said exactly as tab_stars_legend() says it
  starred <- purrr::keep(cols, fmt_stars_applicable)
  if (any(vapply(starred, function(cl) any(nzchar(get_stars(cl))), logical(1)))) return(TRUE)
  sig <- get_color_signif(cols) %in% c("grey_non_signif", "guaranteed_effect") &
    !get_color(cols) %in% c("", "no") & !is.na(get_color(cols))
  if (any(sig)) return(TRUE)
  tt <- get_test(x)
  !is.null(tt) && nrow(tt) > 0 && any(!is.na(tt$pvalue))
}

# THE weight this table was built with, or NULL. Two homes, because a crosstab stamps it into its
# `vars` while tab_reg() keeps it in the model record -- so the `weight` row's `default` predicate and
# its builder read ONE function and cannot disagree about what "there is a weight" means.
#' @keywords internal
#' @noRd
footer_wt_name <- function(x) {
  ok <- function(v) !is.null(v) && length(v) > 0L && !is.na(v[[1]]) && nzchar(v[[1]])
  wt <- tryCatch(get_vars_attr(x)$wt, error = function(e) NULL)
  if (!ok(wt)) wt <- tryCatch(reg_call(x)$wt, error = function(e) NULL)
  if (!ok(wt)) return(NULL)
  as.character(wt)[1]
}

# the weight footer line, shown FIRST when the table was built with a weight (NULL when unweighted).
# ONE sentence per INFERENCE BASIS, generated from the stored basis -- so the claim cannot outlive
# the computation, and a weighted estimate on a raw-n interval (the DEFAULT) is stated, not silent.
# ⚠ EACH BASIS HAS TWO FORMS and tab_shows_inference() picks, because that caveat only has a subject
# where an interval, a star or a test is on the page. The long form warns about what they rest on;
# the short one says the only thing left to say, that the table is weighted.
tab_weight_line <- function(x, lang = NULL) {
  wt <- footer_wt_name(x)
  if (is.null(wt)) return(NULL)
  # the basis is a STORED fact, read through its one resolver -- and derived from the COLUMNS, so the
  # sentence survives every rebuild that keeps them.
  basis <- tryCatch(tab_inference_basis(x), error = function(e) "n")
  # `.svy_weights` is the INTERNAL name of a design's sampling weights and must never be printed. This
  # only fires when a design table's stored inference was lost -> drop the line (missing-metadata
  # contract), never invent a claim about the intervals.
  if (identical(wt, svy_wt_col) && !basis %in% c("design", "design_partial")) return(NULL)
  infer <- isTRUE(tryCatch(tab_shows_inference(x), error = function(e) TRUE))
  with_legend_lang(lang, function(lg) enc2utf8(
    if (!infer) switch(
      basis,
      "design"         = ,
      "design_partial" = gettext("Design-based (survey): weighted estimates."),
      gettextf("Weighted by %s.", wt)
    )
    else switch(
      basis,
      "design" = gettext(
        "Design-based (survey): weighted estimates, intervals and tests account for the sample design."),
      "design_partial" = gettext(
        "Design-based (survey) estimates; this table's design variance could not be computed, so its intervals account for the weighting only."),
      "weights" = gettextf(
        "Weighted by %s; confidence intervals and tests account for the weighting.", wt),
      gettextf("Weighted by %s; confidence intervals and tests use the unweighted sample size.", wt)
    )
  ))
}


# === SECTION: the user page ========================================================================

# THE placeholder table of ?tabxplor-footer, generated from FOOTER_BLOCKS. The "to change it" column
# is DERIVED: a row's `reads` names the facts it is built from, and TAB_ATTRS says which setter writes
# each -- so no setter is named twice, and a renamed one cannot go stale here.
#' @keywords internal
#' @noRd
# ⚠ WRITE A BARE `%` HERE. These lines OPEN with a roxygen tag, so roxygen re-reads them as roxygen
# and escapes the percent itself -- a hand-escaped `\%` comes out `\\%` and prints the backslash
# (test-non-ascii.R locks it). The opposite holds where an @eval block is spliced into an existing
# @param as raw Rd (R/tab-args.R), which must write `\\%`.
footer_blocks_rd <- function() {
  rows <- Filter(function(b) !is.na(b$token), FOOTER_BLOCKS)
  how  <- function(b) {
    st <- unique(stats::na.omit(vapply(sub("^meta[$]", "", b$reads), function(k)
      if (is.null(TAB_ATTRS[[k]])) NA_character_ else TAB_ATTRS[[k]]$setter %||% NA_character_,
      character(1), USE.NAMES = FALSE)))
    if (!length(st)) "" else paste0(" Change it with ",
                                    paste0("\\code{\\link{", st, "}()}", collapse = ", "), ".")
  }
  gloss <- c(
    weight      = "how the table was weighted, and what its intervals and tests rest on.",
    model       = "a regression's family, outcome, predictors and estimand.",
    interaction = "the aggregated effect-modification test.",
    legend      = "the colour legend: what each shade means. \\code{<legend:terse>} / \\code{<legend:prose>} pin the register.",
    stars       = "what each significance star means.",
    breaks      = "just the coloured ladder of a measure, inside a line of your own. \\code{<breaks:over>} / \\code{<breaks:under>} take one side; \\code{<breaks:contrib>} names a measure where several compete.",
    measure     = "what the colours grade, in the words \\code{\\link{set_legend_words}} gives it.",
    ref         = "the baseline it is graded against, as the compact form brackets it (preposition included); \\code{<ref:noun>} gives the bare noun a sentence points at.",
    method      = "how the intervals were computed (\\emph{Wilson score interval, 95% confidence}).",
    cols        = "the names of the columns a measure describes.",
    conf        = "the confidence level, localised.")
  c("@section The placeholders:",
    "A line that IS one of these is built by \\pkg{tabxplor}; a line that merely contains one of the",
    "last three keeps its own words and has that piece substituted into it. Anything else is your",
    "text, printed as written.",
    "\\describe{",
    vapply(names(rows), function(k) paste0(
      "  \\item{\\code{<", rows[[k]]$token, ">}}{", gloss[[k]] %||% "", how(rows[[k]]), "}"),
      character(1), USE.NAMES = FALSE),
    "}")
}

#' The lines under a table
#'
#' @description
#' Everything \pkg{tabxplor} prints beneath a table --- the weight line, the `Model:` line, the
#' colour legend, the significance-stars key, and your own notes --- is **one template**, and the
#' template is the table's `subtext`. `get_subtext()` shows it, [set_subtext()] replaces it:
#'
#' ```
#' t <- tab(forcats::gss_cat, race, marital, pct = "row", color = "diff")
#' get_subtext(t)
#' #> "<legend>" "<stars>"
#' ```
#'
#' Everything generated is a `<placeholder>`; everything you write is a line; **the order of the
#' lines is the order of the footer**. Re-order them and it re-orders. Delete `<legend>` and no
#' colour legend is generated --- in the console too, which no exporter argument can reach.
#'
#' **A table names only what it can say.** The template above has no `<weight>` because the table is
#' unweighted and no `<model>` because it is not a regression --- both are settled when the table is
#' built and nothing can add them afterwards. `<legend>` and `<stars>` are always named: they are
#' built from the columns, which [set_color()] and the \pkg{dplyr} verbs can still change.
#'
#' @section The one rule:
#' A `subtext` naming **no** placeholder is simply appended to the default footer, which is what a
#' note has always done. Writing one placeholder on a line of its own takes the layout over: only
#' what you name is printed. An unknown `<...>` is not a placeholder --- raw html, `"n < 30"` and
#' `"<30 ans>"` pass through verbatim and claim nothing (`\<` escapes a literal `<`).
#'
#' @section What is built when:
#' Every placeholder is resolved **at render**, so a footer follows the `lang =`, the `theme =` and
#' the medium of the call that prints it, and a table edited after it was built (a `select()`, a
#' [set_display()], a [set_color_breaks()]) says the truth about what it now shows. Your own lines
#' are frozen in the language you wrote them --- the default template holds no prose, so nothing
#' mixes unless you write it.
#'
#' A table that has lost its attributes keeps what its **columns** can still say --- the colour
#' legend and the stars key --- and drops the rest.
#'
#' @eval footer_blocks_rd()
#'
#' @seealso [set_subtext()] to edit the template, [tab_footer_text()] to read what it prints,
#'   [set_legend_words()] to re-state what the legend calls a measure, [set_footer_tabs()] and
#'   [tab_note()] for a table or a note under the whole block, [tabxplor-options] for the
#'   session-wide defaults.
#' @name tabxplor-footer
NULL
