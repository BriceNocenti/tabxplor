# PURPOSE: the colour legend -- the SENTENCE that says what a table's colours mean.
# ROLE: one of the three parts of what a table says about itself. The MEASURE's facts are declared in
#   R/fmt_class.R (MEASURES) and read through measure_facts(); this file renders them into words; the
#   REGION those words are printed in -- the order, the media, the notes and subordinate tables --
#   is R/tab-footer.R.
# KEY CONSTRAINTS:
#   - Driven by the SAME per-channel plan (fmt_color_plan) and slot->palette path the CELLS use, so a
#     legend and the cells it describes can never disagree.
#   - Composed at RENDER, never at build: the theme changes the ladder's length and the note's words,
#     the medium chooses terse vs prose, and a post-build select()/set_display() changes what there is
#     to say. See dev/legend_and_side_tables.md section 3.
#   - Every user-visible word is a gettext() call inside a function, so it resolves at render (a
#     top-level gettext() would freeze the build locale) while potools still extracts the literal.
#   - THE WORDS ARE A FOUR-LAYER FOLD, all in measure_facts(): the MEASURES row, its `guar` override
#     (the significance policy), its `by_scale` one (the ladder), and LAST the table's own
#     (set_legend_words(), meta$legend_words). ⚠ Naming only -- a table attribute must never change a
#     number, so MEASURE_WORD_FIELDS admits no engine fact and no ladder glyph, and a re-stated word
#     is DATA (a string, never a closure: a closure in `meta` captures a namespace and breaks
#     saveRDS()). ⚠ A BASELINE word (`ref`, and the two shapes it feeds) is refused on a measure whose
#     reference is a row of the table rather than a concept -- there the legend names what the table
#     shows, so a word could only be stored and ignored.
#   - The gettext SCOPE is opened ONCE per footer (tab_footer_streams()); with_legend_lang() is
#     re-entrant, so the nested calls here are free and every builder answers in the same language.
# See: dev/legend_and_side_tables.md; CLAUDE.md section "The colour system".

# Pipeline (one spec -> two assemblers -> per-medium renderer):
#   legend_specs(x)                         per col_var group: measures / breaks / ref / method /
#                                           policy / shade names / reg effect word.
#   legend_tokens_terse / _prose            a TOKEN stream (plain-text | coloured-break tokens);
#                                           `terse` = compact (console), `prose` = full sentences
#                                           (exports), translated via gettext (domain "R-tabxplor").
#   legend_render_line(tokens, medium)      console ansi (cli) / html text_spec / md pandoc span /
#                                           excel fmt_txt runs / plain.
# The break-word colours come from the engine's per-side slots (over 1:4, under 5:8) indexed into the
# 8-hex palette -- the exact path fmt_channel_codes() / tx_slot_class() use for the cells.
#
# THE PROSE GRAMMAR, one shape for every case (see legend_tokens_prose):
#   [<col names> -- ]<HEAD><LADDER> <NOTE>
#     HEAD    the measure NAMED IN WORDS ("Percentage points (risk) difference:"), which is what a
#             reader needs first. Dropped where the subject already IS the measure (a regression's
#             effect word) or where the measure writes its own lead (the gap measures). Under
#             `guaranteed_effect` it carries the guarantee and names the interval ONCE, both channels.
#     LADDER  per side "<subject> >= <ref> <breaks> <unit>", the two sides joined by ";" -- ONE
#             sentence. Under `guaranteed_effect` they merge into one list after "from <ref>".
#     NOTE    what an UNCOLOURED (or, on a publication palette, UNMARKED) cell means. Only that:
#             "coloured => significant" is a tautology the cells already show.
# THE MEASURE IS NAMED IN TWO REGISTERS, both facts on the MEASURES row: `word` (short) for the
# console and a plot guide, `word_long` (per SCALE, via by_scale) for the export footers -- a
# difference of proportions, of means and of log odds are three quantities, not one word.

# is THIS column the baseline of its own gap measure? A measure whose baseline is another column leaves
# `obs` empty on the column that IS that baseline (the reference group, or a model with no observed
# counterpart), so not one cell can be coloured. Say what the column is instead of printing an
# unreachable ladder. Tested on the STORED `obs` being empty, not the plan's gate (grey_non_signif also
# gates nothing on a comparable column, which must still show its ladder).
#' @keywords internal
legend_gap_baseline <- function(plan, no_obs)
  !is.null(plan) && isTRUE(no_obs) && measure_own_ref(plan$measure)

#' @keywords internal
# WHAT this column IS, said in the ladder's place. A crude companion is not "no observed effect" --
# it IS the observed effect, the baseline the shades beside it are measured from, which is exactly
# what a reader of a crude/adjusted pair needs told once.
legend_gap_baseline_word <- function(plan, spec = NULL) {
  if (identical(MEASURES[[plan$measure]]$ref_kind, "group"))  return(gettext("reference group"))
  if (identical(spec$role, "emp"))
    return(gettext("the observed effect (the reference for the adjustment)"))
  gettext("no observed effect")
}

# a legend token: plain text (c = NA) or a coloured break-word (c = palette slot 1:8). The CSS class is
# derived at render (tx_slot_class), not stored, so a break-word and the cells it describes name the
# same class. `esc` = escape markdown-active `*` in the md medium.
# DESIGN: a BREAK-WORD's face is the palette's and nothing else, so a legend never puts more emphasis
# on itself than the cells it describes carry. `bold` is the one exception and is not a face at all:
# it marks the VARIABLE NAMES a line opens with, which are a label saying whom the sentence is about,
# so a reader picks out the line they need before reading any of them.
.lg_tok  <- function(t, esc = FALSE, bold = FALSE)
  list(t = t, c = NA_integer_, ch = NA_character_, esc = isTRUE(esc), b = isTRUE(bold))
.lg_ctok <- function(t, slot, ch) list(t = t, c = as.integer(slot), ch = ch, esc = FALSE)

#' @keywords internal
legend_resolve_lang <- function(lang = NULL) {
  if (is.null(lang) || identical(lang, "")) lang <- tx_option("lang")
  lang <- tolower(as.character(lang)[1])
  if (lang %in% c("fr", "french", "francais", "fran\u00e7ais")) return("fr")
  if (lang %in% c("en", "english"))                             return("en")
  # auto: prioritise the MESSAGE-language signals (a user running English R on a French Windows must
  # get English), falling back to the character locale only when none is set.
  sources <- c(Sys.getenv("LANGUAGE"), Sys.getlocale("LC_MESSAGES"),
               Sys.getenv("LC_MESSAGES"), Sys.getenv("LANG"), Sys.getenv("LC_ALL"))
  sources <- sources[nzchar(sources)]
  probe   <- if (length(sources)) sources[1] else Sys.getlocale("LC_CTYPE")
  if (grepl("(^|[^a-z])fr|franc", probe, ignore.case = TRUE)) "fr" else "en"
}

# Flush gettext's cache of already-translated strings, so a mid-session LANGUAGE change is honoured.
# glibc caches per (domain, msgid) and only invalidates on setlocale()/bindtextdomain()/textdomain();
# without this, LANGUAGE changes silently no-op on Linux (they happen to work on Windows/macOS).
# The older Sys.setlocale(LC_MESSAGES) trick fails on musl/Alpine (withr#213).
#
# ⚠ IT MUST RE-BIND OUR OWN DOMAIN, not a throwaway one: glibc keys the cache on (domain, msgid), so
# binding some other name leaves "R-tabxplor" cached and the SECOND language switch of a session
# silently no-ops -- one `lang = "fr"` render used to make every later `lang = "en"` one French.
# Rebinding to tempdir() and back to the real catalogue is the invalidation.
#' @keywords internal
flush_gettext_cache <- function() {
  try({
    po <- system.file("po", package = "tabxplor")
    bindtextdomain("R-tabxplor", tempdir())
    if (nzchar(po)) bindtextdomain("R-tabxplor", po)
  }, silent = TRUE)
  invisible(NULL)
}

legend_num <- function(v, lang) {
  s <- trimws(formatC(v, format = "fg", digits = 4, drop0trailing = TRUE))
  if (identical(lang, "fr")) s <- gsub("[.]", ",", s)
  s
}

# a compact reference word for the terse (console) form. The reference-free ("indep") baseline word is
# a per-channel FACT (ref_word, resolved from the policy-aware MEASURES row), because contrib's two
# readings name it differently. `lang` is set by with_legend_lang() in the calling environment, so
# gettext() already answers in the right language.
legend_ref_short <- function(spec) {
  ref <- spec$ref
  switch(ref$kind,
         "tot"      = if (!is.na(ref$label) && nzchar(ref$label)) ref$label else gettext("Total"),
         "level"    = if (!is.na(ref$label) && nzchar(ref$label)) ref$label else gettext("ref."),
         "category" = gettext("ref."),
         "indep"    = if (!is.null(spec$txt$ref_word)) spec$txt$ref_word else gettext("vs the mean"),
         "")
}

legend_break_label <- function(measure, brk, dir, is_pct, lang, policy = "ignore", scale_key = NULL) {
  m <- measure_facts(measure, policy, scale_key)
  if (is.null(m)) return(as.character(brk))
  scale <- if (isTRUE(m$break_scale) && isTRUE(is_pct)) 100 else 1
  # the ladder follows the CELLS: under `ratio_print = "raw"` a multiplicative threshold is written
  # the way the cells write it -- the plain number, and the inverse below the neutral. Only where the
  # two sides have DIFFERENT glyphs: a contribution is direction-free and reads "x2" on both sides.
  if (isTRUE(m$threshold_mult) && !identical(m$break_under, m$break_over) &&
      tx_ratio_print_raw()) {
    v <- abs(brk) * scale
    return(legend_num(if (dir < 0L) 1 / v else v, lang))
  }
  glyph <- if (dir < 0L) m$break_under else m$break_over
  paste0(glyph, legend_num(abs(brk) * scale, lang))
}

legend_break_tokens <- function(plan, is_pct, channel, lang, theme = "light") {
  if (is.null(plan)) return(list(over = list(), under = list()))
  measure <- plan$measure
  # the legend must not promise a distinction the cells do not make: a publication palette can render
  # two slots the same (the default one gives slots 3 and 4 one rendering), so a token whose rendering
  # repeats the previous one is dropped, keeping the LOWER threshold ("bold = at least +5 points"). NOT
  # a cap inside fmt_color_slots() -- the ENGINE stays theme-blind. The key is the WHOLE rendering,
  # marks included, so a palette that separates two slots by their mark alone keeps both break-words.
  fam <- if (identical(channel, "text")) "text" else "bg"
  hex <- get_color_style("color_code", type = fam, theme = theme)
  fc  <- get_color_style("face",       type = fam, theme = theme)
  look <- function(slot) paste(hex[slot], fc$bold[slot], fc$italic[slot], fc$underline[slot],
                               fc$marks[slot])
  mk_side <- function(breaks, slots, dir) {
    prev <- NA_character_
    out  <- list()
    for (l in seq_along(breaks)) {
      slot <- slots[l + 1L]
      lab  <- legend_break_label(measure, breaks[l], dir, is_pct, lang, plan$policy, plan$scale_key)
      if (is.na(slot) || slot == 0L) { out <- c(out, list(.lg_tok(lab))); prev <- NA_character_; next }
      key <- look(slot)
      if (!is.na(prev) && identical(key, prev)) next     # same rendering as the previous break
      prev <- key
      # a marks palette says nothing typographically, so the break-word must WEAR its mark or the
      # legend would list four thresholds that all look alike.
      out  <- c(out, list(.lg_ctok(paste0(lab, fc$marks[slot]), slot, channel)))
    }
    out
  }
  list(over  = mk_side(plan$over_breaks,  plan$over_slots,  +1L),
       under = mk_side(plan$under_breaks, plan$under_slots, -1L))
}

legend_threshold_phrase <- function(plan, is_pct, is_std, lang, words = NULL) {
  if (is.null(plan)) return(NA_character_)
  md   <- measure_facts(plan$measure, plan$policy, plan$scale_key, words)
  # ONE break, written the way the ladder writes it. The x100 rule is legend_break_label()'s, so the
  # grey note and the ladder it describes cannot disagree.
  one  <- function(brk, glyph) {
    if (isTRUE(md$threshold_mult)) return(paste0(glyph, legend_num(abs(brk), lang)))
    sc100 <- isTRUE(md$break_scale) && isTRUE(is_pct)
    val   <- legend_num(abs(brk) * if (sc100) 100 else 1, lang)
    unit  <- legend_unit_word(md, is_pct, is_std)
    if (nzchar(unit)) paste0(glyph, val, " ", unit) else paste0(glyph, val)
  }
  pick <- function(v) if (length(v) == 0L || is.na(v[[1]])) NA_real_ else v[[1]]
  o <- pick(plan$over_breaks); u <- pick(plan$under_breaks)
  if (is.na(o) && is.na(u)) return(NA_character_)
  if (is.na(o) || is.na(u) || isTRUE(all.equal(o, u)))
    one(if (is.na(o)) u else o, if (isTRUE(md$threshold_mult)) .lg_times else "\u00b1")
  else
    # an ASYMMETRIC ladder enters at a different rung on each side, so the note must name both.
    paste0(one(o, md$break_over), " / ", one(u, md$break_under))
}

# `"diff"` consults the column kind (factor pct vs standardized numeric); the gap scales DECLARE their
# unit, keeping them clear of `is_std`.
# A unit a TABLE re-stated wins over the shared kind map (set_legend_words(unit_word =)).
legend_unit_word <- function(md, is_pct, is_std) {
  uw <- measure_word_of(md$unit_word)
  if (!is.null(uw)) return(uw)
  switch(
    md$unit_kind,
    "diff"    = if (isTRUE(is_pct)) gettext("points") else if (isTRUE(is_std)) gettext("SD") else "",
    "points"  = gettext("points"),
    "std"     = gettext("SD"),
    "contrib" = gettext("the mean contribution"),
    "")
}

# THE lead a measure states its ladder with: the one a TABLE re-stated (`lead_over` / `lead_under`,
# templates taking `%1$s` the subject, `%2$s` the reference, `%3$s` the null), else the measure's own
# declared closure, else NULL for the generic "<subject> >= <reference>".
# ⚠ the DECLARED leads stay closures on purpose: each writes ONE WHOLE SENTENCE per case, which is
#   what a French participle needs and what no shared template can do.
legend_lead_fn <- function(md) {
  ov <- md$lead_over; un <- md$lead_under
  if (is.null(ov) && is.null(un)) return(md$lead)
  function(subject, ref, dir, neutral = NA_character_) {
    tpl <- if (dir > 0) ov else un
    if (is.null(tpl)) return(if (is.null(md$lead)) NULL else md$lead(subject, ref, dir, neutral))
    measure_word_of(tpl, subject,
                    if (is.null(ref) || is.na(ref)) "" else ref,
                    if (is.na(neutral)) "" else neutral)
  }
}

legend_join <- function(toks, sep) {
  if (length(toks) == 0) return(list())
  out <- list(toks[[1]])
  for (i in seq_along(toks)[-1]) out <- c(out, list(.lg_tok(sep)), list(toks[[i]]))
  out
}

# The word a palette gives each DIRECTION, or NA where it gives none.
#
# DESIGN: a COLOUR palette names none. Its two directions are a diverging ramp, and every medium now
# renders it -- the break-words in the legend are themselves blue and red, so "Shades of blue:" said
# in words what the words already looked like. A PUBLICATION palette is the opposite case: greyscale
# collapses the diverging ramp, direction lives in the face alone, and the two sides genuinely need
# naming ("Underlined:" / "Italic:") -- which is why those are the only legends still built as two
# sentences. NA on a side the palette does not name typographically (the emphasis palette's over
# side, both sides of the marks one).
# One pair PER CHANNEL: the background side is a grey fill in every publication palette, so a
# background-only column must not announce "Underlined:" about fills.
legend_shade_names <- function(theme = "light") {
  pal <- print_palette_of(tx_palette_theme(theme))
  if (is.null(pal)) return(list(text = c(over = NA_character_, under = NA_character_),
                                bg   = c(over = NA_character_, under = NA_character_)))
  nm <- function(f) if (is.null(f)) NA_character_ else f()
  list(text = c(over = nm(pal$shade$over), under = nm(pal$shade$under)),
       bg   = c(over = gettext("Grey fill"), under = gettext("Grey fill")))
}

# THE word the colour legend names a regression column by.
#
# ⚠ it is the MEASURE, never the contrast: reg_legend_word() drops the `m` / `ref` marker on purpose,
# because legend_group_by_body() groups columns by their rendered sentence and a crude column reading
# "RR" beside a model column reading "mRR" would split the one block the crude/adjusted merge exists
# to produce. The legend describes the ladder (the measure's); the header and the "Model:" line
# describe the estimand.
legend_reg_eff_word <- function(col, meta) {
  # the column's OWN family (the `model_family` attr), so a mixed table names each column correctly;
  # fall back to the table's scalar family when unset. `effect` / `measure` stay table-level.
  fam <- get_model_family(col); if (!nzchar(fam)) fam <- meta$family
  est <- reg_meta_estimand(meta, family = fam)
  # ⚠ A CRUDE column is named from ITS OWN SHAPE, not from the model's estimand: the two are the same
  # measure wherever they pair (so the block merges), and where they do not -- a poisson AME beside a
  # crude rate ratio -- the crude column must say what it actually holds.
  if (identical(get_role(col), "emp")) {
    ck <- reg_meta_crude_key(meta, fam)
    return(reg_crude_word(reg_crude_shape(ck, est)) %||% NA_character_)
  }
  if (!identical(get_role(col), "model")) return(NA_character_)   # an `n` column names no effect
  # the model column's measure, marker dropped (reg_legend_word). An unnamed additive one falls
  # through to the ladder's own word ("difference"), which reads better than an abbreviation would.
  reg_legend_word(est)
}

# The crude BLOCK a column belongs to, from the table's own record -- the key reg_crude_shape() needs
# to name a crude column. `crude_keys` is stored per outcome; a mixed table finds its own.
#' @keywords internal
reg_meta_crude_key <- function(meta, family = NULL) {
  ck <- meta$crude_keys
  if (is.null(ck) || !length(ck)) return(NA_character_)
  ck <- unlist(ck)
  if (!is.null(family) && nzchar(family)) {
    fk   <- unname(REG_FIT_FAMILY[family]); if (is.na(fk)) fk <- family
    fams <- meta$families %||% meta$family
    hit  <- names(fams)[fams %in% c(family, fk)]
    if (length(hit) && hit[[1]] %in% names(ck)) return(unname(ck[[hit[[1]]]]))
  }
  unname(ck[[1]])
}

legend_ref_label <- function(x, col, orientation) {
  tryCatch({
    if (identical(orientation, "col")) {
      idx <- which(purrr::map_lgl(x, ~ is_fmt(.) && isTRUE(is_refcol(.))))
      if (length(idx) == 0) return(NA_character_)
      nm <- names(x)[idx[[1]]]
      if (isTRUE(is_totcol(x[[idx[[1]]]]))) NA_character_ else nm   # a total column (by stored attr) -> generic "Total"
    } else {
      rv <- tab_get_vars(x)$row_var
      if (is.null(rv) || length(rv) == 0 || is.na(rv)) return(NA_character_)
      idx <- which(is_refrow(col))                           # the marked reference row(s) only
      if (length(idx) == 0) return(NA_character_)
      labs <- unique(as.character(x[[rv]][idx]))
      if (length(labs) == 1) labs else NA_character_          # ambiguous across subtables -> generic
    }
  }, error = function(e) NA_character_)
}

# legend_tottab_label() -- the name of the TOTAL TABLE, for a column that compares against it
# (`comp = "all"`). Composed from the two declared total names, never read off a row label: after a
# spread the total table IS a column block and has no row of its own to name it.
#' @keywords internal
#' @noRd
legend_tottab_label <- function(x) {
  tn <- tab_total_names()
  g  <- tryCatch({
    idx <- which(purrr::map_lgl(x, ~ is_fmt(.) && nzchar(get_col_group(.)) && all(is_tottab(.))))
    if (length(idx)) get_col_group(x[[idx[[1]]]]) else NA_character_
  }, error = function(e) NA_character_)
  if (is.na(g) || !nzchar(g)) g <- unname(tn[["tab"]])
  paste(unname(tn[["row"]]), g)
}

legend_ref_info <- function(x, col, measure, orientation, is_coef = FALSE, is_reg = FALSE,
                            policy = "ignore") {
  base_kind <- measure_facts(measure, policy)$ref_kind  # the measure's baseline concept, one field
  if (identical(base_kind, "indep"))
    return(list(kind = "indep", label = NA_character_, orientation = orientation))
  # these two baselines are NEITHER a total nor a predictor's reference category -- they are another
  # COLUMN's estimate (the observed effect, or the reference group's). Resolve BEFORE the is_reg branch,
  # which would otherwise claim "the reference category" and describe the wrong comparison.
  if (base_kind %in% c("observed", "group"))
    return(list(kind = base_kind, label = NA_character_, orientation = "row"))
  # a regression table has no total row -- every reg column is compared to the predictor's REFERENCE CATEGORY.
  if (isTRUE(is_reg) || identical(base_kind, "category") || isTRUE(is_coef))
    return(list(kind = "category", label = legend_ref_label(x, col, "row"), orientation = "row"))
  ref <- get_ref_type(col); ref <- if (length(ref)) as.character(ref)[1] else "tot"
  # DESIGN: `comp = "all"` moves the baseline from the sub-table's own total to the TOTAL TABLE's --
  # the whole point of the argument, and the legend used to print "Total" for both. Naming it is
  # what makes a spread table readable: the reference is one CELL, in the total-table block.
  if (identical(ref, "tot"))
    list(kind = "tot",
         label = if (isTRUE(get_comp_all(col))) legend_tottab_label(x) else NA_character_,
         orientation = orientation)
  else
    list(kind = "level", label = legend_ref_label(x, col, orientation), orientation = orientation)
}

# THE reference, in the FORM the sentence position needs. A legend line names its baseline three or
# four times, so a long phrase said in full each time buries the numbers:
#   "full"   the first naming in a line -- everything the reader must be told once
#   "short"  every later naming in the same line
#   "plain"  inside the NOTE, where a parenthetical follows: a phrase ending in "(...)" would give
#            two brackets in a row ("...the reference category (in bold) (Newcombe...)")
# A short reference that is already short (a Total row) is the same string in all three.
LEGEND_REF_FORMS <- c("full", "short", "plain")
legend_ref_phrase <- function(spec, form = "full") {
  ref <- spec$ref
  lab <- ref$label
  # "" as the short form: a second naming would only repeat what the first side already said.
  # The NOUN a prose lead points at is the measure's own declared `ref_phrase` (the terse form takes
  # `ref_word` instead, which carries its preposition) -- so a table that re-states one re-states both.
  if (identical(ref$kind, "indep"))
    return(if (identical(form, "short")) ""
           else spec$txt$ref_phrase %||% spec$bg$ref_phrase %||% gettext("independence"))
  # the gap LEAD points at the column beside this one; the grey NOTE names the quantity tested.
  if (identical(ref$kind, "observed"))
    return(if (identical(form, "plain")) gettext("the observed effect")
           else gettext("the observed column"))
  # "...'s effect", not just "the reference group": what differs is the EFFECT, not the group.
  if (identical(ref$kind, "group"))    return(gettext("the reference group's effect"))
  # DESIGN: `ref != "tot"` says ONE thing, whatever level was picked and whether or not it resolves.
  # A merged table has one reference row PER sub-table, so legend_ref_label() returns NA there and the
  # phrase used to fall back to the literal "Total" -- describing a comparison the table never made.
  # The level is already visible (it is the bold row), and "the reference category (in bold)" is word
  # for word what tab_stars_legend() says, so the two footer lines name one thing once.
  if (ref$kind %in% c("category", "level"))
    return(switch(form,
                  short = gettext("ref"),
                  plain = gettext("the reference category"),
                  gettext("the reference category (in bold)")))
  base <- if (identical(ref$orientation, "col")) gettext("column") else gettext("row")
  if (is.na(lab) || !nzchar(lab)) lab <- gettext("Total")
  gettextf("the %s %s", lab, base)                 # EN "the Total row"; FR "la %2$s %1$s" -> "la ligne Total"
}

# the CI-method name (NA when there is none, e.g. contrib). `measure` is passed per channel: a gap
# measure on the background names ITS OWN test.
# THE interval-method labels: one row per engine, keyed on the `ci_method` attribute the producer
# stamps. Each entry is a FUNCTION so gettext() runs at render.
# WARNING: the keys are the values the producers write (tab_ci(), the two leaves, reg_column(),
# emp_col()) -- adding an engine means adding a row here; a missing row degrades to the generic phrase.
#' @keywords internal
CI_METHOD_LABELS <- list(
  wilson       = function() gettext("Wilson score interval"),
  wald         = function() gettext("Wald interval"),
  beta         = function() gettext("Korn-Graubard (beta) interval"),
  newcombe     = function() gettext("Newcombe score interval"),
  ac           = function() gettext("Wald interval with Agresti-Caffo adjustment"),
  welch        = function() gettext("Welch t interval"),
  student      = function() gettext("Student t interval"),
  ols          = function() gettext("Student t interval, pooled over the variable's levels"),
  woolf        = function() gettext("Wald interval on the log odds-ratio"),
  katz         = function() gettext("Wald interval on the log risk-ratio"),
  quasipoisson = function() gettext("quasi-Poisson interval"),
  robust       = function() gettext("robust-Poisson (delta) interval"),
  poisson      = function() gettext("Poisson interval"),
  profile      = function() gettext("profile-likelihood interval")
  # `katz` and `wald_log` live in CI_METHOD_WORDED below: their label needs the effect word.
)

# CI_METHOD_WORDED -- the engine whose LABEL needs a second fact. An OR, an IRR and an RR are the same
# interval on the same log scale, and only the effect WORD tells them apart, so the MODEL's engine is
# the one that has to ask. Every other engine names itself in CI_METHOD_LABELS.
#' @keywords internal
CI_METHOD_WORDED <- list(
  wald_log = list(IRR      = function() gettext("Wald interval on the log rate-ratio"),
                  OR       = function() gettext("Wald interval on the log odds-ratio"),
                  RR       = function() gettext("Wald interval on the log risk-ratio"),
                  .default = function() gettext("Wald interval on the log scale"))
)

# The engines that are a CLOSED FORM of the interval a MODEL column would fit. A crude column
# evaluates one of these instead of fitting, and each reproduces the univariable model's own interval:
# Woolf's 3e-13, Katz's 8e-09, the pooled OLS 2e-14, the dispersion one 5e-09, and the per-group
# (Welch / robust) forms the SANDWICH a design-based or over-dispersed fit reports -- 3e-03 on a
# weighted mean difference, 1.5e-03 on a poisson marginal effect.
#
# DESIGN: a label names the ESTIMAND, not the engine, so on a REGRESSION column these render as the
# interval the model column beside them renders (legend_method_name()) -- otherwise one arithmetic
# gets two legend blocks. The closed form is named once on the merged block instead, which is where a
# reader can act on it. On a plain `tab()` column there is no model twin and each names itself.
#' @keywords internal
CI_METHOD_CLOSED_FORM <- c(woolf = "Woolf", katz = "Katz", quasipoisson = "quasi-Poisson",
                           ols = "pooled OLS", welch = "Welch", robust = "robust")
# NOTE: potools extracts the closures above by static analysis (a gettext() literal inside a closure
# body is statically visible), so no `if (FALSE)` anchor is needed here. Contrast REG_CHECKS
# (R/reg-assumptions.R), whose nouns are BARE STRINGS gettext()ed dynamically -- its anchor is load-bearing.

legend_method_name <- function(spec, measure = spec$measure_text) {
  # a measure that does NOT read the column's own stored interval declares its own `method` -- NA for
  # contrib (no interval), one sentence each for the two gap measures (their SEs come from different
  # mathematics: `between_groups` compares DISJOINT subpopulations -> independent, quadrature exact;
  # `adjustment` compares two estimates on the SAME rows -> the difference of their influence functions).
  if (!is.null(measure) && !is.na(measure) && measure %in% names(MEASURES)) {
    md <- MEASURES[[measure]]
    if ("method" %in% names(md)) return(if (is.function(md$method)) md$method() else NA_character_)
  }
  m <- spec$ci_method
  if (is.null(m) || is.na(m) || !nzchar(m)) return(NA_character_)
  worded <- function(engine) {
    wd <- CI_METHOD_WORDED[[engine]]
    w  <- spec$eff_word; if (is.null(w) || is.na(w)) w <- ""
    (wd[[w]] %||% wd[[".default"]])()
  }
  # D23: on a REGRESSION column a closed form renders the interval its model twin renders, because it
  # IS that interval. Which twin depends only on the column's own scale: a multiplicative estimand is
  # a Wald interval on the log of it, everything else the plain Wald one.
  if (isTRUE(spec$is_reg) && m %in% names(CI_METHOD_CLOSED_FORM))
    return(if (isTRUE(EST_SCALES[[spec$scale]]$mult)) worded("wald_log") else CI_METHOD_LABELS$wald())
  if (!is.null(CI_METHOD_WORDED[[m]])) return(worded(m))
  lab <- CI_METHOD_LABELS[[m]]
  if (is.null(lab)) gettext("confidence interval") else lab()
}

# GATED on the basis: an unweighted / weights-only table refers to z and must not grow a "design df"
# clause that says nothing; only a real survey design gains it (df = t(#PSU - #strata), not z).
#
# ⚠ AND GATED OFF A REGRESSION. There the df is per COLUMN -- a model column and its crude twin are
# fitted on different numbers of parameters -- while this phrase is part of the legend's GROUPING
# key, so naming a number here would split the one crude/adjusted block the pair exists to form. A
# regression states its reference distribution once per model, in the "Model:" footer line instead.
legend_method_phrase <- function(spec, lang, measure = spec$measure_text) {
  conf <- gettextf("%s%% confidence", legend_num(spec$conf_level * 100, lang))
  df   <- spec$degf
  if (!isTRUE(spec$is_reg) && isTRUE(spec$basis %in% c("design", "design_partial")) &&
      !is.null(df) && length(df) == 1L && is.finite(df) && df > 0)
    conf <- gettextf("%s, %s design df", conf, legend_num(df, lang))
  m    <- legend_method_name(spec, measure)
  if (is.na(m)) conf else gettextf("%s, %s", m, conf)
}

# ...and THE reader of that phrase for a whole table: the distinct clauses its coloured columns state,
# optionally narrowed to a set of them. Two callers -- the `<method>` footer token and forest_plot()'s
# caption -- so neither reaches into legend_specs() on its own.
# ⚠ `lang` formats the numbers; the gettext SCOPE belongs to the caller (tab_footer_streams() hoists
# one for the whole footer, fp_caption() opens its own).
#' @keywords internal
#' @noRd
legend_method_phrases <- function(x, cols = NULL, lang = NULL) {
  sp <- tryCatch(legend_specs(x), error = function(e) list())
  if (!is.null(cols)) sp <- Filter(function(s) s$col_name %in% cols, sp)
  if (!length(sp)) return(character(0))
  lg <- legend_resolve_lang(lang)
  sp <- lapply(sp, function(s) legend_resolve_spec(s, lg))
  ph <- unique(stats::na.omit(vapply(sp, function(s) s$method_phrase %||% NA_character_,
                                     character(1))))
  ph[nzchar(ph)]
}

# THE measure's name, in the register the medium can afford: `long = TRUE` (the export footers) gives
# the discipline's term and the base measure together, `long = FALSE` (the console, a plot guide) the
# short word. Both are read through measure_facts(), so the SCALE the ladder is on chooses the name --
# a difference of proportions, of means and of log odds are three quantities, not one word.
legend_measure_word <- function(measure, is_std, eff_word, policy = "ignore",
                                scale_key = NULL, long = FALSE, words = NULL) {
  # an SD-scaled ladder prints bare numbers (`-0.8 -0.4 -0.2 -0.1`) that are not in the outcome's own
  # units, so the name has to carry the unit -- once, before any of them, rather than only in the
  # trailing grey clause where a reader meets it after the numbers.
  if (!is.na(eff_word) && !measure_own_ref(measure))
    return(if (isTRUE(is_std) && identical(measure, "difference")) gettextf("%s in SD", eff_word)
           else eff_word)
  m <- measure_facts(measure, policy, scale_key, words)
  if (is.null(m)) return(measure)
  # fall back leftwards, so a scale that declares nothing still answers with the measure's own word.
  w <- NULL
  if (isTRUE(long) && isTRUE(is_std)) w <- m$word_long_std
  if (is.null(w) && isTRUE(long))     w <- m$word_long
  if (is.null(w) && isTRUE(is_std))   w <- m$word_std
  if (is.null(w))                     w <- m$word
  # `word` is a CLOSURE, so gettext() runs at render (never at build, which would freeze the locale)
  # AND its literal is visible to potools' static extraction. A non-translated word is function() "OR".
  # A word a TABLE re-stated is a plain string instead -- measure_word_of() takes both.
  measure_word_of(w)
}

legend_ucfirst <- function(s) {
  if (!nzchar(s)) return(s)
  paste0(toupper(substr(s, 1, 1)), substr(s, 2, nchar(s)))
}

# pre-compute EVERY per-measure / per-channel display fact into the spec ONCE, so the token assemblers
# below are dumb templates (no switch(measure), no is_reg/is_coef branch). Per-channel facts resolve for
# BOTH channels into spec$txt / spec$bg.
legend_resolve_spec <- function(spec, lang) {
  # each channel resolves its facts under ITS OWN policy. `spec$policy` is the text channel's, and since
  # a gap measure's force_policy is a per-column predicate the two channels can genuinely differ.
  chan <- function(measure, policy = spec$policy, scale_key = NULL) {
    if (is.na(measure)) return(NULL)
    if (is.null(policy)) policy <- spec$policy
    md   <- measure_facts(measure, policy, scale_key, spec$words)
    subj <- if (!is.na(spec$eff_word)) spec$eff_word
            else if (!is.null(md$subject)) measure_word_of(md$subject) else gettext("cell")
    u    <- legend_unit_word(md, spec$is_pct, spec$is_std)
    unit <- if (nzchar(u)) paste0(" ", u) else ""
    # `adjustment` / `between_groups` compare to ANOTHER COLUMN's estimate, so the reference is a
    # per-CHANNEL fact -- the scalar spec$ref_phrase would describe the wrong comparison on the background.
    own_ref <- measure_own_ref(measure)
    own_ref_phrase <- function(form) if (!own_ref) NA_character_ else
      legend_ref_phrase(list(ref = list(kind = md$ref_kind, label = NA_character_)), form)
    list(subject      = subj,
         ref_lead     = own_ref_phrase("full"),
         ref_short    = own_ref_phrase("short"),
         ref_note     = own_ref_phrase("plain"),
         # the measure NAMED IN WORDS, and the interval's bare name -- what the prose head is built of.
         word_long    = legend_measure_word(measure, spec$is_std, spec$eff_word, policy,
                                            scale_key, long = TRUE, words = spec$words),
         word_guar    = if (isTRUE(spec$is_std) && is.function(md$word_guar_std)) md$word_guar_std
                        else md$word_guar,
         method_name  = legend_method_name(spec, measure),
         has_ref_lead = own_ref ||
           (isTRUE(md$has_ref_lead) && !isTRUE(spec$is_coef) && !isTRUE(spec$is_reg)),
         # under `guaranteed_effect` this measure's breaks are ABSOLUTE thresholds (contrib's residual),
         # not a CI floor -- so the sentence must not say "after subtracting the margin of error".
         guar_abs     = identical(md$break_origin, "threshold"),
         # ONE noun, two shapes: `ref` is the baseline a table re-states, `ref_word` brackets it with
         # its preposition and `ref_phrase` is the bare noun a prose lead points at. Declaring `ref`
         # alone is the common case, and the preposition stays translated at RENDER.
         ref_word     = measure_word_of(md$ref_word) %||%
                        (if (!is.null(md$ref)) gettextf("vs %s", measure_word_of(md$ref))) %||%
                        gettext("vs independence"),
         ref_phrase   = measure_word_of(md$ref_phrase) %||% measure_word_of(md$ref) %||%
                        gettext("independence"),
         # the interval NAME is per channel: a gap measure on the background runs its own test, so the
         # tail must not borrow the text channel's model interval.
         method_phrase = legend_method_phrase(spec, lang, measure),
         # a measure the generic "<subject> >= <reference>" lead would mis-state writes its own.
         # ⚠ the DECLARED lead stays a closure (one whole sentence per case, which is what French
         #   agreement needs); `lead_over` / `lead_under` are the templates a TABLE may re-state it
         #   with, taking `%1$s` the subject, `%2$s` the reference and `%3$s` the null.
         lead_fn      = legend_lead_fn(md),
         policy       = policy,
         unit         = unit)
  }
  spec$txt <- chan(spec$measure_text, spec$plan_txt$policy, spec$plan_txt$scale_key)
  spec$bg  <- chan(spec$measure_bg,   spec$plan_bg$policy,  spec$plan_bg$scale_key)
  spec$ref_phrase       <- legend_ref_phrase(spec, "full")
  spec$ref_short        <- legend_ref_phrase(spec, "short")
  spec$ref_plain        <- legend_ref_phrase(spec, "plain")
  spec$method_phrase    <- legend_method_phrase(spec, lang)
  spec$conf_pct         <- legend_num(spec$conf_level * 100, lang)
  # the null a GAP is measured from, as the reader sees it in the column (1 on a ratio, 0 on a
  # difference). NA where the scale declares none -- fmt_gap_lead() then says "no effect" alone.
  nt <- EST_SCALES[[spec$scale %||% ""]]$neutral
  spec$neutral          <- if (is.null(nt) || is.na(nt)) NA_character_ else legend_num(nt, lang)
  primary <- if (is.null(spec$plan_txt)) spec$plan_bg else spec$plan_txt
  spec$threshold_phrase <- legend_threshold_phrase(primary, spec$is_pct, spec$is_std, lang, spec$words)
  spec
}

# ---- assemblers: spec -> token stream (dumb templates over legend_resolve_spec() fields) ------------

legend_tokens_terse <- function(spec, lang, show_names) {
  colon <- if (identical(lang, "fr")) " : " else ": "
  toks <- list()
  # `esc = TRUE`: a COLUMN NAME is data -- a money level ("1-Lt $10000") or a starred one would
  # otherwise reach pandoc as inline math / emphasis.
  if (show_names) toks <- c(toks, list(.lg_tok(legend_name_list(spec$col_names), esc = TRUE,
                                                bold = TRUE),
                                       .lg_tok(colon)))
  rs <- legend_ref_short(spec)
  add_channel <- function(plan, prefix, is_bg) {
    if (legend_gap_baseline(plan, spec$no_obs))
      return(list(.lg_tok(paste0(prefix,
                                 legend_measure_word(plan$measure, spec$is_std, spec$eff_word,
                                                     plan$policy, plan$scale_key,
                                                     words = spec$words),
                                 colon, legend_gap_baseline_word(plan, spec)))))
    mw <- legend_measure_word(plan$measure, spec$is_std, spec$eff_word, plan$policy, plan$scale_key,
                              words = spec$words)
    bt <- legend_break_tokens(plan, spec$is_pct, if (is_bg) "bg" else "text", lang,
                             spec$theme %||% "light")
    seq_toks <- c(rev(bt$under), bt$over)
    lbl <- paste0(prefix, mw, if (!is_bg && nzchar(rs)) paste0(" (", rs, ")") else "", colon)
    c(list(.lg_tok(lbl)), legend_join(seq_toks, " "))
  }
  if (!is.null(spec$plan_txt)) toks <- c(toks, add_channel(spec$plan_txt, "", FALSE))
  if (!is.null(spec$plan_bg))  toks <- c(toks, list(.lg_tok(if (identical(lang, "fr")) " ; " else "; ")),
                                         add_channel(spec$plan_bg, paste0(gettext("bg"), " "), TRUE))
  # grey_non_signif names the first threshold a cell must reach: a grey cell is EITHER not significant
  # OR below that threshold (the guarantee is only coloured => significant).
  thr <- spec$threshold_phrase
  # "or not tested" only where some rows genuinely carry no test (partial_test).
  untested <- if (isTRUE(spec$partial_test)) paste0(", ", gettext("or not tested")) else ""
  pn <- switch(spec$policy,
               "grey_non_signif"   = if (!is.na(thr))
                                       paste0(gettextf("grey: non-significant or under %s", thr), untested)
                                     else paste0(gettext("grey: non-significant or small"), untested),
               # "error-adjusted" describes a CI floor; the absolute-threshold reading (contrib's
               # residual) subtracts nothing -- the breaks ARE the quantity.
               "guaranteed_effect" = if (isTRUE(spec$txt$guar_abs))
                                       gettext("all that is significant is colored")
                                     else gettext("all that is significant is colored, error-adjusted"),
               "")
  if (nzchar(pn)) toks <- c(toks, list(.lg_tok(paste0(" [", pn, "]"))))
  toks
}

# THE export legend, one grammar for every case:
#
#   [<col names> -- ]<HEAD><LADDER> <NOTE>
#
#   HEAD    "<Measure>: "  -- the measure NAMED IN WORDS, which is what a reader needs first and what
#           the old palette-led form ("Shades of blue:") never said. Dropped where the subject IS the
#           measure (a regression column's own effect word) or where the measure writes its own lead
#           (the two gap measures), so no line names one thing twice. Under `guaranteed_effect` it
#           carries the guarantee and names the interval ONCE, for both channels.
#   LADDER  per side "<subject> >= <ref> <breaks> <unit>", the two sides joined by ";" -- one
#           sentence, not two. Under `guaranteed_effect` they merge into ONE list after "from <ref>",
#           since both sides then read off the same interval floor.
#   NOTE    what an UNCOLOURED cell means. Only that: "coloured => significant" is a tautology the
#           reader can see. A publication palette says "Unmarked" for the same fact.
#
# A palette that NAMES its directions (the publication ones -- greyscale has no diverging ramp) is the
# one exception: its two sides stay two sentences, led by the face word.
legend_tokens_prose <- function(spec, lang, show_names) {
  # French typography: a (thin) space before the high punctuation ; : (matches the user's examples).
  semi  <- if (identical(lang, "fr")) " ; " else "; "
  colon <- if (identical(lang, "fr")) " : " else ": "
  mark  <- tx_is_print(tx_palette_theme(spec$theme))

  # ---- one side of one ladder: "<lead> <b1>; <b2>; ... <unit>" --------------------------------
  side_tokens <- function(plan, dir, is_bg, lead) {
    bt   <- legend_break_tokens(plan, spec$is_pct, if (is_bg) "bg" else "text", lang,
                                spec$theme %||% "light")
    side <- if (dir > 0) bt$over else bt$under
    if (length(side) == 0) return(NULL)
    cf <- if (is_bg) spec$bg else spec$txt
    c(list(.lg_tok(paste0(lead, " "))), legend_join(side, semi), list(.lg_tok(cf$unit)))
  }

  # THE REFERENCE IS NAMED IN FULL ONCE PER LINE, then short. A line names its baseline three or four
  # times, and "the reference category (in bold)" said four times buries the thresholds it is there to
  # frame. Line-level, not channel-level: the background channel continues the same sentence.
  named_ref <- FALSE
  ref_of <- function(cf) {
    out <- if (named_ref) { if (!is.na(cf$ref_short)) cf$ref_short else spec$ref_short }
           else           { if (!is.na(cf$ref_lead))  cf$ref_lead  else spec$ref_phrase }
    named_ref <<- TRUE
    out
  }

  # ---- one channel: head + ladder ---------------------------------------------------------------
  channel_tokens <- function(plan, is_bg, with_shades) {
    if (is.null(plan)) return(NULL)
    cf   <- if (is_bg) spec$bg else spec$txt
    guar <- identical(cf$policy, "guaranteed_effect")
    # the baseline column of a gap measure: one clause, no ladder. The measure is named by the ladder
    # beside it, so this states only WHAT the column is -- but on the background channel it must still
    # say which channel it is talking about.
    if (legend_gap_baseline(plan, spec$no_obs)) {
      w <- legend_gap_baseline_word(plan, spec)
      return(list(.lg_tok(if (is_bg) paste0(gettext("Background colour"), colon, w, ".")
                          else       paste0(legend_ucfirst(w), "."))))
    }
    # merge the two sides into one list only where they differ by the SIGN alone: a measure with its
    # own lead says something different on each side ("further from" / "closer to"), and one without
    # a reference has no "from <ref>" to hang the merged list on.
    merged <- guar && !isTRUE(cf$guar_abs) && isTRUE(cf$has_ref_lead) && is.null(cf$lead_fn)
    sh     <- if (with_shades) spec$shades[[if (is_bg) "bg" else "text"]]
              else c(over = NA_character_, under = NA_character_)

    # -- head
    head_txt <- ""
    # a REGRESSION column already names its measure in the subject -- its effect word IS the acronym
    # the header prints -- so a head would say the same thing twice. Everything else takes one,
    # including a measure that writes its own lead: `contrib`'s lead states a DIRECTION, not a name.
    if (is.na(spec$eff_word)) {
      w <- cf$word_long
      if (guar && !isTRUE(cf$guar_abs)) {
        # ONE msgid per measure, not "%s-guaranteed %s": in French the participle agrees with the
        # measure (*differance garantie* vs *rapport garanti*), which a shared template cannot do.
        w <- if (!is.null(cf$word_guar)) measure_word_of(cf$word_guar, spec$conf_pct)
             else gettextf("%s%%-guaranteed %s", spec$conf_pct, w)
        if (!is_bg && !is.na(cf$method_name)) w <- gettextf("%s (%s floor)", w, cf$method_name)
      }
      head_txt <- if (is_bg) gettextf("Background colour, %s", w) else legend_ucfirst(w)
    } else if (is_bg) {
      head_txt <- gettext("Background colour")
    }

    # -- ladder
    if (merged) {
      # NO colon between this head and its ladder: the guarantee reads as ONE sentence
      # ("95%-guaranteed <measure> (<method> floor) from the Total row +0; ..."); a colon cuts it in
      # two. Everywhere else the head is a LABEL and keeps its colon.
      lead <- gettextf("from %s", ref_of(cf))
      if (!nzchar(head_txt)) lead <- legend_ucfirst(lead)
      bt   <- legend_break_tokens(plan, spec$is_pct, if (is_bg) "bg" else "text", lang,
                                  spec$theme %||% "light")
      both <- c(bt$over, bt$under)
      if (length(both) == 0) return(NULL)
      body <- c(list(.lg_tok(paste0(lead, " "))), legend_join(both, semi), list(.lg_tok(cf$unit)))
      return(c(if (nzchar(head_txt)) list(.lg_tok(paste0(head_txt, " "))), body, list(.lg_tok("."))))
    }
    # a named face makes the two sides two SENTENCES (the face is what tells them apart); otherwise
    # they are one sentence with a ";". A palette may name ONE side only (print_emphasis): the other
    # then opens its own sentence and must be capitalised like one.
    named <- !is.na(sh[["over"]]) || !is.na(sh[["under"]])
    one <- function(dir) {
      cmp   <- if (dir > 0) .lg_ge else .lg_le
      rp    <- ref_of(cf)
      lead  <- if (!is.null(cf$lead_fn)) cf$lead_fn(cf$subject, rp, dir, spec$neutral)
               else if (cf$has_ref_lead) gettextf("%s %s %s", cf$subject, cmp, rp)
               else                      gettextf("%s %s", cf$subject, cmp)
      shade <- if (dir > 0) sh[["over"]] else sh[["under"]]
      if (!is.na(shade)) lead <- paste0(shade, colon, lead)
      # ⚠ AN ACRONYM IS DATA, NEVER PROSE: capitalising it printed "CumOR" / "Diff" where the header
      # says `cumOR` / `diff`. Only the generic subject ("cell") ever opens a sentence.
      else if (is.na(spec$eff_word) && (named || !nzchar(head_txt))) lead <- legend_ucfirst(lead)
      side_tokens(plan, dir, is_bg, lead)
    }
    ov <- one(+1L); un <- one(-1L)
    if (is.null(ov) && is.null(un)) return(NULL)
    sep   <- if (named) list(.lg_tok(". ")) else list(.lg_tok(semi))
    body  <- if (is.null(ov)) un else if (is.null(un)) ov else c(ov, sep, un)
    head_tok <- if (!nzchar(head_txt)) NULL
                else if (named) list(.lg_tok(paste0(head_txt, ". ")))
                else            list(.lg_tok(paste0(head_txt, colon)))
    c(head_tok, body, list(.lg_tok(".")))
  }

  toks <- list()
  if (show_names)  # `esc` keeps a variable name DATA (see terse); it is BOLD (see .lg_tok).
    toks <- c(toks, list(.lg_tok(legend_name_list(spec$col_names), esc = TRUE, bold = TRUE),
                         .lg_tok(" \u2014 ")))

  # a measure may declare ONE sentence of honesty about itself (MEASURES$<m>$caveat). Only `adjustment`
  # has one -- see fmt_noncollapsible_caveat().
  for (m in c(spec$measure_text, spec$measure_bg)) {
    if (is.na(m) || is.null(MEASURES[[m]]$caveat)) next
    cv <- measure_word_of(MEASURES[[m]]$caveat, spec)
    if (!is.null(cv)) { spec$caveat <- cv; break }
  }

  # ... but a line that shows no ladder at all (a gap measure's baseline column) says nothing the
  # caveat could qualify, and the ladder line beside it already carries it.
  if (legend_gap_baseline(spec$plan_txt %||% spec$plan_bg, spec$no_obs)) spec$caveat <- NULL

  is_bg_only <- is.null(spec$plan_txt)
  primary    <- if (is_bg_only) spec$plan_bg else spec$plan_txt
  toks <- c(toks, channel_tokens(primary, is_bg_only, with_shades = TRUE))
  # a second measure on the background channel (e.g. color = c("diff","ratio")): it takes no face
  # word (the fills carry magnitude only) but names its own measure.
  if (!is.null(spec$plan_txt) && !is.null(spec$plan_bg)) {
    bg <- channel_tokens(spec$plan_bg, TRUE, with_shades = FALSE)
    if (!is.null(bg)) toks <- c(toks, list(.lg_tok(" ")), bg)
  }

  # ---- the note: what an UNCOLOURED cell means --------------------------------------------------
  # NB: each format string is ONE literal, not paste0(...): xgettext extracts each constant
  # separately, so a paste0-split message never matches the joined string gettextf looks up ->
  # translation silently fails. And ONE WHOLE SENTENCE per variant, never a %s for the verb: a single
  # word carries gender and number in French, which only a full-sentence msgid can get right.
  note <- NULL
  if (identical(spec$policy, "grey_non_signif")) {
    thr <- spec$threshold_phrase
    note <- if (!is.na(thr)) {
      if (mark)
        gettextf("Unmarked: not significantly different from %s (%s) or under the first threshold (%s).",
                 spec$ref_plain, spec$method_phrase, thr)
      else
        gettextf("Uncoloured: not significantly different from %s (%s) or under the first colour threshold (%s).",
                 spec$ref_plain, spec$method_phrase, thr)
    } else {
      if (mark)
        gettextf("Unmarked: not significantly different from %s (%s).", spec$ref_plain, spec$method_phrase)
      else
        gettextf("Uncoloured: not significantly different from %s (%s).", spec$ref_plain, spec$method_phrase)
    }
  } else if (identical(spec$policy, "guaranteed_effect")) {
    # the absolute-threshold reading (contrib's residual) grades the quantity itself, so its note
    # names the significance threshold rather than a guarantee subtracted from a deviation.
    note <- if (isTRUE(spec$txt$guar_abs)) {
      if (mark)
        gettextf("Unmarked: below the significance threshold (%s). The thresholds above are comparable between tables.",
                 spec$method_phrase)
      else
        gettextf("Uncoloured: below the significance threshold (%s). The thresholds above are comparable between tables.",
                 spec$method_phrase)
    } else {
      if (mark) gettextf("Unmarked: not significantly different from %s.", spec$ref_plain)
      else      gettextf("Uncoloured: not significantly different from %s.", spec$ref_plain)
    }
  }
  # where only SOME rows carry a test, uncoloured means a third thing -- say so, or a reader takes an
  # untested cell for a tested-and-null one.
  if (!is.null(note) && isTRUE(spec$partial_test))
    note <- paste0(note, " ", if (mark) gettext("Some rows carry no test and are left unmarked.")
                              else      gettext("Some rows carry no test and are left uncoloured."))
  if (!is.null(note)) toks <- c(toks, list(.lg_tok(paste0(" ", note))))

  # the note above states ONE comparison (the text channel's). A gap measure on the background compares
  # something else, by a test of its own, so it needs one clause -- gated on the BACKGROUND's own
  # resolved policy, not spec$policy (the TEXT channel's).
  if (!identical(spec$plan_bg$policy, "ignore") &&
      !is.null(spec$plan_txt) && !is.null(spec$plan_bg) &&
      !is.null(spec$bg) && !is.na(spec$bg$ref_note)) {
    toks <- c(toks, list(.lg_tok(paste0(" ", gettextf(
      "Background: the same rule, applied to the gap with %s (%s).",
      spec$bg$ref_note, spec$bg$method_phrase)))))
  }
  if (!is.null(spec$caveat)) toks <- c(toks, list(.lg_tok(paste0(" ", spec$caveat))))
  toks
}

# ---- render a token stream for one medium ----------------------------------------------------------
# "runs" -> a list of runs list(text=, color=, bold=); every other medium -> a single string.
# Coloured break-words carry the visual weight of the numbers they describe: TEXT-colour ones stay
# BOLD, BACKGROUND-colour ones are PLAIN (a fill bolds nothing). A PLAIN token is always plain -- the
# palette is the only source of weight here.
# The md branch backslash-escapes `*` in plain-token text so pandoc does not read emphasis.
legend_render_line <- function(tokens, medium, theme, colored, classes = FALSE) {
  # `theme` may be the render intent "auto"; a palette is always light/dark -- resolve it or
  # get_color_style() errors on a length-0 vector.
  pal <- tx_palette_theme(theme)
  # a "runs" medium draws TEXT and cannot fill, so a background break-word borrows the darker bg_legend
  # palette (the fills are invisible on the white page a run sits on). The text channel is the "text" family.
  fam <- function(ch) if (identical(ch, "text")) "text"
                      else if (identical(medium, "runs")) "bg_legend" else "bg"
  slot_hex <- function(slot, ch)
    toupper(unname(get_color_style("color_code", type = fam(ch), theme = pal)[slot]))
  is_colored_tok <- function(tk) isTRUE(colored) && !is.na(tk$c) && tk$c > 0L
  # the break-word wears the SAME face as the cells it describes -- read from the palette, not inferred
  # (the html branch writes `font-weight:bold` inline, which beats the stylesheet, so a "has a hex" ->
  # bold guess would render a print under-side break-word bold while its cells are italic).
  tok_face <- function(tk, k) {
    if (!is_colored_tok(tk)) return(FALSE)
    isTRUE(get_color_style("face", type = fam(tk$ch), theme = pal)[[k]][tk$c])
  }
  is_bold_tok  <- function(tk) isTRUE(tk$b) || tok_face(tk, "bold")
  semantic     <- fmt_face_semantic(pal)
  is_ital_tok  <- function(tk) tok_face(tk, "italic")
  # `underline` is the three-value vocabulary, so it has its own reader.
  is_under_tok <- function(tk) {
    if (!is_colored_tok(tk)) return("")
    get_color_style("face", type = fam(tk$ch), theme = pal)$underline[tk$c]
  }
  if (identical(medium, "runs")) {
    return(lapply(tokens, function(tk) {
      col <- if (is_colored_tok(tk)) slot_hex(tk$c, tk$ch) else NA_character_
      list(text = tk$t, color = col, bold = is_bold_tok(tk),
           italic = is_ital_tok(tk), underline = is_under_tok(tk))
    }))
  }
  parts <- vapply(tokens, function(tk) {
    bold <- is_bold_tok(tk); ital <- is_ital_tok(tk); und <- is_under_tok(tk)
    if (!is_colored_tok(tk)) {
      # plain token: a variable name (bold) or footer text (stars, weight line...). `esc` escapes the
      # pandoc metacharacters so a legend is not re-read as markup (user subtext left raw): `*` runs
      # would pair as emphasis, and `$` runs as INLINE MATH -- which a money level name ("1-Lt
      # $10000", "$25000 or more") triggers as soon as two of them appear in one line.
      # DESIGN: the html medium needs it too -- a knitted page's raw-html goes THROUGH pandoc. The
      # html arm entity-encodes instead, `&` FIRST or it double-escapes the entities it just wrote.
      txt <- tk$t
      if (identical(medium, "md")   && isTRUE(tk$esc)) {
        txt <- gsub("*", "\\*", txt, fixed = TRUE)
        txt <- gsub("$", "\\$", txt, fixed = TRUE)
      }
      if (identical(medium, "html") && isTRUE(tk$esc)) {
        txt <- gsub("&", "&amp;", txt, fixed = TRUE)
        txt <- gsub("<", "&lt;" , txt, fixed = TRUE)
        txt <- gsub("*", "&#42;", txt, fixed = TRUE)
        txt <- gsub("$", "&#36;", txt, fixed = TRUE)
      }
      if (!bold) return(txt)
      if (identical(medium, "console")) return(cli::style_bold(txt))
      if (identical(medium, "html"))    return(paste0("<b>", txt, "</b>"))
      if (identical(medium, "md"))      return(paste0("**", txt, "**"))
      return(txt)
    }
    if (identical(medium, "console")) {
      # `theme` is an argument, so the palette must follow it -- reading the option here would render a
      # legend the caller never asked for.
      style <- get_color_style("crayon", type = fam(tk$ch), theme = pal)[[tk$c]]
      # ⚠ The console PAINTS a fill behind a background break-word (make_ansi_style(bg = TRUE)), so
      # the word keeps the terminal's own foreground -- light, on the theme whose fills are light
      # panels. Compose the theme's `on_fill` ink over it, or the break-word cannot be read.
      if (identical(tk$ch, "bg") && !identical(medium, "runs")) {
        ofc <- tx_chrome_hex(pal)$on_fill
        if (!is.null(ofc) && !is.na(ofc))
          style <- cli::combine_ansi_styles(style, cli::make_ansi_style(ofc))
      }
      out <- style(tk$t)
      if (bold) out <- cli::style_bold(out)
      if (ital) out <- cli::style_italic(out)
      # no terminal rule is portably doubled, so both ruled rungs read as one line here.
      if (nzchar(und))  out <- cli::style_underline(out)
      out
    } else if (identical(medium, "html")) {
      # DESIGN: the span is emitted inline -- no library call; theirs are byte-unstable across releases.
      # `classes` = "our stylesheet ships with this output" -> the break-word carries a slot CLASS
      # (theme-toggle-safe); else keep hex. Weight is per-channel: `font-weight:bold` only on the text
      # channel. `font-weight` is stated EXPLICITLY when the palette says not-bold, since this inline
      # span must override the stylesheet's `.p1..m4{font-weight:bold}` baseline.
      wt <- if (bold) "font-weight:bold;" else if (identical(tk$ch, "text")) "font-weight:normal;" else ""
      if (ital) wt <- paste0(wt, "font-style:italic;")
      if (nzchar(und)) wt <- paste0(wt, "text-decoration:",
                                    if (identical(und, "double")) "underline double" else "underline",
                                    ";")
      # a palette whose meaning is TYPOGRAPHY writes the break-word as markup too, so a sanitizer that
      # strips class/style (GitHub, Word paste) keeps the tags. No-op under the colour palettes.
      lab <- if (semantic) html_face_wrap(tk$t, bold, ital, und) else tk$t
      if (isTRUE(classes)) {
        cls <- tx_slot_class(tk$ch, tk$c)
        if (identical(tk$ch, "text"))
          paste0("<span class=\"", cls, "\" style=\"", wt, "\">", lab, "</span>")
        else paste0("<span class=\"", cls, "\" style=\"", wt, "border-radius:4px;",
                    "padding-right:4px;padding-left:4px;\">", lab, "</span>")
      } else {
        hex <- slot_hex(tk$c, tk$ch)
        if (identical(tk$ch, "text"))
          paste0("<span style=\"", wt, "color:", hex, " !important;\">", lab, "</span>")
        else {
          # no stylesheet ships with this output, so the on_fill ink is stated inline too.
          ofc <- tx_chrome_hex(pal)$on_fill
          ink <- if (is.null(ofc) || is.na(ofc)) "" else paste0("color:", ofc, " !important;")
          paste0("<span style=\"", wt, ink, "background-color:", hex,
                 " !important;border-radius:4px;padding-right:4px;padding-left:4px;\">",
                 lab, "</span>")
        }
      }
    } else if (identical(medium, "md")) {
      # `**` makes the TEXT break-words stand out in RAW markdown too; the background channel is plain.
      # A monochrome palette's under-side is ITALIC (`*[..]{.m1}*`).
      cls <- tx_slot_class(tk$ch, tk$c)
      if (!nzchar(cls)) tk$t
      else {
        out <- paste0("[", tk$t, "]{.", cls, "}")
        if (ital) out <- paste0("*", out, "*")
        if (bold) out <- paste0("**", out, "**")
        out
      }
    } else tk$t
  }, character(1))
  paste0(parts, collapse = "")
}

# ---- build the per col_var specs -------------------------------------------------------------------
#' @keywords internal
# Does any cell of this column carry a value the colour measures in force could grade? Reads the same
# per-measure `raw` getter fmt_color_plan() does, so "coloured nowhere" and "named in no legend" are
# the one fact.
#' @keywords internal
#' @noRd
fmt_has_color_source <- function(col) {
  ks <- unique(c(get_color(col), get_color_bg(col)))
  ks <- ks[!is.na(ks) & !ks %in% c("no", "")]
  if (!length(ks)) return(FALSE)
  any(vapply(ks, function(k) {
    m <- MEASURES[[measure_key(k)]]
    is.null(m) || any(!is.na(m$raw(col)))
  }, logical(1)))
}

legend_specs <- function(x, theme = "light") {
  is_f <- purrr::map_lgl(x, is_fmt)
  ct   <- get_color(x); cbg <- get_color_bg(x)
  keep <- is_f & ((!is.na(ct)  & !ct  %in% c("no", "")) |
                  (!is.na(cbg) & !cbg %in% c("no", "")))
  # ... and a CROSSTAB column must have something for that measure to GRADE. A ladder names the columns
  # it reads, and a column whose measure is void everywhere can never wear a shade -- the row-% Total
  # of an odds-ratio table, whose 2x2 is degenerate (tab_apply_reference), is the case this exists for.
  # ⚠ REGRESSION columns are exempt (`role`): a crude column under `color = "adjustment"` is void by
  # construction -- it IS the baseline the gap is measured from -- and must still be named beside its
  # model column, which is what legend_reg_adapter() folds into one line.
  if (any(keep)) keep[keep] <- purrr::map_lgl(x[keep], function(col)
    nzchar(get_role(col) %||% "") || fmt_has_color_source(col))
  if (!any(keep)) return(list())

  col_vars_levels <- tab_get_vars(x)$col_vars_levels
  col_vars_levels <- col_vars_levels[is_real_col_var(names(col_vars_levels))]
  kept_names <- names(x)[keep]

  # the KIND is a stored fact (meta$spec$kind) -- ask it, not "does this table carry a model recipe": a
  # reg table can legitimately have no `call` and would then be legended as a crosstab.
  meta   <- reg_call(x)
  is_reg <- tab_is_reg(x)
  shades <- legend_shade_names(theme)
  # the table's OWN words for a measure (set_legend_words()), folded by measure_facts() as its last
  # layer. Read once here, carried on every spec, so no reader indexes `meta` for a word.
  words  <- get_legend_words(x)
  # the mean_diff scale in force. Its `std` flag decides whether a numeric/coef diff is sd-standardized
  # (SD units) or raw. SAME source fmt_color_plan() reads, so the legend can never disagree with the cells.
  mean_diff_std <- isTRUE(color_scales()$mean_diff$std)

  # One spec per colored column, so several measures sharing a col_var (a reg outcome span's model +
  # empirical columns) each get their own spec. legend_group_by_body() folds columns with an IDENTICAL
  # rendered body, so a crosstab's level columns still collapse to one line.
  reps <- purrr::imap(col_vars_levels, function(cols, cv) {
    cc <- cols[cols %in% kept_names]
    purrr::map(cc, function(cn) list(cn = cn, cv = cv))
  })
  reps <- purrr::flatten(purrr::compact(reps))
  if (length(reps) == 0) return(list())

  # Build the rich specs. For a reg table the empirical + model columns describe the SAME scale but
  # differ superficially (role, effect word, reference label); legend_reg_adapter reconciles them per
  # col_var so both fold into one line. A crosstab is untouched.
  specs <- purrr::map(reps, function(e) {
    cn <- e$cn; cv <- e$cv
    col      <- x[[cn]]
    # same cross-channel arbiter as the cells (drops a degenerate guaranteed_effect channel), so a
    # disabled channel loses its legend line too.
    pl       <- resolve_color_channel_plans(col)
    plan_txt <- pl$text
    plan_bg  <- pl$bg
    if (is.null(plan_txt) && is.null(plan_bg)) return(NULL)
    # the column's stored scale row answers all of these -- the SAME facts fmt_color_plan() reads, so
    # the legend and the cells cannot describe different ladders.
    scl      <- fmt_scale_row(col)
    is_coef  <- identical(scl$var_kind, "coef")
    is_mean  <- scl$var_kind %in% c("mean", "count")
    # three diff "kinds": factor pct (x100, "points"), numeric/coef STANDARDIZED (SD), numeric/coef RAW.
    # is_pct drives the x100; is_std drives the "SD" wording.
    is_pct   <- identical(scl$ladder, "pct")
    # a NON-gaussian coefficient (measure = "raw_coefficient") colours on the LOGGED odds_ratio scale, NOT the
    # SD-standardized one, so its legend must NOT say "SD". That three-way distinction IS `ladder`.
    is_std   <- identical(scl$ladder, "std") && mean_diff_std
    policy   <- if (!is.null(plan_txt)) plan_txt$policy else plan_bg$policy
    m_txt    <- if (!is.null(plan_txt)) plan_txt$measure else NA_character_
    m_bg     <- if (!is.null(plan_bg))  plan_bg$measure  else NA_character_
    orient   <- if (identical(get_pct_type(col), "col")) "col" else "row"
    eff_word <- if (isTRUE(is_reg)) legend_reg_eff_word(col, meta) else NA_character_
    # the emp/model split reads the column's STORED `role` attr, not the "Emp." name prefix. Fall back
    # to "model" if an old/hand-built reg column lacks it.
    role     <- if (isTRUE(is_reg)) { r <- get_role(col); if (nzchar(r)) r else "model" } else "model"
    # THE BASELINE IS THE COLOURING MEASURE'S, whichever channel carries it: `color = c("no", ...)`
    # is a background-only column, and reading the (absent) TEXT measure left `ref_kind` NULL and
    # aborted the whole footer. Same fallback the `policy` line above already makes.
    m_ref    <- if (!is.na(m_txt)) m_txt else m_bg
    ref      <- legend_ref_info(x, col, m_ref, orient, is_coef = is_coef, is_reg = is_reg,
                                policy = policy)
    scale_key <- get_scale(col)
    ci_method <- get_ci_method(col)
    conf_lvl  <- get_conf_level(col)
    # `fmt_degf_attr()` is the RAW read (an unstamped column must contribute nothing, where get_degf()
    # would answer Inf).
    degf_col  <- fmt_degf_attr(col)
    basis_col <- get_basis(col)
    # does this column carry a test on SOME rows only? A gap measure's SE is missing wherever it could
    # not be computed, and those rows render like a tested-non-significant one, so the grey NOTE must
    # not claim they were all tested. Per-column, so a fully-tested column's legend is unchanged.
    # `no_obs`: this column has no baseline to be compared to -- so it IS the baseline.
    no_obs      <- all(is.na(get_obs(col)))
    gse         <- get_gap_se(col)
    # "a measure whose test may be missing per row" IS "a measure whose baseline is another column"
    # (measure_own_ref names exactly those rows).
    gap_chans    <- c(m_txt, m_bg)
    gap_chans    <- gap_chans[!is.na(gap_chans)]
    partial_test <- !identical(policy, "ignore") &&
      any(vapply(gap_chans, measure_own_ref, logical(1))) &&
      any(is.na(gse)) && any(!is.na(gse))
    # a legend line names the column BLOCK, not the bare variable -- after a spread two blocks share one
    # col_var and differ only by sub-population, so a bare name would say "marital" twice.
    list(col_var = fmt_col_block(cv, get_col_group(col))$label,
         col_name = cn, col_label = tx_strip_col_var_suffix(cn, cv),
         plan_txt = plan_txt, plan_bg = plan_bg,
         partial_test = partial_test, no_obs = no_obs,
         measure_text = m_txt, measure_bg = m_bg,
         is_mean = is_mean, is_std = is_std, is_pct = is_pct, is_coef = is_coef,
         policy = policy, orientation = orient, scale = scale_key,
         ci_method = ci_method, conf_level = conf_lvl, degf = degf_col, basis = basis_col,
         is_reg = is_reg, eff_word = eff_word, role = role, shades = shades,
         theme = theme, words = words,
         model_family = get_model_family(col),        # the collapsibility caveat below
         ref = ref)
  })
  specs <- purrr::compact(specs)
  if (length(specs) == 0) return(list())

  if (isTRUE(is_reg)) specs <- legend_reg_adapter(specs)
  specs
}

# group the per-column specs into legend lines by their RENDERED BODY (the name-less token stream): two
# specs share a line iff they render the same body, so grouping can NEVER drift from what prints.
# Style-local (terse vs prose may fold differently); groups keep first-occurrence order.
legend_group_by_body <- function(specs, style, lang) {
  body_of <- function(s) {
    toks <- if (identical(style, "prose")) legend_tokens_prose(s, lang, FALSE)
            else                           legend_tokens_terse(s, lang, FALSE)
    paste0(vapply(toks, function(tk) tk$t, character(1)), collapse = "")
  }
  bodies <- vapply(specs, body_of, character(1))
  lapply(unique(bodies), function(k) specs[bodies == k])
}

# reconcile the empirical + model specs of each col_var of a REG table so they fold into one legend
# line: when a col_var has one distinct non-NA reference label, apply it to every spec there.
#
# It used to NEUTRALISE the model's additive effect word as well, because a crude column had none to
# match. Both sides name their own measure now (legend_reg_eff_word), and they agree wherever they
# pair -- so neutralising would REINTRODUCE the mismatch it was written to remove.
legend_reg_adapter <- function(specs) {
  by_cv <- split(seq_along(specs), purrr::map_chr(specs, "col_var"))
  for (idx in by_cv) {
    labs <- unique(stats::na.omit(vapply(specs[idx], function(s) s$ref$label, character(1))))
    if (length(labs) == 1L) for (i in idx) specs[[i]]$ref$label <- labs
  }
  specs
}

# The COLUMNS a legend line names, through the one name-list renderer. Only the normalisation is
# local: an html-path wrap marker has to be undone, and an intra-name space protected, before the
# names are joined.
legend_name_list <- function(names, max_n = 6L) {
  norm <- vapply(names, function(nm) {
    nm <- gsub("<br>|\n|\u202f", " ", nm)                  # undo html-path wrap markers
    nm <- trimws(gsub("[[:space:]]+", " ", nm))
    gsub(" ", "\u00a0", nm)                                # protect intra-name spaces (no-break)
  }, character(1), USE.NAMES = FALSE)
  tx_name_list(norm, max = max_n, overflow = "etc")
}

legend_streams <- function(x, style, lang, theme = "light") {
  with_legend_lang(lang, function(lg) {
    # `theme` reaches here for ONE reason -- the shade NAMES a palette gives its directions. Everything
    # else the legend needs is theme-free.
    specs <- legend_specs(x, theme)
    if (length(specs) == 0) return(list())
    specs <- lapply(specs, function(s) legend_resolve_spec(s, lg))
    grp   <- legend_group_by_body(specs, style, lg)
    show_global <- length(grp) > 1
    # a col_var spawning SEVERAL legend lines (a reg outcome span -> model + empirical) is prefixed by
    # the COLUMN names (the col_var alone is ambiguous); a single-line col_var keeps its name.
    cv_lines <- table(unlist(lapply(grp, function(g) unique(purrr::map_chr(g, "col_var")))))
    lapply(grp, function(g) {
      spec <- g[[1]]
      cvs  <- unique(purrr::map_chr(g, "col_var"))
      # a role-MIXED group (empirical + model merge) shows a prefix and names the COLUMNS; a role-uniform
      # group keeps the old rule.
      mixed       <- length(unique(purrr::map_chr(g, "role"))) > 1
      show_this   <- show_global || mixed
      name_by_col <- mixed || any(cv_lines[cvs] > 1)
      spec$col_names <- if (name_by_col) unique(purrr::map_chr(g, "col_label")) else cvs
      # a multi-outcome regression column carries a trailing " [dep]" bracket in its NAME for console
      # clash-avoidance; the col_var span already names the outcome, so the legend strips it. Gated to
      # reg groups so a level label ending in "[...]" is untouched.
      if (any(nzchar(purrr::map_chr(g, "role"))))
        spec$col_names <- tx_strip_outcome_suffix(spec$col_names)
      # A crude column EVALUATES a closed form of the very interval the model column fits, which is
      # why the two share one block (both labels name the estimand). The block names the closed form
      # once, so the reader is told which arithmetic the observed column ran.
      if (identical(style, "prose") && !is.na(spec$method_phrase)) {
        cf <- unique(stats::na.omit(vapply(g, function(o)
          unname(CI_METHOD_CLOSED_FORM[o$ci_method %||% ""]), character(1))))
        if (length(cf) == 1L)
          spec$method_phrase <- if (mixed)
            gettextf("%s; matching %s interval on the observed column", spec$method_phrase, cf)
          else gettextf("%s; %s closed form", spec$method_phrase, cf)
      }
      if (identical(style, "prose")) legend_tokens_prose(spec, lg, show_this)
      else                           legend_tokens_terse(spec, lg, show_this)
    })
  })
}

# fmt_point_palette() -- the 8 slot colours to paint a MARK with (a plotted point, a row band), not a
# glyph. One forced deviation: a publication palette gives every TEXT slot near-black and separates directions by
# bold vs italic, which a point cannot be, so a mark borrows the print palette's dark grey ramp
# (bg_legend). Nothing is lost: in a forest plot the DIRECTION is read off the null line, so colour only
# carries magnitude. Every other theme returns the table's own palette.
#' @keywords internal
fmt_point_palette <- function(theme = "light", channel = c("text", "bg")) {
  get_color_style("color_code", type = tx_plot_ink_family(theme, channel), theme = theme)
}

# legend_guide_spec() -- the colour legend as a real GGPLOT GUIDE instead of a sentence. Same producers,
# a different medium: legend_specs() -> legend_resolve_spec() -> legend_break_tokens() (which already
# drops a break that renders identically, so under a publication palette the twin ladders collapse for free).
#
# The honest limit: a ggplot has exactly ONE scale per aesthetic, so a key list can describe only one
# ladder. When the plotted columns form several legend body-groups this returns NULL and forest_plot()
# falls back to printing the whole legend in the caption -- the same grouping rule the footer uses.
#
# Returns list(title, keys = data.frame(slot, hex, label), grey_hex, grey_label), or NULL.
#' @keywords internal
legend_guide_spec <- function(x, cols, channel = c("text", "bg"), theme = "light", lang = NULL) {
  channel <- match.arg(channel)
  with_legend_lang(lang, function(lg) {
    specs <- legend_specs(x, theme)
    specs <- Filter(function(s) s$col_name %in% cols, specs)
    if (!length(specs)) return(NULL)
    specs <- lapply(specs, function(s) legend_resolve_spec(s, lg))
    pl_of <- function(s) if (identical(channel, "text")) s$plan_txt else s$plan_bg
    specs <- Filter(function(s) !is.null(pl_of(s)), specs)
    if (!length(specs)) return(NULL)
    if (length(legend_group_by_body(specs, "terse", lg)) > 1L) return(NULL)   # several ladders

    spec <- specs[[1]]
    plan <- pl_of(spec)
    tk   <- legend_break_tokens(plan, spec$is_pct, channel, lg, theme)
    if (!length(tk$over) && !length(tk$under)) return(NULL)
    hex  <- fmt_point_palette(theme, channel)      # what the PLOT paints, not what the table prints
    side <- function(toks, glyph) {
      if (!length(toks)) return(NULL)
      data.frame(slot  = vapply(toks, function(t) as.integer(t$c), integer(1)),
                 label = vapply(toks, function(t) paste0(glyph, "\u00a0", t$t), character(1)),
                 stringsAsFactors = FALSE)
    }
    # strongest OVER at the top, then the under side deepening downwards -- the reading order of a
    # vertical guide beside a forest plot whose x axis runs the same way.
    keys <- rbind(side(rev(tk$over), .lg_ge), side(tk$under, .lg_le))
    keys$hex <- hex[keys$slot]
    # A palette whose two directions render the SAME swatch (a publication one) would produce duplicate
    # keys. Merge them: one swatch, both thresholds (the direction is read off the axis anyway).
    if (anyDuplicated(keys$hex)) {
      keys <- do.call(rbind, lapply(unique(keys$hex), function(h) {
        k <- keys[keys$hex == h, , drop = FALSE]
        data.frame(slot = k$slot[1], label = paste(k$label, collapse = " / "), hex = h,
                   stringsAsFactors = FALSE)
      }))
    }
    ch  <- if (identical(channel, "text")) spec$txt else spec$bg
    # the MEASURE names the guide, not the subject word (a two-channel table would say "Cells vs the
    # Total row" twice). legend_measure_word is the namer: an effect word on a reg column, the measure elsewhere.
    meas <- if (identical(channel, "text")) spec$measure_text else spec$measure_bg
    word <- legend_measure_word(meas, spec$is_std, spec$eff_word, plan$policy, plan$scale_key,
                                words = spec$words)
    # the baseline this measure is read against: its OWN, when it has one (the two gap measures name
    # another column), else the column's -- the same two-step legend_tokens_prose() makes.
    rw  <- if (isTRUE(ch$has_ref_lead) && !is.na(ch$ref_lead)) ch$ref_lead else spec$ref_phrase
    # under `guaranteed_effect` the coloured quantity is not the deviation but the part of it the
    # interval guarantees, so the title has to say so -- the grey key ("not guaranteed") already does.
    if (identical(plan$policy, "guaranteed_effect")) {
      cf   <- suppressWarnings(get_conf_level(x[[spec$col_name]])[1])
      word <- if (is.finite(cf)) gettextf("guaranteed (%s%%) %s", format(100 * cf), word)
              else gettextf("guaranteed %s", word)
    }
    list(title = trimws(paste(legend_ucfirst(word),
                              if (is.na(rw) || !nzchar(rw)) "" else gettextf("vs %s", rw))),
         keys = keys, grey_hex = tx_chrome_hex(theme)$grey,
         grey_label = switch(plan$policy,
                             grey_non_signif   = gettext("not significant"),
                             guaranteed_effect = gettext("not guaranteed"),
                             gettext("below the first threshold")))
  })
}

# enc2utf8 guards the gettext catalog output. "runs" -> run-lists (Excel / plot), else a char vector.
render_streams <- function(streams, medium, theme, colored, classes = FALSE) {
  if (identical(medium, "runs")) {
    return(unname(lapply(streams, function(toks)
      lapply(legend_render_line(toks, "runs", theme, colored, classes),
             function(r) { r$text <- enc2utf8(r$text); r }))))
  }
  enc2utf8(vapply(streams, function(toks)
    legend_render_line(toks, medium, theme, colored, classes), character(1)))
}

#' Build the colour legend of a table
#'
#' Internal. Returns one legend line per colour-signature group. For \code{medium = "runs"} each line
#' is a list of runs \code{list(text, color, bold)}; otherwise a character string.
#' @param x A \code{tabxplor_tab}.
#' @param medium One of "console", "html", "md", "runs", "plain". \code{"runs"} is for the media that
#'   draw the legend as coloured TEXT and cannot fill, such as an Excel rich-text cell
#'   (\code{\link{tab_xl}}). It returns the runs unrendered, and draws the
#'   background channel from the darker \code{bg_legend} palette (see \code{\link{set_color_palette}}).
#' @param style "terse" (compact, console default) or "prose" (full sentences, export default).
#' @param lang NULL (auto from locale) / "en" / "fr".
#' @param colored Whether to colour the break-words.
#' @param theme Palette theme (default from options).
#' @param classes `medium = "html"` only: emit the break-words as CSS slot classes rather than inline
#'   hex, because a tabxplor stylesheet ships with the output (`tab_html()`). Then the
#'   legend follows a theme toggle exactly like the cells it describes. `FALSE` (a table rendered
#'   without a stylesheet of ours) keeps inline hex.
#' @return A character vector (or, for "runs", a list of run-lists), or NULL when nothing is coloured.
#' @keywords internal
tab_color_legend <- function(x, medium = c("console", "html", "md", "runs", "plain"),
                             style = NULL, lang = NULL, colored = TRUE,
                             theme = NULL, classes = FALSE) {
  medium <- match.arg(medium)
  if (is.null(style))      style      <- if (identical(medium, "console")) "terse" else "prose"
  if (is.null(theme))      theme      <- tx_theme_option("console")
  streams <- legend_streams(x, style, lang, theme)
  if (length(streams) == 0) return(NULL)
  render_streams(streams, medium, theme, colored, classes)
}

# run f(lg) with LANGUAGE set for the gettext lookups (flushing glibc's cache before/after). Shared by
# the plain-text footer helpers (stars / weight legend), which are not coloured.
# ⚠ RE-ENTRANT: a footer opens ONE scope for the whole region (tab_footer_streams()), and the builders
# inside it open their own. A nested call for the SAME language is then a plain f(lg) -- four
# bindtextdomain() round trips per builder is what the outer scope exists to save. Tracked on a flag
# rather than on LANGUAGE itself: only an ancestor call proves the domain was actually re-bound.
.tx_legend_lang <- new.env(parent = emptyenv())
with_legend_lang <- function(lang, f) {
  lg  <- legend_resolve_lang(lang)
  if (identical(.tx_legend_lang$active, lg)) return(f(lg))
  old <- Sys.getenv("LANGUAGE", unset = NA_character_)
  flush_gettext_cache(); Sys.setenv(LANGUAGE = lg); flush_gettext_cache()
  prev <- .tx_legend_lang$active
  .tx_legend_lang$active <- lg
  on.exit({
    .tx_legend_lang$active <- prev
    if (is.na(old)) Sys.unsetenv("LANGUAGE") else Sys.setenv(LANGUAGE = old)
    flush_gettext_cache()
  }, add = TRUE)
  f(lg)
}
