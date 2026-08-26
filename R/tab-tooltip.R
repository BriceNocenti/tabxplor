# PURPOSE: the html hover tooltip -- one declared line per fact the cell carries.
# ROLE: a rendering layer like the exporters, and the last one to read the fmt record directly.
# KEY CONSTRAINTS:
#   - A TOOLTIP IS A RENDERING OF THE CELL'S OWN RECORD: every value goes through format() on the
#     column's own scale, so a hovered number and a printed one cannot disagree.
#   - A LINE IS NAMED BY THE TOKEN IT RENDERS (DISPLAY_TOKENS$label) -- the same tags the exports'
#     unit row and the console type tag print. Three lines qualify that name; nothing else does.
#   - NOT TRANSLATED, deliberately, like the pillar type abbreviations: the tags stay the fmt FIELD
#     names, so the hover teaches the fields a user reads with `$` and mutate(). It also ends the
#     collision that had `diff` and `gap` both printing "ecart", the package's umbrella word for a
#     deviation.
#   - ONE GATE for every value line, so no line can drift into an exception of its own.
#   - TWO ROWS, declared (TOOLTIP_LINES$group): the cell's own numbers, then the observed
#     comparison -- `obs` and the gap to it, which is a statement about ANOTHER column. Lines join
#     with " ; " inside a row and with a newline between them, which the bootstrap stylesheet
#     honours (tab-css.R writes `white-space: pre`). Group 2 is the LAST row, and that is checked at
#     load: reg_append_empirical_tip() appends onto a finished string and lands there by position.
# See: CLAUDE.md § tabxplor architecture > The display grammar.


# === SECTION: the shared helpers ===================================================================

# format() right-pads to align in the table, per TOKEN inside a composite; a tooltip is prose, so
# every run of padding collapses. A single space is a big.mark ("1 862") and survives.
#' @keywords internal
#' @noRd
tip_num <- function(v) {
  s <- format(v)
  trimws(gsub("[ \u2007]{2,}", " ", s, perl = TRUE), whitespace = "[\\h\\v]")
}

# Does the cell already print this quantity, anywhere in its template? By FIELD wherever the token
# has one -- `diff` and `coef` are one number written two ways, `ci` and `moe` one interval -- and by
# token for the derived ones (`sd`, `cv`, `resid`, `gap`), which are the only renderings of theirs.
#' @keywords internal
#' @noRd
tooltip_shows <- function(disp, tok, scl) {
  fld  <- DISPLAY_TOKENS[[tok]]$field %||% NA_character_
  toks <- if (is.na(fld)) tok else DISPLAY_FIELD_TOKENS[[fld]] %||% tok
  out  <- rep(FALSE, length(disp))
  for (tk in toks) out <- out | fmt_display_shows(disp, tk, scl)
  out
}

# The facts every line reads, resolved once per column.
#' @keywords internal
#' @noRd
tooltip_ctx <- function(x, .ref = NULL, .base_n = NULL) {
  scl  <- fmt_scale_row(x)
  disp <- get_display(x)
  role <- as.character(get_role(x))[1]
  totcol  <- is_totcol(x)
  totrows <- is_totrow(x)
  pct     <- get_pct(x)

  # THE REFERENCE MASK IS ROLE-AWARE, as format()'s own ref_base() is: a regression column's
  # baseline is `in_refrow`, which every producer stamps, while get_reference() answers on the
  # crosstab's pct axis and returns nothing at all there.
  ref <- fmt_ref_cells(x, .ref)
  # A REFERENCE CELL SITS AT THE NEUTRAL of its scale -- the rule format() itself uses to print the
  # bare "1" / "0". So a regression's BASELINE row, which is `in_refrow` and is not at the neutral,
  # tells itself apart with no extra plumbing: it is the reference for nothing, and names nothing.
  # A level scale has no neutral, hence no such split.
  neutral <- scl$neutral
  if (is.na(neutral)) {
    ref_cell <- ref
    base_row <- rep(FALSE, length(x))
  } else {
    est <- fmt_est_of(x)
    at0 <- !is.na(est) & abs(est - neutral) < 1e-8
    ref_cell <- ref & at0
    base_row <- ref & !at0 & nzchar(role)
  }

  n_shown <- NULL
  if (!is.null(.base_n) && length(.base_n)) {
    m <- matrix(as.double(unlist(.base_n)), nrow = length(x))
    n_shown <- rowSums(m == as.double(get_n(x)), na.rm = TRUE) > 0
  }

  list(scl = scl, disp = disp, role = role, vkind = scl$var_kind,
       stat_row = vctrs::field(x, "row_kind") %in% c("pvalue", "gof", "blank"),
       data_row = vctrs::field(x, "row_kind") %in% c("data", "total"),
       pct_type = get_pct_type(x), is_effect = identical(scl$kind, "effect"),
       digits = get_digits(x), note = NULL,
       comparable = !((totcol | totrows) & !is.na(pct) & pct == 1),
       ref_cell = ref_cell, base_row = base_row, n_shown = n_shown)
}


# === SECTION: the labels a bare field name does not identify =======================================

# On a total row the `ctr` field holds the column's MEAN contribution -- the divisor every cell is
# graded against, printed nowhere else. Under `color_signif = "guaranteed_effect"` the measure
# becomes the standardized residual and that mean plays no part, so the cell drops (tip_render_ctr).
#' @keywords internal
#' @noRd
tip_mean_ctr <- function(x) {
  if (get_comp_all(x)) is_totrow(x) & is_tottab(x) & !is_totcol(x) else is_totrow(x) & !is_totcol(x)
}
#' @keywords internal
#' @noRd
tip_label_ctr <- function(x, ctx) ifelse(tip_mean_ctr(x), "mean ctr", "ctr")

# `obs` holds the OBSERVED (crude) effect -- except under a measure whose declared `ref_kind` is
# "group" (`color = "between_groups"`), where it holds the first group's estimate.
#' @keywords internal
#' @noRd
tip_label_obs <- function(x, ctx) {
  ks <- vapply(c(get_color(x), get_color_bg(x)), measure_key, character(1))
  ks <- ks[!is.na(ks) & nzchar(ks)]
  if (any(vapply(ks, function(k) identical(MEASURES[[k]]$ref_kind, "group"), logical(1))))
    "ref grp" else "obs"
}


# === SECTION: the renderers ========================================================================

# THE ESTIMATE LINE, built from the pieces the cell does NOT already carry: a column printing
# "1/2.89" adds only the bracket and the p, a `ci = "cell"` column only its percentage.
# The exact p-value joins it wherever it IS the interval's own inversion. WARNING: a table that also
# computed chi-squared contributions OVERWRITES `pvalue` with the residual's (chi2_write_contrib()),
# which the `resid` line reports instead -- hence the `ctr` test rather than a scale test alone.
#' @keywords internal
#' @noRd
tip_render_est <- function(x, ctx, tok) {
  n <- length(x)
  v_est <- if (is.na(tok)) rep("", n) else
    dplyr::if_else(tooltip_shows(ctx$disp, tok, ctx$scl), "", tip_num(set_display(x, tok)))
  v_est[is.na(v_est)] <- ""
  v_ci   <- rep("", n)
  has_ci <- !is.na(get_ci(x)) & !tooltip_shows(ctx$disp, "ci", ctx$scl)
  if (any(has_ci)) {
    bump <- set_digits(x, dplyr::if_else(ctx$digits == 0L, ctx$digits + 1L, ctx$digits))
    v_ci <- dplyr::if_else(has_ci, tip_num(set_display(bump, "ci")), "")
    v_ci[is.na(v_ci)] <- ""
  }
  # one "%" per line: the estimate carries it, so its own interval does not repeat it.
  both <- nzchar(v_est) & nzchar(v_ci) & endsWith(v_est, "%")
  v_ci[both] <- sub("%$", "", v_ci[both], perl = TRUE)
  out <- trimws(paste(v_est, v_ci), whitespace = "[\\h\\v]")
  if (ctx$is_effect && all(is.na(get_ctr(x)))) {
    pv  <- test_fmt_pvalue(get_pvalue(x))
    out <- dplyr::if_else(is.na(pv), out, trimws(paste0(out, ", p = ", pv), whitespace = "[\\h\\v]"))
  }
  out[is.na(out)] <- ""
  out
}

# Glass's Delta: the difference standardized by the REFERENCE cell's sd, which is what a mean
# column's colour actually grades while the cell shows the raw difference.
#' @keywords internal
#' @noRd
tip_render_std <- function(x, ctx, tok) {
  std <- get_diff(x) / suppressWarnings(sqrt(get_ref_var(x)))
  out <- ifelse(is.finite(std), paste0(sprintf("%+.2f", std), "sd"), "")
  out[is.na(out)] <- ""
  out
}

#' @keywords internal
#' @noRd
tip_render_ctr <- function(x, ctx, tok) {
  ok <- is.finite(get_ctr(x))
  if (identical(get_color_signif(x), "guaranteed_effect")) ok <- ok & !tip_mean_ctr(x)
  if (!any(ok)) return(rep("", length(x)))
  # a contribution has no direction of its own -- the residual line carries the sign.
  out <- dplyr::if_else(ok, sub("^-", "", tip_num(set_display(x, tok)), perl = TRUE), "")
  out[is.na(out)] <- ""
  out
}

#' @keywords internal
#' @noRd
tip_render_resid <- function(x, ctx, tok) {
  ok  <- is.finite(fmt_resid(x))
  if (!any(ok)) return(rep("", length(x)))
  out <- dplyr::if_else(ok, tip_num(set_display(x, tok)), "")
  out[is.na(out)] <- ""
  out
}

# the GAP (size, interval, p) wherever tab_reg wrote a `gap_se`: too much for a cell, and the colour
# IS its display. Read through the helpers the colour engine reads, so hover and fill cannot
# disagree, and RENDERED through format() (fmt_gap_text()), so the line reads exactly like the `diff`
# line above it -- "-1.4% [-2.1;-0.6]%", never a unit the cell itself never prints.
#' @keywords internal
#' @noRd
tip_render_gap <- function(x, ctx, tok) {
  ok <- !is.na(get_gap_se(x)) & !is.na(get_obs(x))
  if (!any(ok)) return(rep("", length(x)))
  sc  <- fmt_adjustment_score(x)
  bd  <- fmt_gap_bounds(x)
  pv  <- test_fmt_pvalue(fmt_gap_p(x))
  txt <- fmt_gap_text(x)
  out <- dplyr::if_else(ok & is.finite(sc) & is.finite(bd$lo) & !is.na(pv),
                        paste0(txt$est, " ", txt$ci, ", p = ", pv), "")
  out[is.na(out)] <- ""
  out
}

# THE BASE COUNT, unless a base-count column already prints it on the same row (tab_base_n_cols()):
# a tooltip does not repeat what the reader can see beside the cell.
#' @keywords internal
#' @noRd
tip_render_n <- function(x, ctx, tok) {
  out <- tip_num(set_display(x, tok))
  out[is.na(out)] <- ""
  if (!is.null(ctx$n_shown)) out[ctx$n_shown] <- ""
  out
}

# what only the WHOLE table knows: which column block each end of a base RANGE belongs to.
#' @keywords internal
#' @noRd
tip_render_note <- function(x, ctx, tok) {
  if (is.null(ctx$note)) return(rep("", length(x)))
  out <- as.character(vctrs::vec_recycle(as.character(ctx$note), length(x)))
  out[is.na(out)] <- ""
  out
}


# === SECTION: TOOLTIP_LINES ========================================================================
#
# One row per LINE, and ROW ORDER IS THE READING ORDER: what this cell IS, then how far it sits from
# its reference, then what it rests on.
#
# COLUMNS
#   token   the DISPLAY_TOKENS key the line renders (a foreign key, checked at load). The two
#           scale-relative ones resolve per column, so `est` names an odds ratio on one table and a
#           coefficient on the next. NA on a line that renders no token of its own.
#   label   NA = the token's own `label`, through display_token_label(). A string, or a
#           function(x, ctx) returning one name or one per cell.
#   gates   which of the shared gates apply:
#             comparable  a total cell that IS its own 100 % base has nothing to compare to
#             not_ref     drop on a reference cell -- the whole class collapses to one "ref"
#             not_base    drop on a regression's baseline row, the reference for nothing
#             not_shown   drop where the cell already prints that FIELD
#             not_emitted drop where an earlier line already printed that field
#   when    optional function(x, ctx) -> TRUE/FALSE: the column-level condition.
#   render  NULL = format(set_display(x, token)); else the line's own renderer(x, ctx, token).
#   group   which ROW of the tooltip the line lands on. Lines within a group join with " ; ",
#           groups with a newline. Group 1 is the cell's own numbers; group 2 is the observed
#           comparison -- the crude effect and the gap to it -- which is a statement about ANOTHER
#           column and reads as its own sentence.
#
# ⚠ `not_ref` / `not_base` apply only where the line names a DEVIATION: a level (a percentage, a
# mean, a count) is a fact about the cell, and a reference cell has one like any other.
#' @keywords internal
#' @noRd
.ttip <- function(token = NA_character_, label = NA_character_,
                  gates = c("comparable", "not_ref", "not_base", "not_shown", "not_emitted"),
                  when = NULL, render = NULL, group = 1L)
  list(token = token, label = label, gates = gates, when = when, render = render,
       group = as.integer(group))

# THE OBSERVED COMPARISON'S row, and the LAST one -- which is what lets reg_append_empirical_tip()
# (R/tab-render-html.R) go on appending the multinomial crude level with " ; " to a finished string
# and still land on the right line. Asserted at load beside the other cross-table checks.
#' @keywords internal
#' @noRd
TOOLTIP_GROUP_OBS <- 2L

#' @keywords internal
#' @noRd
TOOLTIP_LINES <- list(
  # THE ESTIMATE: no `not_shown` gate, because what it adds to the cell is the interval and the exact
  # p-value, and no `not_base` one -- a baseline row keeps its own value, it just does not name it.
  est   = .ttip("est", gates = c("comparable", "not_ref"), render = tip_render_est),
  # THE LEVEL the estimate sits on -- one line for a percentage, a mean and a count alike, since
  # `{base}` resolves per scale. Named row% / col% / adj% / obs% by the token itself.
  base  = .ttip("base", gates = c("not_shown", "not_emitted")),
  # the spread in the variable's OWN unit, beside a cell that now shows the coefficient of variation.
  sd    = .ttip("sd", gates = c("not_shown", "not_emitted"),
                when = function(x, ctx) identical(ctx$vkind, "mean")),
  diff  = .ttip("diff"),
  # a ratio needs a percentage read along one axis, or two means.
  ratio = .ttip("ratio",
                when = function(x, ctx) ctx$pct_type %in% c("row", "col") ||
                  identical(ctx$vkind, "mean")),
  # AN ODDS RATIO IS COMPUTED ON EVERY ROW/COL-% COLUMN, so it is shown on every one of them: it
  # used to appear only where the table was coloured by it. On a regression column it is the model's
  # own estimate attached beside an AME, which `role` marks.
  # ⚠ INCLUDING the column the odds ratio takes as its BASELINE, a whole column of 1s (under
  # pct = "row" the first column IS the complementary category, under pct = "col" the `ref2` column
  # is). "OR: 1" is how a reader finds which column the ratio is read against -- the one thing the
  # cells themselves cannot show -- and it costs no emphasis: a tooltip is plain text, and the
  # `not_ref` collapse belongs to the pct axis's own reference, not to this one.
  or    = .ttip("or",
                when = function(x, ctx) {
                  if (!(ctx$pct_type %in% c("row", "col") || nzchar(ctx$role))) return(FALSE)
                  any(is.finite(get_or(x)))
                }),
  std   = .ttip(label = "std diff", gates = c("comparable", "not_ref", "not_base"),
                when = function(x, ctx) identical(ctx$vkind, "mean"), render = tip_render_std),
  ctr   = .ttip("ctr", label = tip_label_ctr, gates = c("comparable", "not_shown"),
                render = tip_render_ctr),
  resid = .ttip("resid", gates = c("comparable", "not_shown"), render = tip_render_resid),
  # THE OBSERVED COMPARISON, on its own line: the crude effect, then how far the model moved from it.
  obs   = .ttip("obs", label = tip_label_obs, group = TOOLTIP_GROUP_OBS),
  gap   = .ttip("gap", gates = character(), render = tip_render_gap, group = TOOLTIP_GROUP_OBS),
  n     = .ttip("n", gates = c("not_shown", "not_emitted"), render = tip_render_n),
  note  = .ttip(label = "", gates = character(), render = tip_render_note)
)


# === SECTION: the builder ==========================================================================

# Builds the hover text of ONE fmt column, per row. TEXT only -- the popover / tooltip html
# attributes live in tab_tooltip_attrs(). `.ref` is the pre-computed reference mask (fmt_col_ann()),
# `.note` the per-block breakdown behind a base RANGE, `.base_n` the counts a base-count column of
# the same table already shows.
#' @keywords internal
#' @noRd
tab_tooltip_text <- function(x, .ref = NULL, .note = NULL, .base_n = NULL) {
  n   <- length(x)
  ctx <- tooltip_ctx(x, .ref, .base_n)
  ctx$note <- .note

  # two slots per line: the "ref" word takes the one just before the first DEVIATION line, which is
  # where the comparison it replaces would have been read. `rep(each = 2L)` gives a line and its own
  # "ref" slot the same group, so the word lands on the row of the line it replaces.
  frags   <- vector("list", 2L * length(TOOLTIP_LINES))
  grp     <- rep(vapply(TOOLTIP_LINES, function(l) l$group %||% 1L, integer(1)), each = 2L)
  ref_any <- rep(FALSE, n)
  ref_pos <- NA_integer_
  emitted <- character()

  for (i in seq_along(TOOLTIP_LINES)) {
    nm <- names(TOOLTIP_LINES)[[i]]
    ln <- TOOLTIP_LINES[[nm]]
    if (!is.null(ln$when) && !isTRUE(ln$when(x, ctx))) next
    tok <- if (is.na(ln$token)) NA_character_ else
      fmt_resolve_scale_tokens(ln$token, ctx$scl)[[1]]
    if (!is.na(tok) && identical(tok, "blank")) next
    # the QUANTITY key: the field where the token has one, the token itself where it is derived.
    key <- if (is.na(tok)) NA_character_ else DISPLAY_TOKENS[[tok]]$field %||% tok
    if ("not_emitted" %in% ln$gates && !is.na(key) && key %in% emitted) next

    # a line naming a LEVEL states a fact about the cell, reference or not.
    geo  <- unname(DISPLAY_TOKEN_GEOMETRY[tok])
    devi <- is.na(tok) || is.na(geo) || !identical(geo, "level")
    if (devi && "not_ref" %in% ln$gates && is.na(ref_pos)) ref_pos <- 2L * i - 1L

    txt <- if (is.null(ln$render)) tip_num(set_display(x, tok)) else ln$render(x, ctx, tok)
    txt[is.na(txt)] <- ""
    keep <- nzchar(txt)
    if ("comparable" %in% ln$gates) keep <- keep & ctx$comparable
    if ("not_shown"  %in% ln$gates && !is.na(tok))
      keep <- keep & !tooltip_shows(ctx$disp, tok, ctx$scl)
    # A REFERENCE CELL SAYS "ref" ONCE, in place of every deviation at once -- and it says it as soon
    # as the column HAS a deviation to state (`any(nzchar(txt))`), never per cell: whether this one
    # cell's own field happens to be filled is what made a crude column and its model twin disagree
    # about the same row. On a DATA row only: a synthetic base-count row is not part of the
    # comparison, and a transposed render puts one in the middle of an ordinary column.
    if (devi && "not_ref" %in% ln$gates) {
      if (any(nzchar(txt)))
        ref_any <- ref_any | (ctx$ref_cell & ctx$comparable & ctx$data_row)
      keep <- keep & !ctx$ref_cell
    }
    if (devi && "not_base" %in% ln$gates) keep <- keep & !ctx$base_row
    if (!any(keep)) next
    if (!is.na(key)) emitted <- c(emitted, key)

    lab <- if (is.function(ln$label)) ln$label(x, ctx)
           else if (!is.na(ln$label)) ln$label
           else if (!is.na(tok)) display_token_label(tok, x)
           else ""
    lab <- rep_len(as.character(lab), n)
    # THE BASELINE ROW'S OWN VALUE NAMES NOTHING: it is not the deviation the column's tag would
    # claim (an odds column holds the baseline ODDS there), and the row already says what it is
    # ("Constant", "Reference profile"). Its level and its base count keep their names.
    if (identical(nm, "est")) lab[ctx$base_row] <- ""
    f <- rep("", n)
    f[keep] <- ifelse(nzchar(lab[keep]), paste0(lab[keep], ": ", txt[keep]), txt[keep])
    frags[[2L * i]] <- f
  }

  if (any(ref_any))
    frags[[if (is.na(ref_pos)) 1L else ref_pos]] <- ifelse(ref_any, "ref", "")

  # ONE ROW PER GROUP, so a cell with nothing to say in group 2 gets no trailing newline.
  out <- rep("", n)
  for (g in sort(unique(grp))) {
    og <- rep("", n)
    for (f in frags[grp == g]) {
      if (is.null(f)) next
      k <- !is.na(f) & nzchar(f)
      if (!any(k)) next
      og[k] <- paste0(og[k], ifelse(nzchar(og[k]), " ; ", ""), f[k])
    }
    k <- nzchar(og)
    out[k] <- paste0(out[k], ifelse(nzchar(out[k]), "\n", ""), og[k])
  }

  # A STATISTICAL ROW IS NOT A CELL OF THE TABLE: a model-fit number, a chi-squared p-value or a
  # masked cell holds fields never meant to be compared. Both the row KIND and the token are read --
  # a footer p-value row displays `pvalue`, which is no model-fit token, and used to come out as an
  # estimate line with an empty value ("OR: , p = <0.01%").
  disp <- fmt_resolve_scale_tokens(display_primary(ctx$disp), ctx$scl)
  out[ctx$stat_row | disp %in% c(DISPLAY_GOF_TOKENS, "blank")] <- ""
  enc2utf8(out)
}

# The counts a BASE-COUNT column of the same table already prints, one column per block
# (tab_base_n_cols()): a matrix of them, or NULL where the table has none. Read by tip_render_n().
#' @keywords internal
#' @noRd
tab_base_n_values <- function(tab) {
  if (!is.data.frame(tab)) return(NULL)
  nms <- names(tab)[purrr::map_lgl(tab, ~ is_fmt(.) && fmt_has_role(., "n"))]
  if (!length(nms)) return(NULL)
  matrix(as.double(unlist(lapply(tab[nms], get_n))), nrow = nrow(tab))
}

# THE MULTINOMIAL CRUDE COMPANION (reg_spec_tips_mnl()): the observed LEVEL and its interval,
# rendered as an ordinary fmt cell so hover and cells agree about decimals, glyphs and the "%". The
# cell already folds in the crude odds ratio, so this line states the level and stops there.
#' @keywords internal
#' @noRd
tip_crude_level <- function(pct, inf, sup) {
  col <- fmt(n = NA_integer_, pct = pct, ci_inf = inf, ci_sup = sup, display = "{pct} {ci}",
             scale = "level_pct", pct_type = "row", role = "emp", digits = 0L)
  txt <- sub("\\]%$", "]", tip_num(format(col)), perl = TRUE)
  paste0(display_token_label("pct", col), ": ", txt)
}
