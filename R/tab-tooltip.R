# PURPOSE: the html hover tooltip -- one declared line per fact the cell carries.
# ROLE: a rendering layer like the exporters, and the last one to read the fmt record directly.
# KEY CONSTRAINTS:
#   - A TOOLTIP IS A RENDERING OF THE CELL'S OWN RECORD: every value goes through format() on the
#     column's own scale, so a hovered number and a printed one cannot disagree.
#   - A LINE IS NAMED BY THE TOKEN IT RENDERS (DISPLAY_TOKENS$label) -- the same tags the exports'
#     unit row and the console type tag print. Three lines qualify that name; nothing else does.
#   - NOT TRANSLATED, deliberately, like the pillar type abbreviations: the tags stay the fmt FIELD
#     names, so the hover teaches the fields a user reads with `$` and mutate(). It also keeps two
#     fields from sharing one word, which a translation into the package's umbrella vocabulary would
#     force.
#   - ONE GATE for every value line, so no line can drift into an exception of its own.
#   - TWO ROWS, declared (TOOLTIP_LINES$group): the cell's own numbers, then the observed
#     comparison -- `obs` and the gap to it, which is a statement about ANOTHER column. Lines join
#     with " ; " inside a row and with a newline between them, which the stylesheet honours
#     (R/tab-css.R writes `white-space: pre`). Group 2 is the LAST row, and that is checked at load:
#     reg_append_empirical_tip() appends onto a finished string and lands there by position.
# See: CLAUDE.md section "tabxplor architecture" (the display grammar).


# === SECTION: the shared helpers ===================================================================

# format() right-pads to align in a table; a tooltip is prose, so runs of padding collapse (a single
# space is a big.mark, "1 862", and survives).
#' @noRd
tip_num <- function(v) {
  s <- format(v)
  trimws(gsub("[ \u2007]{2,}", " ", s, perl = TRUE), whitespace = "[\\h\\v]")
}

# Does the cell already print this quantity? By FIELD where the token has one (`diff`/`coef` are one
# number written two ways), by token for the derived ones (`sd`, `cv`, `resid`, `gap`).
#' @noRd
tooltip_shows <- function(disp, tok, scl) {
  fld  <- DISPLAY_TOKENS[[tok]]$field %||% NA_character_
  toks <- if (is.na(fld)) tok else DISPLAY_FIELD_TOKENS[[fld]] %||% tok
  out  <- rep(FALSE, length(disp))
  for (tk in toks) out <- out | fmt_display_shows(disp, tk, scl)
  out
}

#' @noRd
tooltip_ctx <- function(x, .ref = NULL, .base_n = NULL) {
  scl  <- fmt_scale_row(x)
  disp <- get_display(x)
  role <- as.character(get_role(x))[1]
  totcol  <- is_totcol(x)
  totrows <- is_totrow(x)
  pct     <- get_pct(x)

  # role-aware, as format()'s own ref_base() is: a regression baseline is `in_refrow`, while
  # get_reference() answers on the crosstab's pct axis alone.
  ref <- fmt_ref_cells(x, .ref)
  # a reference cell sits at the NEUTRAL of its scale (format()'s own rule for the bare "1"/"0"), so
  # a regression's baseline row -- `in_refrow` but not at the neutral -- tells itself apart for free.
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

# On a total row `ctr` holds the column's MEAN contribution -- the divisor every cell is graded
# against, printed nowhere else. Drops under `color_signif = "guaranteed_effect"` (tip_render_ctr),
# where the measure is the standardized residual and that mean plays no part.
#' @noRd
tip_mean_ctr <- function(x) {
  if (get_comp_all(x)) is_totrow(x) & is_tottab(x) & !is_totcol(x) else is_totrow(x) & !is_totcol(x)
}
#' @noRd
tip_label_ctr <- function(x, ctx) ifelse(tip_mean_ctr(x), "mean ctr", "ctr")

# `obs` holds the OBSERVED (crude) effect, except under a "group"-`ref_kind` measure
# (`color = "between_groups"`), where it holds the first group's estimate.
#' @noRd
tip_label_obs <- function(x, ctx) {
  ks <- vapply(c(get_color(x), get_color_bg(x)), measure_key, character(1))
  ks <- ks[!is.na(ks) & nzchar(ks)]
  if (any(vapply(ks, function(k) identical(MEASURES[[k]]$ref_kind, "group"), logical(1))))
    "ref grp" else "obs"
}


# === SECTION: the renderers ========================================================================

# Built from the pieces the cell does NOT already carry: a column printing "1/2.89" adds only the
# bracket and the p, a `ci = "cell"` column only its percentage.
# ⚠ a table that also computed chi-squared contributions OVERWRITES `pvalue` with the residual's
# (chi2_write_contrib()), which the `resid` line reports instead -- hence the `ctr` test.
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
#' @noRd
tip_render_std <- function(x, ctx, tok) {
  std <- get_diff(x) / suppressWarnings(sqrt(get_ref_var(x)))
  out <- ifelse(is.finite(std), paste0(sprintf("%+.2f", std), "sd"), "")
  out[is.na(out)] <- ""
  out
}

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

#' @noRd
tip_render_resid <- function(x, ctx, tok) {
  ok  <- is.finite(fmt_resid(x))
  if (!any(ok)) return(rep("", length(x)))
  out <- dplyr::if_else(ok, tip_num(set_display(x, tok)), "")
  out[is.na(out)] <- ""
  out
}

# The GAP (size, interval, p) wherever tab_reg wrote a `gap_se`: too much for a cell, and the colour
# IS its display. Read through the same helpers the colour engine reads, so hover and fill cannot
# disagree, and rendered through format() so the line matches the `diff` line above it.
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
#' @noRd
tip_render_n <- function(x, ctx, tok) {
  out <- tip_num(set_display(x, tok))
  out[is.na(out)] <- ""
  if (!is.null(ctx$n_shown)) out[ctx$n_shown] <- ""
  out
}

# what only the WHOLE table knows: which column block each end of a base RANGE belongs to.
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
#   group   which of the two rows the line lands on (see the header).
#
# ⚠ `not_ref` / `not_base` apply only where the line names a DEVIATION: a level (a percentage, a
# mean, a count) is a fact about the cell, and a reference cell has one like any other.
#' @noRd
.ttip <- function(token = NA_character_, label = NA_character_,
                  gates = c("comparable", "not_ref", "not_base", "not_shown", "not_emitted"),
                  when = NULL, render = NULL, group = 1L)
  list(token = token, label = label, gates = gates, when = when, render = render,
       group = as.integer(group))

# The LAST row: reg_append_empirical_tip() (R/tab-render-html.R) appends the multinomial crude level
# onto a finished string and lands there by position.
#' @noRd
TOOLTIP_GROUP_OBS <- 2L

#' @noRd
TOOLTIP_LINES <- list(
  # no `not_shown` gate: what this adds is the interval and the exact p-value, never shown elsewhere.
  est   = .ttip("est", gates = c("comparable", "not_ref"), render = tip_render_est),
  # the level the estimate sits on -- one line for a percentage, a mean and a count alike, since
  # `{base}` resolves per scale.
  base  = .ttip("base", gates = c("not_shown", "not_emitted")),
  sd    = .ttip("sd", gates = c("not_shown", "not_emitted"),
                when = function(x, ctx) identical(ctx$vkind, "mean")),
  diff  = .ttip("diff"),
  ratio = .ttip("ratio",
                when = function(x, ctx) ctx$pct_type %in% c("row", "col") ||
                  identical(ctx$vkind, "mean")),
  # shown on every row/col-% column, INCLUDING its own baseline (a whole column of 1s): "OR: 1" is
  # how a reader finds which column the ratio is read against.
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
#' @noRd
tab_tooltip_text <- function(x, .ref = NULL, .note = NULL, .base_n = NULL) {
  n   <- length(x)
  ctx <- tooltip_ctx(x, .ref, .base_n)
  ctx$note <- .note

  # two slots per line: the "ref" word takes the slot just before the first DEVIATION line, so it
  # lands where the comparison it replaces would have been read.
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
    # a reference cell says "ref" ONCE, as soon as the column HAS a deviation to state
    # (`any(nzchar(txt))`), never per cell -- and only on a DATA row, never a synthetic base-count one.
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
    # the baseline row's own value names nothing -- it is not the deviation the tag would claim, and
    # the row already says what it is ("Constant"). Its level and base count keep their names.
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

  # a statistical row is not a cell of the table -- both the row KIND and the token are checked, since
  # a footer p-value row displays `pvalue`, no model-fit token of its own.
  disp <- fmt_resolve_scale_tokens(display_primary(ctx$disp), ctx$scl)
  out[ctx$stat_row | disp %in% c(DISPLAY_GOF_TOKENS, "blank")] <- ""
  enc2utf8(out)
}

# The counts a BASE-COUNT column of the same table already prints, one column per block
# (tab_base_n_cols()): a matrix of them, or NULL where the table has none. Read by tip_render_n().
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
#' @noRd
tip_crude_level <- function(pct, inf, sup) {
  col <- fmt(n = NA_integer_, pct = pct, ci_inf = inf, ci_sup = sup, display = "{pct} {ci}",
             scale = "level_pct", pct_type = "row", role = "emp", digits = 0L)
  txt <- sub("\\]%$", "]", tip_num(format(col)), perl = TRUE)
  paste0(display_token_label("pct", col), ": ", txt)
}
