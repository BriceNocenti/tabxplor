# PURPOSE: THE DISPLAY GRAMMAR -- how a built cell decides what to print.
# ROLE: The `{}` template writer and its declared vocabulary (DISPLAY_TOKENS), plus the add_n /
#   add_pct materialisation the exporters run. A display is an OVERLAY: get_num(), the colour
#   engine and the Excel bypass all keep reading the PRIMARY field, so changing a display never
#   changes a number.
# KEY CONSTRAINTS:
#   - display_write_col() is THE per-column template writer, shared by build-time `tab(display =)`
#     and post-hoc `set_display(col, "num_ci")`. It refuses to print one geometry's estimate beside
#     another's bracket, but a LEVEL names no comparison and so constrains the bracket not at all --
#     "48% [-3;+4]" is tabxplor's flagship cell, not a mismatch.
#   - A token whose field is empty renders VOID and the note names the argument that would fill it;
#     it never silently substitutes the column's own primary field.
#   - WARNING: display_write_col()'s across() callback must stay a NAMED function -- dplyr inlines
#     an anonymous one into the data mask, `r$col` yields NULL, and across() DROPS the column.
#   - DISPLAY_TOKENS' row ORDER is a contract, and its `doc=` / `source=` strings are user-facing
#     documentation: display_tokens_rd() emits them into ?fmt and ?tab through an `@eval` block.
# See: CLAUDE.md § tabxplor architecture (the display grammar); R/fmt_class.R (the fields shown).

#' @keywords internal
#' @noRd
tab_apply_display <- function(tabs, display) {
  if (is.null(display) || length(display) == 0L) return(tabs)
  ds <- display[[1]]
  # "auto" is the jamovi ComboBox's idle value, and means what NULL / "" / "no" mean here.
  if (is.na(ds) || ds %in% c("", "no", "auto")) return(tabs)
  # A bare token is the same request as its one-field template, so accept both spellings.
  if (ds %in% DISPLAY_BARE_TOKENS) ds <- paste0("{", ds, "}")
  ds <- if (identical(ds, "num_ci")) ds else validate_display_template(ds)
  missing_tok <- character()
  write_col <- function(col) {
    r <- display_write_col(col, ds)
    missing_tok <<- union(missing_tok, r$missing)
    r$col
  }
  set_one <- function(tab) dplyr::mutate(tab, dplyr::across(dplyr::where(is_fmt), write_col))
  out <- if (is.data.frame(tabs)) set_one(tabs) else purrr::map(tabs, set_one)
  if (length(missing_tok)) display_note_empty(missing_tok)
  out
}

# Returns the column, plus the fields empty in the WHOLE column (only the table-level caller can
# note those once). `tmpl` is a validated {} template, or the type-adaptive alias "num_ci":
# "{pct} {ci}" on a percentage column, "{mean} {ci}" on a mean one, resolved per column.
#' @keywords internal
#' @noRd
display_write_col <- function(col, tmpl) {
  if (identical(tmpl, "num_ci"))
    tmpl <- paste0("{", if (identical(fmt_var_kind(col), "mean")) "mean" else "pct", "} {ci}")
  fields <- parse_display_template(tmpl)$fields
  # DESIGN: a ONE-FIELD "composite" must render as the pipeline's own bare token -- the composite
  # renderer calls format(special_formatting = FALSE), dropping the odds ratio's 1/x form and its
  # reference-cell annotation. DISPLAY_BARE_TOKENS only: the other fields have no simple renderer.
  bare <- if (length(fields) == 1L && identical(tmpl, paste0("{", fields, "}")) &&
              fields %in% DISPLAY_BARE_TOKENS) fields else tmpl
  d    <- get_display(col)
  # Only genuine value cells; p-value / blank / total-marker cells keep their own token. Reads the
  # RAW display, not display_primary(): a cell already carrying a composite is not re-templatable.
  elig <- d %in% DISPLAY_VALUE_CELLS
  if (!any(elig)) return(list(col = col, missing = character()))
  display_refuse_mismatch(col, fields, tmpl)
  # DESIGN: the void rule is PER-CELL -- the template is written on the cells carrying EVERY one of
  # its fields (a total row is the reference, so it has no difference interval and "{pct} {ci}"
  # leaves it a bare `pct`). The note fires only for a field empty in the whole column.
  missing <- character()
  for (f in fields) {
    have <- !is.na(get_num(set_display(col, f)))
    if (all(!have[elig])) missing <- union(missing, f)
    elig <- elig & have
  }
  d[elig] <- bare
  list(col = set_display(col, d), missing = missing)
}

# fmt_blank_fields() -- the helper rows / columns copy a real column and re-display it, so every
# field describing the ORIGINAL cell must go: keeping them would let a display switch, a colour
# measure or a tooltip read a number belonging to another quantity. `pct` is the only variable.
#' @keywords internal
#' @noRd
fmt_blank_fields <- function(col, pct = FALSE) {
  col <- set_diff(col, NA_real_) |> set_ci(NA_real_) |> set_mean(NA_real_)
  if (pct) col <- set_pct(col, NA_real_)
  set_ctr(col, NA_real_) |> set_var(NA_real_)
}

# =====================================================================================================
# DISPLAY_TOKENS -- THE per-token relation of the display grammar (a token names what a cell PRINTS).
#
# ⚠ ROW ORDER IS A CONTRACT. Rows 1-12 are the user-typeable fields in the order
# validate_display_template()'s "Valid fields" message and ?tab print them; rows 1-8 are additionally
# the bare tokens. Both derive by FILTERING, so the order is preserved by construction.
#
# ⚠ `OR` / `OR_pct` are ROWS, not aliases of `or` / `or_pct`: display_primary() returns a display
# verbatim and fmt_display_shows() compares the RAW value, so aliasing changes what a template matches.
#
# ⚠ WHY `settable` EXISTS: get_num() had arms set_num() lacked, and vec_arith goes through set_num(),
# so arithmetic on a column displaying `pct_ci` / `mean_ci` / `pvalue` silently returned it UNCHANGED
# (`x * 2` == `x`, no warning). The stopifnot() at the tail of this file keeps the three in step.
#
# THE HOT PATH STAYS HAND-WRITTEN: get_num()/set_num() are vectorised mask writes and format() is
# ~15 rendering-class masks crossed with the column's `scale`. This table drives the VOCABULARIES.
#
# COLUMNS
#   field      the fmt field get_num() reads. NA = the token has none of its own: `resid` is DERIVED
#              (fmt_resid(), from pvalue + sign(ctr)), `blank` prints nothing, and `est_ci` reads
#              whichever field the COLUMN's scale centres on -- fmt_center_field(), which is
#              EST_SCALES' vocabulary and deliberately not folded in here.
#   settable   set_num() writes the field back. FALSE only where there is nothing to write.
#   user       may be typed inside a {} template (and is named in the "Valid fields" message).
#   bare       a one-field template collapses onto this token, inheriting its own rendering.
#   value_cell display_write_col() may re-template a cell showing this -- a genuine value cell, as
#              opposed to a p-value / blank / total-marker cell, which keeps its own token.
#   footer     a footer STATISTIC, not data: it never carries a significance star, and a row whose
#              every cell is one is a regression's model-fit block (read black + bold, not greyed).
#   colour     may a cell showing this be coloured. `pvalue` is TRUE here while `footer` is also TRUE,
#              on purpose -- it is coloured as a significance warning (fmt_color_slots()). That one
#              disagreement is why this is two columns and not one "numberless".
#   geometry   which effect geometry the token NAMES, for the mismatch refusal. NA = it names none:
#              `ci` IS the bracket, and `ctr`/`var`/`resid`/`obs` are not estimates of a contrast.
#   comparison the colour MEASURE the token names, for the `color` -> `display` -> difference chain.
#   source     the argument that would fill an empty field, for the void note. NA where it always
#              exists (pct / n / wn), which display_note_empty() drops.
#   alias      this row is not a token but a legacy SPELLING of one, resolved by display_primary().
#   doc        what the token shows, one phrase, for the GENERATED ?fmt / ?tab sections
#              (display_tokens_rd()) -- user-facing documentation, written as such.
#
# The defaults below are the documentation: a row states only what is unusual about it.
#' @keywords internal
#' @noRd
.dtok <- function(field = NA_character_, settable = TRUE, user = FALSE, bare = FALSE,
                  value_cell = FALSE, footer = FALSE, colour = TRUE, geometry = NA_character_,
                  comparison = NA_character_, source = NA_character_, alias = NA_character_,
                  doc = NA_character_)
  list(field = field, settable = settable, user = user, bare = bare, value_cell = value_cell,
       footer = footer, colour = colour, geometry = geometry, comparison = comparison,
       source = source, alias = alias, doc = doc)

#' @keywords internal
#' @noRd
DISPLAY_TOKENS <- list(
  # --- the twelve a user may type, IN THE ORDER THEY ARE LISTED TO THEM -------------------------
  pct     = .dtok("pct" , user = TRUE, bare = TRUE, value_cell = TRUE, geometry = "level",
                  doc = 'the percentage'),
  n       = .dtok("n"   , user = TRUE, bare = TRUE, value_cell = TRUE, geometry = "level",
                  doc = 'the count'),
  wn      = .dtok("wn"  , user = TRUE, bare = TRUE, value_cell = TRUE, geometry = "level",
                  doc = 'the weighted count'),
  mean    = .dtok("mean", user = TRUE, bare = TRUE, value_cell = TRUE, geometry = "level",
                  source = 'a numeric col_var',
                  doc = 'the mean'),
  diff    = .dtok("diff" , user = TRUE, bare = TRUE, geometry = "difference",
                  comparison = "difference",
                  source = 'a `ref` to compare to, and pct = "row" / "col"',
                  doc = 'the difference from the reference'),
  ratio   = .dtok("ratio", user = TRUE, bare = TRUE, geometry = "ratio", comparison = "ratio",
                  source = 'a `ref` to compare to, and pct = "row" / "col"',
                  doc = 'the ratio to the reference (relative risk, or a ratio of means)'),
  ci      = .dtok("ci"   , user = TRUE, bare = TRUE,
                  source = 'ci = "ref"  (or ci = "cell" for each cell\'s own interval)',
                  doc = 'the confidence interval of whatever the column compares'),
  or      = .dtok("or"   , user = TRUE, bare = TRUE, geometry = "ratio", comparison = "odds_ratio",
                  source = 'pct = "row" / "col"  (an odds ratio needs a percentage base)',
                  doc = 'the odds ratio'),
  ctr     = .dtok("ctr"  , user = TRUE,
                  source = 'test = TRUE  (the contributions come from the chi-squared)',
                  doc = "the cell's contribution to the chi-squared"),
  var     = .dtok("var"  , user = TRUE, source = 'a numeric col_var',
                  doc = 'the variance'),
  resid   = .dtok(          user = TRUE, settable = FALSE,
                  source = 'test = TRUE  (the residual comes from the chi-squared)',
                  doc = paste('the adjusted standardized residual -- whether the cell departs from',
                              'independence. Derived from the p-value and the sign of `ctr`, so it',
                              'is read-only')),
  obs     = .dtok("obs"  , user = TRUE,
                  source = 'tab_reg(empirical = TRUE)  (an observed effect to compare the model to)',
                  doc = paste('the OBSERVED (crude) effect a modelled one is compared to.',
                              '`tab_reg()` tables only')),
  # --- the ten the PIPELINE writes; never user-typed ---------------------------------------------
  pct_ci  = .dtok("pct"   , doc = 'the percentage, with its interval printed beside it'),
  mean_ci = .dtok("mean"  , doc = 'the mean, with its interval printed beside it'),
  or_pct  = .dtok("or"    , doc = 'the odds ratio, with its percentage'),
  OR      = .dtok("or"    , doc = 'a legacy spelling of `or`, rendered identically'),
  OR_pct  = .dtok("or"    , doc = 'a legacy spelling of `or_pct`, rendered identically'),
  pvalue  = .dtok("pvalue", footer = TRUE,           # footer, yet deliberately coloured
                  doc = "a test's p-value"),
  coef    = .dtok("diff"  , doc = 'a regression coefficient, on its own scale'),
  gof     = .dtok("diff"  , footer = TRUE, colour = FALSE,
                  doc = 'a model-fit statistic (N, R2, AIC, BIC, dispersion)'),
  est_ci  = .dtok(          doc = paste('the estimate with a visible interval, reading whichever',
                                        'field the column\'s scale centres on')),
  blank   = .dtok(          settable = FALSE, footer = TRUE, colour = FALSE,
                  doc = 'nothing: a cell masked by `n_min`'),
  # --- a legacy SPELLING, resolved to its token by display_primary() -----------------------------
  rr      = .dtok(          settable = FALSE, alias = "ratio",
                  doc = 'the legacy synonym of `ratio`, still accepted')
)

#' @keywords internal
#' @noRd
.dtok_chr <- function(field)
  vapply(DISPLAY_TOKENS, function(r) r[[field]] %||% NA_character_, character(1))
#' @keywords internal
#' @noRd
.dtok_lgl <- function(field) vapply(DISPLAY_TOKENS, function(r) isTRUE(r[[field]]), logical(1))
# a real token, as opposed to an alias row -- every set below is of tokens
.dtok_real <- is.na(.dtok_chr("alias"))
#' @keywords internal
#' @noRd
.dtok_which <- function(field) names(DISPLAY_TOKENS)[.dtok_real & .dtok_lgl(field)]
#' @keywords internal
#' @noRd
.dtok_map <- function(field) {m <- .dtok_chr(field); m[!is.na(m)]}

# --- the vocabularies, DERIVED (each keeps its own name, shape and order) ------------------------
#' @keywords internal
#' @noRd
DISPLAY_USER_FIELDS    <- .dtok_which("user")
#' @keywords internal
#' @noRd
DISPLAY_ALIASES        <- .dtok_map("alias")
#' @keywords internal
#' @noRd
DISPLAY_BARE_TOKENS    <- .dtok_which("bare")
#' @keywords internal
#' @noRd
DISPLAY_VALUE_CELLS    <- .dtok_which("value_cell")
#' @keywords internal
#' @noRd
DISPLAY_FOOTER_TOKENS  <- .dtok_which("footer")
#' @keywords internal
#' @noRd
DISPLAY_NO_COLOR       <- names(DISPLAY_TOKENS)[.dtok_real & !.dtok_lgl("colour")]
#' @keywords internal
#' @noRd
DISPLAY_SETTABLE       <- .dtok_which("settable")
#' @keywords internal
#' @noRd
DISPLAY_TOKEN_GEOMETRY <- .dtok_map("geometry")
#' @keywords internal
#' @noRd
DISPLAY_COMPARISON     <- .dtok_map("comparison")
#' @keywords internal
#' @noRd
DISPLAY_FIELD_SOURCE   <- .dtok_map("source")


# --- the GENERATED help section -----------------------------------------------------------------
# Called from a roxygen `@eval` block, so ?fmt / ?tab cannot drift from the tokens the package has.
#   user_only = TRUE -> ?tab: the twelve a user may type. FALSE -> ?fmt: every token.
#' @keywords internal
#' @noRd
display_tokens_rd <- function(user_only = TRUE) {
  esc <- function(s) {
    s <- gsub("%", "\\\\%", gsub("\\", "\\\\", s, fixed = TRUE))
    gsub("`([^`]+)`", "\\\\code{\\1}", s)
  }
  toks <- names(DISPLAY_TOKENS)
  toks <- if (user_only) intersect(toks, DISPLAY_USER_FIELDS) else toks
  # ?fmt already glosses each same-name field above (fmt_fields_rd): name it here, gloss it there.
  same_name <- if (user_only) character(0) else intersect(toks, fmt_field_names)
  toks      <- setdiff(toks, same_name)
  line <- function(tk) {
    r    <- DISPLAY_TOKENS[[tk]]
    doc  <- if (is.na(r$doc)) "" else paste0(" --- ", esc(r$doc))
    need <- if (!user_only || is.na(r$source)) "" else paste0(". Needs ", esc(r$source))
    paste0("  \\item \\code{", tk, "}", doc, need, ".")
  }
  c(if (user_only) "@section Display fields:" else "@section Every display token:",
    if (user_only)
      c("The fields a \\code{\\{\\}} template may name. The first one is the \\emph{primary}:",
        "it is what Excel shows and what the colours read.")
    else
      c("Generated from the package's own display table, so it cannot drift from what",
        "\\code{get_num()} reads. Each of",
        paste0(paste0("\\code{", same_name, "}", collapse = ", "), " shows the field of the same"),
        "name, described above. The rest are composed or derived by the pipeline itself, and",
        "the last few are not meant to be typed:"),
    "\\itemize{", vapply(toks, line, character(1)), "}")
}

#' @keywords internal
#' @noRd
display_note_empty <- function(fields) {
  hints <- DISPLAY_FIELD_SOURCE[fields]
  hints <- hints[!is.na(hints)]
  cli::cli_inform(c(
    "i" = paste0("{.arg display}: {cli::qty(length(fields))}{?field/fields} ",
                 "{.val {fields}} {?is/are} empty in this table, so {?it renders/they render} void."),
    stats::setNames(paste0("{.field ", names(hints), "} needs ", unname(hints), "."),
                    rep("i", length(hints)))
  ))
}

# Refuse a `{ci}` bracket whose geometry differs from the estimate beside it: the stored interval is
# the one this column's comparison is tested on, so `x1.8 ([2;4]%)` -- a ratio over a percentage-POINT
# interval -- aborts instead of printing. The `scale` and the token's geometry make it a lookup.
#' @keywords internal
#' @noRd
display_refuse_mismatch <- function(col, fields, tmpl) {
  if (!"ci" %in% fields) return(invisible(NULL))
  est <- intersect(fields, names(DISPLAY_TOKEN_GEOMETRY))
  if (!length(est)) return(invisible(NULL))
  have <- EST_SCALES[[get_scale(col)]]$geometry
  if (is.null(have)) return(invisible(NULL))
  want <- unname(DISPLAY_TOKEN_GEOMETRY[est[1]])
  if (identical(have, want)) return(invisible(NULL))
  # WARNING: a LEVEL names no comparison, so it constrains the bracket not at all -- "48% [-3;+4]"
  # (a percentage beside the difference interval it was tested on) is tabxplor's flagship cell, and
  # `display = "num_ci"` is exactly that template. The class this refusal closes is TWO EFFECT
  # geometries disagreeing, never a level.
  if ("level" %in% c(have, want)) return(invisible(NULL))
  cli::cli_abort(c(
    "{.arg display} = {.val {tmpl}} prints a {.field {want}} beside a {.field {have}} interval.",
    "x" = "A cell carries ONE interval, and this column's is on the {.field {have}} scale.",
    "i" = paste0("Ask for the matching comparison ({.code color = \"",
                 # a colour measure IS the geometry, bar a LEVEL and any MEASURES cannot name.
                 if (identical(have, "level")) "no"
                 else if (have %in% names(MEASURES)) have else "difference",
                 "\"} / {.code ci = }), or drop the {.code {{ci}}} bracket.")
  ))
}


# tab_append_pctcol_rows() -- under pct = "col" the add_n / add_pct extras are ROWS: a re-displayed
# copy of EACH sub-table's total row, spliced in after its own source row. `transform` returns the
# row(s) to insert; `role` is the row_kind ("pct" / "n") stamped on their cells (NA = don't stamp).
# Slicing runs UNGROUPED -- slice() would index within each group, the total-row index is global.
# WARNING: the group column must NOT be relabelled -- the copy keeps its sub-table's `row_var` value
# so it stays inside that group; `transform` only relabels tab_get_vars()$row_var.
tab_append_pctcol_rows <- function(tab, transform, role = NA_character_) {
  gv   <- dplyr::group_vars(tab)
  flat <- dplyr::ungroup(tab)
  n0   <- nrow(flat)
  tot  <- is_totrow(flat) & !is_placeholder_var(tab_get_vars(flat)$row_var)
  if (!any(tot)) return(tab)
  gid  <- if (length(gv) > 0) dplyr::group_indices(tab) else rep(1L, n0)
  grps <- unique(gid[tot])
  # SOURCE = each sub-table's last total row; ANCHOR = the END of that sub-table. They differ once a
  # previous pass inserted an extra (add_pct runs first); anchoring on the end keeps the order.
  src    <- vapply(grps, function(g) { i <- which(tot & gid == g); i[[length(i)]] }, integer(1))
  anchor <- vapply(grps, function(g) { i <- which(gid == g);       i[[length(i)]] }, integer(1))
  ord    <- order(src)
  src    <- src[ord]; anchor <- anchor[ord]
  added  <- transform(dplyr::slice(flat, src))
  if (!is.na(role))
    added <- dplyr::mutate(added, dplyr::across(dplyr::where(is_fmt), ~ set_row_kind(., role)))
  out    <- dplyr::bind_rows(flat, added)
  # splice: bind_rows put the new rows at the very end, so re-order by "just after my sub-table".
  reord  <- order(c(seq_len(n0), anchor + 0.5))
  out    <- dplyr::slice(out, reord)
  if (length(gv) > 0) dplyr::group_by(out, dplyr::across(tidyselect::all_of(gv))) else out
}


# tab_add_n_pct() -- append the base-n column (add_n) and/or the col%/row% companion (add_pct) to
# each built factor table (the tabs_text LIST, one entry per row_var); shared by tab_many() and
# tab_counts(). The `n` COLUMN is "xl"-only: text folds the base into the Total cell instead
# (tab_fold_addn_incell). The pct = "col" rows are backend-invariant.
tab_add_n_pct <- function(tabs_text, add_n, add_pct, backend = "xl") {
  if (!add_n && !add_pct) return(tabs_text)

    # cols, with pct = "row"
    last_totcols_pct_rows <- tabs_text |>
      purrr::imap_chr(
        ~ dplyr::last(names(.x)[is_totcol(.x) & get_pct_base(.x) == "row" &
                                  is_real_col_var(get_col_var(.x)) &
                                  !is_placeholder_var(tab_get_vars(.)$row_var)]) |>
          purrr::set_names(.y)
      )

    last_totcols_pct_rows <- last_totcols_pct_rows[!is.na(last_totcols_pct_rows)]

    if (length(last_totcols_pct_rows) > 0) {
      if (add_pct) {
        tabs_text <- tabs_text |>
          purrr::map2(
            last_totcols_pct_rows,
            ~ dplyr::mutate(
              .x,
              col_pct := dplyr::mutate(
                !!rlang::sym(.y),
                pct = get_wn(!!rlang::sym(.y)) /
                  dplyr::last(get_wn(!!rlang::sym(.y)),
                  )
              ) |>
                set_scale("level_pct") |> set_pct_base("col") |>
                as_totcol(FALSE) |> set_color("no") |>
                # a whole-table helper: no col_var, and its `role` says what it is
                set_col_var("") |> set_role("pct") |>
                fmt_blank_fields(pct = FALSE)
            )
          )
      }

      if (add_n && !identical(backend, "text")) {
        tabs_text <- tabs_text |>
          purrr::map2(
            last_totcols_pct_rows, ~ dplyr::mutate(
              .x,
              n = set_display(!!rlang::sym(.y), "n") |>
                set_count_col() |> as_totcol(FALSE) |> set_color("no") |>
                set_col_var("") |> set_role("n") |>
                fmt_blank_fields(pct = TRUE)
            )
          )
      }

    }


    # rows, with pct = "col"
    last_totrow <- tabs_text |>
      purrr::map_int(
        ~ dplyr::last(which(is_totrow(.) & !is_placeholder_var(tab_get_vars(.)$row_var)),
                      default = NA_integer_)
      )
    last_totrow <- last_totrow[!is.na(last_totrow)]
    if (length(last_totrow) > 0) {


      last_totrow_pct_cols <- tabs_text |>
        purrr::map(~ names(.)[get_pct_base(.) == "col" & is_real_col_var(get_col_var(.)) &
                                 names(.) != "col_pct"] )
      last_totrow_pct_cols_no_empty <- purrr::map_lgl(last_totrow_pct_cols, ~ length(.) > 0)


      if (any(last_totrow_pct_cols_no_empty)) {

        if (add_pct) {
          tabs_text <-
            purrr::pmap(
              list(tabs_text, last_totrow_pct_cols_no_empty, last_totrow_pct_cols),
              ~ {
                totcols_ref <- purrr::map_chr(detect_totcols(..1), as.character)
                val_cols    <- ..3
                row_lab     <- tab_get_vars(..1)$row_var
                if (..2) {
                  tab_append_pctcol_rows(..1, function(src) {
                    src |>
                      dplyr::mutate(
                        dplyr::across(
                          where(is_fmt),
                          ~ dplyr::mutate(
                            .,
                            pct = get_wn(.) /
                              get_wn(rlang::eval_tidy(
                                rlang::sym(totcols_ref[[dplyr::cur_column()]])
                              ))
                          )
                        ),
                        dplyr::across(where(is_fmt),
                                      ~ fmt_blank_fields(as_totrow(., FALSE), pct = FALSE)),
                        dplyr::across(
                          where(is_fmt) & -tidyselect::all_of(val_cols),
                          ~ set_num(., value = NA_real_)
                        ),
                        dplyr::across(
                          all_of(row_lab),
                          # WARNING: a declared index column must keep its declaration
                          ~ lvl_restore(factor("row_pct"), .)
                        )
                      )
                  }, role = "pct")
                } else {
                  ..1
                }
              }
            )
        }

        if (add_n) {
          tabs_text <-
            purrr::pmap(list(tabs_text, last_totrow_pct_cols_no_empty, last_totrow_pct_cols),
                        ~ {
                          val_cols <- ..3
                          row_lab  <- tab_get_vars(..1)$row_var
                          if (..2) {
                            tab_append_pctcol_rows(..1, function(src) {
                              src |> set_display("n") |>
                                dplyr::mutate(
                                  dplyr::across(where(is_fmt),
                                                ~ fmt_blank_fields(as_totrow(., FALSE), pct = TRUE)),
                                  dplyr::across(
                                    where(is_fmt) & -tidyselect::all_of(val_cols),
                                    ~ set_num(., value = NA_real_)
                                  ),
                                  dplyr::across(
                                    all_of(row_lab),
                                    # WARNING: a declared index column must keep its declaration
                                    ~ lvl_restore(factor("n"), .)
                                  )
                                )
                            }, role = "n")
                          } else {
                            ..1
                          }
                        }
            )
        }

      }

    }


  tabs_text
}


# tab_is_or_display() -- TRUE when the table DISPLAYS odds ratios, so its "100%" total is meaningless
# and gets folded to the base n / dropped. Keyed on the DISPLAYED quantity, NOT ci_type: `color =
# "OR"` with `OR = "no"` shows real percentages (a meaningful 100%) yet can carry an OR interval.
tab_is_or_display <- function(tab) {
  if (!is.data.frame(tab)) return(FALSE)
  fc <- purrr::map_lgl(tab, is_fmt)
  if (!any(fc)) return(FALSE)
  any(purrr::map_lgl(tab[fc], ~ any(get_display(.) %in% c("or", "or_pct"))))
}

# tab_fold_addn_incell() -- for TEXT backends the add_n base shows inside the Total cell, as the
# composite `{pct} (n={n})` read from that column's OWN `n` field: the only base a cell can honestly
# show, since format() aligns per unique template and a per-row literal (a `[min;max]` over col_vars
# with differing bases) defeats that padding. On an OR/RRR table the "100%" goes, leaving `n={n}`.
# WARNING: runs BEFORE tab_pvalue_lines(), so the Total column has only data/total cells.
tab_fold_addn_incell <- function(tab) {
  tot_nm <- dplyr::last(names(tab)[is_totcol(tab) & get_pct_base(tab) == "row" &
                                     is_real_col_var(get_col_var(tab))])
  if (length(tot_nm) != 1 || is.na(tot_nm)) return(dplyr::select(tab, -tidyselect::any_of("n")))
  is_or <- tab_is_or_display(tab)

  tmpl <- if (is_or) rep("n={n}", nrow(tab)) else NULL

  tab <- dplyr::select(tab, -tidyselect::any_of("n"))
  dplyr::mutate(tab, dplyr::across(tidyselect::all_of(tot_nm), function(col) {
    d    <- get_display(col)
    # both fields must render; here the Total column is all pct/n non-NA, so this is every cell.
    elig <- !is.na(get_num(set_display(col, "pct"))) & !is.na(get_num(set_display(col, "n")))
    if (is.null(tmpl)) d[elig] <- "{pct} (n={n})" else d[elig] <- tmpl[elig]
    set_display(col, d)
  }))
}

# tab_or_total_col() -- what the in-cell fold cannot cover: drop the meaningless "100%" column for
# EXCEL (the base n is its own column there) and for console add_n = FALSE (no base to fold).
tab_or_total_col <- function(tab, backend, add_n_on) {
  if (!is.data.frame(tab) || !tab_is_or_display(tab)) return(tab)
  tot_nm <- names(tab)[purrr::map_lgl(tab, ~ is_fmt(.) && is_totcol(.) &&
                                        get_pct_base(.) == "row" && is_real_col_var(get_col_var(.)))]
  if (!length(tot_nm)) return(tab)
  if (identical(backend, "xl") || !isTRUE(add_n_on)) {
    tab <- dplyr::select(tab, -tidyselect::all_of(tot_nm))
  }
  tab
}


# tab_apply_n_min() -- the small-base display filter. A PURE DISPLAY helper: it recomputes NOTHING
# (no fields, no chi2/ANOVA, no CI), it just strips the noise of unreliable small-base cells.
# Rule: for row-oriented columns (a row / all-tabs percentage, or a mean) drop a row only if its
# LARGEST base across them is < n_min, then blank each surviving cell whose OWN base is < n_min; for
# col-oriented ones (pct = "col") drop the whole column when its base is < n_min. Orientation comes
# from stored facts, so mixed tables just work. Base = get_tot_n() for proportions, get_n() for
# means; an NA base is never weak. NEVER drops: total rows/tables, the total column, the add_n /
# add_pct helper rows and columns, or the p-value line.
tab_apply_n_min <- function(tab, n_min) {
  if (length(n_min) == 0 || is.na(n_min[1]) || n_min[1] <= 0) return(tab)
  n_min <- n_min[1]
  if (!is.data.frame(tab)) return(tab)

  fmt_names <- names(tab)[purrr::map_lgl(tab, is_fmt)]
  if (length(fmt_names) == 0) return(tab)

  base   <- purrr::map_chr(tab[fmt_names], get_pct_base)
  vkind  <- purrr::map_chr(tab[fmt_names], fmt_var_kind)
  row_like <- base %in% c("row", "all") | vkind == "mean"
  totcol <- purrr::map_lgl(tab[fmt_names], is_totcol)

  cell_base <- function(col) if (fmt_var_kind(col) == "mean") get_n(col) else get_tot_n(col)

  # --- protected rows (never dropped) --------------------------------------------------------
  # n_min runs at build, on the CORE table: the helper rows and columns only exist at display time.
  fmt_all <- tab[fmt_names]
  totrow  <- purrr::reduce(purrr::map(fmt_all, is_totrow), `|`)
  tottab  <- purrr::reduce(purrr::map(fmt_all, is_tottab), `|`)
  protect <- totrow | tottab

  # --- row-drop + cell-blank on row-oriented columns -----------------------------------------
  row_cols <- fmt_names[row_like]                           # totcol INCLUDED in the max
  if (length(row_cols) > 0) {
    bases    <- purrr::map(tab[row_cols], ~ { b <- cell_base(.); b[is.na(b)] <- Inf; b })
    row_base <- purrr::reduce(bases, pmax)
    keep     <- protect | !(row_base < n_min)
    if (!all(keep)) {
      # a grouped_tab would split the length-n `keep` per group: ungroup, filter, restore.
      gv  <- dplyr::group_vars(tab)
      tab <- dplyr::ungroup(tab)
      tab <- dplyr::filter(tab, keep)
      if (length(gv) > 0) tab <- dplyr::group_by(tab, dplyr::across(tidyselect::all_of(gv)))
    }
  }
  blank_cols <- fmt_names[row_like & !totcol]
  blank_cols <- intersect(blank_cols, names(tab))
  if (length(blank_cols) > 0) {
    tab <- dplyr::mutate(tab, dplyr::across(
      tidyselect::all_of(blank_cols),
      ~ {
        b <- cell_base(.)
        w <- !is.na(b) & b < n_min
        if (any(w)) .[w] <- set_display(.[w], "blank")
        .
      }
    ))
  }

  # --- column-drop on col-oriented columns (pct = "col") -------------------------------------
  drop_cols <- fmt_names[base == "col" & !totcol]
  drop_cols <- intersect(drop_cols, names(tab))
  if (length(drop_cols) > 0) {
    weak <- purrr::map_lgl(tab[drop_cols], ~ {
      mb <- suppressWarnings(max(get_tot_n(.), na.rm = TRUE))
      is.finite(mb) && mb < n_min
    })
    if (any(weak)) tab <- dplyr::select(tab, -tidyselect::all_of(drop_cols[weak]))
  }

  tab
}


# =====================================================================================================
# BUILD-TIME EXHAUSTIVENESS -- the two switches and the table say the same thing, or the package does
# not install.
#
# ⚠ This block must sit HERE, not in R/fmt_class.R: get_num()/set_num() live there, DISPLAY_TOKENS
# lives here, and fmt_class.R sorts FIRST in R's C collation -- this is the first file where all
# three are in scope. Move it and the package stops installing.
#
# ⚠ SCOPE: get_num() and set_num() only. Both are pure per-token maps, so every length-1 character
# constant in their bodies IS a token, making the check TWO-directional (an undeclared arm and an
# unhandled row both fail). format() is excluded: its body is full of unrelated constants, so the
# same walk would assert nothing.
#' @keywords internal
#' @noRd
display_switch_tokens <- function(fn) {
  out <- character()
  walk <- function(e) {
    if (is.character(e) && length(e) == 1L) out <<- c(out, e)
    else if (is.call(e) || is.pairlist(e)) for (i in seq_along(e)) walk(e[[i]])
  }
  walk(body(fn))
  unique(out)
}

local({
  declared <- names(DISPLAY_TOKENS)[.dtok_real]
  read     <- display_switch_tokens(get_num)
  written  <- display_switch_tokens(set_num)
  # `n` is get_num()'s fall-through initialiser, so it never appears there as a literal.
  stopifnot(
    "every token get_num() reads must be declared in DISPLAY_TOKENS" =
      all(read %in% declared),
    "every declared token must be read by get_num()" =
      all(setdiff(declared, "n") %in% read),
    "every token set_num() writes must be declared in DISPLAY_TOKENS" =
      all(written %in% declared),
    "every token declared `settable` must have a set_num() arm" =
      all(DISPLAY_SETTABLE %in% written)
  )
})
