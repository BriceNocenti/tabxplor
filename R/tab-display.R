# PURPOSE: THE DISPLAY GRAMMAR -- how a built cell decides what to print. The `{}` template writer
#   and its vocabulary, plus the add_n / add_pct materialisation the exporters run.
# ROLE: Carved out of R/tab.R by Phase 19l (whole functions, no behaviour change).
# KEY CONSTRAINTS:
#   - display_write_col() is THE per-column template writer, shared by build-time tab(display =) and
#     post-hoc set_display(col, "num_ci"). D22 is PER-CELL there (a template is written only where
#     every one of its fields exists) and D23 refuses two EFFECT geometries, never a LEVEL beside a
#     comparison interval ("48% [-3;+4]" IS the flagship cell).
#   - WARNING: display_write_col()\'s across() callback must stay a NAMED function -- dplyr inlines an
#     anonymous one into the data mask, `r$col` yields NULL, and across() DROPS the column.
#   - A display is an OVERLAY: get_num(), the colour engine and the Excel bypass all keep reading the
#     PRIMARY field. Changing a display never changes a number.
# See: CLAUDE.md Repository Map > R/tab-display.R.


# Phase 10i-A: apply an opt-in COMPOSITE display recipe (curated sugar "pct (n)"/"n (pct)"/"pct_n",
# or a raw "{pct} (n={n})" template) to a built table (single tab, grouped tab, or a list of tabs).
# It is a DISPLAY overlay only (text backends via format()); get_num(), coloring and the Excel bypass
# keep showing the PRIMARY field. validate_display_template() checks the {} template (aborts on
# bad input); the {} template is written into the `display` FIELD but ONLY on genuine value cells, so
# the already-present p-value / blank / total-marker cells keep their own token (this write runs last
# in tab(), after those rows exist).
#' @keywords internal
tab_apply_display <- function(tabs, display) {
  if (is.null(display) || length(display) == 0L) return(tabs)
  ds <- display[[1]]
  # "auto" = "every column keeps the display it was built with", i.e. exactly what NULL / "" / "no"
  # already mean. It is the jamovi ComboBox's idle value, which reached tab() only through the armed
  # build (`.return_armed = TRUE` returns before this tail), so the two vocabularies silently differed
  # by one token -- and any caller mirroring the jamovi options had to state the mapping itself.
  if (is.na(ds) || ds %in% c("", "no", "auto")) return(tabs)
  # A BARE token is the same request as its one-field template, so accept it: `display = "n"` reads
  # better than `display = "{n}"`, and it is the spelling the jamovi ComboBox has always used --
  # which is why the module had to keep its own writer, and why that writer stamped the literal
  # "{or}" where this one normalises back to the bare `or` the pipeline itself writes.
  if (ds %in% DISPLAY_BARE_TOKENS) ds <- paste0("{", ds, "}")
  ds <- if (identical(ds, "num_ci")) ds else validate_display_template(ds)
  missing_tok <- character()
  # WARNING: this must be a NAMED function, never an anonymous one written inside across(). dplyr
  # INLINES an anonymous `.fns` body into the mutate expression, where `r <- display_write_col(...)`
  # then `r$col` resolves against the data mask and yields NULL -- and a NULL from across() DROPS the
  # column. Measured: every <fmt> column of the table silently disappeared, leaving only the labels.
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

# display_write_col() -- THE one writer of a display template onto a column, shared by the build-time
# `tab(display =)` (tab_apply_display, over a table) and the post-hoc `set_display(col, "num_ci")`
# (over a lone column, no table in scope). Phase 19e folded the second copy in: they had drifted --
# the alias skipped a cell whose interval is empty, the template wrote the composite anyway, so
# `display = "num_ci"` and its own documented equivalent `display = "{pct} {ci}"` disagreed on every
# total row. Returns the column plus the fields that were empty in the WHOLE column (D22's note,
# which only the table-level caller can sensibly emit once).
#
# `tmpl` is a validated {} template, or the type-adaptive alias "num_ci": not a single template but
# "{pct} {ci}" on a percentage column and "{mean} {ci}" on a mean one, so a mixed table resolves each
# column by its own kind.
#' @keywords internal
#' @noRd
display_write_col <- function(col, tmpl) {
  if (identical(tmpl, "num_ci"))
    tmpl <- paste0("{", if (identical(fmt_var_kind(col), "mean")) "mean" else "pct", "} {ci}")
  fields <- parse_display_template(tmpl)$fields
  # Phase 19d: a ONE-FIELD "composite" is not a composite -- it is that field's own display, and it
  # must render exactly as the pipeline's own token does. Writing "{or}" as the bare `or` token is
  # what makes `display = "{or}"` a faithful front door for the retired `OR = "OR"`: the composite
  # renderer calls format(special_formatting = FALSE) on each token, which drops the odds ratio's
  # 1/x form and its reference-cell annotation. One general rule, no curated recipe.
  # Restricted to the tokens the pipeline itself writes as a bare display: `resid` is derived and
  # `obs`/`var`/`ctr` have no simple-token renderer of their own.
  bare <- if (length(fields) == 1L && identical(tmpl, paste0("{", fields, "}")) &&
              fields %in% DISPLAY_BARE_TOKENS) fields else tmpl
  d    <- get_display(col)
  # Only genuine value cells -- the p-value / blank / total-marker cells keep their own token.
  elig <- d %in% c("pct", "mean", "n", "wn")
  if (!any(elig)) return(list(col = col, missing = character()))
  # Phase 19d (D23): the stored interval is the one this column's comparison is tested on, and a
  # `{ci}` bracket renders THAT interval. A template that prints one geometry's estimate beside
  # another's bracket is REFUSED (ruling d) rather than silently printed -- measured today as
  # `x1.8 ([2;4]%)`, a ratio over a percentage-POINT interval.
  display_refuse_mismatch(col, fields, tmpl)
  # Phase 19d (D22): a token whose field is empty renders VOID, and the note names the argument that
  # would fill it. It used to silently SUBSTITUTE the column's own primary field -- so
  # `display = "{or}"` on a table with no odds ratio printed the percentage, and the stored `display`
  # came back `pct`: a plausible table that is not the one asked for.
  # It is a PER-CELL rule: the template is written on the cells that can carry EVERY one of its
  # fields (a total row is the reference, so it has no difference interval and "{pct} {ci}" leaves it
  # a bare `pct`). The note fires only where a field is empty in the whole column -- the case where
  # what the user asked for is genuinely not in this table.
  missing <- character()
  for (f in fields) {
    have <- !is.na(get_num(set_display(col, f)))
    if (all(!have[elig])) missing <- union(missing, f)
    elig <- elig & have
  }
  d[elig] <- bare
  list(col = set_display(col, d), missing = missing)
}

# The display tokens the PIPELINE itself writes as a bare value (so a one-field template collapses
# onto them and inherits their rendering). `resid` is derived and `obs`/`var`/`ctr` have no
# simple-token renderer, so they stay composites.
#' @keywords internal
#' @noRd
DISPLAY_BARE_TOKENS <- c("pct", "n", "wn", "mean", "diff", "ratio", "ci", "or")

# The argument that would fill each display field -- so D22's note can NAME it instead of leaving
# the user with a blank column. One declared table, read only by display_note_empty().
#' @keywords internal
#' @noRd
DISPLAY_FIELD_SOURCE <- c(
  ci    = 'ci = "ref"  (or ci = "cell" for each cell\'s own interval)',
  or    = 'pct = "row" / "col"  (an odds ratio needs a percentage base)',
  diff  = 'a `ref` to compare to, and pct = "row" / "col"',
  ratio = 'a `ref` to compare to, and pct = "row" / "col"',
  ctr   = 'test = TRUE  (the contributions come from the chi-squared)',
  resid = 'test = TRUE  (the residual comes from the chi-squared)',
  obs   = 'tab_reg(empirical = TRUE)  (an observed effect to compare the model to)',
  mean  = 'a numeric col_var',
  var   = 'a numeric col_var'
)

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

# Phase 19d (D23): refuse a `{ci}` bracket whose geometry differs from the estimate beside it. The
# column's stored `scale` says which estimate its interval belongs to (KEY 2), and each display
# token names a geometry, so this is a lookup, not a heuristic -- and it is the ONLY thing that can
# close the class, because `display` must stay free (differentiator 1) and no argument can reach it.
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
  # `display = "num_ci"` is literally that template. The class this refusal closes is TWO EFFECT
  # geometries disagreeing (`{ratio} {ci}` over a percentage-POINT interval), never a level.
  if ("level" %in% c(have, want)) return(invisible(NULL))
  cli::cli_abort(c(
    "{.arg display} = {.val {tmpl}} prints a {.field {want}} beside a {.field {have}} interval.",
    "x" = "A cell carries ONE interval, and this column's is on the {.field {have}} scale.",
    "i" = paste0("Ask for the matching comparison ({.code color = \"", 
                 c(level = "no", difference = "difference", ratio = "ratio",
                   log = "log")[have] %||% "difference",
                 "\"} / {.code ci = }), or drop the {.code {{ci}}} bracket.")
  ))
}

#' @keywords internal
#' @noRd
DISPLAY_TOKEN_GEOMETRY <- c(pct = "level", mean = "level", n = "level", wn = "level",
                            diff = "difference", ratio = "ratio", or = "ratio")


# tab_append_pctcol_rows() -- Phase 14a. Under pct = "col" the add_n / add_pct extras are ROWS: a
# re-displayed copy of a sub-table's total row. `transform` takes the sliced source row(s) and
# returns the row(s) to insert. Two bugs lived in the inline `bind_rows(tab, slice(tab, last_totrow))`
# this replaces:
#   1. `last_totrow` is a GLOBAL row index (is_totrow.data.frame is not group-aware), but a merged
#      multi-row_var tab is a grouped_df where dplyr::slice() indexes WITHIN each group. No group had
#      that many rows, so slice() returned ZERO rows and bind_rows() silently dropped the extra --
#      the reported "the n row disappears with several row_vars". Fix: slice on the ungrouped tab.
#   2. only the LAST total row of the whole table was copied, and appended at the very bottom. With
#      several row_vars that single row would sit under the last sub-table as if it belonged to it.
#      Fix: one row per sub-table, spliced in right after its OWN source row.
# Byte-identical wherever a table has one sub-table whose total row is last (every shape the goldens
# cover): one source row, spliced after the last row == appended.
# WARNING: the group column must NOT be relabelled -- the copy keeps its sub-table's `row_var` value
# so it stays inside that group; `transform` only relabels tab_get_vars()$row_var (= "levels" on a
# compacted tab, the real row_var otherwise).
# Phase 19f: `role` -- the row_kind ("pct" / "n") STAMPED on the appended rows' fmt cells. It rides
# the rows through the splice below with no bookkeeping at all; 17c had to extend a positional vector
# by K and push it through the same re-order to keep it aligned. NA = don't stamp (a non-materialiser
# caller).
tab_append_pctcol_rows <- function(tab, transform, role = NA_character_) {
  gv   <- dplyr::group_vars(tab)
  flat <- dplyr::ungroup(tab)
  n0   <- nrow(flat)
  tot  <- is_totrow(flat) & tab_get_vars(flat)$row_var != "no_row_var"
  if (!any(tot)) return(tab)
  gid  <- if (length(gv) > 0) dplyr::group_indices(tab) else rep(1L, n0)
  grps <- unique(gid[tot])
  # SOURCE = each sub-table's last total row; ANCHOR = the END of that sub-table. They differ once a
  # previous pass has already inserted an extra (add_pct runs before add_n), and anchoring on the
  # group's end is what keeps the historical `Total | row_pct | n` order -- with one ungrouped
  # sub-table it is exactly the old `bind_rows(tab, ...)` append.
  src    <- vapply(grps, function(g) { i <- which(tot & gid == g); i[[length(i)]] }, integer(1))
  anchor <- vapply(grps, function(g) { i <- which(gid == g);       i[[length(i)]] }, integer(1))
  ord    <- order(src)
  src    <- src[ord]; anchor <- anchor[ord]
  added  <- transform(dplyr::slice(flat, src))
  if (!is.na(role))                                  # the new rows say what they ARE, in the record
    added <- dplyr::mutate(added, dplyr::across(dplyr::where(is_fmt), ~ set_row_kind(., role)))
  out    <- dplyr::bind_rows(flat, added)
  # splice: bind_rows put the new rows at the very end, so re-order by "just after my sub-table".
  reord  <- order(c(seq_len(n0), anchor + 0.5))
  out    <- dplyr::slice(out, reord)
  if (length(gv) > 0) dplyr::group_by(out, dplyr::across(tidyselect::all_of(gv))) else out
}


# tab_add_n_pct() -- append the base-n column (add_n) and/or the col%/row% companion
# (add_pct) to each built factor table. Extracted verbatim from tab_many()'s finalize so
# BOTH tab_many() and tab_counts() share ONE implementation (no divergence). Operates on the
# tabs_text LIST (one entry per row_var); returns it modified. See CLAUDE.md Phase 4.
# Phase 17g: `backend` -- the TEXT backends fold the add_n base into the Total cell directly from its
# own `n` field (tab_fold_addn_incell), so the separate `n` COLUMN would only be built to be dropped.
# It is therefore built for "xl" ONLY (default "xl" = build it, for any caller not naming a backend);
# text skips it. The pct = "col" `n` ROW + the add_pct col_pct / row_pct companions are backend-invariant.
tab_add_n_pct <- function(tabs_text, add_n, add_pct, backend = "xl") {
  if (!add_n && !add_pct) return(tabs_text)

    # cols, with pct = "row"
    last_totcols_pct_rows <- tabs_text |>
      purrr::imap_chr(
        ~ dplyr::last(names(.x)[is_totcol(.x) & get_pct_base(.x) == "row" &
                                  get_col_var(.x) != "no_col_var" &
                                  tab_get_vars(.)$row_var != "no_row_var"]) |>
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
                              #which(get_reference(!!rlang::sym(.y), "lines"))
                  )
              ) |>
                set_scale("level_pct") |> set_pct_base("col") |>
                as_totcol(FALSE) |> set_color("no") |>
                # 19l: DECLARED as a whole-table helper (role), with no col_var -- it used to borrow
                # the "all_col_vars" tag, whose other, opposite meaning is the legacy grand total.
                set_col_var("") |> set_role("pct") |>
                set_diff(NA_real_) |> set_ci(NA_real_) |> set_mean(NA_real_) |>
                set_ctr(NA_real_) |> set_var(NA_real_)
            )
          )
      }

      # Phase 17g: the add_n `n` COLUMN is an Excel-only layout column -- text folds the base into the
      # Total cell instead (tab_fold_addn_incell), so building it there just to drop it is skipped.
      if (add_n && !identical(backend, "text")) {
        tabs_text <- tabs_text |>
          purrr::map2(
            last_totcols_pct_rows, ~ dplyr::mutate(
              .x, # !!rlang::sym(paste0(names(.y), "_n"))
              n = set_display(!!rlang::sym(.y), "n") |>
                set_count_col() |> as_totcol(FALSE) |> set_color("no") |>
                set_col_var("") |> set_role("n") |>
                set_diff(NA_real_) |> set_ci(NA_real_) |> set_mean(NA_real_) |>
                set_pct(NA_real_) |> set_ctr(NA_real_) |> set_var(NA_real_)
            )
          )
      }

    }


    # rows, with pct = "col"
    last_totrow <- tabs_text |>
      purrr::map_int(
        ~ dplyr::last(which(is_totrow(.) & tab_get_vars(.)$row_var != "no_row_var"),
                      default = NA_integer_)
      )
    last_totrow <- last_totrow[!is.na(last_totrow)]
    if (length(last_totrow) > 0) {


      last_totrow_pct_cols <- tabs_text |>
        purrr::map(~ names(.)[get_pct_base(.) == "col" & get_col_var(.) != "no_col_var" &
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
                        dplyr::across(where(is_fmt), ~ as_totrow(., FALSE) |>
                                        set_diff(NA_real_) |> set_ci(NA_real_) |>
                                        set_mean(NA_real_) |>
                                        set_ctr(NA_real_) |> set_var(NA_real_)
                                        ),
                        dplyr::across(
                          where(is_fmt) & -tidyselect::all_of(val_cols),
                          ~ set_num(., value = NA_real_)
                        ),
                        dplyr::across(
                          all_of(row_lab),
                          # WARNING: a declared index column must keep its declaration (Phase 19f)
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
                                  dplyr::across(where(is_fmt), ~ as_totrow(., FALSE)  |>
                                                  set_diff(NA_real_) |> set_ci(NA_real_) |>
                                                  set_mean(NA_real_) |> set_pct(NA_real_) |>
                                                  set_ctr(NA_real_) |> set_var(NA_real_)
                                                ),
                                  dplyr::across(
                                    where(is_fmt) & -tidyselect::all_of(val_cols),
                                    ~ set_num(., value = NA_real_)
                                  ),
                                  dplyr::across(
                                    all_of(row_lab),
                                    # WARNING: a declared index column must keep its declaration (Phase 19f)
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


# tab_is_or_display() -- Phase 16c. TRUE when the table DISPLAYS odds ratios (any fmt value column with
# display "or"/"or_pct"). The "100%" total column is meaningless for such a table (ORs don't sum to 1),
# so the Total column shows only the base n (console) / is dropped in favour of the base-n column
# (export). Keyed on the DISPLAYED quantity, NOT ci_type: `color = "OR"` with `OR = "no"` shows real
# percentages (a meaningful 100% total) yet can still carry an OR interval.
tab_is_or_display <- function(tab) {
  if (!is.data.frame(tab)) return(FALSE)
  fc <- purrr::map_lgl(tab, is_fmt)
  if (!any(fc)) return(FALSE)
  any(purrr::map_lgl(tab[fc], ~ any(get_display(.) %in% c("or", "or_pct"))))
}

# tab_fold_addn_incell() -- Phase 10i-B decision 1. For TEXT backends (console / kable / md), the
# add_n base shows in the Total cell as an in-cell composite `{pct} (n={n})` (via the Phase-10i-A
# display grammar), reading the base from the Total column's OWN `n` field. Phase 17g: text no longer
# builds the separate `n` COLUMN at all (tab_add_n_pct skips it), so the leading select(-any_of("n"))
# is now a no-op guard (it still runs for any stray column). Each Total cell shows its OWN base
# `{n}`. DORMANT: the retired option `tabxplor.totcol_range` ("range"/"min") once swapped in the
# cross-col_var base via tab_totcol_range() (a per-row literal `[min;max]` / smallest) -- see the
# commented branch below and the DORMANT note in utils.R .onLoad.
# Phase 16c: for an OR/RRR table the "100%" is dropped -> the cell shows only `n={n}` (the base).
# NB: run BEFORE tab_pvalue_lines(), so the Total column has only data/total cells (all eligible).
tab_fold_addn_incell <- function(tab) {
  tot_nm <- dplyr::last(names(tab)[is_totcol(tab) & get_pct_base(tab) == "row" &
                                     get_col_var(tab) != "no_col_var"])
  if (length(tot_nm) != 1 || is.na(tot_nm)) return(dplyr::select(tab, -tidyselect::any_of("n")))
  is_or <- tab_is_or_display(tab)

  # DORMANT (possible future implementation): the retired tabxplor.totcol_range option.
  # Re-enabling = uncomment these lines (and the option seed in utils.R .onLoad):
  # style <- getOption("tabxplor.totcol_range", "off")
  # rng <- if (!identical(style, "off")) {
  #   fmt_cols <- which(purrr::map_lgl(tab, is_fmt))
  #   tab_totcol_range(tab, fmt_cols, get_col_var(tab), which(is_totcol(tab)), style = style)
  # } else NULL
  rng <- NULL

  tmpl <- if (is_or) {                                # OR/RRR: show only the base n, drop the "100%"
    if (is.null(rng)) rep("n={n}", nrow(tab))
    else dplyr::if_else(is.na(rng$text), "", paste0("n=", rng$text))
  } else if (is.null(rng)) {
    NULL                                              # uniform "{pct} (n={n})"
  } else {
    # per-row literal: "{pct} (n=<base>)"; a row with no base falls back to "{pct}".
    dplyr::if_else(is.na(rng$text), "{pct}", paste0("{pct} (n=", rng$text, ")"))
  }

  tab <- dplyr::select(tab, -tidyselect::any_of("n"))   # drop the xl-style `n` column
  dplyr::mutate(tab, dplyr::across(tidyselect::all_of(tot_nm), function(col) {
    d    <- get_display(col)
    # only genuine value cells where both fields render (Phase-10i-A `both` guard); the Total
    # column is all pct/n non-NA here (p-value rows are materialised later), so this is all cells.
    elig <- !is.na(get_num(set_display(col, "pct"))) & !is.na(get_num(set_display(col, "n")))
    if (is.null(tmpl)) d[elig] <- "{pct} (n={n})" else d[elig] <- tmpl[elig]
    set_display(col, d)
  }))
}

# tab_or_total_col() -- Phase 16c. Complements tab_fold_addn_incell for the cases the in-cell fold does
# not cover: the "100%" total column is meaningless on an OR/RRR table, so drop it for EXCEL (the base n
# is exported as its own `n` column when add_n is on, nothing otherwise) and for the CONSOLE add_n=FALSE
# case (no base to fold -> nothing). The console add_n=TRUE case is already handled by the fold above
# (the Total cell shows `n={n}`), so this no-ops there.
tab_or_total_col <- function(tab, backend, add_n_on) {
  if (!is.data.frame(tab) || !tab_is_or_display(tab)) return(tab)
  tot_nm <- names(tab)[purrr::map_lgl(tab, ~ is_fmt(.) && is_totcol(.) &&
                                        get_pct_base(.) == "row" && get_col_var(.) != "no_col_var")]
  if (!length(tot_nm)) return(tab)
  if (identical(backend, "xl") || !isTRUE(add_n_on)) {
    tab <- dplyr::select(tab, -tidyselect::all_of(tot_nm))
  }
  tab
}


# tab_apply_n_min() -- the small-base display filter (Phase 7g). A PURE end-of-pipeline DISPLAY
# helper: it recomputes NOTHING (no fields, no chi2/ANOVA, no CI). The user has already seen the
# whole table; n_min just strips the noise of unreliable small-base cells so it reads cleanly.
# Rule: for row-oriented columns (type row/all/mean) drop a row only if its LARGEST base across
# those columns is < n_min, then blank (display "") each surviving cell whose OWN base < n_min;
# for col-oriented columns (type "col", the pct="col" case) drop the whole column when its base
# is < n_min. Orientation is read from each fmt column's stored `type`, so no `pct` argument is
# needed and mixed tables Just Work. Base = get_tot_n() for proportions, get_n() for means; an NA
# base is never weak. NEVER drops: total rows/tables, the total column, add_n/add_pct helper rows
# (row_var "n"/"row_pct") or columns (col_var "all_col_vars"), or the p-value line (all n NA).
# Class + attributes (subtext/test/grouping) survive via the tabxplor dplyr S3 methods.
tab_apply_n_min <- function(tab, n_min) {
  if (length(n_min) == 0 || is.na(n_min[1]) || n_min[1] <= 0) return(tab)
  n_min <- n_min[1]
  if (!is.data.frame(tab)) return(tab)

  fmt_names <- names(tab)[purrr::map_lgl(tab, is_fmt)]
  if (length(fmt_names) == 0) return(tab)

  # Phase 19b: a "row-oriented" column is one whose base is a ROW (a row / all-tabs percentage, or a
  # mean); a "col-oriented" one is a column percentage. Two stored facts, where this read the old
  # 8-value `type`.
  base   <- purrr::map_chr(tab[fmt_names], get_pct_base)
  vkind  <- purrr::map_chr(tab[fmt_names], fmt_var_kind)
  row_like <- base %in% c("row", "all") | vkind == "mean"
  totcol <- purrr::map_lgl(tab[fmt_names], is_totcol)

  cell_base <- function(col) if (fmt_var_kind(col) == "mean") get_n(col) else get_tot_n(col)

  # --- protected rows (never dropped) --------------------------------------------------------
  # Phase 10i-B: n_min runs at build on the CORE table -- the add_n/add_pct/p-value extras are
  # materialised later, at display -- so the former helper-COLUMN ("all_col_vars") and helper-ROW
  # ("n"/"row_pct"/p-value) protections are dead. Only the total row / total table are protected.
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
      # Filter globally: a grouped_tab would split the length-n `keep` per group, so ungroup,
      # filter, then restore the grouping (the tabxplor S3 methods carry subtext/test through).
      gv  <- dplyr::group_vars(tab)
      tab <- dplyr::ungroup(tab)
      tab <- dplyr::filter(tab, keep)
      if (length(gv) > 0) tab <- dplyr::group_by(tab, dplyr::across(tidyselect::all_of(gv)))
    }
  }
  # blank surviving weak cells (row-oriented, non-total stat columns)
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
