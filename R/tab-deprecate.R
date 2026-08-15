# PURPOSE: The 1.3.1 -> 2.0.0 translation layer for tab() -- the retired arguments and the
#   superseded tab_many() entry point.
# ROLE: Carved out of R/tab.R by Phase 19l. Grouped so the live build path never meets them: every
#   function here exists to map an OLD spelling onto a current one and then get out of the way.
# KEY CONSTRAINTS:
#   - Each shim is LOSSLESS or it aborts -- never a silent approximation. `OR` becomes display/ref2,
#     `chi2` becomes `test`, `na_drop_all` becomes an exact `filter` (both applied immediately before
#     tab_prepare()), `sup_cols` becomes the (col_vars, levels, pct) triple.
#   - tab_many() takes only its five LEADING positional slots: the two functions\' 6th formals differ
#     (`pct` vs `sup_cols`), so an unnamed 6th is REFUSED, not silently mis-assigned.
#   - Soft-deprecation only (lifecycle), never a hard break: this is CRAN-released surface.
# See: CLAUDE.md Repository Map > R/tab-deprecate.R.


# tab_deprecate_or() -- Phase 19d (KEY 8a): THE `OR` retirement, in one place, for all four entry
# points (tab / tab_many / tab_plain / tab_counts).
#
# `OR` was three existing answers welded into one argument -- a measure, a display and a
# dichotomisation -- and the weld is where D20 (`ci = "cell"` silently dropped the odds ratios) and
# D21 (a percentage-point interval on an odds-ratio column) lived. Since the odds ratio is computed
# unconditionally on every row/col-percentage table, each value decomposes mechanically:
#
#   OR = "OR" / "or"           ->  display = "{or}"
#   OR = "OR_pct" / "or_pct"   ->  display = "{or} ({pct})"
#   OR = "cumOR"               ->  ref2 = "cumulative"   (+ display = "{or}")
#
# `ref` is carried too, and deliberately: `ref = "auto"` used to resolve to the FIRST row for an OR
# table, and it now follows the colour measure like every other comparison. A shim must be lossless,
# so it pins "first" itself -- which is also the sentence the message teaches.
#' @keywords internal
#' @noRd
tab_deprecate_or <- function(OR, display, ref2, ref) {
  out <- list(display = display, ref2 = ref2, ref = ref)
  if (length(OR) == 0L) return(out)
  # The row_var axis is globalised on tab() (Phase 6), and `display` -- the argument `OR` retires
  # onto -- is scalar, so a per-row_var vector has nowhere to land. Refuse rather than silently keep
  # the first entry.
  if (length(OR) > 1L)
    cli::cli_abort(c("{.arg OR} must be a single value.",
                     "i" = "It is retired: use {.code display = \"{{or}}\"} (scalar, like every {.arg display})."))
  if (is.logical(OR)) OR <- if (isTRUE(OR[1])) "OR" else "no"
  OR <- as.character(OR)[1]
  if (is.na(OR) || OR %in% c("no", "")) return(out)
  ok <- c("OR", "or", "OR_pct", "or_pct", "cumOR")
  if (!OR %in% ok)
    cli::cli_abort(c("Unknown {.arg OR} value {.val {OR}}.", "i" = "Valid: {.val {ok}}."))
  new_display <- if (OR %in% c("OR_pct", "or_pct")) "{or} ({pct})" else "{or}"
  new_ref2    <- if (identical(OR, "cumOR")) "cumulative" else ref2
  with_txt <- paste0('tab(display = "', new_display, '"',
                     if (identical(OR, "cumOR")) ', ref2 = "cumulative"' else "", ')')
  lifecycle::deprecate_soft("2.0.0", I(paste0('tab(OR = "', OR, '")')), with = I(with_txt),
                            details = paste0(
                              "The odds ratio is now computed on every row/col-percentage table: ",
                              "`display` shows it, `color = \"odds_ratio\"` colours it and `ref2` ",
                              "picks the 2x2. Its row reference follows `ref` like every other ",
                              "comparison (this call keeps the old `ref = \"first\"`)."),
                            user_env = rlang::caller_env(2))
  # a user-set `display` wins -- it is the argument the deprecation points at.
  if (is.null(display) || length(display) == 0L || is.na(display[[1]]) || !nzchar(display[[1]]))
    out$display <- new_display
  out$ref2 <- new_ref2
  if (length(ref) == 1L && identical(as.character(ref)[1], "auto")) out$ref <- "first"
  out
}


# tab_deprecate_many() -- Phase 19h (KEY 7): THE `tab_many()` vocabulary map, in one place, on the
# tab_deprecate_or() model.
#
# `tab_many()` is the only surviving home of the pre-2.0.0 spellings, which is why four public
# functions documented four spellings of one table. Each legacy formal has an exact `tab()`
# equivalent -- they were RENAMED, not removed -- so the shim translates rather than degrades:
#
#   chi2                  ->  test                 (a numeric col_var's whole-table test is Welch's F,
#                                                    so the old name named half of what it does)
#   totrow / totcol       ->  tot                  ("each"/"all_col_vars" collapse to one total
#                                                    column, never an error -- study SS5)
#   compact               ->  output_list          (inverted)
#   na_drop_all = c(a,b)  ->  filter = !is.na(a) & !is.na(b)
#
# The `na_drop_all` mapping is exact, not an approximation: `filter` is materialised on the
# UNSELECTED data (tab_build) and applied immediately before tab_prepare(), which is where
# na_drop_all's own na.omit() ran. Its only other effect was the na_text/na_num "keep" shortcut,
# which changes timing and not results (the rows are gone either way).
#
# Returns only the entries the caller actually set, so tab()'s own defaults apply to the rest --
# a shim that passed `tot = "row"` because `totrow` defaults to TRUE would silently drop the
# total column.
#' @keywords internal
#' @noRd
tab_deprecate_many <- function(chi2 = NULL, totrow = NULL, totcol = NULL, compact = NULL) {
  out <- list()
  if (!is.null(chi2)) {
    lifecycle::deprecate_soft("2.0.0", "tab_many(chi2 = )", "tab(test = )")
    out$test <- chi2
  }
  # A total row is always computed and exactly one total column is shown; both are cosmetic.
  if (!is.null(totrow) || !is.null(totcol)) {
    ok_totcol <- c("last", "each", "all_col_vars", "no")
    if (!is.null(totcol) && !as.character(totcol)[1] %in% ok_totcol)
      cli::cli_abort(c("Unknown {.arg totcol} value {.val {as.character(totcol)[1]}}.",
                       "i" = "Valid: {.val {ok_totcol}} -- and {.fn tab} spells it {.arg tot}."))
    row_on <- if (is.null(totrow)) TRUE else all(as.logical(totrow))
    col_on <- if (is.null(totcol)) TRUE else !identical(as.character(totcol)[1], "no")
    if (!is.null(totrow) && !row_on)
      lifecycle::deprecate_soft(
        "2.0.0", "tab_many(totrow = )", I('tab(tot = "col")'),
        details = "A total row is always computed; drop it with `dplyr::filter(!is_totrow(.))`.")
    if (!is.null(totcol) && !identical(as.character(totcol)[1], "last"))
      lifecycle::deprecate_soft(
        "2.0.0", "tab_many(totcol = )", I('tab(tot = )'),
        details = paste0(
          "Exactly one total column is shown; `\"each\"` and `\"all_col_vars\"` now give that same ",
          "single column. Move or drop columns with dplyr afterwards."))
    out$tot <- c(if (row_on) "row", if (col_on) "col")
    if (length(out$tot) == 0L) out$tot <- "no"
  }
  if (!is.null(compact)) {
    lifecycle::deprecate_soft("2.0.0", "tab_many(compact = )", "tab(output_list = )")
    out$output_list <- !isTRUE(compact)
  }
  out
}


# tab_deprecate_sup_cols() -- Phase 19h (KEY 7): the deprecated `sup_cols` axis folded into the
# col_var axis in ONE place.
#
# `sup_cols` is mechanically `col_vars` + `levels = "first"` + `pct = "row"`, and the code said so --
# by writing that sentence three times, inside three different arguments of the tab_build() call
# (plus a fourth, `ref`, written and commented out). Three mirrors of one rule is how a deprecated
# argument keeps costing edits long after it stopped being used.
#' @keywords internal
#' @noRd
tab_deprecate_sup_cols <- function(sup_cols, col_var, levels, pct) {
  list(
    col_vars = c(col_var, sup_cols),
    # `levels` and `pct` are per col_var and recycle over the MAIN ones; the supplementary columns
    # always show their first level, as a row percentage.
    levels   = c(rep(levels, length.out = length(col_var)), rep("first", length(sup_cols))),
    pct      = c(rep(pct   , length.out = length(col_var)), rep("row"  , length(sup_cols)))
  )
}


# The `na_drop_all` half of the map, split out because it needs the DATA (a tidy-select) and may
# have to compose with a user `filter`. Returns a quosure or NULL.
#' @keywords internal
#' @noRd
tab_deprecate_na_drop_all <- function(cols, filter_quo = NULL) {
  if (length(cols) == 0L) return(filter_quo)
  lifecycle::deprecate_soft(
    "2.0.0", "tab_many(na_drop_all = )", I("tab(filter = )"),
    details = paste0("This call becomes `filter = ",
                     paste0("!is.na(", cols, ")", collapse = " & "), "`."))
  na_expr <- rlang::parse_expr(paste0("!is.na(", cols, ")", collapse = " & "))
  if (is.null(filter_quo) || rlang::quo_is_missing(filter_quo) || rlang::quo_is_null(filter_quo))
    return(rlang::new_quosure(na_expr, rlang::caller_env(2)))
  # a character `filter` (tab_many's documented tribble idiom) is parsed first, so the two halves
  # meet as expressions rather than one being pasted into the other's text.
  # WARNING: a CONSTANT quosure carries the EMPTY environment, in which even `%in%` is unbound.
  fx <- rlang::quo_get_expr(filter_quo)
  if (is.character(fx)) {
    env <- rlang::quo_get_env(filter_quo)
    if (identical(env, rlang::empty_env())) env <- rlang::caller_env(2)
    filter_quo <- rlang::new_quosure(rlang::parse_expr(fx), env)
  }
  rlang::quo(!!filter_quo & !!na_expr)
}


# DESIGN (Phase 19h, KEY 7): tab_many() is a TRANSLATING SHIM over tab(), and nothing else.
#
# It was the last function carrying the pre-2.0.0 vocabulary (`chi2`, `totrow`/`totcol`,
# `compact`, `na_drop_all`), so four public functions documented four slightly different spellings
# of one table -- 42 formals of mirror surface that drifted every time tab() moved. Every one of
# those spellings has an exact tab() equivalent (tab_deprecate_many(), above), so the shim
# translates and forwards; it computes nothing.
#
# WARNING -- POSITIONAL ARGUMENTS. Only the first five (data, row_vars, col_vars, tab_vars, wt) are
# positional, because those are the five tab() and tab_many() always agreed on. tab_many()'s 6th
# formal was `pct` while tab()'s is `sup_cols`, so forwarding an unnamed 6th argument would silently
# set the WRONG argument. An unnamed element in `...` is therefore refused, not forwarded.
#' Many cross-tables as one, with color helpers
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' Superseded (2.0.0) by [tab()], the unified entry point: it accepts several `row_vars` /
#' `col_vars` and merges them into one table by default (`output_list = TRUE` gives the list
#' `tab_many()` used to return).
#'
#' `tab_many()` now forwards everything to [tab()], translating the five arguments that were
#' renamed:
#'
#' | `tab_many()` | [tab()] |
#' | --- | --- |
#' | `chi2 = TRUE` | `test = TRUE` |
#' | `totrow = FALSE` | `tot = "col"` |
#' | `totcol = "no"` | `tot = "row"` |
#' | `compact = TRUE` | `output_list = FALSE` |
#' | `na_drop_all = c(a, b)` | `filter = !is.na(a) & !is.na(b)` |
#'
#' Everything else keeps its name and meaning — see [tab()].
#'
#' @param data A data frame, or a \code{\link[survey:svydesign]{survey::svydesign}}.
#' @param row_vars,col_vars,tab_vars,wt The variable roles — see [tab()]. These four (and `data`)
#'   are the only arguments that may be passed by position; everything else must be named, because
#'   [tab()]'s argument order differs from the historical one.
#' @param ... Passed on to [tab()].
#' @param chi2 `r lifecycle::badge("deprecated")` Use [tab()]'s `test`.
#' @param totrow,totcol `r lifecycle::badge("deprecated")` Use [tab()]'s `tot`. A total row is
#'   always computed and exactly one total column is shown, so both are cosmetic; `totcol = "each"`
#'   and `"all_col_vars"` now give that same single total column instead of erroring.
#' @param compact `r lifecycle::badge("deprecated")` Use [tab()]'s `output_list` (inverted).
#' @param na_drop_all `r lifecycle::badge("deprecated")` <\link[tidyr:tidyr_tidy_select]{tidy-select}>
#'   Use [tab()]'s `filter`: `na_drop_all = c(a, b)` is `filter = !is.na(a) & !is.na(b)`.
#' @param filter `r lifecycle::badge("superseded")` A \code{\link[dplyr:filter]{dplyr::filter}} to
#'   apply to the data frame first — see [tab()]. Prefer filtering upstream.
#'
#' @inheritDotParams tab
#'
#' @return What [tab()] returns: a \code{tabxplor_tab} (a \code{tabxplor_grouped_tab} with
#'   `tab_vars`), or a \code{tabxplor_tabs} list under `output_list = TRUE` / `compact = FALSE`.
#' @export
#'
#' @examples # Make a summary table with many col_vars, showing only one specific level :
#' \donttest{
#' library(dplyr)
#' first_lvs <- c("Married", "$25000 or more", "Strong republican", "Protestant")
#' data <- forcats::gss_cat |> mutate(across(
#'   where(is.factor),
#'   ~ forcats::fct_relevel(., first_lvs[first_lvs %in% levels(.)])
#' ))
#' tab_many(data, race, c(marital, rincome, partyid, relig, age, tvhours),
#'          levels = "first", pct = "row", test = TRUE, color = "auto")
#'}
#'
#' # Can be used with map and tribble to program several tables with different parameters
#' #  all at once, in a readable way:
#' \donttest{
#' library(purrr)
#' library(tibble)
#' pmap(
#'   tribble(
#'     ~row_vars, ~col_vars      , ~pct , ~filter              , ~subtext               ,
#'     "race"  , "marital"       , "row", NULL                 , "Source: GSS 2000-2014",
#'     "relig" , c("race", "age"), "row", "year %in% 2000:2010", "Source: GSS 2000-2010",
#'     NA_character_, "race"     , "no" , NULL                 , "Source: GSS 2000-2014",
#'   ),
#'   .f = tab_many,
#'   data = forcats::gss_cat, color = "auto", test = TRUE)
#' }
tab_many <- function(data, row_vars, col_vars, tab_vars, wt, ...,
                     chi2, totrow, totcol, compact, na_drop_all, filter) {
  # Silent for same-package callers (e.g. the jmvtab module), so only direct external users are nudged.
  lifecycle::deprecate_soft(
    "2.0.0", "tab_many()", "tab()",
    details = c(
      "i" = paste0("tab() accepts several row_vars / col_vars. It merges >=2 row_vars into one ",
                   "table by default; pass output_list = TRUE for a list (tab_many()'s old default).")
    )
  )

  # `...` is captured ONLY to read its NAMES (see the WARNING above, and `output_list` below); it is
  # forwarded bare, so every argument reaches tab() with its own environment and its own missingness
  # intact -- splicing captured quosures into a non-data-masked call would pass them as objects.
  dot_names <- rlang::names2(rlang::enquos(..., .ignore_empty = "all"))
  unnamed   <- which(!nzchar(dot_names))
  if (length(unnamed) != 0L)
    cli::cli_abort(c(
      "{.fn tab_many} takes only {.arg data}, {.arg row_vars}, {.arg col_vars}, {.arg tab_vars} and {.arg wt} by position.",
      "x" = "Argument{?s} {unnamed + 5L} {?is/are} unnamed.",
      "i" = "Name {?it/them}: the rest is passed to {.fn tab}, whose argument order differs."
    ))

  extra <- tab_deprecate_many(
    chi2    = if (missing(chi2))    NULL else chi2,
    totrow  = if (missing(totrow))  NULL else totrow,
    totcol  = if (missing(totcol))  NULL else totcol,
    compact = if (missing(compact)) NULL else compact
  )
  # tab_many()'s historical shape is a list for >=2 row_vars and a BARE TABLE for one, where tab()
  # merges. That shape is exactly the unpredictable return KEY 7 removes, so it is not an engine
  # mode any more (`output = "legacy"` is deleted): the shim asks for a list and unwraps a length-1
  # result itself, keeping the legacy irregularity inside the legacy function, where it dies with it.
  # `compact` overrides the list; an explicit `output_list` in `...` wins over both -- passing it
  # twice would be a duplicate argument.
  legacy_shape <- is.null(extra$output_list) && !"output_list" %in% dot_names
  if (legacy_shape) extra$output_list <- TRUE

  # `na_drop_all` is a tidy-select, so it needs the frame -- and a survey design must be unwrapped
  # first (the same ONE boundary tab() uses, called again only to resolve names; it is a pure unwrap).
  na_cols <- if (missing(na_drop_all)) character() else {
    svy  <- svy_unwrap_data(data, "tab_many")
    names(tidyselect::eval_select(rlang::enquo(na_drop_all),
                                  if (is.null(svy)) data else svy$data))
  }
  filter_quo <- tab_deprecate_na_drop_all(
    na_cols, if (missing(filter)) NULL else rlang::enquo(filter))

  out <- if (is.null(filter_quo)) {
    rlang::inject(tab(data = data,
                      row_vars = {{ row_vars }}, col_vars = {{ col_vars }},
                      tab_vars = {{ tab_vars }}, wt = {{ wt }}, ..., !!!extra))
  } else {
    rlang::inject(tab(data = data,
                      row_vars = {{ row_vars }}, col_vars = {{ col_vars }},
                      tab_vars = {{ tab_vars }}, wt = {{ wt }}, ...,
                      filter = !!filter_quo, !!!extra))
  }
  if (legacy_shape && is.list(out) && !is.data.frame(out) && length(out) == 1L) out[[1]] else out
}
