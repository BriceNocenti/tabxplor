# PURPOSE: The 1.x -> 2.0.0 translation layer -- the retired arguments, the superseded tab_many()
#   entry point, the retired `type` vocabulary of the fmt column attributes, and the retired `chi2`
#   table attribute.
# ROLE: Grouped here so the live build path never meets them: every function in this file exists to
#   map an OLD spelling onto a current one and then get out of the way.
# KEY CONSTRAINTS:
#   - Each shim is LOSSLESS or it aborts -- never a silent approximation. `OR` becomes display/ref2,
#     `chi2` becomes `test`, `na_drop_all` becomes an exact `filter`, `sup_cols` becomes the
#     (col_vars, levels, pct) triple, and `type` becomes the (scale, pct_type) pair.
#   - A shim returns only the entries the caller actually SET, so tab()'s own defaults apply to the
#     rest. One that passed `tot = "row"` because `totrow` defaults to TRUE would silently drop the
#     total column.
#   - tab_many() takes only its five LEADING positional slots: the two functions' 6th formals differ
#     (`pct` vs `sup_cols`), so an unnamed 6th argument is REFUSED, not silently mis-assigned.
#   - Soft-deprecation only (lifecycle), never a hard break: this is CRAN-released surface.
# See: CLAUDE.md § tabxplor architecture (the crosstab API).

# DESIGN: the odds ratio exists on every row/col-pct table, so `OR` only picked a display and a 2x2:
#   OR = "OR" / "or"         ->  display = "{or}"
#   OR = "OR_pct" / "or_pct" ->  display = "{or} ({pct})"
#   OR = "cumOR"             ->  ref2 = "cumulative"  (+ display = "{or}")
# `ref` is carried too: a shim must be lossless, so it pins "first" -- as the message teaches.
#' @keywords internal
#' @noRd
tab_deprecate_or <- function(OR, display, ref2, ref, user_env = rlang::caller_env(2)) {
  out <- list(display = display, ref2 = ref2, ref = ref)
  if (length(OR) == 0L) return(out)
  # DESIGN: `display` is scalar, so a per-row_var `OR` vector has nowhere to land -- refuse it.
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
                            user_env = user_env)
  # DESIGN: a user-set `display` wins -- it is the argument the deprecation points at.
  if (is.null(display) || length(display) == 0L || is.na(display[[1]]) || !nzchar(display[[1]]))
    out$display <- new_display
  out$ref2 <- new_ref2
  if (length(ref) == 1L && identical(as.character(ref)[1], "auto")) out$ref <- "first"
  out
}


# DESIGN: the tab_many() vocabulary map -- each legacy formal has an exact tab() equivalent:
#   chi2 -> test;  totrow / totcol -> tot ("each"/"all_col_vars" collapse to one total column);
#   compact -> output_list (inverted);  na_drop_all = c(a, b) -> filter = !is.na(a) & !is.na(b).
# The last is exact, not an approximation: `filter` is materialised on the UNSELECTED data and
# applied immediately before tab_prepare() -- exactly where na_drop_all's own na.omit() ran.
#' @keywords internal
#' @noRd
tab_deprecate_many <- function(chi2 = NULL, totrow = NULL, totcol = NULL, compact = NULL) {
  out <- list()
  if (!is.null(chi2)) {
    lifecycle::deprecate_soft("2.0.0", "tab_many(chi2 = )", "tab(test = )")
    out$test <- chi2
  }
  if (!is.null(totrow) || !is.null(totcol)) {
    ok_totcol <- TAB_ARG_VALUES$totcol$values
    if (!is.null(totcol) && !as.character(totcol)[1] %in% ok_totcol)
      cli::cli_abort(c("Unknown {.arg totcol} value {.val {as.character(totcol)[1]}}.",
                       "i" = "Valid: {.val {ok_totcol}}; {.fn tab} spells it {.arg tot}."))
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


# DESIGN: `sup_cols` is mechanically `col_vars` + `levels = "first"` + `pct = "row"`, folded here.
#' @keywords internal
#' @noRd
tab_deprecate_sup_cols <- function(sup_cols, col_var, levels, pct) {
  list(
    col_vars = c(col_var, sup_cols),
    # `levels`/`pct` recycle over the MAIN col_vars; supplementary ones are first-level, row-pct.
    levels   = c(rep(levels, length.out = length(col_var)), rep("first", length(sup_cols))),
    pct      = c(rep(pct   , length.out = length(col_var)), rep("row"  , length(sup_cols)))
  )
}


# The `na_drop_all` half: it needs the DATA (a tidy-select) and may compose with a user `filter`.
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
  # A character `filter` (tab_many's tribble idiom) is parsed so the two halves meet as expressions.
  # WARNING: a CONSTANT quosure carries the EMPTY environment, in which even `%in%` is unbound.
  fx <- rlang::quo_get_expr(filter_quo)
  if (is.character(fx)) {
    env <- rlang::quo_get_env(filter_quo)
    if (identical(env, rlang::empty_env())) env <- rlang::caller_env(2)
    filter_quo <- rlang::new_quosure(rlang::parse_expr(fx), env)
  }
  rlang::quo(!!filter_quo & !!na_expr)
}


# WARNING -- POSITIONAL ARGUMENTS: tab_many()'s 6th formal is `pct` where tab()'s is `sup_cols`, so
# an unnamed element in `...` is REFUSED rather than forwarded into the wrong argument.
# `@inheritDotParams tab` is deliberately absent: it inlines hundreds of Rd lines already on `?tab`.
#' Many cross-tables as one, with color helpers
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' Superseded (2.0.0) by [tab()], the unified entry point: it accepts several `row_vars` /
#' `col_vars` and merges them into one table by default (`output_list = TRUE` gives the list shape
#' `tab_many()` returns).
#'
#' `tab_many()` forwards everything to [tab()], translating the five renamed arguments:
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
#' @param row_vars,col_vars,tab_vars,wt The variable roles — see [tab()]. With `data`, the only
#'   arguments that may be passed by position: everything else must be named, because [tab()]'s
#'   argument order differs.
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
  # Silent for same-package callers (jmvtab), so only direct external users are nudged.
  lifecycle::deprecate_soft(
    "2.0.0", "tab_many()", "tab()",
    details = c(
      "i" = paste0("tab() accepts several row_vars / col_vars. It merges >=2 row_vars into one ",
                   "table by default; pass output_list = TRUE for a list (tab_many()'s old default).")
    )
  )

  # `...` is captured ONLY for its NAMES, then forwarded bare: environments and missingness survive.
  dot_names <- rlang::names2(rlang::enquos(..., .ignore_empty = "all"))
  unnamed   <- which(!nzchar(dot_names))
  if (length(unnamed) != 0L)
    # WARNING: cli takes `{?s}` from the LAST substitution before it -- a plural-opening message
    # needs cli::qty(), else "Cannot pluralize without a quantity".
    cli::cli_abort(c(
      "{.fn tab_many} takes only {.arg data}, {.arg row_vars}, {.arg col_vars}, {.arg tab_vars} and {.arg wt} by position.",
      "x" = "{cli::qty(length(unnamed))}Argument{?s} {unnamed + 5L} {?is/are} unnamed.",
      "i" = "{cli::qty(length(unnamed))}Name {?it/them}."))

  extra <- tab_deprecate_many(
    chi2    = if (missing(chi2))    NULL else chi2,
    totrow  = if (missing(totrow))  NULL else totrow,
    totcol  = if (missing(totcol))  NULL else totcol,
    compact = if (missing(compact)) NULL else compact
  )
  # tab_many()'s historical shape is a list for >=2 row_vars and a BARE TABLE for one: the shim asks
  # for a list and unwraps a length-1 result itself, keeping that irregularity in the legacy function.
  # `compact` overrides the list; an explicit `output_list` in `...` wins over both.
  legacy_shape <- is.null(extra$output_list) && !"output_list" %in% dot_names
  if (legacy_shape) extra$output_list <- TRUE

  # `na_drop_all` is a tidy-select, so it needs the frame -- a survey design is unwrapped first.
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


# === SECTION: The retired `type` vocabulary =========================================================
# `type` conflated two facts -- WHAT a column estimates and WHICH percentage it is -- and 2.0.0 split
# them into `scale` (a key into EST_SCALES) and `pct_type`. The map back is stated ONCE, here, and
# read by all three entry points that still admit the old word: fmt(type = ), set_type(), get_type().

# DESIGN: the allow-list IS 1.x's, so a call that worked then works now and one that did not still
#   aborts with the same seven values. `n` is the pair, not the scale alone: a column repurposed as a
#   count (or a mean, or a coefficient) whose `pct_type` still claimed "row" would keep percentage
#   semantics it no longer has -- and 1.x, having one attribute, could not have claimed them.
#' @keywords internal
#' @noRd
fmt_type_legacy <- function(type, call = rlang::caller_env()) {
  type <- as.character(type)[1]
  if (is.na(type) || type %in% c("no", "")) type <- "n"
  ok <- c("row", "col", "all", "all_tabs", "mean", "n", "coef")
  if (!type %in% ok)
    cli::cli_abort(c("{.arg type} must be one of {.val {ok}}.",
                     "i" = "It is retired: {.arg scale} and {.arg pct_type} replace it."),
                   call = call)
  switch(type,
         mean = list(scale = "level_mean", pct_type = "none"),
         n    = list(scale = "level_n"   , pct_type = "none"),
         # "coef" named a regression-coefficient column, which is a raw difference in 2.0.0.
         coef = list(scale = "raw_diff"  , pct_type = "none"),
         list(scale = "level_pct", pct_type = type))
}

# The message every `type` shim delivers -- one sentence, the argument that replaces it, written as
# code. Shared so the three entry points cannot say three different things.
#' @keywords internal
#' @noRd
fmt_type_deprecate <- function(what, pair, user_env = rlang::caller_env(2)) {
  with_txt <- paste0('scale = "', pair$scale, '", pct_type = "', pair$pct_type, '"')
  lifecycle::deprecate_soft(
    "2.0.0", what, with = I(with_txt), user_env = user_env,
    details = paste0("`type` said both what a column estimates and which percentage it is; ",
                     "`scale` and `pct_type` say them separately."))
}

#' Column types, the tabxplor 1.x spelling
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' In tabxplor 1.x a `fmt` column carried one `type` attribute, whose seven values conflated two
#' facts. Since 2.0.0 they are two attributes: [get_scale()] / [set_scale()] say **what the column
#' estimates** (a key into the declared scale table), and [get_pct_type()] / [set_pct_type()] say
#' **which kind of percentage** it holds. These two functions translate, so 1.x code keeps running;
#' they are defunct in tabxplor 2.1.0.
#'
#' `get_type()` re-fuses what 2.0.0 split, so it is a reading aid rather than an accessor:
#' `level_mean` reads back `"mean"`, `level_n` reads `"n"`, a level percentage reads its own
#' `pct_type`, and every effect scale (a difference, a ratio, an odds ratio, a coefficient) reads
#' `"coef"` --- distinctions 1.x could not make are lost on the way back.
#'
#' @param x A `fmt` vector, or a data frame of them.
#' @param type One of `"row"`, `"col"`, `"all"`, `"all_tabs"`, `"mean"`, `"n"`, `"coef"`.
#' @param ... Used in methods to add arguments in the future.
#' @return `get_type()` a character vector; `set_type()` a modified `fmt` vector.
#' @seealso [get_scale()], [set_scale()], [get_pct_type()], [set_pct_type()], [fmt_attr()].
#' @name tabxplor-type
#' @examples
#' x <- fmt(n = c(10, 20), pct = c(0.3, 0.7), scale = "level_pct", pct_type = "row")
#' get_type(x)
NULL

#' @describeIn tabxplor-type set the retired `type` attribute of a `fmt` vector
#' @export
set_type <- function(x, type) {
  pair <- fmt_type_legacy(type)
  fmt_type_deprecate("set_type()", pair, user_env = rlang::caller_env())
  # WARNING: through the validating setters, never `attr<-`: a shim must not be a back door into a
  #   vocabulary the live path refuses.
  set_pct_type(set_scale(x, pair$scale), pair$pct_type)
}

#' @describeIn tabxplor-type get the retired `type` of `fmt` columns
#' @export
get_type <- function(x, ...) UseMethod("get_type")
#' @method get_type default
#' @export
#' @noRd
get_type.default      <- function(x, ...) {
  if (is.null(purrr::attr_getter("scale")(x))) "" else fmt_type_from_scale(x)
}
#' @method get_type tabxplor_fmt
#' @export
#' @noRd
get_type.tabxplor_fmt <- function(x, ...) fmt_type_from_scale(x)
#' @method get_type data.frame
#' @export
#' @noRd
get_type.data.frame   <- function(x, ...) purrr::map_chr(x, ~ get_type(.))

# The lossy way back: EST_SCALES' own `kind`/`est_field` decide, so a scale added later needs no edit
# here -- it is an effect, so it reads "coef", which is what 1.x called any non-level column.
#' @keywords internal
#' @noRd
fmt_type_from_scale <- function(x) {
  scale <- get_scale(x)
  row   <- EST_SCALES[[scale]]
  if (is.null(row) || !identical(row$kind, "level")) return("coef")
  switch(row$est_field, mean = "mean", n = "n", get_pct_type(x))
}


# === SECTION: The retired `chi2` table attribute ==================================================

# 1.x's only programmatic route to a table's test, reached with `tabxplor:::get_chi2()`. `test` is
# the same attribute, renamed and widened (an ANOVA, a model fit and a global test are rows of it
# now), so this is a RENAME, not a schema translation -- and the two rows that moved are named in
# the message rather than mapped, because the wide `chi2 stats` frame no longer exists to map onto.
#' @keywords internal
#' @noRd
get_chi2 <- function(x) {
  lifecycle::deprecate_soft("2.0.0", I("get_chi2()"), "get_test()", details = c(
    "i" = "`df` is `df1` (`df2` on an F test) and `count` is `statistic`.",
    "i" = "The per-cell chi-2 contributions are the cells' own `ctr` field."
  ))
  get_test(x)
}
