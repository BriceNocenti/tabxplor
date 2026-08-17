# PURPOSE: What shape a tabxplor result has (merged / grouped / a list), and which reshape operations
#   accept it -- so "can I transpose a grouped table?" is read, not discovered by hitting an abort.
# ROLE: The single reader every shape-sensitive operation (tab_compact, tab_transpose, the exporters'
#   transpose = TRUE) consults before refusing a table.
# KEY CONSTRAINTS:
#   - Shape is read from the DECLARED model only -- the index columns (R/row-model.R) and
#     `meta$spec$kind` (R/table-spec.R). Never a column NAME, never a heuristic.
#   - Not every refusal is a shape fact: tab_transpose() also refuses duplicated row keys and multiple
#     total rows/columns, which are properties of the CONTENT and stay local to that function.
# See: dev/tabxplor_architecture.md (declarative architecture); TAB_OPS below.
#
# THE MODEL -- one reader, one declared table:
#   tab_shape(x)        the shape facts. Exported ("what have I got?" is a user question).
#   TAB_OPS             one ROW per operation: which facts it requires and why. A new operation is a
#                       row; a new shape fact is a column.
#   tab_supports(x, op) the predicate, so a caller can ASK instead of trying. Exported.
#   tab_check_shape()   the internal enforcer every abort site calls.


# --- the facts ---------------------------------------------------------------------------------------

#' The shape of a tabxplor table
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' What kind of object a `tabxplor` result is, read from its own declared model — the row-index
#' columns (their stored roles) and the table's stated kind — rather than guessed from column names.
#' Use it with [tab_supports()] to know, before trying, whether an operation accepts what you have.
#'
#' @param x A `tabxplor_tab` / `tabxplor_grouped_tab`, or a list of them (`output_list = TRUE`).
#'
#' @return A named list:
#' \describe{
#'   \item{`container`}{`"table"` or `"list"`.}
#'   \item{`kind`}{`"crosstab"` or `"regression"` (`NA` when the table carries no metadata).}
#'   \item{`merged`}{`TRUE` when several row variables are stacked in one table (a `var` column
#'     names each row's variable).}
#'   \item{`grouped`}{`TRUE` when the table has `tab_vars` (sub-tables).}
#'   \item{`row_vars`, `tab_vars`, `col_vars`}{the variables on each axis.}
#'   \item{`same_col_vars`, `same_tab_vars`}{for a list only: whether its tables agree.}
#' }
#'
#' @seealso [tab_supports()] for what each shape allows.
#' @export
#'
#' @examples
#' \donttest{
#' t <- tab(forcats::gss_cat, c(marital, relig), race, pct = "row")
#' tab_shape(t)$merged
#' }
tab_shape <- function(x) {
  if (is.list(x) && !is.data.frame(x)) {
    parts <- lapply(x, tab_shape)
    keep  <- vapply(parts, function(p) !is.null(p), logical(1))
    parts <- parts[keep]
    # `tab_vars` must MATCH (there is no one sub-table axis to merge otherwise); `col_vars` need only be
    # nested -- every table's set a subset of the widest one -- so a table with fewer columns still merges.
    same <- function(field) {
      vs <- lapply(parts, `[[`, field)
      if (length(vs) < 2L) return(TRUE)
      length(unique(vs)) == 1L
    }
    nested <- function(field) {
      vs <- lapply(parts, `[[`, field)
      if (length(vs) < 2L) return(TRUE)
      widest <- vs[[which.max(lengths(vs))]]
      all(vapply(vs, function(v) all(v %in% widest), logical(1)))
    }
    return(list(
      container = "list",
      kind      = if (!length(parts)) NA_character_ else parts[[1]]$kind,
      merged    = any(vapply(parts, function(p) isTRUE(p$merged) , logical(1))),
      grouped   = any(vapply(parts, function(p) isTRUE(p$grouped), logical(1))),
      row_vars  = unique(unlist(lapply(parts, `[[`, "row_vars"))),
      tab_vars  = unique(unlist(lapply(parts, `[[`, "tab_vars"))),
      col_vars  = unique(unlist(lapply(parts, `[[`, "col_vars"))),
      same_col_vars = nested("col_vars"),
      same_tab_vars = same("tab_vars")
    ))
  }
  if (!is.data.frame(x)) return(NULL)
  # the row axis: the DECLARED index columns. A table with no declared index reports an empty model.
  dv <- tab_declared_vars(x)
  if (is.null(dv)) dv <- list(row_vars = character(0), tab_vars = character(0), compacted = FALSE)
  list(
    container = "table",
    kind      = tab_kind(x),
    merged    = isTRUE(dv$compacted),
    grouped   = length(dv$tab_vars) != 0L,
    row_vars  = as.character(dv$row_vars),
    tab_vars  = as.character(dv$tab_vars),
    # the column axis is the fmt columns' own attribute
    col_vars  = unique(Filter(is_real_col_var, unique(get_col_var(x)))),
    same_col_vars = TRUE,
    same_tab_vars = TRUE
  )
}


# tab_columns() -- the COLUMN-axis mirror of tab_shape(): one row per fmt column with the attributes
# that decide what it shows, estimates, colours and how its interval was computed. The only place the
# four inference facts (conf_level / degf / basis / ci_method) can be read side by side. It reports the
# STORED attributes (fmt_attrs_of()), never a render-time default: `conf_level = NA` honestly means
# "no interval was stamped on this column".

#' Every `fmt` column of a table, and what it carries
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' One row per numeric (`tabxplor_fmt`) column, with the per-column attributes that decide what it
#' shows, what it estimates, how it is coloured and how its confidence interval was computed. The
#' column-axis companion of [tab_shape()], which describes the table as a whole.
#'
#' @param x A `tabxplor_tab` / `tabxplor_grouped_tab`, or any data.frame holding `fmt` columns.
#'
#' @return A tibble, one row per `fmt` column:
#' \describe{
#'   \item{`column`}{the column name.}
#'   \item{`col_var`, `col_group`}{the column variable, and the sub-population its block belongs to
#'     (`""` when the table was never spread).}
#'   \item{`scale`, `pct_base`}{what the column estimates, and on which percentage base.}
#'   \item{`display`}{the display template(s) its cells carry.}
#'   \item{`ref`, `comp_all`, `totcol`, `refcol`}{the comparison model: which baseline, whether it
#'     compares across sub-tables, and whether this column is a total or the reference.}
#'   \item{`color`, `color_bg`, `color_signif`}{the colour measure of each channel and the
#'     significance policy.}
#'   \item{`conf_level`, `degf`, `basis`, `ci_method`}{how this column's interval was computed — the
#'     level, the degrees of freedom it is referred to (`NA` = the normal quantile), whether it rests
#'     on the raw count, the weights or the survey design, and by which method.}
#'   \item{`model_family`, `role`}{for a [tab_reg()] table: the column's model family, and whether it
#'     holds the model estimate (`"model"`) or its observed counterpart (`"emp"`).}
#' }
#' @seealso [tab_shape()] for the table's own shape; [fmt_attr()] to read or write one attribute;
#'   [fmt()] for what each attribute means.
#' @export
#'
#' @examples
#' \donttest{
#' t <- tab(forcats::gss_cat, marital, race, pct = "row", ci = "ref")
#' tab_columns(t)
#' }
tab_columns <- function(x) {
  if (!is.data.frame(x))
    cli::cli_abort("{.fn tab_columns} needs a table (a data.frame with {.cls tabxplor_fmt} columns).")
  fmt_cols <- names(x)[vapply(x, is_fmt, logical(1))]
  if (!length(fmt_cols)) return(tibble::tibble(column = character(0)))
  rows <- lapply(fmt_cols, function(cn) {
    col <- x[[cn]]
    a   <- fmt_attrs_of(col)
    tibble::tibble(
      column       = cn,
      col_var      = a$col_var,
      col_group    = a$col_group,
      scale        = a$scale,
      pct_base     = a$pct_base,
      # the display is a per-CELL field, so a column may legitimately carry more than one (a total
      # row often shows `n` where the body shows `pct`): report them all, in order of appearance.
      display      = paste(unique(as.character(get_display(col))), collapse = " / "),
      ref          = a$ref,
      comp_all     = a$comp_all,
      totcol       = a$totcol,
      refcol       = a$refcol,
      color        = a$color[1],
      color_bg     = if (length(a$color) > 1L) a$color[2] else NA_character_,
      color_signif = a$color_signif,
      conf_level   = a$conf_level,
      degf         = a$degf,
      basis        = a$basis,
      ci_method    = a$ci_method,
      model_family = a$model_family,
      role         = a$role
    )
  })
  vctrs::vec_rbind(!!!rows)
}


# rd_shape() -- the SAME record, read off a finished render model (R/tab-export-prep.R) instead of a
# table, whose `$vars` already IS the variable model. One record type, one checker for both producers.
#' @keywords internal
#' @noRd
rd_shape <- function(rd) {
  list(container = "table",
       kind      = NA_character_,
       merged    = isTRUE(rd$vars$compacted),
       grouped   = length(rd$vars$tab_vars) != 0L,
       row_vars  = as.character(rd$vars$row_vars),
       tab_vars  = as.character(rd$vars$tab_vars),
       col_vars  = as.character(rd$vars$col_vars),
       same_col_vars = TRUE, same_tab_vars = TRUE)
}


# --- the declared support matrix ----------------------------------------------------------------------
# One row per operation. Each requirement is a PREDICATE on the shape record, paired with the reason
# it exists; `severity` says what happens when it is not met:
#   "abort"  the operation cannot produce a meaningful result   -> cli_abort
#   "bail"   the operation is a no-op here                      -> message + return the input unchanged
#            (tab_compact()'s contract: it must never break a pipeline)
# The `why` entries are CLOSURES so gettext() runs at render, not at build time (a top-level gettext
# would freeze the build locale), while staying statically extractable by potools.
#' @keywords internal
#' @noRd
TAB_OPS <- list(
  compact = list(
    label    = function() gettext("tab_compact()"),
    severity = "bail",
    checks   = list(
      list(ok = function(s) isTRUE(s$same_tab_vars),
           why = function() gettext("the tables have different tab_vars")),
      list(ok = function(s) isTRUE(s$same_col_vars),
           why = function() gettext("the tables have different col_vars"))
    )
  ),
  transpose_object = list(
    label    = function() gettext("tab_transpose()"),
    severity = "abort",
    checks   = list(
      # `kind` FIRST, so a regression gets its own reason rather than failing the crosstab-shaped
      # `!merged` check below (a reg table carries a `var`-role predictor column, so it reads as
      # merged) with the misleading "it needs exactly one row variable".
      list(ok = function(s) !identical(s$kind, "regression"),
           why = function() gettext("it transposes a crosstab; a regression table transposes via tab_export(transpose = TRUE)")),
      list(ok = function(s) !isTRUE(s$grouped),
           why = function() gettext("it transposes a single table, with no sub-tables (tab_vars)")),
      list(ok = function(s) length(s$row_vars) == 1L && !isTRUE(s$merged),
           why = function() gettext("it needs a table with exactly one row variable"))
    )
  ),
  transpose_render = list(
    label    = function() gettext("transpose = TRUE"),
    severity = "abort",
    checks   = list(
      # unlike the object-level flip, this one handles a MERGED table (several row_vars): it flips a
      # finished render model, where the stacked variables are already one index block.
      list(ok = function(s) !isTRUE(s$grouped),
           why = function() gettext("it flips a table with no sub-tables (tab_vars)"))
    )
  )
)


# --- the readers --------------------------------------------------------------------------------------

#' Does this table's shape allow an operation?
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' The support matrix of the shape-sensitive operations, as a predicate. Every place the package
#' refuses a table for its shape reads this same table of rules, so what is allowed can be *read*
#' instead of discovered.
#'
#' @param x A table or list of tables — see [tab_shape()].
#' @param op One of `"compact"`, `"transpose_object"` (the deprecated object-level
#'   [tab_transpose()]) or `"transpose_render"` (the `transpose = TRUE` argument of the exporters).
#'
#' @return A single `TRUE`/`FALSE`.
#' @seealso [tab_shape()].
#' @export
#'
#' @examples
#' \donttest{
#' t <- tab(forcats::gss_cat, marital, race, year, pct = "row")
#' tab_supports(t, "transpose_render")   # FALSE: it has tab_vars
#' }
tab_supports <- function(x, op) {
  op   <- match.arg(op, names(TAB_OPS))
  spec <- TAB_OPS[[op]]
  s    <- tab_shape(x)
  if (is.null(s)) return(FALSE)
  all(vapply(spec$checks, function(ck) isTRUE(ck$ok(s)), logical(1)))
}

# tab_check_shape() -- THE enforcer. Returns TRUE when the operation may proceed; otherwise it aborts
# or (severity "bail") messages and returns FALSE, so the caller returns its input unchanged.
#' @keywords internal
#' @noRd
tab_check_shape <- function(x, op) {
  spec <- TAB_OPS[[op]]
  # `x` is a table / list, or an already-built shape record (rd_shape(), the render stack's producer)
  s    <- if (is.list(x) && !is.data.frame(x) && !is.null(x$container)) x else tab_shape(x)
  if (is.null(s)) return(TRUE)
  bad  <- Filter(function(ck) !isTRUE(ck$ok(s)), spec$checks)
  if (!length(bad)) return(TRUE)
  why <- bad[[1]]$why()
  if (identical(spec$severity, "bail")) {
    cli::cli_inform("{spec$label()} was not applied: {why}.")
    return(FALSE)
  }
  cli::cli_abort(c("{spec$label()} does not support this table.", "i" = "{why}."))
}
