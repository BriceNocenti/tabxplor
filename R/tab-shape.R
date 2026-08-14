# =====================================================================================================
# R/tab-shape.R -- WHAT SHAPE IS THIS TABLE, AND WHICH OPERATIONS ACCEPT IT (Phase 19h, KEY 7)
# =====================================================================================================
# The package has several ways to say "more than one table": a merged table (several `row_vars`
# stacked, one declared `var` column), a grouped table (`tab_vars`, i.e. sub-tables), and a list of
# tables. Which operation accepts which combination was written down NOWHERE, and enforced by five
# scattered aborts in three files -- so "can I transpose a grouped table?" had no single answer to
# read, only an answer to discover by hitting it.
#
# THE MODEL -- one reader, one declared table:
#
#   tab_shape(x)        the FACTS, read from the declared model and nothing else: the index columns
#                       (R/row-model.R) and `meta$spec$kind` (R/table-spec.R). Never a column NAME,
#                       never a heuristic. Exported, because "what have I got?" is a user question.
#   TAB_OPS             one ROW per operation, stating which facts it requires and why. Adding an
#                       operation is one row; a new shape fact is one column.
#   tab_supports(x, op) the predicate. Exported, so a caller can ASK instead of trying.
#   tab_check_shape()   the internal enforcer every abort site calls.
#
# WARNING: not every refusal is a shape fact. tab_transpose() also refuses duplicated row keys, more
# than one total row and more than one total column; those are properties of the CONTENT, they have
# nothing to do with the row/column model, and they stay local to that function.
# =====================================================================================================


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
    # `tab_vars` must MATCH (there is no one sub-table axis to merge otherwise); `col_vars` need only
    # be nested -- every table's set a subset of the widest one -- which is the rule tab_compact()
    # has always applied, so a table with fewer columns still merges.
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
  # the row axis: the DECLARED index columns; the degraded reader is tab_render_vars()'s job, not
  # this one's -- a table with no declared index simply reports an empty model.
  dv <- tab_declared_vars(x)
  if (is.null(dv)) dv <- list(row_vars = character(0), tab_vars = character(0), compacted = FALSE)
  list(
    container = "table",
    kind      = tab_kind(x),
    merged    = isTRUE(dv$compacted),
    grouped   = length(dv$tab_vars) != 0L,
    row_vars  = as.character(dv$row_vars),
    tab_vars  = as.character(dv$tab_vars),
    # the column axis is, as always, the fmt columns' own attribute
    col_vars  = setdiff(unique(get_col_var(x)), c("", "all_col_vars")),
    same_col_vars = TRUE,
    same_tab_vars = TRUE
  )
}


# rd_shape() -- the SAME record, read off a finished render model (R/tab-export-prep.R) instead of a
# table. Two producers, one record type, one checker: the render stack works on `rd`, whose `$vars`
# already IS the variable model, so re-deriving it from a table it no longer holds would be the
# second encoding this key exists to delete.
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
