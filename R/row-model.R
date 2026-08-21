# PURPOSE: The row model -- rows describe themselves the way columns do, so "what is this row" reads
#   stored facts instead of re-deriving them from labels or render-time vectors.
# ROLE: Feeds the crosstab pipeline's row axis; read by the colour engine (is_totrow()), the shape
#   model (R/tab-structure.R) and every exporter.
# KEY CONSTRAINTS:
#   - `role` says what the COLUMN is; `var` says which VARIABLE its labels belong to. On a merged
#     `levels` column `var` is NA. Never infer either from a column NAME.
#   - `row_kind` must live on the fmt FIELD, not the table: fmt_color_plan() calls is_totrow() on a
#     lone extracted column with no table in scope (locked by test-degraded-attrs.R).
# See: CLAUDE.md § tabxplor architecture (row model); R/fmt_class.R (the row_kind field).
#
# TWO facts, TWO carriers:
#   1. WHAT KIND OF ROW  ->  the `row_kind` FIELD of tabxplor_fmt (ROW_KINDS below): data / total, and
#      the synthetic n / pct / p-value / gof / blank display rows.
#   2. WHAT IS THIS LABEL COLUMN FOR  ->  `tabxplor_lvl`, a factor SUBCLASS carrying `role`, `var`, and
#      a per-variable `ordered` map. It stays a factor, so base / dplyr / forcats operations keep
#      working; only vec_c / bind_rows and droplevels() need a method (they rebuild the factor).


# --- the declared row-kind vocabulary ----------------------------------------------------------------
# Every value `row_kind` may take, in reading order. "data" is the neutral (a real body row); the rest
# are synthetic or summary rows a producer added:
#   total    a total row (the only kind is_totrow() asks about)
#   n        the base-count row
#   pct      the add_pct percentage row
#   pvalue   an appended test row (crosstab chi2/F, or a reg per-term test line)
#   gof      a regression goodness-of-fit / model-summary footer row
#   blank    a spacer row between footer blocks
# ORDER matters: fmt_row_kind() reduces several columns' kinds to one per row by "first non-data wins".
ROW_KINDS <- c("data", "total", "n", "pct", "pvalue", "gof", "blank")


# --- the declared label-column roles -----------------------------------------------------------------
# "level"    the column holding the row LEVELS (`marital`, or the literal `levels` when merged)
# "var"      the column naming, per row, WHICH variable that row's level belongs to (merged tables,
#            and a regression's predictor column)
# "tab_var"  a sub-table variable (`tab_vars =`), which is ALSO a dplyr grouping column


# --- the class ----------------------------------------------------------------------------------------

#' A declared tabxplor label column
#'
#' `tabxplor_lvl` is a light **factor subclass** carrying what a row-index column is *for*: its
#' `role` (`"level"` / `"var"` / `"tab_var"`), the `var` its labels belong to, and — per variable —
#' whether that variable was `ordered` in the source data. It is still a factor
#' (`is.factor()` is `TRUE`), so every base, dplyr and forcats operation keeps working unchanged.
#'
#' @param x A factor (or anything `factor()` accepts).
#' @param role One of `"level"`, `"var"`, `"tab_var"`.
#' @param var The variable name the labels belong to; `NA` on a merged `levels` column.
#' @param ordered A named logical vector, one entry per variable, saying whether that variable was
#'   ordered in the source data. A single-variable column keeps its own `ordered` class as well.
#'
#' @return A `tabxplor_lvl` vector.
#' @export
#' @keywords internal
new_lvl <- function(x, role = "level", var = NA_character_, ordered = NULL) {
  if (!is.factor(x)) x <- factor(x)
  x <- unlvl(x)
  if (is.null(ordered)) {
    ordered <- if (!is.na(var[1])) stats::setNames(is.ordered(x), var[1]) else logical(0)
  }
  structure(x, role = as.character(role)[1], var = as.character(var)[1],
            ordered = ordered, class = unique(c("tabxplor_lvl", class(x))))
}

#' @rdname new_lvl
#' @export
#' @keywords internal
is_lvl <- function(x) inherits(x, "tabxplor_lvl")

#' @keywords internal
#' @noRd
unlvl <- function(x) {
  if (!inherits(x, "tabxplor_lvl")) return(x)
  attr(x, "role") <- NULL; attr(x, "var") <- NULL; attr(x, "ordered") <- NULL
  class(x) <- setdiff(class(x), "tabxplor_lvl")
  x
}

#' @keywords internal
#' @noRd
lvl_role <- function(x) if (is_lvl(x)) attr(x, "role", exact = TRUE) else NA_character_
#' @keywords internal
#' @noRd
lvl_var  <- function(x) if (is_lvl(x)) attr(x, "var" , exact = TRUE) else NA_character_
# The per-variable `ordered` map. On a merged `levels` column it is the only record that some stacked
# variables were ordinal -- the factor itself must be plain (vctrs refuses to combine differently
# ordered factors), so the fact moves to the declaration.
#' @keywords internal
#' @noRd
lvl_ordered <- function(x) {
  if (!is_lvl(x)) return(logical(0))
  o <- attr(x, "ordered", exact = TRUE)
  if (is.null(o)) logical(0) else o
}

#' @keywords internal
#' @noRd
lvl_restore <- function(to, from) {
  if (!is_lvl(from)) return(to)
  new_lvl(to, lvl_role(from), lvl_var(from), lvl_ordered(from))
}

# lvl_add_label() -- a SYNTHETIC index label (the "n" / "row_pct" rows the display adds), minted in
# the column's OWN type. Building it as a fresh factor() restored only the declaration, not the
# type: an `ordered` index column became a plain factor and the bind that splices the row in had no
# common type. Appending the level instead keeps the column bindable by construction.
#' @keywords internal
#' @noRd
lvl_add_label <- function(x, label) {
  u  <- unlvl(x)
  lv <- levels(u)
  if (!label %in% lv) lv <- c(lv, label)
  lvl_restore(factor(label, levels = lv, ordered = is.ordered(u)), x)
}

# Reconcile two declarations on a bind: a shared fact survives, differing facts fall back to the
# neutral (role "level", no single var). The `ordered` maps UNION, so a merged table remembers which
# stacked variables were ordinal.
#' @keywords internal
#' @noRd
lvl_reconcile <- function(x, y) {
  ox <- lvl_ordered(x); oy <- lvl_ordered(y)
  o  <- c(ox, oy[!names(oy) %in% names(ox)])
  list(role    = if (identical(lvl_role(x), lvl_role(y))) lvl_role(x) else "level",
       var     = if (identical(lvl_var(x) , lvl_var(y) )) lvl_var(x)  else NA_character_,
       ordered = o)
}


# --- the methods a factor subclass needs --------------------------------------------------------------
# Everything else (filter / arrange / mutate / slice / group_by / as.data.frame / forcats) preserves
# class and attributes with no code; only these rebuild the factor from scratch.

#' @export
`[.tabxplor_lvl` <- function(x, ..., drop = FALSE) {
  out <- NextMethod("[")            # -> `[.factor`, which keeps class but drops other attributes
  lvl_restore(out, x)
}

#' @export
droplevels.tabxplor_lvl <- function(x, ...) lvl_restore(droplevels(unlvl(x), ...), x)

# lvl_ptype2_union() -- THE combining rule for a label column, and the reason it is not the factor
# one: two index columns may legitimately reach a bind with different level sets (one block lost its
# NA level, a synthetic row adds its own), and `ordered` refuses that outright. A label column is a
# LABEL: when no common ordered type exists it degrades to a plain factor over the union, and the
# ordinality survives where it is read from anyway -- the `ordered` map of the declaration.
#' @keywords internal
#' @noRd
lvl_ptype2_union <- function(x, y, ...) {
  tryCatch(vctrs::vec_ptype2(x, y, ...),
           vctrs_error_incompatible_type = function(e)
             factor(levels = union(levels(x), levels(y))))
}

#' @export
vec_ptype2.tabxplor_lvl.tabxplor_lvl <- function(x, y, ...) {
  r <- lvl_reconcile(x, y)
  new_lvl(lvl_ptype2_union(unlvl(x), unlvl(y), ...), r$role, r$var, r$ordered)
}
# DESIGN: an index column bound against an UNDECLARED factor keeps its declaration -- the other side
# states nothing, so there is nothing to reconcile away. Returning the bare union instead dropped the
# `tabxplor_lvl` class, after which tab_index_cols() found no index and the next bind of two ordered
# factors was unprotected again.
#' @export
vec_ptype2.tabxplor_lvl.factor <- function(x, y, ...)
  lvl_restore(lvl_ptype2_union(unlvl(x), y, ...), x)
#' @export
vec_ptype2.factor.tabxplor_lvl <- function(x, y, ...)
  lvl_restore(lvl_ptype2_union(x, unlvl(y), ...), y)
#' @export
vec_ptype2.tabxplor_lvl.character <- function(x, y, ...) vctrs::vec_ptype2(unlvl(x), y, ...)
#' @export
vec_ptype2.character.tabxplor_lvl <- function(x, y, ...) vctrs::vec_ptype2(x, unlvl(y), ...)

# The cast twin of lvl_ptype2_union(): once the common type has degraded to a plain factor, the
# ORDERED half of the bind must still reach it -- vctrs refuses ordered -> factor, so route through
# the labels, which is all a label column ever holds.
#' @keywords internal
#' @noRd
lvl_cast_labels <- function(x, to, ...) {
  tryCatch(vctrs::vec_cast(x, to, ...),
           vctrs_error_incompatible_type = function(e)
             vctrs::vec_cast(as.character(x), to))
}

#' @export
vec_cast.tabxplor_lvl.tabxplor_lvl <- function(x, to, ...)
  lvl_restore(lvl_cast_labels(unlvl(x), unlvl(to), ...), to)
#' @export
vec_cast.tabxplor_lvl.factor    <- function(x, to, ...) lvl_restore(lvl_cast_labels(x, unlvl(to), ...), to)
#' @export
vec_cast.factor.tabxplor_lvl    <- function(x, to, ...) lvl_cast_labels(unlvl(x), to, ...)
#' @export
vec_cast.tabxplor_lvl.character <- function(x, to, ...) lvl_restore(vctrs::vec_cast(x, unlvl(to), ...), to)
#' @export
vec_cast.character.tabxplor_lvl <- function(x, to, ...) vctrs::vec_cast(unlvl(x), to, ...)


# --- a declared LEVEL OPERATION: the collapse spec ----------------------------------------------------
# The row model owns the SPEC of "merge these levels into one"; the APPLIER is tab_collapse_levels() at
# the prepare stage (R/tab.R) -- a collapse changes COUNTS, so it is a pre-aggregate microdata recode,
# while `tabxplor_lvl` exists only on a built table's index columns.
# The canonical shape IS forcats::fct_collapse()'s: a named list, one element per variable, each a
# named list of character vectors (name = merged label):
#     list(marital = list(`Not married` = c("Never married", "Divorced", "Separated")))

# The labels a level may NOT take, for the two questions that ask. Reading the OPTION (not the English
# defaults) keeps both refusals true in every locale.
#   "merge" -- a label new_lvl_collapse() may not mint: the four synthetic labels tab() makes, plus "NA".
#   "data"  -- a level the source data may not already carry: the three TOTAL labels, plus the leaf's
#              pre-rename sentinel "Total". NOT "NA" / "Others" -- those render fine and refusing them
#              would be a false positive on ordinary survey labels.
#' @keywords internal
#' @noRd
lvl_reserved_labels <- function(what = c("merge", "data")) {
  what <- match.arg(what)
  tn <- tab_total_names_merge(tx_option("total_names"))
  if (identical(what, "merge")) unname(c(tn, "NA"))
  else unique(unname(c(tn[c("row", "col", "tab")], "Total")))
}

# lvl_check_reserved() -- REFUSE a source level colliding with a label tab() mints. It aborts rather
# than warns because "Total" is the leaf's pre-rename key for every total row/tab/column: a data level
# of that name is read back as a total row (bold, out of the percentage base, printed twice), which has
# no correct reading. Runs at the END of tab_prepare(), so it also catches a collision a recode created
# (cleannames, a merge), not only one the raw data carried.
#' @keywords internal
#' @noRd
lvl_check_reserved <- function(data, vars, call = NULL) {
  if (length(vars) == 0L) return(invisible(data))
  reserved <- lvl_reserved_labels("data")
  for (v in intersect(as.character(vars), names(data))) {
    x  <- data[[v]]
    lv <- if (is.factor(x)) levels(x) else unique(as.character(x))
    bad <- intersect(lv, reserved)
    if (length(bad) == 0L) next
    cli::cli_abort(c(
      "{.var {v}} has {cli::qty(length(bad))}{?a level/levels} named {.val {bad}}, which
       {.fn tab} uses for its own total {cli::qty(length(bad))}{?row/rows}.",
      "x" = "Left alone, {cli::qty(length(bad))}{?that level/those levels} would be read as a
             total {cli::qty(length(bad))}{?row/rows}: bold, out of the percentage base, and
             printed twice.",
      "i" = "Rename the level, or move {.fn tab}'s own labels with
             {.code options(tabxplor.total_names = c(row = \"...\"))}."), call = call)
  }
  invisible(data)
}

# new_lvl_collapse() -- normalise + validate a collapse spec, or NULL for "nothing to merge". Refusals,
# each a thing that would otherwise be silently wrong:
#   - one level claimed by two groups: fct_collapse() gives it to the LAST, with no message
#   - a merged label colliding with a synthetic one: leaf_rename_totals() / tab_lump_others() key on
#     those strings, so the merged level would be treated as a total/Others
# An empty label defaults here to the constituent labels joined (so the jamovi text box can be blank).
#' @keywords internal
#' @noRd
new_lvl_collapse <- function(spec) {
  if (length(spec) == 0L) return(NULL)
  if (!is.list(spec) || is.null(names(spec)) || any(!nzchar(names(spec))))
    cli::cli_abort(c("A level-collapse spec must be a NAMED list, one element per variable.",
                     "i" = "e.g. {.code list(marital = list(`Not married` = c(\"Divorced\", \"Separated\")))}."),
                   call = NULL)
  reserved <- lvl_reserved_labels("merge")
  out <- list()
  for (v in names(spec)) {
    groups <- spec[[v]]
    if (length(groups) == 0L) next
    if (!is.list(groups)) groups <- list(groups)
    labs <- names(groups) %||% rep("", length(groups))
    keep <- list()
    for (i in seq_along(groups)) {
      lv <- as.character(unlist(groups[[i]], use.names = FALSE))
      lv <- unique(lv[!is.na(lv) & nzchar(lv)])
      if (length(lv) < 2L) next                      # a collapse never RENAMES: < 2 levels is a no-op
      lab <- if (i <= length(labs) && !is.na(labs[[i]]) && nzchar(labs[[i]])) labs[[i]]
             else paste(lv, collapse = ", ")
      if (lab %in% reserved)
        cli::cli_abort(c("{.val {lab}} cannot name a merged level of {.var {v}}.",
                         "x" = "It is one of the labels {.fn tab} mints itself.",
                         "i" = "Reserved: {.val {reserved}}."), call = NULL)
      keep[[lab]] <- unique(c(keep[[lab]], lv))      # same label twice = one group
    }
    if (length(keep) == 0L) next
    dup <- unlist(keep, use.names = FALSE)
    dup <- unique(dup[duplicated(dup)])
    if (length(dup))
      # ⚠ cli::qty() -- one quantity per message, or "Multiple quantities for pluralization".
      cli::cli_abort(c("{cli::qty(dup)}Level{?s} {.val {dup}} of {.var {v}}: in more than one merged group.",
                       "i" = "Each level may be merged into one group only."), call = NULL)
    out[[v]] <- keep
  }
  if (length(out) == 0L) NULL else out
}


# --- reading the declaration back off a table ---------------------------------------------------------

# tab_index_cols() -- the DECLARED row-index block of a table: every tabxplor_lvl column, in column
# order, with its role and variable. Returns NULL on a table that declares nothing (a hand-built frame,
# an old-version object, or a column re-created by `mutate(levels = as.character(levels))`), so callers
# fall back.
#' @keywords internal
#' @noRd
tab_index_cols <- function(tabs) {
  if (!is.data.frame(tabs)) return(NULL)
  cols <- unclass(tabs)
  keep <- vapply(cols, is_lvl, logical(1))
  if (!any(keep)) return(NULL)
  nms <- names(cols)[keep]
  list(name = nms,
       role = vapply(cols[keep], lvl_role, character(1), USE.NAMES = FALSE),
       var  = vapply(cols[keep], lvl_var , character(1), USE.NAMES = FALSE))
}

# tab_declared_vars() -- the variable model DERIVED from the declared columns, NULL when the table
# declares nothing:
#   row_var    the COLUMN holding the row levels (role "level")
#   tab_vars   the sub-table columns (role "tab_var"), in column order
#   var_col    the column NAMING each row's variable (role "var"), if any
#   row_vars   the SOURCE variable names -- the `var` column's values (merged), else the level column's
#              own `var` attribute
#   compacted  several row_vars merged into one table (a "var"-role column exists)
# WARNING: `row_var` (singular, a column name) and `row_vars` (plural, source variable names) differ on
# a merged table. A title wants the plural, an index the singular.
#' @keywords internal
#' @noRd
tab_declared_vars <- function(tabs) {
  idx <- tab_index_cols(tabs)
  if (is.null(idx)) return(NULL)
  lvl_col <- idx$name[idx$role == "level"]
  if (length(lvl_col) != 1L) return(NULL)
  var_col <- idx$name[idx$role == "var"]
  row_vars <- if (length(var_col) == 1L) {
    v <- tabs[[var_col[1]]]
    lv <- levels(v); if (is.null(lv)) unique(as.character(v)) else lv
  } else {
    v <- idx$var[idx$role == "level"]
    if (is.na(v)) character(0) else v
  }
  list(row_var   = lvl_col,
       tab_vars  = idx$name[idx$role == "tab_var"],
       var_col   = if (length(var_col) == 1L) var_col[1] else character(0),
       row_vars  = as.character(row_vars),
       compacted = length(var_col) == 1L)
}

# tab_stamp_index() -- declare a table's row-index columns in ONE call, at the point the producer knows
# the truth. Called by the two leaves through their shared tail leaf_finish() (R/tab.R), so every
# tab() / tab_many() / tab_counts() table gets it; producers that build their own index (tab_compact(),
# tab_reg(), the transpose) call new_lvl() directly on the columns they create.
#   level    the column holding the row levels
#   var      the source variable name that column's levels belong to (NA on a merged column)
#   tab_vars the sub-table columns
#   var_col  the column naming each row's variable (merged / regression tables)
#' @keywords internal
#' @noRd
tab_stamp_index <- function(tabs, level = NULL, var = NA_character_, tab_vars = character(0),
                            var_col = NULL, ordered = NULL) {
  nms <- names(tabs)
  if (!is.null(level) && length(level) == 1L && level %in% nms)
    tabs[[level]] <- new_lvl(tabs[[level]], "level", var, ordered)
  if (!is.null(var_col) && length(var_col) == 1L && var_col %in% nms)
    tabs[[var_col]] <- new_lvl(tabs[[var_col]], "var", NA_character_, logical(0))
  for (tv in intersect(as.character(tab_vars), nms))
    tabs[[tv]] <- new_lvl(tabs[[tv]], "tab_var", tv, stats::setNames(is.ordered(tabs[[tv]]), tv))
  tabs
}
