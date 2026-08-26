# PURPOSE: how a declared fact table is WRITTEN, and the fold that turns the written grid into the
#   shape every reader indexes.
# ROLE: the declarative architecture's one shared mechanism. A fact table states each fact once, in
#   one row; this file says what that row looks like on the page, and hands back the named list of
#   named lists the accessors, the roxygen generators and zzz-fact-keys.R already read.
# KEY CONSTRAINTS:
#   - This file sorts FIRST in C collation, and must: a grid is folded at SOURCE time, so every
#     table built with tx_grid() lives in a file that comes after it.
#   - A NULL cell means "this row has no such field" and is DROPPED, so `%||%` reads keep working;
#     NA means the field is declared and empty, and is kept. Nothing else distinguishes them.
# See: CLAUDE.md section "The declarative architecture".

# THE GRID RULE -- one row per fact, fields in one fixed order, aligned in columns, a column
# dictionary immediately above, nothing about a row stated anywhere else. A row may run long: it is
# a grid, not prose, and it is read unwrapped.
#   every field a scalar        -> a tibble::tribble(), folded by tx_grid() below
#   a closure or a doc paragraph -> a list() of one aligned block per row; the fixed field order and
#                                   the alignment are what make it a grid
# A tribble takes comment lines BETWEEN rows, so a threshold's justification still sits on its row,
# and a cell holds anything R holds -- a closure, a quote(), a vector -- as a list column.

# Fold a written grid into the named list of named lists every reader indexes. `key` names the
# column holding the row names.
#' @keywords internal
#' @noRd
tx_grid <- function(x, key = 1L) {
  keys <- as.character(x[[key]])
  cols <- setdiff(names(x), names(x)[[key]])
  rows <- lapply(seq_len(nrow(x)), function(i) {
    r <- lapply(cols, function(cn) { v <- x[[cn]]; if (is.list(v)) v[[i]] else v[[i]] })
    names(r) <- cols
    r[!vapply(r, is.null, logical(1))]
  })
  stats::setNames(rows, keys)
}
