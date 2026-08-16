# =====================================================================================================
# R/table-spec.R -- THE TABLE IDENTITY (Phase 19g, KEY 6)
# =====================================================================================================
# PURPOSE: one statement of *what kind of table this is, which variables are in it, and how it was
#   made* -- for BOTH producers. Before this file a crosstab recorded its variables in `meta$vars` and
#   a regression recorded none of them, carrying a parallel 20-field vocabulary in `meta$reg_meta`
#   instead; and the KIND of table was not stored at all -- `is_reg_footer()` decided "is this a
#   regression" by asking whether the `test` tibble happened to contain a reg-flavoured discriminator,
#   in the same file whose header comment said a reg table carries `reg_meta`. Two encodings of one
#   fact, one of them unused.
#
# THE MODEL -- ONE `meta$spec`, three slots and no more:
#
#   spec$kind   "crosstab" | "regression".  Stated by the producer, never sniffed.
#   spec$vars   what NO COLUMN can carry: `wt`, `caption`, `var_labels`.  Everything else about the
#               variable model is DERIVED -- the row axis from the declared index columns
#               (R/row-model.R: tab_declared_vars), the column axis from the fmt columns' own
#               `col_var` attribute. Uniform across producers by construction, because it is the
#               columns themselves that answer.
#   spec$call   the producer's own recipe: how this table was made. A regression stores its model
#               record here (family / outcome / predictors / reference / `fit_spec`, the ~4 KB
#               recipe reg_check_plots() refits from). A crosstab stores nothing yet -- everything it
#               would record already rides its columns -- so the slot is absent there rather than
#               filled with a duplicate.
#
# WARNING: `spec` is OPTIONAL, like every other table-level fact (Phase k's missing-metadata
#   contract). A table that lost it -- or a plain tibble in the middle of a pipeline -- still prints
#   and exports; tab_kind() then DERIVES the kind from the `test` tibble's discriminators, which is
#   what the deleted is_reg_footer() did full time. That path is the degraded fallback, never the
#   normal one.
# =====================================================================================================


# --- the declared table kinds ------------------------------------------------------------------------
# "crosstab"    tab() / tab_plain() / tab_num() / tab_counts() and everything derived from them
# "regression"  tab_reg() and its wrappers
TAB_KINDS <- c("crosstab", "regression")


# new_spec() -- the constructor. Empty slots are dropped, so "absent when unset" holds at the
# sub-field level too (a spec is never a list of NULLs).
#' @keywords internal
#' @noRd
new_spec <- function(kind = "crosstab", vars = NULL, call = NULL) {
  kind <- match.arg(kind, TAB_KINDS)
  out <- list(kind = kind, vars = vars, call = call)
  out[!vapply(out, is.null, logical(1))]
}

#' @keywords internal
#' @noRd
get_spec <- function(x) get_meta(x)[["spec"]]

# Write ONE spec slot, preserving the others. NULL removes the slot, and an emptied spec removes the
# whole thing -- "absent when unset", one level deeper.
# WARNING: it deliberately does NOT invent a `kind`. Stating the kind is the PRODUCER's job (through
# new_spec()); materialising tab_kind()'s degraded guess here would turn a fallback into a stored
# fact, and a `set_vars_attr(x, NULL)` could then no longer leave the table as it found it.
#' @keywords internal
#' @noRd
set_spec_field <- function(x, field, value) {
  sp <- get_spec(x) %||% list()
  sp[[field]] <- value
  sp <- sp[!vapply(sp, is.null, logical(1))]
  set_meta_field(x, "spec", if (length(sp)) sp else NULL)
}

# tab_kind() -- what kind of table this is. The stored fact first; the DEGRADED fallback (a table that
# lost its `meta`) reads the `test` tibble's discriminators, exactly as the deleted is_reg_footer()
# did. Phase 20c: it asks TEST_ROWS which discriminators belong to the reg PRODUCER, so it no longer
# has to know that a regression carrying only interaction rows (`stats = FALSE`) is one whose keys
# live outside the footer spec -- two vocabularies became one column.
#' @keywords internal
#' @noRd
tab_kind <- function(x) {
  k <- get_spec(x)[["kind"]]
  if (!is.null(k)) return(k)
  tt <- get_test(x)
  if (!is.null(tt) && nrow(tt) > 0 && any(tt$test %in% TEST_REG_KEYS)) "regression"
  else "crosstab"
}

#' @keywords internal
#' @noRd
tab_is_reg <- function(x) identical(tab_kind(x), "regression")

# tab_call() -- the producer's recipe, or NULL. `reg_call()` is the same read, gated on the kind, so a
# crosstab can never be mistaken for a model record by a consumer that forgot to ask.
#' @keywords internal
#' @noRd
tab_call <- function(x) get_spec(x)[["call"]]

#' @keywords internal
#' @noRd
reg_call <- function(x) if (tab_is_reg(x)) tab_call(x) else NULL

#' @keywords internal
#' @noRd
set_reg_call <- function(x, call) {
  x <- set_meta_field(x, "spec", new_spec("regression", vars = get_spec(x)[["vars"]], call = call))
  x
}

# reg_spec() -- the identity a regression table carries out of reg_build(): its kind, plus the one
# thing no column of it can carry (the variable-label map for the opt-in name swap). The `call`
# recipe is attached at the tail of tab_reg(), where the whole model record is known.
#' @keywords internal
#' @noRd
reg_spec <- function(var_labels = character(0)) {
  new_spec("regression",
           vars = if (length(var_labels)) new_vars_attr(var_labels = var_labels) else NULL)
}

# The bind reconcile of two specs (the `meta_bind_rules` entry). Slot by slot, x wins on a
# disagreement -- the same "first non-NULL" default every other `meta` sub-field takes, applied one
# level deeper so a bind cannot drop the recipe of one side while keeping the vars of the other.
#' @keywords internal
#' @noRd
spec_bind <- function(sx, sy) {
  if (is.null(sx)) return(sy)
  if (is.null(sy)) return(sx)
  out <- list(kind = sx$kind %||% sy$kind,
              vars = sx$vars %||% sy$vars,
              call = sx$call %||% sy$call)
  out[!vapply(out, is.null, logical(1))]
}
