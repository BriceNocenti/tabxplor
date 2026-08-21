# PURPOSE: The table identity -- what kind of table this is, which variables are in it, and how it
#   was made -- for both producers (tab() and tab_reg()).
# ROLE: One `meta$spec` list, read through tab_kind() / tab_is_reg() / tab_call(); consumed by the
#   structure model (R/tab-structure.R), the exporters and the render tail.
# KEY CONSTRAINTS:
#   - `spec$kind` is STATED by the producer, never sniffed from the data.
#   - `spec` is OPTIONAL: a table that lost it (or a plain tibble mid-pipeline) still prints and
#     exports. tab_kind() then DERIVES the kind from the `test` tibble's discriminators (TEST_REG_KEYS)
#     -- the degraded fallback, never the normal path.
# See: CLAUDE.md § tabxplor architecture (declarative architecture); R/tab-structure.R.
#
# THE MODEL -- one `meta$spec`, three slots:
#   kind   "crosstab" | "regression".
#   vars   only what NO COLUMN can carry: `wt`, `caption`, `var_labels`. The rest of the variable
#          model is DERIVED -- the row axis from the declared index columns (R/row-model.R), the
#          column axis from the fmt columns' own `col_var` attribute -- so it is uniform by
#          construction.
#   call   the producer's recipe. A regression stores its model record (family / outcome / predictors
#          / reference / `fit_spec`, the recipe reg_check_plots() refits from); a crosstab needs none
#          (everything already rides its columns), so the slot is absent there.


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

# Write ONE spec slot, preserving the others. NULL removes the slot; an emptied spec drops the whole
# thing ("absent when unset").
# WARNING: never invents a `kind` -- stating the kind is the producer's job. Materialising
# tab_kind()'s degraded guess here would turn a fallback into a stored fact.
#' @keywords internal
#' @noRd
set_spec_field <- function(x, field, value) {
  sp <- get_spec(x) %||% list()
  sp[[field]] <- value
  sp <- sp[!vapply(sp, is.null, logical(1))]
  set_meta_field(x, "spec", if (length(sp)) sp else NULL)
}

# tab_kind() -- what kind of table this is: the stored fact first; the degraded fallback (a table that
# lost its `meta`) reads whether the `test` tibble carries a reg-producer discriminator (TEST_REG_KEYS,
# declared in TEST_ROWS).
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

# spec_bind() -- the bind reconcile of two specs (the `meta_bind_rules` entry): slot by slot, first
# non-NULL wins, so a bind cannot drop one side's recipe while keeping the other's vars.
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
