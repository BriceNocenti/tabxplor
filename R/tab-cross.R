# PURPOSE: the `a*b` entries of `tab(col_vars =)` -- a crossed pair prepared as COLUMNS.
# ROLE: the crosstab half of the interaction subsystem. Everything about what a cross IS lives in
#   R/reg-cross.R (the peel, the validation, the autocut, the two arms); this file says what each arm
#   MEANS on a column axis, and where in tab()'s pipeline it is materialised.
# KEY CONSTRAINTS:
#   - THE SAME SPELLING AND THE SAME ARMS AS tab_reg(): `a*b`, `:` refused by name, REG_CROSS_ARMS.
#     A user who has met an interaction in one producer has met it in the other.
#   - a cross is PREPARED, never special-cased downstream: after materialisation every stage reads
#     ordinary col_vars, so the aggregate, the chi2/ANOVA, the intervals, the colour engine and every
#     exporter need no code of their own;
#   - an interaction is accepted in `col_vars` ONLY. The row and tab axes take no uncut number (so a
#     `nested` cross has nothing to nest), and two factors down the rows are what `tab_vars` is;
#   - the internal column names are SYNTACTIC; the typed key is the DISPLAY identity (`col_var`).
# See: CLAUDE.md section "tabxplor architecture" (the crosstab API and pipeline).
#
# THE TWO ARMS, read as columns (REG_CROSS_ARMS, R/reg-cross.R):
#
#   cells   both parents categorical -> ONE materialised factor whose levels are the observed cells,
#           so the block is one column per cell. Both parents are absorbed, exactly as they are in a
#           model: `keeps = character(0)`.
#   nested  the modified parent continuous -> ONE MEAN COLUMN PER MODERATOR LEVEL, and the moderator
#           keeps its own block beside them (`keeps = "moderator"`). It is the column reading of the
#           model's "one slope within each level of M": here, one mean within each level of M.
#
# ⚠ WHY THE NESTED ARM IS MATERIALISED LATE. Its columns are named by the moderator's LEVELS, and a
# moderator that `shape` will cut has no levels until the cut has run -- which happens in
# tab_prepare_pop(), after the filter and the NA policy, so the breaks describe the population
# actually tabulated. So the boundary puts ONE placeholder column in `col_vars` (a numeric copy of
# the modified variable) and tab_prepare_pop() expands it, completing the settings spine exactly as
# it already completes it with `lv1` and `na`.


# === SECTION: the boundary ========================================================================

# Peel `a*b` out of a col_vars quosure, BEFORE tidyselect ever sees it: to tidyselect an operator
# between two names is arithmetic on column positions. Returns the selected names in the user's own
# order, the interaction keys among them, and the keys separately.
#' @keywords internal
#' @noRd
tab_cross_peel <- function(quo, data) {
  sl <- reg_cross_slots_quo(quo, data)
  if (is.null(sl)) return(list(vars = tidy_select_chr(quo, data), keys = character(0)))
  vars <- reg_cross_slots_select(sl, data)
  list(vars = vars, keys = unname(vars[reg_cross_has_op(vars)]))
}

# The row and tab axes refuse a key by NAME, before tidyselect turns it into "column `a * b` does not
# exist". The reasons are the maintainer's own: a row or tab variable takes no uncut number, so a
# nested cross has nothing to nest there, and two factors down the rows is what `tab_vars` already is.
#' @keywords internal
#' @noRd
tab_cross_refuse_axis <- function(quo, data, arg) {
  if (quo_miss_na_null_empty_no(quo)) return(invisible(NULL))
  sl <- reg_cross_slots_quo(quo, data)
  if (is.null(sl)) return(invisible(NULL))
  k <- vapply(sl[vapply(sl, function(s) isTRUE(s$key), logical(1))], `[[`, character(1), "value")
  if (!length(k)) return(invisible(NULL))
  cli::cli_abort(c(
    "{.arg {arg}}: {.val {k}} is an interaction, and only {.arg col_vars} takes one.",
    "i" = paste("A cross needs a column axis: it is either one column per CELL of the pair, or one",
                "mean column per level of the moderator."),
    "i" = paste0("For one sub-table per combination of the two, name them both: ",
                 "{.code tab_vars = c(", gsub("*", ", ", k[[1]], fixed = TRUE), ")}.")),
    call = NULL)
}

# THE PLAN, at the boundary: validate every key, let two continuous parents cut their moderator, and
# decide each arm. The arm is decided on the SHAPE PREDICTION, never on the column's current class --
# the same question tab_setup()'s col_var classification asks, and the reason `shape` is resolved
# (cheaply, purely) here as well as there.
#' @keywords internal
#' @noRd
tab_cross_plan <- function(keys, col_var, data, shape, tab_vars) {
  cross <- reg_parse_crosses(col_var, data, outcome = NULL, tab_vars = tab_vars, arg = "col_vars")
  if (length(cross$keys) == 0L) return(list(shape = shape, crosses = list()))
  pred  <- shape_resolve(shape, data, cross$parents, "tab")
  add   <- reg_cross_autocut(cross$keys, data, pred)
  if (length(add)) {
    shape <- c(as.list(shape %||% list()), add)
    pred  <- shape_resolve(shape, data, cross$parents, "tab")
  }
  # ⚠ a still-NUMERIC transform (`log`, `sqrt`) on a parent is refused: shape_rename_transformed()
  # renames the columns named in `col_vars`, and an ABSORBED parent is not one, so the cross would
  # silently be built on the raw column while the table claimed the transform.
  bad <- intersect(cross$parents, names(pred)[purrr::map_lgl(pred, ~ identical(shape_produces(.), "numeric"))])
  if (length(bad))
    cli::cli_abort(c(
      "{.arg shape}: {.val {bad}} is a variable of an interaction, so it cannot keep a number.",
      "i" = paste("A cross needs levels to cross. Cut it instead --",
                  '{.code shape = c({bad[[1]]} = "quartiles")}.')), call = NULL)
  list(shape = shape, crosses = reg_cross_resolve(cross$keys, data, pred, arg = "col_vars"))
}

# THE PLACEHOLDERS, one column per cross, so tidyselect and the whole settings spine see an ordinary
# col_var. `cells` is already its final column; `nested` is a numeric stand-in that
# tab_cross_materialise() expands once the moderator's levels exist.
# The column is named by tab_cross_col(), not by the key: `tab()` re-selects its variables at several
# later stages, and a `*` in a data-frame name is not worth auditing every one of them for. The typed
# key stays the DISPLAY identity, stamped as `col_var`.
#' @keywords internal
#' @noRd
tab_cross_placeholders <- function(data, crosses, col_var) {
  for (r in crosses) {
    nm <- tab_cross_col(r$key)
    data[[nm]] <- if (identical(r$arm, "cells"))
      reg_cross_column(data[[r$modified]], data[[r$moderator]]) else data[[r$modified]]
  }
  # `cells` absorbs both parents; `nested` prints the moderator's own block first, then its means.
  vars <- unlist(lapply(col_var, function(v) {
    r <- crosses[[v]]
    if (is.null(r)) v
    else if (identical(r$arm, "cells")) tab_cross_col(r$key)
    else c(r$moderator, tab_cross_col(r$key))
  }), use.names = FALSE)
  list(data = data, col_var = unique(vars))
}

# The syntactic internal name of a cross's column(s). One function, so the boundary, the
# materialisation and the restamp cannot disagree.
#' @keywords internal
#' @noRd
tab_cross_col <- function(key) paste0(".tx_x_", gsub("[^A-Za-z0-9_.]", "_", key))

#' @keywords internal
#' @noRd
tab_cross_col_lvl <- function(key, i) paste0(tab_cross_col(key), "_", i)


# === SECTION: the materialisation, in tab_prepare_pop() ===========================================

# Expand every `nested` placeholder into one numeric column per moderator level -- the modified
# variable where the moderator is that level, NA elsewhere -- and complete the settings spine with
# the new columns. A numeric col_var's own aggregate already excludes NAs from both the mean and the
# base count (num_moment_scan(), R/tab-agg.R), so each column's `n` IS its group's count and nothing
# downstream learns a thing.
# ⚠ RUNS AFTER tab_prepare()'s `na` policy, never before: under `na = "drop_all"` the complete-case
# rule is applied to the placeholder -- one variable -- where applied to K mutually exclusive columns
# it would empty the table, every row being NA in all but one of them.
#' @keywords internal
#' @noRd
tab_cross_materialise <- function(data, crosses, col_vars, settings, shapes = list()) {
  if (!length(crosses)) return(list(data = data, col_vars = col_vars, settings = settings,
                                    crosses = crosses))
  # ⚠ A `cells` COLUMN IS REBUILT WHERE A PARENT WAS SHAPED. The boundary built it from the raw
  # parents so tidyselect and the variable classification had a column to read; the cut runs here,
  # and a combination of raw values is not a combination of groups.
  for (r in crosses)
    if (identical(r$arm, "cells") && any(c(r$modified, r$moderator) %in% names(shapes)) &&
        all(c(r$modified, r$moderator) %in% names(data)))
      data[[tab_cross_col(r$key)]] <- reg_cross_column(data[[r$modified]], data[[r$moderator]])
  nested <- purrr::keep(crosses, ~ identical(.$arm, "nested"))
  if (!length(nested)) return(list(data = data, col_vars = col_vars, settings = settings,
                                   crosses = crosses))
  cv  <- vars_chr(col_vars)
  map <- stats::setNames(as.list(cv), cv)                    # every column maps to itself...
  for (nm_key in names(nested)) {                            # ... but a placeholder to its levels
    r  <- nested[[nm_key]]
    ph <- tab_cross_col(r$key)
    if (!ph %in% cv) next
    lv <- levels(forcats::fct_drop(as.factor(data[[r$moderator]])))
    nm <- tab_cross_col_lvl(r$key, seq_along(lv))
    for (i in seq_along(lv))
      data[[nm[[i]]]] <- ifelse(!is.na(data[[r$moderator]]) & data[[r$moderator]] == lv[[i]],
                                as.numeric(data[[ph]]), NA_real_)
    data[[ph]]  <- NULL
    map[[ph]]   <- nm
    # the LEVELS ride on the record, which is what the rename reads: an internal column name is
    # positional on purpose (a level may hold anything, a column name may not).
    crosses[[nm_key]]$levels <- lv
  }
  k   <- lengths(map)
  new <- unlist(map, use.names = FALSE)
  list(data = data, col_vars = rlang::syms(new), crosses = crosses,
       settings = tab_cross_expand_spine(settings, k, new))
}

# The spine's own expansion: `cols` is one row per col_var and `pairs` is ROW-MAJOR over them, so a
# placeholder's row is simply repeated once per column it became. Every other setting the block
# carries -- its pct, its ref, its digits, its `lvs` -- is a fact about the BLOCK and applies to each
# of its columns unchanged.
#' @keywords internal
#' @noRd
tab_cross_expand_spine <- function(settings, k, new) {
  if (all(k == 1L)) return(settings)
  i   <- rep(seq_along(k), k)
  nrv <- nrow(settings$pairs) / nrow(settings$cols)
  settings$cols <- settings$cols[i, , drop = FALSE]
  settings$cols$col_var <- new
  settings$pairs <- settings$pairs[rep(seq_len(nrow(settings$pairs)), rep(k, times = nrv)), ,
                                   drop = FALSE]
  settings$pairs$col_var <- rep(new, times = nrv)
  settings
}


# === SECTION: the display identity ================================================================

# Stamp the typed key onto every column a cross produced, so the block reads as ONE block named
# `age*race` -- tab_col_block_ids() keys on `col_var`, and tab_col_units() then writes the unit once
# -- and rename each `nested` column to the moderator LEVEL it holds.
# ⚠ AFTER the numeric/text column sort (tab_transform), which matches `col_var` against `col_vars`:
# stamping earlier would leave every cross column unmatched and dump the whole block last.
# ⚠ the level ALONE would collide with the moderator's own block, which prints the same levels as
# columns of its own -- and a duplicate name is not a table. `<level>_<key>` is a spread's own
# convention, and the export prep already strips a trailing `_<col_var>` off a header, so the column
# shows `White` under a span reading `age*race`.
#' @keywords internal
#' @noRd
tab_cross_stamp <- function(tab, crosses) {
  if (!length(crosses) || !is.data.frame(tab)) return(tab)
  nms <- names(tab)
  cv  <- purrr::map_chr(tab, ~ if (is_fmt(.)) get_col_var(.) else NA_character_)
  for (r in crosses) {
    ph  <- tab_cross_col(r$key)
    hit <- which(!is.na(cv) & (cv == ph | startsWith(cv, paste0(ph, "_"))))
    if (!length(hit)) next
    for (j in hit) tab[[j]] <- set_col_var(tab[[j]], r$key)
    # a `cells` column is already named by its own cell; only a nested one is named positionally
    if (identical(r$arm, "nested") && length(r$levels)) {
      i  <- suppressWarnings(as.integer(sub(paste0("^\\Q", ph, "\\E_"), "", cv[hit], perl = TRUE)))
      ok <- !is.na(i) & i >= 1L & i <= length(r$levels)
      nms[hit[ok]] <- paste0(r$levels[i[ok]], "_", r$key)
    }
  }
  names(tab) <- nms
  tab
}
