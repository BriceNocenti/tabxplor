# PURPOSE: interactions -- a crossed pair, prepared as a VARIABLE before anything reads it.
# ROLE: the SHARED surface, for both producers. The `a*b` entries of `tab_reg(predictors =)` are
#   parsed at the argument boundary, materialised at the end of the preparation, and read back by the
#   skeleton, the counts, the crude block and the footer test; `tab(col_vars =)` reuses the peel, the
#   validation, the autocut and the two arms, and R/tab-cross.R says what each arm means as COLUMNS.
#   Every message names its own argument (`arg =`).
# KEY CONSTRAINTS:
#   - a cross is prepared, never special-cased downstream: every subsystem must keep reading an
#     ordinary predictor (or, in tab(), an ordinary col_var);
#   - the two arms are declared in REG_CROSS_ARMS, never re-derived from a pair of kinds -- and the
#     kind a parent WILL have is what decides, `shape` included: reg_cross_resolve() runs after the
#     recode in tab_reg() and before it in tab(), and must answer the same either way;
#   - a parent may not also be a plain predictor;
#   - `a*b` must be read from an INJECTED literal as well as from an inline call or a variable:
#     the jamovi bridge builds its tab_reg() call with rlang::inject(), and quo_peek_extern() sees
#     only a bare symbol -- so reg_cross_slots_quo() reads the expression's own value;
#   - the block is NAMED AS IT WAS TYPED (`age*tvhours`), so the var column, the footer rows and
#     reg_formulas() all read the string the user wrote in `predictors`.
# See: CLAUDE.md section "tabxplor architecture" (the regression subsystem).
#
# THE IDEA, in one sentence: an interaction is a predictor whose levels are combinations, and whose
# univariable model is its own saturated fit. Prepare the pair as a variable and there is nothing to
# integrate -- reg_skeleton() builds `term = paste0(var, level)`, reg_column() joins on that term,
# reg_level_counts() counts a factor column, reg_crude_saturated() gives a factor the exact closed
# form, and reg_marginal_gcomp() sweeps whatever levels the column has. It is the same move
# `shape` makes for ONE variable, and `ref`'s anchor makes for one column's origin.
#
# TWO ARMS, and the difference is a parametrisation, never a statistic (`y ~ X*M`, `y ~ M/X` and
# `y ~ combined_factor` are the same fit -- identical logLik and rank, fitted values within 5.6e-16):
#
#   cells   both parents categorical -> ONE materialised factor whose levels are the observed cells,
#           every cell read against one common reference cell. Both parents are absorbed.
#   nested  the modified parent continuous -> the term `M:X` beside M's own main effect, i.e. R's
#           own `M/X`. The rows are X's slope WITHIN each level of M, straight out of the fit, and
#           the moderator keeps its own row block because the model contains it.
#
# WHY THE ORDER MATTERS. The pair is parsed in S2 (so both parents reach `shape`, `multiplier`, the
# anchor and the complete-case frame as ordinary variables) and materialised at the END of S5 -- after
# the shape recode (a parent may be cut to a factor), after the anchor shift, and after the relevel,
# because the REFERENCE CELL is composed from the parents' own `ref` and there is no second grammar
# for it.


# The two arms, and everything that differs between them. A third arm would be a row here.
#   kinds   the (modified, moderator) variable kinds it applies to
#   makes   "column" a materialised factor in `data` | "term" an RHS term beside the moderator
#   keeps   which parent stays a model predictor of its own (the rest are absorbed)
#   crude   "closed_form" the saturated cell table | "nested_fit" the univariable `y ~ M/X`
#   count   which column a row's `n` is counted on
#' @keywords internal
#' @noRd
REG_CROSS_ARMS <- list(
  cells  = list(kinds = c(modified = "factor",  moderator = "factor"),
                makes = "column", keeps = character(0),
                crude = "closed_form", count = "cell"),
  nested = list(kinds = c(modified = "numeric", moderator = "factor"),
                makes = "term",   keeps = "moderator",
                crude = "nested_fit", count = "moderator")
)

# The CELL separator, inside a combined factor's own levels ("White \u00b7 [0,1)"). \uXXXX so the
# source stays ASCII.
# WARNING: this is DATA -- a materialised factor's levels, and every key built from them -- not a
# label. The nested arm's row LABEL uses its own separator (reg_stage_rows), which is free to change.
#' @keywords internal
#' @noRd
reg_cross_sep <- function() " \u00b7 "

# The NESTED arm's row-label separator, between "<var> per <unit>" and the moderator's level. A dash
# rather than the cell dot: the two sides name different variables, where a combined factor's level
# joins two values of one comparison.
#' @keywords internal
#' @noRd
reg_cross_row_sep <- function() " \u2014 "

# The rows of a NESTED block: slopes, so they have no level pair and no adjusted level of their own
# (reg_fill_base() skips them). Read as VAR names, which since the block is named by its own key are
# the keys themselves.
#' @keywords internal
#' @noRd
reg_cross_nested_vars <- function(crosses) {
  if (!length(crosses)) return(character(0))
  keep <- vapply(crosses, function(r) identical(r$arm, "nested"), logical(1))
  vapply(crosses[keep], function(r) r$var, character(1), USE.NAMES = FALSE)
}


# === SECTION: the surface -- peeling `a*b` out of a tidy-select ===================================
#
# THE SPELLING IS `a*b`, R's own, and it is the honest one: what tabxplor fits IS `a + b + a:b`
# (a combined factor spans exactly that, and the nested arm is `M/X`). R's `a:b` means something
# else -- the interaction term WITHOUT its main effects -- which happens to coincide for two factors
# and does not for a continuous parent (measured: logLik -14505.62 against -14443.20), and which is
# the origin-dependent model reg_cross_resolve() exists to refuse. So `:` is not a synonym here: it
# is intercepted and aborted, naming `*` for an interaction and `all_of()` for a column range.
#
# The peel runs BEFORE selection, on the expression and on a peeked value alike, because tidyselect
# sees an operator between two names as nothing but positions.

# Does this string carry an interaction operator? `:` is caught too, so it can be REFUSED with the
# message that names `*` rather than falling through to tidyselect's "column does not exist".
#' @keywords internal
#' @noRd
reg_cross_has_op <- function(x) grepl("[*:]", x)

# Is this top-level element an interaction rather than something to select?
#' @keywords internal
#' @noRd
reg_cross_is_term <- function(el) {
  if (is.character(el) && length(el) == 1L) return(reg_cross_has_op(el))
  # `1:3` is a positional range, not a pair of variables -- only names and strings cross.
  (rlang::is_call(el, "*") || rlang::is_call(el, ":")) && length(rlang::call_args(el)) == 2L &&
    all(vapply(rlang::call_args(el),
               function(a) rlang::is_symbol(a) || (is.character(a) && length(a) == 1L),
               logical(1)))
}

# The key, KEEPING the operator the user wrote -- reg_parse_crosses() refuses `:` by reading it back.
#' @keywords internal
#' @noRd
reg_cross_key <- function(el) {
  if (is.character(el)) return(gsub("\\s", "", el))
  op <- if (rlang::is_call(el, "*")) "*" else ":"
  paste(vapply(rlang::call_args(el), function(a)
    if (rlang::is_symbol(a)) rlang::as_string(a) else as.character(a), character(1)), collapse = op)
}

# Split a character vector into the names to select and the interaction keys.
#' @keywords internal
#' @noRd
reg_cross_peel_chr <- function(x) {
  if (!is.character(x)) return(list(plain = x, keys = character(0)))
  hit <- reg_cross_has_op(x)
  list(plain = unname(x[!hit]), keys = vapply(x[hit], reg_cross_key, character(1), USE.NAMES = FALSE))
}

# One model's declaration as ORDERED SLOTS -- each top-level element is either an interaction key or
# something to select -- so a cross keeps the position the user wrote it at. NULL means "no
# interaction here", and the caller selects the whole expression in one go, exactly as before.
#
# ⚠ with an interaction present the other elements are selected ONE AT A TIME, which is what keeps
# the order exact. A selection that only makes sense relative to the others (a bare `-x`) therefore
# cannot share a `c()` with an interaction -- write it as its own `predictors` call.
#' @keywords internal
#' @noRd
reg_cross_slots_chr <- function(x) {
  lapply(x, function(el) if (reg_cross_has_op(el))
    list(key = TRUE, value = reg_cross_key(el)) else list(key = FALSE, value = el))
}

#' @keywords internal
#' @noRd
reg_cross_slots_quo <- function(quo, data) {
  expr <- rlang::quo_get_expr(quo)
  env  <- rlang::quo_get_env(quo)
  els  <- if (rlang::is_call(expr, "c")) rlang::call_args(expr) else list(expr)
  hit  <- vapply(els, reg_cross_is_term, logical(1))
  if (any(hit))
    return(lapply(seq_along(els), function(i)
      if (hit[[i]]) list(key = TRUE, value = reg_cross_key(els[[i]]))
      else list(key = FALSE, quo = rlang::new_quosure(els[[i]], env))))
  # ⚠ an INJECTED literal: `rlang::inject(predictors = !!p)` splices the VALUE into the expression,
  # so it is neither a bare symbol (all quo_peek_extern() looks at) nor a `c()` call, and the peek
  # below returns NULL. The jamovi bridge builds its call exactly that way on purpose -- an injected
  # value cannot be mistaken for a dataset column -- so without this line every interaction picked
  # there tried to SELECT a column named `a*b`, and the fold could never work end to end.
  if (is.character(expr) && any(reg_cross_has_op(expr))) return(reg_cross_slots_chr(expr))
  # a variable holding the names: the interaction is in its VALUE, which tidyselect never sees.
  pk <- quo_peek_extern(quo, data)
  if (is.character(pk) && any(reg_cross_has_op(pk))) return(reg_cross_slots_chr(pk))
  NULL
}

# Select one slot list, in order.
#' @keywords internal
#' @noRd
reg_cross_slots_select <- function(slots, data) {
  unlist(lapply(slots, function(s)
    if (isTRUE(s$key)) s$value
    else tidy_select_chr(s$quo %||% rlang::quo(tidyselect::all_of(!!s$value)), data)),
    use.names = FALSE)
}


# === SECTION: the boundary -- parse (S2), then materialise (S5) ===================================

# S2. Validate every `a*b` entry and name both parents, so they reach `shape` / `multiplier` / `ref`
# and the complete-case frame as ordinary variables. The ARM is not decided here: it depends on the
# kinds AFTER `shape`, which has not run yet.
#' @keywords internal
#' @noRd
reg_parse_crosses <- function(predictors, data, outcome, tab_vars = NULL,
                              arg = "predictors") {
  keys <- unique(unlist(lapply(if (is.list(predictors)) predictors else list(predictors),
                               function(p) reg_cross_peel_chr(p)$keys), use.names = FALSE))
  if (length(keys) == 0L) return(list(keys = character(0), parents = character(0)))
  # `:` is R's interaction TERM, i.e. the pair without its main effects -- a different model, and
  # the origin-dependent one. Refuse it by name rather than fit something else under its spelling.
  colon <- keys[grepl(":", keys, fixed = TRUE)]
  if (length(colon) > 0L) {
    star <- gsub(":", "*", colon[[1]], fixed = TRUE)
    cli::cli_abort(c(
      "{.arg {arg}}: {.val {colon}} -- {.code :} is not how an interaction is written here.",
      "i" = paste("Write {.code {star}}. In R {.code a:b} is the interaction term WITHOUT its main",
                  "effects, which is a different model and depends on where each variable's zero",
                  "is; {.code a*b} is {.code a + b + a:b}, which is what this table fits."),
      "i" = "For a RANGE of columns, name them: {.code all_of(names(data)[3:7])}."), call = NULL)
  }
  parts <- lapply(keys, function(k) strsplit(k, "*", fixed = TRUE)[[1]])
  for (i in seq_along(keys)) {
    p <- parts[[i]]
    if (length(p) != 2L || !all(nzchar(p)))
      cli::cli_abort(c("{.arg {arg}}: {.val {keys[[i]]}} is not a pair of variables.",
                       "i" = "An interaction crosses exactly two: {.code a*b}."), call = NULL)
    bad <- setdiff(p, names(data))
    if (length(bad) > 0L)
      cli::cli_abort(c("{.arg {arg}}: {.val {keys[[i]]}} names {?a column/columns} that {?does/do} not exist.",
                       "x" = "Not {?a column/columns} of {.arg data}: {.val {bad}}.",
                       "i" = "An interaction is written {.code a*b} with both variables named."),
                     call = NULL)
    if (p[[1]] == p[[2]])
      cli::cli_abort("{.arg {arg}}: {.val {keys[[i]]}} crosses {.val {p[[1]]}} with itself.",
                     call = NULL)
    clash <- intersect(p, c(as.character(outcome), vars_chr(tab_vars)))
    if (length(clash) > 0L) {
      role <- if (is.null(outcome)) "{.arg tab_vars}" else "the outcome or {.arg tab_vars}"
      cli::cli_abort(c("{.arg {arg}}: {.val {keys[[i]]}} crosses a variable with another role.",
                       "x" = paste0("{.val {clash}} {?is/are} already ", role, ".")),
                     call = NULL)
    }
  }
  parents <- unique(unlist(parts, use.names = FALSE))
  # THE PARENT RULE, one for both arms: the cross supplies its parents. It refuses no model a user
  # can soundly ask for -- `a:b`, `a + a:b` and `a + b + a:b` are ONE fit wherever the moderator is
  # categorical (measured identical), and listing a parent beside the pair is either rank-deficient
  # (`cells`) or a silent reparametrisation (`nested`, where R codes a slope per level only while
  # the modified variable has no main effect). The message therefore says so, rather than only
  # refusing. See dev/reg_interactions_and_predictor_terms.md section 8.
  # ⚠ PER MODEL, never across the list: `list(additive = c(a, b), crossed = c(a:b))` is exactly the
  # comparison this spelling exists for, and a whole-call check would refuse it.
  for (mp in if (is.list(predictors)) predictors else list(predictors)) {
    pl  <- reg_cross_peel_chr(mp)
    own <- unique(unlist(parts[match(pl$keys, keys)], use.names = FALSE))
    dup <- intersect(pl$plain, own)
    if (length(dup) > 0L)
      cli::cli_abort(c(
        "{.arg {arg}} lists {.val {dup}} beside an interaction it is part of.",
        "i" = if (is.null(outcome))
          paste("A CROSS SUPPLIES ITS VARIABLES: two factors become one block of cells, and a",
                "number crossed with a factor already prints the factor's own block beside its",
                "means.")
        else paste("The MODEL is the same either way: {.code a*b} IS {.code a + b + a:b}, so an",
                   "interaction already carries both its variables."),
        "i" = if (is.null(outcome))
          "Drop {.val {dup}}. What changes is only which COLUMNS are printed."
        else paste("Drop {.val {dup}}. What changes is only which ROWS are printed -- one per cell",
                   "of the pair, or one slope per group, instead of a main effect plus",
                   "differences.")), call = NULL)
  }
  list(keys = keys, parents = parents)
}

# TWO CONTINUOUS PARENTS have no rows to cross, so the MODERATOR is cut into quartiles and the table
# is built -- with a one-line message, never silently. The cut is a MODELLING choice, not a
# presentation one: it changes the fit, and the `Interaction (LR)` row moves with the bin count
# (measured, p = 0.0007 at 4 groups against 0.0086 at 3). So it is stated, and both ways out are
# named -- pick the cut with `shape`, or swap the order to cut the other one.
#
# It runs where `shape` is RESOLVED and before it is applied (S2, block G), because that is the one
# point where "will this variable still be continuous?" is answerable. A variable the user has
# already shaped is left alone: their choice stands, and if it keeps the moderator continuous
# (`"log"`, `"sqrt"`) reg_cross_resolve() aborts as before.
#' @keywords internal
#' @noRd
reg_cross_autocut <- function(keys, data, reg_shapes) {
  add <- list()
  cuts <- c("quantiles", "sd_bands")
  stays_numeric <- function(v)
    !is.null(data[[v]]) && !reg_is_factor_var(data[[v]]) &&
      !(reg_shapes[[v]]$kind %||% "") %in% cuts
  for (k in keys) {
    p <- strsplit(k, "*", fixed = TRUE)[[1]]
    if (length(p) != 2L) next
    md <- p[[1]]; mr <- p[[2]]
    if (!stays_numeric(md) || !stays_numeric(mr)) next
    if (!is.null(reg_shapes[[mr]]) || !is.null(add[[mr]])) next
    add[[mr]] <- "quartiles"
    cli::cli_inform(c("i" = paste0(
      "{.code ", k, "}: two continuous variables have no cells to cross, so {.val ", mr, "} was cut ",
      "-- {.code shape = c(", mr, ' = "quartiles")}. Write {.code ', mr, "*", md,
      "} to cut {.val ", md, "} instead.")))
  }
  add
}

# S5, last of all. Decide each cross's arm from the FINAL columns, then build what the arm makes.
#' @keywords internal
#' @noRd
reg_cross_resolve <- function(keys, data, reg_shapes = NULL, arg = "predictors") {
  if (length(keys) == 0L) return(list())
  out <- stats::setNames(vector("list", length(keys)), keys)
  for (i in seq_along(keys)) {
    k  <- keys[[i]]
    p  <- strsplit(k, "*", fixed = TRUE)[[1]]
    md <- p[[1]]; mr <- p[[2]]
    # ⚠ the kind a variable WILL HAVE, not the one it has. In tab_reg() this runs after the shape
    # recode and the two answers agree; in tab() it runs at the boundary, where a column about to be
    # cut is already a factor as far as every classification is concerned -- the same prediction
    # tab_setup()'s own col_var classification makes.
    kind <- function(v)
      if (reg_is_factor_var(data[[v]]) || shape_is_factor(reg_shapes[[v]])) "factor" else "numeric"
    # ⚠ THE SWAP. `*` is symmetric in the MODEL -- `a*b` and `b*a` are one fit -- so where only the
    # ORDER is wrong there is exactly one table that can exist, and refusing it would make the user
    # retype for no information. Swap to it and say so in one line; the block is then named as the
    # swap, which is what the var column, the footer and reg_formulas() print.
    if (kind(mr) != "factor" && kind(md) == "factor") {
      cli::cli_inform(c("i" = paste0(
        "{.code ", k, "} read as {.code ", mr, "*", md, "}: the rows are about the FIRST variable, ",
        "and only a continuous one has slopes to show within groups.")))
      swap <- md; md <- mr; mr <- swap
      k    <- paste0(md, "*", mr)
    }
    if (kind(mr) != "factor")
      cli::cli_abort(c(
        "{.arg {arg}}: {.val {k}} needs a categorical moderator.",
        "x" = "{.val {mr}} is continuous.",
        "i" = paste0('Cut it: {.code shape = c(', mr, ' = "quartiles")} -- one slope per group, ',
                     "each with its own count and observed effect."),
        "i" = if (identical(arg, "predictors"))
          paste0("For the classical coefficient instead, write the model as a formula: ",
                 "{.code outcome = y ~ ... + ", md, " * ", mr, "}.")
        else NULL), call = NULL)
    arm <- if (kind(md) == "factor") "cells" else "nested"
    if (arm == "nested" && identical(reg_shapes[[md]]$kind %||% "", "quadratic"))
      cli::cli_abort(c("{.arg {arg}}: {.val {k}} cannot cross a squared predictor.",
                       "x" = paste("{.code shape = c({md} = \"quadratic\")} adds a term that would",
                                   "sit outside the interaction."),
                       "i" = 'Cut it instead: {.code shape = c({md} = "quartiles")}.'), call = NULL)
    # DESIGN: the block is NAMED AS IT WAS TYPED -- `age*tvhours`, the `predictors =` key itself.
    # `*` is the one accepted spelling (`:` is refused above), so the key is canonical, and a
    # prettified "age x tvhours" would only make the var column, the footer rows and reg_formulas()
    # disagree with the call the user wrote.
    # ⚠ the list NAME stays the DECLARED key -- `sp$cross`, reg_cross_add() and reg_cross_keys() all
    # look a record up by what the user typed -- while `key`/`var` carry the swap, which is what the
    # table prints.
    out[[keys[[i]]]] <- list(key = k, modified = md, moderator = mr, arm = arm,
                     var  = k,
                     term = if (arm == "nested") paste0("`", mr, "`:`", md, "`") else NA_character_)
  }
  out
}

# The combined factor: the observed cells, the MODIFIED variable varying fastest so the moderator
# groups, empty cells dropped. Its first level is the pair of the parents' own reference levels --
# which is why this runs after the relevel, and why `ref` needs no cross-specific grammar.
#' @keywords internal
#' @noRd
reg_cross_column <- function(md, mr) {
  lm_ <- levels(forcats::fct_drop(as.factor(md)))
  lr  <- levels(forcats::fct_drop(as.factor(mr)))
  grid <- expand.grid(md = lm_, mr = lr, stringsAsFactors = FALSE)   # `md` varies fastest
  x <- paste(as.character(md), as.character(mr), sep = reg_cross_sep())
  x[is.na(md) | is.na(mr)] <- NA_character_
  forcats::fct_drop(factor(x, levels = paste(grid$md, grid$mr, sep = reg_cross_sep())))
}

#' @keywords internal
#' @noRd
reg_cross_apply <- function(data, crosses) {
  for (r in crosses) if (identical(r$arm, "cells"))
    data[[r$var]] <- reg_cross_column(data[[r$modified]], data[[r$moderator]])
  data
}

# The model's own predictor vector: every `a*b` becomes what its arm puts in the formula's main
# effects -- the compound column, or the moderator the nested term hangs on.
#' @keywords internal
#' @noRd
reg_cross_predictors <- function(preds, crosses) {
  if (length(crosses) == 0L || length(preds) == 0L) return(preds)
  unique(unlist(lapply(preds, function(p) {
    r <- crosses[[p]]
    if (is.null(r)) p else if (identical(r$arm, "cells")) r$var else r$moderator
  }), use.names = FALSE))
}

# The model's ROW blocks, in declared order. It differs from the formula's main effects by exactly
# what each arm absorbs: `cells` shows the compound block alone, `nested` shows the moderator's own
# block (the model contains it) and then the slopes.
#' @keywords internal
#' @noRd
reg_cross_row_vars <- function(preds, crosses) {
  if (length(crosses) == 0L || length(preds) == 0L) return(preds)
  unique(unlist(lapply(preds, function(p) {
    r <- crosses[[p]]
    if (is.null(r)) p else if (identical(r$arm, "cells")) r$var else c(r$moderator, r$var)
  }), use.names = FALSE))
}

# The block names a set of keys carries.
#' @keywords internal
#' @noRd
reg_cross_vars <- function(crosses, keys) {
  k <- intersect(keys, names(crosses))
  if (length(k) == 0L) return(character(0))
  vapply(crosses[k], `[[`, character(1), "var", USE.NAMES = FALSE)
}

# The keys one model declares, in its own order -- stored on the spec, so every rebuild of the
# formula (reg_fit(), reg_formulas(), the crude fit, reg_check_plots()) reads one fact.
#' @keywords internal
#' @noRd
reg_cross_keys <- function(preds, crosses) {
  if (length(crosses) == 0L) return(character(0))
  intersect(preds, names(crosses))
}

# The RHS terms a model's crosses add, the `reg_shape_add()` of this subsystem: filtered per model,
# so a comparison never asks for a term its model lacks.
#' @keywords internal
#' @noRd
reg_cross_add <- function(crosses, keys) {
  if (length(crosses) == 0L || length(keys) == 0L) return(NULL)
  tm <- vapply(crosses[intersect(keys, names(crosses))], function(r) r$term %||% NA_character_,
               character(1))
  tm <- unname(tm[!is.na(tm)])
  if (length(tm) == 0L) NULL else tm
}

# The blocks whose crude effect has no closed form: a slope has no cells, so its observed twin is
# the univariable NESTED fit `y ~ M/X` through the same reg_fit() producer.
#' @keywords internal
#' @noRd
reg_cross_nested_vars <- function(crosses) {
  if (length(crosses) == 0L) return(character(0))
  k <- vapply(crosses, function(r) identical(r$arm, "nested"), logical(1))
  if (!any(k)) return(character(0))
  vapply(crosses[k], `[[`, character(1), "var", USE.NAMES = FALSE)
}

# Every parent of every cross: the real columns a crossed model is fitted on, so the complete-case
# frame the counts and the gap test use is the FIT's own population.
#' @keywords internal
#' @noRd
reg_cross_parents <- function(crosses) {
  if (length(crosses) == 0L) return(character(0))
  unique(unlist(lapply(crosses, function(r) c(r$modified, r$moderator)), use.names = FALSE))
}

# Term labels with every DECLARED cross expanded into its parents -- the one thing a syntactic
# nesting proof cannot see. `y ~ a + b` really does nest in `y ~ a:b`'s combined factor and in
# `y ~ b/a`, but their term labels share no name, so reg_compare_guard() would refuse the LR test
# between "additive" and "crossed" -- the comparison this spelling exists for.
# ⚠ only a DECLARED cross expands: a hand-written `a:b` in a compound formula may genuinely not
# contain its main effects, and claiming otherwise would compute a bogus likelihood ratio.
#' @keywords internal
#' @noRd
reg_cross_expand_terms <- function(tl, crosses) {
  if (length(crosses) == 0L || length(tl) == 0L) return(tl)
  bare <- gsub("`", "", tl, fixed = TRUE)
  pair <- vapply(crosses, function(r) paste(r$moderator, r$modified, sep = ":"), character(1))
  unique(unlist(lapply(bare, function(x) {
    r <- reg_cross_of(crosses, x) %||% crosses[[match(x, pair)[1]]]
    if (is.null(r)) x else c(r$modified, r$moderator)
  }), use.names = FALSE))
}

# Term labels renamed to the ROW BLOCK each belongs to: a nested cross's own term (`race:age`) is
# the block `age x race`, everything else is itself. What lets the per-predictor overall-association
# test find a crossed block -- the joint test of its slopes -- exactly as it finds a factor's.
#' @keywords internal
#' @noRd
reg_cross_term_var <- function(labels, crosses) {
  if (length(crosses) == 0L || length(labels) == 0L) return(labels)
  pair <- vapply(crosses, function(r) paste(r$moderator, r$modified, sep = ":"), character(1))
  vapply(labels, function(x) {
    i <- match(x, pair)
    if (is.na(i)) x else crosses[[i]]$var
  }, character(1), USE.NAMES = FALSE)
}

# The record a cross block's rows belong to (NULL for an ordinary predictor).
#' @keywords internal
#' @noRd
reg_cross_of <- function(crosses, var) {
  if (length(crosses) == 0L) return(NULL)
  hit <- vapply(crosses, function(r) identical(r$var, var), logical(1))
  if (!any(hit)) NULL else crosses[[which(hit)[[1]]]]
}


# === SECTION: the row axis -- the nested arm's skeleton ===========================================
#
# The `cells` arm needs nothing here: its column is an ordinary factor predictor, so reg_skeleton()'s
# own factor branch builds it.
#
# WARNING: the term is `M:X`, in that order, because terms() names an interaction's parts by their
# position in the formula and the moderator's main effect always precedes the term. Building `X:M`
# here would silently produce a block of empty rows -- reg_column() joins on this string.
#' @keywords internal
#' @noRd
reg_cross_skeleton <- function(rec, data) {
  lv <- levels(forcats::fct_drop(as.factor(data[[rec$moderator]])))
  tibble::tibble(var = rec$var, level = lv,
                 term = paste0(rec$moderator, lv, ":", rec$modified),
                 is_ref = rep(FALSE, length(lv)))
}

# A nested row's `n` is its MODERATOR level's count -- the count a continuous predictor never had.
#' @keywords internal
#' @noRd
reg_cross_count_var <- function(rec) if (identical(rec$arm, "nested")) rec$moderator else rec$var


# === SECTION: the footer -- is the interaction real? ==============================================
#
# The omnibus test of one crossed pair, as a MODEL COMPARISON with its additive counterpart. That
# framing is what covers both arms from one producer: a combined factor has no interaction TERM for
# drop1() to drop, but it still nests the additive model exactly (`y ~ a + b` is a sub-model of
# `y ~ a:b`'s saturated cells), and for the nested arm `M + X` nests in `M/X` the same way.
#
# It costs ONE extra fit, paid only when a cross was asked for: ~20 ms on a glm, which is why
# reg_footer_stats() puts it in the default set there, and ~0.5-1.1 s on polr / multinom, where it
# roughly doubles the fitting time and is opt-in through `stats = "interaction"`.
#' @keywords internal
#' @noRd
reg_cross_rows <- function(f, sp, ctx, col_var) {
  list2env(reg_ctx_locals(ctx), environment())
  if (length(sp$cross) == 0L || is.null(f) || is.null(f$fit) || isTRUE(sp$compound)) return(NULL)
  grouped <- reg_is_grouped_binomial(sp$fit_family, sp$trials, sp$compound)
  if (!("interaction" %in% reg_footer_stats(sp$fit_family, weighted, grouped, stats)))
    return(NULL)
  use_f    <- reg_fam_disp_estimated(sp$fit_family)
  # a quasi-likelihood or design fit has no honest LR, so it takes the same Wald branch the
  # between-model comparison takes.
  use_wald <- reg_fam_svy_fitted(sp$fit_family, weighted)
  inv_sp   <- reg_outcome_level_of(sp$outcome_level) %||% outcome_level

  rows <- purrr::map(sp$cross, function(k) {
    rec <- crosses[[k]]
    if (is.null(rec)) return(NULL)
    # THE ADDITIVE COUNTERPART: the same model with this one pair un-crossed. `cells` puts both
    # parents back as main effects in place of the compound column; `nested` adds the modified
    # variable's own main effect and drops the term. Every OTHER cross stays as it is.
    add_preds <- if (identical(rec$arm, "cells"))
      unique(c(setdiff(sp$predictors, rec$var), rec$modified, rec$moderator))
    else unique(c(sp$predictors, rec$modified))
    a <- tryCatch(suppressMessages(reg_fit(
      data, sp$outcome, add_preds, sp$fit_family, design_spec, isTRUE(sp$est$exp),
      inv_sp, conf_level, "wald", trials = sp$trials, formula = NULL, multiplier = NULL,
      drop_extra = unique(c(na_shared_vars, reg_cross_parents(crosses))),
      add_terms = c(reg_shape_add(shape_terms, add_preds),
                    reg_cross_add(crosses, setdiff(sp$cross, k))))),
      error = function(e) NULL)
    if (is.null(a) || is.null(a$fit)) return(NULL)
    # ⚠ NOT reg_compare_guard(), which proves nesting from the TERM LABELS: a combined factor's
    # label contains neither parent's. Here nesting holds BY CONSTRUCTION, so what is left to check
    # is the same rows and more parameters.
    df_a <- tryCatch(stats::df.residual(a$fit), error = function(e) NA_real_)
    df_f <- tryCatch(stats::df.residual(f$fit), error = function(e) NA_real_)
    if (!isTRUE(a$nobs == f$nobs) || !isTRUE(df_a > df_f)) return(NULL)
    e <- if (use_wald) tryCatch({
      an <- stats::anova(a$fit, f$fit, method = "Wald", test = "F")
      list(stat = as.numeric(an$Ftest), df1 = as.numeric(an$df), df2 = as.numeric(an$ddf),
           p = as.numeric(an$p))
    }, error = function(e) NULL)
    else {
      an <- tryCatch(stats::anova(a$fit, f$fit, test = if (use_f) "F" else "Chisq"),
                     error = function(e) NULL)
      if (is.null(an)) NULL else reg_compare_extract(an, use_f)
    }
    if (is.null(e) || is.na(e$p)) return(NULL)
    key <- test_row_key("interaction", if (use_wald) "wald" else if (use_f) "f" else "lr")
    reg_test_row(key, col_var, statistic = e$stat, df1 = e$df1, df2 = e$df2, pvalue = e$p,
                 nobs = as.numeric(f$nobs), outcome = sp$outcome, var = rec$var)
  })
  rows <- purrr::compact(rows)
  if (length(rows) == 0L) NULL else dplyr::bind_rows(rows)
}
