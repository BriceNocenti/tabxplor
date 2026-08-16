# =====================================================================================================
# R/zzz-fact-keys.R -- REFERENTIAL INTEGRITY BETWEEN THE DECLARED FACT TABLES (Phase 20a, KEY 2)
# =====================================================================================================
# Phase 19 replaced ~15 vocabularies-written-in-their-consumers with ~15 declared fact tables. That
# closed one class of drift and opened another: **a key written by hand in one table and read by name
# in another is a foreign key**, and nothing checked them.
#
#   19d renamed the colour measures to full words and did not reach `EST_SCALES$label_meas`. The
#   forest plot lost its glyphs and ERRORED on lookup. The fix shipped with a comment reading
#   "WARNING: a MEASURES KEY -- 19d's full-word rename had to reach here, and did not", which is
#   hard rule 4's forbidden pattern ("two encodings kept in sync by a comment") one level up.
#
# THE RULE: a dangling key is a **LOAD** failure, not a runtime one. Adding an edge is one row.
#
# WHY THIS FILE SORTS LAST. There is no `Collate:` field, so R sources R/ in C collation, and the
# tables are spread across seven files: `COLOR_SCALES` is in tab_classes.R, `REG_EMPIRICAL` in
# tab_reg.R, `DISPLAY_TOKENS` in tab-display.R, `CI_GEOMS` in tab-agg.R, `MEASURES`/`EST_SCALES` in
# fmt_class.R. `reg-estimand.R` -- 19o's proposed home -- sees none of the first three. `zzz-` is the
# only prefix that is last **by construction**, whatever the other files are called.
#
# WHAT IS HERE, AND WHAT IS NOT.
#   here     every edge from one declared table's VALUES to another's KEYS, including a table's
#            references to itself (COLOR_SCALES$derive, DISPLAY_TOKENS$alias).
#   not here a table's self-consistency ("does it cover its own key set", "does it agree with the
#            two switch bodies") -- those stay beside the table, where they already are and where
#            their operands are in scope:
#              fmt_class.R   FMT_FIELD_DOC vs fmt_field_names; fmt_attr_rules vs fmt_col_attrs;
#                            MEASURES' required members; COLOR_BUILD_ORDER <-> MEASURES$builds
#              tab-agg.R     CI_GEOMS' member list and key reconstruction
#              tab-display.R DISPLAY_TOKENS vs the get_num()/set_num() switch bodies
#              reg-estimand.R  per-family defaults, duplicate (effect, measure), REG_FAMILY_MULT_WORD
#
# ⚠ READ EVERY TABLE WITH `[[`, NEVER `$`. `MEASURES$adjustment` has `scale_from` and no `scale`, so
# `$scale` PARTIAL-MATCHES to `"gap"` and a generic checker would silently validate the wrong string.
# `[[` on a list is exact by default; the two readers below are the only way rows are read here.
# =====================================================================================================


# --- reading a table's rows ---------------------------------------------------------------------

# ONE value per row (the row's own default when the member is absent).
#' @keywords internal
#' @noRd
tx_fk_scalar <- function(tbl, field) {
  vapply(tbl, function(r) {
    v <- r[[field]]
    if (is.null(v) || !length(v)) NA_character_ else as.character(v)[[1]]
  }, character(1), USE.NAMES = FALSE)
}

# EVERY value of a member, over every row (a member may be a vector: MEASURES$applies_to, a 3-slot
# `scale`, REG_CHECKS$families).
#' @keywords internal
#' @noRd
tx_fk_all <- function(tbl, field) {
  unlist(lapply(tbl, function(r) {
    v <- r[[field]]
    if (is.null(v)) character(0) else as.character(v)
  }), use.names = FALSE)
}

# REG_ESTIMANDS is a list BY FAMILY of `list(default =, rows =)`, so its rows are two levels down.
#' @keywords internal
#' @noRd
tx_fk_reg_rows <- function() unlist(lapply(REG_ESTIMANDS, `[[`, "rows"), recursive = FALSE)

# REG_EMPIRICAL mixes per-family SCALARS (method_diff, coef, coef_log) with SHAPE rows; only the
# latter are rows with members.
#' @keywords internal
#' @noRd
tx_fk_emp_shapes <- function() {
  out <- unlist(lapply(REG_EMPIRICAL, function(fam) fam[vapply(fam, is.list, logical(1))]),
                recursive = FALSE, use.names = FALSE)
  out
}
#' @keywords internal
#' @noRd
tx_fk_emp_shape_names <- function()
  unique(unlist(lapply(REG_EMPIRICAL, function(fam) names(fam)[vapply(fam, is.list, logical(1))])))


# --- the declared edges --------------------------------------------------------------------------
# Each row: `from` (what the message names) · `get` (the values, as a closure) · `to` (the legal key
# set, as a closure) · `allow` (values that are legal but are NOT keys of the target -- each one is a
# stated fact, never a way to silence a real dangling key) · `orphan` (also report target keys that
# NO edge references: "this row is dead weight", reported by tx_check_foreign_keys() rather than
# aborting, because an unreferenced row is not a wrong number).
#' @keywords internal
#' @noRd
tx_fk <- function(from, get, to, allow = character(0), orphan = FALSE)
  list(from = from, get = get, to = to, allow = allow, orphan = orphan)

#' @keywords internal
#' @noRd
TAB_FOREIGN_KEYS <- list(

  # --- into MEASURES (the colour measures) ---------------------------------------------------
  # THE edge 19d broke: the forest plot takes a scale's break glyphs from a MEASURES row.
  tx_fk("EST_SCALES$label_meas",   function() tx_fk_scalar(EST_SCALES, "label_meas"),
        function() names(MEASURES)),
  tx_fk("DISPLAY_TOKENS$comparison", function() tx_fk_scalar(DISPLAY_TOKENS, "comparison"),
        function() names(MEASURES)),
  tx_fk("COLOR_ALIASES$measure",   function() tx_fk_scalar(COLOR_ALIASES, "measure"),
        function() names(MEASURES)),
  # a crude column's colour is written in the user's spelling, so an ALIAS is legal there too.
  tx_fk("REG_EMPIRICAL$*$color",   function() tx_fk_scalar(tx_fk_emp_shapes(), "color"),
        function() c(names(MEASURES), names(COLOR_ALIASES)), allow = ""),

  # --- into COLOR_SCALES (the break ladders) -------------------------------------------------
  tx_fk("EST_SCALES$break_key",    function() tx_fk_scalar(EST_SCALES, "break_key"),
        function() names(COLOR_SCALES), orphan = TRUE),
  tx_fk("EST_SCALES$gap_key",      function() tx_fk_scalar(EST_SCALES, "gap_key"),
        function() names(COLOR_SCALES), orphan = TRUE),
  tx_fk("MEASURES$scale",          function() tx_fk_all(MEASURES, "scale"),
        function() names(COLOR_SCALES), orphan = TRUE),
  tx_fk("MEASURES$guar$scale",
        function() unlist(lapply(MEASURES, function(m) as.character(m[["guar"]][["scale"]]))),
        function() names(COLOR_SCALES), orphan = TRUE),
  # here the NAMES are the key: `by_scale` is "this measure's per-SCALE override", keyed by ladder.
  tx_fk("names(MEASURES$by_scale)",
        function() unlist(lapply(MEASURES, function(m) names(m[["by_scale"]]))),
        function() names(COLOR_SCALES), orphan = TRUE),
  # a DERIVED scale names its parent instead of owning a switch arm in fmt_color_plan().
  tx_fk("COLOR_SCALES$derive$from",
        function() unlist(lapply(COLOR_SCALES, function(s) as.character(s[["derive"]][["from"]]))),
        function() names(COLOR_SCALES)),

  # --- into EST_SCALES (what a column ESTIMATES) ---------------------------------------------
  tx_fk("CI_GEOMS$scale_key",      function() tx_fk_scalar(CI_GEOMS, "scale_key"),
        function() names(EST_SCALES)),
  # moved here from reg-estimand.R (Phase 20a): its target lives in fmt_class.R, so this is a
  # cross-table edge like any other, and keeping it apart is what made the inventory incomplete.
  tx_fk("REG_ESTIMANDS$rows$scale", function() tx_fk_scalar(tx_fk_reg_rows(), "scale"),
        function() names(EST_SCALES)),
  tx_fk("REG_EMPIRICAL$*$scale",   function() tx_fk_scalar(tx_fk_emp_shapes(), "scale"),
        function() names(EST_SCALES)),
  # the LADDER a scale reads on a measure: MEASURES$scale is a 3-slot c(pct=, std=, log=) vector and
  # fmt_color_plan() indexes it by this string.
  tx_fk("EST_SCALES$ladder",       function() tx_fk_scalar(EST_SCALES, "ladder"),
        function() unique(unlist(lapply(MEASURES, function(m) names(m[["scale"]]))))),

  # --- into the interval vocabulary ----------------------------------------------------------
  tx_fk("CI_GEOMS$method_slot",    function() tx_fk_scalar(CI_GEOMS, "method_slot"),
        function() names(CI_METHODS)),
  # ⚠ "katz" is deliberately outside CI_METHODS: a proportion RATIO has one interval, so it is not a
  # choice a user makes, and declaring it as one would put an empty menu in `ci_method`.
  tx_fk("CI_GEOMS$method_fixed",   function() tx_fk_scalar(CI_GEOMS, "method_fixed"),
        function() unlist(CI_METHODS, use.names = FALSE), allow = "katz"),
  # ⚠ the crude columns add three engines of their own, for the same reason: the Woolf log-OR, the
  # Katz log-RR and the log-scale Wald are the ONLY interval of their geometry, so none of them is a
  # `ci_method` a user picks. All three have a CI_METHOD_LABELS / CI_METHOD_WORDED row, which is
  # where the legend names them.
  tx_fk("REG_EMPIRICAL$*$ci_method", function() tx_fk_scalar(tx_fk_emp_shapes(), "ci_method"),
        function() unlist(CI_METHODS, use.names = FALSE), allow = c("woolf", "katz", "wald_log")),

  # --- into DISPLAY_TOKENS (what a cell shows) -----------------------------------------------
  tx_fk("EST_SCALES$default_display", function() tx_fk_scalar(EST_SCALES, "default_display"),
        function() names(DISPLAY_TOKENS)),
  tx_fk("REG_ESTIMANDS$rows$display", function() tx_fk_scalar(tx_fk_reg_rows(), "display"),
        function() names(DISPLAY_TOKENS)),
  tx_fk("REG_EMPIRICAL$*$display", function() tx_fk_scalar(tx_fk_emp_shapes(), "display"),
        function() names(DISPLAY_TOKENS)),
  # `rr` -> `ratio`: a token may name another token as its spelling.
  tx_fk("DISPLAY_TOKENS$alias",    function() tx_fk_scalar(DISPLAY_TOKENS, "alias"),
        function() names(DISPLAY_TOKENS)),
  # the reg display shorthands resolve to a TOKEN or to a `{}` TEMPLATE; only the first kind is a
  # key here (a template is validated field by field by display_write_col()).
  tx_fk("REG_DISPLAY_SHORTHANDS",
        function() { v <- as.character(REG_DISPLAY_SHORTHANDS); v[!grepl("{", v, fixed = TRUE)] },
        function() names(DISPLAY_TOKENS), allow = "value"),

  # --- into fmt_field_names (the record) -----------------------------------------------------
  # ⚠ `ci` is DERIVED (get_ci() is a shim over the ci_inf/ci_sup bounds), so the `ci` token names a
  # quantity, not a field. It is the one legal non-field, and it is stated here rather than by
  # widening fmt_field_names, which would claim a 22nd field that new_fmt() does not have.
  tx_fk("DISPLAY_TOKENS$field",    function() tx_fk_scalar(DISPLAY_TOKENS, "field"),
        function() fmt_field_names, allow = "ci"),
  tx_fk("EST_SCALES$est_field",    function() tx_fk_scalar(EST_SCALES, "est_field"),
        function() fmt_field_names),

  # --- into the small declared enums ---------------------------------------------------------
  tx_fk("REG_EMPIRICAL$*$pct_base", function() tx_fk_scalar(tx_fk_emp_shapes(), "pct_base"),
        function() PCT_BASES),
  tx_fk("COLOR_ALIASES$policy",    function() tx_fk_scalar(COLOR_ALIASES, "policy"),
        function() COLOR_SIGNIF_VALUES),
  tx_fk("MEASURES$applies_to",     function() tx_fk_all(MEASURES, "applies_to"),
        function() COLOR_COL_KINDS),
  tx_fk("REG_CHECKS$kind",         function() tx_fk_scalar(REG_CHECKS, "kind"),
        function() ROW_KINDS),

  # --- into the regression family vocabulary -------------------------------------------------
  tx_fk("names(REG_ESTIMANDS)",    function() names(REG_ESTIMANDS),
        function() names(REG_FAMILIES)),
  tx_fk("REG_ESTIMANDS$rows$fit",  function() tx_fk_scalar(tx_fk_reg_rows(), "fit"),
        function() names(REG_FAMILIES)),
  # "every estimand's fit has model checks" -- the invariant 19l added reactively, after discovering
  # that 19e's two new estimands silently had none. Stated on the ROWS now, which is stronger than
  # the REG_FIT_FAMILY subset it replaces (that one only covered the three link-key families).
  tx_fk("REG_ESTIMANDS$rows$fit -> checks", function() tx_fk_scalar(tx_fk_reg_rows(), "fit"),
        function() REG_CHECK_FAMILIES),
  tx_fk("REG_CHECKS$families",     function() tx_fk_all(REG_CHECKS, "families"),
        function() REG_CHECK_FAMILIES),
  tx_fk("REG_OUTCOME_KINDS$detect", function() tx_fk_all(REG_OUTCOME_KINDS, "detect"),
        function() names(REG_FAMILIES)),
  tx_fk("REG_OUTCOME_KINDS$offers", function() tx_fk_all(REG_OUTCOME_KINDS, "offers"),
        function() names(REG_FAMILIES)),

  # --- into REG_EMPIRICAL (which crude column pairs with which estimand) ----------------------
  # ⚠ "auto" is the sentinel for "the outcome's own family", resolved at run time.
  tx_fk("REG_ESTIMANDS$rows$crude_fam", function() tx_fk_scalar(tx_fk_reg_rows(), "crude_fam"),
        function() names(REG_EMPIRICAL), allow = "auto"),
  # the (crude_fam, crude_shape) PAIR is resolved at run time (an "auto" family, plus the documented
  # cross-family borrow: a binomial marginal ratio takes REG_EMPIRICAL$rr$rr), so what is checkable
  # here is that the shape name exists at all -- which is what a typo breaks.
  tx_fk("REG_ESTIMANDS$rows$crude_shape", function() tx_fk_scalar(tx_fk_reg_rows(), "crude_shape"),
        function() tx_fk_emp_shape_names()),

  # --- into the estimand vocabulary ----------------------------------------------------------
  # the VALUE is the measure a `log_*` spelling pins; the NAME is the spelling itself, so it must be
  # one the alias table can resolve.
  tx_fk("REG_LOG_BASE",            function() as.character(REG_LOG_BASE),
        function() REG_MEASURES_VALUES),
  tx_fk("names(REG_LOG_BASE)",     function() names(REG_LOG_BASE),
        function() names(REG_MEASURE_ALIASES)),
  tx_fk("REG_MEASURE_ALIASES",     function() as.character(REG_MEASURE_ALIASES),
        function() REG_MEASURES_VALUES)
)


# --- the checker ----------------------------------------------------------------------------------

# tx_check_foreign_keys() -- run at LOAD (below). `stop()`, not cli::cli_abort(): this executes while
# the namespace is still being built, so it must not depend on anything of ours.
#
# @param keys  the edges to check. A parameter ONLY so the test suite can hand it a deliberately
#   broken edge and see the failure -- every real caller uses the default.
# @return invisibly, the ORPHAN report: target keys that no edge references. Not a failure (an
#   unreferenced colour scale is dead weight, not a wrong number), so it is returned rather than
#   thrown, and asserted by test-fact-keys.R.
#' @keywords internal
#' @noRd
tx_check_foreign_keys <- function(keys = TAB_FOREIGN_KEYS) {
  seen <- list()                                   # target signature -> values referencing it
  for (k in keys) {
    ok  <- unique(c(k$to(), k$allow))
    got <- k$get()
    got <- unique(got[!is.na(got) & nzchar(got)])
    bad <- setdiff(got, ok)
    if (length(bad))
      stop("tabxplor: dangling key in ", k$from, ": ",
           paste(sQuote(bad), collapse = ", "),
           "\n  legal: ", paste(ok, collapse = ", "), call. = FALSE)
    if (isTRUE(k$orphan)) {
      sig <- paste(sort(k$to()), collapse = "\r")
      seen[[sig]] <- unique(c(seen[[sig]], got))
    }
  }
  orphans <- list()
  for (k in keys) {
    if (!isTRUE(k$orphan)) next
    tgt <- k$to()
    sig <- paste(sort(tgt), collapse = "\r")
    left <- setdiff(tgt, seen[[sig]])
    if (length(left)) orphans[[k$from]] <- left
  }
  invisible(orphans)
}

# THE load-time check. It runs at R CMD INSTALL / pkgload::load_all(), so a rename that does not
# reach a fact table breaks the BUILD, at the moment it is made -- which is the whole point.
tx_check_foreign_keys()
