# verify_golden_field_delta.R -- prove that adding a per-cell fmt FIELD, or a per-column fmt ATTRIBUTE,
# changed the goldens by exactly that member and nothing else.
#
# WHY THIS FILE EXISTS. Four phases have added a member (n_eff, obs, gap_se, conf_level). Each time the 36
# structural `_golden/*.rds` had to be regenerated, each time a throwaway script proved "the only delta
# is an all-NA column", and each time the script was deleted -- so the next phase wrote it again from
# the prose in dev/make_golden.R. It is committed now.
#
# HOW TO USE IT, around a field addition:
#   1. BEFORE regenerating, run it once: it reads the committed goldens and rebuilds the same cases
#      from the current source, then reports, per case and per fmt column, (a) which fields are new,
#      (b) whether every SHARED field is bit-identical, (c) whether each new field is entirely NA, and
#      (d) whether the column ATTRIBUTES are unchanged apart from the DECLARED new ones, whose value it
#      prints and checks against the expectation below.
#   2. Only if every case says OK, run dev/make_golden.R and record the delta in its ledger.
#
# DECLARE THE ADDITION HERE before running. A field is expected to be all-NA (nothing computes it yet
# on these cases); an attribute has a definite expected VALUE -- stating it is the point, since
# "unchanged" is exactly what an attribute addition is not.
# A single "CHANGED" line means the new field is not the only difference -- investigate before
# regenerating, because the goldens are the byte-identity contract.
#
# Rendering is NOT checked here (a non-displayed field must not move `_snaps/golden.md` either);
# testthat reports that by itself when the suite runs.

devtools::load_all("~/github/tabxplor", quiet = TRUE)
source("tests/testthat/helper-golden.R")

# ⚠ RESET THESE DECLARATIONS AT THE START OF EVERY PHASE. They describe THIS phase's intended
# delta, not the history -- a declaration left behind from the previous phase reports its own
# already-landed change as a PROBLEM (measured in Phase 19a: z16-iiiii's `ci_settings` reshape rule
# fired on four cases whose committed goldens already carry the new shape).
#
# Phase 19b (KEY 2): `type` and `ci_type` are REPLACED by `scale` + `pct_base`. An EXPECTED_ATTR entry
# may be a PREDICATE function(old_attrs, new_value) -> TRUE, which is what makes this a proof rather
# than an assertion: `scale` is checked, on every one of the ~190 golden columns, against what the
# DELETED dispatch derived from that column's own old (type, ci_type) -- so "the stored scale is
# exactly what the code used to compute" is verified per column, not claimed.
ADDED_ATTRS   <- c("scale", "pct_base", "ci_method")
REMOVED_ATTRS <- c("type", "ci_type")

# The pre-19b dispatch, replicated here ONCE so the delta can be proved against it. (It is the body
# of est_scale_key() at HEAD~, minus the `kind`/`display` overrides, which no golden case reaches:
# every golden column either stores an interval or is a plain level column. The `or` row was renamed
# `odds_ratio` in the same phase, so the replica returns the NEW key.)
legacy_scale_key <- function(ci_type, type, has_var = FALSE) {
  cit <- as.character(ci_type)[1]; typ <- as.character(type)[1]
  if (identical(cit, "or"))    return("odds_ratio")
  if (identical(cit, "ratio")) return(if (identical(typ, "mean")) "mean_ratio" else "pct_ratio")
  if (identical(typ, "coef") && isTRUE(has_var)) return("raw_diff")
  if (cit %in% c("diff", "diff_row", "diff_col"))
    return(if (identical(typ, "mean")) "mean_diff" else "points")
  if (identical(typ, "mean")) return("level_mean")
  if (identical(typ, "n"))    return("level_n")     # 19b gave the count column its own row
  "level_pct"
}
EXPECTED_ATTR <- list(
  scale = function(ao, v, col) {
    # an OR table stamps `odds_ratio` on EVERY column of the col_var, its reference one included --
    # D19, the one deliberate divergence from the old per-column ci_type. Recognise it by the display.
    d <- unique(as.character(vctrs::field(col, "display")))
    if (any(d %in% c("or", "OR", "or_pct", "OR_pct"))) return(identical(v, "odds_ratio"))
    identical(v, legacy_scale_key(ao$ci_type, ao$type, !all(is.na(vctrs::field(col, "var")))))
  },
  pct_base = function(ao, v, col)
    identical(v, if (ao$type %in% c("row", "col", "all", "all_tabs")) ao$type else "none"),
  # `ci_method` (the second half of the phase) has no pre-19b counterpart to compare against -- it
  # was a table-wide meta$ci_settings vector the legend indexed BY MEASURE. What CAN be proved per
  # column is the invariant that replaced it: a column names a method exactly when it carries a
  # contrast/cell interval, i.e. when its old `ci_type` was not "" -- and never otherwise.
  # (`ci_type` could also literally hold "no" -- num_core recorded its `ci` ARGUMENT rather than the
  # fact, which is one more instance of the disease this key cures.)
  ci_method = function(ao, v, col)
    identical(nzchar(v), !as.character(ao$ci_type)[1] %in% c("", "no"))
)

ADDED_TEST_COLS <- character(0)

# Phase 18z16-i: the same pass also adds a `meta` SUB-FIELD (`inference` = the stored inference
# basis). Unlike a field or a test column it has a definite VALUE, and it is stored only on the
# WEIGHTED cases -- an unweighted golden must keep exactly the metadata it had. Declare it here and
# the check prints what it actually found, per case.
# Phase 18z16-iiiii REMOVES one instead: `inference` left `meta` for the two column attributes
# above. No golden case is weighted, so none of them stored it -- declare nothing.
ADDED_META_FIELDS   <- character(0)
REMOVED_META_FIELDS <- c("ci_settings")

# A phase can also RESHAPE a `meta` sub-field without changing what it says. Phase 18z16-iiiii folds
# `ci_settings`' five `method_*` scalars into ONE named vector, and drops `method_ratio` (a one-value
# argument). Declare the field with a predicate proving old and new carry the SAME information; the
# script then treats that sub-field as accounted for and still demands bit-identity of everything else.
# Phase 19a: nothing is reshaped. (The z16-iiiii `ci_settings` rule that used to sit here was left
# behind after its goldens were regenerated, so it then compared two copies of the NEW shape and
# reported four false PROBLEMS -- hence the reset warning at the top.)
RESHAPED_META_FIELDS <- list()

cases   <- golden_cases()
gdir    <- "tests/testthat/_golden"
n_cells <- 0L
issues  <- character(0)
seen_attrs <- list()

for (nm in names(cases)) {
  f <- file.path(gdir, paste0(nm, ".rds"))
  if (!file.exists(f)) { cat("SKIP  ", nm, "(no committed golden)\n"); next }
  old <- readRDS(f)
  new <- tryCatch(cases[[nm]](), error = function(e) {
    issues <<- c(issues, paste0(nm, ": rebuild failed -- ", conditionMessage(e))); NULL
  })
  if (is.null(new)) next

  if (!identical(dim(old), dim(new)) || !identical(names(old), names(new))) {
    issues <- c(issues, paste0(nm, ": SHAPE CHANGED (dim or names)")); next
  }
  added <- character(0)
  for (col in names(old)) {
    if (!is_fmt(old[[col]])) {
      if (!identical(old[[col]], new[[col]]))
        issues <- c(issues, paste0(nm, " / ", col, ": non-fmt column CHANGED"))
      next
    }
    do <- as.list(vctrs::vec_data(old[[col]]))
    dn <- as.list(vctrs::vec_data(new[[col]]))
    added <- union(added, setdiff(names(dn), names(do)))
    if (length(setdiff(names(do), names(dn))))
      issues <- c(issues, paste0(nm, " / ", col, ": field(s) REMOVED: ",
                                 paste(setdiff(names(do), names(dn)), collapse = ", ")))
    for (fd in intersect(names(do), names(dn))) {
      if (!identical(do[[fd]], dn[[fd]]))
        issues <- c(issues, paste0(nm, " / ", col, " / ", fd, ": field CHANGED"))
    }
    for (fd in setdiff(names(dn), names(do))) {
      if (!all(is.na(dn[[fd]]) | identical(dn[[fd]], FALSE)))
        issues <- c(issues, paste0(nm, " / ", col, " / ", fd, ": new field is NOT all-NA"))
    }
    ao <- attributes(old[[col]]); an <- attributes(new[[col]])
    ao$names <- an$names <- NULL                       # the field-name vector is the added field
    new_at <- setdiff(names(an), names(ao))
    if (!setequal(new_at, intersect(ADDED_ATTRS, new_at)))
      issues <- c(issues, paste0(nm, " / ", col, ": UNDECLARED new attribute(s): ",
                                 paste(setdiff(new_at, ADDED_ATTRS), collapse = ", ")))
    gone <- setdiff(names(ao), names(an))
    if (!setequal(gone, intersect(REMOVED_ATTRS, gone)))
      issues <- c(issues, paste0(nm, " / ", col, ": UNDECLARED removed attribute(s): ",
                                 paste(setdiff(gone, REMOVED_ATTRS), collapse = ", ")))
    for (a in setdiff(intersect(names(ao), names(an)), REMOVED_ATTRS))
      if (!identical(ao[[a]], an[[a]]))
        issues <- c(issues, paste0(nm, " / ", col, " / ", a, ": attribute CHANGED"))
    for (a in new_at) {
      seen_attrs[[a]] <- unique(c(seen_attrs[[a]], an[[a]]))
      exp <- EXPECTED_ATTR[[a]]
      ok  <- if (is.null(exp)) TRUE
             else if (is.function(exp)) isTRUE(exp(ao, an[[a]], new[[col]]))
             else isTRUE(all.equal(an[[a]], exp))
      if (!ok)
        issues <- c(issues, paste0(nm, " / ", col, " / ", a, ": new attribute is ",
                                   paste(an[[a]], collapse = ", "),
                                   " -- NOT what the old (type = ", ao$type, ", ci_type = ",
                                   ao$ci_type, ") derived"))
    }
    n_cells <- n_cells + length(old[[col]])
  }
  # table-level attributes (subtext / test / meta) must be untouched by a field pass -- EXCEPT for the
  # `test` tibble's declared new COLUMNS (ADDED_TEST_COLS), which are checked the same way a new field
  # is: present, empty, and everything else bit-identical.
  ta <- function(t) attributes(t)[intersect(names(attributes(t)), c("subtext", "test", "meta"))]
  ao <- ta(old); an <- ta(new)
  if (length(ADDED_TEST_COLS) && !is.null(an$test)) {
    add <- setdiff(names(an$test), names(ao$test))
    if (!setequal(add, intersect(ADDED_TEST_COLS, add)))
      issues <- c(issues, paste0(nm, ": UNDECLARED new `test` column(s): ",
                                 paste(setdiff(add, ADDED_TEST_COLS), collapse = ", ")))
    for (cl in add) {
      v <- an$test[[cl]]
      if (!all(is.na(v) | (is.character(v) & !nzchar(v))))
        issues <- c(issues, paste0(nm, " / test / ", cl, ": new column is NOT empty"))
    }
    an$test <- an$test[, setdiff(names(an$test), add), drop = FALSE]
  }
  # A phase can also REMOVE a `meta` sub-field -- z16-iiiii moved `inference` out to two per-column
  # attributes. Symmetric to the addition check: declare it, and the script proves that the ONLY
  # table-attribute delta is its disappearance (and prints what was dropped, per case).
  if (length(REMOVED_META_FIELDS)) {
    remm <- setdiff(names(ao$meta), names(an$meta))
    if (!setequal(remm, intersect(REMOVED_META_FIELDS, remm)))
      issues <- c(issues, paste0(nm, ": UNDECLARED removed `meta` field(s): ",
                                 paste(setdiff(remm, REMOVED_META_FIELDS), collapse = ", ")))
    for (fl in remm)
      cat(sprintf("      %-24s dropped meta$%s = %s\n", nm, fl,
                  paste(names(ao$meta[[fl]]), unlist(ao$meta[[fl]]), sep = "=", collapse = ", ")))
    ao$meta <- ao$meta[setdiff(names(ao$meta), remm)]
    if (!length(ao$meta)) ao$meta <- NULL
    if (is.null(an$meta)) ao <- ao[names(ao) != "meta"]
  }
  for (fl in names(RESHAPED_META_FIELDS)) {
    if (is.null(ao$meta[[fl]]) && is.null(an$meta[[fl]])) next
    ok <- isTRUE(tryCatch(RESHAPED_META_FIELDS[[fl]](ao$meta[[fl]], an$meta[[fl]]),
                          error = function(e) FALSE))
    if (!ok) issues <- c(issues, paste0(nm, ": meta$", fl, " RESHAPE lost information"))
    else cat(sprintf("      %-24s meta$%s reshaped, same information\n", nm, fl))
    ao$meta[[fl]] <- NULL; an$meta[[fl]] <- NULL
  }
  if (length(ADDED_META_FIELDS)) {
    addm <- setdiff(names(an$meta), names(ao$meta))
    if (!setequal(addm, intersect(ADDED_META_FIELDS, addm)))
      issues <- c(issues, paste0(nm, ": UNDECLARED new `meta` field(s): ",
                                 paste(setdiff(addm, ADDED_META_FIELDS), collapse = ", ")))
    for (fl in addm)
      cat(sprintf("      %-24s new meta$%s = %s\n", nm, fl,
                  paste(names(an$meta[[fl]]), unlist(an$meta[[fl]]), sep = "=", collapse = ", ")))
    an$meta <- an$meta[setdiff(names(an$meta), addm)]
    if (!length(an$meta)) an$meta <- NULL
    if (is.null(ao$meta)) an <- an[names(an) != "meta"]
  }
  if (!identical(ao, an))
    issues <- c(issues, paste0(nm, ": TABLE attributes changed"))

  cat(sprintf("OK    %-28s %s\n", nm,
              if (length(added)) paste0("new field(s): ", paste(added, collapse = ", "))
              else "(no new field)"))
}

cat("\n", n_cells, " cells checked across ", length(cases), " cases.\n", sep = "")
for (a in names(seen_attrs))
  cat("new attribute ", a, ": observed value(s) ", paste(seen_attrs[[a]], collapse = ", "), "\n", sep = "")
if (length(issues)) {
  cat("\nPROBLEMS -- do NOT regenerate:\n"); cat(paste0("  ", issues, collapse = "\n"), "\n")
} else {
  cat("Only the declared addition differs. Safe to run dev/make_golden.R.\n")
}
