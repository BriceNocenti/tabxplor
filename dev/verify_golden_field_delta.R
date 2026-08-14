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
# Phase 19d-tail (the green-light pass): the intended delta is EMPTY. It unifies the `ci` anchor rule,
# gives the leaf the dichotomised level geometry its odds ratio needs, and repairs the jamovi tier-3
# tuple -- none of which may move a per-cell field, a column attribute, a `test` column or a `meta`
# sub-field on any golden. A single CHANGED line is a regression, not a declaration.
ADDED_ATTRS   <- character(0)
REMOVED_ATTRS <- character(0)
EXPECTED_ATTR <- list()

# Phase 19f (KEY 1) needs TWO modes this script did not have, both declared here.
#
# RENAMED_FIELDS -- a field REPLACED by another that says the same thing plus more. `in_totrow`
# (logical) became `row_kind` (7 values), so "unchanged" is not bit-identity but a stated MAPPING:
# the new field must equal map(old) on every cell of every golden. That is the phase's central claim
# about the record, proved rather than asserted.
# Phase 19g: nothing renamed in the RECORD (19f's in_totrow -> row_kind is landed and its goldens
# are regenerated; leaving the rule here would compare two copies of the new field -- the reset
# hazard named at the top).
RENAMED_FIELDS <- list()

# RENAMED_TEST_COLS -- a `test` tibble column REPLACED by another saying the same thing. Phase 19g
# merges `row_var` (which variable, on a crosstab row) and `term` (which predictor, on a reg row)
# into ONE `var`, and renames `col_var` -> `col`. Each entry states the mapping; the script then
# demands bit-identity of every other column.
# Phase 19d-tail: nothing renamed (19g's re-key is landed and its goldens are regenerated; leaving
# the rules here would compare two copies of the new schema -- the reset hazard named at the top).
RENAMED_TEST_COLS <- list()
# DECLARED_INDEX_COLS -- the non-fmt label columns that GAIN the tabxplor_lvl class in this phase.
# Their VALUES must be identical (a declaration is not data); only class/role/var/ordered may appear.
# Phase 19g: FALSE -- 19f's declaration is landed and its goldens are regenerated, so leaving this on
# would compare a declared column against a stripped copy of itself (the reset hazard at the top).
DECLARE_INDEX_COLS <- FALSE

ADDED_TEST_COLS <- character(0)

# Phase 18z16-i: the same pass also adds a `meta` SUB-FIELD (`inference` = the stored inference
# basis). Unlike a field or a test column it has a definite VALUE, and it is stored only on the
# WEIGHTED cases -- an unweighted golden must keep exactly the metadata it had. Declare it here and
# the check prints what it actually found, per case.
# Phase 18z16-iiiii REMOVES one instead: `inference` left `meta` for the two column attributes
# above. No golden case is weighted, so none of them stored it -- declare nothing.
# Phase 19c: nothing. (`ci_settings` was 19b's removal and its goldens are regenerated, so leaving
# that rule here would compare two copies of the new shape -- the reset hazard named at the top.)
ADDED_META_FIELDS   <- character(0)
REMOVED_META_FIELDS <- character(0)

# A phase can also RESHAPE a `meta` sub-field without changing what it says. Phase 18z16-iiiii folds
# `ci_settings`' five `method_*` scalars into ONE named vector, and drops `method_ratio` (a one-value
# argument). Declare the field with a predicate proving old and new carry the SAME information; the
# script then treats that sub-field as accounted for and still demands bit-identity of everything else.
# Phase 19a: nothing is reshaped. (The z16-iiiii `ci_settings` rule that used to sit here was left
# behind after its goldens were regenerated, so it then compared two copies of the NEW shape and
# reported four false PROBLEMS -- hence the reset warning at the top.)
# META_RESHAPE_WHOLE -- a whole-`meta` predicate, for a phase that re-shapes the container itself
# rather than one sub-field. NULL = "no reshape this phase", and then plain bit-identity is demanded.
# Phase 19d-tail: NULL. 19g's rule (vars moved verbatim under spec, a `kind` stated) is landed and its
# goldens are regenerated, so it was comparing the new shape against a stripped copy of itself -- and
# it PASSED on 35 cases only because `old$vars %||% list()` and an empty `spec$vars` are both
# `list()`. The single weighted case, whose `vars` holds `wt`, reported a PROBLEM that was purely the
# stale declaration. Third time this hazard has fired; see the reset warning at the top.
META_RESHAPE_WHOLE <- NULL

RESHAPED_META_FIELDS <- list(
  # Phase 19f: `meta$vars` loses the whole variable MODEL -- row_vars / col_vars / tab_vars /
  # compacted are DERIVED now (from the declared index columns above and from the fmt columns' own
  # `col_var`), and row_roles is the `row_kind` field. What may remain is only what no column can
  # carry. The derivation itself is proved by the per-column lines this script prints and by
  # test-export-prep.R; here we prove nothing ELSE was dropped and nothing carried was altered.
)

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
      # Phase 19f: a declared index column keeps its VALUES and gains only its declaration.
      if (DECLARE_INDEX_COLS && is_lvl(new[[col]])) {
        flat <- unlvl(new[[col]])
        if (!identical(old[[col]], flat))
          issues <- c(issues, paste0(nm, " / ", col, ": declared index column's VALUES changed"))
        else cat(sprintf("      %-24s %s declared role=%s var=%s\n", nm, col,
                         lvl_role(new[[col]]), lvl_var(new[[col]])))
      } else if (!identical(old[[col]], new[[col]])) {
        issues <- c(issues, paste0(nm, " / ", col, ": non-fmt column CHANGED"))
      }
      next
    }
    do <- as.list(vctrs::vec_data(old[[col]]))
    dn <- as.list(vctrs::vec_data(new[[col]]))
    for (rn in names(RENAMED_FIELDS)) {                      # Phase 19f: the stated field MAPPING
      r <- RENAMED_FIELDS[[rn]]
      if (!rn %in% names(do) || !r$to %in% names(dn)) next
      if (!identical(r$map(do[[rn]]), dn[[r$to]]))
        issues <- c(issues, paste0(nm, " / ", col, " / ", rn, " -> ", r$to,
                                   ": the renamed field is NOT the declared mapping of the old one"))
      do[[rn]] <- NULL; dn[[r$to]] <- NULL
    }
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
  # Phase 19g: the declared `test` COLUMN renames -- prove the mapping, then compare the remainder.
  if (length(RENAMED_TEST_COLS) && !is.null(ao$test) && !is.null(an$test)) {
    for (cn in names(RENAMED_TEST_COLS)) {
      r <- RENAMED_TEST_COLS[[cn]]
      if (!cn %in% names(ao$test)) next
      if (is.null(r$to)) {                                   # absorbed / dropped
        ao$test[[cn]] <- NULL; next
      }
      if (!r$to %in% names(an$test)) {
        issues <- c(issues, paste0(nm, ": `test` column ", cn, " -> ", r$to, " is MISSING")); next
      }
      if (!identical(r$map(ao$test[[cn]], ao$test), an$test[[r$to]]))
        issues <- c(issues, paste0(nm, ": `test` column ", cn, " -> ", r$to,
                                   " is NOT the declared mapping"))
      ao$test[[cn]] <- NULL; an$test[[r$to]] <- NULL
    }
    ao$test <- ao$test[, order(names(ao$test)), drop = FALSE]
    an$test <- an$test[, order(names(an$test)), drop = FALSE]
  }
  # Phase 19g: the whole-`meta` reshape (vars / reg_meta -> spec).
  if (is.function(META_RESHAPE_WHOLE)) {
    if (!isTRUE(tryCatch(META_RESHAPE_WHOLE(ao$meta, an$meta), error = function(e) FALSE)))
      issues <- c(issues, paste0(nm, ": meta RESHAPE lost information"))
    else cat(sprintf("      %-24s meta reshaped into spec(kind=%s), same information\n", nm,
                     an$meta$spec$kind %||% "?"))
    ao$meta <- NULL; an$meta <- NULL
    ao <- ao[!vapply(ao, is.null, logical(1))]; an <- an[!vapply(an, is.null, logical(1))]
  }
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
