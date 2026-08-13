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

# Phase 18z16-iiiii: `degf` + `basis` -- the two per-column attributes that took over from the
# table-level meta$inference. Every golden case is UNWEIGHTED, so the honest values are "no design df"
# and "no claim": NA and "n", i.e. exactly the constructor defaults, which is why the rendering does
# not move either.
ADDED_ATTRS   <- c("degf", "basis")
EXPECTED_ATTR <- list(degf = NA_real_, basis = "n")

# Phase 18z16-i: no fmt member at all -- the addition is a COLUMN on the table-level `test` tibble
# (`deff` = the design effect the row's test corrected by). A classic-basis table never fills it, so on
# these goldens it must be present and all-NA.
ADDED_TEST_COLS <- character(0)

# Phase 18z16-i: the same pass also adds a `meta` SUB-FIELD (`inference` = the stored inference
# basis). Unlike a field or a test column it has a definite VALUE, and it is stored only on the
# WEIGHTED cases -- an unweighted golden must keep exactly the metadata it had. Declare it here and
# the check prints what it actually found, per case.
# Phase 18z16-iiiii REMOVES one instead: `inference` left `meta` for the two column attributes
# above. No golden case is weighted, so none of them stored it -- declare nothing.
ADDED_META_FIELDS   <- character(0)
REMOVED_META_FIELDS <- character(0)

# A phase can also RESHAPE a `meta` sub-field without changing what it says. Phase 18z16-iiiii folds
# `ci_settings`' five `method_*` scalars into ONE named vector, and drops `method_ratio` (a one-value
# argument). Declare the field with a predicate proving old and new carry the SAME information; the
# script then treats that sub-field as accounted for and still demands bit-identity of everything else.
RESHAPED_META_FIELDS <- list(
  ci_settings = function(old, new) {
    isTRUE(all.equal(old$conf_level, new$conf_level)) &&
      identical(unname(new$method[["cell"]]),       old$method_cell) &&
      identical(unname(new$method[["diff"]]),       old$method_diff) &&
      identical(unname(new$method[["mean_diff"]]),  old$method_mean_diff) &&
      identical(unname(new$method[["mean_ratio"]]), old$method_mean_ratio) &&
      identical(old$method_ratio, "katz")          # the dropped slot had exactly one legal value
  }
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
    if (length(setdiff(names(ao), names(an))))
      issues <- c(issues, paste0(nm, " / ", col, ": attribute(s) REMOVED: ",
                                 paste(setdiff(names(ao), names(an)), collapse = ", ")))
    for (a in intersect(names(ao), names(an))) if (!identical(ao[[a]], an[[a]]))
      issues <- c(issues, paste0(nm, " / ", col, " / ", a, ": attribute CHANGED"))
    for (a in new_at) {
      seen_attrs[[a]] <- unique(c(seen_attrs[[a]], an[[a]]))
      if (a %in% names(EXPECTED_ATTR) && !isTRUE(all.equal(an[[a]], EXPECTED_ATTR[[a]])))
        issues <- c(issues, paste0(nm, " / ", col, " / ", a, ": new attribute is ",
                                   paste(an[[a]], collapse = ", "), ", expected ",
                                   paste(EXPECTED_ATTR[[a]], collapse = ", ")))
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
