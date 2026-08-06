# verify_golden_field_delta.R -- prove that adding a per-cell fmt FIELD changed the goldens by exactly
# that field and nothing else.
#
# WHY THIS FILE EXISTS. Three phases have added a field (n_eff, obs, gap_se). Each time the 36
# structural `_golden/*.rds` had to be regenerated, each time a throwaway script proved "the only delta
# is an all-NA column", and each time the script was deleted -- so the next phase wrote it again from
# the prose in dev/make_golden.R. It is committed now.
#
# HOW TO USE IT, around a field addition:
#   1. BEFORE regenerating, run it once: it reads the committed goldens and rebuilds the same cases
#      from the current source, then reports, per case and per fmt column, (a) which fields are new,
#      (b) whether every SHARED field is bit-identical, (c) whether each new field is entirely NA, and
#      (d) whether the column ATTRIBUTES are unchanged.
#   2. Only if every case says OK, run dev/make_golden.R and record the delta in its ledger.
# A single "CHANGED" line means the new field is not the only difference -- investigate before
# regenerating, because the goldens are the byte-identity contract.
#
# Rendering is NOT checked here (a non-displayed field must not move `_snaps/golden.md` either);
# testthat reports that by itself when the suite runs.

devtools::load_all("~/github/tabxplor", quiet = TRUE)
source("tests/testthat/helper-golden.R")

cases   <- golden_cases()
gdir    <- "tests/testthat/_golden"
n_cells <- 0L
issues  <- character(0)

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
    if (!identical(ao, an))
      issues <- c(issues, paste0(nm, " / ", col, ": column ATTRIBUTES changed"))
    n_cells <- n_cells + length(old[[col]])
  }
  # table-level attributes (subtext / test / meta) must be untouched by a field pass
  ta <- function(t) attributes(t)[intersect(names(attributes(t)), c("subtext", "test", "meta"))]
  if (!identical(ta(old), ta(new)))
    issues <- c(issues, paste0(nm, ": TABLE attributes changed"))

  cat(if (length(added)) sprintf("OK    %-28s new field(s): %s\n", nm, paste(added, collapse = ", "))
      else               sprintf("OK    %-28s (no new field)\n", nm))
}

cat("\n", n_cells, " cells checked across ", length(cases), " cases.\n", sep = "")
if (length(issues)) {
  cat("\nPROBLEMS -- do NOT regenerate:\n"); cat(paste0("  ", issues, collapse = "\n"), "\n")
} else {
  cat("Only the added field differs. Safe to run dev/make_golden.R.\n")
}
