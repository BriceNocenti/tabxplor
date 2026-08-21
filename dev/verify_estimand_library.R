# PURPOSE: the composed estimand library equals the declared one it replaced (Phase 22b-xv-1).
# ROLE: the one-off sweep behind the derivation of REG_ESTIMANDS -- every (family, effect, measure)
#   resolved at the family's OWN link, compared member by member against a snapshot of the
#   hand-written table. Kept in dev/ rather than tests/: it needs a snapshot of a retired table.
#
# USAGE: Rscript dev/verify_estimand_library.R <snapshot.rds>
#   where the snapshot was written, before the rewrite, by
#     lapply(REG_ESTIMANDS, ...) with `why` / `note` closures evaluated.
suppressMessages(devtools::load_all(".", quiet = TRUE))
args <- commandArgs(trailingOnly = TRUE)
old  <- readRDS(args[[1]])                       # head_resolved.rds: a list of flattened rows

EFF <- c(coefficient = "conditional", marginal = "marginal", at_reference = "at_reference")
flat <- function(r) {
  r$why  <- if (is.function(r$why))  r$why()  else NA_character_
  r$note <- if (is.function(r$note)) r$note() else NA_character_
  r
}
cmp <- c("status", "effect", "measure", "builder", "fit", "exp", "word", "scale",
         "crude_fam", "crude_shape", "comparison", "obs", "note", "why")
same <- function(a, b) {
  if (is.null(a) && is.null(b)) return(TRUE)
  if (is.null(a) || is.null(b)) return(FALSE)
  if (is.na(a[[1]]) && is.na(b[[1]])) return(TRUE)
  isTRUE(all.equal(a, b))
}

diffs <- list()
for (o in old) {
  k <- strsplit(o$key, " ")[[1]]
  fam <- k[[1]]; eff <- k[[2]]; mea <- k[[3]]
  n <- flat(reg_estimand(fam, measure = mea, effect = unname(EFF[eff])))
  n$obs <- if (identical(n$status, "ok")) reg_estimand_obs(n) else NULL
  n$effect <- if (is.null(n$effect)) NULL else names(EFF)[match(n$effect, EFF)]
  for (m in cmp) {
    if (!same(o[[m]], n[[m]]))
      diffs[[length(diffs) + 1L]] <- sprintf(
        "%-46s %-12s  was %-58s now %s", o$key, m,
        paste(utils::head(as.character(o[[m]]), 1), collapse = ""),
        paste(utils::head(as.character(n[[m]]), 1), collapse = ""))
  }
}
cat(length(old), "resolved cells swept,", length(diffs), "member differences\n\n")
cat(paste(unlist(diffs), collapse = "\n"), "\n")
