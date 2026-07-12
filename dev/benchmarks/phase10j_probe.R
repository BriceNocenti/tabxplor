#!/usr/bin/env Rscript
# PURPOSE: Phase 10j-B (B-i, step 2a-fine) -- LINE profile of chi2_compute_test() + tab_chi2(), to
#   split the 17 % chi2_compute_test cost into: subtable-grouping select (L5624, resists a clean base-R
#   port) vs the long-frame dplyr marshalling (L5635-5698, the clean data.table target) vs the engine
#   (agg_chi2, not a target) vs the final assembly. Grounds the abandon-rule gate.
# USAGE (from package root):  Rscript dev/benchmarks/phase10j_probe.R

pkg <- normalizePath(".", winslash = "/")
suppressMessages(devtools::load_all(pkg, quiet = TRUE))

gss <- forcats::gss_cat
gss$year <- factor(gss$year)
FIX <- function() tab(gss, c(marital, race, rincome, partyid), c(relig, denom, year),
                      pct = "row", color = "diff", chi2 = TRUE)
invisible(FIX())

prof <- tempfile(fileext = ".out")
Rprof(prof, interval = 0.002, line.profiling = TRUE)
for (i in seq_len(60L)) invisible(FIX())
Rprof(NULL)
sm <- summaryRprof(prof, lines = "show")

bl <- sm$by.line
# keep only tab.R lines in the chi2 region
rn <- rownames(bl)
keep <- grepl("tab\\.R#5(5[2-9][0-9]|[67][0-9][0-9])", rn)  # tab.R lines ~5520-5799
sub <- bl[keep, , drop = FALSE]
sub <- sub[order(-sub$total.pct), , drop = FALSE]
cat("==== by.line, tab.R chi2 region (top 30 by total.pct) ====\n")
print(utils::head(data.frame(total.pct = sub$total.pct, self.pct = sub$self.pct,
                             row.names = rownames(sub)), 30))
