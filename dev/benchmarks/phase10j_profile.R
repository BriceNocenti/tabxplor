#!/usr/bin/env Rscript
# PURPOSE: Phase 10j-B (B-i, step 2a) -- fresh line-profile decomposing the coarse "~22 %"
#   whole-table test path (tab_apply_tests -> tab_chi2 -> chi2_compute_test) into its real hot
#   sub-ops, so the PoC (phase10j_tests_parity.R) targets the frames that actually cost time and
#   the abandon-rule gate is grounded. Method = Rprof by.total over a warm loop (the §30 / phase9b2
#   convention). dev/benchmarks/ is .Rbuildignore'd (never run by the suite).
# USAGE (from package root):  Rscript dev/benchmarks/phase10j_profile.R
# NOTE: gss_cat has only 7 discrete vars, so the §30 "5x3" is approximated by a 4x3 factor fixture
#   (12 chi2 tables), factor x factor, pct="row" color="diff" chi2=TRUE -- a representative exploratory
#   crosstab build with the whole-table test on.

pkg <- normalizePath(".", winslash = "/")
suppressMessages(devtools::load_all(pkg, quiet = TRUE))
source(file.path(pkg, "dev", "tests", "testthat", "helper-benchmark.R"))

gss <- forcats::gss_cat
gss$year <- factor(gss$year)     # keep col_vars all-factor -> all chi2 (not ANOVA)

FIX <- function() tab(gss,
                      c(marital, race, rincome, partyid),   # 4 row_vars
                      c(relig, denom, year),                # 3 col_vars (factors)
                      pct = "row", color = "diff", chi2 = TRUE)

invisible(FIX())                                            # warm

# ---- whole-call median (the recoverable-% denominator) ----
tot <- benchmark_measure(FIX, iterations = 20L)
cat(sprintf("whole tab() call: median %.3f s  (%s MB)\n\n", tot$median_s,
            ifelse(is.na(tot$mem_mb), "NA", sprintf("%.0f", tot$mem_mb))))

# ---- Rprof by.total ----
prof <- tempfile(fileext = ".out")
Rprof(prof, interval = 0.003, line.profiling = FALSE)
for (i in seq_len(60L)) invisible(FIX())
Rprof(NULL)
sm <- summaryRprof(prof)
byt <- sm$by.total

frames <- c("tab_apply_tests", "tab_chi2", "chi2_compute_test", "chi2_write_contrib",
            "tab_match_groups_and_totrows", "tab_match_comp_and_tottab", "tab_add_totcol_if_no",
            "detect_totcols", "tab_get_vars", "agg_chi2", "agg_anova",
            "tab_ci", "tab_compact", "tab_build", "tab_transform", "tab_assemble_tables")

cat("==== by.total for the target frames (%% of total run) ====\n")
for (f in frames) {
  key <- paste0("\"", f, "\"")
  if (key %in% rownames(byt)) {
    cat(sprintf("  %-30s total.pct = %5.1f   self.pct = %5.1f\n",
                f, byt[key, "total.pct"], byt[key, "self.pct"]))
  } else {
    cat(sprintf("  %-30s (not on stack)\n", f))
  }
}

cat("\n==== TOP 30 by.total (context) ====\n")
ord <- byt[order(-byt$total.pct), , drop = FALSE]
print(utils::head(data.frame(total.pct = ord$total.pct, self.pct = ord$self.pct,
                             row.names = rownames(ord)), 30))
