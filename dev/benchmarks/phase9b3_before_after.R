#!/usr/bin/env Rscript
# PURPOSE: Phase 9b-3 before/after timer -- the four representative shapes (common/ci/contrib/
#          numeric), merged + output_list, median wall + MB. Run on the pre-9b-3 source to make the
#          "before" file, then re-run after each step. Same fixtures as phase9b2_fmt_cost_decomp.R.
# USAGE:   Rscript dev/benchmarks/phase9b3_before_after.R <label>
#          -> prints a table; compose dev/benchmarks/results_2.0.0/phase9b3_<label>.txt from it.
pkg <- normalizePath(".", winslash = "/")
suppressMessages(devtools::load_all(pkg, quiet = TRUE))
source(file.path(pkg, "tests", "testthat", "helper-benchmark.R"))  # benchmark_measure()

label <- (function() { a <- commandArgs(TRUE); if (length(a)) a[[1]] else "run" })()
gss <- forcats::gss_cat
q   <- function(x) suppressWarnings(suppressMessages(x))
N_ROWVARS <- 5L

fx_call <- list(
  common  = function(ol) q(tab(gss, c(marital, race, relig, rincome, partyid),
                               c(race, marital, partyid),
                               pct = "row", color = "diff", chi2 = TRUE, output_list = ol)),
  ci      = function(ol) q(tab(gss, c(marital, race, relig, rincome, partyid),
                               c(race, marital, partyid),
                               pct = "row", color = "diff", ci = "diff", chi2 = TRUE,
                               output_list = ol)),
  contrib = function(ol) q(tab(gss, c(marital, race, relig, rincome, partyid),
                               c(race, marital, partyid),
                               color = "contrib", output_list = ol)),
  numeric = function(ol) q(tab(gss, c(marital, race, relig, rincome, partyid),
                               c(age, tvhours),
                               color = "diff", output_list = ol))
)

cat("\n#### Phase 9b-3 before/after -- label:", label, "| R",
    as.character(getRversion()), "\n\n")
rows <- lapply(names(fx_call), function(fx) {
  merged <- benchmark_measure(function() fx_call[[fx]](FALSE), iterations = 6L)
  listed <- benchmark_measure(function() fx_call[[fx]](TRUE),  iterations = 6L)
  data.frame(fixture = fx,
             merge_s = round(merged$median_s, 4),
             list_s  = round(listed$median_s, 4),
             per_tab_s = round(listed$median_s / N_ROWVARS, 4),
             merge_mb = round(merged$mem_mb, 1),
             stringsAsFactors = FALSE)
})
print(do.call(rbind, rows), row.names = FALSE)
cat("\n  merge_s = default tab() (runs tab_compact) ; list_s = output_list (no merge)\n")
cat("  per_tab_s = list_s / 5 (the per-row_var build = 9b-3 target)\n")
