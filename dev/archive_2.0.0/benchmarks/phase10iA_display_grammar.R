# Phase 10i-A -- does the display {} grammar slow the display/export pipeline "for nothing"?
# Measures format()/get_num()/print()/tab_kable()/tab_md()/tab_xl() on NO-composite tables (the cost
# every normal table now pays for the display_primary() gate), plus a composite table (informational).
# Run A/B via git stash (see the runner in the header of results_2.0.0/phase10iA_*.txt):
#   Rscript dev/benchmarks/phase10iA_display_grammar.R
# Uses only PUBLIC API + system.time, so it runs identically on the pre-10i-A baseline and after.

devtools::load_all("d:/Statistiques/github/tabxplor", quiet = TRUE)
suppressMessages(library(dplyr))

timeit <- function(expr, reps = 20L) {
  expr <- substitute(expr)
  env  <- parent.frame()
  ts   <- replicate(reps, system.time(eval(expr, env))[["elapsed"]])
  stats::median(ts)
}

# --- fixtures --------------------------------------------------------------------
N   <- 200000L
big <- fmt(n = rep(c(10L, 20L, 30L), length.out = N), type = "row",
           pct = rep(c(0.4, 0.35, 0.25), length.out = N), display = "pct")
big_c <- set_display(big, "{pct} (n={n})")     # composite version of the same column

gss <- forcats::gss_cat
suppressMessages({
  t_plain <- tab(gss, marital, c(race, relig, partyid), pct = "row")
  t_comp  <- tab(gss, marital, c(race, relig, partyid), pct = "row", display = "{pct} (n={n})")
})

has_xl  <- requireNamespace("openxlsx2", quietly = TRUE)
xl_path <- tempfile(fileext = ".xlsx")

# --- measurements ----------------------------------------------------------------
rows <- list(
  c("format() 200k-cell pct col (no composite)", timeit(format(big), 20L)),
  c("get_num() 200k-cell pct col (no composite)", timeit(get_num(big), 40L)),
  c("build tab() 1x3 col_vars",                   timeit(tab(gss, marital, c(race, relig, partyid), pct = "row"), 15L)),
  c("print() plain tab",                          timeit(invisible(capture.output(print(t_plain))), 15L)),
  c("tab_md() plain tab",                         timeit(tab_md(t_plain), 20L)),
  c("format() 200k-cell COMPOSITE col (info)",    timeit(format(big_c), 20L)),
  c("print() COMPOSITE tab (info)",               timeit(invisible(capture.output(print(t_comp))), 15L))
)
kb <- tryCatch(timeit(tab_kable(t_plain), 10L), error = function(e) NA_real_)
rows <- c(rows, list(c("tab_kable() plain tab", kb)))
if (has_xl) {
  xl <- tryCatch(timeit(tab_xl(t_plain, path = xl_path, replace = TRUE), 10L),
                 error = function(e) NA_real_)
  rows <- c(rows, list(c("tab_xl() plain tab", xl)))
}

cat(sprintf("%-46s %10s\n", "operation", "median_s"))
for (r in rows) cat(sprintf("%-46s %10.4f\n", r[[1]], as.numeric(r[[2]])))
