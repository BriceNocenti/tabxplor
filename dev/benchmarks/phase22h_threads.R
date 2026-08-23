# PURPOSE: Decide, correctly, whether data.table's default multithreading helps or hurts tabxplor.
# ROLE: The third piece of the Phase 22h harness, separate from phase22h_perf_review.R because the
#       question needs a DIFFERENT experimental design and the wrong one gives the wrong answer.
# KEY CONSTRAINTS:
#   - ⚠ ONE thread setting per PROCESS. `setDTthreads()` tears down and rebuilds data.table's OpenMP
#     pool, and the rebuild lands on whatever is timed next -- so flipping the setting inside a loop
#     measures the flip, not the work. Measured in Phase 22h: an interleaved loop reported the 8M
#     numeric case 2.2x SLOWER with threads, where this design reports it 1.1x FASTER.
#   - ⚠ And never two settings inside one long-running script either: the review harness's own
#     A/B between two whole runs disagreed with this one, because by the time a late suite runs the
#     process carries a different heap and a different pool history.
#   - Each condition is a fresh child Rscript that warms every case, then takes the MIN of 5 runs.
#     The conditions alternate (1, auto, 1, auto) so machine drift cannot favour either.
# USAGE (from the package root):
#   Rscript dev/benchmarks/phase22h_threads.R            # -> results_2.0.0/phase22h_threads.csv
#   Rscript dev/benchmarks/phase22h_threads.R --child=1  # (internal: one condition, one process)

args   <- commandArgs(trailingOnly = TRUE)
child  <- grep("^--child=", args, value = TRUE)
OUT    <- "dev/benchmarks/results_2.0.0/phase22h_threads.csv"
ROUNDS <- 2L                                             # (1, auto) pairs

# The cases: the two sizes where the answer could plausibly differ, and the shapes that stress
# different engines (a contingency grid, a weighted one, moment sums for numeric column variables).
build_cases <- function() {
  source("dev/benchmarks/gen_big_df.R")
  big <- gen_big_df(cache = "dev/benchmarks/big_df.rds")
  g   <- forcats::gss_cat
  q   <- function(e) suppressWarnings(suppressMessages(e))
  list(
    `gss 21k  pct=row`          = function() q(tab(g, "marital", "race", pct = "row")),
    `gss 21k  pct+test+color`   = function() q(tab(g, "marital", "race", pct = "row",
                                                   test = TRUE, color = TRUE)),
    `gss 21k  15 tables`        = function() q(tab(g, c("marital", "relig", "partyid"),
                                                   c("race", "marital"), pct = "row", test = TRUE)),
    `8M  counts`                = function() q(tab(big, "region", "response")),
    `8M  pct=row`               = function() q(tab(big, "region", "response", pct = "row")),
    `8M  weighted pct=col`      = function() q(tab(big, "region", "response", wt = weight, pct = "col")),
    `8M  numeric means x2`      = function() q(tab_num(big, region, c(score, income), response,
                                                       comp = "all")))
}

# --- the child: one thread setting, one process ------------------------------------------------
if (length(child)) {
  thr <- as.integer(sub("^--child=", "", child[[1L]]))
  suppressMessages(devtools::load_all(".", quiet = TRUE))
  data.table::setDTthreads(thr)
  cases <- build_cases()
  for (f in cases) f()                                   # warm EVERY path before any timing
  for (nm in names(cases)) {
    ts <- replicate(5L, system.time(cases[[nm]]())[["elapsed"]])
    cat(sprintf("%s\t%d\t%.4f\t%.4f\n", nm, data.table::getDTthreads(), min(ts), stats::median(ts)))
  }
  quit(save = "no")
}

# --- the parent: alternate the conditions, collect, compare ------------------------------------
run_child <- function(thr) {
  out <- system2("Rscript", c("dev/benchmarks/phase22h_threads.R", paste0("--child=", thr)),
                 stdout = TRUE, stderr = FALSE)
  out <- out[grepl("\t", out)]
  p   <- do.call(rbind, strsplit(out, "\t", fixed = TRUE))
  data.frame(case = p[, 1], threads = as.integer(p[, 2]),
             min_s = as.numeric(p[, 3]), median_s = as.numeric(p[, 4]),
             stringsAsFactors = FALSE)
}

res <- list()
for (i in seq_len(ROUNDS)) for (thr in c(1L, 0L)) {      # 0 = data.table's own maximum
  cat("round ", i, ", setDTthreads(", thr, ") ...\n", sep = ""); flush.console()
  r <- run_child(thr); r$round <- i; r$asked <- thr
  res[[length(res) + 1L]] <- r
}
res <- do.call(rbind, res)

agg <- function(a) stats::aggregate(min_s ~ case, data = res[res$asked == a, ], FUN = min)
one <- agg(1L); many <- agg(0L)
cmp <- merge(one, many, by = "case", suffixes = c("_1", "_auto"))
cmp <- cmp[match(unique(res$case), cmp$case), , drop = FALSE]
cmp$`auto is` <- ifelse(cmp$min_s_auto < cmp$min_s_1,
                        sprintf("%.2fx faster", cmp$min_s_1 / cmp$min_s_auto),
                        sprintf("%.2fx slower", cmp$min_s_auto / cmp$min_s_1))

cat("\ndata.table threads: 1 vs the package default (best of ", ROUNDS,
    " alternating runs, min of 5 each)\n\n", sep = "")
print(data.frame(case = cmp$case, `1 thread` = sprintf("%.4f", cmp$min_s_1),
                 auto = sprintf("%.4f", cmp$min_s_auto), verdict = cmp$`auto is`,
                 check.names = FALSE), row.names = FALSE)
cat("\nauto threads on this machine: ", max(res$threads), "\n", sep = "")

dir.create(dirname(OUT), showWarnings = FALSE, recursive = TRUE)
utils::write.csv(res, OUT, row.names = FALSE)
cat("written: ", OUT, "\n", sep = "")
