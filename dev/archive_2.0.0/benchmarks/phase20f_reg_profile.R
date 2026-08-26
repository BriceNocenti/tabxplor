# phase20f_reg_profile.R -- Phase 20f: where a `tab_reg()` call actually spends its time, and how
# often it computes the same thing twice.
#
# THE QUESTION 20f had to answer was "is a process pool worth anything now that 20d and 20e have
# landed". This harness is the measurement that answered it -- with a "no", because the remaining
# cost turned out not to be many fits to spread but one quantity computed several times and read
# once (dev/tabxplor_reg_performance.md holds the verdict and its reasons).
#
# It measures TWO things, and the second is what makes a regression visible:
#   1. wall clock per table SHAPE, default `stats` against `stats = FALSE` -- the share of a call
#      that is the model-check footer rather than the table;
#   2. CALL COUNTS of the three expensive repeated quantities: nnet:::multinomHess (the multinomial
#      vcov), brant::brant (the ordinal proportional-odds test) and tabxplor:::reg_term_tests (the
#      drop1 refit). A count is a contract: it cannot drift quietly the way a second can.
#
# USAGE (unsandboxed, nothing else running -- CLAUDE.md's orphan / oversubscription warnings):
#   OMP_NUM_THREADS=1 Rscript dev/benchmarks/phase20f_reg_profile.R \
#     > dev/benchmarks/results_2.0.0/phase20f_reg_profile.txt
#
# Public API + system.time + trace(), so it runs unchanged on any tree -- run it on a stashed BEFORE
# and on the AFTER and diff the two files. forcats::gss_cat is the repo's standard fixture; the
# 200 000-row case is a seeded resample of it, because the check cost is O(N) while the table is not.
#
# WARNING the timings are ext4/WSL2 and are NOT comparable to the committed Windows baselines
# (dev/benchmarks/baseline.csv and the three tests/testthat/*benchmark_baseline.csv).

suppressMessages(pkgload::load_all("~/github/tabxplor", export_all = TRUE, helpers = FALSE,
                                   quiet = TRUE))
data.table::setDTthreads(1L)
set.seed(1)

# trace() evaluates its tracer in the traced function's own frame, so the counter cannot be a local
# of this script -- it lives in an environment reached from the global one.
CNT <- new.env(parent = emptyenv())

counting <- function(expr, probes) {
  env <- parent.frame()
  CNT$n <- stats::setNames(integer(length(probes)), names(probes))
  on.exit(for (p in names(probes))
    suppressMessages(try(untrace(probes[[p]][["fun"]], where = asNamespace(probes[[p]][["ns"]])),
                         silent = TRUE)), add = TRUE)
  for (p in names(probes)) {
    key <- p
    ok <- tryCatch({
      suppressMessages(trace(probes[[p]][["fun"]], where = asNamespace(probes[[p]][["ns"]]),
                             print = FALSE,
                             tracer = bquote({ CNT$n[[.(key)]] <- CNT$n[[.(key)]] + 1L })))
      TRUE
    }, error = function(e) FALSE)
    if (!ok) CNT$n[[key]] <- NA_integer_                      # the package is not installed
  }
  suppressMessages(suppressWarnings(eval(expr, env)))
  CNT$n
}

PROBES <- list(
  multinomHess  = list(fun = "multinomHess",   ns = "nnet"),
  brant         = list(fun = "brant",          ns = "brant"),
  reg_term_test = list(fun = "reg_term_tests", ns = "tabxplor")
)

timeit <- function(expr, reps = 3L) {
  env <- parent.frame()
  suppressMessages(suppressWarnings(eval(expr, env)))         # warm up (JIT, first-call setup)
  min(replicate(reps, system.time(suppressMessages(suppressWarnings(eval(expr, env))))[["elapsed"]]))
}

# One row = one shape, timed with the default footer and with none, then counted.
CASES <- list()
case <- function(label, expr, expr_nostats)
  CASES[[label]] <<- list(expr = substitute(expr), nostats = substitute(expr_nostats))

# --- the fixtures ---------------------------------------------------------------------------------
g          <- forcats::gss_cat
g$age10    <- g$age / 10
g$married  <- factor(ifelse(g$marital == "Married", "yes", "no"))
g$relig3   <- forcats::fct_lump_n(g$relig, 2)

big        <- g[sample(nrow(g), 200000L, replace = TRUE), ]
big$x1     <- stats::rnorm(nrow(big))
big$x2     <- stats::rnorm(nrow(big))

small      <- g[sample(nrow(g), 2000L), ]

P4  <- c("race", "rincome", "age10", "year")
P3  <- c("race", "age10", "year")
P6  <- c("race", "rincome", "age10", "year", "x1", "x2")

case("binomial 4 preds, n = 2 000",
     tab_reg(small, "married", P4),
     tab_reg(small, "married", P4, stats = FALSE))
case("binomial 4 preds, n = 21 483",
     tab_reg(g, "married", P4),
     tab_reg(g, "married", P4, stats = FALSE))
case("binomial 6 preds (4 numeric), n = 200 000",
     tab_reg(big, "married", P6),
     tab_reg(big, "married", P6, stats = FALSE))
case("binomial 4 preds + empirical, n = 21 483",
     tab_reg(g, "married", P4, empirical = TRUE),
     tab_reg(g, "married", P4, empirical = TRUE, stats = FALSE))
case("binomial marginal, n = 21 483",
     tab_reg(g, "married", P4, effect = "marginal"),
     tab_reg(g, "married", P4, effect = "marginal", stats = FALSE))
case("multinomial (3 lvl) 4 preds, n = 21 483",
     tab_reg(g, "relig3", P4),
     tab_reg(g, "relig3", P4, stats = FALSE))
case("ordinal (16 lvl) 3 preds, n = 21 483",
     tab_reg(g, "rincome", P3, family = "ordinal"),
     tab_reg(g, "rincome", P3, family = "ordinal", stats = FALSE))
case("gaussian 4 preds, n = 21 483",
     tab_reg(g[!is.na(g$tvhours), ], "tvhours", P4),
     tab_reg(g[!is.na(g$tvhours), ], "tvhours", P4, stats = FALSE))
case("3-model comparison, n = 200 000",
     tab_reg(big, "married", list(M1 = "race", M2 = c("race", "age10"), M3 = P6)),
     tab_reg(big, "married", list(M1 = "race", M2 = c("race", "age10"), M3 = P6), stats = FALSE))
case("tab_vars (4 groups), n = 200 000",
     tab_reg(big, "married", c("rincome", "age10", "x1"), tab_vars = "race"),
     tab_reg(big, "married", c("rincome", "age10", "x1"), tab_vars = "race", stats = FALSE))

# --- run ------------------------------------------------------------------------------------------
cat("tabxplor Phase 20f -- tab_reg() cost profile --", R.version.string, "\n")
cat("fixture: forcats::gss_cat (", nrow(g), " rows), a seeded 200 000-row resample, a 2 000-row draw\n",
    sep = "")
cat("platform: ext4 / WSL2 -- NOT comparable to the committed Windows baselines\n")
cat("timings: min of 3 warm runs.  counts: one instrumented run.\n\n")

cat(sprintf("%-44s %9s %9s %7s   %s\n", "shape", "default", "no stats", "checks",
            "multinomHess / brant / drop1-refit"))
cat(strrep("-", 118), "\n")

out <- lapply(names(CASES), function(lbl) {
  cs   <- CASES[[lbl]]
  full <- timeit(cs$expr)
  bare <- timeit(cs$nostats)
  n    <- counting(cs$expr, PROBES)
  cat(sprintf("%-44s %7.2f s %7.2f s %6.0f%%   %s\n", lbl, full, bare,
              100 * (full - bare) / full,
              paste(ifelse(is.na(n), "-", n), collapse = " / ")))
  data.frame(case = lbl, seconds = round(full, 3), seconds_no_stats = round(bare, 3),
             check_share = round((full - bare) / full, 3),
             multinom_hess = unname(n[["multinomHess"]]), brant = unname(n[["brant"]]),
             term_tests = unname(n[["reg_term_test"]]), stringsAsFactors = FALSE)
})

cat("\n")
cat("`checks` = the share of the call spent on the model-check footer (default minus stats = FALSE).\n")
cat("The three counts are quantities a table computes MORE THAN ONCE and reads ONCE; each is a\n")
cat("contract, so a later phase that re-runs this file sees a regression as a count, not a second.\n")

utils::write.csv(do.call(rbind, out),
                 file.path("dev", "benchmarks", "results_2.0.0",
                           paste0("phase20f_reg_profile_", Sys.getenv("TABXPLOR_BENCH_TAG", "run"),
                                  ".csv")),
                 row.names = FALSE)
