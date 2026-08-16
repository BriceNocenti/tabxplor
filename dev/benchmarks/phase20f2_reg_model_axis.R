# phase20f2_reg_model_axis.R -- Phase 20f-ii: is there a win in running a `tab_reg()` call's
# SEVERAL INDEPENDENT MODELS in parallel?
#
# 20f-i answered "no pool" for the inside of ONE model build (dev/tabxplor_reg_performance.md).
# This harness asks the question one axis out, over the three shapes that build several models:
#   G  the `tab_vars` split groups          -- reg_stage_split(), a recursive reg_build() per level
#   S  several outcomes / a models list     -- one `spec` per model, ONE table, seven per-spec loops
#   R  several outcomes x a models list     -- tab_reg()'s own recursion, one table per outcome
#
# IT MEASURES THREE THINGS, and the second is what decides the phase:
#   1a the AXIS GRID    -- the whole call, and each unit built ALONE. From those: the parallel
#      CEILING (Amdahl with enough workers: serial_shared + max_unit) and the BALANCE (max/mean).
#      An unbalanced axis cannot reach its unit count however many cores are free.
#   1b the TRANSPORT    -- daemon spin-up and the everywhere() ship of the fixture. The ceiling is
#      only worth having if it is not eaten by getting the data to the workers.
#   1c the REDUNDANCY   -- call counts of the quantities a multi-model call may compute per unit and
#      read once. 20f-i's whole finding was that this, not concurrency, is where the time is; the
#      counts are the contract that says whether it repeats one axis out.
#
# USAGE (unsandboxed, nothing else running -- CLAUDE.md's orphan / oversubscription warnings):
#   OMP_NUM_THREADS=1 Rscript dev/benchmarks/phase20f2_reg_model_axis.R \
#     > dev/benchmarks/results_2.0.0/phase20f2_reg_model_axis.txt
#
# Public API + system.time + trace(), so it runs unchanged on any tree -- run it on the BEFORE tree
# and on the AFTER and diff the two files. Same fixtures as phase20f_reg_profile.R.
#
# WARNING the timings are ext4/WSL2 and are NOT comparable to the committed Windows baselines.

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
    if (!ok) CNT$n[[key]] <- NA_integer_
  }
  suppressMessages(suppressWarnings(eval(expr, env)))
  CNT$n
}

# The five quantities a MULTI-MODEL call can repeat. `reg_fit` is the unit of real cost; the other
# four are the per-unit work 20f-ii suspects of being computed k times and read once.
PROBES <- list(
  reg_fit          = list(fun = "reg_fit",           ns = "tabxplor"),
  reg_skeleton     = list(fun = "reg_skeleton",      ns = "tabxplor"),
  reg_resolve_args = list(fun = "reg_resolve_args",  ns = "tabxplor"),
  reg_empirical    = list(fun = "reg_empirical",     ns = "tabxplor"),
  reg_emp_fit      = list(fun = "reg_empirical_fit", ns = "tabxplor")
)

timeit <- function(expr, reps = 3L, env = parent.frame()) {
  suppressMessages(suppressWarnings(eval(expr, env)))           # warm up (JIT, first-call setup)
  min(replicate(reps, system.time(suppressMessages(suppressWarnings(eval(expr, env))))[["elapsed"]]))
}

# --- the fixtures (phase20f_reg_profile.R's, plus the outcomes the S axis needs) -------------------
g          <- forcats::gss_cat
g$age10    <- g$age / 10
g$married  <- factor(ifelse(g$marital == "Married", "yes", "no"))
g$relig3   <- forcats::fct_lump_n(g$relig, 2)
g$dem      <- factor(ifelse(grepl("dem", g$partyid, ignore.case = TRUE), "yes", "no"))
g$norelig  <- factor(ifelse(g$relig == "None", "yes", "no"))
g$tv3      <- factor(ifelse(g$tvhours >= 3, "yes", "no"))

big        <- g[sample(nrow(g), 200000L, replace = TRUE), ]
big$x1     <- stats::rnorm(nrow(big))
big$x2     <- stats::rnorm(nrow(big))
# `tab_vars` takes a factor/character. `yearf` is the EVEN axis (8 near-equal survey waves) beside
# `race`, which is the uneven one -- balance is what decides this phase, so measure both.
big$yearf  <- factor(big$year)

P4  <- c("race", "rincome", "age10", "year")
P6  <- c("race", "rincome", "age10", "year", "x1", "x2")
P5  <- c("race", "rincome", "age10", "year", "x1")

# ==================================================================================================
# 1a -- THE AXIS GRID
# ==================================================================================================
# One entry = one shape. `whole` is the real call; `units` are the same models built ALONE, in the
# same order, so sum(units) is the parallelisable work and max(units) is what a perfect pool leaves.
CASES <- list()
# `whole` is substituted; `units` is an ordinary list of QUOTED calls (bquote), so each one is
# re-evaluated on its own and never rebuilds its siblings.
case <- function(label, axis, whole, units)
  CASES[[label]] <<- list(axis = axis, n_units = length(units),
                          whole = substitute(whole), units = units)

race_lv <- levels(forcats::fct_drop(as.factor(big$race)))       # 4 groups, UNEVEN (White dominates)
year_lv <- levels(big$yearf)                                    # 8 groups, EVEN

# the group subsets are pre-computed: subsetting inside the timed expression would charge the unit
# for work the real split loop also does, and would bias the ceiling against a pool.
SUB_RACE   <- lapply(race_lv, function(lv) big[big$race == lv, ])
SUB_YEAR   <- lapply(year_lv, function(lv) big[big$yearf == lv, ])
SUB_RACE_G <- lapply(race_lv, function(lv) g[g$race == lv, ])

case("G tab_vars 4 groups (race), n = 200 000", "G",
     tab_reg(big, "married", c("rincome", "age10", "x1"), tab_vars = "race"),
     lapply(seq_along(race_lv), function(i)
       bquote(tab_reg(SUB_RACE[[.(i)]], "married", c("rincome", "age10", "x1")))))

case("G tab_vars 8 groups (year), n = 200 000", "G",
     tab_reg(big, "married", c("rincome", "age10", "x1"), tab_vars = "yearf"),
     lapply(seq_along(year_lv), function(i)
       bquote(tab_reg(SUB_YEAR[[.(i)]], "married", c("rincome", "age10", "x1")))))

case("G tab_vars 4 groups (race), n = 21 483", "G",
     tab_reg(g, "married", c("rincome", "age10"), tab_vars = "race"),
     lapply(seq_along(race_lv), function(i)
       bquote(tab_reg(SUB_RACE_G[[.(i)]], "married", c("rincome", "age10")))))

case("S 2 outcomes, one table, n = 200 000", "S",
     tab_reg(big, c("married", "dem"), P6),
     lapply(c("married", "dem"), function(y) bquote(tab_reg(big, .(y), P6))))

case("S 4 outcomes, one table, n = 200 000", "S",
     tab_reg(big, c("married", "dem", "norelig", "tv3"), P6),
     lapply(c("married", "dem", "norelig", "tv3"), function(y) bquote(tab_reg(big, .(y), P6))))

case("S 3-model comparison UNBALANCED, n = 200 000", "S",
     tab_reg(big, "married", list(M1 = "race", M2 = c("race", "age10"), M3 = P6)),
     lapply(list("race", c("race", "age10"), P6),
            function(p) bquote(tab_reg(big, "married", .(p)))))

case("S 3-model comparison BALANCED, n = 200 000", "S",
     tab_reg(big, "married", list(M1 = P4, M2 = P5, M3 = P6)),
     lapply(list(P4, P5, P6), function(p) bquote(tab_reg(big, "married", .(p)))))

case("R 2 outcomes x a models list, n = 200 000", "R",
     tab_reg(big, c("married", "dem"), list(M1 = P4, M2 = P6)),
     lapply(c("married", "dem"),
            function(y) bquote(tab_reg(big, .(y), list(M1 = P4, M2 = P6)))))

cat("tabxplor Phase 20f-ii -- tab_reg() MODEL-AXIS profile --", R.version.string, "\n")
cat("fixture: forcats::gss_cat (", nrow(g), " rows) and a seeded 200 000-row resample\n", sep = "")
cat("platform: ext4 / WSL2 -- NOT comparable to the committed Windows baselines\n")
cat("timings: min of 3 warm runs.  counts: one instrumented run.\n\n")

cat("=== 1a  THE AXIS GRID ===\n")
cat("whole   = the real call.   units = the same models built one at a time.\n")
cat("ceiling = (whole - sum units) + max unit, i.e. a perfect pool with a unit per core.\n")
cat("balance = max unit / mean unit; 1.00 is perfectly even, high means one unit dominates.\n\n")
cat(sprintf("%-44s %8s %8s %8s %8s %8s   %s\n",
            "shape", "whole", "sum u", "max u", "ceiling", "balance", "speedup"))
cat(strrep("-", 118), "\n")

grid <- lapply(names(CASES), function(lbl) {
  cs    <- CASES[[lbl]]
  # one bad case must not cost the other nine: a 200 000-row grid is ~10 minutes.
  whole <- tryCatch(timeit(cs$whole), error = function(e) {
    cat(sprintf("%-44s  SKIPPED: %s\n", lbl, conditionMessage(e))); NA_real_ })
  if (is.na(whole)) return(NULL)
  us    <- vapply(cs$units, function(e) timeit(e), numeric(1))
  # ⚠ clamped at max(us). A unit built ALONE re-runs the argument boundary that the real call runs
  # once, so `sum us` can exceed `whole` and the shared term go negative -- which would put the
  # ceiling BELOW the longest unit, i.e. below what one worker must take however many others help.
  ceiling  <- max(max(us), (whole - sum(us)) + max(us))
  speedup  <- whole / ceiling
  cat(sprintf("%-44s %6.2f s %6.2f s %6.2f s %6.2f s %7.2f   %6.2fx\n",
              lbl, whole, sum(us), max(us), ceiling, max(us) / mean(us), speedup))
  data.frame(case = lbl, axis = cs$axis, n_units = cs$n_units,
             whole = round(whole, 3), sum_units = round(sum(us), 3),
             max_unit = round(max(us), 3), ceiling = round(ceiling, 3),
             balance = round(max(us) / mean(us), 3), speedup_ceiling = round(speedup, 3),
             stringsAsFactors = FALSE)
})

cat("\nNOTE `sum units` is an OVER-estimate of the loop's own content: each unit built alone re-runs\n")
cat("the argument boundary, which the real call runs once. So `ceiling` is conservative -- a real\n")
cat("pool cannot beat it, and may fall short of it.\n")

# ==================================================================================================
# 1b -- THE TRANSPORT
# ==================================================================================================
cat("\n=== 1b  THE TRANSPORT (what a pool costs before it computes anything) ===\n")
transport <- NULL
if (requireNamespace("mirai", quietly = TRUE)) {
  sz <- function(x) length(serialize(x, NULL)) / 1024^2
  cat(sprintf("fixture serialized: gss_cat %.1f MB, the 200 000-row resample %.1f MB\n",
              sz(g), sz(big)))
  try({
    mirai::daemons(0, .compute = "bench20f2")
    t_spin <- system.time(mirai::daemons(4L, .compute = "bench20f2"))[["elapsed"]]
    t_dev  <- system.time(mirai::everywhere(
      { suppressMessages(pkgload::load_all(dev, quiet = TRUE)) },
      dev = "~/github/tabxplor", .compute = "bench20f2"))[["elapsed"]]
    t_ship <- system.time(mirai::everywhere({ assign("D", d, envir = .GlobalEnv) },
                                            d = big, .compute = "bench20f2"))[["elapsed"]]
    # ⚠ the FIRST mirai_map on a fresh pool pays the dispatcher's own connection setup, which is a
    # once-per-pool cost, not a per-call one. Measuring only that would charge every call for it and
    # answer the phase's question wrongly, so measure both and report both.
    t_rt1  <- system.time(mirai::mirai_map(as.list(1:4), function(i) i,
                                           .compute = "bench20f2")[])[["elapsed"]]
    t_rtw  <- min(replicate(5, system.time(mirai::mirai_map(as.list(1:4), function(i) i,
                                           .compute = "bench20f2")[])[["elapsed"]]))
    mirai::daemons(0, .compute = "bench20f2")
    cat(sprintf("daemons(4) spin-up         %6.2f s   } once per session\n", t_spin))
    cat(sprintf("everywhere(load_all)       %6.2f s   } (dev only -- an installed package costs 0)\n", t_dev))
    cat(sprintf("first 4-task round-trip    %6.2f s   } dispatcher connection setup\n", t_rt1))
    cat(sprintf("everywhere(ship 200k rows) %6.2f s   <- once per dispatch\n", t_ship))
    cat(sprintf("warm 4-task round-trip     %6.3f s  <- once per dispatch\n", t_rtw))
    transport <- data.frame(spin_up = round(t_spin, 3), load_all = round(t_dev, 3),
                            ship_big = round(t_ship, 3), round_trip_first = round(t_rt1, 3),
                            round_trip_warm = round(t_rtw, 4),
                            mb_gss = round(sz(g), 1), mb_big = round(sz(big), 1))
  }, silent = FALSE)
} else {
  cat("mirai not installed -- transport not measured.\n")
}

# ==================================================================================================
# 1c -- THE REDUNDANCY
# ==================================================================================================
cat("\n=== 1c  THE REDUNDANCY (what a multi-model call computes per unit and reads once) ===\n")
cat(sprintf("%-44s   %s\n", "shape", paste(names(PROBES), collapse = " / ")))
cat(strrep("-", 118), "\n")

# ⚠ these run on `g`, so their predictor sets must exist there -- `x1` / `x2` are the big fixture's.
Q2 <- c("race", "age10")
Q3 <- c("race", "rincome", "age10")
RED <- list(
  "1 model, empirical, n = 21 483" =
    quote(tab_reg(g, "married", P4, empirical = TRUE)),
  "3-model comparison, empirical, n = 21 483" =
    quote(tab_reg(g, "married", list(M1 = Q2, M2 = Q3, M3 = P4), empirical = TRUE)),
  "3-model comparison, adjustment colour, n = 21 483" =
    quote(tab_reg(g, "married", list(M1 = Q2, M2 = Q3, M3 = P4), color = c(TRUE, "adjustment"))),
  "2 outcomes, empirical, n = 21 483" =
    quote(tab_reg(g, c("married", "dem"), P4, empirical = TRUE)),
  "tab_vars 4 groups, n = 21 483" =
    quote(tab_reg(g, "married", c("rincome", "age10"), tab_vars = "race")),
  "2 outcomes x a models list, n = 21 483" =
    quote(tab_reg(g, c("married", "dem"), list(M1 = Q2, M2 = Q3)))
)

red <- lapply(names(RED), function(lbl) {
  n <- tryCatch(counting(RED[[lbl]], PROBES), error = function(e) {
    cat(sprintf("%-44s   SKIPPED: %s\n", lbl, conditionMessage(e))); NULL })
  if (is.null(n)) return(NULL)
  cat(sprintf("%-44s   %s\n", lbl, paste(ifelse(is.na(n), "-", n), collapse = " / ")))
  cbind(data.frame(case = lbl, stringsAsFactors = FALSE), as.data.frame(as.list(n)))
})

cat("\nA count is a contract: a later run that sees a bigger number has found a regression, and one\n")
cat("that sees a smaller one has found a de-duplication. `reg_empirical_fit` is the expensive column\n")
cat("here -- it fits one univariable model per predictor with no closed form.\n")

# --- write ----------------------------------------------------------------------------------------
tag <- Sys.getenv("TABXPLOR_BENCH_TAG", "run")
dir <- file.path("dev", "benchmarks", "results_2.0.0")
utils::write.csv(do.call(rbind, grid),
                 file.path(dir, paste0("phase20f2_axis_grid_", tag, ".csv")), row.names = FALSE)
utils::write.csv(do.call(rbind, red),
                 file.path(dir, paste0("phase20f2_redundancy_", tag, ".csv")), row.names = FALSE)
if (!is.null(transport))
  utils::write.csv(transport, file.path(dir, paste0("phase20f2_transport_", tag, ".csv")),
                   row.names = FALSE)
