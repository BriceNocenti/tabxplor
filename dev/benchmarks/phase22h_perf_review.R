# PURPOSE: The Phase 22h performance review harness -- times tabxplor on REAL-WORLD use cases so the
#          maintainer can decide which defaults are cheap enough to stay on and which must become
#          opt-in. Reusable: re-run it after any change to get the same grid of numbers.
# ROLE: Standalone dev harness. NOT part of the test suite, NEVER run by R CMD check
#       (dev/benchmarks/ is .Rbuildignore'd). It complements, and does not replace, run_bench.R
#       (the frozen 8M-row baseline grid, reproduced here as suite G so the two stay comparable).
# KEY CONSTRAINTS:
#   - Every case is ONE user-visible call. The unit of the review is what a user waits for, never an
#     internal function -- a default is judged by the wait it imposes, not by where the time goes.
#   - Suite B is a MARGINAL-COST matrix: each row is the same table with exactly ONE option moved,
#     so a number is read as "what this option costs", which is the question the phase asks.
#   - Adaptive repetition, bounded: one discarded warm-up, then repeats within a per-case time
#     budget. A slow case is measured once or twice, never seven times.
#   - Engine-agnostic fixtures: fx_gss() is plain dplyr/forcats, NOT gss_cat_data_formatting(),
#     because 1.3.1 does not have that helper and the A/B must feed both engines the same frame.
#   - A case that errors records NA + its message; the grid never aborts half-way.
# See: dev/tabxplor_2.0.0_performance_review.md (the report this produces).
#
# USAGE (from the package root):
#   Rscript dev/benchmarks/phase22h_perf_review.R                       # everything, current source
#   Rscript dev/benchmarks/phase22h_perf_review.R --suites=A,B,E        # a subset
#   Rscript dev/benchmarks/phase22h_perf_review.R --tag=1core --cores=1 # label the run
#   taskset -c 0,1 Rscript dev/benchmarks/phase22h_perf_review.R --tag=student --cores=2
#   Rscript dev/benchmarks/phase22h_perf_review.R --engine=v131 --suites=Z   # the CRAN 1.3.1 side
# Output: dev/benchmarks/results_2.0.0/phase22h_<tag>_<engine>.csv (+ a printed summary).

# === SECTION: CLI + configuration ===================================================

args <- commandArgs(trailingOnly = TRUE)
arg_of <- function(name, default = NULL) {
  hit <- grep(paste0("^--", name, "="), args, value = TRUE)
  if (!length(hit)) return(default)
  sub(paste0("^--", name, "="), "", hit[[1L]])
}

CFG <- list(
  engine  = arg_of("engine", "current"),                 # "current" (load_all) | "v131" (CRAN lib)
  tag     = arg_of("tag", "desktop"),                    # free label for the run (machine/profile)
  suites  = strsplit(arg_of("suites", "A,B,C,D,E,F,G,H,Z"), ",")[[1L]],
  cores   = as.integer(arg_of("cores", NA)),             # informational: what the run was PINNED to
  budget  = as.numeric(arg_of("budget", 1.2)),           # seconds of repeats per case
  v131lib = arg_of("v131lib", path.expand("~/R/tabxplor131")),
  outdir  = arg_of("outdir", "dev/benchmarks/results_2.0.0"),
  big     = arg_of("big", "dev/benchmarks/big_df.rds")
)
if (is.na(CFG$cores)) CFG$cores <- parallel::detectCores()

# DESIGN: one data.table thread by default. The review measures what a DEFAULT costs, and a number
# that silently used 6 cores cannot be compared with the same number on a 2-core student machine.
# Suite C is the only place parallelism is the subject, and it turns it on explicitly.
dt_threads <- as.integer(arg_of("dt_threads", 1L))

# DESIGN: BLAS is left ALONE by default -- the review must show what a user's own R does. `--blas_
# threads=1` is the counterfactual: on an OpenBLAS-pthread build (Debian/Ubuntu's default, and this
# box's) every glm() in tab_reg() spawns one thread per core for a matrix of a few thousand rows,
# and the spawn costs more than the algebra. Measured in Phase 22h: 10x on a small binomial.
blas_threads <- suppressWarnings(as.integer(arg_of("blas_threads", NA)))

# === SECTION: engine ================================================================

if (identical(CFG$engine, "v131")) {
  .libPaths(c(CFG$v131lib, .libPaths()))
  suppressMessages(library(tabxplor))
  ENGINE_VERSION <- as.character(utils::packageVersion("tabxplor"))
  if (!identical(ENGINE_VERSION, "1.3.1"))
    stop("--engine=v131 wanted tabxplor 1.3.1, found ", ENGINE_VERSION)
} else {
  suppressMessages(devtools::load_all(".", quiet = TRUE))
  ENGINE_VERSION <- as.character(utils::packageVersion("tabxplor"))
}
suppressMessages(data.table::setDTthreads(dt_threads))
if (!is.na(blas_threads) && requireNamespace("RhpcBLASctl", quietly = TRUE)) {
  RhpcBLASctl::blas_set_num_threads(blas_threads)
  RhpcBLASctl::omp_set_num_threads(blas_threads)
}

# === SECTION: fixtures ==============================================================
# Engine-agnostic on purpose (see the header): both 1.3.1 and 2.0.0 must receive byte-identical data.

fx_gss <- function() {
  forcats::gss_cat |>
    dplyr::mutate(
      married  = factor(dplyr::if_else(marital == "Married", "01-Married", "02-Not married")),
      black    = factor(dplyr::if_else(race == "Black", "01-Black", "02-Not black")),
      race     = forcats::fct_relevel(race, "White", "Black", "Other"),
      marital  = forcats::fct_relevel(marital, "Married", "Separated", "Divorced",
                                      "Widowed", "Never married", "No answer"),
      year     = as.factor(year),
      rincome  = forcats::fct_collapse(
        rincome,
        "1-Lt $10000"       = c("Lt $1000", "$1000 to 2999", "$3000 to 3999", "$4000 to 4999",
                                "$5000 to 5999", "$6000 to 6999", "$7000 to 7999", "$8000 to 9999"),
        "2-$10000 to 14999" = "$10000 - 14999",
        "3-$15000 to 24999" = c("$15000 - 19999", "$20000 - 24999"),
        "4-$25000 or more"  = "$25000 or more"),
      dplyr::across(dplyr::where(is.factor),
                    ~ forcats::fct_recode(., NULL = "No answer", NULL = "Refused",
                                          NULL = "Don't know", NULL = "Not applicable")),
      # a plausible survey weight: no design, just unequal probabilities of selection.
      # NA-free by construction -- survey::svydesign() refuses a weight with missings.
      w = 0.4 + (dplyr::coalesce(as.integer(race), 1L) * 0.35) +
        (dplyr::coalesce(as.integer(marital), 1L) %% 3L) * 0.2
    )
}

# A mid-size survey (the Enquete Emploi / EU-SILC order of magnitude), built by replication so the
# level structure -- and therefore the number of CELLS -- is identical to gss. Only n moves.
fx_rep <- function(d, times) {
  out <- d[rep(seq_len(nrow(d)), times), , drop = FALSE]
  rownames(out) <- NULL
  tibble::as_tibble(out)
}

fx_big <- function() {
  if (!file.exists(CFG$big)) {
    source("dev/benchmarks/gen_big_df.R")
    return(gen_big_df(cache = CFG$big))
  }
  readRDS(CFG$big)
}

FX <- new.env(parent = emptyenv())
fx <- function(name) {                                   # lazy: a suite never pays for a fixture it skips
  if (!is.null(FX[[name]])) return(FX[[name]])
  FX[[name]] <- switch(
    name,
    gss    = fx_gss(),
    gss10  = fx_rep(fx("gss"), 10L),                     # ~215k rows
    big    = fx_big(),                                   # 8M rows
    tea    = { utils::data("tea", package = "FactoMineR", envir = environment()); tibble::as_tibble(tea) },
    stop("unknown fixture: ", name)
  )
  FX[[name]]
}

# === SECTION: measurement ===========================================================

quiet <- function(expr) suppressWarnings(suppressMessages(expr))

timeit <- function(f) {
  t <- system.time(force(f()))
  as.numeric(t[["elapsed"]])
}

# One discarded warm-up, then repeats inside CFG$budget. A case slower than `slow_at` is repeated
# once only: repeating a 20 s call seven times buys precision nobody needs and costs two minutes.
# WARNING: never fewer than THREE measured reps below `slow_at`. A median of two IS their mean, so a
# single cold rep -- a render path the warm-up did not touch -- lands at half its weight in the
# headline number. Measured: the console-print case read 0.29 s from {0.088, 0.49} until this was 3.
measure <- function(f, budget = CFG$budget, slow_at = 2, max_reps = 7L) {
  gc(FALSE)
  t0   <- timeit(f)
  reps <- if (t0 > slow_at) 1L else max(3L, min(max_reps, ceiling(budget / max(t0, 1e-3))))
  ts   <- vapply(seq_len(reps), function(i) timeit(f), numeric(1))
  list(median_s = stats::median(ts), min_s = min(ts), reps = reps, warm_s = t0)
}

# DESIGN: one throwaway build before ANY case. R's byte-compiler, the S4/vctrs method caches and the
# first data.table call together cost ~0.4 s, and without this they land entirely on whichever case
# happens to run first -- which is how a warm-up artefact gets read as "the default is expensive".
warm_engine <- function() {
  d <- fx("gss")
  # 1.3.1 spells the whole-table test `chi2 =` and has no numeric col_var in tab().
  if (identical(CFG$engine, "v131")) {
    quiet(tab(d, "race", "marital", pct = "row", chi2 = TRUE, color = "diff"))
    quiet(tab_num(d, race, tvhours))
  } else {
    quiet(tab(d, "race", "marital", pct = "row", test = TRUE, color = TRUE))
    quiet(tab(d, "race", "tvhours"))
  }
  if (identical(CFG$engine, "current")) {
    # tab_reg pulls in broom / MASS / nnet on FIRST use. Warm all three families on a small slice so
    # the load lands nowhere: suite H measures that one-time cost deliberately, in its own process.
    s <- d[seq_len(2000L), , drop = FALSE]
    for (e in list(quote(tab_reg(s, outcome = "married", predictors = "race", family = "binomial")),
                   quote(tab_reg(s, outcome = "relig",   predictors = "race", family = "multinomial")),
                   quote(tab_reg(s, outcome = "rincome", predictors = "race", family = "ordinal"))))
      try(quiet(eval(e)), silent = TRUE)
    # the render paths have their own first-call cost (pillar, the html engine, openxlsx2)
    tt <- quiet(tab(d, "race", "marital", pct = "row", test = TRUE, color = TRUE))
    try(quiet(utils::capture.output(print(tt))), silent = TRUE)
    try(quiet(tab_html(tt)), silent = TRUE)
    try(quiet(tab_md(tt, print = FALSE)), silent = TRUE)
    try(quiet(tab_xl(tt, path = tempfile(fileext = ".xlsx"), open = FALSE)), silent = TRUE)
  }
  invisible(NULL)
}
warm_engine()

RESULTS <- list()

# `expr` is quoted: a case is only ever evaluated inside measure(), never at registration.
bm <- function(suite, case, expr, fixture = "", note = "", group = "") {
  e <- substitute(expr); pf <- parent.frame()
  f <- function() quiet(eval(e, pf))
  row <- tryCatch({
    m <- measure(f)
    data.frame(suite = suite, case = case, group = group, fixture = fixture,
               median_s = round(m$median_s, 4), min_s = round(m$min_s, 4),
               reps = m$reps, note = note, error = "", stringsAsFactors = FALSE)
  }, error = function(e2) {
    data.frame(suite = suite, case = case, group = group, fixture = fixture,
               median_s = NA_real_, min_s = NA_real_, reps = 0L, note = note,
               error = substr(conditionMessage(e2), 1, 160), stringsAsFactors = FALSE)
  })
  RESULTS[[length(RESULTS) + 1L]] <<- row
  cat(sprintf("  %-8s %-42s %9s  %s\n", suite, case,
              if (is.na(row$median_s)) "ERROR" else sprintf("%.4f s", row$median_s),
              row$error))
  flush.console()
  invisible(row)
}

run_suite <- function(id) id %in% CFG$suites

# === SECTION: suite A -- what a plain table costs, by size ==========================
# The scale curve of the ONE call every user makes first.

if (run_suite("A")) {
  cat("\n== A: the plain table, by size ==\n")
  for (nm in c("gss", "gss10", "big")) {
    d  <- fx(nm)
    rv <- if (nm == "big") "region"   else "rincome"
    cv <- if (nm == "big") "response" else "marital"
    bm("A", paste0("counts only [", nm, "]"),      tab(d, rv, cv), nm, group = "counts")
    bm("A", paste0("pct=row, defaults [", nm, "]"), tab(d, rv, cv, pct = "row"), nm, group = "pct")
    bm("A", paste0("pct=row+test+color [", nm, "]"),
       tab(d, rv, cv, pct = "row", test = TRUE, color = TRUE), nm, group = "full")
  }
}

# === SECTION: suite B -- the marginal cost of one option ============================
# THE suite this phase exists for. Same table every time, exactly one option moved. Read a row as
# "what turning this on costs a user", and compare it with the `baseline` row of the same fixture.

bm_options <- function(nm) {
  d  <- fx(nm)
  rv <- if (nm == "big") "region"   else "rincome"
  cv <- if (nm == "big") "response" else "marital"
  wv <- if (nm == "big") "weight"   else "w"
  nv <- if (nm == "big") "score"    else "tvhours"
  tg <- if (nm == "big") "age_grp"  else "race"
  L  <- function(x) paste0(x, " [", nm, "]")

  bm("B", L("baseline pct=row"),        tab(d, rv, cv, pct = "row"), nm, group = "baseline")
  # -- the interval, which `ci = "auto"` turns on by itself whenever stars are asked for
  bm("B", L("stars=FALSE (no interval)"), tab(d, rv, cv, pct = "row", stars = FALSE), nm, group = "ci")
  bm("B", L("ci='no' explicit"),        tab(d, rv, cv, pct = "row", ci = "no"), nm, group = "ci")
  bm("B", L("ci='cell'"),               tab(d, rv, cv, pct = "row", ci = "cell"), nm, group = "ci")
  # -- the whole-table test and the colour engine
  bm("B", L("test=TRUE"),               tab(d, rv, cv, pct = "row", test = TRUE), nm, group = "test")
  bm("B", L("color=TRUE"),              tab(d, rv, cv, pct = "row", color = TRUE), nm, group = "color")
  bm("B", L("color='contrib'"),         tab(d, rv, cv, pct = "row", test = TRUE, color = "contrib"),
     nm, group = "color")
  bm("B", L("color_signif='guaranteed'"),
     tab(d, rv, cv, pct = "row", color = TRUE, color_signif = "guaranteed_effect"), nm, group = "color")
  # -- weights and the design-effect interval
  bm("B", L("wt="),                     tab(d, rv, cv, pct = "row", wt = !!wv), nm, group = "weights")
  bm("B", L("wt= + design_effect"),
     tab(d, rv, cv, pct = "row", wt = !!wv, design_effect = TRUE), nm, group = "weights")
  des <- try(suppressWarnings(survey::svydesign(
    ids = ~1, weights = stats::as.formula(paste0("~", wv)), data = as.data.frame(d))), silent = TRUE)
  if (!inherits(des, "try-error"))
    bm("B", L("survey design (ids=~1)"), tab(des, rv, cv, pct = "row"), nm, group = "weights")
  # -- shape of the table
  bm("B", L("comp='all'"),              tab(d, rv, cv, pct = "row", comp = "all"), nm, group = "shape")
  bm("B", L("tab_vars (3-6 subtables)"),
     tab(d, rv, cv, tab_vars = tg, pct = "row"), nm, group = "shape")
  bm("B", L("totaltab='table'"),
     tab(d, rv, cv, tab_vars = tg, pct = "row", totaltab = "table"), nm, group = "shape")
  bm("B", L("spread_vars"),
     tab(d, rv, cv, tab_vars = tg, pct = "row", spread_vars = tg), nm, group = "shape")
  bm("B", L("n='range'"),               tab(d, rv, cv, pct = "row", n = "range"), nm, group = "display")
  bm("B", L("digits=1"),                tab(d, rv, cv, pct = "row", digits = 1), nm, group = "display")
  bm("B", L("cleannames=FALSE"),        tab(d, rv, cv, pct = "row", cleannames = FALSE), nm, group = "display")
  bm("B", L("na='drop'"),               tab(d, rv, cv, pct = "row", na = "drop"), nm, group = "shape")
  # -- the numeric column axis (means), where the cost profile is entirely different
  bm("B", L("numeric col_var (mean)"),  tab(d, rv, nv), nm, group = "numeric")
  bm("B", L("numeric col_var stars=F"), tab(d, rv, nv, stars = FALSE), nm, group = "numeric")
  bm("B", L("numeric col_var display={mean}"), tab(d, rv, nv, display = "{mean}"), nm, group = "numeric")
  bm("B", L("numeric row_var (auto shape)"),
     tab(d, if (nm == "big") "income" else "age", cv, pct = "row"), nm, group = "numeric")
  bm("B", L("numeric row_var shape=quartiles"),
     tab(d, if (nm == "big") "income" else "age", cv, pct = "row",
         shape = if (nm == "big") c(income = "quartiles") else c(age = "quartiles")), nm, group = "numeric")
}

if (run_suite("B")) {
  cat("\n== B: the marginal cost of one option ==\n")
  bm_options("gss")
  bm_options("gss10")
}

# === SECTION: suite C -- the table-of-tables, serial vs parallel ====================
# The core exploratory workflow, and the ONLY place parallelism is the subject.

if (run_suite("C")) {
  cat("\n== C: the exploratory table-of-tables ==\n")
  rvs <- c("marital", "race", "rincome", "relig", "partyid", "age", "tvhours", "year")
  cvs <- c("race", "marital", "partyid")
  for (nm in c("gss", "gss10")) {
    d <- fx(nm)
    for (p in list(FALSE, 2L, 4L, 8L)) {
      lbl <- if (isFALSE(p)) "serial" else paste0("parallel=", p)
      old <- options(tabxplor.parallel = p)
      bm("C", paste0("8 row_vars x 3 col_vars, ", lbl, " [", nm, "]"),
         tab(d, rvs, cvs, pct = "row", test = TRUE, color = TRUE), nm, group = lbl)
      options(old)
    }
  }
  options(tabxplor.parallel = FALSE)
}

# === SECTION: suite D -- the exports ================================================

if (run_suite("D")) {
  cat("\n== D: exports ==\n")
  g  <- fx("gss")
  t1 <- quiet(tab(g, "rincome", "marital", pct = "row", test = TRUE, color = TRUE))
  tN <- quiet(tab(g, c("marital", "race", "rincome", "relig", "partyid"),
                  c("race", "marital"), pct = "row", test = TRUE, color = TRUE))
  xl <- file.path(tempdir(), "phase22h.xlsx")
  bm("D", "print to console (1 table)",   utils::capture.output(print(t1)), "gss", group = "console")
  bm("D", "tab_html (1 table)",           tab_html(t1), "gss", group = "html")
  bm("D", "tab_html tooltips=FALSE",      tab_html(t1, tooltips = FALSE), "gss", group = "html")
  bm("D", "tab_html (5 row_vars)",        tab_html(tN), "gss", group = "html")
  bm("D", "tab_kable (1 table)",          tab_kable(t1), "gss", group = "kable")
  bm("D", "tab_md (1 table)",             tab_md(t1, print = FALSE), "gss", group = "md")
  bm("D", "tab_xl (1 table)",             tab_xl(t1, path = xl, replace = TRUE, open = FALSE),
     "gss", group = "xl")
  bm("D", "tab_xl (5 row_vars)",          tab_xl(tN, path = xl, replace = TRUE, open = FALSE),
     "gss", group = "xl")
  bm("D", "tab_plot (1 table)",           tab_plot(t1), "gss", group = "plot")
}

# === SECTION: suite E -- regressions ================================================

if (run_suite("E")) {
  cat("\n== E: regressions ==\n")
  g <- fx("gss")
  p3 <- c("rincome", "race", "age")
  p5 <- c("rincome", "race", "age", "relig", "partyid")

  # -- the default binomial table, and what each option adds to it
  bm("E", "binomial, 3 predictors (default)",
     tab_reg(g, outcome = "married", predictors = p3, family = "binomial"), "gss", group = "base")
  bm("E", "binomial, stats='none'",
     tab_reg(g, outcome = "married", predictors = p3, family = "binomial", stats = "none"),
     "gss", group = "stats")
  bm("E", "binomial, stats='all'",
     tab_reg(g, outcome = "married", predictors = p3, family = "binomial", stats = "all"),
     "gss", group = "stats")
  bm("E", "binomial, color='no'",
     tab_reg(g, outcome = "married", predictors = p3, family = "binomial", color = "no"),
     "gss", group = "color")
  bm("E", "binomial + empirical=TRUE",
     tab_reg(g, outcome = "married", predictors = p3, family = "binomial", empirical = TRUE),
     "gss", group = "empirical")
  bm("E", "binomial + empirical + adjustment colour",
     tab_reg(g, outcome = "married", predictors = p3, family = "binomial", empirical = TRUE,
             color = "adjustment"), "gss", group = "empirical")
  bm("E", "binomial, 5 predictors",
     tab_reg(g, outcome = "married", predictors = p5, family = "binomial"), "gss", group = "base")

  # -- the estimand cascade: what each (measure, effect) costs
  bm("E", "binomial measure='ratio' (RR fit)",
     tab_reg(g, outcome = "married", predictors = p3, family = "binomial", measure = "ratio"),
     "gss", group = "estimand")
  bm("E", "binomial measure='difference' (RD fit)",
     tab_reg(g, outcome = "married", predictors = p3, family = "binomial", measure = "difference"),
     "gss", group = "estimand")
  bm("E", "binomial effect='marginal' RD",
     tab_reg(g, outcome = "married", predictors = p3, family = "binomial",
             effect = "marginal", measure = "difference"), "gss", group = "estimand")
  bm("E", "binomial effect='at_reference'",
     tab_reg(g, outcome = "married", predictors = p3, family = "binomial",
             effect = "at_reference", measure = "difference"), "gss", group = "estimand")

  # -- the other families
  bm("E", "gaussian (numeric outcome)",
     tab_reg(g, outcome = "age", predictors = c("rincome", "race", "marital"), family = "gaussian"),
     "gss", group = "family")
  bm("E", "poisson (count outcome)",
     tab_reg(g, outcome = "tvhours", predictors = p3, family = "poisson"), "gss", group = "family")
  bm("E", "multinomial (relig, 13 levels)",
     tab_reg(g, outcome = "relig", predictors = c("race", "age"), family = "multinomial"),
     "gss", group = "family")
  bm("E", "ordinal (rincome, 4 levels)",
     tab_reg(g, outcome = "rincome", predictors = c("race", "age"), family = "ordinal"),
     "gss", group = "family")
  bm("E", "ordinal, stats='none' (no Brant refit)",
     tab_reg(g, outcome = "rincome", predictors = c("race", "age"), family = "ordinal",
             stats = "none"), "gss", group = "family")

  # -- the three nesting axes
  bm("E", "2 outcomes",
     tab_reg(g, outcome = c("married", "tvhours"), predictors = p3,
             family = c("binomial", "poisson")), "gss", group = "axes")
  bm("E", "4 nested models",
     tab_reg(g, outcome = "married", family = "binomial",
             predictors = list(a = "race", b = c("race", "rincome"),
                               c = c("race", "rincome", "age"),
                               d = c("race", "rincome", "age", "relig"))), "gss", group = "axes")
  bm("E", "tab_vars (3 groups)",
     tab_reg(g, outcome = "married", predictors = c("rincome", "age"), family = "binomial",
             tab_vars = "race"), "gss", group = "axes")

  # -- numeric predictors: the shape sparkline and the linearity material
  bm("E", "numeric predictor + sparkline",
     tab_reg(g, outcome = "married", predictors = c("age", "tvhours"), family = "binomial"),
     "gss", group = "shape")
  bm("E", "numeric predictor shape='quartiles'",
     tab_reg(g, outcome = "married", predictors = c("age", "tvhours"), family = "binomial",
             shape = c(age = "quartiles")), "gss", group = "shape")
  bm("E", "numeric predictor, spark='no'", {
     old <- options(tabxplor.spark = "no"); on.exit(options(old), add = TRUE)
     tab_reg(g, outcome = "married", predictors = c("age", "tvhours"), family = "binomial")
     }, "gss", group = "shape")

  # -- the same table on a REAL survey n: where the model fit, not tabxplor, sets the wait
  g10 <- fx("gss10")
  bm("E", "binomial 3 predictors [gss10 215k]",
     tab_reg(g10, outcome = "married", predictors = p3, family = "binomial"), "gss10", group = "size")
  bm("E", "binomial + empirical [gss10 215k]",
     tab_reg(g10, outcome = "married", predictors = p3, family = "binomial", empirical = TRUE),
     "gss10", group = "size")
  bm("E", "binomial effect='marginal' RD [gss10 215k]",
     tab_reg(g10, outcome = "married", predictors = p3, family = "binomial",
             effect = "marginal", measure = "difference"), "gss10", group = "size")
  bm("E", "binomial stats='none' [gss10 215k]",
     tab_reg(g10, outcome = "married", predictors = p3, family = "binomial", stats = "none"),
     "gss10", group = "size")

  # -- the model-check plots (an explicit `check =`, never a default)
  if (requireNamespace("ggplot2", quietly = TRUE) && requireNamespace("gridExtra", quietly = TRUE)) {
    rr <- quiet(tab_reg(g, outcome = "married", predictors = p3, family = "binomial"))
    bm("E", "reg_check_plots(check='auto')", {
      grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
      reg_check_plots(rr, data = g, check = "auto")
    }, "gss", group = "plots")
  }

  # -- a real survey design
  if (requireNamespace("survey", quietly = TRUE)) {
    des <- survey::svydesign(ids = ~1, weights = ~w, data = as.data.frame(g))
    bm("E", "survey design, binomial",
       tab_reg(des, outcome = "married", predictors = p3, family = "binomial"),
       "gss", group = "survey")
  }
}

# === SECTION: suite F -- the jamovi live UI =========================================
# The cost the STUDENT feels: one option changed with the cache warm.

if (run_suite("F") && identical(CFG$engine, "current")) {
  cat("\n== F: jamovi live UI (warm cache) ==\n")
  if (file.exists("tests/testthat/helper-benchmark.R")) {
    source("tests/testthat/helper-benchmark.R")
    for (sz in c("small", "big")) {
      ops <- quiet(if (sz == "small") benchmark_jmvtab_ops() else benchmark_jmvtab_big_ops())
      for (op in names(ops)) {
        f <- ops[[op]]
        bm("F", paste0(op, " [", sz, "]"), f(), "gss", group = sz)
      }
    }
  }
}

# === SECTION: suite G -- the huge-dataframe standard grid ===========================
# The SAME operations as run_bench.R, so this review stays comparable with baseline.csv.

if (run_suite("G")) {
  cat("\n== G: the 8M-row standard grid ==\n")
  big <- fx("big")
  bm("G", "tab_row_pct",    tab(big, region, response, pct = "row"), "big", group = "std")
  bm("G", "tab_ci",         tab(big, region, response, pct = "row", ci = "cell"), "big", group = "std")
  bm("G", "tab_chi2",       tab(big, region, response, pct = "row", test = TRUE), "big", group = "std")
  bm("G", "tab_num_mean",   tab_num(big, region, c(score, income), response, comp = "all"), "big", group = "std")
  bm("G", "tab_num_w",      tab_num(big, region, c(score, income), response, wt = weight, comp = "all"),
     "big", group = "std")
  bm("G", "tab_many_multi", tab(big, region, c(response, age_grp), pct = "row"), "big", group = "std")
  bm("G", "tab_weighted",   tab(big, region, response, wt = weight, pct = "col"), "big", group = "std")
}

# === SECTION: suite Z -- the 1.3.1 <-> 2.0.0 A/B ====================================
# Paired cases, written once per engine. The API bridge: 1.3.1 merges with tab_many(compact = TRUE),
# names the test `chi2 =` and has NO `stars` / `test` / `design_effect`. A pair is only a pair when
# the two calls ask for the SAME table -- which is why `ci` is stated explicitly on both sides.

if (run_suite("Z")) {
  cat("\n== Z: 1.3.1 vs 2.0.0 (same data, same table) ==\n")
  v131 <- identical(CFG$engine, "v131")
  # DESIGN: row and column sets are DISJOINT. 1.3.1's tab_many(compact = FALSE) aborts on a variable
  # that is both ("Names must be unique"), so an overlapping grid has no 1.3.1 half to compare with.
  zrv <- c("marital", "rincome", "relig", "partyid", "year")
  zcv <- c("race", "black", "married")
  for (nm in c("gss", "gss10")) {
    d <- fx(nm); L <- function(x) paste0(x, " [", nm, "]")
    if (v131) {
      bm("Z", L("1 table, counts"),        tab(d, rincome, marital), nm, group = "one")
      bm("Z", L("1 table, pct=row"),       tab(d, rincome, marital, pct = "row"), nm, group = "one")
      bm("Z", L("1 table, pct+chi2+color"),
         tab(d, rincome, marital, pct = "row", chi2 = TRUE, color = "diff"), nm, group = "one")
      bm("Z", L("1 table, pct + ci"),      tab(d, rincome, marital, pct = "row", ci = "cell"), nm, group = "one")
      bm("Z", L("15 tables merged"),
         tab_many(d, zrv, zcv,
                  pct = "row", chi2 = TRUE, color = "diff", compact = TRUE), nm, group = "many")
      bm("Z", L("15 tables, list"),
         tab_many(d, zrv, zcv,
                  pct = "row", chi2 = TRUE, color = "diff", compact = FALSE), nm, group = "many")
      bm("Z", L("numeric means (tab_num)"),
         tab_num(d, race, c(age, tvhours), marital, comp = "all"), nm, group = "num")
      bm("Z", L("weighted col%"),          tab(d, rincome, marital, wt = w, pct = "col"), nm, group = "wt")
    } else {
      bm("Z", L("1 table, counts"),        tab(d, rincome, marital), nm, group = "one")
      bm("Z", L("1 table, pct=row"),       tab(d, rincome, marital, pct = "row", ci = "no"), nm, group = "one")
      bm("Z", L("1 table, pct+chi2+color"),
         tab(d, rincome, marital, pct = "row", ci = "no", test = TRUE, color = "diff"), nm, group = "one")
      bm("Z", L("1 table, pct + ci"),      tab(d, rincome, marital, pct = "row", ci = "cell"), nm, group = "one")
      bm("Z", L("15 tables merged"),
         tab(d, zrv, zcv,
             pct = "row", ci = "no", test = TRUE, color = "diff"), nm, group = "many")
      bm("Z", L("15 tables, list"),
         tab(d, zrv, zcv,
             pct = "row", ci = "no", test = TRUE, color = "diff", output_list = TRUE), nm, group = "many")
      bm("Z", L("numeric means (tab_num)"),
         tab_num(d, race, c(age, tvhours), marital, comp = "all"), nm, group = "num")
      bm("Z", L("weighted col%"),          tab(d, rincome, marital, wt = w, pct = "col", ci = "no"), nm, group = "wt")
      # The 2.0.0 DEFAULT of the same call -- what a user actually gets today, interval included.
      bm("Z", L("1 table, pct=row (2.0.0 defaults)"),
         tab(d, rincome, marital, pct = "row"), nm, group = "one_default")
      bm("Z", L("15 tables merged (2.0.0 defaults)"),
         tab(d, zrv, zcv,
             pct = "row", test = TRUE, color = "diff"), nm, group = "many_default")
    }
  }
}

# === SECTION: suite H -- the cold session ===========================================
# What the FIRST table of a session costs, which is the only number a student ever notices. Measured
# in a fresh Rscript per case, against an INSTALLED build (never load_all, which no user runs).
# `--headlib=` names it; the suite skips itself when that library holds no tabxplor.

if (run_suite("H")) {
  cat("\n== H: the cold session (fresh Rscript, installed build) ==\n")
  headlib <- arg_of("headlib", path.expand("~/R/tabxplorhead"))
  ok <- length(list.files(headlib, pattern = "^tabxplor$")) > 0L
  if (!ok) {
    cat("  (skipped: no tabxplor in ", headlib, ")\n", sep = "")
  } else {
    cold <- function(body) {
      f <- tempfile(fileext = ".R")
      writeLines(c(sprintf('.libPaths(c("%s", .libPaths()))', headlib), body), f)
      function() {
        t <- system.time(system2("Rscript", c("--vanilla", f), stdout = FALSE, stderr = FALSE))
        as.numeric(t[["elapsed"]])
      }
    }
    cold_case <- function(case, body, note = "") {
      f <- cold(body)
      t0 <- f(); ts <- c(t0, f())                       # a process spawn has no warm-up to discard
      RESULTS[[length(RESULTS) + 1L]] <<- data.frame(
        suite = "H", case = case, group = "cold", fixture = "gss",
        median_s = round(stats::median(ts), 4), min_s = round(min(ts), 4),
        reps = 2L, note = note, error = "", stringsAsFactors = FALSE)
      cat(sprintf("  %-8s %-42s %9s\n", "H", case, sprintf("%.4f s", stats::median(ts))))
      flush.console()
    }
    cold_case("R startup only (the floor)", "invisible(NULL)")
    cold_case("+ library(tabxplor)", "suppressMessages(library(tabxplor))")
    cold_case("+ first tab()", c("suppressMessages(library(tabxplor))",
      "g <- gss_cat_data_formatting()",
      'invisible(tab(g, "rincome", "marital", pct = "row", color = TRUE))'))
    cold_case("+ first tab_reg()", c("suppressMessages(library(tabxplor))",
      "g <- gss_cat_data_formatting()",
      'invisible(tab_reg(g, outcome = "married", predictors = c("race","age"), family = "binomial"))'))
    cold_case("+ first tab_xl()", c("suppressMessages(library(tabxplor))",
      "g <- gss_cat_data_formatting()",
      'x <- tab(g, "rincome", "marital", pct = "row", color = TRUE)',
      'invisible(tab_xl(x, path = tempfile(fileext = ".xlsx"), open = FALSE))'))
  }
}

# === SECTION: write =================================================================

out <- do.call(rbind, RESULTS)
out$engine      <- CFG$engine
out$version     <- ENGINE_VERSION
out$tag         <- CFG$tag
out$cores       <- CFG$cores
out$dt_threads  <- dt_threads
out$blas_threads <- blas_threads
out$r_version   <- as.character(getRversion())

dir.create(CFG$outdir, showWarnings = FALSE, recursive = TRUE)
path <- file.path(CFG$outdir, sprintf("phase22h_%s_%s.csv", CFG$tag, CFG$engine))
utils::write.csv(out, path, row.names = FALSE)

cat("\n---------------------------------------------------------------\n")
cat(sprintf("tabxplor %s | engine=%s | tag=%s | cores=%d | dt_threads=%d | blas=%s | R %s\n",
            ENGINE_VERSION, CFG$engine, CFG$tag, CFG$cores, dt_threads,
            if (is.na(blas_threads)) "default" else as.character(blas_threads), getRversion()))
cat(sprintf("%d cases, %d errors, total measured %.1f s\n",
            nrow(out), sum(nzchar(out$error)), sum(out$median_s * out$reps, na.rm = TRUE)))
cat("written: ", path, "\n", sep = "")
