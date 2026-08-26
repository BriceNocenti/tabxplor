# phase19m3_measurements.R -- Phase 19m-iii: the three measurements 19j and 19k asked for and 19l
# and 19m-i both deferred. It CHANGES NOTHING and decides nothing: each number closes an open
# question in the roadmap, and any optimisation that follows is a later phase's.
#
#   1. The per-col_var agg_chi2() cost (19j). Since 19j the factor arm calls chi2_compute_test()
#      once PER col_var (leaf_chi2, singular `col_var`) where the superseded tab_chi2() batched
#      every col_var into one agg_chi2() pass. What does the un-batching cost?
#   2. The reg fit-cache digest path (19k). `color = "adjustment"` and any `shape` need the fitted
#      object, so neither can take the KB-sized digest fast path: a reference change REFITS. Both
#      were previously unreachable from the UI because the options did not exist, so 19k added a
#      real new cost on those two paths and did not measure it.
#   3. 19d's unconditional odds ratio, on a WIDE table. 19d measured +16 ms on a 216 ms 3x2 build
#      (~7 %) and flagged "re-measure wide before release". The question is not the absolute cost
#      but whether the SHARE grows with the number of col_vars.
#
# USAGE (unsandboxed, nothing else running -- see CLAUDE.md's orphan/oversubscription warnings):
#   OMP_NUM_THREADS=1 Rscript dev/benchmarks/phase19m3_measurements.R \
#     > dev/benchmarks/results_2.0.0/phase19m3.txt
#
# Public API + system.time only, so it runs unchanged on any tree. forcats::gss_cat is the repo's
# standard fixture (dev/verify_color_attrs.R, dev/make_golden.R): marital (6), race (4), partyid
# (10), relig (16), denom (~30) give col_vars of varying width, and `year` is a ready-made tab_var.

suppressMessages(pkgload::load_all("~/github/tabxplor", export_all = TRUE, helpers = FALSE,
                                   quiet = TRUE))
data.table::setDTthreads(1L)
set.seed(1)

timeit <- function(expr, reps = 5L) {
  expr <- substitute(expr); env <- parent.frame()
  eval(expr, env)                                            # warm up (JIT, first-call setup)
  stats::median(replicate(reps, system.time(eval(expr, env))[["elapsed"]]))
}
ms  <- function(x) sprintf("%7.1f ms", x * 1000)
hdr <- function(...) cat("\n", paste0(...), "\n", strrep("-", 92), "\n", sep = "")

d <- forcats::gss_cat
cat("tabxplor phase 19m-iii measurements --", R.version.string, "\n")
cat("fixture: forcats::gss_cat,", nrow(d), "rows\n")


# =====================================================================================================
hdr("1. THE PER-col_var agg_chi2() COST (19j)")
# =====================================================================================================
# TIER 1 -- agg_chi2() in isolation: one call carrying K*S table_ids against K calls carrying S each,
# with the SAME total number of cells. This is the pure batching delta, with no pipeline around it,
# and it is the honest headline: a tier-3 head-to-head against tab_chi2() would credit 19j with
# savings that have nothing to do with batching (it also deleted six table-reconstruction passes).
cat("TIER 1 -- agg_chi2() alone, same total cells, batched vs one call per col_var\n\n")
cat(sprintf("  %-4s %-4s %-5s %11s %11s %11s %8s\n",
            "K", "S", "cells", "batched", "K calls", "delta", "per call"))
for (K in c(1L, 2L, 4L, 8L, 16L)) for (S in c(1L, 5L)) {
  R_ <- 6L; C_ <- 5L                                          # rows x cols of each little table
  one <- expand.grid(row_id = seq_len(R_), col_id = seq_len(C_), s = seq_len(S))
  mk  <- function(k) transform(one, table_id = paste(k, one$s, sep = "\r"),
                               o = stats::rpois(nrow(one), 40) + 1)
  parts <- lapply(seq_len(K), mk)
  all1  <- do.call(rbind, parts)
  tb <- timeit(agg_chi2(all1$table_id, all1$row_id, all1$col_id, all1$o), reps = 11L)
  tk <- timeit(for (p in parts) agg_chi2(p$table_id, p$row_id, p$col_id, p$o), reps = 11L)
  cat(sprintf("  %-4d %-4d %-5d %11s %11s %11s %8s\n", K, S, nrow(all1),
              ms(tb), ms(tk), ms(tk - tb), ms((tk - tb) / max(K - 1L, 1L))))
}

# TIER 3 -- what a user actually pays: the same table with and without the whole-table test. The
# leaf takes test = "no" when it is off, so the ONLY thing toggling is leaf_chi2().
cat("\nTIER 3 -- end-to-end tab(): test = TRUE minus test = FALSE, sweeping the col_var count\n\n")
cvs <- c("race", "partyid", "relig", "marital", "denom", "rincome", "race", "partyid")
cat(sprintf("  %-4s %-6s %11s %11s %11s %8s\n", "K", "tabvar", "test=FALSE", "test=TRUE", "delta", "per cv"))
for (K in c(1L, 2L, 4L, 8L)) for (tv in c(FALSE, TRUE)) {
  sel <- cvs[seq_len(K)]
  f <- if (tv) function(te) tab(d, marital, tidyselect::all_of(sel), tab_vars = year, pct = "row", test = te)
       else    function(te) tab(d, marital, tidyselect::all_of(sel), pct = "row", test = te)
  t0 <- timeit(f(FALSE), reps = 5L); t1 <- timeit(f(TRUE), reps = 5L)
  cat(sprintf("  %-4d %-6s %11s %11s %11s %8s\n", K, if (tv) "year" else "-",
              ms(t0), ms(t1), ms(t1 - t0), ms((t1 - t0) / K)))
}


# =====================================================================================================
hdr("2. THE REG FIT-CACHE DIGEST PATH (19k)")
# =====================================================================================================
# A reference change on a warm cache. The digest fast path recomputes the coefficients by contrast
# from a KB-sized digest (reg_reref_fit_res, byte-identical to a refit); `color = "adjustment"` needs
# the crude leg's influence function and `shape` changes the model matrix, so both must REFIT.
dd <- d[!is.na(d$age) & !is.na(d$tvhours) & d$marital != "No answer", ]
dd$y <- as.integer(dd$rincome %in% c("$25000 or more", "$20000 - 24999"))
cases <- list(
  "plain (digest path)"  = list(),
  "color = adjustment"   = list(color = c(TRUE, "adjustment"), empirical = TRUE),
  "shape = quadratic"    = list(shape = c(age = "quadratic"))
)
cat(sprintf("  %-22s %12s %12s %12s\n", "case", "1st build", "re-ref", "vs plain"))
base_reref <- NA_real_
for (nm in names(cases)) {
  st  <- jmvreg_cache_new()
  env <- jmvreg_cache_env(st)
  call1 <- function(ref) do.call(tab_reg, c(list(
    data = dd, outcome = "y", predictors = c("marital", "race", "age"),
    family = "binomial", reference = ref, .fit_cache = env), cases[[nm]]))
  t1 <- timeit(call1(NULL), reps = 3L)
  t2 <- timeit(call1(c(marital = "Divorced")), reps = 3L)   # a reference change, cache warm
  if (is.na(base_reref)) base_reref <- t2
  cat(sprintf("  %-22s %12s %12s %12s\n", nm, ms(t1), ms(t2),
              if (nm == names(cases)[1]) "--" else sprintf("x%.1f", t2 / base_reref)))
}


# =====================================================================================================
hdr("3. 19d's UNCONDITIONAL ODDS RATIO, ON A WIDE TABLE")
# =====================================================================================================
# Since 19d, tab_apply_reference() computes `or`/`rr` in the SAME matrix sweep that produces `diff`
# and `ratio`, on every row/col-percentage table. There is no switch to turn it off, so the honest
# measurement is Rprof's share of the whole build spent in that function -- an UPPER BOUND on the
# odds ratio's own cost, since the sweep also does the work that was always there. What matters is
# whether the share GROWS with width.
prof_share <- function(expr, fns) {
  f <- tempfile(fileext = ".Rprof")
  utils::Rprof(f, interval = 0.01, line.profiling = FALSE)   # 10 ms = this platform's floor
  for (i in 1:5) force(expr)
  utils::Rprof(NULL)
  s <- tryCatch(summaryRprof(f)$by.total, error = function(e) NULL)
  unlink(f)
  if (is.null(s) || !nrow(s)) return(stats::setNames(rep(NA_real_, length(fns)), fns))
  tot <- max(s$total.time)
  vapply(fns, function(fn) {
    r <- s[rownames(s) == paste0("\"", fn, "\""), , drop = FALSE]
    if (!nrow(r)) 0 else 100 * r$total.time[[1]] / tot
  }, numeric(1))
}
cat(sprintf("  %-4s %11s %14s %14s\n", "K", "build", "apply_reference", "ci_or (Woolf)"))
for (K in c(1L, 2L, 4L, 8L)) {
  sel <- cvs[seq_len(K)]
  bld <- function() tab(d, marital, tidyselect::all_of(sel), pct = "row", display = "{or}",
                        ref = "first", stars = TRUE)
  tt <- timeit(bld(), reps = 5L)
  sh <- prof_share(bld(), c("tab_apply_reference", "ci_or"))
  cat(sprintf("  %-4d %11s %13.1f%% %13.1f%%\n", K, ms(tt), sh[["tab_apply_reference"]], sh[["ci_or"]]))
}

cat("\nNOTE: Rprof samples at 10 ms over 5 builds -- read the TREND across K, not the digit.\n")
cat("done.\n")
