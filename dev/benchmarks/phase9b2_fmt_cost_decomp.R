#!/usr/bin/env Rscript
# PURPOSE: Phase 9b-2 THROWAWAY measurement spike -- decompose a tab() call's per-table build
#          cost to decide the Phase 9b-3 go/no-go (deferred fmt materialization). It answers ONE
#          question: of the O(cells) tabxplor_fmt machinery that §29 pins at ~99% of tab(), how
#          much is the IRREDUCIBLE build-once (new_fmt of the final columns) vs the RECOVERABLE
#          reconstruction/writers paid through join / slice / rbind / set_* between the leaf and
#          the final table (which 9b-3 removes by materializing once at the end)?
# ROLE: Standalone, dev/benchmarks/ (.Rbuildignore'd), NEVER run by R CMD check. Makes NO change
#        to R/*.R -- it only times the existing pipeline (Parts 1-2) and a plain-vs-fmt micro-model
#        (Part 3). The finding: 9b-2's plain-field CI/chi2 writers are ~a no-op on the common
#        color="diff" path (tab_ci does not run; tab_chi2 writes no records), so the win is 9b-3.
#
# USAGE (from package root, in a dev R session or headless):
#   Rscript dev/benchmarks/phase9b2_fmt_cost_decomp.R
#   # or:  source("dev/benchmarks/phase9b2_fmt_cost_decomp.R", encoding = "UTF-8")
#
# Deterministic (forcats::gss_cat + fixed options, no RNG). Uses bench::mark when available
# (median wall + MB), else system.time. Rprof at a fine interval over an accumulation loop gives
# auditable self-time attribution; the raw top-N by.self rows are printed so the bucketing is
# checkable. Compose dev/benchmarks/results_1.4.0/phase9b2_decomposition.txt from this output.

pkg <- normalizePath(".", winslash = "/")
suppressMessages(devtools::load_all(pkg, quiet = TRUE))
source(file.path(pkg, "tests", "testthat", "helper-benchmark.R"))  # benchmark_measure()

gss <- forcats::gss_cat
q   <- function(x) suppressWarnings(suppressMessages(x))            # timing, not a correctness check
N_ROWVARS <- 5L

# --- Fixtures: 5 row_vars x the four representative shapes -------------------------------------
# Each is a 0-arg thunk producing the MERGED tab(); a `_list` variant sets output_list = TRUE so
# tab_compact() (the merge) is skipped -> isolates the per-row_var build (the 9b-3 target).
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
fixtures <- names(fx_call)

cat("\n=============================================================================\n")
cat("Phase 9b-2 -- fmt-materialization cost decomposition (throwaway spike)\n")
cat("Fixture DB: forcats::gss_cat (", nrow(gss), " rows) ; 5 row_vars\n", sep = "")
cat("R ", as.character(getRversion()), " | bench: ",
    requireNamespace("bench", quietly = TRUE), "\n", sep = "")
cat("=============================================================================\n")

# ============================================================================================
# PART 1 -- whole-call vs no-merge (output_list) : the per-table build floor + merge share
# ============================================================================================
cat("\n## PART 1 -- per-table build floor + merge share (median wall seconds)\n\n")
p1 <- lapply(fixtures, function(fx) {
  merged <- benchmark_measure(function() fx_call[[fx]](FALSE), iterations = 5L)
  listed <- benchmark_measure(function() fx_call[[fx]](TRUE),  iterations = 5L)
  data.frame(
    fixture        = fx,
    merge_s        = round(merged$median_s, 4),
    list_s         = round(listed$median_s, 4),
    merge_delta_s  = round(merged$median_s - listed$median_s, 4),
    per_table_s    = round(listed$median_s / N_ROWVARS, 4),
    merge_mb       = round(merged$mem_mb, 1),
    list_mb        = round(listed$mem_mb, 1),
    stringsAsFactors = FALSE
  )
})
p1 <- do.call(rbind, p1)
print(p1, row.names = FALSE)
cat("\n  merge_s      = default tab() (output = 'single', runs tab_compact)\n")
cat("  list_s       = tab(output_list = TRUE) (no merge) = the per-row_var build sum\n")
cat("  merge_delta  = merge cost (leaf build cancels)     per_table_s = list_s / 5\n")

# ============================================================================================
# PART 2 -- Rprof by.TOTAL curated markers of the NO-MERGE build (per fixture)
# ============================================================================================
# Self-time bucketing is unusable here: the real fmt/vctrs/data.table work runs in compiled code
# (.Call / .External2) and anonymous closures, so >75% of SELF-time lands in un-attributable
# "other". by.TOTAL of a curated set of MARKER functions captures the C callees under each and is
# the robust view. Markers overlap by nesting (new_rcrd sits inside tab_plain, vec_case_when inside
# mutate) -- they are read INDIVIDUALLY, never summed. The two go/no-go markers:
#   new_rcrd  = the record MATERIALIZATION share (the irreducible build-once 9b-3 keeps);
#   fmt-primitive self-time (vec_case_when / vec_slice / list_unchop / vec_restore / field / structure
#   / new_data_frame / df_list) = a LOWER BOUND on fmt-record machinery (self-time never double-
#   counts; more hides inside .Call). The raw top-N by.self is printed for audit.
markers <- c(
  "leaf tab_plain"        = "^tab_plain$",
  "leaf tab_num"          = "^tab_num$",
  "tab_apply_tests"       = "^tab_apply_tests$",
  "tab_ci (CI writers)"   = "^tab_ci$",
  "tab_chi2"              = "^tab_chi2$",
  "new_fmt"               = "^new_fmt$",
  "new_rcrd (materialize)"= "new_rcrd",
  "vec_case_when"         = "vec_case_when",
  "reduce/full_join"      = "full_join",
  "vec_slice*"            = "vec_slice",
  "list_unchop"           = "list_unchop",
  "vec_restore*"          = "vec_restore",
  "[.data.table (agg)"    = "\\[\\.data\\.table"
)
# fmt-record primitives whose SELF-time is a non-double-counting lower bound on fmt machinery.
fmt_prim <- "new_rcrd|new_fmt|new_data_frame|df_list|structure|vec_case_when|vec_slice|vec_c$|list_unchop|vec_restore|vec_proxy|vctrs::field|field<-|vec_cast|vec_recycle"

rprof_attribution <- function(fx, target_secs = 6) {
  one <- benchmark_measure(function() fx_call[[fx]](TRUE), iterations = 3L)$median_s
  reps <- max(3L, min(300L, ceiling(target_secs / max(one, 1e-3))))
  tmp  <- tempfile(fileext = ".Rprof")
  fn   <- fx_call[[fx]]
  utils::Rprof(tmp, interval = 0.003, line.profiling = FALSE, memory.profiling = FALSE)
  for (i in seq_len(reps)) fn(TRUE)
  utils::Rprof(NULL)
  s <- utils::summaryRprof(tmp)
  unlink(tmp)
  bt  <- s$by.total; bt$fn <- rownames(bt)
  bs  <- s$by.self;  bs$fn <- rownames(bs)
  tot <- s$sampling.time
  # by.total for each marker: the OUTERMOST matching frame (max total.time) to avoid nested dupes.
  mk <- lapply(markers, function(pat) {
    hit <- grepl(pat, bt$fn)
    if (!any(hit)) return(c(t = 0, p = 0))
    i <- which.max(bt$total.time[hit])
    c(t = bt$total.time[hit][i], p = bt$total.pct[hit][i])
  })
  prim_self <- sum(bs$self.time[grepl(fmt_prim, bs$fn)])
  list(fx = fx, reps = reps, one_s = one, sampling_s = tot,
       samples = round(tot / 0.003), mk = mk, prim_self = prim_self,
       top = utils::head(bs[order(-bs$self.time), ], 18))
}

cat("\n## PART 2 -- Rprof by.total curated markers of the no-merge build\n")
p2 <- lapply(fixtures, rprof_attribution)
names(p2) <- fixtures
for (r in p2) {
  cat("\n--- ", r$fx, " : ", r$reps, " reps, ~", r$samples, " samples, ",
      round(r$sampling_s, 2), "s sampled ---\n", sep = "")
  tbl <- data.frame(marker = names(markers),
                    total_pct = round(vapply(r$mk, function(x) unname(x["p"]), numeric(1)), 1),
                    stringsAsFactors = FALSE)
  print(tbl, row.names = FALSE)
  cat("  fmt-primitive self-time (lower bound on fmt machinery) = ",
      round(100 * r$prim_self / r$sampling_s, 1), "% of the build\n", sep = "")
  cat("  -- top self-time functions (audit) --\n")
  top <- data.frame(fn = r$top$fn, self_pct = r$top$self.pct, stringsAsFactors = FALSE)
  print(top, row.names = FALSE)
}

# ============================================================================================
# PART 3 -- micro cross-check : materialize-once floor vs reconstruct-through-ops (ESTIMATE)
# ============================================================================================
# Take ONE real per-table result (single row_var). Compare, on its fmt columns:
#  (A) materialize-ONCE floor  = rebuild all columns via new_fmt from plain field-frames, once.
#  (B) reconstruct-through-ops = push each fmt column through K vec_c/vec_slice round-trips
#      (what join/rbind/slice cost when the fields ride inside records), vs the SAME K rounds on
#      the underlying plain data.frames + one final materialization.
# (B_fmt - B_plain) / per_table_build is a rough estimate of the deferred-materialization win;
# it is a MICRO-MODEL of the pipeline's fmt-op cost, not the full pipeline. Cross-check vs Part 2.
cat("\n## PART 3 -- micro cross-check (materialize-once floor vs reconstruct round-trips)\n\n")

t1        <- q(tab(gss, marital, c(race, marital, partyid), pct = "row", color = "diff", chi2 = TRUE))
is_fmt    <- get("is_fmt", envir = asNamespace("tabxplor"))
new_fmt   <- get("new_fmt", envir = asNamespace("tabxplor"))
fmt_cols  <- t1[vapply(t1, is_fmt, logical(1))]
n_col     <- length(fmt_cols)
n_row     <- nrow(t1)

# Plain field-frames (exactly today's vec_data) + the 9 scalar attributes, for a real re-materialize.
attr_names <- c("type", "comp_all", "ref", "ci_type", "col_var", "totcol", "refcol",
                "color", "color_signif")
frames <- lapply(fmt_cols, function(cl) as.list(vctrs::vec_data(cl)))
metas  <- lapply(fmt_cols, function(cl) attributes(cl)[attr_names])
rematerialize_all <- function() {
  for (j in seq_len(n_col)) do.call(new_fmt, c(frames[[j]], metas[[j]]))
  invisible(NULL)
}

K <- 6L  # number of reconstruct rounds (join/rbind/slice-like ops between leaf and final)
recon_fmt <- function() {
  for (j in seq_len(n_col)) {
    z <- fmt_cols[[j]]
    for (k in seq_len(K)) z <- vctrs::vec_slice(vctrs::vec_c(z, z), seq_len(n_row))
  }
  invisible(NULL)
}
recon_plain <- function() {
  for (j in seq_len(n_col)) {
    d <- frames[[j]]
    for (k in seq_len(K)) d <- lapply(d, function(col) c(col, col)[seq_len(n_row)])
    do.call(new_fmt, c(d, metas[[j]]))   # materialize ONCE at the end
  }
  invisible(NULL)
}

floor_s <- benchmark_measure(rematerialize_all, iterations = 30L)$median_s
bf      <- benchmark_measure(recon_fmt,   iterations = 30L)$median_s
bp      <- benchmark_measure(recon_plain, iterations = 30L)$median_s
per_tab <- p1$per_table_s[p1$fixture == "common"]

cat("  representative table: ", n_row, " rows x ", n_col, " fmt columns\n", sep = "")
cat("  materialize-once floor (new_fmt x ", n_col, " cols)        : ",
    round(floor_s * 1000, 2), " ms\n", sep = "")
cat("  reconstruct ", K, " rounds -- fmt records                    : ",
    round(bf * 1000, 2), " ms\n", sep = "")
cat("  reconstruct ", K, " rounds -- plain frames + 1 materialize   : ",
    round(bp * 1000, 2), " ms\n", sep = "")
cat("  reconstruct overhead removed by deferral (fmt - plain): ",
    round((bf - bp) * 1000, 2), " ms  (fmt/plain ratio = ",
    round(bf / bp, 2), "x)\n", sep = "")
cat("  common per_table build (Part 1)                        : ",
    round(per_tab * 1000, 2), " ms\n", sep = "")

cat("\n=============================================================================\n")
cat("READING: fmt_machinery% (Part 2) is what a fully-deferred plain pipeline replaces with\n")
cat("data.table ops + ONE new_fmt. RECOVERABLE% = reconstruct+writers (build-once stays).\n")
cat(">25% recoverable on 'common' -> greenlight 9b-3. writers% on ci/contrib -> whether a\n")
cat("committable 9b-2 (plain tab_ci/tab_chi2 writers) is a worthwhile separate rung.\n")
cat("=============================================================================\n")
