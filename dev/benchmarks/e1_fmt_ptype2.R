# e1_fmt_ptype2.R -- Phase 19a / E1: the perf guard of the rule-driven fmt reconstructors.
#
# WHY. `vec_ptype2.tabxplor_fmt.tabxplor_fmt()` is the hottest fmt call site in the package: the
# tab_compact() merge runs a per-column `vec_ptype_common()` reduce, which IS the whole tab() merge
# marginal (dev/tabxplor_2.0.0_decisions.md 30, and the Phase-9c comment in fmt_class.R that records
# why the 9 dplyr::if_else were replaced by base `if`). E1 replaces the 7 hand-written
# 14-attribute enumerations with one declared rule table + index-driven loops, so this file proves
# the change is not paid for in the merge.
#
# HOW TO USE IT, around E1:
#   1. BEFORE editing:  Rscript dev/benchmarks/e1_fmt_ptype2.R > dev/benchmarks/results_2.0.0/e1_before.txt
#   2. AFTER  editing:  Rscript dev/benchmarks/e1_fmt_ptype2.R > dev/benchmarks/results_2.0.0/e1_after.txt
#   3. diff them. The MICRO numbers explain; the END-TO-END compact merge at the bottom is the one
#      that decides. A regression there is a blocker, whatever the micro says.
#
# It writes nothing and builds no fixture: `forcats::gss_cat` is enough (the attribute reconcile is
# O(1) in the number of rows -- it is called once per column pair, not once per cell).

suppressMessages(pkgload::load_all("~/github/tabxplor", export_all = TRUE, helpers = FALSE,
                                  quiet = TRUE))
stopifnot(requireNamespace("bench", quietly = TRUE))
data.table::setDTthreads(1L)

t <- tab(forcats::gss_cat, marital, race, pct = "row", color = TRUE, ci = "diff", test = TRUE)
x <- t[["Black"]]
y <- t[["White"]]                                   # (ii) differing attributes (col_var is the same)
# force the three inference attributes apart too, so the weakest-claim rules are exercised
x <- tabxplor:::set_conf_level(tabxplor:::set_basis(tabxplor:::set_degf(x, 13), "design"), 0.99)
xx <- x                                             # (i) identical attributes

cat("=== micro: the vctrs entry points ===\n")
bm <- bench::mark(
  ptype2_same   = vctrs::vec_ptype2(x, xx),
  ptype2_diff   = vctrs::vec_ptype2(x, y),
  ptype_common4 = vctrs::vec_ptype_common(x, y, xx, y),   # the tab_compact() reduce shape
  cast_fmt_fmt  = vctrs::vec_cast(x, y),
  cast_double   = vctrs::vec_cast(as.double(seq_along(x)), y),
  cast_integer  = vctrs::vec_cast(seq_along(x), y),
  c_two_cols    = vctrs::vec_c(x, y),
  arith_plus    = suppressWarnings(x + y),
  math_sum      = sum(x),
  math_mean     = mean(x),
  check = FALSE, iterations = 4000, filter_gc = FALSE
)
print(bm[, c("expression", "median", "itr/sec", "mem_alloc")], n = 20)

cat("\n=== end-to-end: the compact merge (THE guard) ===\n")
gss <- forcats::gss_cat
print(bench::mark(
  merge_2x3 = tab(gss, c(marital, relig), c(race, partyid, year), pct = "row", color = TRUE),
  check = FALSE, iterations = 10, filter_gc = FALSE
)[, c("expression", "median", "mem_alloc")])

cat("\nR ", as.character(getRversion()), " | ", R.version$platform, "\n", sep = "")
