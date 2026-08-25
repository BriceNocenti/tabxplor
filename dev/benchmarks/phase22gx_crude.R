# PURPOSE: Phase 22g-x -- what the CRUDE block's own cache is worth in the jamovi Regressions panel,
#   and what a level reorder costs once the order is display rather than a relevel of the data.
# ROLE: MEASURES; it prints, it does not assert. Run it alone (see CLAUDE.md > Testing) and record
#   the output under dev/benchmarks/results_2.0.0/.
#   It is the REPRODUCIBLE version of the "of which crude" table Phase 22j's scoping quoted: that
#   column was an ad-hoc `empirical` difference, and phase22j_digest.R runs everything with
#   `empirical = FALSE`, so nothing in the repo could re-derive it.
# USAGE: Rscript dev/benchmarks/phase22gx_crude.R [<pkg path>]
#   The path lets the SAME script measure a BEFORE tree, which is how the phase's own numbers were
#   taken:  git archive HEAD | tar -x -C <tmp>  &&  Rscript <this> <tmp>
# ⚠ the multinomial arms are tens of seconds each; the whole script runs in a few minutes.
# ⚠ NEVER build an options list with utils::modifyList() and an UNNAMED list value (a jamovi Array
#   is one): modifyList recurses into two lists and an unnamed one contributes nothing, so the option
#   silently stays at its default and the case measures a re-apply. `set()` below assigns instead.
# See: CLAUDE.md > 2.0.0 roadmap > Phase 22g-x ; dev/benchmarks/phase22j_digest.R.

pkg <- (function(a) if (length(a)) a[[1]] else "~/github/tabxplor")(commandArgs(TRUE))
suppressMessages(devtools::load_all(pkg, quiet = TRUE))
cat("package: ", pkg, "\n\n", sep = "")

gss <- gss_cat_data_formatting()
set.seed(1)
gss$age2 <- gss$age + stats::runif(nrow(gss), -3, 3)   # a third numeric, so the 3-numeric case exists

# the jamovi options list, in the shape .opts() sends. ⚠ do NOT copy phase22j_digest.R's: it still
# carries the stats_compare / stats_baseline / stats_checks options Phase 22g-iii deleted.
base <- list(
  outcome = "married", predictors = c("race", "age", "relig"), wt = character(), tab_vars = NULL,
  effect = "conditional", display = "auto", empirical = TRUE, ref = NULL, conf_level = 0.95,
  ci_method = "wald", stars = TRUE, color = NULL, color_signif = "grey_non_signif",
  na = "drop_by_outcome", cleannames = TRUE, n = "range", subtext = "", digits = 0L,
  family = list(list(var = "married", family = "binomial")), link = list(), outcome_level = list(),
  trials = list(), multiplier = list(), shape = list(), levels_collapse = list(), crosses = list(),
  models = list(), levels_order = list())
set <- function(o, ...) { for (nm in names(list(...))) o[[nm]] <- list(...)[[nm]]; o }
q   <- function(e) suppressWarnings(suppressMessages(e))
tm  <- function(f) { f(); min(replicate(3, system.time(f())[["elapsed"]])) }
kb  <- function(x) as.numeric(object.size(x)) / 1024
served <- function(op) { st <- q(jmvtab_reg_build(gss, op, NULL))$store
                         list(t = tm(function() q(jmvtab_reg_build(gss, op, st))), st = st) }

cases <- list(
  "binomial, factors + 1 numeric"    = base,
  "binomial, 3 numeric predictors"   = set(base, predictors = c("age", "tvhours", "age2")),
  "binomial, marginal RD"            = set(base, effect = "marginal", measure = "difference"),
  "multinomial, factors + 1 numeric" = set(base, outcome = "partyid",
                                           family = list(list(var = "partyid",
                                                              family = "multinomial"))),
  "multinomial, marginal RD"         = set(base, outcome = "partyid", effect = "marginal",
                                           measure = "difference",
                                           family = list(list(var = "partyid",
                                                              family = "multinomial"))))

# --- 1. what the crude block still costs a SERVED build ----------------------------------------
cat("=== the crude share of a served build (empirical TRUE vs FALSE, both on a warm store) ===\n")
cat(sprintf("%-36s %8s %8s %8s %6s %8s\n", "case", "emp=T", "emp=F", "crude", "share", "store KB"))
for (nm in names(cases)) {
  a <- served(cases[[nm]])
  b <- served(set(cases[[nm]], empirical = FALSE))
  cat(sprintf("%-36s %8.3f %8.3f %8.3f %5.0f%% %8.1f\n",
              nm, a$t, b$t, a$t - b$t, 100 * (a$t - b$t) / a$t, kb(a$st)))
}

# --- 2. the store: how many records ------------------------------------------------------------
cat("\n=== the store: one record per model + one per NON-SATURATED crude predictor ===\n")
for (nm in names(cases)) {
  st <- q(jmvtab_reg_build(gss, cases[[nm]], NULL))$store
  cat(sprintf("%-36s %2d records %8.1f KB\n", nm, length(st[["fit"]]), kb(st)))
}

# --- 3. one interaction, on a warm store -------------------------------------------------------
# every var-table pick writes an option that is in .opts(), so it RE-RUNS the analysis: the
# drop-down latency IS that re-run, not the widget (a `ref` pick's own handler is a bare option
# write, and the level fetch is cached per variable and never repeated).
cat("\n=== one interaction, on a warm store ===\n")
ord_disp <- list(list(var = "race", levels = c("White", "Other", "Black")))
ord_base <- list(list(var = "race", levels = c("Black", "White", "Other")))
for (nm in c("binomial, factors + 1 numeric", "multinomial, factors + 1 numeric")) {
  bs <- cases[[nm]]
  st <- q(jmvtab_reg_build(gss, bs, NULL))$store
  cat(sprintf("-- %s\n%-36s %8s %6s\n", nm, "interaction", "s", "hits"))
  inter <- list(
    "re-apply"                = bs,
    "measure + effect"        = set(bs, effect = "marginal", measure = "difference"),
    "reorder (not the first)" = set(bs, levels_order = ord_disp),
    "reorder onto the first"  = set(bs, levels_order = ord_base, ref = c(race = "Black")),
    "multiplier"              = set(bs, multiplier = list(list(var = "age", k = "1"))),
    "conf_level"              = set(bs, conf_level = 0.90))
  for (k in names(inter)) {
    r <- q(jmvtab_reg_build(gss, inter[[k]], st))
    cat(sprintf("%-36s %8.3f %6d\n", k,
                tm(function() q(jmvtab_reg_build(gss, inter[[k]], st))), r$hits))
  }
}
