# PURPOSE: Phase 22i -- what it is worth to take `multiplier` out of the jamovi fit-cache key, now
#   that the scaling is applied at reg_tidy_finalize() and `tidy_native` is genuinely native.
# ROLE: MEASURES; it prints, it does not assert. Run it alone (see CLAUDE.md > Testing) and record
#   the output under dev/benchmarks/results_2.0.0/.
# USAGE: Rscript dev/benchmarks/phase22i_multiplier.R [<pkg path>]
#   The path lets the SAME script measure a BEFORE tree, which is how this phase's numbers were
#   taken:  git archive HEAD | tar -x -C <tmp>  &&  Rscript <this> <tmp>
# ⚠ the multinomial arms are seconds each; the whole script runs in a couple of minutes.
# ⚠ NEVER build the options list with utils::modifyList() and an UNNAMED list value (a jamovi Array
#   is one): modifyList recurses and an unnamed list contributes nothing, so the option silently
#   stays at its default and the case measures a re-apply. `set()` below assigns instead.
# See: CLAUDE.md > 2.0.0 roadmap > Phase 22i ; dev/benchmarks/phase22gx_crude.R.

pkg <- (function(a) if (length(a)) a[[1]] else "~/github/tabxplor")(commandArgs(TRUE))
suppressMessages(devtools::load_all(pkg, quiet = TRUE))
cat("package: ", pkg, "\n\n", sep = "")

gss <- gss_cat_data_formatting()

base <- list(
  outcome = "married", predictors = c("race", "age"), wt = character(), tab_vars = NULL,
  effect = "conditional", display = "auto", empirical = FALSE, ref = NULL, conf_level = 0.95,
  ci_method = "wald", stars = TRUE, color = NULL, color_signif = "grey_non_signif",
  na = "drop_by_outcome", cleannames = TRUE, n = "range", subtext = "", digits = 0L,
  family = list(list(var = "married", family = "binomial")), link = list(), outcome_level = list(),
  trials = list(), multiplier = list(), shape = list(), levels_collapse = list(), crosses = list(),
  models = list(), levels_order = list())
set  <- function(o, ...) { for (nm in names(list(...))) o[[nm]] <- list(...)[[nm]]; o }
q    <- function(e) suppressWarnings(suppressMessages(e))
mult <- function(o, k) set(o, multiplier = list(list(var = "age", k = as.character(k))))
tm   <- function(f) { f(); min(replicate(3, system.time(f())[["elapsed"]])) }

# the display strings of every fmt column: two builds are "the same table" iff these match
render <- function(tabs) lapply(if (is.data.frame(tabs)) list(tabs) else tabs, function(t) {
  t <- dplyr::ungroup(t)
  lapply(names(t)[vapply(t, is_fmt, logical(1))], function(nm) format(t[[nm]]))
})

cases <- list(
  "binomial, factors + 1 numeric" = base,
  "binomial, marginal RD"         = set(base, effect = "marginal", measure = "difference"),
  "multinomial, factors + 1 num." = set(base, outcome = "partyid",
                                        family = list(list(var = "partyid",
                                                           family = "multinomial"))),
  "ordinal, factors + 1 numeric"  = set(base, outcome = "rincome",
                                        family = list(list(var = "rincome",
                                                           family = "ordinal"))))

# --- 1. a `multiplier` pick on a WARM store ----------------------------------------------------
# It is a REPORTING choice -- it scales the tidy and cannot move a fit -- so it should be a HIT that
# re-reports. Before Phase 22i it rode in the key's `extra` and every pick was a full refit.
cat("=== a `multiplier` pick on a warm store (k = 10 -> 5) ===\n")
cat(sprintf("%-32s %8s %8s %6s %8s\n", "case", "cold", "pick", "hits", "changed"))
for (nm in names(cases)) {
  o10 <- mult(cases[[nm]], 10); o05 <- mult(cases[[nm]], 5)
  b1  <- q(jmvtab_reg_build(gss, o10, NULL))
  cold <- tm(function() q(jmvtab_reg_build(gss, o10, NULL)))
  pick <- tm(function() q(jmvtab_reg_build(gss, o05, b1$store)))
  b2   <- q(jmvtab_reg_build(gss, o05, b1$store))
  cat(sprintf("%-32s %8.3f %8.3f %6d %8s\n", nm, cold, pick, b2$hits,
              !identical(render(b1$tabs), render(b2$tabs))))
}

# --- 2. the scaling must not move a NUMBER, only its units -------------------------------------
# The estimate scales by k and the SE by |k|, so |est/se| -- and therefore every p-value and every
# star -- is unchanged. This is why no test statistic could move when the rescale relocated.
cat("\n=== the p-value is scale-invariant (max |dp| over every model row) ===\n")
for (nm in names(cases)) {
  o1 <- mult(cases[[nm]], 1); o7 <- mult(cases[[nm]], 7)
  p  <- function(o) { tt <- q(jmvtab_reg_build(gss, o, NULL))$tabs
                      t <- if (is.data.frame(tt)) tt else tt[[1]]
                      unlist(lapply(names(t)[vapply(t, is_fmt, logical(1))],
                                    function(nm) get_pvalue(t[[nm]]))) }
  a <- p(o1); b <- p(o7)
  cat(sprintf("%-32s %.3e\n", nm, max(abs(a - b), na.rm = TRUE)))
}
