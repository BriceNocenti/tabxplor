# PURPOSE: Phase 22j -- what the fit digest is worth in the jamovi Regressions panel.
# ROLE: MEASURES; it prints, it does not assert. Run it alone (see CLAUDE.md > Testing) and record
#   the output under dev/benchmarks/results_2.0.0/.
#   Reads: the store's SIZE (the freeze this phase removed), which UI interactions are cache HITS
#   now that the key carries no estimand, and the one case a cache genuinely earns -- a multinomial,
#   where a `measure` / `effect` change used to refit.
# ⚠ the multinomial cold arm is ~15 s x 3 reps; the whole script runs in a few minutes.
# See: dev/tabxplor_2.0.0_performance_review.md section 8.5 ; CLAUDE.md > Phase 22j.

suppressMessages(devtools::load_all("~/github/tabxplor", quiet = TRUE))
gss <- gss_cat_data_formatting()
base <- list(outcome="married", predictors=c("race","age","relig"), wt=character(), tab_vars=NULL,
  effect="conditional", display="auto", empirical=FALSE, ref=NULL, conf_level=0.95,
  ci_method="wald", stars=TRUE, color=NULL, color_signif="grey_non_signif",
  na="drop_by_outcome", cleannames=TRUE, n="range", subtext="",
  stats_compare="none", stats_baseline=1L, stats_checks=FALSE,
  family=list(list(var="married", family="binomial")), link=list(), outcome_level=list(),
  trials=list(), multiplier=list(), shape=list(), levels_collapse=list(), crosses=list(), models=list())
o <- function(...) utils::modifyList(base, list(...))
tm <- function(f) { f(); min(replicate(3, system.time(f())[["elapsed"]])) }
q <- function(e) suppressWarnings(suppressMessages(e))

b <- q(jmvtab_reg_build(gss, o(), NULL)); st <- b$store
cat(sprintf("store: %.1f KB  (was MB-sized)\n", as.numeric(object.size(st))/1024))
cat(sprintf("%-34s %7s %7s\n", "interaction", "s", "hits"))
for (nm in c("re-apply", "display", "colour", "conf_level", "measure+effect", "reference")) {
  opts <- switch(nm,
    "re-apply"       = o(),
    "display"        = o(display = "est_ci"),
    "colour"         = o(color = "no"),
    "conf_level"     = o(conf_level = 0.90),
    "measure+effect" = o(effect = "marginal", measure = "difference"),
    "reference"      = o(ref = c(race = "Black")))
  r <- q(jmvtab_reg_build(gss, opts, st))
  t <- tm(function() q(jmvtab_reg_build(gss, opts, st)))
  cat(sprintf("%-34s %7.3f %7d\n", nm, t, r$hits))
}
cat(sprintf("%-34s %7.3f %7s\n", "cold (no store)", tm(function() q(jmvtab_reg_build(gss, o(), NULL))), "-"))
# multinomial: the case a cache actually earns
m <- o(outcome = "partyid", predictors = c("race","age","relig"),
       family = list(list(var = "partyid", family = "multinomial")))
bm <- q(jmvtab_reg_build(gss, m, NULL)); stm <- bm$store
cat(sprintf("\nmultinomial cold      %7.3f s\n", tm(function() q(jmvtab_reg_build(gss, m, NULL)))))
r <- q(jmvtab_reg_build(gss, utils::modifyList(m, list(effect="marginal", measure="difference")), stm))
cat(sprintf("multinomial measure   %7.3f s  hits=%d  store %.1f KB\n",
    tm(function() q(jmvtab_reg_build(gss, utils::modifyList(m, list(effect="marginal", measure="difference")), stm))),
    r$hits, as.numeric(object.size(stm))/1024))
# at_reference off a cached record (the revive path)
ar <- q(jmvtab_reg_build(gss, o(effect = "at_reference"), st))
cat(sprintf("\nat_reference from a hit: hits=%d, table built=%s\n", ar$hits, !is.null(ar$tabs)))
