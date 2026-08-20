# PURPOSE: the EXHAUSTIVE version of tests/testthat/test-reg-invariants.R -- every reachable
#          `family x effect x measure` of `tab_reg()`, under both `empirical` modes, checked against
#          the three invariants every regression cell must satisfy.
# ROLE: the wide net. The suite carries a light grid (one case per family plus the logged contrasts)
#   so it stays inside the test budget; this is what you run after touching an estimand, a crude
#   shape, a marginal finaliser or a scale. Not a testthat file: it takes minutes.
# KEY CONSTRAINTS:
#   - the invariants are read off the column's own DECLARED scale (EST_SCALES), never per family --
#     that is what makes one loop cover every producer.
#   - a REFUSAL is an expected outcome, not a failure: most of the grid is deliberately not offered.
# Run:  OMP_NUM_THREADS=1 Rscript dev/verify_reg_invariants.R
# See: CLAUDE.md section "tabxplor architecture" (the regression subsystem).

suppressMessages(devtools::load_all("~/github/tabxplor", quiet = TRUE))
options(width = 170)
data.table::setDTthreads(1L)

# --- the data: one outcome per family, plus a summed-score battery ---------------------------------
inv_data <- function(n = 4000) {
  d <- gss_cat_data_formatting()
  d <- d[!is.na(d$married) & !is.na(d$party3) & !is.na(d$rincome) &
           !is.na(d$race) & !is.na(d$age) & !is.na(d$tvhours), ]
  set.seed(20260820)
  d[sample(nrow(d), min(n, nrow(d))), ]
}
inv_tea <- function() {
  e <- new.env(); utils::data("tea", package = "FactoMineR", envir = e); tea <- e$tea
  items <- c("home", "tearoom", "work", "friends", "resto", "pub")
  tea$tea_where <- rowSums(vapply(items, function(v) as.integer(tea[[v]] == v),
                                  integer(nrow(tea))))
  tea$sex <- factor(tea$sex); tea$SPC <- factor(tea$SPC)
  tea
}

d <- inv_data(); tea <- inv_tea()
CASES <- list(
  list(fam = "gaussian",     data = d,   y = "age",       x = c("race", "tvhours"), trials = NA),
  list(fam = "binomial",     data = d,   y = "married",   x = c("race", "age"),     trials = NA),
  list(fam = "poisson",      data = d,   y = "tvhours",   x = c("race", "age"),     trials = NA),
  list(fam = "quasipoisson", data = d,   y = "tvhours",   x = c("race", "age"),     trials = NA),
  list(fam = "multinomial",  data = d,   y = "party3",    x = c("race", "age"),     trials = NA),
  list(fam = "ordinal",      data = d,   y = "rincome",   x = c("race", "age"),     trials = NA),
  list(fam = "binomial",     data = tea, y = "tea_where", x = c("sex", "SPC"),      trials = 6)
)
EFFECTS  <- REG_EFFECTS_VALUES
MEASURES_ <- c("auto", "odds_ratio", "ratio", "difference", "log", "log_odds", "log_risk")
MODES    <- c("column", "cell")

# --- the three invariants, per column ---------------------------------------------------------------
check_col <- function(t, col) {
  scr <- EST_SCALES[[get_scale(col)]]
  est <- fmt_est_of(col); lo <- get_ci_inf(col); hi <- get_ci_sup(col); p <- get_pvalue(col)
  ok  <- is.finite(est) & is.finite(lo) & is.finite(hi)
  ref <- is_refrow(col) & as.character(t$var) != "Constant" & is.finite(est)
  okp <- ok & is.finite(p)
  n0  <- if (is.na(scr$neutral)) NA_real_ else scr$neutral
  c(cells    = sum(ok),
    bad_ci   = sum(ok & (est < lo - 1e-9 | est > hi + 1e-9)),
    bad_ref  = if (is.na(n0)) 0 else sum(abs(est[ref] - n0) > 1e-9),
    bad_star = if (is.na(n0)) 0 else
      sum((p[okp] < 0.05) != (lo[okp] > n0 + 1e-12 | hi[okp] < n0 - 1e-12)))
}

rows <- list()
for (cs in CASES) for (e in EFFECTS) for (m in MEASURES_) for (mode in MODES) {
  args <- list(cs$data, outcome = cs$y, predictors = cs$x, family = cs$fam,
               effect = e, measure = m, empirical = mode)
  if (!is.na(cs$trials)) args$trials <- cs$trials
  t <- tryCatch(suppressWarnings(suppressMessages(do.call(tab_reg, args))),
                error = function(er) conditionMessage(er))
  tag <- sprintf("%-13s %-12s %-11s %-6s", cs$fam, e, m, mode)
  if (is.character(t)) {
    typed <- grepl("does not offer|is not defined|returns the coefficient", t)
    rows[[length(rows) + 1L]] <- data.frame(
      tag, col = "-", scale = "-", cells = 0,
      status = if (typed) "refused" else "ERROR",
      detail = substr(gsub("\n", " ", t), 1, 90))
    next
  }
  est_cols <- names(t)[vapply(t, function(x)
    is_fmt(x) && get_role(x) %in% c("model", "emp"), logical(1))]
  for (cn in est_cols) {
    r <- check_col(t, t[[cn]])
    rows[[length(rows) + 1L]] <- data.frame(
      tag, col = cn, scale = get_scale(t[[cn]]), cells = unname(r[["cells"]]),
      status = if (sum(r[c("bad_ci", "bad_ref", "bad_star")]) == 0) "ok" else "FAIL",
      detail = sprintf("ci=%d ref=%d star=%d", r[["bad_ci"]], r[["bad_ref"]], r[["bad_star"]]))
  }
}
R <- do.call(rbind, rows)

cat("\n===== FAILURES =====\n")
print(R[R$status == "FAIL", ], row.names = FALSE)
cat("\n===== UNTYPED ERRORS (a refusal must say why) =====\n")
print(R[R$status == "ERROR", ], row.names = FALSE)
cat("\n===== summary =====\n"); print(table(R$status))
cat("cells checked:", sum(R$cells), "\n")
invisible(R)
