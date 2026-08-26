# PURPOSE: the suite's populations, and the memo that makes them free to ask for.
# ROLE: testthat sources helper files ONCE per parallel worker (queue_setup()'s load_hook), and that
#   worker then runs many test files -- so a value cached here is built once and shared by all of
#   them. Before this, 24 files recomputed gss_cat_data_formatting() (a 30-arm fct_recode over
#   21 483 rows) and ~25 rebuilt the same crosstab.
# KEY CONSTRAINTS:
#   - THERE ARE TWO POPULATIONS, AND THE REASON IS COST, NOT TASTE. A crosstab aggregates on
#     data.table and does not care how many rows it reads: measured, tab() and tab_html() take the
#     SAME time on 3 000 rows as on 21 483. A model fit does care -- tab_reg() is 5.3x faster on the
#     sample, nnet::multinom() 14x. So the crosstab side reads the WHOLE frame (fx_gss), and only
#     the regression side reads the sample (fx_reg_df). Moving a crosstab test onto the sample buys
#     nothing and costs it its statistical power.
#   - fx_gss() is what every golden was recorded on, and it IS forcats::gss_cat -- so the goldens
#     never had to move for this. Keep it that way.
#   - levels in the sample are FLOORED, never lumped: a test may name any level of the variables it
#     crosses, so a level must not vanish. Small cells are kept on purpose.
# See: CLAUDE.md section "Testing".

fx_cache <- local({
  e <- new.env(parent = emptyenv())
  function(key, build) {
    if (!exists(key, envir = e, inherits = FALSE)) assign(key, build(), envir = e)
    get(key, envir = e, inherits = FALSE)
  }
})

# --- the whole population: the crosstab side ------------------------------------------------------
fx_gss     <- function() forcats::gss_cat
fx_gss_fmt <- function() fx_cache("gss_fmt", gss_cat_data_formatting)

# --- the sample: the regression side --------------------------------------------------------------
FX_REG_N     <- 3000L   # rows
FX_REG_FLOOR <- 10L     # minimum rows per level of the variables the suite crosses

fx_reg_rows <- function() fx_cache("reg_rows", function() {
  g <- forcats::gss_cat
  set.seed(23005L)
  keep <- logical(nrow(g))
  for (v in c("marital", "race", "rincome", "partyid", "relig")) {
    f <- g[[v]]
    for (lv in levels(f)) {
      have <- sum(keep & f == lv, na.rm = TRUE)
      if (have >= FX_REG_FLOOR) next
      idx <- which(f == lv & !keep)
      if (length(idx)) keep[sample(idx, min(FX_REG_FLOOR - have, length(idx)))] <- TRUE
    }
  }
  rest <- which(!keep)
  if (FX_REG_N > sum(keep) && length(rest)) {
    keep[sample(rest, min(FX_REG_N - sum(keep), length(rest)))] <- TRUE
  }
  sort(which(keep))
})

fx_reg_df  <- function() fx_cache("reg_df",  function() fx_gss()[fx_reg_rows(), ])
fx_reg_fmt <- function() fx_cache("reg_fmt", function() fx_gss_fmt()[fx_reg_rows(), ])
