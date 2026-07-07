# PURPOSE: Single source of truth for the golden characterization matrix.
# ROLE: Used by BOTH dev/make_golden.R (writes the .rds fixtures) and test-golden.R
#        (checks live output against them), so generator and test can never drift.
# KEY CONSTRAINTS:
#   - Every case must be DETERMINISTIC (no Sys.time / unseeded random).
#   - Adding/removing a case is fine; CHANGING an existing case's call means its golden
#     output legitimately changes -> regenerate consciously (see CLAUDE.md golden protocol).
# See: CLAUDE.md > 1.4.0 roadmap > Golden regeneration protocol.

# Small synthetic frame for weighting + controlled, DIFFERING NA patterns across two
# col_vars (h vs k). This is the motivating fixture for the future `tot_n` field: with
# na = "drop", h and k have different total counts, so the current "use the last col_var's
# total column" logic is an approximation. The captured golden documents today's behaviour.
golden_syn_df <- function() {
  set.seed(42)
  n <- 600L
  g <- sample(c("A", "B", "C"), n, replace = TRUE)
  h <- sample(c("x", "y"),      n, replace = TRUE)
  k <- sample(c("p", "q", "r"), n, replace = TRUE)
  w <- stats::runif(n, 0.5, 2)
  h[seq(1L, n, by = 7L)]  <- NA         # ~14% missing
  k[seq(1L, n, by = 11L)] <- NA         # ~9% missing, different rows
  tibble::tibble(
    g = factor(g), h = factor(h), k = factor(k), w = w
  )
}

# Named list of zero-arg thunks, each producing one table. Names are the fixture basenames.
golden_cases <- function() {
  gss <- forcats::gss_cat
  syn <- golden_syn_df()

  list(
    # --- factor tables via tab() ---
    f_row_pct        = function() tab(gss, marital, race, pct = "row"),
    f_col_pct        = function() tab(gss, marital, race, pct = "col"),
    f_all_pct        = function() tab(gss, marital, race, pct = "all"),
    f_counts         = function() tab(gss, marital, race, pct = "no"),
    f_ci_cell        = function() tab(gss, marital, race, pct = "row", ci = "cell"),
    f_ci_diff        = function() tab(gss, marital, race, pct = "row", ci = "diff"),   # AC diff-interval; Phase 3 -> Newcombe + stars
    f_chi2           = function() tab(gss, marital, race, pct = "row", chi2 = TRUE),
    f_ref_first      = function() tab(gss, marital, race, pct = "row", ref = "first"),
    f_or             = function() tab(gss, marital, race, pct = "col", OR = "OR"),     # empirical OR; Phase 1 (rr->ratio) / Phase 3 (Wald p, 1/OR)
    f_color_diff     = function() tab(gss, marital, race, pct = "row", color = "diff"),
    f_color_afterci  = function() tab(gss, marital, race, pct = "row", ci = "cell", color = "after_ci"),
    f_color_contrib  = function() tab(gss, marital, race, pct = "row", color = "contrib"),
    f_subtab         = function() tab(gss, marital, race, relig, pct = "row"),  # grouped_tab

    # --- numeric (means) via tab_num() ---
    n_mean           = function() tab_num(gss, race, c(age, tvhours), marital, comp = "all", digits = 1L),
    n_mean_color     = function() tab_num(gss, race, c(age, tvhours), comp = "all", color = "diff", digits = 1L),
    n_mean_ci        = function() tab_num(gss, race, c(age, tvhours), comp = "all", ci = "cell", digits = 1L),  # z-based mean CI; Phase 3 -> bounds / Welch-t

    # --- tab_many() multi col_var + weighting + tot_n motivating cases ---
    m_multi          = function() tab_many(syn, g, c(h, k), pct = "row"),
    totn_keep        = function() tab_many(syn, g, c(h, k), pct = "col", na = "keep"),
    totn_drop        = function() tab_many(syn, g, c(h, k), pct = "col", na = "drop"),
    f_totcol_each    = function() tab_many(gss, marital, c(race, relig), pct = "row", totcol = "each"),  # per-col_var totals; Phase 6 -> one total col
    w_weighted       = function() tab_many(syn, g, h, wt = w, pct = "col")
  )
}

# Subset whose user-facing display (tab_md) is ALSO snapshotted, on top of structural equality.
golden_display_cases <- c("f_row_pct", "f_ci_cell", "f_ci_diff", "f_color_diff",
                          "n_mean", "n_mean_ci", "totn_drop")

# Directory holding the structural .rds fixtures (relative to tests/testthat/).
golden_dir <- function() testthat::test_path("_golden")
