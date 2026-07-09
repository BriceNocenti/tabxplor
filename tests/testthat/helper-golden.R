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
  v <- stats::rnorm(n, 10, 3)           # numeric col_var for the weighted-mean golden case
  h[seq(1L, n, by = 7L)]  <- NA         # ~14% missing
  k[seq(1L, n, by = 11L)] <- NA         # ~9% missing, different rows
  tibble::tibble(
    g = factor(g), h = factor(h), k = factor(k), w = w, v = v
  )
}

# Sparse numeric frame for the Phase 2 moment-sum variance edge cases: group "B" has a
# single non-NA value (n = 1 -> stats::var() is NA, so no sd is shown) and group "C" is
# all-NA (mean and var both NA). Locks the NaN->NA handling the moment-sum rewrite must
# reproduce (0/0 from Sigma-form must map to the same NA stats::var/mean produce).
golden_sparse_df <- function() {
  tibble::tibble(
    grp = factor(c("A", "A", "A", "A", "B", "B", "C", "C")),
    v   = c(1, 2, 3, 4, 5, NA, NA, NA)
  )
}

# Named list of zero-arg thunks, each producing one table. Names are the fixture basenames.
golden_cases <- function() {
  gss <- forcats::gss_cat
  syn <- golden_syn_df()
  sparse <- golden_sparse_df()

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
    f_color_afterci  = function() suppressWarnings(tab(gss, marital, race, pct = "row", ci = "cell", color = "after_ci")),  # deprecated color string
    f_color_contrib  = function() tab(gss, marital, race, pct = "row", color = "contrib"),
    f_subtab         = function() tab(gss, marital, race, relig, pct = "row"),  # grouped_tab
    f_selfcross      = function() tab(gss, marital, marital, pct = "row"),  # _colvarbis self-crosstab lock (Phase 2)
    f_merge2         = function() tab(gss, c(marital, relig), race, pct = "row"),  # Phase 6 (§13): tab() merges >=2 row_vars by default
    f_ref_named      = function() tab(gss, c(marital, relig), race, pct = "row",   # Phase 6 (§4): named per-row_var ref vector
                                      ref = c(marital = "tot", relig = "first"), color = "diff"),
    f_common_base    = function() tab(syn, g, c(h, k), pct = "row", na = "common_base"),  # Phase 6g (§4, S3): old-tab() population rule

    # --- numeric (means) via tab_num() ---
    n_mean           = function() tab_num(gss, race, c(age, tvhours), marital, comp = "all", digits = 1L),
    n_mean_color     = function() tab_num(gss, race, c(age, tvhours), comp = "all", color = "diff", digits = 1L),
    n_mean_ci        = function() tab_num(gss, race, c(age, tvhours), comp = "all", ci = "cell", digits = 1L),  # z-based mean CI; Phase 3 -> bounds / Welch-t
    n_mean_w         = function() tab_num(syn, g, v, wt = w, comp = "all"),   # weighted ML-variance branch lock (Phase 2a)
    n_mean_sparse    = function() tab_num(sparse, grp, v, comp = "all"),      # n<=1 / all-NA variance edge, NaN->NA (Phase 2a)
    n_mean_tottab    = function() tab_num(gss, race, c(age, tvhours), marital, comp = "all", totaltab = "table", digits = 1L),  # total-table rollup lock (Phase 2)
    n_ci_tabvars     = function() tab_num(gss, race, c(age, tvhours), marital, ci = "cell", digits = 1L),              # Phase 6e: previously-crashing ci="cell" + tab_vars (comp="tab")
    n_ci_tabvars_all = function() tab_num(gss, race, c(age, tvhours), marital, ci = "cell", comp = "all", digits = 1L), # Phase 6e: ... (comp="all")

    # --- multi col_var + weighting + tot_n motivating cases ---
    # tab()-equivalent cases (single row_var, na="keep"/none) go through the public tab().
    # Per-col_var na="drop" (distinct per-column bases) is now also a tab() behaviour (Phase 7a
    # fixed tab()'s "drop"); these fixtures keep driving the internal engine tab_build() directly
    # (byte-identical), as does the engine-only totcol="each".
    m_multi          = function() tab(syn, g, c(h, k), pct = "row"),
    totn_keep        = function() tab(syn, g, c(h, k), pct = "col", na = "keep"),
    totn_drop        = function() tabxplor:::tab_build(syn, g, c(h, k), pct = "col", na = "drop", output = "single"),
    totn_row_drop    = function() tabxplor:::tab_build(syn, g, c(h, k), pct = "row", na = "drop", output = "single"),  # cross-col_var tot_n exactness lock (Phase 2c)
    f_totcol_each    = function() tabxplor:::tab_build(gss, marital, c(race, relig), pct = "row", totcol = "each", output = "single"),  # per-col_var totals; Phase 6 -> one total col
    w_weighted       = function() tab(syn, g, h, wt = w, pct = "col")
  )
}

# Subset whose user-facing display (tab_md) is ALSO snapshotted, on top of structural equality.
golden_display_cases <- c("f_row_pct", "f_ci_cell", "f_ci_diff", "f_color_diff",
                          "n_mean", "n_mean_ci", "totn_drop",
                          "f_selfcross", "n_mean_w", "n_mean_sparse", "totn_row_drop",
                          "n_mean_color",     # locks the D3-interim numeric coloring (-> Phase 5)
                          "n_mean_tottab")    # locks the total-table rollup (Phase 2)

# Directory holding the structural .rds fixtures (relative to tests/testthat/).
golden_dir <- function() testthat::test_path("_golden")
