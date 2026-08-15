# PURPOSE: The whole-table chi2 / ANOVA test and the per-cell CONTRIBUTION to variance -- the two
#   computations that read a built table\'s cells and write either a tidy `test` tibble or the
#   `ctr` / `var` / `pvalue` fields.
# ROLE: Carved out of R/tab.R by Phase 19l (whole functions, no behaviour change).
# KEY CONSTRAINTS:
#   - TWO callers, ONE implementation: the leaf (leaf_chi2 / leaf_chi2_num, R/tab-leaf.R) and the
#     superseded tab_chi2() step (R/tab-steps-legacy.R). 19j moved the QUESTION into the leaf and
#     deliberately did NOT re-implement the arithmetic here, so a step and a build cannot compute two
#     different answers.
#   - chi2_compute_test() is READ-ONLY over the cells (it only builds the tibble); chi2_write_contrib()
#     is the ONE mutate(across()) pass that writes them.
#   - The contribution helpers (contrib_zero_inner / var_contrib_ctr_signed / contrib_adj_resid /
#     contrib_pvalue) work on plain vectors, never on the record -- that is what makes the pass cheap.
# See: CLAUDE.md Repository Map > R/tab-chi2.R; dev/chi2_cell_residuals_and_contributions.md.


# chi2_compute_test() -- the whole-table chi2 (factor col_vars) + ANOVA (mean col_vars) tests for one
# built factor table, returning the tidy `test` tibble (one row per subtable x col_var x test-type).
# Phase 9b-5: extracted from tab_chi2() as a READ-ONLY marshalling step -- it reads the aggregated cell
# statistics (get_n / get_mean / get_var) and the subtable grouping, feeds the plain-vector engines
# agg_chi2()/agg_anova() (R/tab-agg.R), and NEVER modifies the cells (so cell byte-identity is a given;
# only this plain tibble is recomputed). `tabs` is the prepped, post-tab_match_* record; the remaining
# args are its already-computed metadata (from tab_chi2()'s head).
# DESIGN: chi2/ANOVA run on the already-AGGREGATED cell statistics, never a raw N-scan -- cost scales
# with cells, not observations. Every (subtable x col_var) is one "table_id"; ALL tables are stacked
# and tested in ONE agg_chi2 / agg_anova pass (see the engine header).
# DESIGN (Phase 18z14-i, ruling Q3): the chi2 and the effect size are computed on the WEIGHTED table
#   whenever the table is weighted -- the weighted counts rescaled so they sum to the raw n. That is
#   the convention every OTHER inference in the same table already follows: the CIs are
#   Wilson(weighted p, unweighted n), and the ANOVA F has always taken §14's weighted group mean/var
#   with the unweighted n. Only the factor chi2 was still fully unweighted, so a weighted table
#   reported a p and a Cramer's V describing a population nobody had asked about.
#   It is a rescale, not a branch: get_wn() falls back to get_n() when there are no weights, so the
#   scale factor is exactly 1 and unweighted output is byte-identical BY CONSTRUCTION. Cramer's V is
#   scale-invariant, so it is the weighted V at any scale.
# WARNING: keep byte-identical to the pre-9b-5 inline block for UNWEIGHTED tables (locked by
#   test-calculations.R: chi2 + Yates, Welch/classic F, add_n parity; test-golden.R: `test`).
chi2_compute_test <- function(tabs, comp, row_var, col_vars_levels,
                              col_vars_levels_no_tot, is_a_mean, all_col_tot) {
  # Phase 9b-5: the kept-rows MASK over `tabs` (replaces the tabs2 = tabs[!is_totrow,] record-slice,
  # which reconstructed every fmt column just to read counts off it). Drops total rows (and total tabs
  # under comp = "all"). is_totrow/is_tottab are the pass-2 fmt_row_flag fast path (plain logical, no
  # reconstruction). Phase 10i-B: the former add_n/add_pct row exclusion ("n"/"row_pct") is gone --
  # chi2 runs at build on the CORE table, which never carries those display-only rows.
  mask2 <- if (comp == "all") !is_totrow(tabs) & !is_tottab(tabs) else !is_totrow(tabs)
  n_rows2 <- sum(mask2)

  # Subtable grouping over the kept rows. Byte-identical to group_indices()/group_keys() of the
  # totrow-dropped grouped_df -- computed on a fmt-FREE view (fmt columns dropped first) so the row
  # slice reconstructs NO fmt records; the same dplyr grouping machinery (incl. `.drop` and the
  # lv1_group_vars downgrade) runs, and grouping depends only on the untouched grouping columns.
  tabs2_grp    <- dplyr::select(tabs, !where(is_fmt))[mask2, ]
  subtab_idx   <- dplyr::group_indices(tabs2_grp)
  subtab_keys  <- dplyr::group_keys(tabs2_grp)
  tab_vars_chr <- names(subtab_keys)

  factor_cvs <- names(col_vars_levels)[!is_a_mean & !all_col_tot]
  mean_cvs   <- names(col_vars_levels)[ is_a_mean & !all_col_tot]

  # --- Chi2 for factor col_vars (WEIGHTED counts, rescaled to the raw n; see the DESIGN note) ---
  chi2_rows <- NULL
  if (length(factor_cvs) > 0 && n_rows2 > 0) {
    long <- dplyr::bind_rows(purrr::imap(
      col_vars_levels_no_tot[factor_cvs],
      function(levels, cv) {
        lv_cols <- purrr::map_chr(levels, rlang::as_name)
        if (length(lv_cols) == 0) return(NULL)
        M  <- vapply(lv_cols, function(cc) as.double(get_wn(tabs[[cc]])[mask2]), double(n_rows2))
        Mn <- vapply(lv_cols, function(cc) as.double(get_n (tabs[[cc]])[mask2]), double(n_rows2))
        # Phase 14a: `length(lv_cols)`, NOT `ncol(M)`. vapply() only returns a MATRIX when
        # FUN.VALUE has length > 1, so a row_var with exactly ONE non-total row (n_rows2 == 1 --
        # e.g. all but one level emptied by na = "drop") made M a plain vector, ncol(M) NULL, and
        # every rep(times = ncM) below died with "invalid 'times' argument". It surfaced as a
        # mirai error ("In index: 3 ... Caused by error in rep()"), but was never parallel-specific:
        # the serial map hits the identical line. `length(lv_cols)` is the column count by
        # construction and is shape-independent (as.vector(M) is column-major either way).
        ncM <- length(lv_cols)
        tibble::tibble(
          col_var  = cv,
          subtab   = rep(subtab_idx, times = ncM),
          table_id = paste(cv, rep(subtab_idx, times = ncM), sep = "\r"),
          row_id   = rep(seq_len(n_rows2), times = ncM),
          col_id   = rep(seq_len(ncM), each = n_rows2),
          o        = as.vector(M),
          o_raw    = as.vector(Mn)
        )
      }
    ))
    if (nrow(long) > 0) {
      # Rescale each table's weighted counts to sum to its raw n (the sample size the test is
      # entitled to). Unweighted: o == o_raw, so the factor is exactly 1 and nothing moves.
      weighted_tbl <- !identical(long$o, long$o_raw)
      if (weighted_tbl) {
        gs <- rowsum(cbind(long$o, long$o_raw), long$table_id, na.rm = TRUE)
        k  <- ifelse(gs[, 1] > 0, gs[, 2] / gs[, 1], 1)
        long$o <- long$o * k[as.character(long$table_id)]
      }
      res <- agg_chi2(long$table_id, long$row_id, long$col_id, long$o, correct = TRUE)
      map <- dplyr::distinct(long, .data$table_id, .data$col_var, .data$subtab)
      chi2_rows <- dplyr::left_join(map, tibble::as_tibble(res$tables), by = "table_id") |>
        dplyr::transmute(
          .data$subtab, .data$col_var, test = "chi2",
          statistic = .data$statistic, df1 = as.double(.data$df), df2 = NA_real_,
          pvalue = .data$pvalue, n = as.double(.data$n), min_e = .data$min_e,
          effect_size = .data$effect_size, es_type = .data$es_type,
          # Phase 18z16-i (W8): `deff` -- the design effect this test corrected by. NA on the
          # classic basis (there is none), filled by tab_robust_overlay() on the others.
          pvalue_exact = NA_real_, deff = NA_real_)

      # Phase 18j: Fisher's exact on the SMALL weak tables (smallest expected count < test_weak_min_e
      # AND a total feasible for an exact test), where the Pearson chi2 is unreliable -- stored as
      # `pvalue_exact` ON the chi2 row (NOT a separate row, so the tidy shape / row count is unchanged).
      # Only a NON-simulated (genuinely exact) p is kept: a large table drags min_e down via one rare
      # category but its chi2 is fine, so agg_fisher simulates there and we keep the chi2 (weak "!" flag).
      # The display prefers pvalue_exact when present.
      # Phase 18z14-i: skipped on a WEIGHTED table -- an exact test enumerates integer tables, and
      # weighted counts are not counts. The weak "!" flag still fires from min_e.
      weak_ids <- if (weighted_tbl) character() else
        res$tables$table_id[!is.na(res$tables$min_e) & res$tables$min_e < test_weak_min_e]
      if (length(weak_ids) > 0) {
        fish <- tibble::as_tibble(
          agg_fisher(long$table_id, long$row_id, long$col_id, long$o, weak_ids))
        fish$pvalue[fish$simulated] <- NA_real_          # keep only the exact (small-sample) p
        fmap <- dplyr::left_join(map, fish, by = "table_id")
        chi2_rows$pvalue_exact <- fmap$pvalue[
          match(paste(chi2_rows$subtab, chi2_rows$col_var, sep = "\r"),
                paste(fmap$subtab, fmap$col_var, sep = "\r"))]
      }
    }
  }

  # --- ANOVA for mean col_vars (Welch + classic F, from per-group summary stats) ---
  anova_rows <- NULL
  if (length(mean_cvs) > 0 && n_rows2 > 0) {
    longA <- dplyr::bind_rows(purrr::imap(
      col_vars_levels[mean_cvs],
      function(levels, cv) {
        cols <- purrr::map_chr(levels, rlang::as_name)
        keep <- purrr::map_lgl(cols, ~ fmt_var_kind(tabs[[.x]]) == "mean" &&
                                 !any(is_totcol(tabs[[.x]])))
        col  <- cols[keep][1]
        if (is.na(col)) return(NULL)
        tibble::tibble(
          col_var  = cv,
          subtab   = subtab_idx,
          table_id = paste(cv, subtab_idx, sep = "\r"),
          group_id = seq_len(n_rows2),
          n        = as.double(get_n(tabs[[col]])[mask2]),
          mean     = get_mean(tabs[[col]])[mask2],
          var      = get_var(tabs[[col]])[mask2])
      }
    ))
    if (nrow(longA) > 0) {
      resA  <- tibble::as_tibble(agg_anova(longA$table_id, longA$group_id,
                                           longA$n, longA$mean, longA$var))
      mapA  <- dplyr::distinct(longA, .data$table_id, .data$col_var, .data$subtab)
      baseA <- dplyr::left_join(mapA, resA, by = "table_id")
      welch <- dplyr::transmute(
        baseA, .data$subtab, .data$col_var, test = "F_welch",
        statistic = .data$statistic, df1 = .data$df1, df2 = .data$df2,
        pvalue = .data$pvalue, n = as.double(.data$n), min_e = NA_real_,
        effect_size = .data$effect_size, es_type = "eta2", deff = NA_real_)
      classic <- dplyr::transmute(
        baseA, .data$subtab, .data$col_var, test = "F_classic",
        statistic = .data$statistic_classic, df1 = .data$df1_classic, df2 = .data$df2_classic,
        pvalue = .data$pvalue_classic, n = as.double(.data$n), min_e = NA_real_,
        effect_size = .data$effect_size, es_type = "eta2", deff = NA_real_)
      anova_rows <- dplyr::bind_rows(welch, classic)
    }
  }

  # --- Assemble the tidy `test` attribute (one row per subtable x col_var x test-type) ---
  test_tbl <- dplyr::bind_rows(chi2_rows, anova_rows)
  if (nrow(test_tbl) == 0) {
    test_tbl <- new_test_tibble()
  } else {
    subtab_keys2 <- dplyr::mutate(subtab_keys, subtab = dplyr::row_number())
    test_tbl <- test_tbl |>
      dplyr::arrange(.data$subtab, .data$col_var, .data$test) |>
      dplyr::left_join(subtab_keys2, by = "subtab") |>
      # Phase 19g (KEY 6): the uniform key -- `var` (which variable this test is about), `col` (which
      # column it keys under), and the tab_var columns naming the sub-population.
      dplyr::mutate(var = !!row_var) |>
      dplyr::rename(col = "col_var") |>
      dplyr::select(-"subtab") |>
      dplyr::relocate(tidyselect::any_of(tab_vars_chr), "var", "col")
  }

  test_tbl
}


# contrib_zero_inner() -- the comp = "all" prologue shared by the two contribution helpers below:
# zero the INTERMEDIATE total rows/tabs (all but the last element, which is the grand total) so a
# comp = "all" pass decomposes the data cells only. A no-op under comp = "tab". Extracted
# (Phase 18z4) so the contribution and its residual can never disagree about which cells are in
# the table.
contrib_zero_inner <- function(xwn, twn, in_totrow, in_tottab, comp) {
  if (comp == "all") {
    idx <- seq_len(length(xwn) - 1L)
    tor <- in_totrow[idx] | in_tottab[idx]
    xwn[idx] <- dplyr::if_else(tor, 0, xwn[idx])
    twn[idx] <- dplyr::if_else(tor, 0, twn[idx])
  }
  list(xwn = xwn, twn = twn)
}

# var_contrib_ctr_signed() -- the signed absolute contribution of each cell to the (weighted) chi2,
# from the column's weighted counts `xwn` (get_wn) and its total column's `twn`, using the LAST
# element as the grand total. (The former fmt-vector helper var_contrib() with its "ctr_with_sign"
# branch was removed in Phase 17a; this plain-vector form, used by chi2_write_contrib(), is the sole
# live path.) DESIGN: the contribution stays WEIGHTED -- it is an ESTIMATE of the population table's
# inertia decomposition, which is what a weighted correspondence analysis reads (Phase 18z4 §4.4).
# Its significance is a separate quantity on the package's inference base: contrib_adj_resid().
var_contrib_ctr_signed <- function(xwn, twn, in_totrow, in_tottab, comp) {
  z   <- contrib_zero_inner(xwn, twn, in_totrow, in_tottab, comp)
  xwn <- z$xwn; twn <- z$twn
  n   <- length(xwn)
  observed_freq <- xwn / twn[n]
  expected_freq <- xwn[n] * twn / twn[n]^2
  spread        <- observed_freq - expected_freq
  sign(spread) * spread^2 / expected_freq
}

# contrib_adj_resid() -- the ADJUSTED STANDARDISED (Haberman 1973) residual of each cell, the signed
# quantity that both gates and (under `guaranteed_effect`) colours `color = "contrib"`. Same inputs as
# var_contrib_ctr_signed() plus `n_base`, the INFERENCE base (see chi2_write_contrib):
#
#   p_i = twn/N (row marginal)   p_j = xwn[n]/N (column marginal)   e_f = p_i*p_j (expected frequency)
#   z   = (xwn/N - e_f) * sqrt(n_base) / sqrt(e_f * (1 - p_i) * (1 - p_j))
#
# WARNING (Phase 18z4, the two defects this replaces):
#  1. It is the ADJUSTED residual, not the Pearson one `(o-e)/sqrt(e)` the old gate used. Pearson's
#     variance is (1-p_i)(1-p_j) < 1, so testing it at 1.96 under-rejects by up to 1/sqrt((1-p_i)(1-p_j))
#     -- measured 1.10 to 3.09x too strict on one 3x4 table. Only the adjusted residual is ~N(0,1), so
#     only for it is the +/-1.96 (or the textbook +/-2 / +/-3) rule correct.
#  2. `n_base` is an UNWEIGHTED sample size -- the raw n, or the effective one the inference basis
#     yields (see chi2_write_contrib) -- never the weighted total. The estimate is
#     weighted, the base is not -- the same rule as every confidence interval in the package (?tab,
#     Phase 18s). The old weighted base made every cell p-value 0 as soon as weights carried
#     population scale.
# On an unweighted table with n_base = N this reduces EXACTLY to (o-e)/sqrt(e(1-p_i)(1-p_j)), i.e.
# stats::chisq.test()$stdres (pinned by test-calculations.R).
# Sparse guard: a cell whose EXPECTED COUNT (e_f * n_base) is below 1 gets NA -- the normal
# approximation does not hold there (a cell with expected 0.2 otherwise flags at |z| = 6). A 1-row or
# 1-column table gives (1-p) = 0 -> non-finite -> NA, which is correct (no residual is defined).
contrib_adj_resid <- function(xwn, twn, n_base, in_totrow, in_tottab, comp) {
  z   <- contrib_zero_inner(xwn, twn, in_totrow, in_tottab, comp)
  xwn <- z$xwn; twn <- z$twn
  n   <- length(xwn)
  N   <- twn[n]
  p_i <- twn / N
  p_j <- xwn[n] / N
  e_f <- p_i * p_j                       # == xwn[n] * twn / N^2, var_contrib's expected_freq
  out <- (xwn / N - e_f) * sqrt(n_base) / sqrt(e_f * (1 - p_i) * (1 - p_j))
  out[e_f * n_base < 1]  <- NA_real_     # sparse: expected count < 1, asymptotics invalid
  out[!is.finite(out)]   <- NA_real_
  out
}

# contrib_pvalue() -- the two-sided p-value of contrib_adj_resid()'s standardized residual. Total
# rows/tabs are margins, not cells -> NA. Written into the `pvalue` field by chi2_write_contrib() so
# fmt_color_plan() can gate `color = "contrib"` under a significance policy (contrib has NO confidence
# interval to gate on), and so the residual itself stays recoverable at render time WITHOUT a new fmt
# field: |z| = -qnorm(p/2), sign from the signed contribution (fmt_resid(), R/fmt_class.R).
contrib_pvalue <- function(z, in_totrow, in_tottab, comp) {
  pv   <- 2 * stats::pnorm(-abs(z))
  prot <- if (comp == "all") in_totrow | in_tottab else in_totrow
  pv[prot] <- NA_real_
  pv[!is.finite(pv)] <- NA_real_
  pv
}

# chi2_write_contrib() -- Phase 9b-5: the per-cell contribution-to-variance WRITES (the `var` = signed
# absolute contribution, and the `ctr` = relative contribution = |cell| / group-total) plus the
# `comp_all` / contrib-`color` col-meta. The pre-9b-5 record path did this in ~6 successive
# mutate(across(where(is_fmt), set_*)) passes -- EACH a full tabxplor_fmt reconstruction. Here every
# value is PRECOMPUTED as a plain vector (plain field reads + the group sums run through the SAME dplyr
# but on fmt-FREE tibbles, so no reconstruction), then applied in ONE mutate(across()) with the real
# setters. `tabs` is the prepped, post-tab_match_* record; the remaining args are tab_chi2()'s already-
# computed metadata (`tot_cols` = detect_totcols()'s per-column total-column syms). Returns the modified
# `tabs`. `var` is written whenever calc has "var"/"ctr"; `ctr`/`comp_all`/`color` only under "ctr".
# WARNING: byte-identical to the pre-9b-5 blocks (locked by test-calculations.R variance-contributions
# + test-color-golden.R + test-golden.R). The dead `variances_by_group`/`cells_by_group` of the old
# path (computed, never used) are dropped.
chi2_write_contrib <- function(tabs, calc, comp, color, col_vars_levels,
                               col_vars_levels_no_tot, is_a_mean, all_col_tot, tot_cols,
                               deff = NULL) {
  do_ctr  <- "ctr" %in% calc
  fmt_nms <- names(tabs)[purrr::map_lgl(tabs, is_fmt)]
  # var_contrib_ctr_signed / the ctr seed are PER SUBTABLE: the pre-9b-5 writes were GROUPED mutates, so each
  # subtable's contributions use its own last (total) row. gid = the (post-prep) subtable of each row
  # (all 1s when ungrouped, e.g. comp = "all"). The row-wise ctr divide + colour don't depend on it.
  gid <- dplyr::group_indices(tabs)
  gids <- unique(gid)

  # --- 1a. absolute signed contribution -> `var` (eligible: non-mean cells of a real col_var) ---
  # Phase 10i-B: the `all_col_vars` exclusion (add_n/add_pct helper columns) is gone -- contrib runs
  # at build on the CORE table, which never carries them; only the total column (`no_col_var`) is out.
  var_after <- purrr::set_names(lapply(fmt_nms, function(nm) get_var(tabs[[nm]])), fmt_nms)
  # Phase 18a bug-fix: the per-cell standardized-residual p-value, computed here (where N = twn[n],
  # the subtable grand total, is in hand) and stored in `pvalue` so fmt_color_plan() can gate
  # `color = "contrib"` under a significance policy. Only under `do_ctr` (contrib coloring is on); the
  # pipeline computes contributions solely then (calc = c("ctr","p")), so plain tables are untouched.
  pval_after <- if (do_ctr) purrr::set_names(lapply(fmt_nms, function(nm) get_pvalue(tabs[[nm]])), fmt_nms)
  elig_col  <- purrr::keep(fmt_nms, function(nm) fmt_var_kind(tabs[[nm]]) != "mean" &&
                             get_col_var(tabs[[nm]]) != "no_col_var")
  # Phase 18z4: the residual's INFERENCE BASE, read off the total column's grand-total cell (the
  # LAST element of each subtable slice, exactly where var_contrib_ctr_signed reads the weighted N).
  # The effective `n_eff` when the table carries one, else the raw unweighted `n`; the weighted total
  # is used only as a last-resort fallback (it is what a table built without either would carry). This
  # is the SAME ladder as every confidence interval in the package (?tab, Phase 18s), so "weighted
  # estimate, unweighted or effective base" is one rule, not two.
  # Phase 18z16-iii (W3, ruling Q3): ONE base for every table SHAPE -- always the total column's
  # grand cell -- and the `type %in% c("n","all","all_tabs")` guess is GONE. That guess is what made
  # the same data give two irreconcilable significance patterns: a counts table read the cell's own
  # n_eff (whole-table base) while a row-percentage table read the total column's, which under a
  # design was degenerate (p = 1) and fell all the way back to the raw n -- measured 1.6e-11 vs 0.052
  # for the same cell (W3). The grand cell's own base is B^2/S at EVERY shape (its proportion is 1, so
  # the degenerate fallback returns the whole subtable's effective n), which is exactly why a counts
  # table and a percentage table of the same data now give identical residuals BY CONSTRUCTION -- the
  # residual is a property of the joint distribution and must not depend on `pct`.
  # It is the standard FIRST-ORDER correction, z_design = z_classic * sqrt(n_base / N).
  # Phase 18z16-iv (W-B): but the grand cell's OWN effective n is the wrong quantity to correct an
  # ASSOCIATION by. Its proportion is 1, so its design variance is 0 and it ALWAYS took the degenerate
  # flat fallback B^2/S -- the weights-only number -- at EVERY basis, so a stratified + clustered table
  # and a flat one gave residuals identical to the last digit while their CELL intervals differed.
  # Measured on a cluster-level row_var (a geography / school / establishment -- the commonest reason
  # to have clusters at all): |z| overstated x2.52, two of three cells reading p = 3.7e-04 and 2.7e-06
  # whose design-honest values are 0.18 and 0.080, i.e. coloured where they should be greyed.
  # The honest base is the raw n over Rao-Scott's mean generalized design effect of THIS test -- the
  # same delta-bar the omnibus row reports, so the colours and the p in one table describe ONE design
  # effect (they were also 2.5 % apart at basis "weights"). `deff` is the producer's grid, keyed here
  # onto this table's own groups; it is NULL at basis "n", so the raw-n base a correspondence analysis
  # reads stands BY CONSTRUCTION, not by a branch (maintainer's ruling). It is still the FIRST-ORDER
  # correction: an exact per-cell design residual needs each cell's own influence function -- stated
  # as the honest residue in ?tab.
  dl   <- if (is.null(deff)) NULL else svy_deff_lookup(deff, dplyr::group_vars(tabs))
  gkey <- if (is.null(dl)) NULL else {
    gk <- dplyr::group_keys(tabs)
    if (ncol(gk) == 0L) rep("", max(1L, nrow(gk)))
    else do.call(paste, c(lapply(gk, svy_key_chr), list(sep = "\r")))
  }
  for (nm in elig_col) {
    tot_nm <- as.character(tot_cols[[nm]])
    xwn <- get_wn(tabs[[nm]]); twn <- get_wn(tabs[[tot_nm]])
    itr <- is_totrow(tabs[[nm]]); itt <- is_tottab(tabs[[nm]])
    tn  <- if (do_ctr) get_n(tabs[[tot_nm]])
    tne <- if (do_ctr) get_n_eff(tabs[[tot_nm]])
    cv  <- if (do_ctr) get_col_var(tabs[[nm]])
    v   <- var_after[[nm]]
    pv  <- if (do_ctr) pval_after[[nm]]
    for (g in gids) {
      r <- which(gid == g)
      v[r] <- var_contrib_ctr_signed(xwn[r], twn[r], itr[r], itt[r], comp)
      if (do_ctr) {
        last   <- r[length(r)]
        ne     <- tne[last]
        n_base <- ifelse(is.finite(ne) & ne > 0, ne, tn[last])
        n_base[!is.finite(n_base) | n_base <= 0] <- twn[last]
        # a missing delta-bar (svychisq failed, a 1-level factor, under 3 obs) falls THROUGH to the
        # ladder above: at basis "weights" B^2/S IS the flat design's own effective n, and under a
        # design it is the weighting-only correction the package already declares elsewhere.
        if (!is.null(dl)) {
          dd <- unname(dl[paste(gkey[[min(g, length(gkey))]], cv, sep = "\r")])
          if (isTRUE(is.finite(dd) && dd > 0 && is.finite(tn[last]) && tn[last] > 0))
            n_base <- tn[last] / dd
        }
        zres   <- contrib_adj_resid(xwn[r], twn[r], n_base, itr[r], itt[r], comp)
        pv[r]  <- contrib_pvalue(zres, itr[r], itt[r], comp)
      }
    }
    var_after[[nm]] <- v
    if (do_ctr) pval_after[[nm]] <- pv
  }

  ctr_final <- NULL; comp_all_val <- NULL; color_apply <- character(0)
  if (do_ctr) {
    gv           <- dplyr::group_vars(tabs)
    grp_cols     <- purrr::set_names(lapply(gv, function(g) tabs[[g]]), gv)
    table_totrow <- is_totrow(tabs)
    elig_cv      <- names(col_vars_levels)[!is_a_mean & !all_col_tot]

    # per eligible col_var: variances_by_row + cells_by_row -- plain grouped tibbles mirroring the old
    # variances_calc / cells_calc, run through the EXACT original downstream dplyr (no fmt columns).
    ctr_after <- purrr::set_names(lapply(fmt_nms, function(nm) get_ctr(tabs[[nm]])), fmt_nms)
    for (cv in elig_cv) {
      lev_nt <- purrr::map_chr(col_vars_levels_no_tot[[cv]], rlang::as_name)
      vcalc  <- tibble::as_tibble(c(
        grp_cols,
        purrr::set_names(lapply(lev_nt, function(cc) abs(var_after[[cc]])), lev_nt)))
      if (length(gv)) vcalc <- dplyr::group_by(vcalc, dplyr::across(dplyr::all_of(gv)))

      vbr <- vcalc |>
        dplyr::mutate(dplyr::across(where(is.double), ~ sum(., na.rm = TRUE))) |>
        dplyr::ungroup() |> dplyr::select(where(is.double)) |> rowSums(na.rm = TRUE)

      cbr <- vcalc |> tibble::add_column(totrows = table_totrow) |>
        dplyr::mutate(dplyr::across(where(is.double),
          ~ dplyr::if_else(.data$totrows, 0, dplyr::if_else(is.na(.), 0, 1)))) |>
        dplyr::select(-"totrows") |>
        dplyr::mutate(cells = sum(!!!col_vars_levels_no_tot[[cv]]), .groups = "drop") |>
        dplyr::pull(.data$cells)

      # relative-contribution seed on ALL of cv's level columns (incl. its total column):
      # total rows -> 1/cells, others -> the group total variance (broadcast).
      for (L in purrr::map_chr(col_vars_levels[[cv]], rlang::as_name)) {
        ctr_after[[L]] <- dplyr::if_else(is_totrow(tabs[[L]]), 1 / cbr, vbr)
      }
    }

    # divide by the seed to get the relative contribution (|cell| / group-total), keeping the protected
    # total rows untouched (comp = "tab": total rows; comp = "all": total rows of the total table).
    ctr_final <- purrr::set_names(lapply(fmt_nms, function(nm) {
      # comp = "all": protect the total table's total row (it holds the whole-table mean-contribution
      # seed, read back by get_mean_contrib); grand_totrow() degrades to the plain total row when
      # there is no total table (no tab_vars), so the seed is stored, not overwritten.
      prot <- if (comp == "tab") is_totrow(tabs[[nm]]) else grand_totrow(tabs[[nm]])
      dplyr::if_else(prot, ctr_after[[nm]], var_after[[nm]] / ctr_after[[nm]])
    }), fmt_nms)

    comp_all_val <- comp[1] == "all"

    if (!is.na(color[1]) && color[1] != "no") {
      # Phase 19b: which KINDS of column `color = "contrib"` may paint. A count column has no
      # percentage base, so it is named by its var_kind; the rest by theirs.
      color_condition <- switch(color[1],
        "auto"    = c("all", "all_tabs"),
        "all"     = c("row", "col", "all", "all_tabs"),
        "all_pct" = c("all", "all_tabs"))
      want_counts <- color[1] %in% c("auto", "all")
      color_apply <- purrr::keep(fmt_nms, function(nm)
        get_pct_base(tabs[[nm]]) %in% color_condition ||
          (want_counts && fmt_var_kind(tabs[[nm]]) == "count"))
    }
  }

  # single write pass over the UNGROUPED table (so each `col` is the full column that the full-length
  # precomputed vectors match), then restore the original grouping: `var` (always) + `ctr`/`comp_all`/
  # `color` (only under "ctr" calc). The values are group-correct already (var per subtable above; the
  # ctr divide + colour are row-wise), so ungroup/rewrite/regroup is byte-identical.
  grp <- dplyr::group_vars(tabs)
  drp <- dplyr::group_by_drop_default(tabs)
  res <- dplyr::mutate(dplyr::ungroup(tabs), dplyr::across(where(is_fmt), function(col) {
    nm  <- dplyr::cur_column()
    col <- set_var(col, var_after[[nm]])
    if (do_ctr) {
      col <- set_ctr(col, ctr_final[[nm]])
      # Reproduce a byte-identity quirk of the pre-9b-5 path: its ctr writes used dplyr::if_else() over
      # fmt columns, and combining fmt vectors MATERIALISES the `wn` field (NA -> the n fallback). The
      # plain set_ctr here does not, so fill wn from get_wn() (a no-op when wn is already set / weighted;
      # matters only for an unweighted table built via tab_plain() |> tab_chi2(), where wn was NA).
      col <- set_wn(col, get_wn(col))
      # Phase 18a bug-fix: the standardized-residual p-value (contrib significance gate). A no-op on
      # non-eligible columns (pval_after there is the original get_pvalue); the residual on contrib cells.
      col <- set_pvalue(col, pval_after[[nm]])
      col <- set_comp_all(col, comp_all_val)
      if (nm %in% color_apply) col <- set_color(col, "contrib")
    }
    col
  }))
  if (length(grp)) res <- dplyr::group_by(res, dplyr::across(dplyr::all_of(grp)), .drop = drp)
  res
}
