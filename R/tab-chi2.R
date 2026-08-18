# PURPOSE: The whole-table chi2 / ANOVA test and the per-cell CONTRIBUTION to variance.
# ROLE: The two computations that read a BUILT table's cells and write either the tidy `test`
#   tibble or the `ctr` / `var` / `pvalue` fields. Two callers, one implementation: the leaf
#   (leaf_chi2 / leaf_chi2_num, R/tab-leaf.R) and the superseded tab_chi2() step
#   (R/tab-steps-legacy.R), so a step and a build cannot compute two different answers.
# KEY CONSTRAINTS:
#   - chi2_compute_test() is READ-ONLY over the cells (it only builds the tibble);
#     chi2_write_contrib() is the ONE mutate(across()) pass that writes them.
#   - The tests run on the ALREADY-AGGREGATED cell statistics, never a raw N-scan, so cost scales
#     with cells and not observations. Every (subtable x col_var) is one `table_id`, and ALL tables
#     are stacked and tested in ONE agg_chi2() / agg_anova() pass.
#   - WEIGHTED whenever the table is weighted -- the weighted counts rescaled to sum to the raw n.
#     That is the package's rule everywhere (weighted estimate, unweighted base), and it is a
#     rescale rather than a branch: get_wn() falls back to get_n(), so on unweighted data the factor
#     is exactly 1 and the output is byte-identical by construction. Cramer's V is scale-invariant,
#     so it is the weighted V at any scale.
#   - The contribution helpers (contrib_zero_inner / var_contrib_ctr_signed / contrib_adj_resid /
#     contrib_pvalue) work on plain vectors, never on the record -- that is what keeps the pass cheap.
# See: CLAUDE.md § tabxplor architecture (the inference layer);
#      dev/chi2_cell_residuals_and_contributions.md (the residual / contribution derivation).


# === SECTION: the whole-table test =================================================

# WARNING: byte-identity is locked by test-calculations.R (chi2 + Yates, Welch/classic F) and test-golden.R.
#' @keywords internal
#' @noRd
chi2_compute_test <- function(tabs, comp, row_var, col_vars_levels,
                              col_vars_levels_no_tot, is_a_mean, all_col_tot) {
  mask2 <- if (comp == "all") !is_totrow(tabs) & !is_tottab(tabs) else !is_totrow(tabs)
  n_rows2 <- sum(mask2)

  tabs2_grp    <- dplyr::select(tabs, !where(is_fmt))[mask2, ]
  subtab_idx   <- dplyr::group_indices(tabs2_grp)
  subtab_keys  <- dplyr::group_keys(tabs2_grp)
  tab_vars_chr <- names(subtab_keys)

  factor_cvs <- names(col_vars_levels)[!is_a_mean & !all_col_tot]
  mean_cvs   <- names(col_vars_levels)[ is_a_mean & !all_col_tot]

  # --- Chi2 for factor col_vars (weighted counts rescaled to the raw n) ---
  chi2_rows <- NULL
  if (length(factor_cvs) > 0 && n_rows2 > 0) {
    long <- dplyr::bind_rows(purrr::imap(
      col_vars_levels_no_tot[factor_cvs],
      function(levels, cv) {
        lv_cols <- purrr::map_chr(levels, rlang::as_name)
        if (length(lv_cols) == 0) return(NULL)
        M  <- vapply(lv_cols, function(cc) as.double(get_wn(tabs[[cc]])[mask2]), double(n_rows2))
        Mn <- vapply(lv_cols, function(cc) as.double(get_n (tabs[[cc]])[mask2]), double(n_rows2))
        # WARNING: length(lv_cols), never ncol(M) -- vapply() returns a bare vector when there is one row.
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
          pvalue_exact = NA_real_, deff = NA_real_, outcome = NA_character_)

      # DESIGN: Fisher's exact on the weak tables (min expected < test_weak_min_e), kept ON the chi2
      #   row so the tidy shape is unchanged; only a genuinely exact p, and never on weighted counts.
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
      resA  <- tibble::as_tibble(agg_anova(longA$table_id,
                                           longA$n, longA$mean, longA$var))
      mapA  <- dplyr::distinct(longA, .data$table_id, .data$col_var, .data$subtab)
      baseA <- dplyr::left_join(mapA, resA, by = "table_id")
      welch <- dplyr::transmute(
        baseA, .data$subtab, .data$col_var, test = "F_welch",
        statistic = .data$statistic, df1 = .data$df1, df2 = .data$df2,
        pvalue = .data$pvalue, n = as.double(.data$n), min_e = NA_real_,
        effect_size = .data$effect_size, es_type = "eta2", deff = NA_real_, outcome = NA_character_)
      classic <- dplyr::transmute(
        baseA, .data$subtab, .data$col_var, test = "F_classic",
        statistic = .data$statistic_classic, df1 = .data$df1_classic, df2 = .data$df2_classic,
        pvalue = .data$pvalue_classic, n = as.double(.data$n), min_e = NA_real_,
        effect_size = .data$effect_size, es_type = "eta2", deff = NA_real_, outcome = NA_character_)
      anova_rows <- dplyr::bind_rows(welch, classic)
    }
  }

  test_tbl <- dplyr::bind_rows(chi2_rows, anova_rows)
  if (nrow(test_tbl) == 0) {
    test_tbl <- new_test_tibble()
  } else {
    subtab_keys2 <- dplyr::mutate(subtab_keys, subtab = dplyr::row_number())
    test_tbl <- test_tbl |>
      dplyr::arrange(.data$subtab, .data$col_var, .data$test) |>
      dplyr::left_join(subtab_keys2, by = "subtab") |>
      dplyr::mutate(var = !!row_var) |>
      dplyr::rename(col = "col_var") |>
      dplyr::select(-"subtab") |>
      dplyr::relocate(tidyselect::any_of(tab_vars_chr), "var", "col")
  }

  test_tbl
}


# === SECTION: per-cell contributions and residuals =================================

# DESIGN: zero the INTERMEDIATE totals (all but the last, the grand total) so a comp = "all" pass
#   decomposes data cells only -- shared, so contribution and residual agree on which cells are in.
#' @keywords internal
#' @noRd
contrib_zero_inner <- function(xwn, twn, in_totrow, in_tottab, comp) {
  if (comp == "all") {
    idx <- seq_len(length(xwn) - 1L)
    tor <- in_totrow[idx] | in_tottab[idx]
    xwn[idx] <- dplyr::if_else(tor, 0, xwn[idx])
    twn[idx] <- dplyr::if_else(tor, 0, twn[idx])
  }
  list(xwn = xwn, twn = twn)
}

# DESIGN: the contribution stays WEIGHTED -- it ESTIMATES the population table's inertia decomposition,
#   what a weighted correspondence analysis reads. Significance is separate: contrib_adj_resid().
#' @keywords internal
#' @noRd
var_contrib_ctr_signed <- function(xwn, twn, in_totrow, in_tottab, comp) {
  z   <- contrib_zero_inner(xwn, twn, in_totrow, in_tottab, comp)
  xwn <- z$xwn; twn <- z$twn
  n   <- length(xwn)
  observed_freq <- xwn / twn[n]
  expected_freq <- xwn[n] * twn / twn[n]^2
  spread        <- observed_freq - expected_freq
  sign(spread) * spread^2 / expected_freq
}

# The ADJUSTED STANDARDISED (Haberman) residual. With p_i = twn/N, p_j = xwn[n]/N and e_f = p_i*p_j:
#   z = (xwn/N - e_f) * sqrt(n_base) / sqrt(e_f * (1 - p_i) * (1 - p_j))
# DESIGN: adjusted, NOT Pearson's (o-e)/sqrt(e) -- Pearson's variance is (1-p_i)(1-p_j) < 1, so a 1.96
#   test on it under-rejects; only the adjusted residual is ~N(0,1). At n_base = N it reduces EXACTLY to
#   stats::chisq.test()$stdres (pinned by test-calculations.R).
# WARNING: `n_base` is an UNWEIGHTED size -- the raw n, or the effective one the inference basis yields,
#   never the weighted total (weighted estimate, unweighted base, the rule of every CI here). Sparse
#   guard: expected count < 1 -> NA (0.2 would flag at |z| = 6); a 1-row/1-column table gives (1-p) = 0.
#' @keywords internal
#' @noRd
contrib_adj_resid <- function(xwn, twn, n_base, in_totrow, in_tottab, comp) {
  z   <- contrib_zero_inner(xwn, twn, in_totrow, in_tottab, comp)
  xwn <- z$xwn; twn <- z$twn
  n   <- length(xwn)
  N   <- twn[n]
  p_i <- twn / N
  p_j <- xwn[n] / N
  e_f <- p_i * p_j
  out <- (xwn / N - e_f) * sqrt(n_base) / sqrt(e_f * (1 - p_i) * (1 - p_j))
  out[e_f * n_base < 1]  <- NA_real_
  out[!is.finite(out)]   <- NA_real_
  out
}

# DESIGN: two-sided p of the adjusted residual; total rows/tabs are margins, not cells -> NA. Stored in
#   `pvalue` so fmt_color_plan() can gate `color = "contrib"` (which has no CI to gate on), and so the
#   residual stays recoverable at render time with NO new fmt field: |z| = -qnorm(p/2), sign from `var`.
#' @keywords internal
#' @noRd
contrib_pvalue <- function(z, in_totrow, in_tottab, comp) {
  pv   <- 2 * stats::pnorm(-abs(z))
  prot <- if (comp == "all") in_totrow | in_tottab else in_totrow
  pv[prot] <- NA_real_
  pv[!is.finite(pv)] <- NA_real_
  pv
}

# Writes `var` (signed absolute contribution) and, under calc "ctr", `ctr` (= |cell| / group total),
# `pvalue`, `comp_all` and the contrib `color`. Locked by test-calculations.R + test-color-golden.R.
#' @keywords internal
#' @noRd
chi2_write_contrib <- function(tabs, calc, comp, color, col_vars_levels,
                               col_vars_levels_no_tot, is_a_mean, all_col_tot, tot_cols,
                               deff = NULL) {
  do_ctr  <- "ctr" %in% calc
  fmt_nms <- names(tabs)[purrr::map_lgl(tabs, is_fmt)]
  # DESIGN: the signed contribution and the ctr seed are PER SUBTABLE -- `gid` is each row's subtable.
  gid <- dplyr::group_indices(tabs)
  gids <- unique(gid)


  var_after <- purrr::set_names(lapply(fmt_nms, function(nm) get_var(tabs[[nm]])), fmt_nms)
  pval_after <- if (do_ctr) purrr::set_names(lapply(fmt_nms, function(nm) get_pvalue(tabs[[nm]])), fmt_nms)
  elig_col  <- purrr::keep(fmt_nms, function(nm) fmt_var_kind(tabs[[nm]]) != "mean" &&
                             is_real_col_var(get_col_var(tabs[[nm]])))
  # DESIGN: the residual's INFERENCE BASE is ALWAYS the total column's grand cell, at every table shape,
  #   so counts and percentages of the same data give identical residuals -- the residual is a property
  #   of the joint distribution and must not depend on `pct`. Design-honest: the raw n over Rao-Scott's
  #   mean generalized design effect of THIS test -- the delta-bar the omnibus row reports, so colours
  #   and p describe ONE design effect; `deff` is NULL at basis "n", so the raw-n base stands BY
  #   CONSTRUCTION, not by a branch. A FIRST-ORDER correction only.
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
        # A missing delta-bar falls through to the ladder above: effective n if any, else the raw n.
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

      for (L in purrr::map_chr(col_vars_levels[[cv]], rlang::as_name)) {
        ctr_after[[L]] <- dplyr::if_else(is_totrow(tabs[[L]]), 1 / cbr, vbr)
      }
    }

    # DESIGN: protected totals KEEP the seed instead of the divide -- comp = "tab": is_totrow;
    #   comp = "all": grand_totrow(), which degrades to the plain total row with no total table.
    ctr_final <- purrr::set_names(lapply(fmt_nms, function(nm) {
      prot <- if (comp == "tab") is_totrow(tabs[[nm]]) else grand_totrow(tabs[[nm]])
      dplyr::if_else(prot, ctr_after[[nm]], var_after[[nm]] / ctr_after[[nm]])
    }), fmt_nms)

    comp_all_val <- comp[1] == "all"

    if (!is.na(color[1]) && color[1] != "no") {
      color_condition <- switch(color[1],
        "auto"    = c("all", "all_tabs"),
        "all"     = c("row", "col", "all", "all_tabs"),
        "all_pct" = c("all", "all_tabs"))
      want_counts <- color[1] %in% c("auto", "all")
      color_apply <- purrr::keep(fmt_nms, function(nm)
        get_pct_type(tabs[[nm]]) %in% color_condition ||
          (want_counts && fmt_var_kind(tabs[[nm]]) == "count"))
    }
  }

  # ONE write pass on the UNGROUPED table (the precomputed vectors are full-length), then regroup.
  grp <- dplyr::group_vars(tabs)
  drp <- dplyr::group_by_drop_default(tabs)
  res <- dplyr::mutate(dplyr::ungroup(tabs), dplyr::across(where(is_fmt), function(col) {
    nm  <- dplyr::cur_column()
    col <- set_var(col, var_after[[nm]])
    if (do_ctr) {
      col <- set_ctr(col, ctr_final[[nm]])
      # WARNING: set_ctr() alone does not materialise `wn`; see fmt_materialize_wn() (R/fmt_class.R).
      col <- fmt_materialize_wn(col)
      col <- set_pvalue(col, pval_after[[nm]])
      col <- set_comp_all(col, comp_all_val)
      if (nm %in% color_apply) col <- set_color(col, "contrib")
    }
    col
  }))
  if (length(grp)) res <- dplyr::group_by(res, dplyr::across(dplyr::all_of(grp)), .drop = drp)
  res
}
