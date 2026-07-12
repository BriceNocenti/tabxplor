#!/usr/bin/env Rscript
# PURPOSE: Phase 10j-B (B-i, step 2b) -- BYTE-IDENTITY PoC for a base-R/data.table rewrite of the
#   whole-table test marshalling, PROVEN across a shape matrix BEFORE any source edit (the 9d method).
#   Two candidates, each vs the REAL live function via identical():
#     (1) fast_chi2_compute_test() -- rewrites chi2_compute_test()'s long-frame build + engine join-back
#         as plain vector building + a match()-based decode (KEEPS the cheap subtable-grouping select
#         and the small final-assembly dplyr, whose byte order/locale would be the only risky ports).
#     (2) fast_is_a_mean() -- replaces tab_chi2()'s L5545 per-col_var dplyr::select(ungroup(tabs))
#         (the 4.6 % head cost) with a direct get_type() read.
#   agg_chi2()/agg_anova() engines are UNCHANGED (already data.table, 5.6 % -- not a target).
# USAGE (from package root):  Rscript dev/benchmarks/phase10j_tests_parity.R
# dev/benchmarks/ is .Rbuildignore'd (never run by the suite).

pkg <- normalizePath(".", winslash = "/")
suppressMessages(devtools::load_all(pkg, quiet = TRUE))
suppressMessages(library(dplyr))
source(file.path(pkg, "tests", "testthat", "helper-benchmark.R"))
tx <- asNamespace("tabxplor")

# ------------------------------------------------------------------ head replication (capture inputs)
# Faithful copy of tab_chi2()'s head (tab.R L5529-5567) up to chi2_compute_test()'s call, returning
# the exact argument list chi2_compute_test() receives.
capture_inputs <- function(tabs, comp_arg = NULL) {
  gv0 <- tx$tab_get_vars(tabs)
  row_var <- gv0$row_var
  col_vars_levels <- purrr::map(gv0$col_vars_levels, rlang::syms)
  comp <- tx$tab_validate_comp(tabs, comp = ifelse(is.null(comp_arg), "null", comp_arg))
  tabs <- tabs %>% tx$tab_match_comp_and_tottab(comp)
  is_a_mean <- purrr::map_lgl(
    col_vars_levels,
    ~ purrr::map_lgl(dplyr::select(dplyr::ungroup(tabs), !!!.), ~ tx$get_type(.) == "mean") %>% any())
  if (!all(is_a_mean)) tabs <- tabs %>% tx$tab_match_groups_and_totrows() %>% tx$tab_add_totcol_if_no()
  if (comp == "all") tabs <- tabs %>% dplyr::ungroup()
  all_col_tot <- names(col_vars_levels) == "all_col_vars"
  tot_cols_names <- purrr::map_lgl(tabs, tx$is_totcol); tot_cols_names <- names(tot_cols_names[tot_cols_names])
  col_vars_levels_no_tot <- purrr::map(col_vars_levels, ~ purrr::discard(., . %in% tot_cols_names))
  list(tabs = tabs, comp = comp, row_var = row_var, col_vars_levels = col_vars_levels,
       col_vars_levels_no_tot = col_vars_levels_no_tot, is_a_mean = is_a_mean, all_col_tot = all_col_tot)
}

# ------------------------------------------------------------------------------- candidate (1)
fast_chi2_compute_test <- function(tabs, comp, row_var, col_vars_levels,
                                   col_vars_levels_no_tot, is_a_mean, all_col_tot) {
  is_totrow <- tx$is_totrow; is_tottab <- tx$is_tottab; is_fmt <- tx$is_fmt
  get_n <- tx$get_n; get_mean <- tx$get_mean; get_var <- tx$get_var; get_type <- tx$get_type
  is_totcol <- tx$is_totcol; agg_chi2 <- tx$agg_chi2; agg_anova <- tx$agg_anova
  new_test_tibble <- tx$new_test_tibble

  mask2 <- if (comp == "all") !is_totrow(tabs) & !is_tottab(tabs) else !is_totrow(tabs)
  n_rows2 <- sum(mask2)

  tabs2_grp    <- dplyr::select(tabs, !where(is_fmt))[mask2, ]           # KEEP (cheap, byte-safe order)
  subtab_idx   <- dplyr::group_indices(tabs2_grp)
  subtab_keys  <- dplyr::group_keys(tabs2_grp)
  tab_vars_chr <- names(subtab_keys)

  factor_cvs <- names(col_vars_levels)[!is_a_mean & !all_col_tot]
  mean_cvs   <- names(col_vars_levels)[ is_a_mean & !all_col_tot]

  # --- Chi2 (factor col_vars): build engine vectors directly; then match engine output onto the FULL
  # (table_id, cv, subtab) tuple set (agg_chi2 DROPS degenerate tables -> NA rows, as distinct+left_join). ---
  chi2_rows <- NULL
  if (length(factor_cvs) > 0 && n_rows2 > 0) {
    tid <- list(); rid <- list(); cid <- list(); ov <- list()
    map_tid <- character(0); map_cv <- character(0); map_sub <- integer(0)
    for (cv in factor_cvs) {
      lv_cols <- purrr::map_chr(col_vars_levels_no_tot[[cv]], rlang::as_name)
      if (length(lv_cols) == 0) next
      M   <- vapply(lv_cols, function(cc) as.double(get_n(tabs[[cc]])[mask2]), double(n_rows2))
      ncM <- ncol(M)
      tid[[cv]] <- paste(cv, rep(subtab_idx, times = ncM), sep = "\r")
      rid[[cv]] <- rep(seq_len(n_rows2), times = ncM)
      cid[[cv]] <- rep(seq_len(ncM), each = n_rows2)
      ov[[cv]]  <- as.vector(M)
      usub <- unique(subtab_idx)                                   # distinct-subtab, first-appearance
      map_tid <- c(map_tid, paste(cv, usub, sep = "\r"))
      map_cv  <- c(map_cv, rep(cv, length(usub))); map_sub <- c(map_sub, usub)
    }
    if (length(ov) > 0) {
      TID <- unlist(tid, use.names = FALSE); RID <- unlist(rid, use.names = FALSE)
      CID <- unlist(cid, use.names = FALSE); OV  <- unlist(ov,  use.names = FALSE)
      res <- agg_chi2(TID, RID, CID, OV, correct = TRUE)
      rt  <- res$tables
      j   <- match(map_tid, rt$table_id)                           # NA where engine dropped a table
      chi2_rows <- tibble::tibble(
        subtab = map_sub, col_var = map_cv, test = "chi2",
        statistic = rt$statistic[j], df1 = as.double(rt$df[j]), df2 = NA_real_,
        pvalue = rt$pvalue[j], n = as.double(rt$n[j]), variance = NA_real_, min_e = rt$min_e[j])
    }
  }

  # --- ANOVA (mean col_vars) ---
  anova_rows <- NULL
  if (length(mean_cvs) > 0 && n_rows2 > 0) {
    tid <- list(); gid <- list(); nn <- list(); mm <- list(); vv <- list()
    map_tid <- character(0); map_cv <- character(0); map_sub <- integer(0)
    for (cv in mean_cvs) {
      cols <- purrr::map_chr(col_vars_levels[[cv]], rlang::as_name)
      keep <- vapply(cols, function(.x) get_type(tabs[[.x]]) == "mean" && !any(is_totcol(tabs[[.x]])),
                     logical(1))
      col  <- cols[keep][1]
      if (is.na(col)) next
      tid[[cv]] <- paste(cv, subtab_idx, sep = "\r")
      gid[[cv]] <- seq_len(n_rows2)
      nn[[cv]]  <- as.double(get_n(tabs[[col]])[mask2])
      mm[[cv]]  <- get_mean(tabs[[col]])[mask2]
      vv[[cv]]  <- get_var(tabs[[col]])[mask2]
      usub <- unique(subtab_idx)
      map_tid <- c(map_tid, paste(cv, usub, sep = "\r"))
      map_cv  <- c(map_cv, rep(cv, length(usub))); map_sub <- c(map_sub, usub)
    }
    if (length(tid) > 0) {
      TID <- unlist(tid, use.names = FALSE); GID <- unlist(gid, use.names = FALSE)
      NN  <- unlist(nn, use.names = FALSE); MM <- unlist(mm, use.names = FALSE); VV <- unlist(vv, use.names = FALSE)
      resA <- agg_anova(TID, GID, NN, MM, VV)
      j    <- match(map_tid, resA$table_id)                        # NA where engine dropped a table
      welch <- tibble::tibble(
        subtab = map_sub, col_var = map_cv, test = "F_welch",
        statistic = resA$statistic[j], df1 = resA$df1[j], df2 = resA$df2[j],
        pvalue = resA$pvalue[j], n = as.double(resA$n[j]), variance = NA_real_, min_e = NA_real_)
      classic <- tibble::tibble(
        subtab = map_sub, col_var = map_cv, test = "F_classic",
        statistic = resA$statistic_classic[j], df1 = resA$df1_classic[j], df2 = resA$df2_classic[j],
        pvalue = resA$pvalue_classic[j], n = as.double(resA$n[j]), variance = NA_real_, min_e = NA_real_)
      anova_rows <- dplyr::bind_rows(welch, classic)
    }
  }

  # --- assembly: KEEP the original dplyr (cheap, byte-safe row/col order) ---
  test_tbl <- dplyr::bind_rows(chi2_rows, anova_rows)
  if (nrow(test_tbl) == 0) {
    test_tbl <- new_test_tibble()
  } else {
    subtab_keys2 <- dplyr::mutate(subtab_keys, subtab = dplyr::row_number())
    test_tbl <- test_tbl %>%
      dplyr::arrange(.data$subtab, .data$col_var, .data$test) %>%
      dplyr::left_join(subtab_keys2, by = "subtab") %>%
      dplyr::mutate(row_var = !!row_var) %>%
      dplyr::select(-"subtab") %>%
      dplyr::relocate(tidyselect::any_of(tab_vars_chr), "row_var", "col_var")
  }
  test_tbl
}

# ------------------------------------------------------------------------------- candidate (2)
fast_is_a_mean <- function(tabs, col_vars_levels) {
  get_type <- tx$get_type
  purrr::map_lgl(col_vars_levels, function(levs) {
    cols <- purrr::map_chr(levs, rlang::as_name)
    any(vapply(cols, function(cc) get_type(tabs[[cc]]) == "mean", logical(1)))
  })
}

# =================================================================== shape matrix + parity
gss <- forcats::gss_cat
gss$year <- factor(gss$year)
gssw <- gss; gssw$w <- as.double(gss$tvhours); gssw$w[is.na(gssw$w)] <- 1  # a positive weight
gss2 <- dplyr::filter(gss, marital %in% c("Married", "Divorced"),
                      race %in% c("White", "Black")) |>
  dplyr::mutate(marital = forcats::fct_drop(marital), race = forcats::fct_drop(race))  # 2x2 (Yates)

# builder for one shape -> a pre-chi2 (chi2 = FALSE) single-row_var tab, or NULL if invalid
build_shape <- function(colspec, comp, ntab, wtd, yates) {
  d  <- if (yates) gss2 else if (wtd) gssw else gss
  rv <- if (yates) rlang::expr(marital) else rlang::expr(rincome)
  cv <- switch(colspec,
               factor = if (yates) rlang::expr(race) else rlang::expr(c(race, relig)),
               mixed  = rlang::expr(c(race, tvhours)),
               mean   = rlang::expr(c(age, tvhours)))
  tv <- switch(as.character(ntab), "0" = NULL, "1" = rlang::expr(partyid),
               "2" = rlang::expr(c(partyid, denom)))
  wt <- if (wtd) rlang::expr(w) else NULL
  args <- list(quote(d), rv, cv)
  if (!is.null(tv)) args$tab_vars <- tv
  if (!is.null(wt)) args$wt <- wt
  args <- c(args, list(pct = "row", chi2 = FALSE, color = "no"))
  fn <- if (colspec == "mean") tab_num else tab
  # tab_num signature: (data, row_var, col_vars, tab_vars, ...); tab: (data, row_vars, col_vars, ...)
  call <- rlang::call2(fn, !!!args)
  out <- tryCatch(suppressWarnings(rlang::eval_tidy(call, data = list(d = d))),
                  error = function(e) structure("err", msg = conditionMessage(e)))
  out
}

grid <- expand.grid(
  colspec = c("factor", "mixed", "mean"),
  comp    = c("tab", "all"),
  ntab    = c(0L, 1L, 2L),
  wtd     = c(FALSE, TRUE),
  yates   = c(FALSE, TRUE),
  stringsAsFactors = FALSE)
# prune: yates only with factor colspec + ntab 0 + unweighted (a clean 2x2); mean has no Yates
grid <- grid[!(grid$yates & (grid$colspec != "factor" | grid$ntab != 0L | grid$wtd)), ]

cat("shapes to test:", nrow(grid), "\n")
results <- vector("list", nrow(grid))
for (i in seq_len(nrow(grid))) {
  g <- grid[i, ]
  t0 <- tryCatch(build_shape(g$colspec, g$comp, g$ntab, g$wtd, g$yates),
                 error = function(e) structure("err", msg = conditionMessage(e)))
  if (is.list(t0) || inherits(t0, "tbl_df")) {
    inp <- tryCatch(capture_inputs(t0, comp_arg = g$comp), error = function(e) NULL)
    if (is.null(inp)) { results[[i]] <- list(ok_chi2 = NA, ok_mean = NA, note = "capture-fail"); next }
    cur <- tryCatch(tx$chi2_compute_test(inp$tabs, inp$comp, inp$row_var, inp$col_vars_levels,
                                         inp$col_vars_levels_no_tot, inp$is_a_mean, inp$all_col_tot),
                    error = function(e) structure("err", msg = conditionMessage(e)))
    fst <- tryCatch(fast_chi2_compute_test(inp$tabs, inp$comp, inp$row_var, inp$col_vars_levels,
                                           inp$col_vars_levels_no_tot, inp$is_a_mean, inp$all_col_tot),
                    error = function(e) structure("err", msg = conditionMessage(e)))
    ok_chi2 <- isTRUE(identical(cur, fst))
    iam_cur <- inp$is_a_mean
    iam_fst <- tryCatch(fast_is_a_mean(inp$tabs, inp$col_vars_levels), error = function(e) NULL)
    ok_mean <- isTRUE(identical(unname(iam_cur), unname(iam_fst)))
    note <- if (inherits(cur, "err") || inherits(fst, "err")) "err-in-fn" else ""
    results[[i]] <- list(ok_chi2 = ok_chi2, ok_mean = ok_mean, note = note)
  } else {
    results[[i]] <- list(ok_chi2 = NA, ok_mean = NA, note = paste0("build-skip(", attr(t0, "msg"), ")"))
  }
}

R <- dplyr::bind_cols(grid, dplyr::bind_rows(lapply(results, function(z)
  tibble::tibble(ok_chi2 = z$ok_chi2, ok_mean = z$ok_mean, note = z$note))))
tested <- R[!is.na(R$ok_chi2), ]
cat(sprintf("\nchi2_compute_test identical(): %d / %d\n", sum(tested$ok_chi2), nrow(tested)))
cat(sprintf("is_a_mean       identical(): %d / %d\n", sum(tested$ok_mean), nrow(tested)))
fails <- R[(!is.na(R$ok_chi2) & !R$ok_chi2) | (!is.na(R$ok_mean) & !R$ok_mean) |
             grepl("fail|err", R$note), ]
if (nrow(fails)) { cat("\n---- FAILURES / skips ----\n"); print(fails) } else cat("ALL PARITY GREEN\n")

# =================================================================== timing (isolated speedup)
cat("\n==== isolated speedup on a chunky factor shape (rincome x (race,relig), 2 tab_vars) ====\n")
tt <- build_shape("factor", "tab", 2L, FALSE, FALSE)
inp <- capture_inputs(tt, comp_arg = "tab")
cur_t <- benchmark_measure(function() tx$chi2_compute_test(
  inp$tabs, inp$comp, inp$row_var, inp$col_vars_levels,
  inp$col_vars_levels_no_tot, inp$is_a_mean, inp$all_col_tot), iterations = 200L)
fst_t <- benchmark_measure(function() fast_chi2_compute_test(
  inp$tabs, inp$comp, inp$row_var, inp$col_vars_levels,
  inp$col_vars_levels_no_tot, inp$is_a_mean, inp$all_col_tot), iterations = 200L)
cat(sprintf("chi2_compute_test: current %.5f s | fast %.5f s | speedup %.2fx\n",
            cur_t$median_s, fst_t$median_s, cur_t$median_s / fst_t$median_s))

iam_cur_t <- benchmark_measure(function() purrr::map_lgl(
  inp$col_vars_levels, ~ purrr::map_lgl(dplyr::select(dplyr::ungroup(inp$tabs), !!!.),
                                        ~ tx$get_type(.) == "mean") %>% any()), iterations = 200L)
iam_fst_t <- benchmark_measure(function() fast_is_a_mean(inp$tabs, inp$col_vars_levels), iterations = 200L)
cat(sprintf("is_a_mean        : current %.5f s | fast %.5f s | speedup %.2fx\n",
            iam_cur_t$median_s, iam_fst_t$median_s, iam_cur_t$median_s / iam_fst_t$median_s))
