#!/usr/bin/env Rscript
# PURPOSE: Phase 9d proof-of-concept -- prove the base-R/matrix rewrite of tab_plain()'s leaf
#          arithmetic (Region E pct/tot_n + Region F diff/ratio/rr/or) and the total-row group-sums
#          (Regions B/C) is BYTE-IDENTICAL to the current data.table code, and measure the speedup,
#          BEFORE editing R/tab.R. Each block is tested in ISOLATION on identical inputs:
#            - Region E  : verbatim current data.table idiom (copied from tab.R:2958-3004) vs matrix.
#            - Region F  : the REAL tabxplor:::tab_apply_reference() vs a matrix reimplementation.
#            - Regions B/C: verbatim current idiom (copied from tab.R:2835-2888) vs base-`sum()` group.
#          Parity = identical() (NOT all.equal) across a shape grid. Timing = per-block (the gate) +
#          end-to-end tab_plain() (the denominator for the >=5% per-row_var-build gate).
# DECISIVE TRAP (spike-confirmed): the current code group-sums via purrr::map(.SD, sum, na.rm=TRUE) =
#          base::sum() (LONG-DOUBLE accumulator). rowsum()/data.table-gforce use a plain-DOUBLE
#          accumulator -> 1-ULP drift -> identical() FALSE. So B/C use base sum() per split() group.
# ROLE: Standalone; dev/benchmarks/ is .Rbuildignore'd -> never run by the test suite or R CMD check.
# USAGE (from package root): source("dev/benchmarks/phase9d_leaf_math_parity.R", encoding = "UTF-8")

pkg <- normalizePath(".", winslash = "/")
suppressMessages(devtools::load_all(pkg, quiet = TRUE))
source(file.path(pkg, "tests", "testthat", "helper-benchmark.R"))  # benchmark_measure()
suppressMessages(library(data.table))

`%||%` <- function(a, b) if (is.null(a)) b else a

# ============================================================================================
# 0. Build a representative POST-DCAST base table (no totals, no "Total" column) for a shape,
#    by replicating tab_plain()'s factor-path dcast (tab.R:2771-2791) + na policy exactly.
# ============================================================================================
mk_base <- function(data, row_var, col_var, tab_vars = character(), weighted = FALSE, na = "drop") {
  dt   <- as.data.table(data)
  keys <- c(tab_vars, row_var)
  if (weighted) {
    long <- dt[, list(n = .N, wn = sum(as.numeric(w), na.rm = TRUE)), keyby = c(keys, col_var)]
  } else {
    long <- dt[, list(n = .N),                                        keyby = c(keys, col_var)]
  }
  data.table::setnames(long, col_var, "col_var")
  tabs <- data.table::dcast(long, formula = stats::as.formula(paste(paste(keys, collapse = "+"), "~ col_var")),
                            value.var = if (weighted) c("n", "wn") else "n", fill = 0)
  # coerce key cols to factor (tab.R:2800-2804)
  for (v in keys) if (!is.factor(tabs[[v]])) tabs[[v]] <- forcats::as_factor(tabs[[v]])
  # NA columns (tab.R:2807-2814)
  na_cols <- names(tabs) %in% c("n_NA", "wn_NA", "NA")
  if (any(na_cols)) {
    if (na == "drop") suppressWarnings(tabs[, c("n_NA", "wn_NA", "NA") := NULL])
    else data.table::setcolorder(tabs, c(names(tabs)[!na_cols], names(tabs)[na_cols]))
  }
  # NA rows (tab.R:2816-2830)
  na_rows <- tabs[, keys, with = FALSE][, Reduce(`|`, lapply(.SD, is.na))]
  if (any(na_rows %||% FALSE)) {
    if (na == "drop") tabs <- tabs[!na_rows]
    else {
      data.table::setorderv(tabs, keys, na.last = TRUE)
      tabs[, (keys) := lapply(.SD, forcats::fct_na_value_to_level, level = "NA"), .SDcols = keys]
    }
  }
  tabs[]
}

# num_cols BEFORE Region D (the n_/wn_ value cells; no "Total" yet)
value_cols <- function(tabs, keys) setdiff(names(tabs), keys)

# ============================================================================================
# 1. REGIONS B/C -- verbatim current vs base-sum() matrix
# ============================================================================================

# ---- verbatim current (copied from tab.R:2835-2888) -----------------------------------------
current_BC <- function(tabs, row_var, tab_vars, totaltab, tot) {
  tabs           <- data.table::copy(tabs)
  tab_row_names  <- c(tab_vars, row_var)
  num_cols       <- value_cols(tabs, tab_row_names)

  if (totaltab %in% c("table", "line")) {
    tabs_totaltab <- switch(
      totaltab,
      "table" = tabs[, c(purrr::set_names(rep("Total", length(tab_vars)), tab_vars),
                         purrr::map(.SD, sum, na.rm = TRUE)),
                     .SDcols = num_cols, keyby = row_var],
      "line"  = tabs[, c(purrr::set_names(rep("Total", length(tab_row_names)), tab_row_names),
                         purrr::map(.SD, sum, na.rm = TRUE)),
                     .SDcols = num_cols]
    )
    tabs <- rbind(tabs, tabs_totaltab)
    data.table::setorderv(tabs, tab_row_names)
  }

  if ("row" %in% tot) {
    if (length(tab_vars) != 0) {
      group_vars <- rev(purrr::accumulate(tab_vars, ~ c(.x, .y)))
      total_vars <- purrr::map(group_vars, ~ c(tab_vars[!tab_vars %in% .], row_var))
    } else {
      group_vars <- list(character())
      total_vars <- row_var
    }
    tabs_tot <- purrr::map2_dfr(
      group_vars, total_vars,
      ~ tabs[, c(purrr::set_names(rep("Total", length(.y)), .y), purrr::map(.SD, sum, na.rm = TRUE)),
             .SDcols = num_cols, keyby = eval(.x)]
    )
    tabs_tot <- data.table::setorderv(tabs_tot, tab_row_names) |> unique()

    if (totaltab == "line") {
      no_totaltab_line <- dplyr::select(tabs_tot, tidyselect::all_of(tab_row_names)) |>
        dplyr::transmute(total_line = dplyr::if_any(tidyselect::everything(), ~ . != "Total")) |>
        tibble::deframe() |> which()
      tabs_tot <- tabs_tot[no_totaltab_line, ]
    }
    tabs <- rbind(tabs, tabs_tot)
    data.table::setorderv(tabs, tab_row_names)
  }
  tabs[]
}

# ---- shared base-sum() group builder --------------------------------------------------------
# DESIGN: base::sum() per split() group -- NOT rowsum()/gforce (plain-double accumulator drifts).
build_total_rows <- function(tabs, keys, totvars, tab_row_names, num_cols) {
  n <- nrow(tabs)
  if (length(keys) == 0) { idx <- list(seq_len(n)); kf <- NULL } else {
    key <- do.call(paste, c(lapply(keys, function(k) as.character(tabs[[k]])), sep = "\r"))
    f   <- factor(key, levels = unique(key))
    idx <- split(seq_len(n), f)
    kf  <- as.data.frame(do.call(rbind, strsplit(levels(f), "\r", fixed = TRUE)),
                         stringsAsFactors = FALSE)
    names(kf) <- keys
  }
  summ <- lapply(num_cols, function(cc) {
    col <- tabs[[cc]]; fv <- if (is.integer(col)) integer(1) else numeric(1)
    vapply(idx, function(ii) sum(col[ii], na.rm = TRUE), fv)
  })
  names(summ) <- num_cols
  lab <- lapply(tab_row_names, function(v)
    if (!is.null(kf) && v %in% names(kf)) kf[[v]] else rep("Total", length(idx)))
  names(lab) <- tab_row_names
  out <- cbind(as.data.frame(lab, stringsAsFactors = FALSE),
               as.data.frame(summ, stringsAsFactors = FALSE))
  out[, c(tab_row_names, num_cols), drop = FALSE]
}

# expand factor levels + rbind + setorderv, matching data.table rbind(keyby-factor, "Total"-char)
finalize_total_rows <- function(tabs, extra, cols_get_total, tab_row_names) {
  for (v in cols_get_total) if (v %in% names(tabs))
    tabs[[v]] <- factor(tabs[[v]], levels = unique(c(levels(tabs[[v]]), "Total")))
  for (v in tab_row_names)
    extra[[v]] <- factor(extra[[v]], levels = levels(tabs[[v]]))
  out <- rbind(tabs, data.table::as.data.table(extra))
  data.table::setorderv(out, tab_row_names)
  out[]
}

matrix_BC <- function(tabs, row_var, tab_vars, totaltab, tot) {
  tabs          <- data.table::copy(tabs)
  tab_row_names <- c(tab_vars, row_var)
  num_cols      <- value_cols(tabs, tab_row_names)

  if (totaltab %in% c("table", "line")) {
    if (totaltab == "table") { keys <- row_var;        totvars <- tab_vars }
    else                     { keys <- character();    totvars <- tab_row_names }
    extra <- build_total_rows(tabs, keys, totvars, tab_row_names, num_cols)
    tabs  <- finalize_total_rows(tabs, extra, totvars, tab_row_names)
  }

  if ("row" %in% tot) {
    if (length(tab_vars) != 0) {
      group_vars <- rev(purrr::accumulate(tab_vars, ~ c(.x, .y)))
      total_vars <- purrr::map(group_vars, ~ c(tab_vars[!tab_vars %in% .], row_var))
    } else {
      group_vars <- list(character()); total_vars <- list(row_var)
    }
    parts <- purrr::map2(group_vars, total_vars,
                         ~ build_total_rows(tabs, .x, .y, tab_row_names, num_cols))
    tabs_tot <- do.call(rbind, parts)
    cols_get_total <- unique(unlist(total_vars))
    # dedup like setorderv(...) |> unique(): sort keys, drop duplicate total rows
    tabs_tot <- tabs_tot[do.call(order, tabs_tot[tab_row_names]), , drop = FALSE]
    tabs_tot <- tabs_tot[!duplicated(tabs_tot), , drop = FALSE]
    if (totaltab == "line") {
      keep <- Reduce(`|`, lapply(tab_row_names, function(v) as.character(tabs_tot[[v]]) != "Total"))
      tabs_tot <- tabs_tot[keep, , drop = FALSE]
    }
    tabs <- finalize_total_rows(tabs, tabs_tot, cols_get_total, tab_row_names)
  }
  tabs[]
}

# ============================================================================================
# 2. REGION D -- add the "Total" column + split tabs_n / tabs_wn (KEPT as-is, tab.R:2905-2949)
#    Returns list(tabs_n, tabs_wn, cols, text_vars) matching tab_plain's locals.
# ============================================================================================
region_D <- function(tabs, tab_row_names, tot, weighted) {
  tabs      <- data.table::copy(tabs)
  text_vars <- !purrr::map_lgl(tabs, is.numeric); text_vars <- text_vars[text_vars]
  if (!weighted) {
    if ("col" %in% tot)
      tabs[, "Total" := as.integer(rowSums(tabs[, -names(text_vars), with = FALSE]))]
    tabs_n <- tabs; tabs_wn <- NULL
  } else {
    n_index  <- stringr::str_detect(names(tabs), "^n_")  | (!purrr::map_lgl(tabs, is.numeric))
    wn_index <- stringr::str_detect(names(tabs), "^wn_") | (!purrr::map_lgl(tabs, is.numeric))
    tabs_n  <- data.table::setnames(tabs[, n_index,  with = FALSE], \(.x) stringr::str_remove(.x, "^n_"))
    tabs_wn <- data.table::setnames(tabs[, wn_index, with = FALSE], \(.x) stringr::str_remove(.x, "^wn_"))
    tabs_wn[, (names(tabs_wn)) := purrr::map(.SD, as.double)]
    if ("col" %in% tot) {
      tabs_n [, "Total" := as.integer(rowSums(tabs_n [, -names(text_vars), with = FALSE]))]
      tabs_wn[, "Total" := rowSums(tabs_wn[, -names(text_vars), with = FALSE])]
    }
  }
  cols <- purrr::map_lgl(tabs_n, is.numeric); cols <- cols[cols]
  list(tabs_n = tabs_n, tabs_wn = tabs_wn, cols = cols, text_vars = text_vars)
}

# ============================================================================================
# 3. REGION E -- verbatim current vs matrix
# ============================================================================================
current_E <- function(tabs_n, tabs_wn, pct, tab_vars, cols) {
  nm <- names(cols)
  if (!is.null(tabs_wn)) tabs_pct <- data.table::copy(tabs_wn) else {
    tabs_pct <- data.table::copy(tabs_n)
    tabs_pct[, (nm) := purrr::map(.SD, as.double), .SDcols = nm]
  }
  switch(pct,
    "row"      = tabs_pct[, (nm) := purrr::map(.SD, ~ . / eval(rlang::sym("Total"))), .SDcols = nm],
    "col"      = tabs_pct[, (nm) := purrr::map(.SD, ~ . / dplyr::last(.)), by = tab_vars, .SDcols = nm],
    "all"      = tabs_pct[, (nm) := purrr::map(.SD, ~ . / dplyr::last(eval(rlang::sym("Total")))), by = tab_vars, .SDcols = nm],
    "all_tabs" = tabs_pct[, (nm) := purrr::map(.SD, ~ . / dplyr::last(eval(rlang::sym("Total")))), .SDcols = nm]
  )
  tabs_pct[, (nm) := purrr::map(.SD, ~ tidyr::replace_na(., 0)), .SDcols = nm]

  tabs_totn <- data.table::copy(tabs_n)
  tabs_totn[, (nm) := purrr::map(.SD, as.double), .SDcols = nm]
  switch(pct,
    "row"      = tabs_totn[, (nm) := purrr::map(.SD, ~ as.double(eval(rlang::sym("Total")))), .SDcols = nm],
    "col"      = tabs_totn[, (nm) := purrr::map(.SD, ~ rep(dplyr::last(.), length(.))), by = tab_vars, .SDcols = nm],
    "all"      = tabs_totn[, (nm) := purrr::map(.SD, ~ rep(dplyr::last(eval(rlang::sym("Total"))), length(.))), by = tab_vars, .SDcols = nm],
    "all_tabs" = tabs_totn[, (nm) := purrr::map(.SD, ~ rep(dplyr::last(eval(rlang::sym("Total"))), length(.))), .SDcols = nm]
  )
  list(pct = tabs_pct[], tot_n = tabs_totn[])
}

leaf_wide_pct <- function(tabs_n, tabs_wn, pct, tab_vars, cols) {
  nm <- names(cols); n <- nrow(tabs_n); k <- length(nm)
  grp <- if (length(tab_vars) == 0) rep(1L, n) else {
    key <- do.call(paste, c(lapply(tab_vars, function(v) as.character(tabs_n[[v]])), sep = "\r"))
    match(key, unique(key))
  }
  grp_last <- stats::ave(seq_len(n), grp, FUN = max)
  M_pct  <- if (!is.null(tabs_wn)) as.matrix(tabs_wn[, nm, with = FALSE]) else
                                   as.matrix(tabs_n[,  nm, with = FALSE]) * 1.0
  M_totn <- as.matrix(tabs_n[, nm, with = FALSE]) * 1.0
  Dmat <- function(M) switch(pct,
    "row"      = matrix(M[, "Total"],            n, k),
    "col"      = M[grp_last, , drop = FALSE],
    "all"      = matrix(M[grp_last, "Total"],    n, k),
    "all_tabs" = matrix(M[n,        "Total"],    n, k))
  P <- M_pct / Dmat(M_pct); P[is.na(P)] <- 0
  Tn <- Dmat(M_totn)
  wb <- function(src, M2) {
    dt <- data.table::copy(src)
    dt[, (nm) := lapply(seq_len(k), function(j) M2[, j])]
    dt[]
  }
  list(pct   = wb(if (!is.null(tabs_wn)) tabs_wn else tabs_n, P),
       tot_n = wb(tabs_n, Tn))
}

# ============================================================================================
# 4. REGION F -- REAL tab_apply_reference() vs matrix reimplementation
# ============================================================================================
matrix_apply_reference <- function(tabs, tabs_pct, ref, ref2, comp, OR, color, pct,
                                   tab_row_names, tab_vars, row_var, tottab_vector,
                                   totrow_vector, cols) {
  nm <- names(cols); n <- nrow(tabs_pct); k <- length(nm)
  P  <- as.matrix(tabs_pct[, nm, with = FALSE]) * 1.0
  tabs_diff <- data.table::copy(tabs_pct); tabs_mean <- data.table::copy(tabs_pct)   # init (tab.R:3202-3203)
  refrows <- NULL; refcols_vector <- NULL; tabs_rr <- NULL; tabs_or <- NULL
  tv_chr <- as.character(tab_vars)

  grp_comp <- if (comp == "tab" && length(tv_chr) != 0)
    do.call(paste, c(lapply(tv_chr, function(v) as.character(tabs[[v]])), sep = "\r")) else rep(1L, n)
  ref_abs <- function(refr) {
    out <- rep(NA_integer_, n)
    for (rows in split(seq_len(n), grp_comp)) {
      p <- which(refr[rows])[1]; if (!is.na(p)) out[rows] <- rows[p]
    }
    out
  }
  wb <- function(M2) { dt <- data.table::copy(tabs_pct); dt[, (nm) := lapply(seq_len(k), \(j) M2[, j])]; dt[] }

  if (pct == "row") {
    refrows <- calculate_refrows(tabs, ref, comp, tab_row_names, tab_vars, row_var,
                                 tottab_vector, totrow_vector, num_names = nm)
    ra   <- ref_abs(refrows)
    Pref <- P[ra, , drop = FALSE]
    tabs_diff <- wb(P - Pref)
    tabs_mean <- wb(P / Pref)

    if (OR %in% c("OR", "OR_pct", "or", "or_pct") | color %in% c("or", "OR")) {
      refcols <- dplyr::nth(nm, diff_index(ref2, row_var = dplyr::pull(tabs_pct, !!row_var),
                                           num_names = nm, pct = "col"))
      if (length(refcols) != 0 && !is.na(refcols)) {
        refcols_vector <- nm == refcols
        RR <- P / P[, refcols]
      } else {
        warning("no ref2 column matched (matrix)"); RR <- matrix(NA_real_, n, k)
      }
      tabs_rr <- wb(RR)
      tabs_or <- wb(RR / RR[ra, , drop = FALSE])
    }
  }

  if (pct == "col") {
    refcols <- dplyr::nth(nm, diff_index(ref, num_names = nm, pct = pct))
    refcols_vector <- nm == refcols   # set BEFORE the if (tab.R:3303): all-NA on no-match
    if (length(refcols) != 0 && !is.na(refcols)) {
      tabs_diff <- wb(P - P[, refcols])
      tabs_mean <- wb(P / P[, refcols])
    } else {
      warning("no ref column matched (matrix)")
      tabs_diff <- wb(matrix(NA_real_, n, k)); tabs_mean <- wb(matrix(NA_real_, n, k))
    }
    if (OR %in% c("OR", "OR_pct", "or", "or_pct") | color %in% c("or", "OR")) {
      refrows <- calculate_refrows(tabs, ref2, comp, tab_row_names, tab_vars, row_var,
                                   tottab_vector, totrow_vector, num_names = nm)
      ra <- ref_abs(refrows)
      RR <- P / P[ra, , drop = FALSE]
      tabs_rr <- wb(RR)
      tabs_or <- if (length(refcols) != 0 && !is.na(refcols)) wb(RR / RR[, refcols]) else wb(matrix(NA_real_, n, k))
    }
  }

  list(diff = tabs_diff, ratio = tabs_mean, rr = tabs_rr, or = tabs_or,
       refcols_vector = refcols_vector, refrows = refrows)
}

# ============================================================================================
# helpers to derive the vectors that tab_apply_reference() needs (totrow/tottab), from a wide
# table already carrying total rows/table. Mirrors tab_plain (tab.R:2890-2900).
# ============================================================================================
mk_vectors <- function(tabs, row_var, tab_vars) {
  totrow <- as.character(tabs[[row_var]]) == "Total"
  tottab <- if (length(tab_vars) == 0) rep(FALSE, nrow(tabs)) else
    Reduce(`&`, lapply(tab_vars, function(v) as.character(tabs[[v]]) == "Total"))
  list(totrow = totrow, tottab = tottab)
}

# ============================================================================================
# 5. PARITY GRID
# ============================================================================================
q <- function(x) suppressWarnings(suppressMessages(x))

gss <- forcats::gss_cat
gss$w <- ((as.integer(gss$marital) * 3L + as.integer(gss$race)) %% 5L) + 1
gss$marital[seq(1L, nrow(gss), by = 500L)] <- NA

# a compact but representative grid
shapes <- local({
  base <- expand.grid(
    pct      = c("row", "col", "all", "all_tabs"),
    comp     = c("tab", "all"),
    OR       = c("no", "OR"),
    ntab     = c(0L, 1L, 2L),
    weighted = c(FALSE, TRUE),
    na       = c("drop", "keep"),
    totaltab = c("line", "table", "no"),
    ref      = c("tot", "first", "3", "zzz_nomatch"),
    stringsAsFactors = FALSE
  )
  # prune: OR only meaningful with pct row/col ; all/all_tabs use ref=tot ; totaltab requires ntab>0
  base <- base[!(base$OR == "OR" & !base$pct %in% c("row", "col")), ]
  base <- base[!(base$pct %in% c("all", "all_tabs") & base$ref != "tot"), ]
  base <- base[!(base$ntab == 0L & base$totaltab != "no"), ]
  base <- base[!(base$ntab > 0L  & base$totaltab == "no"), ]
  base <- base[!(base$comp == "all" & base$ntab == 0L), ]  # comp=all needs tab_vars + a total table
  base
})

col_var <- "race"
run_shape <- function(s) {
  row_var  <- "marital"
  tab_vars <- if (s$ntab == 0L) character() else c("relig", "partyid")[seq_len(s$ntab)]
  tab_row_names <- c(tab_vars, row_var)
  tot <- if (s$pct == "col") c("row", "col") else c("row", "col")  # both totals (default)
  totaltab <- if (s$ntab == 0L) "no" else s$totaltab

  base <- q(mk_base(gss, row_var, col_var, tab_vars, weighted = s$weighted, na = s$na))
  if (nrow(base) == 0) return(NULL)

  # --- B/C parity ---
  bc_cur <- q(current_BC(base, row_var, tab_vars, totaltab, tot))
  bc_mx  <- q(matrix_BC (base, row_var, tab_vars, totaltab, tot))
  ok_bc  <- isTRUE(all.equal(as.data.frame(bc_cur), as.data.frame(bc_mx))) &&
            identical(lapply(bc_cur, class), lapply(bc_mx, class))

  # --- D (shared) on the current B/C output ---
  D <- region_D(bc_cur, tab_row_names, tot, s$weighted)
  vecs <- mk_vectors(D$tabs_n, row_var, tab_vars)

  # --- E parity ---
  e_cur <- q(current_E   (D$tabs_n, D$tabs_wn, s$pct, tab_vars, D$cols))
  e_mx  <- q(leaf_wide_pct(D$tabs_n, D$tabs_wn, s$pct, tab_vars, D$cols))
  ok_e  <- identical(e_cur$pct, e_mx$pct) && identical(e_cur$tot_n, e_mx$tot_n)

  # --- F parity (on the current pct) ---
  ok_f <- NA
  if (s$pct %in% c("row", "col")) {   # tab_apply_reference() is only called for row/col (tab.R:3008)
    # DIRECT call (NOT do.call): a bare rlang::sym() in a do.call arg list is spliced into the
    # constructed call and evaluated as a VARIABLE -> "object 'marital' not found".
    call_ref <- function(fn) fn(
      tabs = bc_cur, tabs_pct = e_cur$pct, ref = s$ref, ref2 = "first", comp = s$comp,
      OR = s$OR, color = "no", pct = s$pct, tab_row_names = tab_row_names,
      tab_vars = rlang::syms(tab_vars), row_var = rlang::sym(row_var),
      tottab_vector = vecs$tottab, totrow_vector = vecs$totrow, cols = D$cols)
    f_cur <- q(call_ref(tab_apply_reference))
    f_mx  <- q(call_ref(matrix_apply_reference))
    ok_f <- identical(f_cur$diff, f_mx$diff) && identical(f_cur$ratio, f_mx$ratio) &&
            identical(f_cur$rr, f_mx$rr) && identical(f_cur$or, f_mx$or) &&
            identical(f_cur$refcols_vector, f_mx$refcols_vector) &&
            identical(f_cur$refrows, f_mx$refrows)
  }
  data.frame(pct = s$pct, comp = s$comp, OR = s$OR, ntab = s$ntab, wt = s$weighted,
             na = s$na, totaltab = totaltab, ref = s$ref,
             BC = ok_bc, E = ok_e, F = ok_f, stringsAsFactors = FALSE)
}

cat("\n#### Phase 9d parity grid --", nrow(shapes), "shapes | R", as.character(getRversion()), "\n\n")
run_safe <- function(i) tryCatch(run_shape(shapes[i, ]), error = function(e) {
  s <- shapes[i, ]
  data.frame(pct = s$pct, comp = s$comp, OR = s$OR, ntab = s$ntab, wt = s$weighted, na = s$na,
             totaltab = s$totaltab, ref = s$ref, BC = NA, E = NA, F = NA,
             err = conditionMessage(e), stringsAsFactors = FALSE)
})
res <- dplyr::bind_rows(lapply(seq_len(nrow(shapes)), run_safe))
if (!"err" %in% names(res)) res$err <- NA_character_
errs <- res[!is.na(res$err), ]
if (nrow(errs) > 0) { cat("ERRORS (", nrow(errs), "):\n"); print(utils::head(errs, 8), row.names = FALSE) }
res <- res[is.na(res$err), ]
fails <- res[!(res$BC & res$E & (is.na(res$F) | res$F)), ]
cat("BC pass:", sum(res$BC), "/", nrow(res),
    "| E pass:", sum(res$E), "/", nrow(res),
    "| F pass:", sum(res$F, na.rm = TRUE), "/", sum(!is.na(res$F)), "\n")
if (nrow(fails) > 0) { cat("\nFAILURES:\n"); print(fails, row.names = FALSE) } else cat("ALL PARITY GREEN\n")

# ============================================================================================
# 6. TIMING -- per-block (the gate) + end-to-end tab_plain()
# ============================================================================================
cat("\n#### Timing (per-block, isolated)\n\n")
# a representative mid-size shape: 2 tab_vars, weighted, row %, OR on, comp tab
row_var <- "marital"; tab_vars <- c("relig", "partyid")
tab_row_names <- c(tab_vars, row_var); tot <- c("row", "col")
base  <- q(mk_base(gss, row_var, col_var, tab_vars, weighted = TRUE, na = "drop"))
bc0   <- q(current_BC(base, row_var, tab_vars, "line", tot))
D0    <- region_D(bc0, tab_row_names, tot, TRUE)
v0    <- mk_vectors(D0$tabs_n, row_var, tab_vars)
pct0  <- current_E(D0$tabs_n, D0$tabs_wn, "row", tab_vars, D0$cols)$pct
call_ref0 <- function(fn) fn(
  tabs = bc0, tabs_pct = pct0, ref = "tot", ref2 = "first", comp = "tab",
  OR = "OR", color = "no", pct = "row", tab_row_names = tab_row_names,
  tab_vars = rlang::syms(tab_vars), row_var = rlang::sym(row_var),
  tottab_vector = v0$tottab, totrow_vector = v0$totrow, cols = D0$cols)

time_block <- function(label, cur, mx) {
  tc <- benchmark_measure(cur, iterations = 200L)$median_s
  tm <- benchmark_measure(mx,  iterations = 200L)$median_s
  cat(sprintf("  %-18s current %8.5f s | matrix %8.5f s | speedup %5.2fx\n",
              label, tc, tm, tc / tm))
}
time_block("B/C totals",  function() current_BC(base, row_var, tab_vars, "line", tot),
                          function() matrix_BC (base, row_var, tab_vars, "line", tot))
time_block("E pct/tot_n", function() current_E   (D0$tabs_n, D0$tabs_wn, "row", tab_vars, D0$cols),
                          function() leaf_wide_pct(D0$tabs_n, D0$tabs_wn, "row", tab_vars, D0$cols))
time_block("F reference", function() call_ref0(tab_apply_reference),
                          function() call_ref0(matrix_apply_reference))

cat("\n#### End-to-end tab_plain() (denominator for the >=5% per-row_var gate)\n\n")
# no tab_vars, 1 row_var x 1 col_var -- the leaf. Report unweighted + weighted, plain + with tab_vars.
t_uw <- benchmark_measure(function()
  q(tabxplor:::tab_plain(gss, marital, race, pct = "row", color = "diff", ref = "tot", na = "drop")),
  iterations = 60L)$median_s
t_w  <- benchmark_measure(function()
  q(tabxplor:::tab_plain(gss, marital, race, wt = w, pct = "row", color = "diff", ref = "tot", na = "drop")),
  iterations = 60L)$median_s
t_tv <- benchmark_measure(function()
  q(tabxplor:::tab_plain(gss, marital, race, c(relig, partyid), wt = w, pct = "row", color = "diff",
                         ref = "tot", comp = "tab", na = "drop")),
  iterations = 40L)$median_s
cat(sprintf("  tab_plain unweighted (no tab_vars) %8.4f s\n", t_uw))
cat(sprintf("  tab_plain weighted   (no tab_vars) %8.4f s\n", t_w))
cat(sprintf("  tab_plain weighted   (2 tab_vars)  %8.4f s\n", t_tv))
cat("\nDONE.\n")
