# PURPOSE: Survey-design construction + the opt-in ROBUST omnibus tests for tab() crosstabs/means.
# ROLE: Two things live here, both keyed off the microdata that tab_transform() still holds:
#   1. The design constructors (svy_*), shared by tab_reg's weighted models AND tab()'s survey tests --
#      one place turns a weight column (+ ids/strata/fpc/nest) or a prebuilt survey.design into a design.
#   2. tab_robust_overlay(): recompute each whole-table omnibus p-value under a robustness mode and
#      overlay it on the classic `test` attribute (Last Phase j). Two modes, both OPT-IN -- the default
#      unweighted-chi2 / S14-F path never runs this, stays byte-identical:
#        - "kish"   : first-order Rao-Scott -- the Pearson chi2 on the WEIGHTED proportions rescaled to
#                     the effective n_eff = (sum w)^2 / sum w^2; the numeric F on per-group n_eff.
#        - "survey" : design-based -- survey::svychisq (Rao-Scott 2nd-order F) for factors, a svyglm +
#                     regTermTest Wald F for means. Needs `survey` (an Import; guarded for consistency).
# KEY CONSTRAINTS:
#   - The robust p replaces ONLY the p-value / statistic / df / n on the chi2 / F rows; the descriptive
#     effect size (Cramer's V / eta^2, from the unweighted count aggregate) is carried through unchanged.
#   - Robust tests run on complete cases of (row_var, col_var) per subtable (the survey convention);
#     this can differ slightly from the classic chi2 when na = "keep" counts NA as a category -- documented.
#   - Fisher rows are dropped in robust mode (the robust p is the answer there).
# See: dev/tabxplor_1.4.0_decisions.md S51; CLAUDE.md Last Phase j; R/tab_reg.R (the design precedent).

# === SECTION: design construction (shared with tab_reg) =============================================

# Coerce a design argument (NULL / a column name, symbol or char vector / a formula) to a survey
# formula. as.character() also normalises a bare symbol (tab()'s resolved weight is a symbol).
svy_design_formula <- function(x) {
  if (is.null(x)) return(NULL)
  if (rlang::is_formula(x)) return(x)
  stats::reformulate(as.character(x))
}

# The data columns a design spec references (so a complete-case drop never feeds NA weights/strata/fpc
# to svydesign). A prebuilt design carries its own metadata -> character(0).
svy_design_vars <- function(design_spec) {
  if (!is.null(design_spec$design)) return(character(0))
  parts <- list(design_spec$wt, design_spec$ids, design_spec$strata, design_spec$fpc)
  unique(unlist(purrr::map(parts, function(x) {
    if (is.null(x)) character(0) else if (rlang::is_formula(x)) all.vars(x) else as.character(x)
  })))
}

# Build a survey.design from a weight column (+ optional ids/strata/fpc/nest). ids = ~1 (no clustering)
# reproduces the flat weighted path exactly.
svy_make_design <- function(data, wt, ids, strata, fpc, nest) {
  survey::svydesign(
    ids     = if (is.null(ids)) stats::as.formula("~1") else svy_design_formula(ids),
    strata  = svy_design_formula(strata),
    fpc     = svy_design_formula(fpc),
    weights = svy_design_formula(wt),
    data    = data,
    nest    = nest
  )
}

# === SECTION: the robust omnibus overlay ============================================================

# The list of every data column a (subtable subset -> test) needs, so the microdata carried into the
# overlay is minimal. row_var + col_vars + tab_vars + wt + any design-referenced column.
svy_test_vars <- function(row_var, col_vars, tab_vars, wt, design_spec)
  unique(c(row_var, col_vars, tab_vars,
           if (length(wt)) as.character(wt) else character(0),
           svy_design_vars(design_spec)))

# ONE subtable x col_var robust test. `sub` is the subtable frame (a data.frame), `des_pre` a pre-subset
# survey design for the SAME rows (prebuilt-design path) or NULL to build one from `sub`+`wt`. `rv`/`cv`
# are the variable names, `is_num` its type, `wt` the weight name. Returns a one-row list with the test
# discriminator + (statistic, df1, df2, pvalue, n); an all-NA row on any failure (never crashes tab()).
svy_omnibus_one <- function(sub, rv, cv, is_num, wt, mode, des_pre, design_spec, anova) {
  na_row <- function(test) list(test = test, statistic = NA_real_, df1 = NA_real_,
                                df2 = NA_real_, pvalue = NA_real_, n = NA_real_)
  keep <- !is.na(sub[[rv]]) & !is.na(sub[[cv]])
  d    <- sub[keep, , drop = FALSE]
  if (!is_num) d[[rv]] <- droplevels(as.factor(d[[rv]]))
  d[[cv]] <- if (is_num) as.double(d[[cv]]) else droplevels(as.factor(d[[cv]]))
  n_obs <- nrow(d)
  wt_v  <- if (length(wt)) as.double(d[[as.character(wt)]]) else rep(1, n_obs)

  if (mode == "kish") {
    n_eff <- sum(wt_v)^2 / sum(wt_v^2)
    if (!is.finite(n_eff) || n_eff < 2) return(na_row(if (is_num) "F_kish" else "chi2_kish"))
    if (is_num) {
      # per group: weighted mean / ML weighted var (S14) + n_eff_g = wn_g^2 / sum w_g^2
      g   <- split(seq_len(n_obs), d[[rv]])
      st  <- lapply(g, function(ix) {
        w <- wt_v[ix]; x <- d[[cv]][ix]
        wn <- sum(w); m <- sum(w * x) / wn
        list(n = wn^2 / sum(w^2), mean = m, var = sum(w * x^2) / wn - m^2)
      })
      st <- Filter(function(s) is.finite(s$n) && s$n >= 2 && is.finite(s$var) && s$var > 0, st)
      if (length(st) < 2) return(na_row("F_kish"))
      a <- agg_anova(rep(1L, length(st)), seq_along(st),
                     vapply(st, `[[`, double(1), "n"),
                     vapply(st, `[[`, double(1), "mean"),
                     vapply(st, `[[`, double(1), "var"))
      pick <- if (identical(anova, "classic")) "_classic" else ""
      return(list(test = "F_kish",
                  statistic = a[[paste0("statistic", pick)]][1], df1 = a[[paste0("df1", pick)]][1],
                  df2 = a[[paste0("df2", pick)]][1], pvalue = a[[paste0("pvalue", pick)]][1],
                  n = n_eff))
    }
    # factor: Pearson X2 on the WEIGHTED counts, rescaled to n_eff (first-order Rao-Scott, single deff)
    M <- tapply(wt_v, list(d[[rv]], d[[cv]]), sum); M[is.na(M)] <- 0
    M <- M[rowSums(M) > 0, colSums(M) > 0, drop = FALSE]
    if (nrow(M) < 2 || ncol(M) < 2) return(na_row("chi2_kish"))
    gt <- sum(M); E <- outer(rowSums(M), colSums(M)) / gt
    X2 <- sum((M - E)^2 / E) / gt * n_eff
    df <- (nrow(M) - 1) * (ncol(M) - 1)
    return(list(test = "chi2_kish", statistic = X2, df1 = df, df2 = NA_real_,
                pvalue = stats::pchisq(X2, df, lower.tail = FALSE), n = n_eff))
  }

  # mode == "survey": a design-based Rao-Scott F (svychisq) / Wald F (svyglm + regTermTest)
  if (!requireNamespace("survey", quietly = TRUE)) return(na_row(if (is_num) "F_svy" else "chi2_svy"))
  des <- tryCatch({
    if (!is.null(des_pre)) des_pre[keep, ]
    else svy_make_design(d, wt, design_spec$ids, design_spec$strata, design_spec$fpc, design_spec$nest)
  }, error = function(e) NULL)
  if (is.null(des)) return(na_row(if (is_num) "F_svy" else "chi2_svy"))
  old <- options(survey.lonely.psu = "adjust"); on.exit(options(old), add = TRUE)

  if (is_num) {
    res <- tryCatch({
      fit <- survey::svyglm(stats::reformulate(rv, response = cv), design = des)
      rt  <- survey::regTermTest(fit, stats::reformulate(rv), method = "Wald")
      list(test = "F_svy", statistic = as.double(rt$Ftest), df1 = as.double(rt$df),
           df2 = as.double(rt$ddf), pvalue = as.double(rt$p), n = n_obs)
    }, error = function(e) NULL)
    return(res %||% na_row("F_svy"))
  }
  res <- tryCatch({
    ch <- survey::svychisq(stats::reformulate(c(rv, cv)), design = des, statistic = "F")
    list(test = "chi2_svy", statistic = as.double(ch$statistic),
         df1 = as.double(ch$parameter[["ndf"]]), df2 = as.double(ch$parameter[["ddf"]]),
         pvalue = as.double(ch$p.value), n = n_obs)
  }, error = function(e) NULL)
  res %||% na_row("chi2_svy")
}

# Overlay the robust omnibus onto a classic `test` tibble: recompute (statistic, df, pvalue, n) per
# (subtable x col_var), keep the classic effect_size / es_type / min_e, drop Fisher. `col_num` is a
# named logical (col_var -> is numeric). The subtable FRAME is `design_spec$design$variables` for a
# prebuilt design (tests run in design-space, subset by row position), else the prepared microdata
# `data`. `comp = "all"` tests the whole table (one group); else one test per tab_var subtable. The
# result has chi2_compute_test's column shape, so every downstream reader (display, bind) is unchanged.
tab_robust_overlay <- function(test_tbl, data, row_var, col_vars, col_num, tab_vars, wt,
                               mode, design_spec, comp, anova = getOption("tabxplor.anova", "welch")) {
  if (is.null(test_tbl) || nrow(test_tbl) == 0) return(test_tbl)
  prebuilt <- !is.null(design_spec) && !is.null(design_spec$design)
  frame    <- if (prebuilt) as.data.frame(design_spec$design$variables) else as.data.frame(data)
  # the classic per-(subtable x col_var) effect-size / validity facts to carry through
  es_keep <- test_tbl[test_tbl$test %in% c("chi2", "F_welch"), , drop = FALSE]
  tabvars_in <- intersect(tab_vars, names(test_tbl))
  # subtable groups (row positions into `frame`): tab_var levels unless comp = "all" / no tab_vars
  if (length(tabvars_in) == 0 || comp == "all") {
    groups <- list(list(keys = NULL, rows = seq_len(nrow(frame))))
  } else {
    gk <- unique(frame[tabvars_in])
    groups <- lapply(seq_len(nrow(gk)), function(i) {
      sel <- rep(TRUE, nrow(frame))
      for (tc in tabvars_in) sel <- sel & frame[[tc]] == gk[[tc]][i]
      list(keys = gk[i, , drop = FALSE], rows = which(sel))
    })
  }

  out <- list()
  for (g in groups) {
    sub     <- frame[g$rows, , drop = FALSE]
    des_pre <- if (prebuilt) design_spec$design[g$rows, ] else NULL
    for (cv in col_vars) {
      r <- svy_omnibus_one(sub, row_var, cv, isTRUE(col_num[[cv]]), wt, mode,
                           des_pre, design_spec, anova)
      row <- tibble::tibble(row_var = row_var, col_var = cv, test = r$test,
                            statistic = r$statistic, df1 = r$df1, df2 = r$df2,
                            pvalue = r$pvalue, n = r$n)
      if (!is.null(g$keys)) row <- dplyr::bind_cols(g$keys, row)
      out[[length(out) + 1L]] <- row
    }
  }
  rob <- dplyr::bind_rows(out)
  if (nrow(rob) == 0) return(test_tbl)
  # carry the effect size / min_e from the classic rows (join on the identity key + col_var)
  jk  <- intersect(c(tabvars_in, "col_var"), names(es_keep))
  rob <- dplyr::left_join(
    rob, dplyr::select(es_keep, dplyr::all_of(jk), "min_e", "effect_size", "es_type"), by = jk)
  dplyr::relocate(rob, tidyselect::any_of(c(tabvars_in, "row_var", "col_var")))
}
