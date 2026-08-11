# PURPOSE: THE survey-design boundary -- one place turns a design object passed as `data` into the
#   microdata every tabxplor engine already knows how to read -- plus the design constructors and the
#   ROBUST omnibus tests for tab() crosstabs/means.
# ROLE: Three things live here:
#   1. The BOUNDARY (Last Phase z14-i): svy_is_design() / svy_unwrap_data() / svy_check_test().
#      Every public entry point that accepts a survey design (tab, tab_many, tab_plain, tab_num,
#      tab_reg) calls the same two lines; tab_counts() calls svy_is_design() to REFUSE one. Before
#      z14-i the detection was written twice, and the two copies disagreed: tab() materialised the
#      design's weights (tab.R:630) and tab_reg() set `wt <- NULL`, so every crude Obs_* column, the
#      population-average AME, the frozen SD and the "Weighted by" footer silently lost the weights
#      (D1/D2/D8 of dev/full_survey_design_scope.md S2.3).
#   2. The design constructors (svy_*), shared by tab_reg's weighted models.
#   3. tab_robust_overlay(): recompute each whole-table omnibus p-value under a robustness mode and
#      overlay it on the classic `test` attribute (Last Phase j). Two modes, both OPT-IN -- the default
#      unweighted-chi2 / S14-F path never runs this, stays byte-identical:
#        - "kish"   : first-order Rao-Scott -- the Pearson chi2 on the WEIGHTED proportions rescaled to
#                     the effective n_eff = (sum w)^2 / sum w^2; the numeric F on per-group n_eff.
#        - "survey" : design-based -- survey::svychisq (Rao-Scott 2nd-order F) for factors, a svyglm +
#                     regTermTest Wald F for means. Needs `survey` (an Import; guarded for consistency).
# DESIGN: the inference RUNG is derived, never asked for (z14-i, ruling Q2). `test` says only WHETHER
#   to test; what the user already passed says HOW -- weights / weights + tabxplor.kish_neff / a design
#   object. That is why `ids`/`strata`/`fpc`/`nest` are gone: they reached the omnibus p and nothing
#   else, and svydesign() says all four better. Since z14-ii the same rung also chooses each cell's
#   `n_eff` base, so the tests and the intervals cannot describe different samples --
#   see svy_inference_mode() and R/survey-variance.R.
# KEY CONSTRAINTS:
#   - The robust p replaces ONLY the p-value / statistic / df / n on the chi2 / F rows; the descriptive
#     effect size is carried through (it is computed on the same weighted table since z14-i).
#   - Robust tests run on complete cases of (row_var, col_var) per subtable (the survey convention);
#     this can differ slightly from the classic chi2 when na = "keep" counts NA as a category -- documented.
#   - Fisher rows are dropped in robust mode (the robust p is the answer there).
# See: dev/full_survey_design_scope.md (z14-i); dev/tabxplor_2.0.0_decisions.md S51; CLAUDE.md Last Phase j.

# === SECTION: the design boundary ===================================================================

# Package-owned column names written into the unwrapped frame. `.svy_weights` is ALSO the fact "this
# table is design-based": it is the resolved weight name on every path (tab()'s vars_attr, the
# tab_plain/tab_num leaves, tab_reg's reg_meta), so tab_weight_line() reads it as a fact instead of
# printing it as a name (D7). `.svy_row` is the position into the ORIGINAL design, so a table built on
# PREPARED microdata (filtered, lumped, relabelled) can still index the design it came from.
svy_wt_col  <- ".svy_weights"
svy_row_col <- ".svy_row"

# THE class list. Shared by the entry points that accept a design and by tab_counts(), which refuses
# one -- so "what is a design" cannot be answered two ways.
svy_is_design <- function(x)
  inherits(x, c("survey.design", "survey.design2", "svyrep.design", "twophase", "twophase2"))

# Unwrap a survey design passed as `data`. Returns NULL when `data` is not a design -- so the ordinary
# path costs one inherits() and is byte-identical -- else the design's model frame with the two
# package columns added, plus the `spec` that IS the design_spec every consumer reads.
# WARNING: weights(design) returns the n x R REPLICATE MATRIX for a svyrep.design; only
#   type = "sampling" is the full-sample weight vector (D4). survey.design's own method absorbs `type`
#   in `...`, so one call shape is right for both classes.
svy_unwrap_data <- function(data, fn = "tab") {
  if (!svy_is_design(data)) return(NULL)
  if (!requireNamespace("survey", quietly = TRUE))
    cli::cli_abort(c("A {.cls survey.design} passed as {.arg data} needs the {.pkg survey} package.",
                     "i" = 'Install it with {.code install.packages("survey")}.'))
  # Replicate-weight and two-phase designs are OUT (ruling Q5): their variance is a set of alternative
  # weight columns / a two-phase formula, which none of the tabxplor engines can read. Refuse clearly
  # rather than approximate -- a replicate design would otherwise die inside survey with a raw error.
  if (inherits(data, c("svyrep.design", "twophase", "twophase2")))
    cli::cli_abort(c(
      "{.fn {fn}} does not support {.cls {class(data)[[1]]}} designs.",
      "i" = "Build one with {.fn survey::svydesign} (weights, and optionally strata / clusters / fpc).",
      "i" = paste("Replicate weights carry the variance as extra weight columns, which tabxplor",
                  "does not read.")))
  frame <- as.data.frame(data$variables)
  clash <- intersect(c(svy_wt_col, svy_row_col), names(frame))
  if (length(clash))
    cli::cli_abort(c("{.val {clash}} {?is/are} reserved by tabxplor for the survey design.",
                     "i" = "Rename {?it/them} in the design's data before passing it as {.arg data}."))
  frame[[svy_wt_col]]  <- as.double(stats::weights(data, type = "sampling"))
  frame[[svy_row_col]] <- seq_len(nrow(frame))
  cli::cli_inform(c("i" = "Survey design detected: estimates and tests use the design."))
  list(data = frame, spec = list(design = data, wt = svy_wt_col))
}

# `test` says only WHETHER to test -- TRUE/FALSE, nothing else. Validated at the PUBLIC boundary
# (tab / tab_many / tab_counts) so the error points at the user's call; returns the boolean.
# Before z14-i `test` also took "survey"/"design" and was never validated at all, so a typo
# ("surveyy") silently meant no test and tab_counts("survey") silently meant a classic one.
svy_check_test <- function(test, arg = "test") {
  if (!(is.logical(test) && length(test) == 1L && !is.na(test)))
    cli::cli_abort(c(
      "{.arg {arg}} must be {.code TRUE} or {.code FALSE}.",
      "x" = "Got {.val {test}}.",
      "i" = paste("The KIND of test follows what you pass: {.arg wt} gives a weighted test,",
                  "{.code options(tabxplor.kish_neff = TRUE)} rescales it to Kish's effective sample",
                  "size, and a {.fn survey::svydesign} passed as {.arg data} gives a design-based one.")
    ))
  isTRUE(test)
}

# THE inference rung (ruling Q2) -- resolved once, in tab_setup(), where the weight is resolved and the
# design_spec is in the ctx. That is why neither tab() nor tab_many() computes it: they used to drift
# (only tab() had the rule, so tab_many() was silently always classic).
# Last Phase z14-ii: it governs the CELL INTERVALS as well as the whole-table test -- the leaves take
# their `n_eff` base from it (design -> Kish -> raw), instead of re-reading tabxplor.kish_neff in two
# more places. Hence the name: one ladder, one resolution, every inference in the table.
svy_inference_mode <- function(design_spec, wt) {
  if (!is.null(design_spec) && !is.null(design_spec$design))               return("survey")
  if (isTRUE(getOption("tabxplor.kish_neff", FALSE)) && length(wt) > 0L)   return("kish")
  "classic"
}

# === SECTION: design construction (shared with tab_reg) =============================================

# Coerce a design argument (NULL / a column name, symbol or char vector / a formula) to a survey
# formula. as.character() also normalises a bare symbol (tab()'s resolved weight is a symbol).
svy_design_formula <- function(x) {
  if (is.null(x)) return(NULL)
  if (rlang::is_formula(x)) return(x)
  stats::reformulate(as.character(x))
}

# The data columns a design spec references (so a complete-case drop never feeds NA weights to
# svydesign). A prebuilt design carries its own metadata -> character(0), and that early return is
# LOAD-BEARING: it keeps `.svy_weights` out of reg_fit()'s drop_vars, whose complete-case mask must
# stay the design's own row set.
svy_design_vars <- function(design_spec) {
  if (is.null(design_spec) || !is.null(design_spec$design)) return(character(0))
  wt <- design_spec$wt
  if (is.null(wt)) return(character(0))
  if (rlang::is_formula(wt)) all.vars(wt) else as.character(wt)
}

# Build a survey.design from a weight column. ids = ~1 (no clustering) reproduces the flat weighted
# path exactly; anything richer is the user's own svydesign() passed as `data`.
svy_make_design <- function(data, wt) {
  survey::svydesign(ids = stats::as.formula("~1"), weights = svy_design_formula(wt), data = data)
}

# THE domain-estimation helper: restrict a prebuilt design to `rows` (INTEGER positions into the
# original design) and swap its model frame for `frame` -- the prepared / recoded rows, in the same
# order. Both consumers need exactly this: tab()'s robust overlay (whose test must see the lumped,
# relabelled, filtered table the user is looking at, not the design's original variables) and
# tab_reg()'s per-model design (whose fit must see the recoded complete-case frame).
# WARNING: `[` does NOT drop rows on a CALIBRATED or PPS design -- it keeps all n and marks the
#   excluded ones prob = Inf (weight 0), survey's own domain idiom. Assigning a shorter frame into
#   such a design errors ("replacement has m rows, data has n"), which is why tab_reg() used to fail
#   on a calibrated design with ANY incomplete case (D10). Pad back to full length instead: the padded
#   rows carry zero weight, so they can reach neither an estimate nor a variance.
# WARNING: subset ONCE, and always into the ORIGINAL design. design[rows, ][keep, ] applies a short
#   mask to a design `[` may not have shrunk.
svy_domain_design <- function(design, rows, frame) {
  dd <- design[rows, ]
  if (nrow(dd$variables) == nrow(frame)) {
    dd$variables <- frame
  } else {
    idx <- rep(NA_integer_, nrow(dd$variables))
    idx[rows] <- seq_len(nrow(frame))
    idx[is.na(idx)] <- 1L          # any valid row -- these are the zero-weight ones
    dd$variables <- frame[idx, , drop = FALSE]
  }
  dd
}

# === SECTION: the robust omnibus overlay ============================================================

# ONE subtable x col_var robust test. `sub` is the subtable frame (a data.frame, the PREPARED
# microdata), `des_rows` the positions of its rows in the original design (NULL when no design).
# `rv`/`cv` are the variable names, `is_num` its type, `wt` the weight name. Returns a one-row list
# with the test discriminator + (statistic, df1, df2, pvalue, n); an all-NA row on any failure (never
# crashes tab()).
svy_omnibus_one <- function(sub, rv, cv, is_num, wt, mode, des_rows, design_spec, anova) {
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

  # mode == "survey": a design-based Rao-Scott F (svychisq) / Wald F (svyglm + regTermTest).
  # svy_inference_mode() only reaches "survey" with a prebuilt design, so `des_rows` is always present.
  # The design's model frame is swapped for `d` -- the PREPARED rows -- so the test sees the lumped,
  # relabelled, filtered table that is actually displayed; picking the rows is not enough, since
  # svychisq/svyglm read the variables off the design.
  if (!requireNamespace("survey", quietly = TRUE)) return(na_row(if (is_num) "F_svy" else "chi2_svy"))
  des <- tryCatch(svy_domain_design(design_spec$design, des_rows[keep], d),
                  error = function(e) NULL)
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
# named logical (col_var -> is numeric). `comp = "all"` tests the whole table (one group); else one
# test per tab_var subtable. The result has chi2_compute_test's column shape, so every downstream
# reader (display, bind) is unchanged.
# DESIGN (z14-i): the frame is ALWAYS the PREPARED microdata -- the table the user is looking at.
#   It used to be the design's own `$variables` on the prebuilt path, i.e. the ORIGINAL frame, so the
#   design-based p ignored `filter=`, `other_if_less_than` lumping and `cleannames` relabelling and
#   could describe a different table than the one printed (measured: a table displaying `a / Others`
#   carried the p of the unlumped `a / b / c`). The design is reached instead through `.svy_row`, the
#   position each prepared row holds in the original design -- which is also what makes an excluded
#   row a proper survey DOMAIN rather than a rebuilt design.
# The effect size is deliberately NOT recomputed here: it is descriptive, so it describes the weighted
#   population (chi2_compute_test already computes it on the weighted table), never the effective
#   sample an inferential rescale works in.
tab_robust_overlay <- function(test_tbl, data, row_var, col_vars, col_num, tab_vars, wt,
                               mode, design_spec, comp, anova = getOption("tabxplor.anova", "welch")) {
  if (is.null(test_tbl) || nrow(test_tbl) == 0) return(test_tbl)
  frame    <- as.data.frame(data)
  des_rows <- frame[[svy_row_col]]     # NULL without a design; positions into it otherwise
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
    sub    <- frame[g$rows, , drop = FALSE]
    rows_g <- des_rows[g$rows]
    for (cv in col_vars) {
      r <- svy_omnibus_one(sub, row_var, cv, isTRUE(col_num[[cv]]), wt, mode,
                           rows_g, design_spec, anova)
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
