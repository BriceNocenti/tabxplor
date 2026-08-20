# PURPOSE: THE survey-design boundary -- one place turns a `survey` design passed as `data` into the
#   microdata every tabxplor engine already reads -- plus the design constructors and the
#   design-based omnibus tests.
# ROLE: the boundary (svy_is_design / svy_select_frame / svy_unwrap_data / svy_check_test), the
#   constructors shared with tab_reg()'s weighted models, and tab_robust_overlay(), which recomputes
#   each whole-table omnibus ON a design -- the user's own, or the flat one a weight column defines
#   -- and overlays it on the classic `test` attribute. survey::svychisq (Rao-Scott F) for factors,
#   svyglm + regTermTest's Wald F for means.
# DESIGN: the INFERENCE BASIS is derived, never asked for. `test` says only WHETHER to test; `wt`
#   says how the ESTIMATE is computed; the basis says how the INTERVAL and the test are. Those two
#   are orthogonal, and keeping them so is what stops one fact needing several encodings.
#   svy_inference_basis() is the ONLY reader of the option and of the design object; every consumer
#   takes the resolved value.
#     "n"              the raw sample size -- unweighted, or weighted with no design effect (default)
#     "weights"        the design effect of the weights, exactly: the flat ids = ~1 design
#     "design"         the full design: strata, clusters, fpc, calibration
#     "design_partial" a design was given but its variance could not be computed
#   The four are RANKED weakest-first by basis_rank() (R/fmt_class.R) and stamped on every fmt
#   COLUMN by tab_stamp_inference() -- never on the table, which dplyr would drop.
# KEY CONSTRAINTS:
#   - svy_unwrap_data() runs exactly ONCE per call: it informs, adds the two reserved columns and
#     captures the design's degrees of freedom. svy_select_frame() is its side-effect-free twin, for
#     tidy-selection only; its frame must stay a subset of what the unwrap produces.
#   - Replicate-weight (svyrepdesign) and two-phase designs are REFUSED, never approximated.
#   - The robust p replaces only the p-value / statistic / df / n; the effect size is carried through.
#   - Robust tests use complete cases of (row_var, col_var) per subtable, the survey convention, so
#     they can differ from the classic chi2 when na = "keep" counts NA as a category. Fisher rows are
#     dropped in robust mode: the robust p is the answer there.
#   - THE exception to "the test comes from the aggregate": a design-based omnibus needs the
#     observations, so it runs only when the basis is not "n" -- i.e. opt-in.
# See: CLAUDE.md § tabxplor architecture (the inference layer); R/survey-variance.R (the two variance
#   implementations the basis selects between).

# === SECTION: the design boundary ===================================================================

# Package-owned columns of the unwrapped frame. `.svy_weights` is ALSO the fact "this table is
# design-based"; `.svy_row` indexes PREPARED microdata back into the ORIGINAL design.
svy_wt_col  <- ".svy_weights"
svy_row_col <- ".svy_row"

svy_is_design <- function(x)
  inherits(x, c("survey.design", "survey.design2", "svyrep.design", "twophase", "twophase2"))

svy_abort_unsupported_design <- function(data, fn = "tab") {
  if (!inherits(data, c("svyrep.design", "twophase", "twophase2"))) return(invisible(NULL))
  cli::cli_abort(c(
    "{.fn {fn}} does not support {.cls {class(data)[[1]]}} designs.",
    "i" = "Build one with {.fn survey::svydesign} (weights, and optionally strata / clusters / fpc).",
    "i" = paste("Replicate weights carry the variance as extra weight columns, which tabxplor",
                "does not read.")))
}

svy_select_frame <- function(data, fn = "tab") {
  if (!svy_is_design(data)) return(data)
  svy_abort_unsupported_design(data, fn)
  as.data.frame(data$variables)
}

svy_unwrap_data <- function(data, fn = "tab") {
  if (!svy_is_design(data)) return(NULL)
  svy_abort_unsupported_design(data, fn)
  frame <- as.data.frame(data$variables)
  clash <- intersect(c(svy_wt_col, svy_row_col), names(frame))
  if (length(clash))
    cli::cli_abort(c("{.val {clash}} {?is/are} reserved by tabxplor for the survey design.",
                     "i" = "Rename {?it/them} in the design's data before passing it as {.arg data}."))
  # ⚠ weights(design) is the n x R REPLICATE MATRIX for a svyrep.design; only type = "sampling" is the
  #   weight vector. survey.design absorbs `type` in `...`, so one call shape serves both classes.
  frame[[svy_wt_col]]  <- as.double(stats::weights(data, type = "sampling"))
  frame[[svy_row_col]] <- seq_len(nrow(frame))
  cli::cli_inform(c("i" = "Survey design detected: estimates and tests use the design."))
  # degf captured once here: survey refers every interval to t(degf), not to z.
  list(data = frame, spec = list(design = data, wt = svy_wt_col,
                                 degf = svy_degf(data)))
}

# `wt` beside a design is a contradiction, not a preference: the design carries its own weights.
svy_abort_wt_design <- function(wt_given) {
  if (!isTRUE(wt_given)) return(invisible(NULL))
  cli::cli_abort(c(
    "{.arg wt} cannot be used when {.arg data} is a {.cls survey.design}.",
    "x" = "A design already carries its own sampling weights.",
    "i" = "Drop {.arg wt}, or build the design with those weights: \\
           {.code survey::svydesign(ids = ~1, weights = ~w, data = d)}."))
}

svy_degf <- function(design) {
  d <- tryCatch(as.double(survey::degf(design)), error = function(e) NA_real_)
  if (length(d) != 1L || !is.finite(d) || d <= 0) NA_real_ else d
}

svy_check_test <- function(test, arg = "test") {
  if (!(is.logical(test) && length(test) == 1L && !is.na(test)))
    cli::cli_abort(c(
      "{.arg {arg}} must be {.code TRUE} or {.code FALSE}.",
      "x" = "Got {.val {test}}.",
      "i" = paste("The KIND of test follows what you pass: {.arg wt} gives a weighted test,",
                  "{.code options(tabxplor.design_effect = TRUE)} makes it account for the",
                  "weighting, and a {.fn survey::svydesign} passed as {.arg data} gives a",
                  "design-based one.")
    ))
  isTRUE(test)
}

# `force` is tab_reg()'s rule: its crude Obs_* columns are ALWAYS on the weighted basis when weighted,
# so they match the Model_* column beside them, and it never reads the tab()-scoped option.
# `can_serve` is the INPUT's half -- the weighted basis needs a per-observation Sum(w^2), which a
# pre-aggregate (the ctx's `agg_only`) cannot supply.
svy_inference_basis <- function(design_spec, wt, force = FALSE, can_serve = TRUE,
                                design_effect = NULL) {
  if (!is.null(design_spec) && !is.null(design_spec$design))          return("design")
  de <- if (is.null(design_effect)) tx_option("design_effect")
        else isTRUE(design_effect)
  if (length(wt) > 0L && isTRUE(can_serve) && (isTRUE(force) || isTRUE(de))) return("weights")
  "n"
}

svy_weighted <- function(x = NULL, wt = x$wt)
  !is.null(x$design) || length(wt) > 0L

# THE inference object, BUILD-TIME only: what survives the build is the per-column `conf_level` /
# `degf` / `basis` attributes tab_stamp_inference() projects from it.
new_inference <- function(wt = character(), design_spec = NULL,
                          conf_level = conf_level_default(),
                          method = default_ci_method(), agg_only = FALSE, force = FALSE,
                          design_effect = NULL) {
  list(wt         = if (length(wt)) as.character(wt) else character(),
       design     = design_spec$design,
       basis      = svy_inference_basis(design_spec, wt, force = force,
                                        can_serve = !isTRUE(agg_only),
                                        design_effect = design_effect),
       degf       = design_spec$degf %||% Inf,
       conf_level = conf_level,
       method     = method,
       agg_only   = isTRUE(agg_only))
}

# === SECTION: design construction (shared with tab_reg) =============================================

svy_design_formula <- function(x) {
  if (is.null(x)) return(NULL)
  if (rlang::is_formula(x)) return(x)
  stats::reformulate(as.character(x))
}

# The early return on a prebuilt design is LOAD-BEARING: it keeps `.svy_weights` out of reg_fit()'s
# drop_vars, whose complete-case mask must stay the design's own row set.
svy_design_vars <- function(design_spec) {
  if (is.null(design_spec) || !is.null(design_spec$design)) return(character(0))
  wt <- design_spec$wt
  if (is.null(wt)) return(character(0))
  if (rlang::is_formula(wt)) all.vars(wt) else as.character(wt)
}

svy_make_design <- function(data, wt) {
  survey::svydesign(ids = stats::as.formula("~1"), weights = svy_design_formula(wt), data = data)
}

# ⚠ `[` does NOT drop rows on a CALIBRATED or PPS design -- it keeps all n at prob = Inf (weight 0),
#   so a shorter frame cannot be assigned in: pad back to full length, the padding carrying weight 0.
# ⚠ subset ONCE, and always into the ORIGINAL design: design[rows, ][keep, ] applies a short mask to a
#   design `[` may not have shrunk.
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

# DESIGN: two discriminators (`chi2_design` / `F_design`), not four -- a weight column IS a flat
#   design, so both bases run the IDENTICAL survey estimator. `n` is ALWAYS the raw count; the
#   effective information is `deff`. An all-NA row on any failure, so a test never crashes tab().
svy_omnibus_one <- function(sub, rv, cv, is_num, wt, basis, des_rows, design) {
  disc   <- if (is_num) "F_design" else "chi2_design"
  na_row <- function() list(test = disc, statistic = NA_real_, df1 = NA_real_,
                            df2 = NA_real_, pvalue = NA_real_, n = NA_real_, deff = NA_real_)
  keep <- !is.na(sub[[rv]]) & !is.na(sub[[cv]])
  d    <- sub[keep, , drop = FALSE]
  if (!is_num) d[[rv]] <- droplevels(as.factor(d[[rv]]))
  d[[cv]] <- if (is_num) as.double(d[[cv]]) else droplevels(as.factor(d[[cv]]))
  n_obs <- nrow(d)
  if (n_obs < 3) return(na_row())

  des <- tryCatch(
    if (identical(basis, "design")) svy_domain_design(design, des_rows[keep], d)
    else                            svy_make_design(d, wt),
    error = function(e) NULL)
  if (is.null(des)) return(na_row())
  old <- options(survey.lonely.psu = "adjust"); on.exit(options(old), add = TRUE)

  if (is_num) {
    res <- tryCatch({
      fit <- survey::svyglm(stats::reformulate(rv, response = cv), design = des)
      rt  <- survey::regTermTest(fit, stats::reformulate(rv), method = "Wald")
      list(test = disc, statistic = as.double(rt$Ftest), df1 = as.double(rt$df),
           df2 = as.double(rt$ddf), pvalue = as.double(rt$p), n = n_obs, deff = NA_real_)
    }, error = function(e) NULL)
    return(res %||% na_row())
  }
  res <- tryCatch({
    ch <- survey::svychisq(stats::reformulate(c(rv, cv)), design = des, statistic = "F")
    # svychisq's `ndf` is Satterthwaite's d0, not (r-1)(c-1), so the Pearson df is recomputed here for
    # delta-bar = X2_Pearson / (F * df_Pearson), Rao-Scott's mean generalized design effect.
    dfp <- (nlevels(d[[rv]]) - 1) * (nlevels(d[[cv]]) - 1)
    x2p <- tryCatch(as.double(sum((ch$observed - ch$expected)^2 / ch$expected)),
                    error = function(e) NA_real_)
    list(test = disc, statistic = as.double(ch$statistic),
         df1 = as.double(ch$parameter[["ndf"]]), df2 = as.double(ch$parameter[["ddf"]]),
         pvalue = as.double(ch$p.value), n = n_obs,
         deff = if (isTRUE(is.finite(x2p)) && dfp > 0) x2p / (as.double(ch$statistic) * dfp)
                else NA_real_)
  }, error = function(e) NULL)
  res %||% na_row()
}

# THE PRODUCER, straight from the microdata: produced ONCE for two consumers (the `color = "contrib"`
# residual, then the `test` overlay), so the omnibus p and the cell colours describe the SAME effect.
# ⚠ the frame is ALWAYS the PREPARED microdata, the design reached through `.svy_row`: the design's own
#   `$variables` would give the p of the unlumped, unfiltered table.
svy_omnibus_grid <- function(data, row_var, col_vars, col_num, tab_vars, wt,
                             basis, design, comp, totaltab_name = NULL) {
  frame    <- as.data.frame(data)
  if (nrow(frame) == 0 || length(col_vars) == 0) return(NULL)
  des_rows <- frame[[svy_row_col]]     # NULL without a design; positions into it otherwise
  tabvars_in <- intersect(tab_vars, names(frame))
  if (length(tabvars_in) == 0 || comp == "all") {
    groups <- list(list(keys = NULL, rows = seq_len(nrow(frame))))
  } else {
    gk <- unique(frame[tabvars_in])
    # expand each key column's levels ONCE so the total-table row binds as a FACTOR -- a character row
    # would coerce the whole tab_var column of `test` on the vec_rbind.
    add_tot <- !is.null(totaltab_name) && length(totaltab_name) == 1L && nzchar(totaltab_name)
    if (add_tot) for (tc in tabvars_in) {
      lv <- union(levels(as.factor(gk[[tc]])), totaltab_name)
      gk[[tc]] <- factor(as.character(gk[[tc]]), levels = lv)
    }
    groups <- lapply(seq_len(nrow(gk)), function(i) {
      sel <- rep(TRUE, nrow(frame))
      for (tc in tabvars_in) sel <- sel & as.character(frame[[tc]]) == as.character(gk[[tc]][i])
      list(keys = gk[i, , drop = FALSE], rows = which(sel))
    })
    if (add_tot) {
      kt <- gk[1, , drop = FALSE]
      for (tc in tabvars_in) kt[[tc]][1] <- totaltab_name
      groups <- c(groups, list(list(keys = kt, rows = seq_len(nrow(frame)))))
    }
  }

  out <- list()
  for (g in groups) {
    sub    <- frame[g$rows, , drop = FALSE]
    rows_g <- des_rows[g$rows]
    for (cv in col_vars) {
      r <- svy_omnibus_one(sub, row_var, cv, isTRUE(col_num[[cv]]), wt, basis,
                           rows_g, design)
      row <- tibble::tibble(var = row_var, col = cv, test = r$test,
                            statistic = r$statistic, df1 = r$df1, df2 = r$df2,
                            pvalue = r$pvalue, n = r$n, deff = r$deff)
      if (!is.null(g$keys)) row <- dplyr::bind_cols(g$keys, row)
      out[[length(out) + 1L]] <- row
    }
  }
  rob <- dplyr::bind_rows(out)
  if (nrow(rob) == 0) NULL else rob
}

# `tabxplor.anova` (Welch vs classic F) is deliberately NOT read here: it picks between two CLASSIC F
# statistics, and a design-based numeric test is the svyglm Wald F, which has no such variant.
tab_robust_overlay <- function(test_tbl, rob, tab_vars) {
  if (is.null(test_tbl) || nrow(test_tbl) == 0) return(test_tbl)
  if (is.null(rob) || nrow(rob) == 0)           return(test_tbl)
  # EVERY classic crosstab row, deduplicated on the join key: naming test kinds instead would drop a
  # whole design row, p-value included, for a producer emitting one of them without the other.
  es_keep    <- test_tbl[test_tbl$test %in% TEST_CROSSTAB_KEYS[
    !vapply(TEST_CROSSTAB_KEYS, function(k) isTRUE(TEST_ROWS[[k]]$design), logical(1))], , drop = FALSE]
  tabvars_in <- intersect(tab_vars, names(test_tbl))
  jk  <- intersect(c(tabvars_in, "col"), names(es_keep))
  es_keep <- dplyr::distinct(es_keep, dplyr::across(dplyr::all_of(jk)), .keep_all = TRUE)
  # "replace, never invent": a grid row whose subtable the classic test lacks is dropped, not injected.
  rob <- dplyr::semi_join(rob, dplyr::distinct(es_keep[jk]), by = jk)
  if (nrow(rob) == 0) return(test_tbl)
  rob <- dplyr::left_join(
    rob, dplyr::select(es_keep, dplyr::all_of(jk), "min_e", "effect_size", "es_type"), by = jk)
  dplyr::relocate(rob, tidyselect::any_of(c(tabvars_in, "var", "col")))
}

# Key a producer grid onto a BUILT table's groups; svy_key_chr() (R/survey-variance.R) spells both
# sides, so there is no second key convention. NULL -- "use the ladder" -- rather than a wrong number.
svy_deff_lookup <- function(rob, group_vars) {
  if (is.null(rob) || !nrow(rob) || !"deff" %in% names(rob))  return(NULL)
  if (length(group_vars) && !all(group_vars %in% names(rob))) return(NULL)
  key <- if (length(group_vars))
    do.call(paste, c(lapply(rob[group_vars], svy_key_chr), list(rob$col), list(sep = "\r")))
  else paste("", rob$col, sep = "\r")
  ok <- !is.na(rob$deff) & is.finite(rob$deff) & rob$deff > 0
  if (!any(ok)) return(NULL)
  if (anyDuplicated(key[ok])) return(NULL)
  stats::setNames(as.double(rob$deff[ok]), key[ok])
}
