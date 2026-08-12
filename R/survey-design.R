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
#   3. tab_robust_overlay(): recompute each whole-table omnibus p-value ON A DESIGN -- the user's own,
#      or the flat one a weight column defines -- and overlay it on the classic `test` attribute
#      (Last Phase j; z16-iii made the two bases run the SAME survey estimator). survey::svychisq
#      (Rao-Scott 2nd-order F) for factors, svyglm + regTermTest's Wald F for means.
# DESIGN (Last Phase z16-i): the INFERENCE BASIS is derived, never asked for, and now STORED
#   (meta$inference). `test` says only WHETHER to test; what the user already passed says HOW.
#   `wt` says how the ESTIMATE is computed; the basis says how the INTERVAL is computed -- two
#   orthogonal facts, which is why the framework kept needing four encodings of one thing.
#     "n"              the raw sample size (unweighted, or weighted with the option off = the default)
#     "weights"        the design effect of the weights, exactly -- the flat ids = ~1 design
#     "design"         the full design: strata, clusters, fpc, calibration
#     "design_partial" a design was given but its variance could not be computed here
#   svy_inference_basis() is the ONLY place the option or the design object is read; every consumer
#   takes the resolved value. That is also why `ids`/`strata`/`fpc`/`nest` are gone: they reached the
#   omnibus p and nothing else, and svydesign() says all four better.
#   See dev/weights_framework_redesign.md S2.1, and R/survey-variance.R for the two variance
#   implementations the basis selects between.
# KEY CONSTRAINTS:
#   - The robust p replaces ONLY the p-value / statistic / df / n on the chi2 / F rows; the descriptive
#     effect size is carried through (it is computed on the same weighted table since z14-i).
#   - Robust tests run on complete cases of (row_var, col_var) per subtable (the survey convention);
#     this can differ slightly from the classic chi2 when na = "keep" counts NA as a category -- documented.
#   - Fisher rows are dropped in robust mode (the robust p is the answer there).
#   - This is the ONE architectural exception to "the test comes from the aggregate": a design-based
#     omnibus needs the observations. It runs only when the basis is not "n" -- i.e. opt-in.
# See: dev/full_survey_design_scope.md (z14-i); dev/tabxplor_2.0.0_decisions.md S51; CLAUDE.md Last Phase j.

# === SECTION: the design boundary ===================================================================

# Package-owned column names written into the unwrapped frame. `.svy_weights` is ALSO the fact "this
# table is design-based": it is the resolved weight name on every path (tab()'s vars_attr, the
# tab_plain/tab_num leaves, tab_reg's reg_meta), so tab_weight_line() reads it as a fact instead of
# printing it as a name (D7). `.svy_row` is the position into the ORIGINAL design, so a table built on
# PREPARED microdata (filtered, lumped, relabelled) can still index the design it came from.
svy_wt_col  <- ".svy_weights"
svy_row_col <- ".svy_row"

# THE total order of the four inference bases, WEAKEST first (see the DESIGN block above). Declared
# once, here, beside the resolver that produces them; tab_inference_bind() (R/tab_classes.R) takes the
# minimum, so a merged or bound table can never claim more inference than its weakest part carried.
inference_basis_order <- c("n", "weights", "design_partial", "design")

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
  # Last Phase z16-i (W7): the design's DEGREES OF FREEDOM, captured once at the boundary. survey
  # refers every interval to t(degf) where degf = #PSU - #strata; tabxplor referred proportions to z
  # and means to t(n_eff - 1), which is anti-conservative by up to 15 % below 30 PSUs. It rides the
  # spec to the leaves and then meta$inference, so the exported step path gets it too.
  list(data = frame, spec = list(design = data, wt = svy_wt_col,
                                 degf = svy_degf(data)))
}

# Last Phase z16-i (W10): `wt` beside a design is a contradiction, not a preference -- the design
# carries its own weights and the `wt` column was silently thrown away. Every other collision in the
# package aborts (a weight that is also a row_var, a row_var that is also a tab_var); this one now
# does too, from the ONE place both are visible. `wt_given` is TRUE when the user actually passed one.
svy_abort_wt_design <- function(wt_given, fn = "tab") {
  if (!isTRUE(wt_given)) return(invisible(NULL))
  cli::cli_abort(c(
    "{.arg wt} cannot be used when {.arg data} is a {.cls survey.design}.",
    "x" = "A design already carries its own sampling weights.",
    "i" = "Drop {.arg wt}, or build the design with those weights: \\
           {.code survey::svydesign(ids = ~1, weights = ~w, data = d)}."))
}

# The design's degrees of freedom, or NA when they cannot be had (never errors, never guesses).
svy_degf <- function(design) {
  d <- tryCatch(as.double(survey::degf(design)), error = function(e) NA_real_)
  if (length(d) != 1L || !is.finite(d) || d <= 0) NA_real_ else d
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
                  "{.code options(tabxplor.design_effect = TRUE)} makes it account for the",
                  "weighting, and a {.fn survey::svydesign} passed as {.arg data} gives a",
                  "design-based one.")
    ))
  isTRUE(test)
}

# THE inference basis (ruling Q2) -- resolved once, in tab_setup(), where the weight is resolved and
# the design_spec is in the ctx. That is why neither tab() nor tab_many() computes it: they used to
# drift (only tab() had the rule, so tab_many() was silently always classic).
# It governs the CELL INTERVALS, the whole-table test and the contrib residual alike -- one basis, one
# resolution, every inference in the table -- and Last Phase z16-i STORES it (meta$inference), so the
# footer, the exporters and jamovi can name it instead of re-deriving it from a weight-column name.
# `force` is how tab_reg() states its own rule (ruling 1): its crude Obs_* columns are ALWAYS on the
# weighted basis when weighted, so they always match the Model_* column beside them; the option is
# tab()-scoped and tab_reg() never reads it.
svy_inference_basis <- function(design_spec, wt, force = FALSE) {
  if (!is.null(design_spec) && !is.null(design_spec$design))          return("design")
  if (length(wt) > 0L &&
      (isTRUE(force) || isTRUE(getOption("tabxplor.design_effect", FALSE)))) return("weights")
  "n"
}

# "Is anything weighted here?" -- the ONE predicate (W12.3 counted three spellings of it: reg_fit(),
# reg_resolve_multiplier()'s caller, and the crude grid). A design always is; otherwise it is the
# presence of a weight, whatever its shape (NULL / character(0) / a name / a symbol).
svy_weighted <- function(design_spec = NULL, wt = NULL)
  !is.null(design_spec$design) || length(wt) > 0L

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
# `basis` is the resolved inference basis ("weights" / "design"); "n" never reaches here.
# `rv`/`cv` are the variable names, `is_num` its type, `wt` the weight name. Returns a one-row list
# with the test discriminator + (statistic, df1, df2, pvalue, n, deff); an all-NA row on any failure
# (never crashes tab()).
#
# DESIGN (Last Phase z16-iii, ruling 7): ONE estimator, two ways in. A weight column IS a survey
# design -- the flat one -- so the "weights" basis SYNTHESISES `svydesign(ids = ~1, weights = ~w)` and
# runs exactly the same survey estimator the "design" basis runs: survey::svychisq's Rao-Scott
# second-order F for factors, svyglm + regTermTest's Wald F for means. That is why there are two
# discriminators (`chi2` / `chi2_design`) and not four: labelling the two designs differently would be
# a second encoding of the basis, which meta$inference already stores.
#
# It replaces ~35 lines of hand-rolled statistics -- a FIRST-ORDER Rao-Scott rescale of the Pearson
# chi2 to Kish's n_eff, and a weighted ANOVA on per-group Kish n -- which were an approximation of
# exactly this. Not re-implementing it is the module's own standing rule ("`survey` owns the variance
# algebra", R/survey-variance.R): the closed form in survey-variance.R exists because the CELL
# variance is needed per cell in an O(cells) leaf, which is not the shape of a whole-table test.
#
# Last Phase z16-i (W8): `n` is ALWAYS the raw count -- at the old rung 2 it silently became the
# effective sample size, so one column meant two things depending on a global option. The effective
# information moved to `deff`, the mean design effect this test corrected by, at its own grain.
svy_omnibus_one <- function(sub, rv, cv, is_num, wt, basis, des_rows, design_spec) {
  disc   <- if (is_num) "F_design" else "chi2_design"
  na_row <- function() list(test = disc, statistic = NA_real_, df1 = NA_real_,
                            df2 = NA_real_, pvalue = NA_real_, n = NA_real_, deff = NA_real_)
  keep <- !is.na(sub[[rv]]) & !is.na(sub[[cv]])
  d    <- sub[keep, , drop = FALSE]
  if (!is_num) d[[rv]] <- droplevels(as.factor(d[[rv]]))
  d[[cv]] <- if (is_num) as.double(d[[cv]]) else droplevels(as.factor(d[[cv]]))
  n_obs <- nrow(d)
  if (!requireNamespace("survey", quietly = TRUE) || n_obs < 3) return(na_row())

  # The design the test runs on: the user's own (restricted to these rows and given the PREPARED
  # frame -- so the p describes the lumped, relabelled, filtered table that is actually displayed,
  # and an excluded row is a proper survey DOMAIN rather than a rebuilt design), or the flat one the
  # weights themselves define.
  des <- tryCatch(
    if (identical(basis, "design")) svy_domain_design(design_spec$design, des_rows[keep], d)
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
    # deff = Rao-Scott's mean generalized design effect, delta-bar = X2_Pearson / (F * df_Pearson).
    # svychisq's own `ndf` is Satterthwaite's d0, not (r-1)(c-1), so the Pearson df is recomputed here.
    # `ch$observed` is survey's own weighted table rescaled to the raw n, i.e. exactly the table
    # agg_chi2() works on -- so this X2 IS the classic statistic beside it.
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

# svy_omnibus_grid() -- THE PRODUCER: one robust omnibus per (subtable x col_var), straight from the
# microdata, as a raw tibble carrying `deff`. Returns NULL when there is nothing to compute.
# DESIGN (Last Phase z16-iv, W-B): this used to be the head of tab_robust_overlay(), which runs in
#   tab_assemble_tables(). It is split out because TWO consumers need the same numbers at two
#   different times: the `color = "contrib"` residual's base, DURING tab_transform() (the residual is
#   written by chi2_write_contrib(), inside tab_chi2()), and the `test` overlay, AFTER the numeric
#   ANOVA rows are bound in tab_assemble_tables(). Producing it ONCE is also what makes the omnibus p
#   and the cell colours of one table describe the SAME design effect -- they were 2.5 % apart at
#   basis "weights", and a factor 7 apart when the row_var is cluster-level.
# DESIGN (z14-i): the frame is ALWAYS the PREPARED microdata -- the table the user is looking at.
#   It used to be the design's own `$variables` on the prebuilt path, i.e. the ORIGINAL frame, so the
#   design-based p ignored `filter=`, `other_if_less_than` lumping and `cleannames` relabelling and
#   could describe a different table than the one printed (measured: a table displaying `a / Others`
#   carried the p of the unlumped `a / b / c`). The design is reached instead through `.svy_row`, the
#   position each prepared row holds in the original design -- which is also what makes an excluded
#   row a proper survey DOMAIN rather than a rebuilt design.
# `col_num` is a named logical (col_var -> is numeric); `comp = "all"` tests the whole table (one
# group), else one test per tab_var subtable.
# `totaltab_name` adds the TOTAL-TABLE group (every row, keyed by that name in each tab_var column):
#   chi2_compute_test() emits a test row for it, and the overlay used to drop it silently, because its
#   groups came from `unique(frame[tab_vars])`, which has no such level, and it REPLACES the classic
#   tibble. So a weighted / design table with tab_vars + totaltab = "table" lost its whole-table test.
svy_omnibus_grid <- function(data, row_var, col_vars, col_num, tab_vars, wt,
                             basis, design_spec, comp, totaltab_name = NULL) {
  frame    <- as.data.frame(data)
  if (nrow(frame) == 0 || length(col_vars) == 0) return(NULL)
  des_rows <- frame[[svy_row_col]]     # NULL without a design; positions into it otherwise
  tabvars_in <- intersect(tab_vars, names(frame))
  if (length(tabvars_in) == 0 || comp == "all") {
    groups <- list(list(keys = NULL, rows = seq_len(nrow(frame))))
  } else {
    gk <- unique(frame[tabvars_in])
    # the total table's tab_var value IS `totaltab_name` (leaf_rename_totals): expand each key column's
    # levels ONCE so the extra row binds as a FACTOR -- a character row would coerce the whole tab_var
    # column of `test` on the vec_rbind.
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
                           rows_g, design_spec)
      row <- tibble::tibble(row_var = row_var, col_var = cv, test = r$test,
                            statistic = r$statistic, df1 = r$df1, df2 = r$df2,
                            pvalue = r$pvalue, n = r$n, deff = r$deff)
      if (!is.null(g$keys)) row <- dplyr::bind_cols(g$keys, row)
      out[[length(out) + 1L]] <- row
    }
  }
  rob <- dplyr::bind_rows(out)
  if (nrow(rob) == 0) NULL else rob
}

# Overlay the producer's grid onto a classic `test` tibble: replace (statistic, df, pvalue, n, deff)
# per (subtable x col_var), keep the classic effect_size / es_type / min_e, drop Fisher. The result
# has chi2_compute_test's column shape, so every downstream reader (display, bind) is unchanged.
# The effect size is deliberately NOT recomputed here: it is descriptive, so it describes the weighted
#   population (chi2_compute_test already computes it on the weighted table), never the effective
#   sample an inferential rescale works in.
# `tabxplor.anova` (Welch vs classic F) is deliberately NOT read here: it chooses between two CLASSIC
# F statistics, and a design-based numeric test is the svyglm Wald F, which has no such variant.
# The semi_join is the "replace, never invent" rule: a grid row whose subtable the classic test does
# not have (a tab_var level emptied by the complete-case drop, or the total-table group of a table
# built with totaltab = "line") is dropped rather than injected with an NA effect size.
tab_robust_overlay <- function(test_tbl, rob, tab_vars) {
  if (is.null(test_tbl) || nrow(test_tbl) == 0) return(test_tbl)
  if (is.null(rob) || nrow(rob) == 0)           return(test_tbl)
  # the classic per-(subtable x col_var) effect-size / validity facts to carry through
  es_keep    <- test_tbl[test_tbl$test %in% c("chi2", "F_welch"), , drop = FALSE]
  tabvars_in <- intersect(tab_vars, names(test_tbl))
  jk  <- intersect(c(tabvars_in, "col_var"), names(es_keep))
  rob <- dplyr::semi_join(rob, dplyr::distinct(es_keep[jk]), by = jk)
  if (nrow(rob) == 0) return(test_tbl)
  rob <- dplyr::left_join(
    rob, dplyr::select(es_keep, dplyr::all_of(jk), "min_e", "effect_size", "es_type"), by = jk)
  dplyr::relocate(rob, tidyselect::any_of(c(tabvars_in, "row_var", "col_var")))
}

# svy_deff_lookup() -- key a producer grid onto a BUILT table's groups: a named numeric (Rao-Scott's
# mean generalized design effect) keyed on `paste(<group key tuple>, col_var, sep = "\r")`.
# `svy_key_chr()` (R/survey-variance.R) spells BOTH sides the same way -- it exists for exactly this
# ("key values as the wide table and the microdata both spell them"), so there is no second key
# convention. NULL -- "no correction available, use the ladder" -- when the grid cannot be keyed onto
# these groups: a grouping it does not carry, or an ambiguous grain. Never a wrong number.
svy_deff_lookup <- function(rob, group_vars) {
  if (is.null(rob) || !nrow(rob) || !"deff" %in% names(rob))  return(NULL)
  if (length(group_vars) && !all(group_vars %in% names(rob))) return(NULL)
  key <- if (length(group_vars))
    do.call(paste, c(lapply(rob[group_vars], svy_key_chr), list(rob$col_var), list(sep = "\r")))
  else paste("", rob$col_var, sep = "\r")
  ok <- !is.na(rob$deff) & is.finite(rob$deff) & rob$deff > 0
  if (!any(ok)) return(NULL)
  if (anyDuplicated(key[ok])) return(NULL)
  stats::setNames(as.double(rob$deff[ok]), key[ok])
}
