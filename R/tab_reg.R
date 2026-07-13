# PURPOSE: Regression tables (effect measures) as native tabxplor_tab objects.
# ROLE: tab_reg() fits one model per column across families and renders the per-family effect measure
#   -- gaussian beta (additive), binomial OR / poisson IRR / multinomial OR / ordinal cumulative OR
#   (multiplicative) -- through the tabxplor_fmt diff|or / ci_inf|ci_sup / pvalue / var fields, so a
#   regression table prints, colours and exports (kable / md / Excel) exactly like a crosstab.
#   tab_logit()/multi_logit() are thin binomial-family wrappers with the curated binary-outcome UX.
# KEY CONSTRAINTS:
#   - Direct engine: stats::lm/glm (unweighted) / survey::svyglm (weighted) + nnet::multinom (nominal
#     3+ level) + MASS::polr (ordinal 3+ level), all tidied with broom::tidy. No parsnip.
#   - broom (always), survey (wt path), MASS (ordinal + method="profile"), nnet (multinomial), brant
#     (ordinal PO diagnostic) are Suggests -> guarded.
#   - CI <-> p are DUALS (CI <-> stars can never disagree). method="wald" (default): in-house Wald
#     CI (coef +/- crit*se, exp()'d for ratio measures) + the model's own Wald p; crit is z for
#     fixed-dispersion glm (binomial/poisson), t(df.residual) for lm / quasi* / weighted svyglm --
#     matching broom's z/t p exactly. method="profile" (unweighted glm): confint + LR-test p.
#   - Effect shape is driven by `exponentiate` (default "nongaussian"): MULTIPLICATIVE (OR/IRR) ->
#     the `or` field, type="row", display="or", ci_type="or", color="OR" (neutral 1, 1/x reciprocal);
#     ADDITIVE (gaussian beta / log-odds) -> the `diff` field, type="coef", display="coef",
#     ci_type="diff", color="diff" (neutral 0), with `var`=var(Y) so the colour is the effect-size
#     beta/SD(Y) against the mean_diff (Cohen) breaks -- the additive twin of OR-coloured-by-ratio.
#   - 12c-ii: `trials` fits a summed-score outcome as GROUPED binomial (cbind(score, trials-score));
#     a model FORMULA in `dependent` is the escape hatch -- a simple `y ~ a + b` reduces to the
#     dependent+predictors path, a compound one (interactions / poly() / I()) is fit verbatim with a
#     best-effort skeleton read from the fitted terms (reg_skeleton_from_fit).
#   - 12d: nominal 3+ level -> ONE multinom -> reg_build splits its `y.level` tidy into one OR column
#     per non-reference category ("<j> vs <ref>: OR"); the outcome baseline is set by `reference`
#     keyed on the dependent. Ordered 3+ level -> polr -> one cumulative-OR column (cut-point rows
#     dropped -> "Constant" NA), with a Brant PO diagnostic (reg_ordinal_diagnostic, self-heals the
#     fit's $call so brant works out of the fitting scope). Both reuse the OR fmt shape unchanged;
#     both share reg_wald_from_tidy so CI <-> p <-> stars stay exact duals. Weighted MNL/ordinal
#     deferred (guarded error).
# See: CLAUDE.md Phase 12c/12d ; dev/tabxplor_1.4.0_decisions.md S37.

# === Internal engine ================================================================

# broom is needed for every fit; survey only for the weighted (wt) path; nnet / MASS for the
# nominal (multinomial) / ordinal (proportional-odds) families (both R Recommended -> normally present).
reg_check_deps <- function(family, wt) {
  if (!requireNamespace("broom", quietly = TRUE)) {
    cli::cli_abort(c(
      "{.pkg broom} is required for regression tables.",
      "i" = 'Install it with {.code install.packages("broom")}.'
    ))
  }
  if (!is.null(wt) && !requireNamespace("survey", quietly = TRUE)) {
    cli::cli_abort(c(
      "{.pkg survey} is required for weighted regression (the {.arg wt} argument).",
      "i" = 'Install it with {.code install.packages("survey")}.'
    ))
  }
  if (family == "multinomial" && !requireNamespace("nnet", quietly = TRUE)) {
    cli::cli_abort(c(
      "{.pkg nnet} is required for multinomial (nominal 3+ level) outcomes.",
      "i" = 'Install it with {.code install.packages("nnet")}.'
    ))
  }
  if (family == "ordinal" && !requireNamespace("MASS", quietly = TRUE)) {
    cli::cli_abort(c(
      "{.pkg MASS} is required for ordinal (proportional-odds) outcomes.",
      "i" = 'Install it with {.code install.packages("MASS")}.'
    ))
  }
}

# Parse a formula escape-hatch (D9). Returns the LHS outcome name, the bare RHS variables (for
# reference= / family-detect / labels), the verbatim formula, and `simple`: TRUE iff LHS is a bare
# column name AND every RHS term is a bare main-effect column of `data` (no `:`, poly(), I(), calls).
# A simple formula reduces losslessly to the dependent+predictors character path; a compound one is
# fit verbatim with a skeleton read from the fitted terms (reg_skeleton_from_fit).
reg_parse_formula <- function(formula, data) {
  lhs <- rlang::f_lhs(formula)
  if (is.null(lhs)) {
    cli::cli_abort("A regression {.arg formula} needs a response, e.g. {.code y ~ x1 + x2}.")
  }
  lhs_is_name <- rlang::is_symbol(lhs)
  dependent   <- if (lhs_is_name) rlang::as_string(lhs) else all.vars(lhs)[1]

  tt     <- stats::terms(formula, data = data)
  labels <- attr(tt, "term.labels")
  orders <- attr(tt, "order")
  rhs_vars <- all.vars(rlang::f_rhs(formula))

  simple <- lhs_is_name &&
    dependent %in% names(data) &&
    length(labels) > 0L &&
    all(orders == 1L) &&
    all(labels %in% names(data))

  list(dependent = dependent, predictors = rhs_vars, labels = labels,
       formula = formula, lhs_is_name = lhs_is_name, simple = simple)
}

# Auto-detect the model family from the outcome (a message is emitted). The safe data-driven rules
# (S37 D2): 0/1 or any 2-level outcome -> binomial; an ORDERED factor with 3+ levels -> ordinal
# (proportional-odds); an UNORDERED factor / character with 3+ levels -> multinomial; a non-integer
# numeric -> gaussian. An integer/count with 3+ values stays ambiguous (poisson vs grouped-binomial
# vs gaussian) and must be named explicitly.
reg_detect_family <- function(data, dependent) {
  y <- data[[dependent]]
  u <- unique(stats::na.omit(y))
  if (length(u) == 2L) {
    cli::cli_inform(c("i" = paste0(
      "{.val {dependent}}: binary outcome detected -> {.code family = \"binomial\"} (logistic)."
    )))
    return("binomial")
  }
  if (is.ordered(y) && length(u) >= 3L) {
    cli::cli_inform(c("i" = paste0(
      "{.val {dependent}}: ordered outcome detected -> {.code family = \"ordinal\"} ",
      "(proportional-odds)."
    )))
    return("ordinal")
  }
  if ((is.factor(y) || is.character(y)) && length(u) >= 3L) {
    cli::cli_inform(c("i" = paste0(
      "{.val {dependent}}: nominal outcome detected -> {.code family = \"multinomial\"} (multinomial ",
      "logistic)."
    )))
    return("multinomial")
  }
  if (is.numeric(y) && any(y %% 1 != 0, na.rm = TRUE)) {
    cli::cli_inform(c("i" = paste0(
      "{.val {dependent}}: continuous outcome detected -> {.code family = \"gaussian\"} (linear)."
    )))
    return("gaussian")
  }
  cli::cli_abort(c(
    "Cannot auto-detect the model family for {.val {dependent}}.",
    "i" = paste0("Set {.arg family} explicitly: {.val gaussian} (linear), {.val poisson} (counts), ",
                 "{.val binomial} (logistic), {.val multinomial} / {.val ordinal} (3+ level).")
  ))
}

# The effect-measure word shown per column (S37 D1 -- auto-labelled per column, never one global
# header). Additive (raw) -> beta ; multiplicative -> OR (binomial) / IRR (poisson) / exp(beta).
reg_effect_word <- function(family, do_exp) {
  if (!do_exp) return("\u03b2")                  # beta (raw / log-odds coefficient)
  switch(family,
         "binomial" = , "multinomial" = , "ordinal" = "OR",
         "poisson" = , "quasipoisson" = "IRR",
         "exp(\u03b2)")
}

# A one-line note appended to the table's subtext, so a multinomial / ordinal table self-documents its
# estimand (the "vs <ref>" per-category detail lives in the column labels).
reg_model_note <- function(family, do_exp) {
  switch(family,
    "ordinal"     = if (do_exp) "Cumulative odds ratios (proportional-odds model)."
                    else        "Proportional-odds model (log-odds coefficients).",
    "multinomial" = if (do_exp) "Multinomial odds ratios (each category vs the reference)."
                    else        "Multinomial log-odds coefficients.",
    NULL)
}

# Prepare a binary dependent: a 0/1 numeric becomes a 2-level factor ("Not <dep>" / "<dep>"); any
# other input must have exactly 2 levels, optionally reversed so glm models the FIRST level
# (inverse_two_level_factors -- the maintainer's convention, e.g. "1-Married" first = modelled). The
# modelled ("positive") level is returned as an attribute for the column label.
reg_prep_binary <- function(data, dependent, inverse_two_level_factors) {
  y <- data[[dependent]]
  if (is.numeric(y) && all(stats::na.omit(y) %in% c(0, 1))) {
    y <- factor(y, levels = c(0, 1), labels = c(paste0("Not ", dependent), dependent))
    positive <- dependent
  } else {
    y <- forcats::fct_drop(as.factor(y))
    if (nlevels(y) != 2L) {
      cli::cli_abort(c(
        "The dependent variable {.val {dependent}} must be binary (2 levels).",
        "x" = "It has {nlevels(y)} level{?s}: {.val {levels(y)}}.",
        "i" = paste0("For a summed-score outcome (0..q items), pass {.arg trials} to fit a grouped ",
                     "binomial."),
        "i" = "Multinomial / 3+ level outcomes are planned for a later phase (12d)."
      ))
    }
    if (inverse_two_level_factors) y <- forcats::fct_rev(y)
    positive <- levels(y)[2L]
  }
  data[[dependent]] <- y
  attr(data, "positive_level") <- positive
  data
}

# The modelled ("positive") level of a binary dependent, computed from the raw data (for the column
# label, before fitting). Mirrors reg_prep_binary()'s choice.
reg_positive_level <- function(data, dependent, inverse_two_level_factors) {
  y <- data[[dependent]]
  if (is.numeric(y) && all(stats::na.omit(y) %in% c(0, 1))) return(dependent)
  lv <- levels(forcats::fct_drop(as.factor(y)))
  lv[if (inverse_two_level_factors) 1L else 2L]
}

# Relevel factor predictors to user-chosen treatment-contrast baselines. `reference` is a named
# vector c(var = "baseline level"). Regression references are model contrasts (fct_relevel), NOT the
# crosstab comparison-row semantics of resolve_ref_vector().
reg_apply_references <- function(data, reference, predictors) {
  nm <- names(reference)
  if (is.null(nm) || any(!nzchar(nm))) {
    cli::cli_abort(c("{.arg reference} must be a named vector, e.g. {.code c(race = \"White\")}."))
  }
  extra <- setdiff(nm, predictors)
  if (length(extra) > 0L) {
    cli::cli_warn("{.arg reference} name{?s} {.val {extra}} match no predictor; ignored.")
  }
  for (v in intersect(nm, predictors)) {
    f <- data[[v]]
    if (!is.factor(f) && !is.character(f)) {
      cli::cli_warn("{.arg reference} ignored for {.val {v}}: not a factor/character predictor.")
      next
    }
    f   <- as.factor(f)
    lev <- reference[[v]]
    if (!lev %in% levels(f)) {
      cli::cli_abort(c("{.arg reference} level {.val {lev}} not found in {.val {v}}.",
                       "i" = "Levels: {.val {levels(f)}}."))
    }
    data[[v]] <- forcats::fct_relevel(f, lev)
  }
  data
}

# The (var, level, term, is_ref) row skeleton for a set of predictors, in display order: the
# intercept ("Constant") first, then each predictor's levels (factor / character) -- first level =
# reference, no model term -- or a single row for a numeric predictor. `term` matches lm/glm/svyglm
# coefficient names so a fit aligns to the skeleton by term.
reg_skeleton <- function(data, predictors) {
  parts <- purrr::map(predictors, function(p) {
    v <- data[[p]]
    if (is.factor(v) || is.character(v)) {
      lv <- levels(forcats::fct_drop(as.factor(v)))
      tibble::tibble(
        var    = p,
        level  = lv,
        term   = c(NA_character_, paste0(p, lv[-1])),
        is_ref = c(TRUE, rep(FALSE, length(lv) - 1L))
      )
    } else {
      tibble::tibble(var = p, level = p, term = p, is_ref = FALSE)
    }
  })
  dplyr::bind_rows(
    tibble::tibble(var = "Constant", level = "Reference population",
                   term = "(Intercept)", is_ref = TRUE),
    parts
  )
}

# The skeleton for the COMPOUND-formula path (D9): built from the FITTED model, not the data, so the
# rows match whatever the user's formula produced. Intercept first, then each fitted term: a pure
# factor main effect (its label is one of fit$xlevels) expands to level rows (first = reference, no
# term); every other term (numeric main effect, interaction, poly(), I(), fn call) emits one row per
# coefficient column assigned to it, labelled by the coefficient name -- best-effort, no reference row.
# `term` values equal the model-matrix column names, which broom::tidy() reproduces, so reg_column()
# aligns by term exactly as on the data-skeleton path.
reg_skeleton_from_fit <- function(fit) {
  tt      <- stats::terms(fit)
  labels  <- attr(tt, "term.labels")
  assign  <- attr(stats::model.matrix(fit), "assign")   # 0 = intercept, k = labels[k]
  coefnms <- names(stats::coef(fit))
  xlev    <- fit$xlevels

  parts <- purrr::map(seq_along(labels), function(k) {
    lab  <- labels[k]
    cols <- coefnms[assign == k]
    if (lab %in% names(xlev)) {                          # pure factor main effect -> level rows
      lv <- xlev[[lab]]
      tibble::tibble(
        var    = lab,
        level  = lv,
        term   = c(NA_character_, paste0(lab, lv[-1])),
        is_ref = c(TRUE, rep(FALSE, length(lv) - 1L))
      )
    } else {                                             # numeric / interaction / poly / I() -> terms
      lvl <- sub(paste0("^", term_prefix(lab)), "", cols)
      lvl[!nzchar(lvl)] <- cols[!nzchar(lvl)]            # a single-column term keeps the full name
      tibble::tibble(var = lab, level = lvl, term = cols, is_ref = FALSE)
    }
  })
  dplyr::bind_rows(
    tibble::tibble(var = "Constant", level = "Reference population",
                   term = "(Intercept)", is_ref = TRUE),
    parts
  )
}

# Strip a term's own name off its coefficient names for a shorter `level` label (best-effort):
# "poly(age, 2)1" -> "1"; a name we can't shorten is left whole. Regex-escaped so poly()/I() are safe.
term_prefix <- function(label) {
  stringr::str_replace_all(label, "([.\\\\+*?\\[^\\]$(){}=!<>|:#/-])", "\\\\\\1")
}

# Per-coefficient LIKELIHOOD-RATIO p-values (the dual of the profile-likelihood CI). Each coefficient
# is dropped from the model matrix in turn and the deviance change is a 1-df chi-square. Unweighted
# glm only (binomial/poisson); for a factor it tests one level vs the reference, matching the
# per-level effect the table shows.
reg_lr_pvalues <- function(fit) {
  X   <- stats::model.matrix(fit)
  y   <- fit$y
  w   <- fit$prior.weights
  off <- fit$offset
  if (is.null(off)) off <- rep(0, length(y))
  ic  <- which(colnames(X) == "(Intercept)")
  dev_full <- fit$deviance
  p <- vapply(seq_len(ncol(X)), function(j) {
    red <- suppressWarnings(stats::glm.fit(
      X[, -j, drop = FALSE], y, weights = w, offset = off, family = fit$family,
      intercept = length(ic) > 0L && j != ic
    ))
    stats::pchisq(red$deviance - dev_full, df = 1, lower.tail = FALSE)
  }, numeric(1))
  stats::setNames(p, stringr::str_remove_all(colnames(X), "`"))
}

# Wald CI + p from a tidy carrying `estimate` + `std.error` on the log scale. multinom / polr are ML
# with fixed dispersion, so the quantile is z (qnorm) -- the same branch the fixed-dispersion glm path
# uses. Both CI and p come from estimate/se, so they are exact duals (CI <-> stars can never disagree),
# and both survive an NaN se (a rank-deficient / empty cell -> NaN, matching the base model). `do_exp`
# exponentiates the estimate and the bounds (OR/IRR). Fills conf.low/conf.high/p.value in place.
reg_wald_from_tidy <- function(td, conf_level, do_exp) {
  crit <- stats::qnorm(1 - (1 - conf_level) / 2)
  lo   <- td$estimate - crit * td$std.error
  hi   <- td$estimate + crit * td$std.error
  p    <- 2 * stats::pnorm(-abs(td$estimate / td$std.error))
  est  <- td$estimate
  if (do_exp) { est <- exp(est); lo <- exp(lo); hi <- exp(hi) }
  td$estimate <- est; td$conf.low <- lo; td$conf.high <- hi; td$p.value <- p
  td
}

# Nominal 3+ level outcome: ONE multinomial logit (nnet::multinom). exp(coef) is the "OR (j vs the
# reference outcome level)" -- the Begg-Gray estimand, one set of coefficients per non-reference
# category (the tidy carries a `y.level` column that reg_build splits into one OR column per category).
# The reference category is the outcome factor's FIRST level (set via `reference` upstream, MNL only).
reg_fit_multinom <- function(mdata, dependent, predictors, do_exp, conf_level, method) {
  if (method == "profile") {
    cli::cli_inform(c("!" = "Profile intervals are not defined for multinomial models; using Wald."))
  }
  mdata[[dependent]] <- forcats::fct_drop(as.factor(mdata[[dependent]]))
  y_levels <- levels(mdata[[dependent]])
  fml <- stats::as.formula(paste0(
    "`", dependent, "` ~ ", paste0("`", predictors, "`", collapse = " + ")
  ))
  fit <- nnet::multinom(fml, data = mdata, trace = FALSE)
  td  <- broom::tidy(fit)                              # y.level, term, estimate, std.error, ...
  td$term <- stringr::str_remove_all(td$term, "`")     # strip formula backticks -> match skeleton
  td  <- reg_wald_from_tidy(td, conf_level, do_exp)
  list(tidy = td, nobs = nrow(mdata), var_y = NA_real_, positive_level = NULL,
       fit = fit, y_ref = y_levels[1], y_levels = y_levels[-1])
}

# Ordered 3+ level outcome: proportional-odds cumulative logit (MASS::polr). exp(coef) is one
# cumulative OR per predictor level -> ONE column (the cut-point "scale" rows are dropped, so the
# skeleton "Constant" cell stays NA). The parallel-lines assumption is diagnosed (Brant test -> warn).
reg_fit_ordinal <- function(mdata, dependent, predictors, do_exp, conf_level, method) {
  if (method == "profile") {
    cli::cli_inform(c("!" = "Profile intervals are not defined for proportional-odds models; using Wald."))
  }
  y <- mdata[[dependent]]
  if (!is.ordered(y)) {
    y <- as.ordered(forcats::fct_drop(as.factor(y)))
    lv_str <- paste(levels(y), collapse = " < ")
    cli::cli_inform(c("i" = "{.val {dependent}}: treated as ordered ({lv_str})."))
  } else {
    y <- forcats::fct_drop(y)
  }
  mdata[[dependent]] <- y
  fml <- stats::as.formula(paste0(
    "`", dependent, "` ~ ", paste0("`", predictors, "`", collapse = " + ")
  ))
  fit <- MASS::polr(fml, data = mdata, Hess = TRUE, method = "logistic")
  td  <- broom::tidy(fit)
  td  <- td[td$coef.type == "coefficient", , drop = FALSE]   # drop cut-point ("scale") intercepts
  td$term <- stringr::str_remove_all(td$term, "`")
  td  <- reg_wald_from_tidy(td, conf_level, do_exp)
  reg_ordinal_diagnostic(fit)                                # Brant PO test -> warn (gated on brant)
  list(tidy = td, nobs = nrow(mdata), var_y = NA_real_, positive_level = NULL, fit = fit)
}

# Diagnose the proportional-odds (parallel-lines) assumption with the Brant test (the `brant` package,
# a Suggests). Warn when the omnibus test rejects; a missing `brant` skips it with a hint; a failing
# test (sparse data) is swallowed -- a diagnostic must never break the table.
reg_ordinal_diagnostic <- function(fit) {
  if (!requireNamespace("brant", quietly = TRUE)) {
    cli::cli_inform(c("i" = paste0(
      "Proportional-odds (parallel-lines) assumption not tested: install {.pkg brant} to run the ",
      "Brant test."
    )))
    return(invisible())
  }
  # brant rebuilds the model frame via eval.parent(fit$call), needing the fit's `data`/`formula`
  # SYMBOLS resolvable in brant's caller frame -- which fails once we are past the fitting scope. Make
  # this (copy-on-modify) fit self-contained from its own stored model frame so brant works anywhere.
  fit$call$data    <- fit$model
  fit$call$formula <- stats::formula(fit)
  bt <- tryCatch({ utils::capture.output(res <- brant::brant(fit)); res },
                 error = function(e) NULL)
  if (is.null(bt) || !is.matrix(bt) ||
      !"Omnibus" %in% rownames(bt) || !"probability" %in% colnames(bt)) {
    return(invisible())                                      # unexpected shape -> stay silent
  }
  p <- suppressWarnings(as.numeric(bt["Omnibus", "probability"]))
  if (!is.na(p) && p < 0.05) {
    cli::cli_warn(c(
      "!" = "The proportional-odds (parallel-lines) assumption is rejected (Brant omnibus p = {signif(p, 2)}).",
      "i" = paste0("Cumulative odds ratios may mislead; consider {.code family = \"multinomial\"} or a ",
                   "partial proportional-odds model."),
      "i" = "The Brant test over-rejects at large N; inspect the per-variable tests too."
    ))
  }
  invisible()
}

# Fit ONE model on complete cases -> a tidy of the (per-family) effect measure + CI + p + the model n
# (+ var(Y) for the additive gaussian effect-size colour). `do_exp` chooses the estimate scale:
# TRUE -> exp(coef) (OR/IRR, multiplicative); FALSE -> raw coef (beta, additive). Wald CI uses z for
# fixed-dispersion glm (binomial/poisson), else t(df.residual); this matches broom's own z/t p, so
# the CI and the stars are exact duals. method="profile" (unweighted glm) swaps to confint + LR p.
reg_fit <- function(data, dependent, predictors, family, wt, do_exp,
                    inverse_two_level_factors, conf_level, method,
                    trials = NULL, formula = NULL) {
  mdata <- tidyr::drop_na(data, tidyselect::all_of(c(dependent, predictors, wt)))

  fac_preds <- predictors[purrr::map_lgl(
    predictors, ~ is.factor(mdata[[.]]) || is.character(mdata[[.]])
  )]
  if (length(fac_preds) > 0L) {
    mdata <- dplyr::mutate(mdata, dplyr::across(
      tidyselect::all_of(fac_preds), ~ forcats::fct_drop(as.factor(.))
    ))
  }

  # 3+ level categorical outcomes have their own engines (nnet::multinom / MASS::polr); they share the
  # Wald machinery (reg_wald_from_tidy) so the CI <-> p <-> stars duality holds, but not the glm path.
  if (family == "multinomial") {
    return(reg_fit_multinom(mdata, dependent, predictors, do_exp, conf_level, method))
  }
  if (family == "ordinal") {
    return(reg_fit_ordinal(mdata, dependent, predictors, do_exp, conf_level, method))
  }

  positive_level <- NULL
  weighted <- !is.null(wt)
  # grouped binomial: a summed-score outcome (0..trials) fit as cbind(score, trials-score) (D2). Only
  # on the non-formula path (a compound formula controls its own LHS, so `trials` does not apply).
  grouped <- family == "binomial" && !is.null(trials) && is.null(formula)
  if (grouped) {
    s <- mdata[[dependent]]
    if (!is.numeric(s) || any(s %% 1 != 0, na.rm = TRUE)) {
      cli::cli_abort(c("A summed-score outcome ({.arg trials}) must be integer-valued.",
                       "x" = "{.val {dependent}} is {.cls {class(s)}}."))
    }
    if (any(s < 0 | s > trials, na.rm = TRUE)) {
      cli::cli_abort(c("{.val {dependent}} scores must lie in {.val {0}}..{.val {trials}} (= {.arg trials}).",
                       "x" = "Observed range: {.val {range(s, na.rm = TRUE)}}."))
    }
    mdata[[".gb_succ"]] <- s
    mdata[[".gb_fail"]] <- trials - s
  }

  fam_obj <- switch(
    family,
    "binomial" = {
      if (is.null(trials) && is.null(formula)) {
        mdata <- reg_prep_binary(mdata, dependent, inverse_two_level_factors)
        positive_level <- attr(mdata, "positive_level")
      }
      if (weighted) stats::quasibinomial("logit") else stats::binomial("logit")
    },
    "poisson" = if (weighted) stats::quasipoisson("log") else stats::poisson("log"),
    "quasipoisson" = stats::quasipoisson("log"),
    "gaussian" = stats::gaussian(),
    cli::cli_abort("Unsupported {.arg family}: {.val {family}}.")
  )
  if (is.null(formula) && !grouped && family != "binomial" && !is.numeric(mdata[[dependent]])) {
    cli::cli_abort(c(
      "A {.val {family}} outcome must be numeric.",
      "x" = "{.val {dependent}} is {.cls {class(mdata[[dependent]])}}."
    ))
  }

  fml <- if (!is.null(formula)) {
    formula                                            # compound escape-hatch: fit verbatim
  } else {
    resp <- if (grouped) "cbind(`.gb_succ`, `.gb_fail`)" else paste0("`", dependent, "`")
    stats::as.formula(paste0(
      resp, " ~ ", paste0("`", predictors, "`", collapse = " + ")
    ))
  }

  fit <- if (family == "gaussian" && !weighted) {
    stats::lm(fml, data = mdata)
  } else if (!weighted) {
    stats::glm(fml, data = mdata, family = fam_obj)
  } else {
    design <- survey::svydesign(ids = ~1,
                                weights = stats::as.formula(paste0("~`", wt, "`")),
                                data = mdata)
    survey::svyglm(fml, design = design, family = fam_obj)
  }

  td <- broom::tidy(fit)                            # native scale: estimate, std.error, p.value
  td$term <- stringr::str_remove_all(td$term, "`")  # strip formula backticks -> match skeleton

  use_profile <- method == "profile" && !weighted && family %in% c("binomial", "poisson")
  if (method == "profile" && weighted) {
    cli::cli_inform(c("!" = paste0("Profile-likelihood intervals are not defined for survey-weighted ",
                                   "models; using Wald.")))
  }

  if (use_profile) {
    if (!requireNamespace("MASS", quietly = TRUE)) {
      cli::cli_abort(c('{.pkg MASS} is required for {.code method = "profile"}.',
                       "i" = '- Install it, or use {.code method = "wald"} (the default).'))
    }
    ci  <- suppressMessages(stats::confint(fit, level = conf_level))   # log/native scale
    idx <- match(td$term, stringr::str_remove_all(rownames(ci), "`"))
    lo  <- unname(ci[idx, 1]); hi <- unname(ci[idx, 2])
    lrp <- reg_lr_pvalues(fit)
    td$p.value <- unname(lrp[match(td$term, names(lrp))])
  } else {
    # z for fixed-dispersion glm (binomial/poisson, unweighted); else t (lm, quasi*, weighted svyglm)
    disp_known <- !weighted && family %in% c("binomial", "poisson")
    crit <- if (disp_known) stats::qnorm(1 - (1 - conf_level) / 2)
            else            stats::qt(1 - (1 - conf_level) / 2, df = stats::df.residual(fit))
    lo <- td$estimate - crit * td$std.error
    hi <- td$estimate + crit * td$std.error
  }

  est <- td$estimate
  if (do_exp) { est <- exp(est); lo <- exp(lo); hi <- exp(hi) }
  td$estimate <- est; td$conf.low <- lo; td$conf.high <- hi

  # var(Y) drives the additive gaussian effect-size colour (beta/SD(Y)); NA otherwise (no std colour)
  var_y <- if (!do_exp && family == "gaussian") stats::var(mdata[[dependent]]) else NA_real_

  list(tidy = td, nobs = nrow(mdata), var_y = var_y, positive_level = positive_level, fit = fit)
}

# Align one fit to the union skeleton -> a single fmt column (length = nrow(skeleton)), in the
# additive (beta) or multiplicative (OR/IRR) shape. Reference LEVELS of predictors present in this
# model get the neutral value (0 / 1, no CI/p); predictors ABSENT from this model stay NA (empty
# cells); the Constant carries the intercept (baseline) estimate.
reg_column <- function(skeleton, fit_res, model_predictors, col_var, effect_shape,
                       color, color_signif) {
  td  <- fit_res$tidy
  m   <- match(skeleton$term, td$term)
  est <- td$estimate[m]
  lo  <- td$conf.low[m]
  hi  <- td$conf.high[m]
  p   <- td$p.value[m]

  in_model <- skeleton$var %in% c("Constant", model_predictors)
  ref_lvl  <- skeleton$is_ref & skeleton$var != "Constant" & in_model
  neutral  <- if (effect_shape == "ratio") 1 else 0
  est[ref_lvl] <- neutral
  lo[ref_lvl]  <- NA_real_
  hi[ref_lvl]  <- NA_real_
  p[ref_lvl]   <- NA_real_

  n_rows   <- nrow(skeleton)
  refrows  <- ref_lvl | skeleton$var == "Constant"

  if (effect_shape == "ratio") {
    fmt(
      n = rep(as.integer(fit_res$nobs), n_rows),
      or = est, ci_inf = lo, ci_sup = hi, pvalue = p,
      type = "row", display = "or", digits = 2L, ref = "1", ci_type = "or",
      color = color, color_signif = color_signif, col_var = col_var,
      comp_all = FALSE, in_refrow = refrows
    )
  } else {
    fmt(
      n = rep(as.integer(fit_res$nobs), n_rows),
      diff = est, ci_inf = lo, ci_sup = hi, pvalue = p,
      var = rep(fit_res$var_y, n_rows),                 # var(Y): standardizes beta/SD(Y) for colour
      type = "coef", display = "coef", digits = 2L, ci_type = "diff",
      color = color, color_signif = color_signif, col_var = col_var,
      comp_all = FALSE, in_refrow = refrows
    )
  }
}

# Split ONE multinomial fit into one OR column per non-reference outcome category. Each category's
# tidy rows (`y.level == j`, y.level dropped) look like a standard glm tidy, so reg_column() aligns
# them to the shared predictor skeleton unchanged. Label = "<j> vs <ref>: OR" (prefixed by the
# dependent when several dependents / models coexist, to disambiguate). Returns a list of {label, col}.
reg_columns_multinom <- function(skeleton, f, sp, effect_shape, color, color_signif,
                                 eff_word, cleannames, prefix_dep) {
  y_ref <- if (cleannames) stringr::str_remove_all(f$y_ref, cleannames_condition()) else f$y_ref
  purrr::map(f$y_levels, function(j) {
    sub      <- f
    sub$tidy <- f$tidy[f$tidy$y.level == j,
                       setdiff(names(f$tidy), "y.level"), drop = FALSE]
    jc  <- if (cleannames) stringr::str_remove_all(j, cleannames_condition()) else j
    lab <- paste0(if (prefix_dep) paste0(sp$dependent, " - ") else "",
                  jc, " vs ", y_ref, ": ", eff_word)
    list(label = lab,
         col   = reg_column(skeleton, sub, sp$predictors, lab, effect_shape, color, color_signif))
  })
}

# The shared builder: fit every column spec, align to one skeleton, assemble a grouped_tab. specs =
# list of list(dependent, predictors, label, trials, formula, compound). The data-skeleton (union of
# the specs' predictors) is used unless a spec is a compound formula (single model), in which case the
# skeleton is read from its fitted terms (reg_skeleton_from_fit). Fit-all first so the skeleton can
# come from the fit before the columns are aligned. A multinomial fit contributes SEVERAL columns
# (one per outcome category), so the per-spec columns are flattened into one (label, col) list.
reg_build <- function(data, specs, union_predictors, family, wt, do_exp, effect_shape,
                      inverse_two_level_factors, conf_level, method, color, color_signif,
                      cleannames, subtext, eff_word) {
  fits <- purrr::map(specs, function(sp) {
    reg_fit(data, sp$dependent, sp$predictors, family, wt, do_exp,
            inverse_two_level_factors, conf_level, method,
            trials = sp$trials, formula = sp$formula)
  })

  compound <- purrr::map_lgl(specs, ~ isTRUE(.$compound))
  skeleton <- if (any(compound)) reg_skeleton_from_fit(fits[[1]]$fit)
              else                reg_skeleton(data, union_predictors)

  multi_col  <- family == "multinomial"
  prefix_dep <- length(specs) > 1L
  built <- purrr::flatten(purrr::map2(fits, specs, function(f, sp) {
    if (multi_col) {
      reg_columns_multinom(skeleton, f, sp, effect_shape, color, color_signif,
                           eff_word, cleannames, prefix_dep)
    } else {
      # a compound formula is one model: every skeleton row belongs to it (else compound-term rows go NA)
      model_predictors <- if (isTRUE(sp$compound)) unique(skeleton$var) else sp$predictors
      list(list(label = sp$label,
                col   = reg_column(skeleton, f, model_predictors, sp$label,
                                   effect_shape, color, color_signif)))
    }
  }))
  labels <- make.unique(purrr::map_chr(built, "label"))

  disp_levels <- skeleton$level
  if (cleannames) {
    disp_levels <- stringr::str_remove_all(disp_levels, cleannames_condition())
  }

  tab <- tibble::tibble(
    var    = forcats::fct_inorder(skeleton$var),
    levels = forcats::fct_inorder(disp_levels)
  )
  for (i in seq_along(built)) tab[[labels[i]]] <- built[[i]]$col

  tab |>
    new_tab(subtext = subtext) |>
    dplyr::group_by(var)
}


# === Public API =====================================================================

#' Regression table (effect measures) as a tabxplor table
#'
#' Fits one regression model per column and returns a `tabxplor` table of the per-family effect
#' measure -- linear **beta** (gaussian), **odds ratios** (binomial / logistic), **incidence-rate
#' ratios** (poisson), **multinomial odds ratios** (one column per outcome category vs the reference,
#' nominal 3+ level), **cumulative odds ratios** (ordinal / proportional-odds) -- one row per predictor
#' level (the reference level shown as the neutral value `0` or `1`), grouped by predictor. Each cell
#' stores the estimate, its confidence interval and p-value, so the table prints with significance
#' stars, greys out non-significant effects, and exports (kable / Markdown / Excel) like any `tabxplor`
#' crosstab.
#'
#' `predictors` selects the mode: a **character vector** fits one model, and `dependent` may itself
#' be a vector -> one column per dependent; a **named list** of predictor sets fits one model each ->
#' one column per model (predictors absent from a model are left blank), for comparing specifications.
#'
#' Unweighted models use [stats::lm()] / [stats::glm()]; a `wt` weight column switches to a survey
#' design ([survey::svyglm()]), which gives correct design-based standard errors rather than the
#' frequency-inflated ones of `glm(weights=)`. `broom` (always) and `survey` (only with `wt`) are
#' optional dependencies. `tab_logit()` / `multi_logit()` are convenience wrappers for the binomial
#' family.
#'
#' A **nominal** outcome with 3+ unordered levels is fit as one multinomial logit ([nnet::multinom()]),
#' giving **one odds-ratio column per non-reference outcome category** ("`<category>` vs `<reference>`:
#' OR"). An **ordered** outcome with 3+ levels is fit as a proportional-odds cumulative logit
#' ([MASS::polr()]), giving one cumulative-odds-ratio column; the parallel-lines assumption is tested
#' with the Brant test (install the `brant` package) and a warning is issued if it is violated.
#' (Weighted 3+ level models are planned for a later release.)
#'
#' A **summed-score** outcome (a count of "yes" answers out of a fixed number of items) is fit as a
#' grouped binomial when you pass `trials` (the number of items). Power users can pass a **model
#' formula** as `dependent` -- `tab_reg(data, y ~ x1 + poly(x2, 2) + x1:x3)` -- driving the model
#' directly; simple `y ~ a + b` formulas behave exactly like `dependent = "y"`, `predictors = c("a",
#' "b")`, while interactions / `poly()` / `I()` terms render as best-effort term rows.
#'
#' @param data A data frame.
#' @param dependent Character outcome variable name(s), **or a model formula** (the escape hatch).
#'   With a `predictors` character vector, several names give one effect column per outcome; with a
#'   `predictors` list, a single name is required. A formula supplies its own model (leave
#'   `predictors` unset).
#' @param predictors Either a character vector of predictor names (one model), or a **named list**
#'   of character vectors (one model per element, its name labelling the column). Leave `NULL` when
#'   `dependent` is a formula.
#' @param family The model family. `"auto"` (default) detects a binary (-> `"binomial"`), an ordered
#'   3+ level (-> `"ordinal"`), a nominal 3+ level (-> `"multinomial"`), or a continuous
#'   (-> `"gaussian"`) outcome and emits a message; an integer count stays ambiguous and must be named.
#'   Set it explicitly with `"gaussian"` (linear), `"binomial"` (logistic), `"poisson"` /
#'   `"quasipoisson"` (counts), `"multinomial"` (nominal 3+ level), `"ordinal"` (ordered 3+ level).
#' @param wt Optional. Name of a weight column (character). Uses survey-weighted estimation.
#' @param exponentiate Whether to exponentiate coefficients into ratios. `"nongaussian"` (default)
#'   exponentiates every family except gaussian (odds ratios / incidence-rate ratios, leaving linear
#'   betas raw); `TRUE` / `FALSE` force it on / off for all columns.
#' @param trials Grouped-binomial (summed-score) outcomes only. The number of items behind the score,
#'   fitting `cbind(score, trials - score)` as a binomial. `NULL` (default) fits an ordinary binary
#'   logit; a single integer (or a vector named by dependent) sets the item count; `TRUE` uses each
#'   dependent's observed maximum score. Requires `family = "binomial"`.
#' @param conf_level Confidence level for the intervals. Default `0.95`.
#' @param method How the interval and p-value are computed. `"wald"` (default) uses the Wald interval
#'   and the Wald z / t test: fast, matches standard software output, and the only option for weighted
#'   models. `"profile"` uses the profile-likelihood interval ([stats::confint()], needs `MASS`) and
#'   the likelihood-ratio test: more accurate near separation, unweighted binomial/poisson models only
#'   (else it falls back to Wald with a message; gaussian always uses the exact-t interval).
#' @param reference Optional named vector `c(var = "baseline level")` choosing the treatment-contrast
#'   reference level of one or more factor predictors (the effect of every other level is measured
#'   against it). For a **multinomial** outcome, keying the vector by the outcome name (e.g.
#'   `c(partyid = "Independent")`) also sets the baseline outcome category all the OR columns are
#'   compared against. This is how factor contrasts are set; other contrast codings can be applied by
#'   passing a formula in `dependent` with the terms already coded.
#' @param inverse_two_level_factors Logical, binomial only. If `TRUE` (default), models the FIRST
#'   level of a 2-level factor dependent (e.g. `"1-Married"` before `"2-Not married"`).
#' @param color,color_signif How the effect measure is coloured (`NULL` uses the per-family default:
#'   `"OR"` magnitude for ratios, standardized `"diff"` for betas; significance policy
#'   `"grey_non_signif"`). See [tab()].
#' @param cleannames Logical. If `TRUE`, strips numeric prefixes from factor levels for display.
#'   Uses `getOption("tabxplor.cleannames")` when `NULL`.
#' @param subtext Optional character. A note shown below the table.
#'
#' @return A `tabxplor_grouped_tab` (grouped by predictor), one effect column per model / dependent.
#'
#' @examples
#' data <- forcats::gss_cat |>
#'   dplyr::mutate(married = factor(dplyr::if_else(marital == "Married",
#'                                                 "Married", "Not married")))
#' # logistic (odds ratios):
#' tab_reg(data, dependent = "married", predictors = c("race", "rincome"),
#'         family = "binomial")
#' # linear (betas):
#' tab_reg(data, dependent = "tvhours", predictors = c("race", "age"),
#'         family = "gaussian")
#' # formula escape-hatch (same model, terser):
#' tab_reg(data, married ~ race + rincome, family = "binomial")
#' # multinomial (nominal 3+ level): one OR column per outcome category vs the reference
#' tab_reg(forcats::gss_cat, dependent = "partyid", predictors = c("race", "age"),
#'         family = "multinomial", reference = c(partyid = "Independent"))
#' # ordinal (proportional-odds): one cumulative-OR column
#' income3 <- forcats::gss_cat |>
#'   dplyr::mutate(income = factor(rincome, ordered = TRUE))
#' tab_reg(income3, dependent = "income", predictors = "race", family = "ordinal")
#'
#' @export
tab_reg <- function(data, dependent, predictors = NULL,
                    family = "auto", wt = NULL, exponentiate = "nongaussian",
                    trials = NULL, conf_level = 0.95, method = c("wald", "profile"),
                    reference = NULL, inverse_two_level_factors = TRUE,
                    color = NULL, color_signif = NULL,
                    cleannames = NULL, subtext = "") {
  method <- match.arg(method)
  stopifnot(is.data.frame(data))
  cleannames <- if (is.null(cleannames)) getOption("tabxplor.cleannames", TRUE) else cleannames

  # formula escape-hatch (D9): a formula in `dependent` supplies the model. A SIMPLE formula (bare
  # response ~ bare main-effect vars) reduces losslessly to the dependent+predictors character path;
  # a COMPOUND one (interactions / poly() / I() / calls) is fit verbatim with a fit-read skeleton.
  formula_mode <- FALSE
  raw_formula  <- NULL
  if (rlang::is_formula(dependent)) {
    if (!is.null(predictors)) {
      cli::cli_abort("Provide either a formula in {.arg dependent} or {.arg predictors}, not both.")
    }
    parsed <- reg_parse_formula(dependent, data)
    if (!parsed$lhs_is_name && identical(family, "auto")) {
      cli::cli_abort(c("Cannot auto-detect {.arg family} from a transformed formula response.",
                       "i" = "Set {.arg family} explicitly when the response is not a bare variable."))
    }
    dependent <- parsed$dependent
    if (parsed$simple) {
      predictors <- parsed$labels                       # main-effect vars, in formula order
    } else {
      formula_mode <- TRUE
      raw_formula  <- parsed$formula
      predictors   <- parsed$predictors                 # RHS bare vars (reference= / drop_na)
    }
  } else if (is.null(predictors)) {
    cli::cli_abort(c("{.arg predictors} is required.",
                     "i" = "Or pass a model formula as {.arg dependent}, e.g. {.code y ~ x1 + x2}."))
  }
  stopifnot(is.character(dependent), length(dependent) >= 1L)

  # predictors dispatch: named list -> model-comparison ; character vector -> one model per dependent
  is_comparison <- is.list(predictors)
  if (is_comparison && length(dependent) != 1L) {
    cli::cli_abort(c("With a list of models in {.arg predictors}, {.arg dependent} must be a single name.",
                     "i" = "A vector of dependents is for the one-model-per-outcome mode."))
  }
  if (!is_comparison && !is.character(predictors)) {
    cli::cli_abort("{.arg predictors} must be a character vector or a named list of character vectors.")
  }

  if (identical(family, "auto")) family <- reg_detect_family(data, dependent[[1]])
  family <- rlang::arg_match(family, c("gaussian", "binomial", "poisson", "quasipoisson",
                                       "multinomial", "ordinal"))

  # Weighted multinomial / ordinal are deferred (no design-based multinomial engine in survey; svyolr
  # lands with the full survey-design phase). A weighted binary/count/linear model still uses svyglm.
  if (!is.null(wt) && family %in% c("multinomial", "ordinal")) {
    cli::cli_abort(c(
      "Weighted {.val {family}} regression is not yet supported.",
      "i" = "Drop {.arg wt}, or use an unweighted model (survey-weighted 3+ level models are planned)."
    ))
  }

  do_exp       <- isTRUE(exponentiate) ||
    (identical(exponentiate, "nongaussian") && family != "gaussian")
  effect_shape <- if (do_exp) "ratio" else "additive"
  eff_word     <- reg_effect_word(family, do_exp)

  # trials -> grouped binomial (D2): a summed-score outcome fit as cbind(score, trials-score). NULL =
  # off (binary logit). TRUE = observed max per dependent. Numeric / named vector = the item count.
  trials_for <- function(d) NULL
  if (!is.null(trials)) {
    if (family != "binomial") {
      cli::cli_abort("{.arg trials} applies only to the {.val binomial} family (grouped / summed-score).")
    }
    if (formula_mode) {
      cli::cli_warn("{.arg trials} is ignored with a compound formula; write {.code cbind()} in it instead.")
    } else {
      tv <- if (isTRUE(trials))              purrr::map_dbl(dependent, ~ max(data[[.x]], na.rm = TRUE))
            else if (!is.null(names(trials))) unname(trials[dependent])
            else                              rep_len(as.numeric(trials), length(dependent))
      tv <- stats::setNames(as.integer(round(tv)), dependent)
      trials_for <- function(d) tv[[d]]
    }
  }

  # base `%||%` is R >= 4.4 only; the package supports R >= 4.1, so use explicit is.null().
  if (is.null(color))        color        <- if (effect_shape == "ratio") "OR" else "diff"
  if (is.null(color_signif)) color_signif <- "grey_non_signif"

  all_predictors <- if (is_comparison) unique(purrr::flatten_chr(predictors)) else predictors
  if (!is.null(reference)) {
    # A multinomial's baseline is the OUTCOME factor's first level, so `reference` keyed by the
    # dependent relevels it too (unified "reference level of any variable"). An ordinal outcome must
    # keep its order -> never releveled; predictor contrasts are releveled for every family.
    relevelable <- if (family == "multinomial") c(all_predictors, dependent) else all_predictors
    data <- reg_apply_references(data, reference, relevelable)
  }

  if (is_comparison) {
    models <- predictors
    if (is.null(names(models)) || any(names(models) == "")) {
      names(models) <- paste0("model", seq_along(models))
    }
    labels <- make.unique(names(models))
    specs  <- purrr::map2(models, labels,
                          ~ list(dependent = dependent, predictors = .x, label = .y,
                                 trials = trials_for(dependent), compound = FALSE, formula = NULL))
    union_predictors <- all_predictors
  } else {
    labels <- purrr::map_chr(dependent, function(d) {
      # a summed-score / compound-formula binomial has no single "positive level" -> label by name
      base <- if (family == "binomial" && !formula_mode && is.null(trials_for(d))) {
        pl <- reg_positive_level(data, d, inverse_two_level_factors)
        if (cleannames) pl <- stringr::str_remove_all(pl, cleannames_condition())
        pl
      } else d
      paste0(base, ": ", eff_word)
    })
    labels <- make.unique(labels)
    specs  <- purrr::map2(dependent, labels,
                          ~ list(dependent = .x, predictors = predictors, label = .y,
                                 trials = trials_for(.x), compound = formula_mode,
                                 formula = raw_formula))
    union_predictors <- predictors
  }

  note <- reg_model_note(family, do_exp)
  if (!is.null(note)) subtext <- if (nzchar(subtext)) paste0(subtext, " ", note) else note

  reg_check_deps(family, wt)
  reg_build(data, specs, union_predictors, family, wt, do_exp, effect_shape,
            inverse_two_level_factors, conf_level, method, color, color_signif,
            cleannames, subtext, eff_word)
}


#' Logistic-regression table (odds ratios)
#'
#' Convenience wrapper of [tab_reg()] for the binomial family: fits one binary logistic regression
#' per `dependent` on a shared set of `predictors` and returns a `tabxplor` table of odds ratios
#' (one column per dependent, the reference level shown as `1`, grouped by predictor). See [tab_reg()]
#' for the engine, weighting and interval details.
#'
#' @inheritParams tab_reg
#' @param dependent Character vector of binary dependent variable name(s). Each must be a 2-level
#'   factor/character or a 0/1 numeric.
#' @param predictors Character vector of predictor variable name(s).
#' @param color_signif How significance drives the colours. `"grey_non_signif"` (default) colours
#'   only odds ratios whose confidence interval excludes 1 and greys the rest.
#'
#' @return A `tabxplor_grouped_tab` (grouped by predictor), one odds-ratio column per `dependent`.
#'
#' @examples
#' data <- forcats::gss_cat |>
#'   dplyr::mutate(married = factor(dplyr::if_else(marital == "Married",
#'                                                 "Married", "Not married")))
#' tab_logit(data, dependent = "married", predictors = c("race", "rincome"))
#'
#' @export
tab_logit <- function(data, dependent, predictors, wt = NULL,
                      inverse_two_level_factors = TRUE,
                      conf_level = 0.95,
                      method = c("wald", "profile"),
                      color_signif = c("grey_non_signif", "ignore", "color_all_signif"),
                      cleannames = NULL, subtext = "") {
  method       <- match.arg(method)
  color_signif <- match.arg(color_signif)
  stopifnot(is.character(predictors), length(predictors) >= 1L)
  tab_reg(data, dependent = dependent, predictors = predictors, family = "binomial", wt = wt,
          conf_level = conf_level, method = method,
          inverse_two_level_factors = inverse_two_level_factors,
          color_signif = color_signif, cleannames = cleannames, subtext = subtext)
}


#' Compare several logistic-regression models (odds ratios side by side)
#'
#' Convenience wrapper of [tab_reg()] for the binomial family in model-comparison mode: fits several
#' models for ONE binary `dependent`, one per named predictor set in `models`, and returns a
#' `tabxplor` table with one odds-ratio column per model (predictors absent from a model left blank).
#'
#' @inheritParams tab_logit
#' @param dependent Character. Name of the single binary dependent variable.
#' @param models A named list of character vectors; each element is one model's predictor set and its
#'   name labels the column. Unnamed elements are labelled `model1`, `model2`, ...
#'
#' @return A `tabxplor_grouped_tab` (grouped by predictor), one odds-ratio column per model.
#'
#' @examples
#' data <- forcats::gss_cat |>
#'   dplyr::mutate(married = factor(dplyr::if_else(marital == "Married",
#'                                                 "Married", "Not married")))
#' multi_logit(
#'   data, dependent = "married",
#'   models = list(demographic = c("race", "age"),
#'                 full        = c("race", "age", "rincome"))
#' )
#'
#' @export
multi_logit <- function(data, dependent, models, wt = NULL,
                        inverse_two_level_factors = TRUE,
                        conf_level = 0.95,
                        method = c("wald", "profile"),
                        color_signif = c("grey_non_signif", "ignore", "color_all_signif"),
                        cleannames = NULL, subtext = "") {
  method       <- match.arg(method)
  color_signif <- match.arg(color_signif)
  stopifnot(is.character(dependent), length(dependent) == 1L, is.list(models), length(models) >= 1L)
  tab_reg(data, dependent = dependent, predictors = models, family = "binomial", wt = wt,
          conf_level = conf_level, method = method,
          inverse_two_level_factors = inverse_two_level_factors,
          color_signif = color_signif, cleannames = cleannames, subtext = subtext)
}
