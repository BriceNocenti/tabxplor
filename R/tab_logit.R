# PURPOSE: Logistic-regression tables (odds ratios) as native tabxplor_tab objects.
# ROLE: tab_logit()/multi_logit() fit binary logit models and render odds ratios + Wald CIs +
#   p-values through the tabxplor_fmt `or`/`ci_inf`/`ci_sup`/`pvalue` fields, so a regression table
#   prints, colours and exports (kable / md / Excel) exactly like a crosstab.
# KEY CONSTRAINTS:
#   - Direct engine: stats::glm (unweighted) / survey::svyglm (weighted) + broom::tidy. No parsnip.
#   - broom + survey (+ MASS for method="profile") are Suggests -> requireNamespace()-guarded.
#   - CI <-> p are always DUALS (CI <-> stars can never disagree). method="wald" (default): the
#     Wald interval exp(coef +/- crit*se) computed in-house + the Wald p (NOT broom's conf.int,
#     which silently switches to profile likelihood when MASS is loaded). method="profile":
#     profile-likelihood CI (stats::confint) + LR-test p (both likelihood-based). Wald is the only
#     option for weighted models (profile is undefined for survey designs).
#   - OR columns are ordinary fmt: type="row", display="or", color="OR",
#     color_signif="grey_non_signif", ci_type="or" (log-OR Wald exp() bounds, multiplicative
#     neutral 1). ci_center()/fmt_color_plan()/format() read those (Phase 12a fmt patches).
# See: CLAUDE.md Phase 12a ; dev/tabxplor_1.4.0_decisions.md.

# === Internal engine ================================================================

# broom is needed for every fit; survey only for the weighted (wt) path.
logit_check_deps <- function(wt) {
  if (!requireNamespace("broom", quietly = TRUE)) {
    cli::cli_abort(c(
      "{.pkg broom} is required for logistic-regression tables.",
      "i" = 'Install it with {.code install.packages("broom")}.'
    ))
  }
  if (!is.null(wt) && !requireNamespace("survey", quietly = TRUE)) {
    cli::cli_abort(c(
      "{.pkg survey} is required for weighted logistic regression (the {.arg wt} argument).",
      "i" = 'Install it with {.code install.packages("survey")}.'
    ))
  }
}

# Prepare a binary dependent: a 0/1 numeric becomes a 2-level factor ("Not <dep>" / "<dep>");
# any other input must have exactly 2 levels, optionally reversed so glm models the FIRST level
# (inverse_two_level_factors -- the maintainer's convention, e.g. "1-Married" first = modelled).
logit_prep_dependent <- function(data, dependent, inverse_two_level_factors) {
  y <- data[[dependent]]
  if (is.numeric(y) && all(stats::na.omit(y) %in% c(0, 1))) {
    y <- factor(y, levels = c(0, 1), labels = c(paste0("Not ", dependent), dependent))
  } else {
    y <- forcats::fct_drop(as.factor(y))
    if (nlevels(y) != 2L) {
      cli::cli_abort(c(
        "The dependent variable {.val {dependent}} must be binary (2 levels).",
        "x" = "It has {nlevels(y)} level{?s}: {.val {levels(y)}}.",
        "i" = "Multinomial / 3+ level outcomes are planned for a later phase."
      ))
    }
    if (inverse_two_level_factors) y <- forcats::fct_rev(y)
  }
  data[[dependent]] <- y
  data
}

# The modelled ("positive") level whose odds the OR describes = the level glm predicts.
logit_positive_level <- function(data, dependent, inverse_two_level_factors) {
  y <- data[[dependent]]
  if (is.numeric(y) && all(stats::na.omit(y) %in% c(0, 1))) return(dependent)
  lv <- levels(forcats::fct_drop(as.factor(y)))
  lv[if (inverse_two_level_factors) 1L else 2L]
}

# The (var, level, term, is_ref) row skeleton for a set of predictors, in display order: the
# intercept ("Constant") first, then each predictor's levels (factor / character) -- first level =
# reference, no glm term -- or a single row for a numeric predictor. `term` matches glm/svyglm
# coefficient names so a fit aligns to the skeleton by term.
logit_skeleton <- function(data, predictors) {
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

# Per-coefficient LIKELIHOOD-RATIO p-values (the dual of the profile-likelihood CI). Each coefficient
# is dropped from the model matrix in turn and the deviance change is a 1-df chi-square. Works on a
# fitted (unweighted) glm; for a factor this tests one level vs the reference, matching the per-level
# odds ratio the table shows.
logit_lr_pvalues <- function(fit) {
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

# Fit one binary logit on complete cases -> a tidy of ODDS RATIOS + CI + p + the model n.
# method = "wald" (default): Wald interval exp(coef +/- crit*se) + Wald p (glm -> normal z quantile;
#   svyglm -> t with design residual df) -- each the exact dual of broom's p for that engine.
# method = "profile": profile-likelihood CI (stats::confint via MASS) + LR-test p -- both
#   likelihood-based, so still exact duals. Unweighted glm only; a weighted model falls back to Wald
#   (profile likelihood is not defined for survey designs) with a one-time message.
logit_fit <- function(data, dependent, predictors, wt,
                      inverse_two_level_factors, conf_level, method) {
  mdata <- tidyr::drop_na(data, tidyselect::all_of(c(dependent, predictors, wt)))

  fac_preds <- predictors[purrr::map_lgl(
    predictors, ~ is.factor(mdata[[.]]) || is.character(mdata[[.]])
  )]
  if (length(fac_preds) > 0L) {
    mdata <- dplyr::mutate(mdata, dplyr::across(
      tidyselect::all_of(fac_preds), ~ forcats::fct_drop(as.factor(.))
    ))
  }
  mdata <- logit_prep_dependent(mdata, dependent, inverse_two_level_factors)

  fml <- stats::as.formula(paste0(
    "`", dependent, "` ~ ", paste0("`", predictors, "`", collapse = " + ")
  ))

  weighted <- !is.null(wt)
  fit <- if (!weighted) {
    stats::glm(fml, data = mdata, family = stats::binomial("logit"))
  } else {
    design <- survey::svydesign(ids = ~1,
                                weights = stats::as.formula(paste0("~`", wt, "`")),
                                data = mdata)
    survey::svyglm(fml, design = design, family = stats::quasibinomial("logit"))
  }

  td <- broom::tidy(fit)                            # log scale: estimate, std.error, p.value
  td$term <- stringr::str_remove_all(td$term, "`")  # strip formula backticks -> match skeleton

  use_profile <- method == "profile" && !weighted
  if (method == "profile" && weighted) {
    cli::cli_inform(c("!" = paste0("Profile-likelihood intervals are not defined for survey-weighted ",
                                   "models; using Wald for {.arg wt} models.")))
  }

  if (use_profile) {
    if (!requireNamespace("MASS", quietly = TRUE)) {
      cli::cli_abort(c('{.pkg MASS} is required for {.code method = "profile"}.',
                       "i" = '- Install it, or use {.code method = "wald"} (the default).'))
    }
    ci  <- suppressMessages(exp(stats::confint(fit, level = conf_level)))
    idx <- match(td$term, stringr::str_remove_all(rownames(ci), "`"))
    td$conf.low  <- unname(ci[idx, 1])
    td$conf.high <- unname(ci[idx, 2])
    lrp <- logit_lr_pvalues(fit)
    td$p.value   <- unname(lrp[match(td$term, names(lrp))])
    td$estimate  <- exp(td$estimate)
  } else {
    crit <- if (weighted) stats::qt(1 - (1 - conf_level) / 2, df = stats::df.residual(fit))
            else          stats::qnorm(1 - (1 - conf_level) / 2)
    td$conf.low  <- exp(td$estimate - crit * td$std.error)
    td$conf.high <- exp(td$estimate + crit * td$std.error)
    td$estimate  <- exp(td$estimate)
  }

  list(tidy = td, nobs = nrow(mdata))
}

# Align one fit to the union skeleton -> a single OR fmt column (length = nrow(skeleton)).
# Reference LEVELS of predictors present in this model get OR = 1 (no CI/p); predictors ABSENT
# from this model stay NA (empty cells); the Constant carries the intercept (baseline) odds.
logit_column <- function(skeleton, tidy, nobs, model_predictors, col_var, color_signif) {
  m   <- match(skeleton$term, tidy$term)
  est <- tidy$estimate[m]
  lo  <- tidy$conf.low[m]
  hi  <- tidy$conf.high[m]
  p   <- tidy$p.value[m]

  in_model <- skeleton$var %in% c("Constant", model_predictors)
  ref_lvl  <- skeleton$is_ref & skeleton$var != "Constant" & in_model
  est[ref_lvl] <- 1
  lo[ref_lvl]  <- NA_real_
  hi[ref_lvl]  <- NA_real_
  p[ref_lvl]   <- NA_real_

  fmt(
    n            = rep(as.integer(nobs), nrow(skeleton)),
    or           = est,
    ci_inf       = lo,
    ci_sup       = hi,
    pvalue       = p,
    type         = "row",
    display      = "or",
    digits       = 2L,
    ref          = "1",
    ci_type      = "or",
    color        = "OR",
    color_signif = color_signif,
    col_var      = col_var,
    comp_all     = FALSE,
    in_refrow    = ref_lvl | skeleton$var == "Constant"
  )
}

# The shared builder: fit every column spec, align to one union skeleton, assemble a grouped_tab.
# specs = list of list(dependent, predictors, label). union_predictors = the ordered skeleton set.
logit_build <- function(data, specs, union_predictors, wt,
                        inverse_two_level_factors, conf_level, method, color_signif,
                        cleannames, subtext) {
  skeleton <- logit_skeleton(data, union_predictors)

  cols <- purrr::map(specs, function(sp) {
    f <- logit_fit(data, sp$dependent, sp$predictors, wt,
                   inverse_two_level_factors, conf_level, method)
    logit_column(skeleton, f$tidy, f$nobs, sp$predictors, sp$label, color_signif)
  })

  disp_levels <- skeleton$level
  if (cleannames) {
    disp_levels <- stringr::str_remove_all(disp_levels, cleannames_condition())
  }

  tab <- tibble::tibble(
    var    = forcats::fct_inorder(skeleton$var),
    levels = forcats::fct_inorder(disp_levels)
  )
  for (i in seq_along(cols)) tab[[specs[[i]]$label]] <- cols[[i]]

  tab |>
    new_tab(subtext = subtext) |>
    dplyr::group_by(var)
}


# === Public API =====================================================================

#' Logistic-regression table (odds ratios)
#'
#' Fits one binary logistic regression per `dependent` variable on a shared set of `predictors`
#' and returns a `tabxplor` table of odds ratios: one column per dependent, one row per predictor
#' level (the reference level shown as `1`), grouped by predictor. Each cell stores the odds ratio,
#' its log-OR Wald 95% confidence interval and p-value, so the table prints with significance stars,
#' greys out non-significant odds ratios, and exports (kable / Markdown / Excel) like any
#' `tabxplor` crosstab.
#'
#' Unweighted models use [stats::glm()]; a `wt` weight column switches to a survey design
#' ([survey::svyglm()] on [survey::svydesign()]), which gives correct design-based standard errors
#' rather than the frequency-inflated ones of `glm(weights=)`. `broom` (always) and `survey` (only
#' when `wt` is used) are optional dependencies.
#'
#' @param data A data frame.
#' @param dependent Character vector of binary dependent variable name(s). Each must be a 2-level
#'   factor/character or a 0/1 numeric.
#' @param predictors Character vector of predictor variable name(s).
#' @param wt Optional. Name of a weight column (character). Uses survey-weighted estimation.
#' @param inverse_two_level_factors Logical. If `TRUE` (default), models the FIRST level of a
#'   2-level factor dependent (e.g. `"1-Married"` before `"2-Not married"`).
#' @param conf_level Confidence level for the odds-ratio intervals. Default `0.95`.
#' @param method How the interval and p-value are computed. `"wald"` (default) uses the Wald
#'   interval `exp(coef +/- z * se)` and the Wald z / t test: fast, matches standard software output
#'   (R `summary()`, Stata, SPSS), and the only option for weighted models. `"profile"` uses the
#'   profile-likelihood interval (`stats::confint()`, needs the `MASS` package) and the
#'   likelihood-ratio test: more accurate for small samples or near-separation, but unweighted
#'   models only (a weighted model falls back to Wald with a message).
#' @param color_signif How significance drives the colours (odds ratios only). `"grey_non_signif"`
#'   (default) colours only odds ratios whose confidence interval excludes 1 and greys the rest;
#'   `"ignore"` colours every odds ratio by magnitude; `"color_all_signif"` colours the significant
#'   ones by their conservative interval bound.
#' @param cleannames Logical. If `TRUE`, strips numeric prefixes from factor levels for display
#'   (e.g. `"1-Married"` -> `"Married"`). Uses `getOption("tabxplor.cleannames")` when `NULL`.
#' @param subtext Optional character. A note shown below the table.
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
  logit_check_deps(wt)
  method       <- match.arg(method)
  color_signif <- match.arg(color_signif)
  stopifnot(is.data.frame(data), is.character(dependent), is.character(predictors),
            length(predictors) >= 1L)
  cleannames <- if (is.null(cleannames)) getOption("tabxplor.cleannames", TRUE) else cleannames

  labels <- purrr::map_chr(dependent, function(d) {
    pl <- logit_positive_level(data, d, inverse_two_level_factors)
    if (cleannames) pl <- stringr::str_remove_all(pl, cleannames_condition())
    paste0(pl, ": OR")
  })
  labels <- make.unique(labels)

  specs <- purrr::map2(dependent, labels,
                       ~ list(dependent = .x, predictors = predictors, label = .y))

  logit_build(data, specs, union_predictors = predictors, wt = wt,
              inverse_two_level_factors = inverse_two_level_factors,
              conf_level = conf_level, method = method, color_signif = color_signif,
              cleannames = cleannames, subtext = subtext)
}


#' Compare several logistic-regression models (odds ratios side by side)
#'
#' Fits several nested or competing models for ONE binary `dependent`, one per named predictor set
#' in `models`, and returns a `tabxplor` table with one odds-ratio column per model. Predictors
#' absent from a given model are left blank in that column, so the sensitivity of each odds ratio
#' to the model specification is read across a row. Same engine, fields and display as [tab_logit()].
#'
#' @inheritParams tab_logit
#' @param dependent Character. Name of the single binary dependent variable.
#' @param models A named list of character vectors; each element is one model's predictor set and
#'   its name labels the column. Unnamed elements are labelled `model1`, `model2`, ...
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
  logit_check_deps(wt)
  method       <- match.arg(method)
  color_signif <- match.arg(color_signif)
  stopifnot(is.data.frame(data), is.character(dependent), length(dependent) == 1L,
            is.list(models), length(models) >= 1L)
  if (is.null(names(models)) || any(names(models) == "")) {
    names(models) <- paste0("model", seq_along(models))
  }
  cleannames <- if (is.null(cleannames)) getOption("tabxplor.cleannames", TRUE) else cleannames

  labels <- make.unique(names(models))
  specs  <- purrr::map2(models, labels,
                        ~ list(dependent = dependent, predictors = .x, label = .y))
  union_predictors <- unique(purrr::flatten_chr(models))

  logit_build(data, specs, union_predictors = union_predictors, wt = wt,
              inverse_two_level_factors = inverse_two_level_factors,
              conf_level = conf_level, method = method, color_signif = color_signif,
              cleannames = cleannames, subtext = subtext)
}
