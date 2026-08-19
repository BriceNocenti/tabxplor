# PURPOSE: tab_reg() -- one regression model per column, rendered as a native tabxplor_tab.
# ROLE: the package's second producer. It fits the models, turns each estimand into fmt cells and
#   returns the object tab() returns, so the colour engine, the accessors, the reshape operations
#   and every exporter treat a regression table and a crosstab identically. This file owns the
#   fitting, the column builders, the marginal-effects path, the footer and the staged build; the
#   estimand vocabulary is R/reg-estimand.R's, the argument boundary R/reg-resolve.R's, the crude
#   companion R/reg-empirical.R's, and the per-model product R/reg-spec-build.R's.
# KEY CONSTRAINTS:
#   - ONE ENGINE PER SHAPE, all tidied through broom: stats::lm / stats::glm unweighted,
#     survey::svyglm as soon as there are weights or a design, nnet::multinom for a nominal 3+ level
#     outcome, MASS::polr for an ordered one. survey / MASS / nnet / brant / marginaleffects / car
#     are Suggests, and every entry point guards them.
#   - CI AND p ARE DUALS, so an interval and its stars can never disagree. `ci_method = "wald"`
#     (default) builds the interval as estimate +/- crit * se and recomputes p from those same two
#     numbers; the crit refers to z where the family FIXES the dispersion (unweighted binomial,
#     poisson) and to t(df.residual) where it is ESTIMATED (lm, quasi*, weighted svyglm), which is
#     what makes it match broom's own z / t p exactly. `"profile"` pairs confint() with the
#     likelihood-ratio p, its dual.
#   - THE ESTIMAND'S DECLARED SCALE decides a column's whole shape: which fmt field the estimate
#     lands in (multiplicative -> `or`, neutral 1; additive -> `diff`, neutral 0, with `var` = var(Y)
#     where the scale asks for it) and which ladder it is graded on. No builder names a
#     family-specific field, and every column is built displaying the scale-relative `est` token.
#   - EVERY MODEL COLUMN CARRIES ITS ADJUSTED PREDICTION and its additive marginal effect, printed or
#     not (reg_fill_base). That is what makes `display` a purely post-hoc property: choosing a layout
#     triggers no computation and changes no number, so set_display() on a built table gives exactly
#     what asking for that layout at build time would have given.
#   - `trials` fits a summed score as a GROUPED binomial (cbind(score, trials - score)). A model
#     FORMULA in `outcome` is the escape hatch: a plain `y ~ a + b` reduces to the outcome+predictors
#     path, while interactions / poly() / I() are fit verbatim and rendered from the fitted terms.
#   - A 3+ LEVEL OUTCOME becomes several COLUMNS, not several tables: one multinomial fit gives one
#     odds-ratio column per non-reference category, one proportional-odds fit one cumulative-OR
#     column (its cut-point rows are dropped, so the Constant cell is empty). Both reuse the ordinary
#     column shape and share reg_wald_from_tidy(), so the duality above holds there too.
# See: CLAUDE.md section "tabxplor architecture" (the regression subsystem).

# === SECTION: Internal engine ===================================================================

# DESIGN: marginaleffects is needed ONLY where an estimand's engine names it (`at_reference`). Every
# other marginal quantity runs on the dependency-free gcomp engine, which is what lets every model
# column populate them unconditionally.

#' @keywords internal
#' @noRd
reg_abort_marginaleffects <- function(what) {
  cli::cli_abort(c(
    "{.pkg marginaleffects} is required for {.code {what}}.",
    "i" = 'Install it with {.code install.packages("marginaleffects")}, or use
           {.code effect = "coefficient"} / {.code effect = "marginal"}, which need no extra package.'
  ))
}

reg_check_deps <- function(family, weighted, needs_marginaleffects = FALSE) {
  if (needs_marginaleffects && !requireNamespace("marginaleffects", quietly = TRUE))
    reg_abort_marginaleffects('effect = "at_reference"')
  # nnet / MASS need no guard: they are Imports. ⚠ VGAM is called directly on the weighted
  # multinomial path, so it is guarded explicitly -- an implicit guard is invisible to R CMD check.
  if (isTRUE(weighted) && family == "multinomial") {
    missing_pkgs <- c("svyVGAM", "VGAM")[
      !vapply(c("svyVGAM", "VGAM"), requireNamespace, logical(1), quietly = TRUE)
    ]
    if (length(missing_pkgs) > 0) {
      cli::cli_abort(c(
        "{.pkg {missing_pkgs}} {?is/are} required for survey-weighted multinomial (nominal 3+ level) models.",
        "i" = 'Install {?it/them} with {.code install.packages("svyVGAM")}, or drop the weights / design.'
      ))
    }
  }
}

reg_parse_formula <- function(formula, data) {
  lhs <- rlang::f_lhs(formula)
  if (is.null(lhs)) {
    cli::cli_abort("A regression {.arg formula} needs a response, e.g. {.code y ~ x1 + x2}.")
  }
  lhs_is_name <- rlang::is_symbol(lhs)
  outcome   <- if (lhs_is_name) rlang::as_string(lhs) else all.vars(lhs)[1]

  tt     <- stats::terms(formula, data = data)
  labels <- attr(tt, "term.labels")
  orders <- attr(tt, "order")
  rhs_vars <- all.vars(rlang::f_rhs(formula))

  simple <- lhs_is_name &&
    outcome %in% names(data) &&
    length(labels) > 0L &&
    all(orders == 1L) &&
    all(labels %in% names(data))

  list(outcome = outcome, predictors = rhs_vars, labels = labels,
       formula = formula, lhs_is_name = lhs_is_name, simple = simple)
}

# DESIGN: the family PREDICATES -- every "which families behave like X" question asked ONCE here.
reg_is_binary_outcome <- function(y) length(unique(stats::na.omit(y))) == 2L
reg_fam_binary   <- function(f)
  f %in% c("binomial", names(REG_FIT_FAMILY)[REG_FIT_FAMILY == "binomial"])
reg_fam_prob     <- function(f) f %in% c("binomial", "multinomial", "ordinal")
reg_fam_percategory <- function(f) reg_fam_prob(f) & !f %in% "binomial"
reg_fam_count    <- function(f) f %in% c("poisson", "quasipoisson")
# ⚠ the question is about the OUTCOME family, never the fit key: `rr` / `rd` are binomial FITS under
# another link, so a `family == "binomial"` test would drop `trials` on both. A compound formula owns
# its LHS, so `trials` does not apply to it.
reg_is_grouped_binomial <- function(family, trials, compound = FALSE)
  reg_fam_binary(family) && !is.null(trials) && !isTRUE(compound)
# fitted by stats::glm -- not one of the 3+ level machines, which have no glm-shaped coefficient
# table, no anova() and no AIC path.
reg_fam_glm <- function(f) f %in% c("gaussian", "binomial", "poisson", "quasipoisson",
                                    "rr", "rd", "mr")
# over-dispersible, so the nominal variance cannot be trusted: poisson, and a grouped binomial for
# the same reason.
reg_fam_overdispersed <- function(f, grouped = FALSE) f == "poisson" || isTRUE(grouped)
# the dispersion is FIXED BY THE FAMILY (1), so the Wald critical value refers to z, not t.
reg_fam_disp_known <- function(f) f %in% c("binomial", "poisson")
# the dispersion is ESTIMATED from the residuals, so a term test refers to F rather than chi2.
reg_fam_disp_estimated <- function(f) f %in% c("gaussian", "quasipoisson")
# fitted by survey::svyglm -- ONE fact with two consequences: it picks the fitter, and an svyglm has
# no ordinary likelihood, so there is no LR test to run. "rr" / "rd" / "mr" go through it even
# unweighted: all three are deliberately misspecified likelihoods chosen to reach a MEASURE, so their
# honest variance is the Huber-White sandwich that svyglm's design-based variance IS.
reg_fam_svy_fitted <- function(f, weighted = FALSE)
  isTRUE(weighted) || f %in% REG_FIT_ONLY_FAMILIES
# Is the DISPLAYED estimand COLLAPSIBLE -- does a zero model-vs-observed gap mean "no confounding"?
# Everything tabxplor shows is, EXCEPT a CONDITIONAL ODDS RATIO (a probability-scale model's
# coefficient; exponentiating is irrelevant, a raw logit coefficient is the same estimand logged).
# ⚠ ask it of each resolved estimand's FIT, never of the outcome family.
reg_estimand_collapsible <- function(family, effect)
  !(identical(effect, "coefficient") && reg_fam_prob(family))

# THE producer of "the colour you asked for cannot be computed here": one entry per reason, never a
# block at a call site, and run BEFORE the build, which would otherwise repeat itself per group.
#' @keywords internal
reg_color_notes <- function(color, color_signif, ests, tab_vars, na, na_explicit,
                            empirical = FALSE, crude_keys = NULL, trials = NULL) {
  notes <- character(0)
  at    <- if (any(vapply(ests, function(e) identical(e$effect, "at_reference"), logical(1))))
             "reference" else "average"
  effect <- ests[[1]]$effect
  # Interpolated HERE, where the locals the messages name are in scope: the caller only emits them.
  add   <- function(...) notes <<- c(notes, cli::format_inline(paste0(...)))
  gap   <- intersect(c("adjustment", "between_groups"), color)
  if (length(gap) == 0L && !emp_on(empirical)) return(notes)

  if ("between_groups" %in% gap && is.null(tab_vars)) {
    add("{.code color = \"between_groups\"} compares each effect to the first group's, so it needs ",
        "{.arg tab_vars} to say what the groups are. Without it nothing is coloured.")
  }
  if (identical(at, "reference")) {
    add("{.code at = \"reference\"} evaluates the model at the reference profile, while the ",
        "observed columns stay marginal over the whole sample: the two are shown side by side, ",
        "but not compared cell by cell ({.code color = \"adjustment\"} and {.code \"{{obs}}\"} ",
        "stay empty).")
  }
  if ("adjustment" %in% gap) {
    # Asked exactly as reg_same_estimand() asks it at build time, so the note and the gate cannot
    # disagree. ⚠ do NOT approximate it by "does the marginal row reuse the coefficient row's crude
    # shape": sharing that shape is the NORMAL case wherever the two contrasts are one estimand.
    if (!identical(effect, "coefficient")) {
      bare <- unique(vapply(names(ests), function(d) {
        e  <- ests[[d]]
        tr <- if (is.null(trials)) NA else trials[[d]] %||% NA
        sh <- reg_crude_shape(if (is.null(crude_keys)) NA_character_ else crude_keys[[d]], e)
        if (is.null(sh) || !identical(sh$scale, reg_scale_of(e, tr))) e$family else NA_character_
      }, character(1)))
      bare <- stats::na.omit(bare)
      if (length(bare)) {
        add("{.code effect = {.val {effect}}} has no observed counterpart on the same scale for ",
            "{.val {bare}}, so {.code color = \"adjustment\"} stays empty there. Use ",
            "{.code effect = \"coefficient\"} to compare them.")
      }
    }
    if (!is.null(color_signif) && !identical(color_signif, "ignore") &&
        !any(vapply(ests, function(e) reg_estimand_collapsible(e$fit, e$effect), logical(1)))) {
      add("{.arg color_signif} does not apply to an odds-ratio {.val adjustment} gap: part of it is ",
          "non-collapsibility, not confounding. Use {.code effect = \"marginal\"} or ",
          "{.code measure = \"ratio\"} (risk ratios), for a gap the test can read.")
    }
    if (na_explicit && identical(na, "drop_by_model")) {
      add("{.code na = \"drop_by_model\"} lets each model use its own complete cases, so a model ",
          "fitted on rows the observed columns do not cover gets no observed effect at all (no ",
          "colour, no test): their distance would be listwise deletion, not adjustment.")
    }
  }
  notes
}

# `crude_key` -- THE stored fact "which observed counterpart does this model have?": a REG_EMPIRICAL
# key, or NA (a compound formula has no predictor structure to be crude about). Computed ONCE at spec
# construction, where family, trials and the compound flag are all in scope.
reg_crude_key <- function(family, trials = NULL, compound = FALSE) {
  if (isTRUE(compound))                                 return(NA_character_)
  if (identical(family, "quasipoisson"))                return("poisson")
  # ⚠ the grouped test comes FIRST and must stay there: `rd` and `rr` are binomial FITS, so a
  # summed-score outcome under either is a grouped binomial -- its crude base is the mean SCORE.
  if (reg_is_grouped_binomial(family, trials, compound)) return("grouped_binomial")
  if (identical(family, "rd"))                          return("binomial")
  if (is.null(REG_EMPIRICAL[[family]]))                 return(NA_character_)
  family
}

# `trials = TRUE` means "the observed maximum"; NA where there is none (a factor outcome is an
# ordinary logit, a 0/1 numeric has no trial count).
#' @keywords internal
#' @noRd
reg_trials_observed_max <- function(x) {
  if (!is.numeric(x) || is.factor(x)) return(NA_real_)
  m <- suppressWarnings(max(x, na.rm = TRUE))
  if (is.finite(m) && m > 1) m else NA_real_
}

# Is a PREDICTOR a factor (contrasts against a reference) or a numeric (one slope per unit)?
# ⚠ a LOGICAL must take the FACTOR arm -- glm names its coefficient `<var>TRUE`; Date / POSIXct stay
# numeric. The answer is STORED in reg_meta$predictor_types: `cleannames` and the multiplier relabel
# both break the `level == var` convention.
#' @keywords internal
reg_is_factor_var <- function(x) is.factor(x) || is.character(x) || is.logical(x)

#' @keywords internal
reg_predictor_types <- function(data, predictors) {
  if (length(predictors) == 0L) return(stats::setNames(character(0), character(0)))
  stats::setNames(
    vapply(predictors, function(p) if (reg_is_factor_var(data[[p]])) "factor" else "numeric",
           character(1)),
    predictors)
}

#' @keywords internal
reg_factor_preds <- function(data, predictors)
  predictors[purrr::map_lgl(predictors, ~ reg_is_factor_var(data[[.x]]))]

#' @keywords internal
reg_numeric_preds <- function(data, predictors)
  predictors[!purrr::map_lgl(predictors, ~ reg_is_factor_var(data[[.x]]))]


# === SECTION: `multiplier` -- the per-unit scaling of a continuous predictor's effect ============
#
# GRAMMAR: a SCALAR ("sd", "2sd" or a number) applies to every numeric predictor; a NAMED vector
# overrides per variable, anything it does not name keeping the scalar default.
#
# The SD is measured on the complete cases of the PREDICTORS + design variables -- not of the
# outcome -- and resolved ONCE, before `shared` is built, so the split recursion, the compared
# models, the crude companions and the jamovi cache key all see the SAME numbers; a per-group SD
# would make `color = "between_groups"` compare different quantities. ⚠ never passed downstream as
# a KEYWORD: marginaleffects reads "sd" as a CENTRED contrast on the SD of its own `newdata`.

#' @keywords internal
reg_weighted_mean <- function(x, w = NULL) {
  x <- as.numeric(x)
  ok <- is.finite(x)
  if (!is.null(w)) { w <- as.numeric(w); ok <- ok & is.finite(w) & w > 0 }
  if (!any(ok)) return(NA_real_)
  if (is.null(w)) mean(x[ok]) else sum(w[ok] * x[ok]) / sum(w[ok])
}

#' @keywords internal
reg_predictor_sd <- function(x, w = NULL) {
  x <- as.numeric(x)
  ok <- is.finite(x)
  if (!is.null(w)) { w <- as.numeric(w); ok <- ok & is.finite(w) & w > 0 }
  if (sum(ok) < 2L) return(NA_real_)
  if (is.null(w)) return(stats::sd(x[ok]))
  xw <- x[ok]; ww <- w[ok]
  m  <- sum(ww * xw) / sum(ww)
  sqrt(sum(ww * (xw - m)^2) / sum(ww))          # the ML weighted variance, as tab()'s numeric side uses
}


#' @keywords internal
REG_MULTIPLIER_KEYWORDS <- c("sd", "1sd", "2sd")

#' @keywords internal
reg_multiplier_value <- function(value, sd, digits = 3L) {
  v <- if (is.character(value)) trimws(tolower(value)) else value
  if (length(v) != 1L || is.na(v)) return(list(k = NA_real_, label = NA_character_))
  if (is.character(v) && v %in% REG_MULTIPLIER_KEYWORDS) {
    if (!is.finite(sd) || sd <= 0) return(list(k = NA_real_, label = NA_character_))
    mult <- if (identical(v, "2sd")) 2 else 1
    lab  <- if (mult == 1) "1 SD" else "2 SD"
    return(list(k = mult * sd, label = paste0(lab, " (", format(signif(mult * sd, digits)), ")")))
  }
  k <- suppressWarnings(as.numeric(v))
  if (!is.finite(k)) return(list(k = NA_real_, label = NA_character_))
  list(k = k, label = if (k == 1) NA_character_ else paste0(format(k), " units"))
}

#' @keywords internal
reg_resolve_multiplier <- function(multiplier, default, data, num_preds, wt = NULL) {
  if (length(num_preds) == 0L) return(list(k = NULL, label = NULL))
  named  <- if (!is.null(multiplier) && !is.null(names(multiplier))) multiplier else NULL
  scalar <- if (is.null(multiplier)) default
            else if (is.null(names(multiplier))) multiplier[[1]]
            else default
  w   <- if (!is.null(wt) && is.character(wt) && length(wt) == 1L && wt %in% names(data))
           data[[wt]] else NULL
  sds <- vapply(num_preds, function(v) reg_predictor_sd(data[[v]], w), numeric(1))
  res <- purrr::map(stats::setNames(num_preds, num_preds), function(v) {
    val <- if (!is.null(named) && v %in% names(named)) named[[v]] else scalar
    reg_multiplier_value(val, sds[[v]])
  })
  k   <- vapply(res, function(z) z$k,     numeric(1))
  lab <- vapply(res, function(z) z$label, character(1))
  keep <- is.finite(k) & k != 1
  if (!any(keep)) return(list(k = NULL, label = NULL))
  list(k = k[keep], label = lab[keep])
}

# === SECTION: Naming -- the outcome kind, the family, the column headers and labels =============

# REG_OUTCOME_KINDS -- THE outcome-kind table, read here and GENERATED into the jamovi JavaScript, so
# the two sides cannot drift. One row per kind of outcome column, keyed by what BOTH sides compute
# from a column alone (has it levels, and how many):
#   <name> : the kind key.
#   detect : the family auto-detected for it.
#   offers : the families offered beside it, ordered, first = the detected default. A 2-level outcome
#            offers poisson because that is the opt-in modified-Poisson (risk-ratio) route, not a
#            count model.
#   said   : how reg_detect_family() names the kind (a bare string: not translated today).
#' @keywords internal
REG_OUTCOME_KINDS <- list(
  binary   = list(detect = "binomial",    offers = c("binomial", "poisson"),
                  said = "binary outcome detected"),
  ordered  = list(detect = "ordinal",     offers = c("ordinal", "multinomial"),
                  said = "ordered outcome detected"),
  nominal  = list(detect = "multinomial", offers = c("multinomial", "ordinal"),
                  said = "nominal outcome detected"),
  numeric  = list(detect = "gaussian",    offers = c("gaussian", "binomial", "poisson"),
                  said = "continuous outcome detected")
)

#' @keywords internal
reg_outcome_kind <- function(y) {
  u <- unique(stats::na.omit(y))
  if (reg_is_binary_outcome(y))                                return("binary")
  if (is.ordered(y) && length(u) >= 3L)                        return("ordered")
  if ((is.factor(y) || is.character(y)) && length(u) >= 3L)    return("nominal")
  if (is.numeric(y))                                           return("numeric")
  ""
}

reg_detect_family <- function(data, outcome) {
  y    <- data[[outcome]]
  kind <- reg_outcome_kind(y)
  if (!nzchar(kind)) {
    cli::cli_abort(c(
      "Cannot auto-detect the model family for {.val {outcome}}.",
      "i" = paste0("Set {.arg family} explicitly: {.val gaussian} (linear), {.val poisson} (counts), ",
                   "{.val binomial} (logistic), {.val multinomial} / {.val ordinal} (3+ level).")
    ))
  }
  fam  <- REG_OUTCOME_KINDS[[kind]]$detect
  said <- REG_OUTCOME_KINDS[[kind]]$said
  cli::cli_inform(c("i" = paste0(
    "{.val {outcome}}: ", said, " -> {.code family = \"", fam, "\"} (",
    reg_family_short(fam), ")",
    if (identical(kind, "numeric") && !any(y %% 1 != 0, na.rm = TRUE))
      "; it is integer-valued, so {.code family = \"poisson\"} if it is a count" else "",
    "."
  )))
  fam
}

# Does the observed effect ride INSIDE the model cell? One fact, resolved at the argument boundary.
#' @keywords internal
reg_meta_obs_in_cell <- function(meta, deps = NULL) {
  if (!identical(meta$emp_mode, "cell")) return(FALSE)
  ck <- meta$crude_keys
  if (is.null(ck)) return(FALSE)
  if (!is.null(deps)) ck <- ck[intersect(names(ck), deps)]
  any(!is.na(unlist(ck)))
}

#' @keywords internal
reg_meta_estimand <- function(meta, outcome = NULL, family = NULL) {
  d   <- if (is.null(outcome)) NULL else as.character(outcome)
  if (is.null(d) && !is.null(family) && nzchar(family)) {
    fk   <- unname(REG_FIT_FAMILY[family]); if (is.na(fk)) fk <- family
    fams <- meta$families %||% meta$family
    hit  <- names(fams)[fams %in% c(family, fk)]
    if (length(hit)) d <- hit[[1]] else return(reg_estimand(fk, meta$effect %||% "coefficient",
                                                            meta$measure %||% "auto"))
  }
  pick <- function(v, scalar) {
    if (is.null(v)) return(scalar)
    if (!is.null(d) && !is.null(names(v)) && d %in% names(v)) return(unname(v[[d]]))
    unname(v[[1]])
  }
  fam <- pick(meta$families, meta$family)
  eff <- pick(meta$effects,  meta$effect  %||% "coefficient")
  mea <- pick(meta$measures, meta$measure %||% "auto")
  res <- reg_estimand(fam %||% "gaussian", eff %||% "coefficient", mea %||% "auto")
  if (identical(res$status, "ok")) res else reg_estimand(fam %||% "gaussian", "coefficient", "auto")
}

reg_model_line <- function(meta) {
  if (is.null(meta)) return(NULL)
  fam <- reg_family_display_name(reg_meta_estimand(meta)$fit %||% meta$family)
  est <- reg_estimand_note(reg_meta_estimand(meta), obs_in_cell = reg_meta_obs_in_cell(meta))
  # `who` carries no leading space: xgettext strips edge whitespace from a msgid, so the space and
  # the punctuation live in the outer template, which the translation then controls.
  who <- if (isTRUE(meta$comparison)) {
    pl <- meta$positive_level[[1]]
    w  <- if (!is.na(pl)) gettextf("of %s ('%s')", meta$outcome[[1]], pl)
          else            gettextf("of %s", meta$outcome[[1]])
    paste0(" ", w)
  } else ""
  line <- if (nzchar(est)) gettextf("Model: %s%s; %s.", fam, who, est)
          else            gettextf("Model: %s%s.", fam, who)
  enc2utf8(line)
}

reg_model_lines <- function(x, lang = NULL) {
  meta <- reg_call(x)
  if (is.null(meta)) return(character(0))
  with_legend_lang(lang, function(lg) {
    fams <- meta$families; if (is.null(fams)) fams <- meta$family
    uf   <- unique(fams)
    if (length(uf) <= 1L) { rl <- reg_model_line(meta); return(if (is.null(rl)) character(0) else rl) }
    deps <- meta$outcome
    vapply(uf, function(fm) {
      grp   <- deps[fams == fm]
      e     <- reg_meta_estimand(meta, grp[[1]])
      fname <- reg_family_display_name(e$fit %||% fm)
      est   <- reg_estimand_note(e, obs_in_cell = reg_meta_obs_in_cell(meta, grp))
      enc2utf8(if (nzchar(est)) gettextf("Model (%s): %s; %s.", legend_name_list(grp), fname, est)
               else            gettextf("Model (%s): %s.", legend_name_list(grp), fname))
    }, character(1), USE.NAMES = FALSE)
  })
}

# The AGGREGATED effect-modification test, one footer LINE per model. A pooled test belongs to no
# single model column, which is the only thing the footer-ROW machinery can key on.
#' @keywords internal
reg_interaction_lines <- function(x, lang = NULL) {
  tt <- get_test(x)
  if (is.null(tt) || nrow(tt) == 0) return(character(0))
  it <- tt[tt$test %in% reg_interaction_types(), , drop = FALSE]
  if (nrow(it) == 0) return(character(0))
  meta <- reg_call(x)
  sv   <- if (is.null(meta)) NA_character_ else meta$tab_vars
  with_legend_lang(lang, function(lg) {
    tname <- vapply(reg_interaction_types(),
                    function(k) gettext(TEST_ROWS[[k]]$instrument), character(1))
    on_coef <- !is.null(meta) && !identical(meta$effect %||% "coefficient", "coefficient")
    vapply(split(seq_len(nrow(it)), factor(it$col, levels = unique(it$col))), function(idx) {
      d     <- it[idx, , drop = FALSE]
      items <- paste0(test_key_col(d, "var"), " p = ", test_fmt_pvalue(d$pvalue),
                      stars_from_pvalue(d$pvalue))
      kind  <- unname(tname[d$test[1]]); if (is.na(kind)) kind <- gettext("Wald test")
      what  <- if (on_coef) gettextf("%s on the coefficients", kind) else kind
      head  <- if (!is.na(sv) && nzchar(sv)) gettextf("Interaction with %s (%s):", sv, what)
               else                          gettextf("Interaction (%s):", what)
      enc2utf8(paste0(head, " ", paste(items, collapse = ", "), "."))
    }, character(1), USE.NAMES = FALSE)
  })
}

reg_title <- function(meta, max = 2, lang = NULL) {
  if (is.null(meta)) return(NA_character_)
  fams <- meta$families; if (is.null(fams)) fams <- meta$family
  mixed <- length(unique(fams)) > 1L
  with_legend_lang(lang, function(lg) {
    fam <- reg_family_display_name(meta$family)
    Fam <- if (mixed) gettext("Regression models")
           else paste0(toupper(substr(fam, 1, 1)), substr(fam, 2, nchar(fam)))
    dep <- tab_title_names(meta$outcome, max)
    tabbed <- if (!is.null(meta$tab_vars)) paste0(" ", gettextf("(tabbed by %s)", meta$tab_vars)) else ""
    by_of  <- function(preds) if (nzchar(preds)) paste0(" ", gettextf("by %s", preds)) else ""
    if (mixed) return(enc2utf8(paste0(Fam, ": ", dep, by_of(tab_title_names(meta$predictors, max)), tabbed)))
    if (isTRUE(meta$comparison)) {
      pl  <- meta$positive_level[[1]]
      dref <- if (!is.na(pl)) paste0(dep, ", '", pl, "' (", meta$eff_word, ")")
              else            paste0(dep, " (", meta$eff_word, ")")
      enc2utf8(gettextf("%s (models comparison): %s", paste0(Fam, "s"), paste0(dref, tabbed)))
    } else {
      preds <- tab_title_names(meta$predictors, max)
      enc2utf8(paste0(Fam, ": ", dep, by_of(preds), tabbed))
    }
  })
}

reg_sheet_name <- function(meta) {
  if (is.null(meta)) return(NA_character_)
  fams <- meta$families; if (is.null(fams)) fams <- meta$family
  tail <- if (isTRUE(meta$comparison)) c(meta$outcome[[1]], "compare")
          else                         c(meta$outcome, meta$predictors)
  short <- if (length(unique(fams)) > 1L) "reg" else reg_family_short(meta$family)
  paste(c(short, tail), collapse = "_")
}

# The shared col_var of a model column and its crude companion, so ONE span names the outcome and no
# border separates them. ⚠ a SUMMED SCORE (`trials =`) has no level to name -- its "positive level"
# is the internal binomial success code, and naming it split one comparison in two.
reg_shared_col_var <- function(family, outcome, positive_level, cleannames, trials = NULL) {
  named <- reg_fam_binary(family) && is.null(trials) &&
    !is.null(positive_level) && !is.na(positive_level)
  if (named) paste0(outcome, ": ", reg_cleanup(positive_level, cleannames)) else outcome
}

# On a PER-CATEGORY column set the outcome CATEGORY takes the header slot, so the measure has nowhere
# else to go and lives in the span. A comparison keeps each model's name.
reg_category_col_var <- function(sp, is_comparison, positive_level, cleannames) {
  if (isTRUE(is_comparison)) return(sp$label)
  paste0(reg_shared_col_var(sp$fit_family, sp$outcome, positive_level, cleannames, sp$trials), ": ",
         reg_word(sp$est))
}

reg_model_col_name <- function(eff_word, outcome, is_comparison, model_label, n_outcomes) {
  if (isTRUE(is_comparison)) return(model_label)
  if (n_outcomes > 1L) paste0("Model_", eff_word, " [", outcome, "]") else paste0("Model_", eff_word)
}

reg_prep_binary <- function(data, outcome, outcome_level = NULL) {
  y <- data[[outcome]]
  if (is.numeric(y) && all(stats::na.omit(y) %in% c(0, 1))) {
    y <- factor(y, levels = c(0, 1), labels = c(paste0("Not ", outcome), outcome))
    if (!is.null(outcome_level) && outcome_level %in% c(paste0("Not ", outcome), "0"))
      y <- forcats::fct_rev(y)
  } else {
    y <- forcats::fct_drop(as.factor(y))
    if (nlevels(y) != 2L) {
      cli::cli_abort(c(
        "The outcome variable {.val {outcome}} must be binary (2 levels).",
        "x" = "It has {nlevels(y)} level{?s}: {.val {levels(y)}}.",
        "i" = paste0("For a summed score -- how many of q yes/no items each person chose -- pass ",
                     "{.arg trials} to fit a grouped binomial."),
        "i" = paste0("For an outcome with 3 or more categories, use {.code family = \"multinomial\"} ",
                     "(unordered) or {.code family = \"ordinal\"} (ordered).")
      ))
    }
    # glm models levels(y)[2], so the chosen level goes LAST.
    if (identical(levels(y)[[1]], reg_positive_level(data, outcome, outcome_level)))
      y <- forcats::fct_rev(y)
  }
  data[[outcome]] <- y
  attr(data, "positive_level") <- levels(y)[[2L]]
  data
}

reg_positive_level <- function(data, outcome, outcome_level = NULL) {
  y <- data[[outcome]]
  if (is.numeric(y) && all(stats::na.omit(y) %in% c(0, 1))) {
    neg <- paste0("Not ", outcome)
    return(if (identical(outcome_level, "0") || identical(outcome_level, neg)) neg else outcome)
  }
  lv <- levels(forcats::fct_drop(as.factor(y)))
  if (!is.null(outcome_level) && outcome_level %in% lv) outcome_level else lv[[1L]]
}

reg_apply_references <- function(data, ref, predictors, outcomes = character(0)) {
  nm <- names(ref)
  if (is.null(nm) || any(!nzchar(nm))) {
    cli::cli_abort(c("{.arg ref} must be a named vector, e.g. {.code c(race = \"White\")}."))
  }
  extra <- setdiff(nm, predictors)
    # An OUTCOME named here is the other question: `ref` names the level compared AGAINST,
    # `outcome_level` the level MODELLED.
  wrong <- intersect(extra, outcomes)
  if (length(wrong))
    cli::cli_abort(c("{.val {wrong[[1]]}} is an outcome, not a predictor, so {.arg ref} cannot set its level.",
                     "i" = paste0("{.arg ref} names the level other levels are compared AGAINST; ",
                                  "{.arg outcome_level} names the level that is MODELLED."),
                     "i" = 'Did you mean {.code outcome_level = c({wrong[[1]]} = "{ref[[wrong[[1]]]]}")}?'),
                   call = NULL)
  extra <- setdiff(extra, outcomes)
  if (length(extra) > 0L) {
    cli::cli_warn("{.arg ref} name{?s} {.val {extra}} match no predictor; ignored.")
  }
  for (v in intersect(nm, predictors)) {
    f <- data[[v]]
    if (!is.factor(f) && !is.character(f)) {
      cli::cli_warn("{.arg ref} ignored for {.val {v}}: not a factor/character predictor.")
      next
    }
    f   <- as.factor(f)
    lev <- ref[[v]]
    if (!lev %in% levels(f)) {
      cli::cli_abort(c("{.arg ref} level {.val {lev}} not found in {.val {v}}.",
                       "i" = "Levels: {.val {levels(f)}}."))
    }
    data[[v]] <- forcats::fct_relevel(f, lev)
  }
  data
}

# === SECTION: The coefficient skeleton and the shared Wald assembly =============================

# The row skeleton, in display order; `term` matches lm / glm / svyglm coefficient names, so a fit
# aligns to it by term. `shape_terms` adds a non-linear `shape =`'s CURVATURE row, breaking the
# 1-to-1 by a rule: ONE ROW PER MODEL TERM on the coefficient path, ONE ROW PER PREDICTOR on the
# marginal one.
reg_skeleton <- function(data, predictors, shape_terms = NULL) {
  parts <- purrr::map(predictors, function(p) {
    v <- data[[p]]
    if (reg_is_factor_var(v)) {
      lv <- levels(forcats::fct_drop(as.factor(v)))
      tibble::tibble(
        var    = p,
        level  = lv,
        term   = c(NA_character_, paste0(p, lv[-1])),
        is_ref = c(TRUE, rep(FALSE, length(lv) - 1L))
      )
    } else {
      sq <- if (!is.null(shape_terms) && p %in% names(shape_terms)) shape_terms[[p]] else NULL
      tibble::tibble(
        var    = p,
        level  = c(p, if (!is.null(sq)) reg_shape_sq_level(p)),
        term   = c(p, if (!is.null(sq)) gsub("`", "", sq, fixed = TRUE)),
        is_ref = rep(FALSE, 1L + !is.null(sq))
      )
    }
  })
  dplyr::bind_rows(
    tibble::tibble(var = "Constant", level = "Reference population",
                   term = "(Intercept)", is_ref = TRUE),
    parts
  )
}

reg_skeleton_from_fit <- function(fit) {
  tt      <- stats::terms(fit)
  labels  <- attr(tt, "term.labels")
  mm      <- stats::model.matrix(fit)
  assign  <- attr(mm, "assign")                         # 0 = intercept, k = labels[k]
  # ⚠ the coefficient names come off the MODEL MATRIX, the vector `assign` indexes, NEVER off coef(),
  # whose shape is the FITTER's: nnet::multinom returns a MATRIX (names() NULL) and MASS::polr drops
  # the intercept from coef() but not from the model matrix.
  coefnms <- colnames(mm)
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

term_prefix <- function(label) {
  stringi::stri_replace_all_regex(label, "([.\\\\+*?\\[^\\]$(){}=!<>|:#/-])", "\\\\$1")
}

reg_cleanup <- function(x, cleannames)
  if (isTRUE(cleannames)) stringi::stri_replace_all_regex(x, cleannames_condition(), "") else x

# The (var, level [, extra]) join key aligning fitted results onto skeleton rows: the separator is a
# carriage return, which can never appear inside a variable name or a factor level.
reg_skel_key <- function(var, level, extra = NULL)
  if (is.null(extra)) paste(var, level, sep = "\r") else paste(var, level, extra, sep = "\r")

reg_skel_match <- function(skeleton, src) {
  if (is.null(src) || !nrow(src)) return(rep(NA_integer_, nrow(skeleton)))
  match(reg_skel_key(skeleton$var, skeleton$level), reg_skel_key(src$var, src$level))
}

# Per-coefficient LIKELIHOOD-RATIO p-values -- the dual of the profile-likelihood interval;
# unweighted glm only, and for a factor a test of one level against the reference.
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
  stats::setNames(p, stringi::stri_replace_all_regex(colnames(X), "`", ""))
}

# The shared Wald assembly: the interval is est +/- crit * se and the p is recomputed from those same
# two numbers, so bounds and stars are exact duals. `disp_known` picks z (dispersion fixed by the
# family) over t on `df` (dispersion estimated: lm, quasi*, weighted).
reg_wald_finalize <- function(est, do_exp, se = NULL, crit = NULL,
                              lo = NULL, hi = NULL, p = NULL, disp_known = TRUE, df = NULL) {
  if (is.null(lo)) lo <- est - crit * se
  if (is.null(hi)) hi <- est + crit * se
  if (is.null(p))
    p <- if (isTRUE(disp_known)) 2 * stats::pnorm(-abs(est / se))
         else                    2 * stats::pt(-abs(est / se), df = df)
  if (do_exp) { est <- exp(est); lo <- exp(lo); hi <- exp(hi) }
  list(estimate = est, conf.low = lo, conf.high = hi, p.value = p)
}

reg_wald_from_tidy <- function(td, conf_level, do_exp) {
  res <- reg_wald_finalize(td$estimate, do_exp, se = td$std.error,
                           crit = stats::qnorm(1 - (1 - conf_level) / 2))
  td$estimate <- res$estimate; td$conf.low <- res$conf.low
  td$conf.high <- res$conf.high; td$p.value <- res$p.value
  td
}

# === SECTION: The 3+ level engines (multinomial / proportional-odds) ============================

# THE model formula of every fitter: a compound `formula` is fitted VERBATIM (it owns its RHS, so
# the shape terms do not apply). ⚠ it must reach EVERY fitter -- a 3+ level engine building its own
# formula silently dropped the user's interactions from the MODEL, not merely from the table.
#' @keywords internal
#' @noRd
reg_fit_formula <- function(outcome, predictors, add_terms = NULL, formula = NULL) {
  if (!is.null(formula)) return(formula)
  stats::as.formula(paste0(
    "`", outcome, "` ~ ",
    paste(c(paste0("`", predictors, "`"), add_terms), collapse = " + ")))
}

reg_fit_multinom <- function(mdata, outcome, predictors, do_exp, conf_level, method,
                             weighted = FALSE, make_design = NULL, add_terms = NULL,
                             formula = NULL) {
  if (method == "profile") {
    cli::cli_inform(c("!" = "Profile intervals are not defined for multinomial models; using Wald."))
  }
  mdata[[outcome]] <- forcats::fct_drop(as.factor(mdata[[outcome]]))
  y_levels <- levels(mdata[[outcome]])
  # ⚠ re-home the formula to THIS frame: nnet::multinom and MASS::polr store their call and
  # re-evaluate it, so a formula carrying the user's environment resolves `fml` nowhere.
  fml <- reg_fit_formula(outcome, predictors, add_terms, formula)
  environment(fml) <- environment()

  if (weighted) {
    # refLevel = 1 makes the FIRST level the baseline, matching nnet; VGAM names coefficients
    # "term:k" with k the k-th NON-reference category, so each name is parsed back.
    fit <- svyVGAM::svy_vglm(fml, design = make_design(mdata),
                             family = VGAM::multinomial(refLevel = 1))
    cf  <- if (!is.null(fit$coef)) fit$coef else stats::coef(fit)   # svy_vglm stores $coef / $var
    V   <- if (!is.null(fit$var))  fit$var  else stats::vcov(fit)
    se  <- sqrt(diag(V))
    nm  <- names(cf)
    k   <- suppressWarnings(as.integer(sub("^.*:(\\d+)$", "\\1", nm)))
    trm <- sub(":\\d+$", "", nm)
    if (any(is.na(k)) || max(k) > length(y_levels) - 1L) {
      cli::cli_abort(c("Could not map {.pkg svyVGAM} coefficients to outcome categories.",
                       "i" = "Unexpected coefficient names: {.val {nm[is.na(k)]}}."))
    }
    ylev <- y_levels[-1]                               # non-reference categories, in level order
    td   <- tibble::tibble(y.level = ylev[k], term = stringi::stri_replace_all_regex(trm, "`", ""),
                           estimate = unname(cf), std.error = unname(se[nm]))
    td   <- reg_wald_from_tidy(td, conf_level, do_exp)
    return(list(tidy = td, nobs = nrow(mdata), var_y = NA_real_, positive_level = NULL,
                fit = fit, data = mdata, y_ref = y_levels[1], y_levels = y_levels[-1]))
  }

  fit <- nnet::multinom(fml, data = mdata, trace = FALSE)
  td  <- broom::tidy(fit)                              # y.level, term, estimate, std.error, ...
  td$term <- stringi::stri_replace_all_regex(td$term, "`", "")     # strip formula backticks -> match skeleton
  td  <- reg_wald_from_tidy(td, conf_level, do_exp)
  list(tidy = td, nobs = nrow(mdata), var_y = NA_real_, positive_level = NULL,
       fit = fit, data = mdata, y_ref = y_levels[1], y_levels = y_levels[-1])
}

# Ordered 3+ level outcome: proportional-odds cumulative logit -- MASS::polr unweighted,
# survey::svyolr weighted. ONE column of cumulative ORs; the cut-point rows are dropped, so
# "Constant" stays NA.
reg_fit_ordinal <- function(mdata, outcome, predictors, do_exp, conf_level, method,
                            weighted = FALSE, make_design = NULL, add_terms = NULL,
                            formula = NULL) {
  if (method == "profile") {
    cli::cli_inform(c("!" = "Profile intervals are not defined for proportional-odds models; using Wald."))
  }
  y <- mdata[[outcome]]
  if (!is.ordered(y)) {
    y <- as.ordered(forcats::fct_drop(as.factor(y)))
    lv_str <- paste(levels(y), collapse = " < ")
    cli::cli_inform(c("i" = "{.val {outcome}}: treated as ordered ({lv_str})."))
  } else {
    y <- forcats::fct_drop(y)
  }
  mdata[[outcome]] <- y
  # ⚠ re-home the formula to THIS frame -- see reg_fit_multinom().
  fml <- reg_fit_formula(outcome, predictors, add_terms, formula)
  environment(fml) <- environment()

  if (weighted) {
    # svyolr's coef() also returns the cut-point thresholds, so the SLOPES come off fit$coefficients;
    # its start-value glm.fit step cannot take zero or negative weights.
    fit <- tryCatch(
      survey::svyolr(fml, design = make_design(mdata)),
      error = function(e) cli::cli_abort(c(
        "The survey-weighted ordinal model ({.fn survey::svyolr}) failed to fit.",
        "x" = conditionMessage(e),
        "i" = "{.fn svyolr} needs strictly positive weights - check for zero / negative weights."
      ))
    )
    cf  <- fit$coefficients
    se  <- sqrt(diag(stats::vcov(fit)))[names(cf)]
    td  <- tibble::tibble(term = stringi::stri_replace_all_regex(names(cf), "`", ""),
                          estimate = unname(cf), std.error = unname(se))
    td  <- reg_wald_from_tidy(td, conf_level, do_exp)
    cli::cli_inform(c("i" = paste0("The proportional-odds (parallel-lines) assumption is not tested for ",
                                   "survey-weighted ordinal models (the Brant test needs an unweighted fit).")))
    return(list(tidy = td, nobs = nrow(mdata), var_y = NA_real_, positive_level = NULL, fit = fit,
                data = mdata))
  }

  fit <- MASS::polr(fml, data = mdata, Hess = TRUE, method = "logistic")
  td  <- broom::tidy(fit)
  td  <- td[td$coef.type == "coefficient", , drop = FALSE]   # drop cut-point ("scale") intercepts
  td$term <- stringi::stri_replace_all_regex(td$term, "`", "")
  td  <- reg_wald_from_tidy(td, conf_level, do_exp)
  # The Brant test is NOT run here: it is a footer ROW's statistic costing J-1 extra fits, so it is
  # built where that row is -- else every diagnostic and crude polr fit would pay for it.
  list(tidy = td, nobs = nrow(mdata), var_y = NA_real_, positive_level = NULL, fit = fit,
       data = mdata)
}

# Make a fit SELF-CONTAINED: nnet::multinom / MASS::polr store `data = mdata`, a local of reg_fit(),
# so brant::brant() and stats::drop1() would fail with "object 'mdata' not found".
#' @keywords internal
reg_selfheal_call <- function(fit, data) {
  if (is.null(data) || is.null(fit$call)) return(fit)
  fml <- tryCatch(stats::formula(fit), error = function(e) NULL)
  if (is.null(fml)) return(fit)
  fit$call$data    <- data
  fit$call$formula <- fml
  fit
}

# The Brant test (`brant`, a Suggests): a missing package skips it with a hint and a failing test is
# swallowed -- a diagnostic must never break a table.
reg_ordinal_diagnostic <- function(fit) {
  if (!requireNamespace("brant", quietly = TRUE)) {
    cli::cli_inform(c("i" = paste0(
      "Proportional-odds (parallel-lines) assumption not tested: install {.pkg brant} to run the ",
      "Brant test."
    )))
    return(invisible(NA_real_))
  }
  fit <- reg_selfheal_call(fit, fit$model)
  bt <- tryCatch({ utils::capture.output(res <- brant::brant(fit)); res },
                 error = function(e) NULL)
  if (is.null(bt) || !is.matrix(bt) ||
      !"Omnibus" %in% rownames(bt) || !"probability" %in% colnames(bt)) {
    return(invisible(NA_real_))                             # unexpected shape -> stay silent
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
  invisible(p)
}

# === SECTION: Survey design construction =========================================================
# A weight column becomes a survey.design PER MODEL on the complete-case frame: ids = ~1 reproduces
# the flat weighted path exactly. A PREBUILT design is never rebuilt -- a calibrated one cannot be --
# only subset()'d to the model's complete cases (domain estimation).

reg_design_vars <- function(design_spec) svy_design_vars(design_spec)

# The model's complete-case frame -- the ONE definition of "the same population as the model". The
# empirical blocks recompute it from raw `data`, `f$data` being NULL on the reref / digest path.
reg_complete_frame <- function(data, vars)
  tidyr::drop_na(data, tidyselect::all_of(intersect(unique(vars), names(data))))

reg_resolve_design <- function(design_spec, mdata, data, drop_vars) {
  if (!is.null(design_spec$design)) {
    keep <- which(stats::complete.cases(data[, drop_vars, drop = FALSE]))
    # ⚠ index the ORIGINAL design, always. Under tab_vars `data` holds one group's rows, so its own
    # positions are group-local, while `.svy_row` is the position in the design the user passed.
    # Without it a CALIBRATED design -- which `[` does not shrink -- weights the wrong respondents.
    rows <- if (!is.null(data[[svy_row_col]])) as.integer(data[[svy_row_col]])[keep] else keep
    svy_domain_design(design_spec$design, rows, mdata)
  } else {
    svy_make_design(mdata, design_spec$wt)
  }
}
# AIC.svyglm / anova.svyglm refit sub-models with an UNQUALIFIED `svyglm()` call, which fails when
# survey is loaded via `::` but not attached: bind it into a child of the formula's environment.
reg_svyglm_env <- function(fit) {
  env <- tryCatch(environment(stats::formula(fit)), error = function(e) NULL)
  if (is.null(env)) env <- globalenv()
  if (!exists("svyglm", envir = env, inherits = TRUE)) {
    ne <- new.env(parent = env)
    assign("svyglm", survey::svyglm, envir = ne)
    try(environment(fit$formula) <- ne, silent = TRUE)
    try(environment(fit$terms)   <- ne, silent = TRUE)
  }
  fit
}
# Releveling touches only $variables, never the weights / strata / fpc / row set.
reg_relevel_design <- function(design, ref, relevelable) {
  design$variables <- reg_apply_references(design$variables, ref, relevelable)
  design
}

# === SECTION: reg_fit() -- one model, one tidy ===================================================
# Fit ONE model on complete cases -> a tidy of the effect measure + CI + p + n. `do_exp` chooses the
# estimate scale: exp(coef) multiplicative, raw coef additive.
reg_fit <- function(data, outcome, predictors, family, design_spec, do_exp,
                    outcome_level, conf_level, method,
                    trials = NULL, formula = NULL, multiplier = NULL, cross = NULL,
                    drop_extra = NULL, add_terms = NULL) {
  # Three siblings that cannot go through the `formula =` escape hatch, because they must inherit
  # the binary prep, the grouped-binomial cbind, the "rr" route and the design resolution:
  # `add_terms` adds RHS terms naming no new variable; `cross` is a tab_vars, making the POOLED
  # interaction fit; and `drop_extra` joins drop_vars but NOT the formula -- variables the fit must
  # be COMPLETE ON without modelling, which is how a crude univariable fit lands on exactly the
  # model's population. ⚠ a pre-filtered frame passed as `data` is NOT equivalent: a PREBUILT
  # design's keep mask is computed from `data` itself, and a shorter one recycles silently.
  drop_vars <- unique(c(outcome, predictors, cross, drop_extra, reg_design_vars(design_spec)))
  mdata     <- reg_complete_frame(data, drop_vars)

  fac_preds <- reg_factor_preds(mdata, c(predictors, cross))
  if (length(fac_preds) > 0L) {
    # An ORDERED predictor makes glm / polr use polynomial contrasts, which no per-level skeleton can
    # align -- an all-NA column. Only PREDICTORS; an ordinal DEPENDENT keeps its order.
    mdata <- dplyr::mutate(mdata, dplyr::across(
      tidyselect::all_of(fac_preds),
      ~ { f <- forcats::fct_drop(as.factor(.)); factor(f, levels = levels(f), ordered = FALSE) }
    ))
  }

  weighted <- svy_weighted(design_spec, design_spec$wt)
  make_design <- function(recoded_mdata) reg_resolve_design(design_spec, recoded_mdata, data, drop_vars)

  if (family == "multinomial") {
    return(reg_fit_multinom(mdata, outcome, predictors, do_exp, conf_level, method,
                            weighted, make_design, add_terms = add_terms, formula = formula))
  }
  if (family == "ordinal") {
    return(reg_fit_ordinal(mdata, outcome, predictors, do_exp, conf_level, method,
                           weighted, make_design, add_terms = add_terms, formula = formula))
  }

  positive_level <- NULL
  grouped <- reg_is_grouped_binomial(family, trials, !is.null(formula))
  if (grouped) {
    s <- mdata[[outcome]]
    if (!is.numeric(s) || any(s %% 1 != 0, na.rm = TRUE)) {
      cli::cli_abort(c("A summed-score outcome ({.arg trials}) must be integer-valued.",
                       "x" = "{.val {outcome}} is {.cls {class(s)}}."))
    }
    if (any(s < 0 | s > trials, na.rm = TRUE)) {
      cli::cli_abort(c("{.val {outcome}} scores must lie in {.val {0}}..{.val {trials}} (= {.arg trials}).",
                       "x" = "Observed range: {.val {range(s, na.rm = TRUE)}}."))
    }
    mdata[[".gb_succ"]] <- s
    mdata[[".gb_fail"]] <- trials - s
    # for the links that cannot take a two-column response: `.gb_trials` is the modified Poisson's
    # offset (so exp(coef) stays a PER-ITEM ratio), `.gb_prop` the risk the identity link is fitted
    # on.
    mdata[[".gb_trials"]] <- trials
    mdata[[".gb_prop"]]   <- s / trials
  }

  fam_obj <- switch(
    family,
    "binomial" = {
      if (is.null(trials) && is.null(formula)) {
        mdata <- reg_prep_binary(mdata, outcome, outcome_level)
        positive_level <- attr(mdata, "positive_level")
      }
      if (weighted) stats::quasibinomial("logit") else stats::binomial("logit")
    },
    "poisson" = if (weighted) stats::quasipoisson("log") else stats::poisson("log"),
    "quasipoisson" = stats::quasipoisson("log"),
    # modified Poisson on a binary outcome (Zou 2004): the logistic arm's binary prep, then the 0/1
    # NUMERIC a log-link Poisson needs. quasipoisson in BOTH bases -- the fit goes through svyglm
    # either way, and AIC / BIC then return NA, the honest answer for a quasi-likelihood.
    "rr" = {
      if (!grouped) {
        mdata <- reg_prep_binary(mdata, outcome, outcome_level)
        positive_level <- attr(mdata, "positive_level")
        mdata[[outcome]] <- as.numeric(mdata[[outcome]] == positive_level)
      }
      stats::quasipoisson("log")
    },
    "rd" = {
      if (!grouped) {
        mdata <- reg_prep_binary(mdata, outcome, outcome_level)
        positive_level <- attr(mdata, "positive_level")
        mdata[[outcome]] <- as.numeric(mdata[[outcome]] == positive_level)
      }
      stats::binomial("identity")
    },
    # the RATIO OF MEANS: Poisson pseudo-maximum-likelihood with robust SEs -- the log link is the
    # point, not a claim about counts.
    "mr" = {
      y <- suppressWarnings(as.numeric(mdata[[outcome]]))
      if (any(is.finite(y) & y < 0)) cli::cli_abort(c(
        '{.code measure = "ratio"} needs a non-negative outcome: a ratio of means is not defined when {.val {outcome}} can be negative.',
        "i" = 'Model {.code log()} of a positive outcome instead, or use {.code measure = "difference"}.'))
      stats::quasipoisson("log")
    },
    "gaussian" = stats::gaussian(),
    cli::cli_abort("Unsupported {.arg family}: {.val {family}}.")
  )
  if (is.null(formula) && !grouped && !reg_fam_binary(family) && !is.numeric(mdata[[outcome]])) {
    cli::cli_abort(c(
      "A {.val {family}} outcome must be numeric.",
      "x" = "{.val {outcome}} is {.cls {class(mdata[[outcome]])}}."
    ))
  }

  fml_lpm <- NULL
  fml <- if (!is.null(formula)) {
    formula                                            # compound escape-hatch: fit verbatim
  } else {
    resp <- if (!grouped) paste0("`", outcome, "`") else if (identical(family, "rr"))
      "`.gb_succ`" else "cbind(`.gb_succ`, `.gb_fail`)"
    rhs  <- paste0("`", predictors, "`", collapse = " + ")
    if (!is.null(cross)) rhs <- paste0("(", rhs, ") * `", cross, "`")   # the pooled interaction fit
    if (length(add_terms)) rhs <- paste(c(rhs, add_terms), collapse = " + ")
    fml_lpm <- stats::as.formula(
      paste0(if (grouped) "`.gb_prop`" else resp, " ~ ", rhs))
    # A Poisson likelihood has no two-column response: the grouped modified Poisson models the count
    # with log(trials) as OFFSET, which keeps exp(coef) a per-item risk ratio.
    if (grouped && identical(family, "rr")) rhs <- paste0(rhs, " + offset(log(`.gb_trials`))")
    stats::as.formula(paste0(resp, " ~ ", rhs))
  }
  if (is.null(fml_lpm)) fml_lpm <- fml

  # ⚠ "rr" ALWAYS fits through svyglm, weighted or not: a Poisson likelihood on a 0/1 outcome is
  # deliberately misspecified, so its naive SEs must become the Huber-White SANDWICH -- which
  # svyglm's design-based variance IS, so reg_build_digest() stores a vcov already sandwiched and
  # the jamovi reref contract needs no special case. `weighted` stays FALSE here: it means "the USER
  # gave a design".
  use_svy <- reg_fam_svy_fitted(family, weighted)
  fit <- if (family == "gaussian" && !weighted) {
    stats::lm(fml, data = mdata)
  } else if (!use_svy) {
    stats::glm(fml, data = mdata, family = fam_obj)
  } else if (family == "rd") {
    # The identity link needs starting values and can still fail: start from the OLS fit, and on
    # failure BE it. WARNING: that fallback TARGETS the same risk difference but is a different
    # ESTIMATOR -- the two coincide only where the model holds, so the message must name which ran.
    des0  <- make_design(mdata)
    start <- tryCatch(stats::coef(stats::lm(fml_lpm, data = mdata)), error = function(e) NULL)
    fit   <- tryCatch(
      do.call(survey::svyglm, list(fml, design = des0, family = fam_obj, start = start)),
      error = function(e) NULL, warning = function(w) NULL)
    if (is.null(fit) || !isTRUE(fit$converged)) {
      cli::cli_inform(c("!" = paste0(
        "The identity-link risk-difference model did not converge for {.val {outcome}}; ",
        "fitting the {.strong linear probability model} instead. It estimates the same risk ",
        "difference, but is a different estimator: the two agree only where the model holds.")))
      fit <- do.call(survey::svyglm, list(fml_lpm, design = des0, family = stats::gaussian()))
    }
    fit
  } else {
    # WARNING: svyglm is called through do.call() with the family OBJECT spliced in -- some of its
    # methods re-evaluate their own call in the design's data enclosure, where `fam_obj` does not
    # exist.
    do.call(survey::svyglm, list(fml, design = make_design(mdata), family = fam_obj))
  }
  if (inherits(fit, "svyglm")) fit <- reg_svyglm_env(fit)

  td <- broom::tidy(fit)                            # native scale: estimate, std.error, p.value
  td$term <- stringi::stri_replace_all_regex(td$term, "`", "")  # strip formula backticks -> match skeleton

  # A k-unit change multiplies the native-scale coefficient by k (se by |k|). Applied BEFORE the CI,
  # so the Wald interval and the profile bounds scale with it; the p is scale-invariant and is left
  # alone.
  mult_vec <- rep(1, nrow(td))
  if (!is.null(multiplier)) {
    for (v in names(multiplier)) {
      mi <- td$term == v
      if (any(mi)) mult_vec[mi] <- as.numeric(multiplier[[v]])
    }
    td$estimate  <- td$estimate  * mult_vec
    td$std.error <- td$std.error * abs(mult_vec)
  }

  # An unweighted Poisson / grouped-binomial MLE reports naive SEs: scale them by sqrt(phi) so the
  # CI and stars match a quasi fit, while the MLE keeps its likelihood for the AIC / LR footer.
  # Bernoulli dispersion is not identifiable and gaussian has none, so both stay untouched.
  over_disp <- !weighted && reg_fam_overdispersed(family, grouped)
  phi       <- if (over_disp) reg_dispersion(fit) else NA_real_
  scaled    <- over_disp && !is.na(phi) && phi > 0
  if (scaled) {
    td$std.error <- td$std.error * sqrt(phi)
    if (phi > 1.5) cli::cli_warn(c(
      "!" = paste0("Over-dispersion (Pearson dispersion = {signif(phi, 3)}",
                   "{if (phi > 2) ', strong' else ''}); standard errors are scaled by sqrt(dispersion) ",
                   "(quasi-{family}-like)."),
      "i" = "The footer reports the dispersion; use {.code family = \"quasipoisson\"} for the fully quasi fit."
    ))
  }

  # "rr" is excluded by construction, but say so rather than downgrade silently.
  use_profile <- method == "profile" && !weighted && reg_fam_disp_known(family)
  if (method == "profile" && weighted) {
    cli::cli_inform(c("!" = paste0("Profile-likelihood intervals are not defined for survey-weighted ",
                                   "models; using Wald.")))
  } else if (method == "profile" && family == "rr") {
    cli::cli_inform(c("!" = paste0("Profile-likelihood intervals are not defined for a modified Poisson ",
                                   "fit (a quasi-likelihood); using the robust Wald interval.")))
  }

  if (use_profile) {
    ci   <- suppressMessages(stats::confint(fit, level = conf_level))   # log/native scale
    idx  <- match(td$term, stringi::stri_replace_all_regex(rownames(ci), "`", ""))
    lo   <- unname(ci[idx, 1]) * mult_vec; hi <- unname(ci[idx, 2]) * mult_vec  # scale profile bounds
    lrp  <- reg_lr_pvalues(fit)
    p_in <- unname(lrp[match(td$term, names(lrp))])
  } else {
    # z where the family FIXES the dispersion, else t on df.residual -- an ESTIMATED dispersion (lm,
    # quasi*, weighted, or a phi-scaled fit) moves the reference off z.
    disp_known <- !weighted && reg_fam_disp_known(family) && !scaled
    crit <- reg_wald_crit(disp_known, stats::df.residual(fit), conf_level)   # shared with reg_reref (15b)
    lo <- td$estimate - crit * td$std.error
    hi <- td$estimate + crit * td$std.error
    # with the SE scaled and the t reference, p is recomputed from est / se so p, CI and stars stay
    # duals (broom's own p belongs to the un-scaled model).
    p_in <- if (scaled) 2 * stats::pt(-abs(td$estimate / td$std.error), df = stats::df.residual(fit))
            else        td$p.value
  }
  res <- reg_wald_finalize(td$estimate, do_exp, lo = lo, hi = hi, p = p_in)   # shared exp assembly
  td$estimate <- res$estimate; td$conf.low <- res$conf.low
  td$conf.high <- res$conf.high; td$p.value <- res$p.value

  # var(Y) is the standardised ladder's divisor. A summed score needs it too: its additive effect is
  # a difference of mean SCORES, graded against the score's own spread.
  var_y <- if (!do_exp && (family == "gaussian" || !is.na(trials %||% NA)))
    stats::var(as.numeric(mdata[[outcome]])) else NA_real_

  list(tidy = td, nobs = nrow(mdata), var_y = var_y, positive_level = positive_level, fit = fit,
       data = mdata)
}

# === SECTION: The column builders ================================================================
# Align one fit to the union skeleton -> ONE fmt column: a reference LEVEL of a predictor present in
# this model takes the neutral value, a predictor ABSENT from it stays NA.
reg_column <- function(skeleton, fit_res, model_predictors, col_var, est,
                       color, color_signif, model_family = "", method = "wald", trials = NA) {
  effect_shape <- if (isTRUE(est$exp)) "ratio" else "additive"
  # The column's SHAPE is the estimand row's: the fmt field, the EST_SCALES key, the token, the
  # digits. No builder names a family-specific field.
  scale_key    <- reg_scale_of(est, trials)
  est_field    <- EST_SCALES[[scale_key]]$est_field
  disp         <- "est"
  digits       <- reg_cell_digits(scale_key)
  td  <- fit_res$tidy
  m   <- match(skeleton$term, td$term)
  est_v <- td$estimate[m]
  lo  <- td$conf.low[m]
  hi  <- td$conf.high[m]
  p   <- td$p.value[m]

  in_model <- skeleton$var %in% c("Constant", model_predictors)
  ref_lvl  <- skeleton$is_ref & skeleton$var != "Constant" & in_model
  neutral  <- if (effect_shape == "ratio") 1 else 0
  est_v[ref_lvl] <- neutral
  lo[ref_lvl]  <- NA_real_
  hi[ref_lvl]  <- NA_real_
  p[ref_lvl]   <- NA_real_

  n_rows   <- nrow(skeleton)
  # in_refrow is a UNION-skeleton row fact, NOT gated by in_model: a model that OMITS a predictor
  # must not blank its reference flag, or the shared cross-column bold loses it in a comparison.
  refrows  <- (skeleton$is_ref & skeleton$var != "Constant") | skeleton$var == "Constant"

  # ⚠ a SUMMED SCORE's additive effect is a difference of mean SCORES: the fit reports a per-item
  # probability difference, and E[score] = trials x p makes the conversion exact.
  if (identical(scale_key, "raw_diff") && !is.na(trials %||% NA)) {
    k <- as.numeric(trials); est_v <- est_v * k; lo <- lo * k; hi <- hi * k
  }
  fields <- stats::setNames(list(est_v), est_field)
  args <- c(
    # NA here, overwritten in reg_spec_build_one() with each level's own count: the builders do
    # not know the model's complete-case frame, and the count is the same for every column of a fit.
    list(n = rep(NA_integer_, n_rows)),
    fields,
    list(ci_inf = lo, ci_sup = hi, pvalue = p,
         scale = scale_key, display = disp, digits = digits,
         ci_method = if (identical(method, "profile")) "profile"
                     else if (identical(effect_shape, "ratio")) "wald_log" else "wald",
         color = color, color_signif = color_signif, col_var = col_var,
         comp_all = FALSE, in_refrow = refrows, model_family = model_family, role = "model"))
  if (identical(effect_shape, "ratio")) args <- c(args, list(ref = "1"))
  args <- c(args, list(pct_type = reg_pct_type(scale_key)))
  if (identical(EST_SCALES[[scale_key]]$sd_from %||% "", "var"))
    args <- c(args, list(var = rep(fit_res$var_y, n_rows)))
  do.call(fmt, args)
}

# `display` on a regression table IS tab()'s (R/tab-display.R). THE RULE its templates obey: a
# template may ask for an AUXILIARY quantity of the SAME fit, never for a different fit or estimand.
#' @keywords internal
#' @noRd
reg_resolve_display <- function(display) display_resolve(display)

# WHY THE ADJUSTED PREDICTION AND THE ADDITIVE MARGINAL EFFECT ARE ALWAYS STORED: `display` is a
# post-hoc property -- choosing what a cell shows may never trigger a computation nor change a
# number, or set_display() on a built table would be a lie and jamovi's repaint would need a refit.
# Both come from ONE point-estimate g-computation sweep. The prediction lands in the field the
# column's scale names for a LEVEL (`EST_SCALES$base_display`, what `{base}` renders), the effect in
# `diff`. WARNING: neither may write into the column's OWN estimate field.
#' @keywords internal
#' @noRd
reg_fill_base <- function(col, marg, skeleton, model_predictors, group = NULL) {
  if (is.null(marg)) return(col)
  n_rows   <- nrow(skeleton)
  est_fld  <- fmt_center_field(col)
  base_fld <- fmt_scale_row(col)$base_display %||% NA_character_
  in_model <- skeleton$var %in% c("Constant", model_predictors)
  is_ref   <- skeleton$is_ref & skeleton$var != "Constant" & in_model
  # WARNING: where the sweep returns one value per outcome CATEGORY and the column belongs to none --
  # an ordinal cumulative odds ratio, spanning every cut -- the fill is refused rather than guessed.
  slice <- function(d) {
    if (!"group" %in% names(d)) return(d)
    if (all(is.na(d$group)))    return(d)
    if (!is.null(group))  return(d[!is.na(d$group) & d$group == group, , drop = FALSE])
    if (length(unique(stats::na.omit(d$group))) > 1L) return(d[0, , drop = FALSE])
    d
  }
  take  <- function(d, col_nm) {
    d <- slice(d)
    if (is.null(d) || !nrow(d)) return(rep(NA_real_, n_rows))
    d[[col_nm]][reg_skel_match(skeleton, d)]
  }
  pred_v <- if (is.null(marg$pred)) rep(NA_real_, n_rows) else take(marg$pred, "pred")
  if (!is.na(base_fld) && !identical(base_fld, est_fld))
    col <- vctrs::`field<-`(col, base_fld, pred_v)
  if (!identical(est_fld, "diff")) {
    # DESIGN: a factor level's ADDITIVE marginal effect is derived from the two adjusted predictions
    # rather than the sweep's own contrast. The two are the same number (averaging commutes with an
    # additive contrast), but the derived form is reference-INVARIANT, which is what lets jamovi's
    # digest re-reference a cached fit without refitting. A numeric slope comes from the sweep.
    v <- if (is.null(marg$ame)) rep(NA_real_, n_rows) else take(marg$ame, "ame")
    refi   <- which(skeleton$is_ref & in_model)
    ref_of <- pred_v[refi][match(as.character(skeleton$var),
                                 as.character(skeleton$var)[refi])]
    v <- ifelse(is.na(pred_v) | is.na(ref_of), v, pred_v - ref_of)
    v[is_ref] <- NA_real_                                # a reference level has no marginal effect
    col <- vctrs::`field<-`(col, "diff", v)
  }
  col
}

# WARNING: this calls the ANALYTIC engine directly, never reg_marginal(). These quantities are
# AUXILIARY -- what a cell MAY show -- so they are computed where they are free and absent where they
# are not. reg_marginal() would fall back to `marginaleffects` when g-computation refuses, turning an
# optional annotation into a hard dependency and, worse, an abort.
#' @keywords internal
#' @noRd
reg_fill_sweep <- function(fit, data, predictors, conf_level, wt = NULL, multiplier = NULL)
  tryCatch(reg_marginal_gcomp(fit, data, predictors, conf_level, wt, want_pred = TRUE,
                              want_se = FALSE, multiplier = multiplier),
           error = function(e) NULL)

# A pure template writer: every field it can name is already stored, and the per-cell rule is the
# crosstab's own -- a cell takes the template only where every field it names exists.
#' @keywords internal
#' @noRd
reg_apply_display <- function(col, display) {
  if (is.null(display)) return(col)
  display_write_col(col, display)$col
}

# The default layout. With a crude COLUMN the two mirror each other -- "({base}) {est}" against
# "{est} ({base})" -- so the two ESTIMATES end up adjacent, each with its level on the outside, the
# order of the modelling itself. Where the crude effect rides IN the cell, reg_set_obs() folds it in.
#' @keywords internal
#' @noRd
reg_default_display <- function(col, empirical) {
  if (!emp_on(empirical) || identical(empirical, "cell")) return(col)
  display_write_col(col, DISPLAY_PRESETS[[if (identical(get_role(col), "emp")) "base_est"
                                          else "est_base"]])$col
}

# === SECTION: Marginal effects and adjusted predictions (the `at` profile axis) ==================

# The REFERENCE PROFILE: every predictor at its reference -- a factor at its first level (the model's
# treatment-contrast baseline), a numeric at its mean -- which can be an odd baseline.
reg_reference_grid_values <- function(data, predictors) {
  vals <- lapply(predictors, function(v) {
    x <- data[[v]]
    if (is.factor(x))        levels(x)[1]
    else if (is.character(x)) sort(unique(x))[1]
    else                      mean(x, na.rm = TRUE)
  })
  stats::setNames(vals, predictors)
}

# ⚠ THE one place a marginal effect can be silently WRONG: `marginaleffects` re-evaluates a poly() /
# ns() / bs() basis on perturbed data, and an orthogonal basis absorbs a location shift exactly, so
# it returns AME = 0 with no warning. Whether it happens depends on whether the data can be
# recovered, so the answer is to CHECK, not to refuse: the comparator is stats::predict(newdata =),
# which carries the basis's frozen `predvars`. `shape = "quadratic"` is correct through every route.
#' @keywords internal
reg_basis_vars <- function(fit, predictors) {
  lab <- tryCatch(attr(stats::terms(fit), "term.labels"), error = function(e) character(0))
  hit <- grepl("\\b(poly|ns|bs|rcs)\\s*\\(", lab)
  if (!any(hit)) return(character(0))
  predictors[vapply(predictors, function(v)
    any(grepl(paste0("\\b", tolower(v), "\\b"), tolower(lab[hit]))), logical(1))]
}

#' @keywords internal
reg_marginal_basis_ok <- function(fit, data, v, k, est, ratio) {
  truth <- tryCatch({
    p0 <- stats::predict(fit, newdata = data, type = "response")
    d2 <- data; d2[[v]] <- as.numeric(d2[[v]]) + (if (is.finite(k) && k != 0) k else 1)
    mean(as.numeric(stats::predict(fit, newdata = d2, type = "response")) - as.numeric(p0),
         na.rm = TRUE)
  }, error = function(e) NA_real_)
  if (!is.finite(truth) || abs(truth) < 1e-10) return(TRUE)          # nothing to disagree about
  if (isTRUE(ratio)) return(!isTRUE(all.equal(unname(est[[1]]), 1, tolerance = 1e-8)))
  isTRUE(abs(unname(est[[1]]) - truth) <= 0.02 * abs(truth) + 1e-10)
}

# THE dispatcher between the two engines: the fast route returns NULL rather than a wrong number, and
# the fallback then runs for the WHOLE call, so one column carries one convention.
reg_marginal <- function(fit, data, predictors, conf_level, wt = NULL,
                         at = "average", comparison = NULL, want_pred = TRUE,
                         multiplier = NULL, engine = "marginaleffects", want_se = TRUE) {
  do_exp <- !is.null(comparison) && comparison %in% c("lnor", "lnratioavg")
  out <- NULL
  # "lnor" is the MNL j-vs-rest contrast, which only ever comes with at = "reference".
  if (identical(engine, "gcomp") && identical(at, "average") && !identical(comparison, "lnor"))
    out <- reg_marginal_gcomp(fit, data, predictors, conf_level, wt, ratio = do_exp,
                              want_pred = want_pred, want_se = want_se, multiplier = multiplier)
  # THE fallback, and the only place `marginaleffects` is genuinely required: the estimand's engine
  # named it, or gcomp refused this fit -- which the argument boundary cannot know.
  if (is.null(out)) {
    if (!requireNamespace("marginaleffects", quietly = TRUE))
      reg_abort_marginaleffects("this contrast, which has no closed form on this model")
    out <- reg_marginal_me(fit, data, predictors, conf_level, wt, at = at, comparison = comparison,
                           want_pred = want_pred, multiplier = multiplier, want_se = want_se)
  }
  if (identical(at, "average")) reg_marginal_basis_warn(fit, data, predictors, multiplier,
                                                        out$ame, do_exp)
  out
}

#' @keywords internal
reg_marginal_gcomp <- function(fit, data, predictors, conf_level, wt = NULL, ratio = FALSE,
                               want_pred = TRUE, want_se = TRUE, multiplier = NULL) {
  tvars <- tryCatch(all.vars(stats::delete.response(stats::terms(fit))), error = function(e) NULL)
  if (is.null(tvars) || !all(predictors %in% tvars)) return(NULL)
  V <- if (want_se) tryCatch(stats::vcov(fit), error = function(e) NULL) else NULL
  if (want_se && (is.null(V) || !is.matrix(V))) return(NULL)
  per_cat <- inherits(fit, "multinom") || inherits(fit, "polr")
  g <- if (per_cat) reg_gcomp_cat_maker(fit, data, wt, ratio)
       else         reg_gcomp_maker(fit, data, wt, ratio)
  if (is.null(g)) return(NULL)
  crit <- stats::qnorm(1 - (1 - conf_level) / 2)
  amel <- list(); predl <- list()
  for (v in predictors) {
    is_fac <- reg_is_factor_var(data[[v]])
    if (is_fac) {
      lv <- levels(forcats::fct_drop(as.factor(data[[v]])))
      if (length(lv) < 2L) return(NULL)
      cls <- lapply(lv[-1], function(l) list(level = l, at = l, ref = lv[[1]]))
    } else {
      k <- if (!is.null(multiplier) && v %in% names(multiplier)) as.numeric(multiplier[[v]]) else 1
      if (!is.finite(k) || k == 0) k <- 1
      cls <- list(list(level = v, at = k, ref = 0))   # a k-unit FORWARD DIFFERENCE, as `variables=list(v=k)`
    }
    for (ct in cls) {
      p <- g(v, ct$at, ct$ref)
      if (is.null(p)) return(NULL)
      # The 3+ level producer answers for every category at once (K-long, `group` naming them) where
      # a single-equation one is scalar. Hence one loop.
      grp <- if (per_cat) as.character(p$levels) else NA_character_
      se  <- if (per_cat) vapply(p$G, function(gj) reg_delta_se(gj, V), numeric(1))
             else         reg_delta_se(p$G, V)
      res <- reg_wald_finalize(p$est, ratio, se = se, crit = crit)
      amel[[length(amel) + 1L]] <- tibble::tibble(
        var = v, level = as.character(ct$level), group = grp,
        ame = res$estimate, ame_lo = res$conf.low, ame_hi = res$conf.high, ame_p = res$p.value)
      if (want_pred && is_fac) {
        add_pred <- function(l, val) predl[[length(predl) + 1L]] <<-
          tibble::tibble(var = v, level = l, group = grp, pred = val)
        add_pred(as.character(ct$level), p$mean1)
        if (identical(ct$level, lv[[2]])) add_pred(lv[[1]], p$mean0)  # the reference's own, once
      }
    }
  }
  list(ame = dplyr::bind_rows(amel), pred = dplyr::bind_rows(predl))
}

#' @keywords internal
reg_marginal_basis_warn <- function(fit, data, predictors, multiplier, ame, ratio) {
  bv <- reg_basis_vars(fit, predictors)
  if (!length(bv) || is.null(ame) || !nrow(ame)) return(invisible(NULL))
  for (v in bv) {
    if (reg_is_factor_var(data[[v]])) next
    est <- ame$ame[ame$var == v]
    if (length(est) != 1L) next
    k <- if (!is.null(multiplier) && v %in% names(multiplier)) as.numeric(multiplier[[v]]) else 1
    if (reg_marginal_basis_ok(fit, data, v, k, est, ratio)) next
    cli::cli_warn(c(
      "!" = paste0("The marginal effect of {.val {v}} is not trustworthy: it is fitted through a ",
                   "basis expansion ({.code poly()} / {.code ns()}), which the marginal-effects ",
                   "engine re-evaluates on perturbed data."),
      "i" = 'Fit it with {.code shape = c({v} = "quadratic")} instead of a formula basis.'))
  }
  invisible(NULL)
}

# Marginal effects + adjusted predictions on the RESPONSE scale, through `marginaleffects`. `newdata`
# -- the complete-case fitted frame -- is REQUIRED: the package's own data recovery fails past the
# fitting scope and on dropped levels. A single-equation fit has `group = NA`; multinom / polr carry
# the outcome category there.
#   at = "average"   -> averaged over `data`, weighted by `wt`: a population quantity.
#   at = "reference" -> at the reference profile, a single datagrid row: no averaging, no weights.
# `comparison = "lnor"` is the multinomial j-vs-rest contrast (profile only) and `"lnratioavg"` its
# ratio twin; both return a log exp()'d here, so the interval stays a Wald one on the log scale.
reg_marginal_me <- function(fit, data, predictors, conf_level, wt = NULL,
                            at = "average", comparison = NULL, want_pred = TRUE,
                            multiplier = NULL, want_se = TRUE) {
  ref_vals <- if (at == "reference") reg_reference_grid_values(data, predictors) else NULL
  ref_grid <- if (at == "reference")
    do.call(marginaleffects::datagrid, c(list(model = fit), ref_vals)) else NULL
  # weights only at the AVERAGING step; a single-row profile takes none, and `wts = NULL` is
  # rejected.
  wts_arg <- if (at == "reference" || is.null(wt)) list() else list(wts = wt)
  cmp_arg <- if (is.null(comparison)) list() else list(comparison = comparison)
  # WARNING: `comparison` is NULL on the additive default, and `NULL %in% x` is logical(0), not
  # FALSE -- which would make every `if (do_exp)` below error with "argument is of length zero".
  do_exp  <- !is.null(comparison) && comparison %in% c("lnor", "lnratioavg")

  # `variables = list(v = k)` is a k-unit FORWARD DIFFERENCE, not k x the 1-unit AME. ⚠ the KEYWORD
  # is never passed through (see the `multiplier` section).
  var_arg <- function(v) {
    k <- if (!is.null(multiplier) && v %in% names(multiplier)) as.numeric(multiplier[[v]]) else NA_real_
    if (is.finite(k) && k != 1 && !reg_is_factor_var(data[[v]])) stats::setNames(list(k), v) else v
  }
  # the delta-method jacobian costs one re-prediction PER COEFFICIENT, unpaid where the caller
  # discards the interval.
  se_arg <- if (want_se) list() else list(vcov = FALSE)
  amelist <- purrr::map(predictors, function(v) {
    ac <- if (at == "reference")
      as.data.frame(do.call(marginaleffects::comparisons, c(
        list(fit, variables = var_arg(v), newdata = ref_grid, conf_level = conf_level),
        cmp_arg, se_arg)))
    else
      as.data.frame(do.call(marginaleffects::avg_comparisons, c(
        list(fit, variables = var_arg(v), newdata = data, conf_level = conf_level),
        wts_arg, cmp_arg, se_arg)))
    is_fac <- reg_is_factor_var(data[[v]])
    # ⚠ strip the KNOWN prefix and reference suffix off the contrast label rather than splitting on
    # the first " - " or ")": a level containing either ("$20000 - 24999") was truncated and left an
    # NA cell.
    ref_lv <- if (is_fac) levels(forcats::fct_drop(as.factor(data[[v]])))[1] else NA_character_
    level  <- if (!is_fac) v else {
      inner <- if (identical(comparison, "lnor")) "odds" else "mean"
      pre <- if (do_exp) paste0("ln(", inner, "(") else ""
      suf <- if (do_exp) paste0(") / ", inner, "(", ref_lv, "))") else paste0(" - ", ref_lv)
      substr(ac$contrast, nchar(pre) + 1L, nchar(ac$contrast) - nchar(suf))
    }
    grp    <- if ("group" %in% names(ac)) as.character(ac$group) else NA_character_
    est <- ac$estimate
    lo <- ac$conf.low %||% rep(NA_real_, length(est))
    hi <- ac$conf.high %||% rep(NA_real_, length(est))
    pv <- ac$p.value %||% rep(NA_real_, length(est))
    if (do_exp) { est <- exp(est); lo <- exp(lo); hi <- exp(hi) }   # log-ratio -> OR / RR (and its CI)
    tibble::tibble(var = v, level = as.character(level), group = grp,
                   ame = est, ame_lo = lo, ame_hi = hi, ame_p = pv)
  })
  ame <- dplyr::bind_rows(amelist)

  predlist <- if (want_pred) purrr::map(predictors, function(v) {
    if (!reg_is_factor_var(data[[v]])) return(NULL)      # no per-level prediction for numerics
    ap <- if (at == "reference") {
      grid_v <- do.call(marginaleffects::datagrid, c(list(model = fit),
        utils::modifyList(ref_vals, stats::setNames(list(levels(as.factor(data[[v]]))), v))))
      as.data.frame(marginaleffects::predictions(fit, newdata = grid_v, vcov = FALSE))
    } else {
      # the adjusted prediction is the marginal-STANDARDIZED one, which is what COHERES with the AME
      # (adjusted(ref) + AME(level) == adjusted(level)); `by = v` would give the OBSERVED rate.
      as.data.frame(do.call(marginaleffects::avg_predictions, c(
        list(fit, variables = v, newdata = data, vcov = FALSE), wts_arg)))
    }
    grp <- if ("group" %in% names(ap)) as.character(ap$group) else NA_character_
    tibble::tibble(var = v, level = as.character(ap[[v]]), group = grp, pred = ap$estimate)
  }) else list()
  pred <- dplyr::bind_rows(purrr::compact(predlist))

  list(ame = ame, pred = pred)
}

reg_marginal_column <- function(skeleton, marg, model_predictors, shape, var_y,
                                group, color, color_signif, col_var, or_tip = NULL,
                                model_family = "", scale = NULL, trials = NULL) {
  amt <- marg$ame; prd <- marg$pred
  if (!is.na(group)) {
    amt <- amt[!is.na(amt$group) & amt$group == group, , drop = FALSE]
    if (nrow(prd)) prd <- prd[!is.na(prd$group) & prd$group == group, , drop = FALSE]
  }
  m     <- reg_skel_match(skeleton, amt)
  ame_v <- amt$ame[m]; lo_v <- amt$ame_lo[m]; hi_v <- amt$ame_hi[m]; p_v <- amt$ame_p[m]
  pred_v <- if (nrow(prd)) prd$pred[reg_skel_match(skeleton, prd)] else rep(NA_real_, nrow(skeleton))

  n_rows   <- nrow(skeleton)
  in_model <- skeleton$var %in% c("Constant", model_predictors)
  is_const <- skeleton$var == "Constant"
  is_ref   <- skeleton$is_ref & !is_const & in_model
  # in_refrow is the UNION-skeleton row fact (see reg_column); `is_ref` stays in_model-gated below.
  refrows  <- (skeleton$is_ref & !is_const) | is_const

  display <- rep("blank", n_rows)
  show    <- in_model & (!is.na(ame_v) | is_ref)
  if (shape == "prob") {
    display[show] <- "est"
    # ⚠ a SUMMED SCORE's marginal effect is additive on the outcome's own scale, like a gaussian AME,
    # so its reference carries the additive NEUTRAL. A probability-scale AME keeps NA: a
    # percentage-point contrast has no reference value.
    ame_v[is_ref] <- if (identical(scale %||% "points", "raw_diff")) 0 else NA_real_
    # carry the model OR in `or` so the tooltip can surface it although the cell DISPLAYS the AME.
    or_v <- if (is.null(or_tip)) NA_real_ else or_tip
    # ⚠ the SCALE written into is the ESTIMAND's, never the arm's, and the estimate goes in the field
    # that scale declares -- on a summed score `raw_diff`, converted from the per-item contrast by
    # `trials` (exact), beside the mean score as its level.
    sc <- scale %||% "points"
    if (identical(sc, "raw_diff") && !is.na(trials %||% NA)) {
      k <- as.numeric(trials); ame_v <- ame_v * k; lo_v <- lo_v * k; hi_v <- hi_v * k
    }
    do.call(fmt, c(
      stats::setNames(list(pred_v), EST_SCALES[[sc]]$base_display),
      list(
        n = rep(NA_integer_, n_rows),   # the level's own count is stamped by the spec builder
        diff = ame_v, or = or_v, ci_inf = lo_v, ci_sup = hi_v, pvalue = p_v,
        scale = sc, pct_type = reg_pct_type(sc), display = display,
        digits = reg_cell_digits(sc), ci_method = "wald",
        color = color, color_signif = color_signif, col_var = col_var,
        comp_all = FALSE, in_refrow = refrows, model_family = model_family, role = "model"),
      if (identical(EST_SCALES[[sc]]$sd_from %||% "", "var")) list(var = rep(var_y, n_rows))))
  } else if (shape == "prob_ratio") {
    # the RATIO twin of "prob", coherent BY CONSTRUCTION: marginal standardization gives
    # adjusted(ref) x RR(level) == adjusted(level). The reference cell keeps the FULL template.
    display[show] <- "est"
    ame_v[is_ref] <- 1                                         # multiplicative neutral at the reference
    sc <- scale %||% "pct_ratio"
    do.call(fmt, c(
      stats::setNames(list(ame_v), EST_SCALES[[sc]]$est_field),
      stats::setNames(list(pred_v), EST_SCALES[[sc]]$base_display),
      list(
        n = rep(NA_integer_, n_rows),   # the level's own count is stamped by the spec builder
        ci_inf = lo_v, ci_sup = hi_v, pvalue = p_v,
        scale = sc, pct_type = reg_pct_type(sc), display = display,
        digits = reg_cell_digits(sc), ref = "1", ci_method = "wald_log",
        color = color, color_signif = color_signif, col_var = col_var,
        comp_all = FALSE, in_refrow = refrows, model_family = model_family, role = "model")))
  } else if (shape == "raw_ratio") {
    display[show] <- "est"
    ame_v[is_ref] <- 1                                         # multiplicative neutral at the reference
    fmt(
      n = rep(NA_integer_, n_rows),
      ratio = ame_v, ci_inf = lo_v, ci_sup = hi_v, pvalue = p_v,
      scale = "mean_ratio", display = display, digits = reg_cell_digits("mean_ratio"), ref = "1", ci_method = "wald_log",
      color = color, color_signif = color_signif, col_var = col_var,
      comp_all = FALSE, in_refrow = refrows, model_family = model_family, role = "model"
    )
  } else if (shape == "or") {                                  # MNL "j vs rest" OR at the profile
    display[show] <- "est"
    ame_v[is_ref] <- 1                                         # multiplicative neutral at the reference
    fmt(
      n = rep(NA_integer_, n_rows),   # the level's own count is stamped by the spec builder
      or = ame_v, ci_inf = lo_v, ci_sup = hi_v, pvalue = p_v,
      scale = "odds_ratio", pct_type = reg_pct_type("odds_ratio"), display = display, digits = reg_cell_digits("odds_ratio"), ref = "1",
      ci_method = "wald_log",
      color = color, color_signif = color_signif, col_var = col_var,
      comp_all = FALSE, in_refrow = refrows, model_family = model_family, role = "model"
    )
  } else {                                                     # "raw" (gaussian / poisson)
    display[show] <- "est"
    ame_v[is_ref] <- 0                                         # additive neutral at the reference
    fmt(
      n = rep(NA_integer_, n_rows),   # the level's own count is stamped by the spec builder
      diff = ame_v, ci_inf = lo_v, ci_sup = hi_v, pvalue = p_v,
      var = rep(var_y, n_rows),                               # var(Y): standardizes the effect-size colour
      scale = "raw_diff", display = display, digits = reg_cell_digits("raw_diff"), ci_method = "wald",
      color = color, color_signif = color_signif, col_var = col_var,
      comp_all = FALSE, in_refrow = refrows, model_family = model_family, role = "model"
    )
  }
}

reg_columns_multinom <- function(skeleton, f, sp, est, color, color_signif,
                                 cleannames, prefix_dep, col_var, model_family = "multinomial",
                                 method = "wald") {
  y_ref <- reg_cleanup(f$y_ref, cleannames)
  purrr::map(f$y_levels, function(j) {
    sub      <- f
    sub$tidy <- f$tidy[f$tidy$y.level == j,
                       setdiff(names(f$tidy), "y.level"), drop = FALSE]
    jc  <- reg_cleanup(j, cleannames)
    lab <- paste0(if (prefix_dep) paste0(sp$outcome, " - ") else "",
                  jc, " vs ", y_ref)
    list(label = lab, emp_key = j,   # emp_key: raw category, for the empirical tooltip
         col   = reg_column(skeleton, sub, sp$predictors, col_var, est, color, color_signif,
                            model_family = model_family, method = method))
  })
}

# === SECTION: Model-summary footer -- GOF stats stored in the `test` attribute ===================
# The regression GOF lives in the SAME whole-table `test` tibble crosstabs use, adding ROWS whose
# discriminators never collide with the crosstab's -- so each renderer auto-no-ops on the other's
# table. A value stat carries its number in `statistic` (pvalue NA), a test stat statistic + df + p;
# `col_var` is the model's FIRST output column. The footer is DISPLAY-ONLY.

reg_null_loglik <- function(fit, family) {
  # the deviance-based LR null needs a KNOWN dispersion (the deviance is then the likelihood).
  if (reg_fam_disp_known(family) &&
      !is.null(fit$null.deviance) && !is.null(fit$deviance)) {
    ll_f <- tryCatch(as.numeric(stats::logLik(fit)), error = function(e) NA_real_)
    lr   <- fit$null.deviance - fit$deviance
    df   <- fit$df.null - fit$df.residual
    return(list(ll_f = ll_f, ll_0 = ll_f - lr / 2, df = df))
  }
  null <- tryCatch({
    mf   <- stats::model.frame(fit)
    fla  <- stats::reformulate("1", response = names(mf)[1])
    if (inherits(fit, "multinom")) nnet::multinom(fla, data = mf, trace = FALSE)
    else if (inherits(fit, "polr")) MASS::polr(fla, data = mf, Hess = TRUE)
    else NULL
  }, error = function(e) NULL)
  if (is.null(null)) return(NULL)
  llf <- tryCatch(stats::logLik(fit),  error = function(e) NULL)
  ll0 <- tryCatch(stats::logLik(null), error = function(e) NULL)
  if (is.null(llf) || is.null(ll0)) return(NULL)
  list(ll_f = as.numeric(llf), ll_0 = as.numeric(ll0),
       df = attr(llf, "df") - attr(ll0, "df"))
}

# Pearson dispersion: poisson / grouped binomial only -- not identifiable for ungrouped Bernoulli
# data. PURE: the over-dispersion warning belongs to reg_fit(), where the SEs are actually scaled.
# ⚠ the denominator is n - rank, computed HERE, NEVER stats::df.residual(fit): for an svyglm that is
# the DESIGN degrees of freedom, which puts a weighted dispersion off by an order of magnitude.
reg_dispersion <- function(fit) {
  rp  <- tryCatch(stats::residuals(fit, type = "pearson"), error = function(e) NULL)
  if (is.null(rp)) return(NA_real_)
  n    <- sum(is.finite(rp))
  rank <- tryCatch(sum(!is.na(stats::coef(fit))), error = function(e) NA_integer_)
  if (is.na(rank)) return(NA_real_)
  dfr <- n - rank
  if (dfr <= 0) return(NA_real_)
  sum(rp^2, na.rm = TRUE) / dfr
}

reg_aic_value <- function(fit) {
  a <- tryCatch(suppressWarnings(stats::AIC(fit)), error = function(e) NA_real_)
  if (length(a) > 1L && !is.null(names(a)) && "AIC" %in% names(a)) return(as.numeric(a[["AIC"]]))
  as.numeric(a)[1]
}

# GOF stats for ONE fit. quasi* / svyglm have no true likelihood, so those stats stay NA or become a
# relabelled Rao-Scott Wald -- never a false LR.
reg_glance <- function(fit, family, grouped, weighted, nobs) {
  row <- function(test, statistic = NA_real_, df1 = NA_real_, df2 = NA_real_, pvalue = NA_real_)
    tibble::tibble(test = test, statistic = statistic, df1 = df1, df2 = df2, pvalue = pvalue)
  out <- row("n", statistic = as.numeric(nobs))
  wald_null_row <- function(fit) {
    terms_all <- tryCatch(attr(stats::terms(fit), "term.labels"), error = function(e) character(0))
    wt <- if (length(terms_all) > 0)
      tryCatch(suppressWarnings(survey::regTermTest(fit, stats::reformulate(terms_all))),
               error = function(e) NULL)
    else NULL
    if (is.null(wt)) return(NULL)
    row("wald_null", statistic = as.numeric(wt$Ftest), df1 = as.numeric(wt$df),
        df2 = as.numeric(wt$ddf), pvalue = as.numeric(wt$p))
  }

  # a fit that reaches a MEASURE through a misspecified likelihood has no AIC / BIC / McFadden, and a
  # 0/1 Pearson dispersion is a constant, not a diagnostic. Placed FIRST so it holds weighted or not.
  if (family %in% REG_FIT_ONLY_FAMILIES) {
    out <- dplyr::bind_rows(out, wald_null_row(fit))
    return(out)
  }

  if (weighted) {
    out <- dplyr::bind_rows(out, wald_null_row(fit))
    nk <- tryCatch(suppressWarnings(as.numeric(survey::psrsq(fit, method = "Nagelkerke"))),
                   error = function(e) NA_real_)
    if (!is.na(nk)) out <- dplyr::bind_rows(out, row("nagelkerke_r2", statistic = nk))
    cs <- tryCatch(suppressWarnings(as.numeric(survey::psrsq(fit, method = "Cox-Snell"))),
                   error = function(e) NA_real_)                # selectable via stats=; not in the default set
    if (!is.na(cs)) out <- dplyr::bind_rows(out, row("cox_snell_r2", statistic = cs))
    aic <- reg_aic_value(fit)
    if (!is.na(aic)) out <- dplyr::bind_rows(out, row("aic", statistic = aic))
    return(out)
  }

  if (family == "gaussian") {
    g <- tryCatch(broom::glance(fit), error = function(e) NULL)
    if (!is.null(g)) out <- dplyr::bind_rows(out,
      row("r2",      statistic = g$r.squared),
      row("r2_adj",  statistic = g$adj.r.squared),
      row("f_model", statistic = g$statistic, df1 = g$df, df2 = g$df.residual, pvalue = g$p.value),
      row("sigma",   statistic = g$sigma),
      row("aic",     statistic = g$AIC),
      row("bic",     statistic = g$BIC))
    return(out)
  }

  nl <- reg_null_loglik(fit, family)
  if (!is.null(nl) && !is.na(nl$ll_f) && !is.na(nl$ll_0) && !is.na(nl$df) && nl$df > 0) {
    lr <- 2 * (nl$ll_f - nl$ll_0)
    out <- dplyr::bind_rows(out,
      row("lr_null", statistic = lr, df1 = nl$df, pvalue = stats::pchisq(lr, nl$df, lower.tail = FALSE)),
      row("mcfadden_r2", statistic = 1 - nl$ll_f / nl$ll_0))
  }
  aic <- reg_aic_value(fit)
  bic <- tryCatch(as.numeric(stats::BIC(fit)), error = function(e) NA_real_)
  if (!is.na(aic)) out <- dplyr::bind_rows(out, row("aic", statistic = aic))
  if (!is.na(bic)) out <- dplyr::bind_rows(out, row("bic", statistic = bic))
  if (reg_fam_overdispersed(family, grouped)) {
    phi <- reg_dispersion(fit)
    if (!is.na(phi)) out <- dplyr::bind_rows(out, row("phi", statistic = phi))
  }
  out
}

# THE `stats =` / `check =` vocabulary in one place, DERIVED from TEST_ROWS' `glance` block, i.e. the
# rows reg_glance() emits. ⚠ its ORDER is TEST_ROWS' display order.
#' @keywords internal
REG_GOF_KEYS <- .trow_keys(.trow_chr("block") == "glance")

# ⚠ THE UNION IS MANDATORY: `unique(TEST_ROWS$stat)` alone silently DROPS `residuals` and
# `normality`, both legal `check =` values with a panel and, deliberately, no test row -- so they
# have no `stat` here to be derived from.
#' @keywords internal
reg_stat_keys <- function() unique(c(.trow_chr("stat")[.trow_chr("producer") == "reg"],
                                     names(REG_CHECKS)))

# === SECTION: `stats =` is ONE argument ==========================================================
# `stats` says WHAT RIDES THE MODEL-SUMMARY FOOTER, the model comparison included. An element is
# always a KEY -- carried in the NAME when it has a parameter, in the VALUE when it does not, which
# is `ref = c(var = "level")`'s grammar one subsystem over:
#
#   stats = c("n", "aic", "compare_sequential")     each model vs the previous one
#   stats = "compare_baseline"                      each model vs the FIRST
#   stats = c("n", compare_baseline = "M1")         ... vs the model labelled "M1"
#   stats = c("n", compare_baseline = 2)            ... vs the 2nd column
#
# The boundary validates KEYS; the resolver splits them into the plain triple every producer speaks.

#' @keywords internal
#' @noRd
reg_stats_keys_of <- function(stats) {
  if (!is.character(stats) || !length(stats)) return(character(0))
  nm <- names(stats)
  if (is.null(nm)) unname(stats) else ifelse(nzchar(nm), nm, unname(stats))
}

#' @keywords internal
#' @noRd
reg_resolve_stats <- function(stats) {
  none <- list(stats = stats, compare = "none", baseline = NULL)
  # ⚠ FALSE / "none" hides the comparison too: one argument means one list of what the footer shows.
  if (!is.character(stats) || !length(stats) ||
      identical(stats, "all") || identical(stats, "none")) return(none)

  keys  <- reg_stats_keys_of(stats)
  is_cmp <- keys %in% c("compare_baseline", "compare_sequential")
  if (!any(is_cmp)) return(none)
  cmp <- keys[is_cmp]
  if (length(cmp) > 1L)
    cli::cli_abort(c("{.arg stats} names more than one model comparison: {.val {cmp}}.",
                     "i" = "A footer row compares each model to ONE other, so pick one.",
                     call = NULL))

  val <- unname(stats[is_cmp])
  bl  <- if (nzchar(names(stats)[is_cmp] %||% "") && !is.na(val) && nzchar(val)) val else NULL
  if (identical(cmp, "compare_sequential") && !is.null(bl))
    cli::cli_abort(c('{.code stats = c(compare_sequential = {.val {bl}})} names a baseline model.',
                     "x" = "A sequential comparison has none: each model is tested against the previous one.",
                     "i" = 'Did you mean {.code stats = c(compare_baseline = "{bl}")}?'), call = NULL)
  if (!is.null(bl) && grepl("^[0-9]+$", bl)) bl <- as.numeric(bl)

  # ⚠ A comparison key RESTRICTS NOTHING: `stats = "compare_baseline"` asks for a comparison, not
  # for a footer holding only that -- so when the comparison keys are all that was named, what is
  # left is NULL, i.e. the per-family default set. Hiding the whole footer is still `stats = FALSE`
  # / `"none"`.
  rest <- unname(stats[!is_cmp])
  list(stats    = if (!length(rest)) NULL else rest,
       compare  = if (identical(cmp, "compare_sequential")) "sequential" else "baseline",
       baseline = bl)
}

#' @keywords internal
reg_validate_stat_keys <- function(x, arg = "stats", allowed = reg_stat_keys()) {
  bad <- setdiff(x, allowed)
  if (length(bad))
    cli::cli_abort(c("{.arg {arg}} must name model-fit statistics or checks.",
                     "x" = "Unknown: {.val {bad}}.",
                     "i" = "Available: {.val {allowed}}."))
  x
}

# Resolve `stats =` -> the ordered footer discriminators. NULL / TRUE = the per-context default set;
# "all" = every statistic AND check, fit-based ones included; FALSE / "none" = no footer.
reg_footer_stats <- function(family, weighted, grouped, stats) {
  default <- if (family %in% REG_FIT_ONLY_FAMILIES) c("n", "wald_null")
    else if (weighted) c("n", "wald_null", "nagelkerke_r2", "aic")
    else if (family == "gaussian") c("n", "r2", "r2_adj", "f_model", "sigma")
    else { s <- c("n", "lr_null", "mcfadden_r2", "aic", "bic")
           # `phi` is the EXACT Pearson dispersion; the key `dispersion` names the CHECK.
           if (reg_fam_overdispersed(family, grouped)) s <- c(s, "phi"); s }
  # the per-predictor global test is in the DEFAULT set -- the question a multi-level factor block
  # leaves unanswered -- and so are the checks that cost no fit.
  checks  <- reg_checks_for(family, weighted)
  default <- c(default, "global", reg_checks_default(family, weighted))
  if (identical(stats, "all")) return(reg_check_expand(unique(c(default, checks))))
  if (is.null(stats) || isTRUE(stats)) return(reg_check_expand(default))
  if (isFALSE(stats) || identical(stats, "none")) return(character(0))
  reg_check_expand(stats[stats %in% reg_stat_keys()])
}

reg_gof_rows <- function(f, sp, col_var, weighted, grouped, stats) {
  keep <- reg_footer_stats(sp$fit_family, weighted, isTRUE(grouped), stats)
  if (length(keep) == 0) return(NULL)                        # stats = FALSE -> no glance, no warnings
  g <- if (!is.null(f$glance)) f$glance
       else reg_glance(f$fit, sp$fit_family, isTRUE(grouped), weighted, f$nobs)
  g <- g[g$test %in% keep, , drop = FALSE]
  g <- g[order(match(g$test, keep)), , drop = FALSE]           # spec order
  if (nrow(g) == 0) return(NULL)
  reg_test_row(g$test, col_var, statistic = g$statistic, df1 = g$df1, df2 = g$df2,
               pvalue = g$pvalue, nobs = as.numeric(f$nobs), outcome = sp$outcome)
}

# === SECTION: Multi-model comparison -- each model column vs a baseline / the previous one ========
# An LR / F test between two models is only valid on the SAME complete-case set and when one nests in
# the other; a guard failure falls back to Delta-AIC. Nesting is checked in BOTH directions, a
# baseline being allowed to be the SUPERSET.
reg_compare_guard <- function(m_ref, m_full) {
  ok_n   <- tryCatch(stats::nobs(m_ref) == stats::nobs(m_full), error = function(e) FALSE)
  t_ref  <- tryCatch(attr(stats::terms(m_ref),  "term.labels"), error = function(e) NULL)
  t_full <- tryCatch(attr(stats::terms(m_full), "term.labels"), error = function(e) NULL)
  if (is.null(t_ref) || is.null(t_full) || !isTRUE(ok_n)) return(0L)
  if (all(t_ref %in% t_full)) return(1L)                  # ref nested in full
  if (all(t_full %in% t_ref)) return(-1L)                 # full nested in ref (superset baseline)
  0L
}

reg_order_union <- function(models) {
  sets  <- purrr::map(models, unique)
  all_u <- unique(purrr::flatten_chr(sets))
  complete_i <- which(purrr::map_lgl(sets, function(s) all(all_u %in% s)))
  if (length(complete_i) > 0L) unique(sets[[complete_i[length(complete_i)]]]) else all_u
}

reg_compare_extract <- function(an, use_f) {
  k     <- nrow(an)
  p_col <- grep("^Pr\\(", names(an), value = TRUE)
  p     <- if (length(p_col)) suppressWarnings(as.numeric(an[[p_col[1]]][k])) else NA_real_
  df1   <- suppressWarnings(abs(as.numeric(an[["Df"]][k])))
  if (use_f) list(stat = suppressWarnings(as.numeric(an[["F"]][k])), df1 = df1,
                  df2 = suppressWarnings(as.numeric(an[["Res.Df"]][k])), p = p)
  else       list(stat = suppressWarnings(as.numeric(an[["Deviance"]][k])), df1 = df1,
                  df2 = NA_real_, p = p)
}

# One comparison row per model column: LR for binomial / poisson / multinomial / ordinal, F for
# gaussian / quasi, a design-based Wald for a WEIGHTED (or "rr") model so no false-LR claim is made.
# Distinct discriminators per test kind keep each row homogeneous, so its label alone names the test.
reg_compare_rows <- function(reg_gof, fits, specs, family, weighted, fit_first_col,
                             compare = "none", baseline = NULL) {
  if (identical(compare, "none")) return(reg_gof)
  n <- length(fits)
  if (n < 2L) {
    cli::cli_inform(c("i" = paste0("{.arg compare} needs at least two models (a {.arg predictors} list ",
                                   "or several outcomes); ignored.")))
    return(reg_gof)
  }
  use_f  <- reg_fam_disp_estimated(family)
  # an "rr" fit is an svyglm, so it takes the Wald branch either way: an LR between two
  # quasi-likelihood fits would be a false LR.
  use_wald <- reg_fam_svy_fitted(family, weighted)
  base_i <- if (compare == "baseline") {
    if (is.null(baseline))          1L
    else if (is.numeric(baseline))  as.integer(baseline)
    else                            match(baseline, purrr::map_chr(specs, "label"))
  } else NA_integer_
  if (compare == "baseline" && (is.na(base_i) || base_i < 1L || base_i > n)) {
    cli::cli_warn("{.arg baseline} {.val {baseline}} matches no model; using the first.")
    base_i <- 1L
  }

  cmp_outcome <- specs[[1]]$outcome
  row <- function(test, col_var, statistic = NA_real_, df1 = NA_real_, df2 = NA_real_,
                  pvalue = NA_real_, nobs = NA_real_)
    reg_test_row(test, col_var, statistic = statistic, df1 = df1, df2 = df2,
                 pvalue = pvalue, nobs = nobs, outcome = cmp_outcome)

  stat_key <- if (compare == "sequential") "compare_sequential" else "compare_baseline"
  rows <- purrr::map(seq_len(n), function(i) {
    ref_i <- if (compare == "sequential") i - 1L else base_i
    if (is.na(ref_i) || ref_i < 1L || ref_i == i) return(NULL)
    m_full <- fits[[i]]$fit; m_ref <- fits[[ref_i]]$fit
    col    <- fit_first_col[[i]]
    dir  <- reg_compare_guard(m_ref, m_full)
    m_lo <- if (dir >= 0L) m_ref  else m_full
    m_hi <- if (dir >= 0L) m_full else m_ref
    if (dir != 0L) {
      if (use_wald) {
        e <- tryCatch({
          an <- stats::anova(m_lo, m_hi, method = "Wald", test = "F")
          list(stat = as.numeric(an$Ftest), df1 = as.numeric(an$df),
               df2 = as.numeric(an$ddf), p = as.numeric(an$p))
        }, error = function(e) NULL)
        if (!is.null(e) && !is.na(e$p)) {
          return(row(test_row_key(stat_key, "wald"), col, statistic = e$stat, df1 = e$df1,
                     df2 = e$df2, pvalue = e$p, nobs = fits[[i]]$nobs))
        }
      } else {
        an <- tryCatch(stats::anova(m_lo, m_hi, test = if (use_f) "F" else "Chisq"),
                       error = function(e) NULL)
        if (!is.null(an)) {
          e <- reg_compare_extract(an, use_f)
          if (!is.na(e$p)) {
            disc <- test_row_key(stat_key, if (use_f) "f" else "lr")
            return(row(disc, col, statistic = e$stat, df1 = e$df1, df2 = e$df2, pvalue = e$p,
                       nobs = fits[[i]]$nobs))
          }
        }
      }
    }
    daic <- tryCatch(reg_aic_value(m_full) - reg_aic_value(m_ref), error = function(e) NA_real_)
    cli::cli_inform(c(
      "i" = paste0(
        "Column {.val {col}}: models are not nested or N differs -> showing the AIC difference vs the ",
        "{if (compare == 'sequential') 'previous' else 'baseline'} model instead of a likelihood-ratio test."),
      "i" = 'A different N is usually the per-model missing-value drop; set {.code na = "drop_all"} to fit every model on the same complete cases so the likelihood-ratio test can run.'))
    row(test_row_key(stat_key, "aic"), col, statistic = daic, nobs = fits[[i]]$nobs)
  })
  rows <- purrr::compact(rows)
  if (length(rows) == 0) return(reg_gof)
  dplyr::bind_rows(reg_gof, dplyr::bind_rows(rows))
}


# === SECTION: The aggregated effect-modification test (predictor x tab_vars) =====================
# The per-cell `between_groups` colour says how big each group difference is, one cell at a time;
# this says ONCE per predictor whether its effect differs between groups at all -- aggregated, so no
# multiplicity inflation. ONE extra pooled fit `y ~ (predictors) * g`, then drop1() per predictor or
# survey::regTermTest(), the same LR-F-vs-Wald split the model comparison uses.
# DESIGN: these rows are deliberately ABSENT from reg_footer_spec(). A footer ROW is keyed to exactly
# one model column and a pooled test belongs to none, so they stay pure data, rendered as a
# table-wide footer STREAM.

#' @keywords internal
reg_test_row <- function(test, col, var = "", statistic = NA_real_, df1 = NA_real_, df2 = NA_real_,
                         pvalue = NA_real_, nobs = NA_real_, outcome = NA_character_)
  tibble::tibble(var = var, col = col, test = test, statistic = statistic,
                 df1 = df1, df2 = df2, pvalue = pvalue, n = nobs, min_e = NA_real_, outcome = outcome)

#' @keywords internal
reg_interaction_types <- function() unname(test_row_types("interaction"))

# ⚠ THE FOURTH FITTING SITE, and the one that cannot join a per-spec product: it fits the POOLED
# model -- every tab_vars group at once, with the group interacted -- so it lives AFTER the split
# barrier. A per-spec builder runs inside ONE group and can never see the others: not a missed
# parallel axis.
#' @keywords internal
reg_interaction_rows <- function(reg_gof, data, specs, shared, tab_vars, fit_first_col) {
  weighted <- shared$weighted
  rows <- purrr::map(seq_along(specs), function(i) {
    sp <- specs[[i]]
    # No pooled interaction for the engines that are not one glm equation, nor for a compound
    # formula: degrade to no row, never a wrong one.
    if (!reg_fam_glm(sp$fit_family) || isTRUE(sp$compound)) return(NULL)
    preds <- sp$predictors
    if (length(preds) == 0L) return(NULL)
    f <- tryCatch(reg_fit(data, sp$outcome, preds, sp$fit_family, shared$design_spec, isTRUE(sp$est$exp),
                          reg_outcome_level_of(sp$outcome_level) %||% shared$outcome_level,
                          shared$conf_level, "wald", trials = sp$trials, formula = NULL,
                          multiplier = NULL, cross = tab_vars),
                  error = function(e) NULL)
    if (is.null(f) || is.null(f$fit)) return(NULL)
    fit      <- f$fit
    use_f    <- reg_fam_disp_estimated(sp$fit_family)
    use_wald <- reg_fam_svy_fitted(sp$fit_family, weighted)
    # WARNING: take the interaction terms from the FIT's own term.labels, verbatim -- terms() orders
    # the parts of an interaction by formula position, so a hand-built "age:party3" comes back as
    # "party3:age" and drop1() then rejects the scope.
    have  <- tryCatch(attr(stats::terms(fit), "term.labels"), error = function(e) character(0))
    inter <- have[grepl(":", have, fixed = TRUE)]
    keyed <- vapply(inter, function(tl) {
      parts <- gsub("`", "", strsplit(tl, ":", fixed = TRUE)[[1]], fixed = TRUE)
      if (length(parts) == 2L && tab_vars %in% parts) setdiff(parts, tab_vars)[1] else NA_character_
    }, character(1), USE.NAMES = FALSE)
    ok      <- !is.na(keyed) & keyed %in% preds
    terms_i <- inter[ok]
    keep    <- keyed[ok]

    reg_term_tests(fit, keep, terms_i, use_f, use_wald,
                   types = test_row_types("interaction"),
                   col_var = fit_first_col[[i]], nobs = f$nobs, outcome = sp$outcome)
  })
  rows <- purrr::compact(purrr::flatten(purrr::compact(rows)))
  if (length(rows) == 0) return(reg_gof)
  dplyr::bind_rows(reg_gof, dplyr::bind_rows(rows))
}

#' @keywords internal
reg_term_tests <- function(fit, preds, terms, use_f, use_wald, types, col_var, nobs,
                           outcome = NA_character_) {
  if (length(terms) == 0L) return(NULL)
  if (use_wald) {
    return(purrr::map2(preds, terms, function(pv, tm) {
      e <- tryCatch({
        rt <- suppressWarnings(survey::regTermTest(fit, tm))
        list(stat = as.numeric(rt$Ftest), df1 = as.numeric(rt$df),
             df2 = as.numeric(rt$ddf), p = as.numeric(rt$p))
      }, error = function(e) NULL)
      if (is.null(e) || is.na(e$p)) return(NULL)
      reg_test_row(types[["wald"]], col_var, pv, e$stat, e$df1, e$df2, e$p, nobs, outcome = outcome)
    }))
  }
  # WARNING: capture.output, not just suppressMessages -- nnet's drop1.multinom PRINTS its progress
  # with cat(), which no condition handler catches.
  d1 <- tryCatch({
    utils::capture.output(
      res <- suppressWarnings(stats::drop1(fit, scope = terms, test = if (use_f) "F" else "Chisq")))
    res
  }, error = function(e) NULL)
  if (is.null(d1)) return(NULL)
  p_col <- grep("^Pr\\(", names(d1), value = TRUE)
  if (!length(p_col)) return(NULL)
  m <- match(terms, rownames(d1))
  purrr::map(seq_along(preds), function(k) {
    j <- m[[k]]
    if (is.na(j)) return(NULL)
    p <- suppressWarnings(as.numeric(d1[[p_col[1]]][j]))
    if (is.na(p)) return(NULL)
    stat <- suppressWarnings(as.numeric(d1[[if (use_f) "F value" else "LRT"]][j]))
    reg_test_row(types[[if (use_f) "f" else "lr"]], col_var, preds[[k]],
        stat, suppressWarnings(as.numeric(d1[["Df"]][j])),
        if (use_f) suppressWarnings(as.numeric(stats::df.residual(fit))) else NA_real_,
        p, nobs, outcome = outcome)
  })
}

# The per-predictor GLOBAL test -- "is this variable associated with the outcome at all?", the
# answer a block of stars against a reference category cannot give. Emitted only for terms carrying
# 2+ coefficients: a 1-df term's global p IS the single cell's p. ⚠ IT DOES REFIT (the unweighted
# drop1() arm), and that is a DECLARED KEEP: the only cheaper route is a Wald test, which is a
# DIFFERENT NUMBER, and this is a test a reader will quote.
#' @keywords internal
reg_global_types <- function() unname(test_row_types("global"))

#' @keywords internal
reg_global_rows <- function(f, sp, shared, col_var) {
  if (!reg_fam_glm(sp$fit_family) || isTRUE(sp$compound)) return(NULL)
  if (is.null(f) || is.null(f$fit)) return(NULL)              # the jamovi digest path keeps no fit
  fit  <- f$fit
  have <- tryCatch(attr(stats::terms(fit), "term.labels"), error = function(e) character(0))
  asg  <- tryCatch(stats::coef(fit), error = function(e) NULL)
  if (is.null(asg)) return(NULL)
  df_of <- tryCatch({
    a <- attr(stats::model.matrix(fit), "assign")
    vapply(seq_along(have), function(k) sum(a == k), integer(1))
  }, error = function(e) rep(NA_integer_, length(have)))
  keep    <- have %in% sp$predictors & !is.na(df_of) & df_of >= 2L
  terms_i <- have[keep]
  if (length(terms_i) == 0L) return(NULL)
  rows <- purrr::compact(reg_term_tests(fit, terms_i, terms_i,
                                        use_f = reg_fam_disp_estimated(sp$fit_family),
                                        use_wald = reg_fam_svy_fitted(sp$fit_family, shared$weighted),
                                        types = test_row_types("global"),
                                        col_var = col_var, nobs = f$nobs, outcome = sp$outcome))
  if (length(rows) == 0) return(NULL)
  dplyr::bind_rows(rows)
}


# === SECTION: jamovi live-UI fit cache -- digest + reference reparametrization ===================
# A factor-reference change is a LINEAR REPARAMETRIZATION of the SAME fit (likelihood, fitted values
# and dispersion all invariant), so the whole table at any reference is recomputable from the
# coefficients + covariance with NO refit. reg_build_digest() fits ONCE at the canonical
# (natural-first-level) reference and returns a small, reference-INDEPENDENT digest, discarding the
# raw fit; reg_reref_fit_res() reparametrizes it to any display reference, a drop-in for
# reg_column() / reg_gof_rows(). Reached ONLY with `.fit_cache` present, on the single-equation GLM
# coefficient path, and locked byte-identical to a real refit by a test.

reg_wald_crit <- function(disp_known, df_residual, conf_level) {
  if (disp_known) stats::qnorm(1 - (1 - conf_level) / 2)
  else            stats::qt(1 - (1 - conf_level) / 2, df = df_residual)
}

# reg_fit() de-orders factor predictors and drops NA rows deterministically, so the canonical basis
# does not depend on `reference`. Only kilobytes are kept -- never the model object.
reg_build_digest <- function(data, sp, family, design_spec, do_exp, outcome_level,
                             conf_level, weighted, multiplier = NULL) {
  f   <- reg_fit(data, sp$outcome, sp$predictors, family, design_spec, do_exp,
                 outcome_level, conf_level, method = "wald",
                 trials = sp$trials, formula = sp$formula, multiplier = NULL)
  fit <- f$fit
  coef_v <- stats::coef(fit)
  V      <- stats::vcov(fit)
  names(coef_v) <- stringi::stri_replace_all_regex(names(coef_v), "`", "")   # match skeleton terms (as reg_fit does)
  dn <- stringi::stri_replace_all_regex(rownames(V), "`", "")
  dimnames(V) <- list(dn, dn)
  grouped    <- reg_is_grouped_binomial(family, sp$trials, !is.null(sp$formula))
  over_disp  <- !weighted && reg_fam_overdispersed(family, grouped)
  phi        <- if (over_disp) reg_dispersion(fit) else NA_real_
  scaled     <- over_disp && !is.na(phi) && phi > 0
  disp_known <- !weighted && reg_fam_disp_known(family) && !scaled
  # DESIGN: the adjusted predictions and the numeric slopes travel WITH the digest, computed while
  # the fitted object still exists: a counterfactual sweep is not a reparametrization, so it cannot
  # be recovered from `coef` later. They ARE reference-invariant, which keeps a reference change a
  # cache HIT. ⚠ they are NOT multiplier-invariant -- a k-unit contrast on a non-identity link is
  # not k times the one-unit one -- so `multiplier` is part of the digest KEY.
  marg <- reg_fill_sweep(fit, f$data, sp$predictors, conf_level, design_spec$wt, multiplier)
  list(coef = coef_v, vcov = V, df_residual = stats::df.residual(fit),
       phi = phi, scaled = scaled, disp_known = disp_known, do_exp = do_exp,
       var_y = f$var_y, positive_level = f$positive_level, nobs = f$nobs, marg = marg,
       glance = reg_glance(fit, family, grouped, weighted, f$nobs), family = family)
}

# Reparametrize a canonical digest to the DISPLAY reference encoded in `skeleton`: each display term
# is a linear contrast L over the canonical coefficients, so estimate = L'b and se = sqrt(L' V L),
# then the SAME Wald finalize reg_fit() uses.
reg_reref_fit_res <- function(digest, skeleton, conf_level, multiplier = NULL) {
  coef_v <- digest$coef
  V      <- digest$vcov
  cn     <- names(coef_v)
  preds  <- setdiff(unique(skeleton$var), "Constant")
  ref_of <- stats::setNames(vapply(preds, function(p) {
    r <- skeleton$level[skeleton$var == p & skeleton$is_ref]
    if (length(r)) as.character(r[[1]]) else NA_character_
  }, character(1)), preds)

  rows <- skeleton[!is.na(skeleton$term), , drop = FALSE]        # (Intercept) + non-reference terms
  n    <- nrow(rows)
  est  <- numeric(n); se <- numeric(n)
  for (i in seq_len(n)) {
    p <- rows$var[i]; t <- rows$term[i]
    L <- stats::setNames(numeric(length(cn)), cn)
    if (identical(t, "(Intercept)")) {
      if ("(Intercept)" %in% cn) L["(Intercept)"] <- 1
      for (pp in preds) {
        if (is.na(ref_of[[pp]])) next
        rn <- paste0(pp, ref_of[[pp]])
        if (rn %in% cn) L[rn] <- L[rn] + 1
      }
    } else if (p %in% preds && !is.na(ref_of[[p]])) {            # factor level j vs display ref r_p
      rn <- paste0(p, ref_of[[p]])
      if (t  %in% cn) L[t]  <- L[t]  + 1
      if (rn %in% cn) L[rn] <- L[rn] - 1
    } else if (t %in% cn) {                                      # numeric predictor: identity
      L[t] <- 1
    }
    est[i] <- sum(L * coef_v)
    se[i]  <- sqrt(as.numeric(t(L) %*% V %*% L))
  }
  # applied with reg_fit()'s OWN expressions in its OWN order, so the reref stays byte-identical by
  # construction: folding k into the contrast gives sqrt(k^2 V) where reg_fit gives |k| sqrt(V) --
  # equal in exact arithmetic, not in IEEE754.
  if (!is.null(multiplier)) {
    mult_vec <- rep(1, n)
    for (v in names(multiplier)) {
      mi <- !is.na(rows$term) & rows$term == v
      if (any(mi)) mult_vec[mi] <- as.numeric(multiplier[[v]])
    }
    est <- est * mult_vec
    se  <- se  * abs(mult_vec)
  }

  if (isTRUE(digest$scaled)) se <- se * sqrt(digest$phi)                 # caller pre-scales the SE
  crit <- reg_wald_crit(digest$disp_known, digest$df_residual, conf_level)
  res  <- reg_wald_finalize(est, isTRUE(digest$do_exp), se = se, crit = crit,
                            disp_known = digest$disp_known, df = digest$df_residual)
  list(tidy = tibble::tibble(term = rows$term, estimate = res$estimate,
                             conf.low = res$conf.low, conf.high = res$conf.high, p.value = res$p.value),
       nobs = digest$nobs, var_y = digest$var_y, positive_level = digest$positive_level,
       glance = digest$glance, fit = NULL, data = NULL, marg = digest$marg)
}


# Recover a column's per-cell SE, on the estimate's own TEST scale, from the Wald interval it
# stores. ⚠ on a MULTIPLICATIVE scale the SE lives on the LOG, where the gap is measured too.
# DESIGN: divide by z, never by the interval's own critical value -- the gap test is a z test
# throughout.
#' @keywords internal
reg_gap_se_of <- function(col, crit) {
  lo <- get_ci_inf(col); hi <- get_ci_sup(col)
  if (isTRUE(fmt_scale_row(col)$mult)) {          # a multiplicative scale -> the SE lives on the log
    ok <- is.finite(lo) & is.finite(hi) & lo > 0 & hi > 0
    ifelse(ok, (log(hi) - log(lo)) / (2 * crit), NA_real_)
  } else {
    ifelse(is.finite(lo) & is.finite(hi), (hi - lo) / (2 * crit), NA_real_)
  }
}

# Fill each group's `obs` with the REFERENCE GROUP's estimate for the same row, so `color =
# "between_groups"` reads the per-row effect-modification contrast. ⚠ rows are matched BY KEY (var,
# level), never by position: the compound-formula path builds each GROUP's skeleton from its own
# fit, so a group can have fewer rows in a different order. A key match degrades to NA instead of
# pairing the wrong rows, and the reference group's own cells are NA. The same pass writes `gap_se`:
# the two groups are DISJOINT samples, hence a gap variance of sqrt(SE_i^2 + SE_ref^2) (Altman &
# Bland 2003), recovered from the intervals the table already prints, so test and intervals cannot
# disagree. ⚠ a profile interval is asymmetric -> no SE is written.
#' @keywords internal
reg_write_group_gap <- function(parts, color, conf_level = 0.95, method = "wald") {
  if (!"between_groups" %in% color || length(parts) < 2L) return(parts)
  key_of <- function(d) reg_skel_key(as.character(d$var), as.character(d$levels))
  ref_d  <- parts[[1L]]$data                                  # the FIRST split level is the baseline
  ref_k  <- key_of(ref_d)
  fmt_nm <- names(ref_d)[purrr::map_lgl(ref_d, is_fmt)]
  crit   <- if (identical(method, "profile")) NA_real_ else zscore_formula(conf_level)
  est_of <- fmt_est_of
  for (i in seq_along(parts)) {
    d <- parts[[i]]$data
    m <- if (i == 1L) rep(NA_integer_, nrow(d)) else match(key_of(d), ref_k)
    for (nm in intersect(fmt_nm, names(d))) {
      if (!is_fmt(d[[nm]])) next
      # only where a gap measure can READ them: an `Obs_*` companion colours on its own measure.
      # fmt_color_attr, not get_color: a gap usually rides the BACKGROUND channel.
      if (!any(c("adjustment", "between_groups") %in% fmt_color_attr(d[[nm]]))) next
      d[[nm]] <- set_obs(d[[nm]], est_of(ref_d[[nm]])[m])
      if (!is.na(crit)) {
        se_ref <- reg_gap_se_of(ref_d[[nm]], crit)[m]
        d[[nm]] <- set_gap_se(d[[nm]], sqrt(reg_gap_se_of(d[[nm]], crit)^2 + se_ref^2))
      }
    }
    parts[[i]]$data <- d
  }
  parts
}


# THE assembly tail, shared by BOTH branches of reg_build(). A weighted tab_reg() is ALWAYS on the
# weighted basis, so tab()'s design_effect option is never read. ⚠ `basis` / `degf` are NULL on the
# split branch BY DESIGN: each group stamped its own, and the vec_rbind() reconcile took the weakest.
#' @keywords internal
reg_finalize <- function(tab, tests, conf_level, var_labels, group_vars,
                         degf = NULL, basis = NULL, meta_extra = list()) {
  tab |>
    tab_stamp_inference(conf_level, degf, basis) |>
    new_tab(subtext = meta_extra$subtext, test = tests,
            meta = c(meta_extra[setdiff(names(meta_extra), "subtext")],
                     list(spec = reg_spec(var_labels)))) |>
    dplyr::group_by(!!!rlang::syms(group_vars))
}

# === SECTION: The typed records =================================================================

# THE typed record of every per-call setting reg_build() reads. The idiom, shared with
# new_reg_spec(): the FORMALS are the contract and the body is as.list(environment()), so a direct
# caller omitting a slot gets the declared default, not a missing binding. `tab_vars` stays a formal
# of reg_build() instead: it flips to NULL in the split recursion, and NULL cannot round-trip
# through modifyList().
#' @keywords internal
new_reg_shared <- function(union_predictors = character(0), design_spec = list(), weighted = FALSE,
                           outcome_level = NULL, conf_level = conf_level_default(),
                           method = "wald", color_signif = "grey_non_signif", cleannames = TRUE,
                           subtext = "",
                           stats = NULL, compare = "none", baseline = NULL,
                           multiplier = NULL, multiplier_label = NULL,
                           shape_terms = NULL, shape_labels = NULL,
                           empirical = FALSE, display = NULL,
                           var_labels = character(0), na_shared_vars = character(0),
                           base_n = "range") {
  as.list(environment())
}
# ...and THE globalVariables mirror, DERIVED from those formals: reg_build() binds them with
# list2env(), which codetools cannot see, so a hand-kept copy could only fall behind.
utils::globalVariables(names(formals(new_reg_shared)))

# THE typed record of ONE fitted model -- new_reg_shared()'s per-model sibling, same idiom. ⚠
# `fit_family` is the internal LINK key `est$fit` carries ("rr" / "rd" / "mr" included), NOT the
# outcome family, which is `est$family`. `crude_key` is STORED rather than derived: it is a
# five-branch cascade over (fit family, trials, compound) read in six places.
#' @keywords internal
new_reg_spec <- function(outcome = character(0), predictors = character(0), label = "",
                         fit_family = "", trials = NULL, outcome_level = NA_character_,
                         compound = FALSE, formula = NULL,
                         color = NA_character_, est = NULL, crude_key = NA_character_) {
  # `outcome` arrives NAMED on the comparison branch and unnamed on the per-outcome one, and every
  # downstream map_chr(specs, "outcome") compares it to a bare column name.
  outcome <- unname(outcome)
  as.list(environment())
}
utils::globalVariables(names(formals(new_reg_spec)))

reg_inference <- function(shared, degraded = FALSE) {
  ds <- shared$design_spec
  leaf_inference(new_inference(ds$wt, ds, force = TRUE), degraded = degraded)
}


# === SECTION: reg_build() -- THE STAGED BUILD ====================================================
#
# WHICH STAGE PRODUCED WHICH PART OF THE TABLE. Each stage is named after the part it produces and
# runs over ONE typed context; the per-MODEL half of them is ONE declared product (reg_spec_build(),
# R/reg-spec-build.R), so the stages around the loop are cross-spec ASSEMBLERS and the loop itself is
# dispatchable (`parallel`).
# ⚠ THE STAGE ORDER IS THE SOURCE ORDER, and load-bearing: every fit happens inside reg_stage_specs()
# and may inform or warn, so the message stream is part of the output. It is SPEC-major.

# THE typed context of one build, in new_ctx()'s idiom (R/tab.R): a stage product is DECLARED, never
# left to appear -- an undeclared key is ABSENT, list2env() creates no binding for it, and its own
# is.null() guard therefore ERRORS instead of returning TRUE. DESIGN: `shared` stays ONE nested
# element and is PROJECTED into bare names at each stage head, never flattened -- three consumers
# need the record whole. A build-time assert (R/zzz-fact-keys.R) keeps the two name sets DISJOINT,
# so a projection cannot shadow a product.
#' @keywords internal
#' @noRd
new_reg_ctx <- function(
    # --- INPUTS: reg_build()'s own formals ------------------------------------------------------
    # ⚠ `skeleton_data` is FORCED here, before reg_stage_setup() may relevel `data` on the reref
    # path: it means the FULL data, so every split group shares one skeleton.
    # ⚠ `fit_cache` is NOT `.fit_cache`: `as.list(environment())` defaults to all.names = FALSE, so a
    # dot-prefixed key is SILENTLY DROPPED. No ctx key may start with a dot.
    data = NULL, specs = list(), shared = list(), tab_vars = NULL, fit_cache = NULL,
    ref = NULL, reref = FALSE, skeleton_data = NULL, parallel = NULL,
    # --- reg_stage_setup: the skeleton, the table's SHAPE facts and the per-spec PLAN ------------
    # ⚠ `data` is REWRITTEN here on the reref path and read afterwards by four consumers: a declared
    # PRODUCT as well as an input. `data_canon` is the PRE-relevel frame the digest is fitted on.
    family = NA_character_, skeleton = NULL, skeleton_deferred = FALSE, data_canon = NULL,
    compound = logical(0), builders = character(0),
    prefix_dep = FALSE, n_outcomes = 0L, is_comparison = FALSE,
    numeric_preds = character(0), factor_preds = character(0),
    # `spec_plan` is what the builder must be TOLD rather than work out: the de-duplications a worker
    # cannot reproduce, and the one predictor set whose rule is table-scalar.
    spec_plan = list(), want_global = TRUE,
    # --- reg_stage_crude: the ONE observed block of a one-outcome table ---------------------------
    # NULL when the table has several outcomes (each spec builds its own) or no crude companion.
    crude = NULL,
    # --- reg_stage_specs: one new_reg_spec_product() per spec, and the column LAYOUT -------------
    # `built` is the flattened VIEW of the products' `cols`, in order.
    products = list(),
    built = list(), labels = character(0),
    fit_first_idx = integer(0), fit_first_col = character(0),
    emp_degraded = FALSE,
    # --- reg_stage_footer: the `test` tibble -----------------------------------------------------
    test = NULL,
    # --- reg_stage_rows: the row axis ------------------------------------------------------------
    tab = NULL, disp_levels = character(0), assumptions = NULL,
    # --- reg_stage_tips --------------------------------------------------------------------------
    empirical_tips = NULL) {
  as.list(environment())
}
# ...and THE globalVariables mirror, DERIVED from those formals: list2env() is invisible to
# codetools.
utils::globalVariables(names(formals(new_reg_ctx)))

#' @keywords internal
#' @noRd
reg_ctx_locals <- function(ctx) c(ctx, ctx$shared)

reg_build <- function(data, specs, shared, tab_vars = NULL, .fit_cache = NULL, ref = NULL,
                      reref = FALSE, skeleton_data = data, parallel = NULL) {
  shared <- do.call(new_reg_shared, shared[intersect(names(shared), names(formals(new_reg_shared)))])
  ctx <- new_reg_ctx(
    data = data, specs = specs, shared = shared, tab_vars = tab_vars, fit_cache = .fit_cache,
    ref = ref, reref = reref, skeleton_data = skeleton_data, parallel = parallel,
    family = specs[[1]]$fit_family)
  list2env(reg_ctx_locals(ctx), environment())

  # THE STAGES. Each takes and returns the ctx; only the split recursion returns a finished table.
  # ⚠ the ORDER is the source order and is load-bearing (three of them fit models).
  if (!is.null(tab_vars)) return(reg_stage_split(ctx))
  ctx <- reg_stage_setup(ctx)      # the skeleton, the table's shape facts, the per-spec plan
  ctx <- reg_stage_crude(ctx)      # the observed (crude) block of a ONE-outcome table, built once
  ctx <- reg_stage_specs(ctx)      # ONE reg_spec_build() per model (serial or pooled) + the layout
  ctx <- reg_stage_footer(ctx)     # the products' rows + the between-model comparison -> `test`
  ctx <- reg_stage_rows(ctx)       # the row axis: labels, relabels, sparklines -> `tab`
  ctx <- reg_stage_assemble(ctx)   # the crude blocks and the model columns into `tab`
  ctx <- reg_stage_tips(ctx)       # the crude tooltips (multinomial + numeric)
  reg_stage_finalize(ctx)          # the inference basis, then the shared assembly tail
}


# THE tab_vars RECURSION: fit the SAME model(s) within each level of a grouping variable and STACK
# the per-group tables into one grouped_tab, so tab_spread(tab_vars) can pivot them side by side.
# The groups share ONE skeleton (the full data), so each has identical rows and columns. tab_vars is
# placed FIRST because the index columns DECLARE their roles, so the spread machinery needs no
# change. ⚠ it RETURNS A FINISHED TABLE, not a ctx -- the one early return.
#' @keywords internal
#' @noRd
reg_stage_split <- function(ctx) {
  list2env(reg_ctx_locals(ctx), environment())

  sl <- levels(forcats::fct_drop(as.factor(data[[tab_vars]])))
  # ⚠ this axis clears 2x only when the groups are EVEN and the frame is survey-size.
  parts <- tab_pmap(list(g = sl), "reg_build_group",
                    .const = list(sl = sl, tab_vars = tab_vars, specs = specs,
                                  fit_cache = fit_cache),
                    .ship  = list(shared = shared, data = data),
                    .names = as.character(sl),
                    workers = tab_parallel_workers(parallel, fit_cache))
  # `color = "between_groups"` scores each group's estimate against the REFERENCE GROUP's, and THIS
  # is the only point where the groups are parallel, separately addressable tibbles: vec_rbind()
  # then stacks them, and after the spread a group survives only in a name suffix. ⚠ the existing
  # reference machinery cannot do it -- fmt_broadcast_last() groups by runs of `in_refrow`, which
  # cross the split boundary.
  color_ms <- unique(unlist(purrr::map(specs, "color")))
  parts <- reg_write_group_gap(parts, color_ms, conf_level = conf_level, method = method)
  combined <- vctrs::vec_rbind(!!!purrr::map(parts, "data"))
  tests    <- purrr::list_rbind(purrr::compact(purrr::map(parts, "test")))
  if (is.null(tests) || nrow(tests) == 0) tests <- new_test_tibble()
  # the AGGREGATED companion of the per-cell gap colour, automatic under `color = "between_groups"`.
  # It costs one fit per spec, and this is the ONE place with the full data.
  if ("between_groups" %in% color_ms ||
      (is.character(shared$stats) && "interaction" %in% shared$stats)) {
    fit_cols <- unique(tests$col[tests$test %in% reg_footer_test_types()])
    if (length(fit_cols) != length(specs)) fit_cols <- make.unique(purrr::map_chr(specs, "label"))
    tests <- reg_interaction_rows(tests, data, specs, shared, tab_vars, fit_cols)
  }
  # `empirical_tips` / `assumptions` are deliberately NOT carried up: per-GROUP facts, and `meta` has
  # no per-group slot, so merging would attach the FIRST group's numbers to every cell.
  grouped <- reg_finalize(combined, tests, conf_level, var_labels,
                          group_vars = c(tab_vars, "var"),
                          meta_extra = list(subtext = subtext))
  # the groups go side by side whenever that is unambiguous -- ONE model, not multinomial. An
  # internal rule: tab_spread() is the public way to set the layout.
  if (length(specs) == 1L && !identical(family, "multinomial")) {
    return(tab_spread(grouped, tidyselect::all_of(tab_vars)))
  }
  return(grouped)
}


# THE TABLE'S SHAPE, before any model exists: the SKELETON every column is aligned to, the
# whole-table facts, and the PER-SPEC PLAN reg_spec_build() reads. ⚠ it REWRITES `data` on the reref
# path. The fits could leave this stage because the skeleton is fit-FREE in every shape but one --
# the cascade below, whose ORDER is the contract: only an all-coefficient table with a compound
# formula must read it back off the first fit, which is what `skeleton_deferred` names.
#' @keywords internal
#' @noRd
reg_stage_setup <- function(ctx) {
  list2env(reg_ctx_locals(ctx), environment())

  # jamovi live reref: `data` arrives at the CANONICAL reference the digest was fitted on, while the
  # display `reference` is baked into the skeleton -- so `data` is releveled here and the canonical
  # frame travels as `data_canon`. ⚠ off that path `data_canon` stays NULL, not a second name for it.
  data_canon <- NULL
  skeleton   <- NULL
  if (isTRUE(reref)) {
    data_canon <- data
    if (!is.null(ref)) data <- reg_apply_references(data, ref, union_predictors)
    skeleton <- reg_skeleton(data, union_predictors)   # an INPUT to reg_reref_fit_res(), not an output
  }

  compound   <- purrr::map_lgl(specs, ~ isTRUE(.$compound))
  builders   <- purrr::map_chr(specs, ~ .$est$builder %||% "coef")
  skeleton_deferred <- FALSE
  if (is.null(skeleton)) {
    if (any(builders != "coef"))  skeleton <- reg_skeleton(skeleton_data, union_predictors)  # one row per PREDICTOR
    else if (any(compound))       skeleton_deferred <- TRUE          # only here: reg_skeleton_from_fit()
    else                          skeleton <- reg_skeleton(skeleton_data, union_predictors, shape_terms)
  }

  prefix_dep    <- length(specs) > 1L
  n_outcomes    <- length(unique(purrr::map_chr(specs, "outcome")))
  is_comparison <- length(specs) > 1L && n_outcomes == 1L
  numeric_preds <- reg_numeric_preds(skeleton_data, union_predictors)
  factor_preds  <- reg_factor_preds(skeleton_data, union_predictors)

  want_global <- is.null(stats) || identical(stats, "all") || isTRUE(stats) ||
    (is.character(stats) && "global" %in% stats)

  outcomes <- purrr::map_chr(specs, "outcome")
  # THE CRUDE BLOCK BELONGS TO THE OUTCOME: every input is table-wide or per-OUTCOME, so ONE outcome
  # means ONE block built before any model (`want_crude`), while SEVERAL build one per spec
  # (`want_emp`) -- also an outcome, so the work stays on the parallel axis. A numeric predictor
  # gets a crude column from its univariable fit, EXCEPT under a compound formula, where a bare RHS
  # name's term may be an interaction or a basis expansion. ⚠ `any(compound)` is deliberately
  # TABLE-scalar: one compound spec empties this for every block.
  num_e    <- if (any(compound)) character(0) else numeric_preds
  has_pred <- length(factor_preds) > 0L || length(num_e) > 0L
  crude_ok <- !is.na(purrr::map_chr(specs, ~ .$crude_key %||% NA_character_))
  want_emp   <- emp_on(empirical) & has_pred & (n_outcomes > 1L) & crude_ok
  want_crude <- emp_on(empirical) && has_pred && n_outcomes <= 1L && crude_ok[[1L]] &&
    # a deferred skeleton is read off the FIRST fit, so a stage running before them has nothing to
    # align to. Unreachable (see the assert), kept as the stage's statement of what it needs.
    !skeleton_deferred

  # ⚠ THE TWO FACTS reg_stage_crude() RESTS ON, asserted rather than remembered:
  #  (1) with one outcome every spec is built from the same row, so the estimand, family, trials,
  #      crude_key and colour read off specs[[1]] are every spec's;
  #  (2) a deferred skeleton and a crude block cannot co-exist -- `compound` is only ever formula
  #      mode, and reg_crude_key(compound = TRUE) is NA, which turns `empirical` off at the argument
  #      boundary. Without it, `want_emp` not covering spec 1 would silently drop such a block.
  if (length(specs) > 1L && n_outcomes <= 1L) {
    one <- function(f) length(unique(purrr::map(specs, f))) == 1L
    stopifnot(one("outcome"), one("fit_family"), one("trials"), one("crude_key"),
              one(~ .$est$effect), one(~ .$est$measure), one("color"))
  }
  stopifnot(!skeleton_deferred || !emp_on(empirical))

  ctx_update(ctx, list(data = data, data_canon = data_canon, skeleton = skeleton,
                        skeleton_deferred = skeleton_deferred,
                        compound = compound, builders = builders,
                        prefix_dep = prefix_dep, n_outcomes = n_outcomes,
                        is_comparison = is_comparison,
                        numeric_preds = numeric_preds, factor_preds = factor_preds,
                        want_global = want_global,
                        spec_plan = list(want_emp = want_emp, want_crude = want_crude,
                                         num_preds = num_e)))
}


# THE OBSERVED (CRUDE) BLOCK OF A ONE-OUTCOME TABLE, built once, before any model: the descriptive
# companion every model column is compared to -- the crude level and effect, their intervals, the
# complete-case frame and the univariable legs the gap test needs. WHY IT IS A STAGE: the block is a
# function of the OUTCOME, not of a model (hence the assert above). ⚠ IT IS FIT-FREE, which is what
# makes it liftable: the two things it once read off the model object have exact producers of their
# own (reg_positive_level(), and reg_crude_yw()'s collapse of an unknown reference category to the
# crude frame's first level).
#' @keywords internal
#' @noRd
reg_stage_crude <- function(ctx) {
  list2env(reg_ctx_locals(ctx), environment())
  if (!isTRUE(spec_plan$want_crude)) return(ctx)

  sp     <- specs[[1L]]                     # every spec's, with one outcome (reg_stage_setup assert)
  sp_fam <- sp$fit_family
  inv_sp <- reg_outcome_level_of(sp$outcome_level) %||% outcome_level
  key    <- sp$crude_key
  mdata  <- reg_emp_frame(sp$outcome, ctx)  # the same complete-case frame as the model
  pos    <- if (reg_fam_binary(sp_fam)) reg_positive_level(mdata, sp$outcome, inv_sp) else NULL
  y_ref  <- levels(forcats::fct_drop(as.factor(mdata[[sp$outcome]])))[[1L]]
  var_y  <- if (sp_fam == "gaussian")
    suppressWarnings(stats::var(as.numeric(mdata[[sp$outcome]]), na.rm = TRUE)) else NA_real_

  block <- reg_crude_block(sp, sp_fam, inv_sp, key, mdata, pos, y_ref, var_y, ctx)
  # the crude companions share the model column's outcome col_var -- not in comparison mode.
  if (!is_comparison && length(block$cols)) {
    scv <- reg_shared_col_var(sp_fam, sp$outcome, pos, cleannames, sp$trials)
    block$cols <- purrr::map(block$cols, ~ set_col_var(.x, scv))
  }
  block$tips_num <- reg_spec_tips_num(sp, pos, block, ctx)

  ctx_update(ctx, list(crude = block))
}


#' @keywords internal
#' @noRd
reg_crude_block <- function(sp, sp_fam, inv_sp, key, mdata, pos, y_ref, var_y, ctx) {
  list2env(reg_ctx_locals(ctx), environment())
  # ⚠ the two crude predictor sets are TABLE-scalar and come from the declared plan: `num_preds` is
  # emptied when ANY spec has a compound formula, so it is not derivable from `sp` alone.
  fac_preds_e <- factor_preds
  num_preds_e <- spec_plan$num_preds
  emp <- reg_empirical(mdata, fac_preds_e, sp$outcome, key, pos, design_spec$wt,
                       trials = sp$trials, ref_category = y_ref,
                       conf_level = conf_level, design_spec = design_spec)
  # Which predictors have no closed form and must be fitted? The numeric ones, and EVERY predictor
  # under an ordinal outcome (proportional odds is a constraint, so a univariable fit is not
  # saturated).
  fit_preds_e <- c(
    num_preds_e,
    if (!reg_crude_saturated(key, TRUE)) fac_preds_e else character(0))
  # The crude fits take the FULL `data` + `drop_extra`, never the pre-filtered frame: a prebuilt
  # design's keep mask is computed from `data` itself. `marginal` swaps the crude shape for a
  # marginal one only where the model's estimand is marginal AND on a probability scale.
  fit_e <- reg_empirical_fit(
    data, fit_preds_e, sp$outcome, sp_fam, design_spec,
    outcome_level = inv_sp,
    conf_level = conf_level, method = method, skeleton = skeleton, multiplier = multiplier,
    other_preds = union_predictors, est = sp$est, wt = design_spec$wt,
    want_fit = TRUE, trials = sp$trials,
    shape_terms = shape_terms,
    marginal = !identical(sp$est$effect, "coefficient") &&
      (reg_fam_binary(sp_fam) || reg_fam_prob(sp_fam)))
  out <- reg_empirical_columns(skeleton, emp, fac_preds_e, key, sp_fam, sp$est, var_y,
                               conf_level = conf_level, color_signif = color_signif,
                               color = sp$color, fit_est = fit_e,
                               weighted = svy_weighted(design_spec, design_spec$wt),
                               degf = design_spec$degf %||% Inf,
                               emp_mode = empirical)
  # the crude columns take the table's own display -- one grammar, and by default the MIRROR layout.
  dress <- function(cl) purrr::map(cl, function(col)
    if (is.null(display)) reg_default_display(col, empirical) else reg_apply_display(col, display))
  out$cols     <- dress(out$cols)
  out$cat_cols <- dress(out$cat_cols)
  # the block also carries what the GAP TEST needs; none of it leaves reg_build() (reg_emp_slim()).
  out$frame     <- mdata
  out$fac_preds <- fac_preds_e          # ⚠ live: reg_set_obs() -> reg_gap_se_columns(fac_preds =)
  out$fit_preds <- fit_preds_e
  out$fits      <- fit_e$fits
  out$grid      <- emp
  out$degraded  <- isTRUE(attr(emp, "degrade"))
  out
}


# A SUMMED-SCORE outcome's level is the mean SCORE, not the per-item share the fit predicts.
# ⚠ EVERY builder needs this, not just the coefficient one.
#' @keywords internal
#' @noRd
reg_scale_pred <- function(marg, trials) {
  if (is.null(marg$pred) || !nrow(marg$pred) || is.na(trials %||% NA)) return(marg)
  marg$pred$pred <- marg$pred$pred * as.numeric(trials)
  marg
}

#' @keywords internal
#' @noRd
reg_cols_ame <- function(f, sp, ctx) {
  list2env(reg_ctx_locals(ctx), environment())
  sp_fam       <- sp$fit_family
  # the header word is COMPOSED here from the resolved estimand, so it cannot disagree with its
  # column.
  sp_eff       <- reg_word(sp$est)
  sp_col       <- sp$color
  prob_scale   <- reg_fam_prob(sp_fam)
  per_category <- reg_fam_percategory(sp_fam)
  # the contrast asked of the engine and the cell SHAPE both come from the estimand row.
  sp_est       <- sp$est
  ratio_ame    <- !is.na(sp_est$comparison) && identical(sp_est$comparison, "lnratioavg")
  shape        <- if (!prob_scale) (if (ratio_ame) "raw_ratio" else "raw")
                  else if (ratio_ame) "prob_ratio" else "prob"
  marg  <- reg_marginal(f$fit, f$data, sp$predictors, conf_level, design_spec$wt,
                        at = if (identical(sp_est$effect, "at_reference")) "reference" else "average",
                        want_pred = TRUE,
                        comparison = if (ratio_ame) "lnratioavg" else NULL,
                        multiplier = multiplier, engine = reg_marginal_engine(sp_est))
  marg     <- reg_scale_pred(marg, sp$trials)
  marg_add <- if (!ratio_ame) marg
    else if (is.null(f$fit)) NULL
    else reg_scale_pred(reg_fill_sweep(f$fit, f$data, sp$predictors, conf_level,
                                       design_spec$wt, multiplier), sp$trials)
  dress <- function(col, group = NULL) {
    col <- reg_fill_base(col, marg_add, skeleton, sp$predictors, group = group)
    if (is.null(display)) reg_default_display(col, empirical)
    else reg_apply_display(col, display)
  }
  # a summed score is prob-scale by family, but its additive effect is a difference of mean SCORES.
  var_y <- if (!prob_scale || !is.na(sp$trials %||% NA))
    suppressWarnings(stats::var(as.numeric(f$data[[sp$outcome]]))) else NA_real_
  if (per_category) {                            # one AME column per OUTCOME category (all levels)
    groups <- levels(as.factor(f$data[[sp$outcome]]))
    cv_cat <- reg_category_col_var(sp, is_comparison, f$positive_level, cleannames)
    purrr::map(groups, function(g) {
      jc  <- reg_cleanup(g, cleannames)
      lab <- paste0(if (prefix_dep) paste0(sp$outcome, " - ") else "", jc)
      list(label = lab, emp_key = g,   # emp_key: raw category, for the empirical tooltip
           col   = dress(reg_marginal_column(skeleton, marg, sp$predictors, shape,
                                             var_y, g, sp_col, color_signif, cv_cat,
                                             model_family = sp_fam,
                                             scale = reg_scale_of(sp_est, sp$trials),
                                             trials = sp$trials), g))
    })
  } else {
    or_tip <- if (sp_fam == "binomial" && !ratio_ame) {
      td <- broom::tidy(f$fit); td$term <- stringi::stri_replace_all_regex(td$term, "`", "")
      exp(td$estimate[match(skeleton$term, td$term)])
    } else NULL
    cv <- if (is_comparison) sp$label
          else reg_shared_col_var(sp_fam, sp$outcome, f$positive_level, cleannames, sp$trials)
    list(list(
      label = reg_model_col_name(sp_eff, sp$outcome, is_comparison, sp$label, n_outcomes),
      col   = dress(reg_marginal_column(skeleton, marg, sp$predictors, shape,
                                        var_y, NA_character_, sp_col, color_signif,
                                        cv, or_tip = or_tip, model_family = sp_fam,
                                        scale = reg_scale_of(sp_est, sp$trials),
                                        trials = sp$trials))))
  }
}


#' @keywords internal
#' @noRd
reg_cols_vsrest <- function(f, sp, ctx) {
  list2env(reg_ctx_locals(ctx), environment())
  sp_fam <- sp$fit_family
  sp_col <- sp$color
  marg   <- reg_marginal(f$fit, f$data, sp$predictors, conf_level, design_spec$wt,
                         at = "reference", comparison = "lnor", want_pred = FALSE,
                         engine = reg_marginal_engine(sp$est))
  marg_add <- if (is.null(f$fit)) NULL else
    reg_scale_pred(reg_fill_sweep(f$fit, f$data, sp$predictors, conf_level, design_spec$wt),
                   sp$trials)
  groups <- levels(as.factor(f$data[[sp$outcome]]))
  cv_cat <- reg_category_col_var(sp, is_comparison, f$positive_level, cleannames)
  purrr::map(groups, function(g) {
    jc  <- reg_cleanup(g, cleannames)
    lab <- paste0(if (prefix_dep) paste0(sp$outcome, " - ") else "", jc, " vs rest")
    col <- reg_marginal_column(skeleton, marg, sp$predictors, "or",
                               NA_real_, g, sp_col, color_signif, cv_cat,
                               model_family = sp_fam)
    col <- reg_fill_base(col, marg_add, skeleton, sp$predictors, group = g)
    list(label = lab,
         col   = if (is.null(display)) reg_default_display(col, empirical)
                 else reg_apply_display(col, display))
  })
}


#' @keywords internal
#' @noRd
reg_cols_coef <- function(f, sp, ctx) {
  list2env(reg_ctx_locals(ctx), environment())
  sp_fam   <- sp$fit_family
  sp_eff   <- reg_word(sp$est)                  # composed from the estimand, see cols_ame above
  sp_col   <- sp$color
  # the coefficient path runs no marginal sweep of its own, so it runs the one reg_fill_base() needs.
  # WARNING: a cached fit may have been DROPPED (`want_fit = FALSE`, the jamovi repaint path).
  model_predictors <- if (isTRUE(sp$compound)) unique(skeleton$var) else sp$predictors
  marg <- if (!is.null(f$marg)) f$marg               # the digest path already ran the sweep
          else if (is.null(f$fit)) NULL
          else reg_fill_sweep(f$fit, f$data, sp$predictors, conf_level, design_spec$wt, multiplier)
  marg <- reg_scale_pred(marg, sp$trials)
  dress <- function(col, group = NULL) {
    col <- reg_fill_base(col, marg, skeleton, model_predictors, group = group)
    if (is.null(display)) reg_default_display(col, empirical)
    else reg_apply_display(col, display)
  }
  if (sp_fam == "multinomial") {
    cols <- reg_columns_multinom(skeleton, f, sp, sp$est, sp_col, color_signif,
                                 cleannames, prefix_dep,
                                 col_var = reg_category_col_var(sp, is_comparison,
                                                                f$positive_level, cleannames),
                                 model_family = sp_fam, method = method)
    return(purrr::map(cols, function(lc) { lc$col <- dress(lc$col, lc$emp_key); lc }))
  }
  cv  <- if (is_comparison) sp$label
         else reg_shared_col_var(sp_fam, sp$outcome, f$positive_level, cleannames, sp$trials)
  col <- reg_column(skeleton, f, model_predictors, cv, sp$est, sp_col, color_signif,
                    model_family = sp_fam, method = method, trials = sp$trials)
  list(list(label = reg_model_col_name(sp_eff, sp$outcome, is_comparison, sp$label, n_outcomes),
            col = dress(col)))
}


# ONE reg_spec_build() PER MODEL, and the column LAYOUT their products imply. SERIAL OR POOLED:
# reg_specs_independent() is the ONE predicate -- NULL when a spec needs nothing from another, else
# the reason, reported only when `parallel` was explicitly asked for, so what was not parallelised
# is never silent. Its two reasons are exactly what rides the serial branch: the crude block spec 1
# shares with the compared models, and the skeleton read back off the first fit.
#' @keywords internal
#' @noRd
reg_stage_specs <- function(ctx) {
  list2env(reg_ctx_locals(ctx), environment())

  why     <- reg_specs_independent(ctx)
  workers <- if (is.null(why)) tab_parallel_workers(parallel, fit_cache) else 0L
  # ⚠ only when the ARGUMENT was passed: an option set once must not nag on every comparison.
  if (!is.null(why) && !is.null(parallel) && !isFALSE(parallel))
    cli::cli_inform(c("i" = "{.arg parallel}: the models are built one after another here -- {why}."))

  if (workers > 1L) {
    # ⚠ the whole ctx is the shipped object -- data, skeleton, design and crude block -- sent ONCE.
    products <- tab_pmap(list(i = seq_along(specs)), "reg_spec_build",
                         .ship = list(ctx = ctx), .names = purrr::map_chr(specs, "label"),
                         workers = workers)
  } else {
    products <- vector("list", length(specs))
    for (k in seq_along(specs)) {
      products[[k]] <- reg_spec_build(k, ctx)
      if (k == 1L && isTRUE(skeleton_deferred))
        ctx <- ctx_update(ctx, list(skeleton = products[[1]]$skeleton))
    }
  }
  # ⚠ AFTER both branches, not inside the serial one: a table with ONE compound spec defers its
  # skeleton but has nothing to share, so it takes the pooled branch, where the loop-carried update
  # above never runs.
  if (isTRUE(skeleton_deferred)) skeleton <- products[[1]]$skeleton

  built  <- purrr::flatten(purrr::map(products, "cols"))
  labels <- make.unique(purrr::map_chr(built, "label"))

  # the footer keys each fit's GOF to its FIRST output column. ⚠ every model owns at least one column
  # and the LAYOUT depends on it: two models sharing a fit_first_idx would collide in the assembler's
  # match(), silently dropping the second's crude block.
  fit_ncol      <- purrr::map_int(products, ~ length(.x$cols))
  if (any(fit_ncol == 0L)) cli::cli_abort("Internal: a model produced no column.")
  fit_first_idx <- cumsum(c(1L, utils::head(fit_ncol, -1L)))
  fit_first_col <- labels[fit_first_idx]

  ctx_update(ctx, list(products = products, skeleton = skeleton,
                        built = built, labels = labels,
                        fit_first_idx = fit_first_idx, fit_first_col = fit_first_col,
                        emp_degraded = any(purrr::map_lgl(products, ~ isTRUE(.x$degraded))) ||
                          isTRUE(crude$degraded)))
}


# THE `test` TIBBLE, from the products plus the one footer producer that is BETWEEN models. ⚠
# SLOT-MAJOR, not product-major: GOF rows, comparison, global rows, checks -- the order a
# verification script compares. ⚠ reg_compare_rows() STAYS here: a test BETWEEN two fitted models
# needs the fit OBJECTS, which is why `compare != "none"` is reg_specs_independent()'s first
# refusal.
#' @keywords internal
#' @noRd
reg_stage_footer <- function(ctx) {
  list2env(reg_ctx_locals(ctx), environment())

  rekey <- function(slot) {
    rows <- purrr::compact(purrr::map(seq_along(products), function(k) {
      r <- products[[k]][[slot]]
      if (is.null(r) || nrow(r) == 0L) return(NULL)
      r$col <- fit_first_col[[k]]
      r
    }))
    if (length(rows) == 0L) NULL else dplyr::bind_rows(rows)
  }

  reg_gof <- rekey("gof_rows") %||% new_test_tibble()
  reg_gof <- reg_compare_rows(reg_gof, purrr::map(products, "fit"), specs, family, weighted = weighted,
                              fit_first_col = fit_first_col, compare = compare, baseline = baseline)
  gl <- rekey("global_rows"); if (!is.null(gl)) reg_gof <- dplyr::bind_rows(reg_gof, gl)
  ck <- rekey("check_rows");  if (!is.null(ck)) reg_gof <- dplyr::bind_rows(reg_gof, ck)

  ctx_update(ctx, list(test = reg_gof))
}


#' @keywords internal
#' @noRd
reg_stage_rows <- function(ctx) {
  list2env(reg_ctx_locals(ctx), environment())

  disp_levels <- reg_cleanup(skeleton$level, cleannames)
  if (length(shape_labels)) {
    for (v in names(shape_labels)) {
      hit <- skeleton$var == v & !is.na(skeleton$term)
      if (any(hit)) disp_levels[hit] <- sub(v, shape_labels[[v]], disp_levels[hit], fixed = TRUE)
    }
  }
  # relabel each scaled numeric predictor's level to "<var> (per <unit>)", found through the STORED
  # predictor kind and keyed on the LINEAR term, so a curved predictor's squared row claims no unit.
  if (length(multiplier_label)) {
    num_rows <- skeleton$var %in% numeric_preds &
      !is.na(skeleton$term) & skeleton$term == skeleton$var
    for (v in names(multiplier_label)) {
      lab <- multiplier_label[[v]]
      if (is.na(lab)) next
      hit <- num_rows & skeleton$var == v
      if (any(hit)) disp_levels[hit] <- paste0(disp_levels[hit], " (per ", lab, ")")
    }
  }

  # the OBSERVED shape of each continuous predictor, miniaturised into its row label. Fit-free, and
  # drawn on `skeleton_data`: the groups share one skeleton, so a per-group curve would relabel a
  # row.
  assumptions <- reg_curves(skeleton_data, specs, numeric_preds, design_spec$wt,
                            positive_level = products[[1]]$positive_level,
                            design = design_spec$design)
  if (!is.null(assumptions)) {
    spark <- tx_option("spark")
    lin   <- !is.na(skeleton$term) & skeleton$term == skeleton$var
    for (v in names(assumptions$curves)) {
      gl <- rd_spark(assumptions$curves[[v]]$y, spark)
      if (is.na(gl)) next
      hit <- lin & skeleton$var == v
      # a NON-BREAKING space (U+00A0): the glyph run belongs to the label and must not wrap off it
      if (any(hit)) disp_levels[hit] <- paste0(disp_levels[hit], "\u00a0", gl)
    }
  }

  tab <- tibble::tibble(
    var    = new_lvl(forcats::fct_inorder(skeleton$var), "var"),
    levels = new_lvl(forcats::fct_inorder(disp_levels) , "level")
  )

  ctx_update(ctx, list(tab = tab, disp_levels = disp_levels, assumptions = assumptions))
}


# The per-outcome complete-case frame the crude companions and the tooltips share with the model,
# RECOMPUTED from `data` since `fits[[i]]$data` is NULL on the reref / digest path. `na_shared_vars`
# is the same extra-completeness set reg_fit() receives, so under the default it IS the model's own.
#' @keywords internal
#' @noRd
reg_emp_frame <- function(dep, ctx) {
  s <- ctx$shared
  reg_complete_frame(ctx$data, c(dep, s$union_predictors, s$na_shared_vars,
                                 reg_design_vars(s$design_spec)))
}


# ONE model column's `obs` (what it is compared to, on its own scale) and the `gap_se` between them
# -- or NEITHER, when the two estimators are not the same estimand on the same people. `f` / `sp`
# are the fit and spec this COLUMN came from, NOT the crude block's: in comparison mode one block
# serves several models. `key` is the column's OWN outcome category -- a multinomial needs one
# counterpart per column.
#' @keywords internal
#' @noRd
reg_set_obs <- function(bi, e, f, sp, ctx) {
  list2env(reg_ctx_locals(ctx), environment())
  col <- bi$col
  # the "may a crude value be attached?" decision is PER SPEC (the estimand's declared `obs`).
  if (is.null(e) || !isTRUE(sp$est$obs)) return(col)
  if (!reg_same_estimand(e$shape, col)) return(col)     # same scale, or nothing
  # ...and the same PEOPLE, or nothing: otherwise the "gap" is listwise deletion, not adjustment.
  if (!reg_same_frame(e$frame, f)) return(col)
  key <- if (is.null(bi$emp_key)) "" else as.character(bi$emp_key)
  ev  <- cat_get(e$effect, key)
  if (is.null(ev)) return(col)
  col <- set_obs(col, ev)
  # Where the crude effect draws NO column of its own it is folded into the model cell as
  # "{est} ({obs})": `obs` is on the CELL'S OWN SCALE, so the bracket is the same kind of quantity as
  # the estimate and IS what `color = "adjustment"` scores. An explicit `display` wins outright.
  if (identical(empirical, "cell") && is.null(display)) {
    d   <- get_display(col)
    hit <- is.finite(ev) & d %in% DISPLAY_VALUE_CELLS
    if (any(hit)) col <- set_display(col, dplyr::if_else(hit, "{est} ({obs})", d))
  }
  g <- reg_gap_se_columns(f, sp, col, skeleton, e$shape, e$frame,
                          e$fac_preds, sp$est, design_spec$wt,
                          fits_crude = e$fits, fit_preds = e$fit_preds, multiplier = multiplier,
                          category = key)
  if (is.null(g)) col else set_gap_se(col, g)
}


#' @keywords internal
#' @noRd
reg_add_emp_cols <- function(tab, cols, suffix) {
  for (nm in names(cols)) {
    out_nm <- if (nzchar(suffix)) paste0(nm, " [", suffix, "]") else nm
    tab[[out_nm]] <- cols[[nm]]
  }
  tab
}


#' @keywords internal
#' @noRd
reg_stage_assemble <- function(ctx) {
  list2env(reg_ctx_locals(ctx), environment())

  if (n_outcomes <= 1L) {
    if (!is.null(crude)) tab <- reg_add_emp_cols(tab, crude$cols, "")
  } else {
    for (i in seq_along(built)) {
      # ⚠ match(), not fit_of_col: it is non-NA exactly at a fit's FIRST column, which is where that
      # fit's crude block belongs.
      fi <- match(i, fit_first_idx)
      if (!is.na(fi) && !is.null(products[[fi]]$emp))
        tab <- reg_add_emp_cols(tab, products[[fi]]$emp$cols, specs[[fi]]$outcome)
      tab[[labels[i]]] <- built[[i]]$col
    }
    return(ctx_update(ctx, list(tab = tab)))
  }
  for (i in seq_along(built)) tab[[labels[i]]] <- built[[i]]$col

  ctx_update(ctx, list(tab = tab))
}


# `meta$empirical_tips`, from the products' fragments. The two blocks that produce them live in
# reg_spec_build(), because both read the crude block's HEAVY halves -- the grid and the
# complete-case frame -- which must not travel back from a worker. What arrives is keyed by SKELETON
# ROW and, for the multinomial fragment, by WITHIN-SPEC COLUMN. ⚠ SLOT-MAJOR, the order the two
# blocks ran in.
#' @keywords internal
#' @noRd
reg_stage_tips <- function(ctx) {
  list2env(reg_ctx_locals(ctx), environment())

  mnl <- purrr::compact(purrr::map(seq_along(products), function(k) {
    fr <- products[[k]]$tips$mnl
    if (is.null(fr) || nrow(fr) == 0L) return(NULL)
    tibble::tibble(col   = labels[fit_first_idx[[k]] + fr$col_idx - 1L],
                   var   = fr$var,
                   level = disp_levels[fr$row],
                   tip   = fr$tip)
  }))
  # ⚠ the numeric fragments come from the BLOCKS, not the specs: one per outcome -- with a single
  # outcome reg_stage_crude()'s one block, with several each spec's own.
  num <- purrr::compact(purrr::map(c(list(crude$tips_num), purrr::map(products, ~ .x$tips$num)),
                                   function(fr) {
    if (is.null(fr) || nrow(fr) == 0L) return(NULL)
    tibble::tibble(col = fr$col, var = fr$var, level = disp_levels[fr$row], tip = fr$tip)
  }))

  rows <- c(mnl, num)
  ctx_update(ctx, list(empirical_tips = if (length(rows)) purrr::list_rbind(rows) else NULL))
}


#' @keywords internal
#' @noRd
reg_stage_finalize <- function(ctx) {
  list2env(reg_ctx_locals(ctx), environment())

  # The confidence level, the design df and the basis are stamped on EACH fmt column -- the colour
  # engine is per column and cannot read a table attribute.
  reg_inf <- reg_inference(shared, emp_degraded)
  out <- reg_finalize(tab, test, conf_level, var_labels, group_vars = "var",
                      degf = reg_inf$degf, basis = reg_inf$basis,
                      meta_extra = list(subtext = subtext, empirical_tips = empirical_tips,
                                        assumptions = assumptions))
  # the base count is a DISPLAY intent here exactly as in tab(): the column is synthesised at
  # print/export time from the `n` every model column carries.
  set_render_extras(out, list(n = shared$base_n))
}



# === Public API =====================================================================

#' Regression table (effect measures) as a tabxplor table
#'
#' Fits one regression model per column and returns a `tabxplor` table of the per-family effect
#' measure --- a linear **mean difference** (gaussian), **odds ratios** (binomial / logistic),
#' **incidence-rate ratios** (poisson), one **odds-ratio column per outcome category** (nominal 3+
#' level), a **cumulative odds ratio** (ordinal) --- with one row per predictor level, the reference
#' level shown as the neutral value `0` or `1`, grouped by predictor. Each cell stores its estimate,
#' confidence interval and p-value, so the table prints with significance stars, greys out what is
#' not significant, and exports (HTML / Markdown / Excel) like any `tabxplor` crosstab.
#'
#' @details
#' New to regressions with tabxplor? A first model needs three arguments: `data`, `outcome` and
#' `predictors`. The model follows the outcome's type --- a two-level factor gives logistic **odds
#' ratios**, a numeric a linear **mean difference**, a count Poisson **rate ratios**, a 3+ level
#' factor multinomial or ordinal odds ratios --- so you rarely set `family` by hand. Add
#' `empirical = TRUE` to put the crude (unadjusted) effect beside each adjusted one. See
#' `vignette("tabxplor-reg")` for a guided tour.
#'
#' The arguments fall into groups:
#' \itemize{
#'   \item **The model**: `data`, `outcome`, `predictors` (a character vector = one model, a named
#'     list = several models to compare), `family`, `wt` (survey weights).
#'   \item **What each cell shows**: `effect` x `measure` (which contrast, which effect measure),
#'     `display` (the layout), `empirical` (the crude effect beside the adjusted one).
#'   \item **Colours & significance**: `color`, `color_signif`, `stars`, `conf_level`, as in [tab()].
#'   \item **Comparisons & structure**: `ref` (baseline levels), `outcome_level` (the level
#'     modelled), `tab_vars` (one table per group), `multiplier` (a continuous predictor's unit).
#'   \item **The footer**: `stats` --- goodness-of-fit rows, model checks and the model comparison.
#'   \item **Fixing what a check flags**: `shape`, drawn by [reg_check_plots()]. **Charting the
#'     result**: [forest_plot()].
#' }
#'
#' `predictors` selects the mode: a **character vector** fits one model, and `outcome` may then be a
#' vector too (one column per outcome); a **named list** of predictor sets fits one model each, one
#' column per model, for comparing specifications (a predictor absent from a model leaves its cells
#' blank).
#'
#' @param data A data frame, **or a prebuilt survey design** ([survey::svydesign()]). When a design
#'   is passed, its weights (and clustering / stratification / calibration) drive the estimation and
#'   `wt` is ignored. Replicate-weight ([survey::svrepdesign()]) and two-phase designs are refused at
#'   the boundary rather than approximated.
#' @param outcome Character outcome variable name(s), **or a model formula** (the escape hatch).
#'   With a `predictors` character vector, several names give one effect column per outcome; with a
#'   `predictors` list, a single name is required. A formula supplies its own model (leave
#'   `predictors` unset): a plain `y ~ a + b` behaves exactly like `outcome = "y"`,
#'   `predictors = c("a", "b")`, while interactions, `poly()` and `I()` terms render as best-effort
#'   term rows.
#' @param predictors Either a character vector of predictor names (one model), or a **named list**
#'   of character vectors (one model per element, its name labelling the column). Leave `NULL` when
#'   `outcome` is a formula.
#' @param family The model family, **resolved per outcome** so several outcomes with different
#'   families can share one table. `"auto"` (default) detects each one and says so: a binary outcome
#'   gives `"binomial"`, an ordered 3+ level `"ordinal"`, a nominal 3+ level `"multinomial"`, any
#'   other numeric `"gaussian"`. An integer-valued numeric reads as `"gaussian"` too --- age in
#'   years, years of schooling and income in whole units are all integers, and a linear model always
#'   fits --- with the message naming `"poisson"` for a genuine count. Set it explicitly with
#'   `"gaussian"` (linear), `"binomial"` (logistic), `"poisson"` / `"quasipoisson"` (counts),
#'   `"multinomial"`, `"ordinal"`. A **scalar** applies to every outcome; a **vector** aligned to
#'   `outcome`, or a **named** vector keyed by outcome (e.g. `c(income = "poisson", satisfied =
#'   "binomial")`), sets one family per outcome. Mixed families work only with a character
#'   `predictors`; a `predictors` list is single-outcome, hence single-family.
#'
#'   `family = "poisson"` on a **binary** outcome is neither a mistake nor a count model: it fits the
#'   **modified Poisson** (Zou 2004), whose exponentiated coefficient is a **risk ratio**. It is the
#'   same table as `measure = "ratio"`, which names the measure rather than the distribution --- see
#'   there. It is strictly opt-in: a binary outcome still auto-detects as `"binomial"`.
#'
#'   An unweighted `"poisson"` fit auto-scales its standard errors by the square root of the Pearson
#'   dispersion, so with an **over-dispersed** outcome its intervals and p-values are identical to
#'   `"quasipoisson"`, and it warns to say so (the footer reports the dispersion). At equidispersion
#'   the scaling is a no-op and the result matches a plain `glm(family = poisson)`.
#' @param wt Optional. Name of a weight column (character). Switches to design-based survey
#'   estimation ([survey::svyglm()]): the sandwich standard errors are scale-invariant, so raw
#'   population weights are handled correctly (no normalisation) and the point estimates match the
#'   weighted crosstabs. For clustering, stratification, a finite-population correction or
#'   calibration, build the design yourself with [survey::svydesign()] and pass it as `data`; `wt`
#'   alone is a flat `ids = ~1` design, which can understate the variance of a clustered sample.
#' @param effect **Which contrast** the table shows --- one of the two questions an estimand asks.
#'   `"coefficient"` (default) is the model's own conditional effect ("holding the other predictors
#'   constant"). `"marginal"` is the **average marginal effect**: the model's effect averaged over
#'   the observed covariate distribution --- a probability-scale, cross-model-comparable summary
#'   (Mood 2010) for logistic / multinomial / ordinal outcomes, the expected-count change for
#'   poisson, the coefficient itself for gaussian. `"at_reference"` evaluates the same quantity **at
#'   the reference profile** (every other predictor at its reference level or its mean), and for a
#'   **multinomial** outcome the odds ratio of each category *versus the rest* there. Resolved **per
#'   outcome** like `family` (scalar / vector / named vector).
#'
#'   The contrast is a **marker on the measure** in the column header, so the acronym stays the one
#'   thing to look up: `Model_OR`, `Model_mRR`, `Model_refRD` (see *The header acronyms* below). The
#'   observed companion carries the measure alone (`Obs_RR`), a univariable effect having no
#'   adjustment to be marginal over. A marginal quantity is standardized to the covariate
#'   distribution at hand, so under `tab_vars` each group standardizes to its own subpopulation.
#' @param measure **Which effect measure** --- the other question. `"auto"` (default) takes the
#'   family's usual one (odds ratios for a logit, incidence-rate ratios for a count, a mean
#'   difference for a linear model; a difference for a marginal contrast). The full word is the
#'   canonical spelling and the discipline's acronym an accepted synonym, so the argument teaches the
#'   concept while the column header keeps the acronym:
#'
#'   * `"odds_ratio"` (`"OR"`) --- the odds ratio of a logit / multinomial / ordinal fit.
#'   * `"ratio"` (`"RR"`, `"IRR"`, `"RoM"`) --- a **risk** ratio on a binary outcome (the modified
#'     Poisson, Zou 2004), an **incidence-rate** ratio on a count, a **ratio of adjusted means** on a
#'     continuous one (Poisson pseudo-maximum-likelihood, Santos Silva & Tenreyro 2006). Reach for it
#'     when the outcome is **common**: an odds ratio is then much further from 1 than the risk ratio
#'     and is almost always narrated as if it were one ("twice as likely"), and unlike an odds ratio
#'     a risk ratio stays comparable **across nested models**.
#'   * `"difference"` (`"RD"`, `"diff"`) --- a linear coefficient on a continuous outcome; on a
#'     **binary** one the **risk difference** in percentage points, from an identity-link fit with
#'     robust standard errors. That link is unbounded and can fail to converge: the linear
#'     probability model then takes over, with a message, and the footer says which one ran.
#'   * `"log"` (`"log_odds"`, `"log_risk"`, `"log_rate"`) --- the same fit, **un-exponentiated**.
#'     Bare `"log"` logs the family's default measure; the precise spellings pin which one. The
#'     header names what it logs (`Model_log(OR)`), never one greek letter for five quantities.
#'
#'   Resolved **per outcome** like `family`. `effect` and `measure` are orthogonal: a *conditional*
#'   ratio is a different **fit**, a *marginal* one a different **estimator**, and both land on the
#'   same stored scale. Call [reg_measures()] on your outcome to see what it offers, with the reason
#'   wherever it does not; `vignette("tabxplor-reg")` discusses what each route assumes. Elsewhere,
#'   `measure = "ratio"` on a binary outcome is Stata's `binreg y x, rr`; `measure = "log"` is
#'   `exponentiate = FALSE` in broom / parameters / gtsummary; `effect = "marginal"` is
#'   `marginaleffects::avg_comparisons()`.
#' @param trials Grouped-binomial (summed-score) outcomes only. The number of items behind the score,
#'   fitting `cbind(score, trials - score)` as a binomial. `NULL` (default) fits an ordinary binary
#'   logit; a single integer (or a vector named by outcome) sets the item count; `TRUE`, or an `NA`
#'   entry in a named vector, uses that outcome's **observed maximum** score --- so explicit and
#'   automatic counts can be mixed. Requires `family = "binomial"`. It is one count per *outcome*,
#'   never a column name; for a per-row item count, write `cbind()` in a compound `formula`.
#' @param conf_level Confidence level for the intervals. Default `0.95`. It drives every interval in
#'   the table, the significance stars, the greying under `color_signif` and the
#'   model-versus-observed gap interval, and is stored on each column, so it follows this argument
#'   rather than `options("tabxplor.conf_level")`.
#' @param ci_method How the interval and p-value are computed --- the same argument, and the same
#'   named-vector grammar, as in [tab()], whose fifth slot is this producer's:
#'   `ci_method = c(model = "profile")`. On a regression there is only one interval to choose a
#'   method for, so a bare `"profile"` means that slot. `"wald"` (default) uses the Wald interval and
#'   the Wald z / t test: fast, matching standard software output, and the only option for weighted
#'   models. `"profile"` uses the profile-likelihood interval ([stats::confint()], needs `MASS`) and
#'   the likelihood-ratio test: more accurate near separation, unweighted binomial / poisson only
#'   (otherwise it falls back to Wald with a message; gaussian always uses the exact-t interval).
#' @param ref Optional named vector `c(var = "baseline level")` --- the same grammar as [tab()]'s
#'   `ref` --- choosing the treatment-contrast reference level of one or more factor **predictors**
#'   (every other level's effect is measured against it), and of `tab_vars` (which group
#'   `color = "between_groups"` compares to). Other contrast codings can be applied by passing a
#'   formula in `outcome` with the terms already coded. For the level of the **outcome**, see
#'   `outcome_level`: `ref` names the level you compare AGAINST, `outcome_level` the one you MODEL.
#' @param outcome_level Which level of the **outcome** to single out, as a named vector keyed by
#'   outcome name --- `outcome_level = c(married = "Married")` --- so several outcomes each get their
#'   own. It is the twin of `ref`: **`ref` names the level you compare AGAINST, `outcome_level` the
#'   one you MODEL.**
#'   \itemize{
#'     \item **binomial**: the level whose probability is estimated. It becomes the column header,
#'       and every odds ratio is the odds of *that* level. Defaults to the outcome's **first** level
#'       (so a coded factor like `"1-Married"` / `"2-Not married"` models "1-Married"). A 0/1 numeric
#'       outcome is labelled `"Not <outcome>"` / `"<outcome>"`, and either spelling --- or the raw
#'       `"0"` / `"1"` --- may be named.
#'     \item **multinomial**: the baseline category every other category's column is compared to.
#'       With more than two levels you cannot choose what is modelled (all of them are), only the
#'       pivot --- which is why the same argument means the opposite thing here.
#'     \item **ordinal, and any numeric outcome**: refused, with the reason. An ordinal outcome must
#'       keep the order of its levels, so none of them can be singled out.
#'   }
#' @param tab_vars Optional. Name of a grouping variable (character) --- the same argument as
#'   [tab()]'s `tab_vars`: one sub-table per group, the same model(s) fitted **within each level**.
#'   When that leaves one column per group (a single outcome, a single set of predictors, and not a
#'   multinomial) the groups are pivoted into **side-by-side columns**; otherwise the per-group
#'   tables are stacked into one grouped table sharing the variable / level stub --- call
#'   [tab_spread()] yourself for full control there. A level absent from a group shows empty cells.
#'   Two readings of "does this effect hold in every subgroup?" come with it:
#'   `color = "between_groups"` colours and tests each effect against the first group's, row by row,
#'   and `stats = c(..., "interaction")` adds the aggregated test, once per predictor.
#' @param multiplier How a **continuous** predictor's effect is scaled --- the unit its row reports.
#'   One unit of a continuous variable is rarely a readable amount (a one-year change in `age` barely
#'   moves the odds, so its odds ratio sits inside the first colour break and the row reads as "no
#'   effect"), so the default is **one standard deviation**. Give either a **single value**, applied
#'   to every continuous predictor, or a **named vector** overriding chosen ones: `"sd"` (the
#'   default), `"2sd"` (roughly bottom to top of the distribution), or a number of units (`10` = per
#'   decade of age). `multiplier = 1` restores the per-one-unit reading, and the row label names the
#'   unit it used, e.g. `age (per 1 SD (13.5))`.
#'
#'   Everything scales together --- the estimate, its interval, the crude `Obs_*` companion and the
#'   model-versus-observed comparison; the p-value is unchanged. **Because the default is not 1, a
#'   continuous predictor's `Model_*` cell does not equal `exp(coef(glm(...)))` unless you pass
#'   `multiplier = 1`.** The standard deviation is measured **once**, on the complete cases of the
#'   predictors, so one predictor keeps one unit across outcomes, compared models and `tab_vars`
#'   groups. Not applied to multinomial / ordinal outcomes, nor to a `formula` model. A 0/1-coded
#'   **numeric** predictor gets a "per 1 SD (0.5)" reading --- pass it as a factor instead.
#' @param shape How a **continuous** predictor enters the model, when one straight line is not
#'   enough. The `Linearity` footer row and the little curve drawn in the predictor's row label tell
#'   you *whether* a line is enough; this argument is how you fix it without leaving the framework.
#'   A **named vector** over continuous predictors --- everything it does not name stays linear:
#'   \describe{
#'     \item{`"linear"`}{one slope (the default).}
#'     \item{`"quintiles"` / `"quartiles"` / an integer `k`}{cut into `k` quantile groups, so the
#'       predictor becomes an ordinary **factor**: one estimate per group, its own observed
#'       companion, counts and colours per group --- the non-linearity becomes visible in the printed
#'       numbers. Start here; it is the most readable answer.}
#'     \item{`"quadratic"`}{adds a curvature term, so the predictor takes **two rows** --- the slope
#'       at the mean, and the squared term, which says whether the slope flattens or accelerates as
#'       you move away from it.}
#'     \item{`"log"` / `"sqrt"`}{fit `log(x)` / `sqrt(x)` instead of `x` --- diminishing returns. The
#'       row label says which (`log(age)`); `"log"` needs strictly positive values.}
#'   }
#'   Example: `shape = c(age = "quadratic", income = "log")`. Everything else keeps working: the
#'   observed companion is fitted with the same shape, the comparisons compare like with like, and
#'   `multiplier` still names the unit. A `poly()` / `ns()` basis is deliberately never emitted ---
#'   the marginal-effects engine silently returns zero for those.
#' @param empirical Show the **observed, unadjusted (crude)** effect beside each modelled one ---
#'   the same quantity fitted with a single predictor. It IS the modelled quantity when there is only
#'   one predictor, so the distance between the two is exactly what adjustment changed, read left to
#'   right. `FALSE` (default) or `TRUE`; two expert spellings say *where* the crude effect goes:
#'   \itemize{
#'     \item `TRUE` --- a crude **column** beside the model one, except on a 3+ level outcome
#'       (multinomial, or an ordinal marginal effect), where one model column would need one crude
#'       column per outcome category: there the crude value rides **inside** the model cell instead,
#'       as `1/1.63*** (1/1.69)`.
#'     \item `"column"` --- always the column, per outcome category if that is what it takes.
#'     \item `"cell"` --- always in the cell, however few columns it would have taken.
#'   }
#'   The two columns are the same column twice: same estimand, same colour ladder, same layout, one
#'   legend block. Each cell prints the effect with the level it sits on beside it --- the observed
#'   percentage or mean on the crude side, the **adjusted** prediction on the model side --- so the
#'   two effects end up adjacent. Ask for another layout with `display`.
#'
#'   Where the univariable model is **saturated** (a categorical predictor under every family except
#'   ordinal) the crude effect has a closed form; otherwise it is a real fit, so the crude column
#'   shares the model's family, link, interval method and `multiplier` by construction. A
#'   **continuous** predictor has no levels, so its cell shows the univariable slope alone, which
#'   assumes linearity on the model's scale --- check that with `shape` before trusting it.
#'
#'   Every crude quantity is computed on **exactly the same complete-case population as the model**,
#'   so the two are not confounded by differing missingness; under `na = "drop_by_model"` a model
#'   fitted on other rows gets **no** observed value at all, the distance between two such estimates
#'   being listwise deletion rather than adjustment. Both columns are also always on the **same
#'   inference basis** --- which is why a weighted `tab_reg()` is *always* design-corrected where a
#'   weighted [tab()] is not unless asked (`design_effect = TRUE`). The footer says which basis a
#'   table used.
#' @param n How many people the table is about. `"range"` (the default) adds an `n` column holding
#'   the **unadjusted count** behind each predictor level, on the model's own complete cases --- the
#'   numbers a reader needs to judge the estimates beside them --- with the model N on the Constant
#'   row. When several models were fitted on different people it prints the whole range
#'   (`5 139-9 862`), so an unequal base can never pass unnoticed; `"min"` shows the smallest count
#'   only, `"no"` no count at all. Continuous predictors are left blank: on a listwise-complete frame
#'   their count is the model N. With `tab_vars`, one column per group, to the right of the models.
#' @param stats The statistics shown in the model-summary **footer** (one block per model). `NULL`
#'   (default) uses the per-family set: linear models show R square, adjusted R square, the
#'   overall F-test and the residual SD; other models show the likelihood-ratio test versus the
#'   null model, McFadden's pseudo-R square, AIC and BIC (count and grouped-binomial models also show
#'   the Pearson dispersion, `"phi"`). Every default set also carries the overall-association test
#'   `"global"` and the **model checks that cost nothing** (see below). Pass a character vector to
#'   pick the statistics (`"n"`, `"lr_null"`, `"mcfadden_r2"`, `"aic"`, `"bic"`, `"phi"`, `"r2"`,
#'   `"r2_adj"`, `"f_model"`, `"sigma"`, `"global"`, `"interaction"`, `"linearity"`,
#'   `"proportionality"`, `"dispersion"`, `"influence"`, `"collinearity"`), `"all"` for everything
#'   this model can report, or `FALSE` / `"none"` to hide the footer. Weighted models show a reduced,
#'   survey-appropriate set (design-based Wald test, Nagelkerke pseudo-R square, AIC).
#'
#'   **Model comparison** (several models / outcomes only) is two more keys, so it needs no separate
#'   argument: `"compare_sequential"` tests each model against the previous one, and
#'   `"compare_baseline"` each against one reference model --- the first by default, or the one you
#'   name as the key's value, `stats = c("n", "aic", compare_baseline = "Model 1")`. Both use a
#'   likelihood-ratio test (F for linear / quasi models, a design-based Wald test for weighted ones),
#'   falling back to the AIC difference with a message when the models are not nested or have
#'   different N. A comparison key **adds** a row and restricts nothing.
#'
#'   `"global"` adds one **overall test per predictor** --- "is this variable associated with the
#'   outcome at all?", the question a block of stars against a reference category cannot answer. It
#'   costs no extra fit and is shown for predictors carrying two or more coefficients.
#'   `"interaction"` needs `tab_vars` and adds one **aggregated effect-modification test per
#'   predictor**, asked once for all its levels together, so it carries none of the multiplicity of
#'   the per-cell `color = "between_groups"` colours --- which turns it on for you. It costs one
#'   extra fit. Neither test is available for multinomial or ordinal outcomes.
#'
#' @section Model checks:
#'
#' Five checks, in the order of what each one threatens --- the estimate, what the estimate means,
#' its interval, whether it is real at all, and why it is wide. Each is a footer row, so it travels
#' into every export, and each is named in `stats`.
#'
#' \describe{
#'   \item{**Linearity** (p-value, per numeric predictor)}{Is this predictor's effect really one
#'     straight line? The model is refitted with that predictor's centred squared term and the two
#'     compared. The damage a curve does is **not confined to its own row**. Cure it with `shape`.}
#'   \item{**Proportionality (Brant)** (p-value, ordinal outcomes)}{Is one cumulative odds ratio
#'     enough for every cut of the outcome? Read it beside the size of the departure: at survey
#'     sample sizes it rejects on differences the eye calls mild. Weighted ordinal models have no
#'     Brant fit, so the row is absent rather than approximated.}
#'   \item{**Dispersion (robust/model SE)** (a ratio)}{Are the standard errors wide enough? The
#'     largest ratio of a robust (sandwich) standard error to the model-based one. About 1 means the
#'     family's variance assumption holds; above 1 it does not --- over-dispersion,
#'     heteroscedasticity or clustering, by roughly that factor.}
#'   \item{**Influence (max dfbetas)** (a ratio)}{Does one respondent carry the result? The largest
#'     change dropping a single observation makes to a coefficient, in units of its own standard
#'     error. Printed as a *reassurance*: with thousands of respondents a near-zero value is the
#'     finding. Influence is not outlyingness.}
#'   \item{**Collinearity (max VIF)** (a ratio)}{Can the data tell these predictors apart? The
#'     largest variance inflation factor (`car::vif()`). The one check that is not a comparison with
#'     the data --- collinearity biases nothing, it only widens intervals. Needs `car`; refused for
#'     multinomial outcomes.}
#' }
#'
#' Dispersion, Influence and Collinearity are arithmetic on the model already fitted, so they ride
#' the default footer and cost nothing; Linearity and Proportionality fit a model and are therefore
#' **asked for by name** (`stats = c("n", "aic", "linearity")`, or `stats = "all"`). The cheap answer
#' is on screen either way: each numeric predictor's observed shape is binned with no fit at all and
#' drawn as the row's sparkline, and [reg_check_plots()] draws the full panel for **every** check.
#' At survey sample sizes a diagnostic p-value rejects almost anything, which is why three of the
#' five report a *magnitude* instead.
#'
#' @param display What each effect cell shows --- [tab()]'s display grammar, same names, same
#'   meaning, on every family and on the crude column as well as the model one. `NULL` (default)
#'   shows the plain estimate, or, with `empirical`, the estimate with the level it sits on beside
#'   it. The named layouts:
#'   * `"est"` --- the effect alone.
#'   * `"est_ci"` --- with a visible interval: `1/2.22 [1/2.47; 1/1.99]`.
#'   * `"est_base"` --- the effect with the level beside it: `1/2.22 (32.8%)` on a logistic model,
#'     `-0.89 (2.25)` on a linear one. On a model column that level is the **adjusted** prediction;
#'     on a crude column the observed percentage or mean.
#'   * `"base_est"` --- the mirror, level first: `(32.8%) 1/2.22`. The effect stays the number the
#'     cell is about (it carries the stars and the colour); the bracket is the aside.
#'   * `"base"` --- the levels alone, still coloured and starred by the effect.
#'
#'   Or write a `{}` template: `"{est} (obs {obs})"` prints each adjusted effect next to the
#'   unadjusted one, `"{est} ({gap})"` next to how far adjustment moved it.
#'
#'   `display` is a **post-hoc** property: every quantity it can name is already stored, so choosing
#'   a layout never triggers a computation and never changes a number --- [set_display()] on a built
#'   table gives the same result as asking for it here. It never changes the fit or the estimand,
#'   which is `measure`'s job alone.
#' @param color,color_signif Colouring of the effect cells. `color = TRUE` (default) grades each cell
#'   on **its own scale** --- the ladder follows what the column estimates (`measure`), so it is
#'   never asked for separately; `color = FALSE` turns colouring off. `color_signif` is the
#'   significance policy (default `"grey_non_signif"`). See [tab()].
#'
#'   What is left to choose is what each effect is compared **to**. Both such measures are meant for
#'   the *background* channel so the text keeps showing the effect size: `color` is positional,
#'   `c(text, background)`, and `TRUE` in the text slot means "the column's own scale", so
#'   `color = c(TRUE, "adjustment")` answers "how strong is this effect?" and "how much did the model
#'   change it?" in one glance.
#'
#'   * `"adjustment"` --- how far each **modelled** effect sits from its **observed** (crude)
#'     counterpart, i.e. what adjusting for the other predictors did to it. It turns
#'     `empirical = TRUE` on. The ladder follows the estimate's own scale, so a threshold means the
#'     same thing in every table: `x1.1 / x1.25 / x1.5 / x2` for a ratio, `2 / 5 / 10 / 20`
#'     **points** for a probability-scale marginal effect, `0.05 / 0.1 / 0.2 / 0.4` **standard
#'     deviations of the outcome** for an effect in the outcome's own units --- otherwise the same
#'     model on an outcome recorded in hours, minutes or days would read three different ways. Set
#'     them with [set_color_breaks()] (`adj_ratio`, `adj_diff`, `adj_diff_std`). One pole means the
#'     model **strengthened** the effect (suppression), the other that it **attenuated** it,
#'     measured from the null so a protective and a risky effect read the same way.
#'   * `"between_groups"` --- with `tab_vars`, how far each group's effect sits from the **first**
#'     group's, on the same row: a per-predictor reading of effect modification. Pick the baseline
#'     group with `ref` keyed by the split variable (e.g. `ref = c(race = "Black")`). It also adds
#'     the aggregated interaction test to the footer (see `stats`).
#'
#'   The two are mutually exclusive (they share one per-cell slot). The gap is readable as a number
#'   with `display = "{est} ({gap})"`, and the HTML tooltip adds its interval and p-value.
#'
#'   **Significance.** Each measure tests its own gap, and `color_signif` then applies as usual. The
#'   two standard errors differ because they compare different things: two `tab_vars` groups are
#'   **different people**, so that gap's error comes from the two intervals the table already prints,
#'   while an adjustment compares two estimates fitted on the **same rows**, which are correlated, so
#'   its error comes from the difference of their influence functions (Weesie 1999; Mize, Doan & Long
#'   2019) --- design-based when there are weights or a design. The `"adjustment"` test runs only
#'   where a zero gap really means "no confounding", i.e. on a **collapsible** measure (a marginal
#'   effect, a risk ratio, an incidence-rate ratio, a linear mean difference). A **conditional odds
#'   ratio** is not collapsible --- adjusting it moves it away from 1 even when the added variable is
#'   independent of the exposure --- so there the colours stay descriptive, `color_signif` is
#'   ignored, and `tab_reg()` says so once; the same ruling is why a multinomial or ordinal
#'   *coefficient* column shows the observed effect but carries no test, while its marginal path
#'   does.
#'
#'   Read a coloured cell as "adjustment moved this effect", not as "this variable is a confounder",
#'   and read the pattern rather than the single cell: each is tested on its own, with no
#'   multiple-comparison correction. `vignette("tabxplor-reg")` gives the literature and the worked
#'   reading.
#' @param stars Logical (default `TRUE` for regression tables, where significance stars are
#'   standard). When `FALSE`, the per-cell p-value is dropped and no stars are shown (colours still
#'   read the interval).
#' @param na Which rows each model is fitted on --- the grain at which missing values are dropped.
#'   `"drop_by_outcome"` (default) gives every model **of one outcome** the same complete-case
#'   population (no `NA` on the outcome, on any predictor of any model in the call, or on a design
#'   variable); a second outcome keeps its own rows. That is what makes the comparisons honest: the
#'   observed columns are computed on exactly the model's rows, and nested models get equal N so the
#'   likelihood-ratio comparison can run instead of degrading to an AIC difference.
#'   `"drop_by_model"` lets each model use its own complete cases --- more rows, at the price of
#'   comparability: models fitted on different people get no observed effect at all. `"drop_all"`
#'   shares one population across the whole call, all outcomes included.
#' @param cleannames Logical. If `TRUE`, strips numeric prefixes from factor levels for display.
#'   Uses `getOption("tabxplor.cleannames")` when `NULL`.
#' @param subtext Optional character. A note shown below the table.
#' @param parallel Opt-in parallel build of the models of one call, using the (Suggests-only)
#'   \pkg{mirai} package: several `outcome`s, a `predictors` list, or the `tab_vars` groups. `NULL`
#'   (default) reads `getOption("tabxplor.parallel")` (off); `FALSE` forces serial; `TRUE` uses an
#'   auto worker count; an integer sets the number of worker processes. Byte-identical to the serial
#'   result. It pays off for **many, evenly sized** models against a survey-size data frame, and is a
#'   loss otherwise. A model comparison (`stats = "compare_*"`) is always serial and says so when
#'   asked: it is a test *between* the fits, so they are built together. The worker pool persists for
#'   the session; release it with [tab_parallel_stop()].
#' @return A `tabxplor_grouped_tab` (grouped by predictor), one effect column per model / outcome.
#'
#' @seealso [forest_plot()] draws the finished table --- every effect with its interval, its stars
#'   and its colour, and (with `empirical = TRUE`) the observed effect beside it with the margin of
#'   error of the gap. [reg_check_plots()] draws the model checks. [tab()] for cross-tables.
#'
#' @examples
#'   data <- gss_cat_data_formatting()
#'   # a subset keeps the examples fast: fitting these models on all 21,483 rows costs well over
#'   # CRAN's 5-second-per-topic budget. Use the full `data` in real analyses.
#'   reg_data <- head(data, 3000)
#'
#'   # logistic (odds ratios):
#'   tab_reg(reg_data, outcome = "married", predictors = c("race", "rincome"),
#'           family = "binomial")
#'
#' \donttest{
#'   # linear (mean differences), and the same model written as a formula:
#'   tab_reg(reg_data, outcome = "tvhours", predictors = c("rincome", "age"), family = "gaussian")
#'   tab_reg(reg_data, married ~ race + rincome, family = "binomial")
#'
#'   # the observed odds ratio beside the modelled one:
#'   tab_reg(reg_data, outcome = "married", predictors = c("race", "rincome"),
#'           family = "binomial", empirical = TRUE)
#'
#' # average marginal effects, as a RATIO: with a common outcome this is what a reader means by
#' # "x times more likely" -- an odds ratio is not. (Needs the marginaleffects package.)
#' if (requireNamespace("marginaleffects", quietly = TRUE)) {
#'   tab_reg(reg_data, outcome = "married", predictors = c("race", "rincome"),
#'                 family = "binomial", effect = "marginal", measure = "ratio", empirical = TRUE)
#' }
#' # the CONDITIONAL risk ratio: measure = "ratio" on a binary outcome fits the modified Poisson
#' # (Zou 2004), a log link with robust standard errors. Ask for the measure, not the distribution.
#'   tab_reg(reg_data, outcome = "married", predictors = c("race", "rincome"),
#'                 measure = "ratio", empirical = TRUE)
#' # multinomial (nominal 3+ level): one OR column per outcome category vs the baseline
#'   tab_reg(reg_data, outcome = "party3", predictors = c("race", "age"),
#'                 family = "multinomial", outcome_level = c(party3 = "3-Republican"))
#' # ordinal (proportional-odds): one cumulative-OR column
#'   tab_reg(reg_data, outcome = "rincome", predictors = c("race", "age"), family = "ordinal")
#' }
#'
#' @section Out of scope:
#' `tab_reg()` covers linear, logistic, Poisson, multinomial and ordinal models, with survey designs.
#' Three families of models are deliberately **not** supported, and are unlikely to be: **survival /
#' Cox** models, **mixed / multilevel** models, and pooling over **multiply-imputed** datasets. Fit
#' those with their own packages.
#'
#' @references
#' Clogg, C. C., Petkova, E. & Haritou, A. (1995). Statistical Methods for Comparing Regression
#' Coefficients between Models. *American Journal of Sociology*, 100(5), 1261-1293 (with
#' Allison, P. D. (1995), *ibid.* 1294-1305) --- the comparison `color = "adjustment"` implements.
#'
#' Karlson, K. B., Holm, A. & Breen, R. (2012). Comparing Regression Coefficients Between Same-sample
#' Nested Models Using Logit and Probit. *Sociological Methodology*, 42(1), 286-313 --- the KHB
#' decomposition, separating confounding from rescaling in nested logit models.
#'
#' Zou, G. (2004). A Modified Poisson Regression Approach to Prospective Studies with Binary Data.
#' *American Journal of Epidemiology*, 159(7), 702-706.
#'
#' Altman, D. G. & Bland, J. M. (2003). Interaction revisited: the difference between two estimates.
#' *BMJ*, 326, 219 --- the `color = "between_groups"` test.
#'
#' Santos Silva, J. M. C. & Tenreyro, S. (2006). The log of gravity. *The Review of Economics and
#' Statistics*, 88(4), 641-658 --- `measure = "ratio"` on a continuous outcome.
#'
#' @param ... Not a user argument. It carries the internal `.fit_cache` (the jamovi live UI's fit
#'   cache environment), and it is what makes every argument removed or renamed while `tab_reg()` was
#'   in development --- `exponentiate`, `at`, `estimate_display`, `dependent`, `split_var`,
#'   `reference`, `method`, `compare`, `baseline`, `inverse_two_level_factors`, and the `effect`
#'   values `"ame"` / `"ame_ratio"` --- give an error naming its replacement, rather than R's bare
#'   "unused argument".
#' @eval reg_words_rd()
#' @eval reg_measures_rd()
#' @export
tab_reg <- function(data, outcome, predictors = NULL, tab_vars = NULL, wt = NULL,
                    family = "auto", effect = "coefficient", measure = "auto",
                    trials = NULL, empirical = FALSE, n = NULL,
                    color = TRUE, color_signif = NULL, stars = TRUE,
                    conf_level = NULL, ci_method = NULL,
                    outcome_level = NULL, ref = NULL,
                    multiplier = "sd", shape = NULL, stats = NULL,
                    na = c("drop_by_outcome", "drop_by_model", "drop_all"),
                    display = NULL, cleannames = NULL, subtext = "", parallel = NULL, ...) {
  # `.fit_cache` (the jamovi live-UI cache env) and `.levels_collapse` (the level-merge spec shared
  # with tab()) are jamovi-internal plumbing riding `...`; neither is a user argument.
  .dots      <- list(...)
  .fit_cache <- .dots[[".fit_cache"]]
  .levels_collapse <- new_lvl_collapse(.dots[[".levels_collapse"]])
  tab_check_dots(.dots, "tab_reg")
  # ONE `ci_method` grammar for both producers: the named vector tab() takes, whose fifth slot is
  # this one's. A bare "profile" means that slot -- a regression has only one interval to choose for.
  if (is.character(ci_method) && is.null(names(ci_method)) && length(ci_method) == 1L)
    ci_method <- stats::setNames(ci_method, "model")
  method <- resolve_ci_method(ci_method, fn = "tab_reg")[["model"]]
  # ⚠ the un-supplied default is the whole vector, so its LENGTH is "the user did not choose" --
  # read before match.arg() collapses it, so the `na` advice fires on a choice and not on the
  # default it would advise.
  na_explicit <- length(na) == 1L
  na      <- match.arg(na)
  cleannames <- resolve_cleannames(cleannames)


  # A models LIST and SEVERAL outcomes -> one model-comparison table per outcome, returned as a
  # `tabxplor_tabs` list (so tab_export("xl") writes one sheet each). The outcomes loop on the
  # OUTSIDE and each iteration recurses into the ordinary single-outcome comparison, so every
  # argument, message and family detection is reused. It sits BEFORE the design extraction, so a
  # survey design recurses intact.
  #
  # ⚠ `!rlang::is_formula(outcome)` is load-bearing: a two-sided formula is a call of length 3, so
  # `length(outcome) > 1L` is TRUE for every one of them, and without this guard a formula recurses
  # over `~`, its lhs and its rhs instead of reaching the teachable error below.
  if (!rlang::is_formula(outcome) && is.list(predictors) &&
      !inherits(predictors, "formula") && length(outcome) > 1L) {
    if (!is.null(trials) && !isTRUE(trials) && is.null(names(trials)) &&
        length(trials) > 1L && length(trials) != length(outcome)) {
      cli::cli_abort(c("{.arg trials} must be length 1, one per outcome, or a named vector.",
                       "x" = "Got {length(trials)} for {length(outcome)} outcomes."))
    }
    # Every per-outcome argument is SLICED the way `trials` is, through the one slicer
    # (reg_per_outcome()); every whole-call one is forwarded. The recursion itself is the namespaced
    # reg_build_outcome(), so this map IS tab_pmap() -- the cleanest of the three parallel axes,
    # each unit returning a FINISHED table with no cross-unit step at all.
    # ⚠ `parallel = FALSE` inside the unit: THE NESTING RULE, stated once in tab_pmap()'s
    # everywhere() block, where it is also enforced for any site that forgets it.
    args <- purrr::map(seq_along(outcome), function(i) {
      d   <- outcome[[i]]
      tri <- if (is.null(trials) || isTRUE(trials)) trials
             else if (!is.null(names(trials)))      unname(trials[d])
             else if (length(trials) == 1L)         as.numeric(trials)
             else                                   trials[[i]]
      list(outcome = d, predictors = predictors, wt = wt,
           family  = reg_per_outcome(family,  d, i, "auto"),
           effect  = reg_per_outcome(effect,  d, i, "coefficient"),
           measure = reg_per_outcome(measure, d, i, "auto"),
           trials = tri, conf_level = conf_level, ci_method = ci_method,
           ref = ref, outcome_level = outcome_level,
           tab_vars = tab_vars, multiplier = multiplier, shape = shape,
           empirical = empirical, n = n,
           stats = stats,
           display = display, color = color, color_signif = color_signif,
           stars = stars, na = na, cleannames = cleannames, subtext = subtext,
           parallel = FALSE, .fit_cache = .fit_cache,
           .levels_collapse = .levels_collapse)
    })
    tabs <- tab_pmap(list(args = args), "reg_build_outcome", .ship = list(data = data),
                     .names = outcome,
                     workers = tab_parallel_workers(parallel, .fit_cache))
    names(tabs) <- outcome
    return(new_tabxplor_tabs(tabs))
  }

  # THE argument boundary, in one call (R/reg-resolve.R): six stages in the one order they may run
  # in, every check and every rewrite of `data` among them.
  a <- reg_resolve_args(
    data, outcome, predictors, tab_vars = tab_vars, wt = wt,
    family = family, effect = effect, measure = measure, trials = trials,
    empirical = empirical, n = n, color = color, color_signif = color_signif,
    stars = stars, conf_level = conf_level, method = method, ref = ref,
    outcome_level = outcome_level, multiplier = multiplier,
    shape = shape, stats = stats,
    na = na, na_explicit = na_explicit, display = display, cleannames = cleannames,
    subtext = subtext, .fit_cache = .fit_cache, levels_collapse = .levels_collapse)

  res <- reg_build(a$data, a$specs, a$shared, tab_vars = tab_vars,
                   .fit_cache = .fit_cache, ref = ref, reref = a$reref, parallel = parallel)

  # The p-value is stars-only -- colours read the CI bounds -- so `stars = FALSE` just drops it.
  if (!isTRUE(stars)) {
    for (nm in names(res)[vapply(res, is_fmt, logical(1))]) {
      res[[nm]] <- set_pvalue(res[[nm]], NA_real_)
    }
  }

  # THE TABLE'S OWN MODEL RECORD: what drives the title / caption, the "Model:" footer lines and the
  # colour legend -- the table-level narrative only, since the legend reads each column's own
  # `model_family` attribute for the per-column word. It stores the ESTIMAND (`measures` and
  # `effects` per outcome) because a table must remember what it estimated, or a refit silently
  # changes it. Every field is READ OFF the boundary's record rather than recomputed here.
  reg_call_record <- list(
    family = a$families[[1]], families = a$families,
    effect = a$est$effect, measure = a$est$measure, eff_word = a$eff_word,
    measures = vapply(a$ests, function(e) e$measure, character(1)),
    effects  = vapply(a$ests, function(e) e$effect,  character(1)),
    outcome = a$outcome, positive_level = a$positive_levels, predictors = a$union_predictors,
    # ⚠ the predictor-kind map is STORED, never re-derived from the rendered table: the only implicit
    # marker (a numeric row's `level == var`) is already broken by `cleannames` and by the multiplier
    # relabel. `multiplier` records the RESOLVED scaling used, frozen SDs included, so the footer and
    # legend can name the unit.
    predictor_types = reg_predictor_types(a$data, a$union_predictors), multiplier = a$multiplier,
    # THE RECIPE reg_check_plots() refits from: the specs plus the few scalars reg_fit() takes, a few
    # KB of strings. Deliberately NOT the fits -- they are megabytes each, and a refit through the
    # very fitter the table came from is both cheaper and impossible to drift from.
    # ⚠ the RESOLVED conf_level, read back off the boundary record: the formal is NULL on every
    # producer, so tab_reg()'s own local is still NULL here.
    fit_spec = list(specs = a$specs, method = method, conf_level = a$shared$conf_level,
                    outcome_level = outcome_level,
                    na_shared_vars = a$na_shared_vars, shape_terms = a$shape_terms,
                    multiplier = a$multiplier, effect = a$est$effect, measure = a$est$measure,
                    wt = a$wt_disp, design_vars = reg_design_vars(a$design_spec)),
    # which observed counterpart each outcome has (NA = none), and where it went -- stored so the
    # footer can word the in-cell bracket.
    emp_mode = a$empirical,
    crude_keys = if (emp_on(a$empirical))
      stats::setNames(purrr::map_chr(a$specs, ~ .$crude_key), purrr::map_chr(a$specs, "outcome"))
      else stats::setNames(rep(NA_character_, length(a$specs)),
                           purrr::map_chr(a$specs, "outcome")),
    tab_vars = tab_vars, comparison = a$is_comparison, wt = a$wt_disp
  )
  # The model record IS this table's `spec$call` -- "how was this table made", the slot every
  # producer has. ⚠ `conf_level` is deliberately absent from it: the level lives on every COLUMN
  # (get_conf_level() is what consumers read), so a table-wide copy could only ever disagree.
  set_reg_call(res, reg_call_record)
}
