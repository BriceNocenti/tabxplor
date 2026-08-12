# PURPOSE: the plots of a tab_reg() table -- reg_check_plots() (the model checks, drawn) and
#   or_plot() (the odds-ratio forest plot).
#
# ROLE. reg_check_plots() is TEACHING ONLY, and its documentation says so in the first sentence: every
#   decision-grade number is already a footer row of the table, for every model column, in every export
#   (R/reg-assumptions.R). This function exists to show a class what a violation LOOKS like, and to let
#   a careful reader look closer. Nothing in the workflow requires calling it.
#
# ONE ENGINE, TWO ENTRY FORMS (ruling R1). A tab_reg() table + its data, or a bare fit. Both reduce to
#   the same quadruple (fit, frame, family, weights), so the panel builders never branch on the form.
#   The table form REFITS through reg_fit() itself, from the ~4 KB recipe stored in reg_meta$fit_spec:
#   a 60 ms teaching cost, against the ~10 MB per retained fit that was the measured cause of the
#   Phase-o jamovi freeze. There is no second fitting path to keep in sync.
#
# THE PANEL SET IS REG_CHECKS. A panel and a footer row are the same check, so their titles, their
#   applicable families and the `check =` vocabulary all come from that one table; two of its rows are
#   TAUGHT BUT NEVER SCORED (residuals, normality -- measured non-discriminating as verdicts, canonical
#   as lessons) and say so by carrying no discriminator.
#
# KEY CONSTRAINTS:
#   - ggplot2 + gridExtra are Suggests -> every entry point guards with requireNamespace().
#   - or_plot() reads the fmt fields, so it stays in sync with the console/exports for free; it never
#     re-fits a model.
#   - NEVER geom_smooth(method = "auto") -- see the warning in R/reg-assumptions.R's primitives.
# See: dev/regression_assumptions_plots.md.

# Guard the Suggests packages a plot needs, with an install hint.
reg_plot_deps <- function(pkgs = c("ggplot2", "gridExtra")) {
  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      cli::cli_abort(c("{.pkg {pkg}} is required for this plot.",
                       "i" = 'Install it with {.code install.packages("{pkg}")}.'))
    }
  }
  invisible(TRUE)
}

# === SECTION: the shared theme seam =================================================================

# THE plot theme, from the same `tx_chrome_hex()` vocabulary the tables use (z11: light / dark /
# print), so a diagnostic panel beside a table is the same object in the same clothes. It replaced the
# five hard-coded "#c00000" literals: `theme = "print"` matters, because a diagnostic panel is exactly
# what ends up in a thesis appendix in greyscale.
#' @keywords internal
reg_plot_colors <- function(theme = NULL) {
  th <- tx_resolve_theme(if (is.null(theme))
    tx_getOption(c("tabxplor.export_theme", "tabxplor.theme"), "light") else theme)
  if (identical(th, "auto")) th <- "light"
  ch <- tx_chrome_hex(th)
  list(theme = th, text = ch$text, grey = ch$grey, bg = ch$bg,
       # the accent: a hue under colour themes, pure black under `print` (a greyscale panel leans on
       # line TYPE, not on a hue that photocopies to the same grey as the data)
       accent = if (identical(th, "print")) "#000000" else "#c00000",
       point  = if (identical(th, "dark")) "#8fb8dd" else if (identical(th, "print")) "#000000"
                else "#33648c")
}

#' @keywords internal
reg_plot_theme <- function(cols) {
  ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold", size = 10, colour = cols$text),
      plot.subtitle = ggplot2::element_text(size = 8.5, colour = cols$grey),
      text          = ggplot2::element_text(colour = cols$text),
      axis.text     = ggplot2::element_text(colour = cols$grey),
      plot.background   = ggplot2::element_rect(fill = cols$bg, colour = NA),
      panel.background  = ggplot2::element_rect(fill = cols$bg, colour = NA),
      legend.background = ggplot2::element_rect(fill = cols$bg, colour = NA),
      strip.background  = ggplot2::element_rect(fill = cols$bg, colour = cols$grey),
      strip.text        = ggplot2::element_text(colour = cols$text, size = 8))
}


# === SECTION: getting a fit ==========================================================================

# The (fit, frame, family, label, ...) quadruples a call is about. A bare model gives one; a tab_reg()
# table gives one PER MODEL COLUMN (ruling R10: one call diagnoses every model / outcome, faceted --
# when that is a wall, the user passes fewer models, which is a legible failure mode where a silent
# "first model only" is not).
#' @keywords internal
reg_plot_fits <- function(x, data = NULL) {
  if (!inherits(x, "tbl_df") && !is.data.frame(x)) {
    # the secondary form: a bare lm / glm / svyglm / polr / multinom / svyolr
    fr <- tryCatch(stats::model.frame(x), error = function(e) NULL)
    return(list(list(fit = x, data = if (is.null(data)) fr else data,
                     family = reg_plot_family_of(x), dependent = reg_plot_dep_of(x),
                     predictors = reg_plot_preds_of(x), trials = NULL, wt = NULL, design = NULL,
                     label = gettext("Model"))))
  }
  meta <- get_reg_meta(x)
  fs   <- meta$fit_spec
  if (is.null(fs)) {
    cli::cli_abort(c("{.arg x} is not a {.fn tab_reg} table (no model record).",
                     "i" = "Pass a {.fn tab_reg} result and its data, or a fitted model."))
  }
  if (is.null(data)) {
    cli::cli_abort(c("{.arg data} is required with a {.fn tab_reg} table.",
                     "i" = "Diagnostics need the microdata; the table stores only the recipe.",
                     "x" = "e.g. {.code reg_check_plots(t, gss_simple)}."))
  }
  svy  <- svy_unwrap_data(data, "reg_check_plots")
  if (!is.null(svy)) data <- svy$data
  ds   <- list(design = if (is.null(svy)) NULL else svy$spec$design,
               wt = if (is.null(svy)) fs$wt else svy$spec$wt)
  if (!is.null(ds$wt) && is.na(ds$wt)) ds$wt <- NULL
  if (!is.null(ds$wt) && !ds$wt %in% names(data)) ds$wt <- NULL
  nobs_tab <- reg_plot_nobs(x)
  purrr::imap(fs$specs, function(sp, i) {
    f <- tryCatch(suppressMessages(suppressWarnings(reg_fit(
      data, sp$dependent, sp$predictors, sp$family, ds, sp$do_exp,
      if (is.null(sp$inverse)) fs$inverse_two_level_factors else sp$inverse,
      fs$conf_level, fs$method, trials = sp$trials, formula = sp$formula,
      multiplier = fs$multiplier, drop_extra = fs$na_shared_vars,
      add_terms = reg_shape_add(fs$shape_terms, sp$predictors)))),
      error = function(e) NULL)
    if (is.null(f)) return(NULL)
    # THE guard, and it is required rather than optional: a diagnostic plot of the wrong model is
    # worse than no plot. The table already carries each model's N (the `n` footer row), so this needs
    # no extra storage -- and it stays silent when `stats = FALSE` left nothing to compare against.
    n_i <- if (length(nobs_tab) >= i) nobs_tab[[i]] else NA_real_
    if (is.finite(n_i) && f$nobs != n_i) {
      cli::cli_abort(c("{.arg data} does not reproduce the model in {.arg x}.",
                       "x" = "Model {.val {sp$label}} was fitted on {n_i} rows; this data gives {f$nobs}.",
                       "i" = "Pass the same data (and the same weights / design) the table was built from."))
    }
    list(fit = f$fit, data = f$data, family = sp$family, dependent = sp$dependent,
         predictors = sp$predictors, trials = sp$trials, wt = ds$wt, design = ds$design,
         positive_level = f$positive_level, label = sp$label)
  }) |> purrr::compact()
}

# The N of each fit, off the table's own `n` footer rows (NA where `stats = FALSE` stored none).
#' @keywords internal
reg_plot_nobs <- function(x) {
  tt <- get_test(x)
  if (is.null(tt) || !nrow(tt)) return(numeric(0))
  as.numeric(tt$n[tt$test == "n"])
}

# The (family, dependent, predictors) of a BARE fit -- the secondary form's only inference.
#' @keywords internal
reg_plot_family_of <- function(fit) {
  if (inherits(fit, "polr") || inherits(fit, "svyolr")) return("ordinal")
  if (inherits(fit, "multinom")) return("multinomial")
  fam <- tryCatch(stats::family(fit)$family, error = function(e) NULL)
  if (is.null(fam)) return("gaussian")
  if (grepl("binomial", fam)) "binomial" else if (grepl("poisson", fam)) "poisson" else "gaussian"
}
#' @keywords internal
reg_plot_dep_of <- function(fit)
  tryCatch(all.vars(stats::formula(fit))[[1L]], error = function(e) NA_character_)
#' @keywords internal
reg_plot_preds_of <- function(fit)
  tryCatch(setdiff(all.vars(stats::formula(fit)), reg_plot_dep_of(fit)), error = function(e) character(0))


# === SECTION: the panels ============================================================================

# ONE builder per panel key, dispatched here. The table (REG_CHECKS) says WHICH panels exist and for
# which families; this switch says HOW each is drawn. Every builder takes the list of contexts and
# returns a ggplot (or NULL when the data cannot support it).
#' @keywords internal
reg_panel_build <- function(key, ctxs, cols, opts) {
  switch(key,
         linearity       = reg_panel_linearity(ctxs, cols, opts),
         residuals       = reg_panel_residuals(ctxs, cols, opts),
         normality       = reg_panel_normality(ctxs, cols, opts),
         dispersion      = reg_panel_dispersion(ctxs, cols, opts),
         influence       = reg_panel_influence(ctxs, cols, opts),
         collinearity    = reg_panel_collinearity(ctxs, cols, opts),
         proportionality = reg_panel_proportionality(ctxs, cols, opts),
         NULL)
}

# The title / subtitle of a panel: the ASSUMPTION from REG_CHECKS, never the plot type (SS25) -- the
# student meets the same word in the footer row, in the argument and here.
#' @keywords internal
reg_panel_title <- function(key) gettext(REG_CHECKS[[key]]$noun)

# multi-model faceting: only when there IS more than one, so a single model keeps a clean panel.
#' @keywords internal
reg_panel_facet <- function(g, df, ncol = NULL) {
  if (length(unique(df$model)) < 2L) return(g)
  g + ggplot2::facet_wrap(~ model, ncol = ncol)
}

# 1. LINEARITY -- the observed binned curve of each continuous predictor against the STRAIGHT line the
# model assumes. The comparator must be a straight lm, never a loess: the assumption IS linearity, so a
# smoother would trace the curvature and hide the very departure the panel exists to show.
reg_panel_linearity <- function(ctxs, cols, opts) {
  rows <- purrr::list_rbind(purrr::map(ctxs, function(cx) {
    num <- reg_numeric_preds(cx$data, cx$predictors)
    if (!is.null(opts$predictors)) num <- intersect(num, opts$predictors)
    if (!length(num)) return(NULL)
    ly <- rd_link_y(cx$data[[cx$dependent]], cx$family, cx$trials, cx$positive_level)
    w  <- if (!is.null(cx$wt) && cx$wt %in% names(cx$data)) cx$data[[cx$wt]] else NULL
    purrr::list_rbind(purrr::map(num, function(v) {
      # Last Phase z16-iv (W-G.4): the band takes the DESIGN variance when the user handed a
      # svydesign to reg_check_plots(), the exact flat closed form on a plain weight column, and is
      # unchanged (n) unweighted.
      b <- rd_bin(cx$data[[v]], ly$y, w, opts$nbins, ly$link,
                  design = cx$design, des_rows = cx$data[[svy_row_col]])
      if (is.null(b)) return(NULL)
      dplyr::mutate(b, predictor = v, model = cx$label, ylab = ly$lab)
    }))
  }))
  if (is.null(rows) || !nrow(rows)) return(NULL)
  g <- ggplot2::ggplot(rows, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$y - 2 * .data$se, ymax = .data$y + 2 * .data$se),
                         fill = cols$grey, alpha = 0.25, na.rm = TRUE) +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
                         colour = cols$accent, linetype = "dashed", linewidth = 0.6, na.rm = TRUE) +
    ggplot2::geom_line(colour = cols$point, linewidth = 0.7, na.rm = TRUE) +
    ggplot2::geom_point(ggplot2::aes(size = .data$n), colour = cols$point, na.rm = TRUE) +
    ggplot2::scale_size(range = c(0.6, 2.4), guide = "none") +
    ggplot2::labs(title = reg_panel_title("linearity"),
                  subtitle = gettext("Observed curve (10 bins, +/-2 SE) against the straight line the model fits."),
                  x = NULL, y = rows$ylab[[1L]]) +
    reg_plot_theme(cols)
  facets <- if (length(unique(rows$model)) > 1L) ~ model + predictor else ~ predictor
  g + ggplot2::facet_wrap(facets, scales = "free_x", ncol = opts$facet_ncol)
}

# 2. RESIDUALS -- binned residuals against the fitted value. The classic lesson about why a RAW
# residual is useless for a binary outcome (it takes exactly two values given p-hat), and the reason
# every non-gaussian family here uses a randomised quantile residual instead.
reg_panel_residuals <- function(ctxs, cols, opts) {
  rows <- purrr::list_rbind(purrr::map(ctxs, function(cx) {
    r <- rd_resid(cx$fit, cx$family, cx$data[[cx$dependent]], cx$trials, opts$seed)
    f <- tryCatch(as.numeric(stats::fitted(cx$fit)), error = function(e) NULL)
    if (is.null(r) || is.null(f) || length(f) != length(r)) return(NULL)
    b <- rd_bin(f, r, NULL, max(5L, min(60L, floor(sqrt(length(r))))), "identity")
    if (is.null(b)) return(NULL)
    dplyr::mutate(b, model = cx$label)
  }))
  if (is.null(rows) || !nrow(rows)) return(NULL)
  g <- ggplot2::ggplot(rows, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = cols$grey) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = -2 * .data$se, ymax = 2 * .data$se),
                         fill = cols$grey, alpha = 0.2, na.rm = TRUE) +
    ggplot2::geom_point(colour = cols$point, size = 1.2, na.rm = TRUE) +
    ggplot2::labs(title = reg_panel_title("residuals"),
                  subtitle = gettext("Binned residuals against the fitted value; ~95 % should sit in the band."),
                  x = gettext("Fitted value"), y = gettext("Mean residual")) +
    reg_plot_theme(cols)
  reg_panel_facet(g, rows, opts$facet_ncol)
}

# 3. NORMALITY -- the Q-Q plot of the dispatched residual, against the ANALYTIC pointwise band.
reg_panel_normality <- function(ctxs, cols, opts) {
  rows <- purrr::list_rbind(purrr::map(ctxs, function(cx) {
    r <- rd_resid(cx$fit, cx$family, cx$data[[cx$dependent]], cx$trials, opts$seed)
    q <- if (is.null(r)) NULL else rd_qq(r, opts$conf, min(opts$max_points, 400L))
    if (is.null(q)) return(NULL)
    dplyr::mutate(q, model = cx$label)
  }))
  if (is.null(rows) || !nrow(rows)) return(NULL)
  g <- ggplot2::ggplot(rows, ggplot2::aes(x = .data$theoretical, y = .data$sample)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lo, ymax = .data$hi),
                         fill = cols$grey, alpha = 0.2, na.rm = TRUE) +
    ggplot2::geom_abline(slope = 1, intercept = 0, colour = cols$accent,
                         linetype = "dashed", linewidth = 0.6) +
    ggplot2::geom_point(colour = cols$point, size = 0.7, alpha = 0.6, na.rm = TRUE) +
    ggplot2::labs(title = reg_panel_title("normality"),
                  subtitle = gettext("Quantile residuals against the normal. The band is POINTWISE: about 5 % of points fall outside it under a correct model."),
                  x = gettext("Theoretical quantiles"), y = gettext("Quantile residuals")) +
    reg_plot_theme(cols)
  reg_panel_facet(g, rows, opts$facet_ncol)
}

# 4. DISPERSION -- the model's own standard errors against the robust (sandwich) ones, coefficient by
# coefficient. It is exactly the footer row, un-maximised: the row prints the largest of these points'
# distance from the diagonal.
reg_panel_dispersion <- function(ctxs, cols, opts) {
  rows <- purrr::list_rbind(purrr::map(ctxs, function(cx) {
    se <- reg_check_model_se(cx$fit)
    cif <- reg_coef_if_maker(cx$fit)
    if (is.null(se) || is.null(cif)) return(NULL)
    des <- reg_check_design(cx$fit)
    rb <- vapply(seq_along(se), function(j) {
      e <- rep(0, length(se)); e[[j]] <- 1
      d <- cif(e)
      if (is.null(d)) return(NA_real_)
      reg_if_se(d, des)
    }, numeric(1))
    nm <- tryCatch(names(stats::coef(cx$fit)), error = function(e) NULL)
    tibble::tibble(term = if (length(nm) == length(se)) nm else as.character(seq_along(se)),
                   model_se = se, robust_se = rb, model = cx$label)
  }))
  if (is.null(rows) || !nrow(rows) || all(is.na(rows$robust_se))) return(NULL)
  g <- ggplot2::ggplot(rows, ggplot2::aes(x = .data$model_se, y = .data$robust_se)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, colour = cols$accent,
                         linetype = "dashed", linewidth = 0.6) +
    ggplot2::geom_point(colour = cols$point, size = 1.6, na.rm = TRUE) +
    ggplot2::labs(title = reg_panel_title("dispersion"),
                  subtitle = gettext("Robust against model standard errors, one point per coefficient. On the line = the family's variance assumption holds."),
                  x = gettext("Model SE"), y = gettext("Robust SE")) +
    reg_plot_theme(cols)
  reg_panel_facet(g, rows, opts$facet_ncol)
}

# 5. INFLUENCE -- the per-observation version of the footer row: max_j |dfbeta_ij| / SE_j, i.e. how far
# one respondent moves the coefficient it moves most, in that coefficient's own standard errors.
reg_panel_influence <- function(ctxs, cols, opts) {
  rows <- purrr::list_rbind(purrr::map(ctxs, function(cx) {
    se  <- reg_check_model_se(cx$fit)
    cif <- reg_coef_if_maker(cx$fit)
    if (is.null(se) || is.null(cif)) return(NULL)
    m <- NULL
    for (j in seq_along(se)) {
      e <- rep(0, length(se)); e[[j]] <- 1
      d <- cif(e)
      if (is.null(d)) return(NULL)
      v <- abs(as.numeric(d)) / se[[j]]
      m <- if (is.null(m)) v else pmax(m, v)
    }
    keep <- rd_thin(m, opts$max_points, opts$seed)
    tibble::tibble(index = keep, dfbeta = m[keep], model = cx$label)
  }))
  if (is.null(rows) || !nrow(rows)) return(NULL)
  g <- ggplot2::ggplot(rows, ggplot2::aes(x = .data$index, y = .data$dfbeta)) +
    ggplot2::geom_hline(yintercept = 0.25, colour = cols$accent, linetype = "dashed",
                        linewidth = 0.6) +
    ggplot2::geom_point(colour = cols$point, size = 0.7, alpha = 0.5, na.rm = TRUE) +
    ggplot2::labs(title = reg_panel_title("influence"),
                  subtitle = gettext("How far one respondent moves a coefficient, in its own standard errors. Influence is leverage x outlyingness, not outlyingness."),
                  x = gettext("Observation"), y = gettext("max |dfbetas|")) +
    reg_plot_theme(cols)
  reg_panel_facet(g, rows, opts$facet_ncol)
}

# 6. COLLINEARITY -- the VIF of every term, on the 5 / 10 ladder every textbook uses.
reg_panel_collinearity <- function(ctxs, cols, opts) {
  if (!requireNamespace("car", quietly = TRUE)) return(NULL)
  rows <- purrr::list_rbind(purrr::map(ctxs, function(cx) {
    v <- tryCatch(suppressWarnings(car::vif(cx$fit)), error = function(e) NULL)
    if (is.null(v) || !length(v)) return(NULL)
    val <- if (is.matrix(v)) { if (ncol(v) >= 3L) v[, 3]^2 else v[, 1] } else as.numeric(v)
    nm  <- if (is.matrix(v)) rownames(v) else names(v)
    tibble::tibble(term = if (is.null(nm)) as.character(seq_along(val)) else nm,
                   vif = as.numeric(val), model = cx$label)
  }))
  if (is.null(rows) || !nrow(rows)) return(NULL)
  g <- ggplot2::ggplot(rows, ggplot2::aes(x = stats::reorder(.data$term, .data$vif), y = .data$vif)) +
    ggplot2::geom_col(fill = cols$point, width = 0.6) +
    ggplot2::geom_hline(yintercept = c(5, 10), colour = cols$accent,
                        linetype = c("dashed", "dotted"), linewidth = 0.6) +
    ggplot2::coord_flip() +
    ggplot2::labs(title = reg_panel_title("collinearity"),
                  subtitle = gettext("Variance inflation per term (5 and 10 are the usual thresholds). It biases nothing -- it widens intervals."),
                  x = NULL, y = gettext("VIF")) +
    reg_plot_theme(cols)
  reg_panel_facet(g, rows, opts$facet_ncol)
}

# 7. PROPORTIONALITY (ordinal) -- the empirical cumulative logit of each cut, per predictor level. The
# proportional-odds assumption says these lines are PARALLEL; the Brant p in the footer tests it.
reg_panel_proportionality <- function(ctxs, cols, opts) {
  rows <- purrr::list_rbind(purrr::map(ctxs, function(cx) {
    if (cx$family != "ordinal") return(NULL)
    y  <- as.factor(cx$data[[cx$dependent]])
    lv <- levels(y)
    if (length(lv) < 3L) return(NULL)
    fp <- reg_factor_preds(cx$data, cx$predictors)
    if (!length(fp)) return(NULL)
    w  <- if (!is.null(cx$wt) && cx$wt %in% names(cx$data)) cx$data[[cx$wt]] else rep(1, nrow(cx$data))
    purrr::list_rbind(purrr::map(fp, function(v) {
      g <- as.factor(cx$data[[v]])
      purrr::list_rbind(purrr::map(seq_len(length(lv) - 1L), function(k) {
        above <- as.integer(as.integer(y) > k)
        num <- as.numeric(tapply(w * above, g, sum))
        den <- as.numeric(tapply(w, g, sum))
        p   <- (num + 0.5) / (den + 1)
        tibble::tibble(level = levels(g), logit = log(p / (1 - p)),
                       cut = paste0("> ", lv[[k]]), predictor = v, model = cx$label)
      }))
    }))
  }))
  if (is.null(rows) || !nrow(rows)) return(NULL)
  g <- ggplot2::ggplot(rows, ggplot2::aes(x = .data$level, y = .data$logit,
                                          group = .data$cut, colour = .data$cut)) +
    ggplot2::geom_line(na.rm = TRUE) + ggplot2::geom_point(size = 1.2, na.rm = TRUE) +
    ggplot2::labs(title = reg_panel_title("proportionality"),
                  subtitle = gettext("One line per cut of the outcome. Proportional odds means they are parallel."),
                  x = NULL, y = gettext("Empirical cumulative logit"), colour = NULL) +
    reg_plot_theme(cols) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
  facets <- if (length(unique(rows$model)) > 1L) ~ model + predictor else ~ predictor
  g + ggplot2::facet_wrap(facets, scales = "free_x", ncol = opts$facet_ncol)
}


# === SECTION: reg_check_plots() =====================================================================

#' Diagnostic plots of a regression model
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' **A teaching companion, not a decision tool.** Every verdict these panels illustrate is already a
#' row in the table's own footer --- for every model column, in every export, with no plotting package
#' installed (see the `stats` argument of [tab_reg()]). This function exists to *show what a violation
#' looks like*, and to let a careful reader look closer.
#'
#' One call diagnoses **every model** in the table (each outcome, each compared model), drawn as
#' facets. Pass a [tab_reg()] table plus the data it was built from, or a fitted model directly.
#'
#' @param x A [tab_reg()] table, or a fitted model (`lm` / `glm` / `svyglm` / `polr` / `multinom` /
#'   `svyolr`).
#' @param data The data frame or `survey::svydesign` the table was built from. Required with a table
#'   (the table stores a ~4 KB recipe, never the fitted models); ignored with a bare model.
#' @param check Which panels to draw. `"auto"` (default) draws every check that applies to the
#'   model's family; `"all"` is a synonym; or name them: any of `"linearity"`, `"residuals"`,
#'   `"normality"`, `"dispersion"`, `"influence"`, `"collinearity"`, `"proportionality"` --- the same
#'   words the footer rows and [tab_reg()]'s `stats` argument use.
#' @param predictors Optional: restrict the linearity panel to these continuous predictors.
#' @param ncol Number of panel columns in the assembled grid (default: 3, or fewer with few panels).
#' @param facet_ncol Number of facet columns *inside* a panel (default: let \pkg{ggplot2} choose).
#' @param theme `"light"`, `"dark"` or `"print"` (greyscale, for a thesis appendix). Defaults to
#'   `options("tabxplor.theme")`, like the table exporters.
#' @param lang Language of the titles and captions (`"en"`, `"fr"`, ...). Defaults to
#'   `options("tabxplor.lang")`.
#' @param max_points Thin the raw-point layers to about this many observations (statistics, bands and
#'   verdicts are always computed on the full data; the thinning keeps the extremes).
#' @param nbins Bins of the linearity panel's observed curve (default 10, as in the row sparklines).
#' @param conf Confidence level of the Q-Q band. Default `0.95`.
#' @param seed Seed of the randomised quantile residuals (`NULL` = a fresh draw each time, the honest
#'   way to check that a pattern is not a randomisation artefact).
#' @param ... Unused, for future extension.
#'
#' @return Invisibly, the assembled `gtable` (drawn on the current graphics device).
#'
#' @seealso [tab_reg()] and its `stats` argument (the same checks as footer rows), [or_plot()].
#'
#' @examples
#' # \donttest: building a multi-panel ggplot grid costs a few seconds of CPU.
#' \donttest{
#' d <- forcats::gss_cat |>
#'   dplyr::mutate(married = factor(dplyr::if_else(marital == "Married",
#'                                                 "Married", "Not married")))
#' if (requireNamespace("ggplot2", quietly = TRUE) &&
#'     requireNamespace("gridExtra", quietly = TRUE)) {
#'   t <- tab_reg(d, "married", c("race", "age"), family = "binomial")
#'   reg_check_plots(t, d)
#' }
#' }
#' @export
reg_check_plots <- function(x, data = NULL, check = "auto", predictors = NULL,
                            ncol = NULL, facet_ncol = NULL, theme = NULL, lang = NULL,
                            max_points = 2000L, nbins = 10L, conf = 0.95,
                            seed = 20260810, ...) {
  reg_plot_deps()
  ctxs <- reg_plot_fits(x, data)
  if (!length(ctxs)) cli::cli_abort("No model could be refitted from {.arg x}.")
  fam  <- ctxs[[1L]]$family
  weighted <- !is.null(ctxs[[1L]]$wt)
  keys <- reg_checks_for(fam, weighted, has_fit = TRUE, what = "panel")
  if (!identical(check, "auto") && !identical(check, "all")) {
    bad <- setdiff(check, names(REG_CHECKS))
    if (length(bad)) {
      cli::cli_abort(c("{.arg check} must name model checks.",
                       "x" = "Unknown: {.val {bad}}.",
                       "i" = "Available: {.val {names(REG_CHECKS)}}."))
    }
    keys <- intersect(check, keys)
    if (!length(keys)) {
      cli::cli_abort(c("None of those checks can be drawn for a {.val {fam}} model.",
                       "i" = "Available here: {.val {reg_checks_for(fam, weighted, what = 'panel')}}."))
    }
  }
  cols <- reg_plot_colors(theme)
  opts <- list(predictors = predictors, max_points = max_points, nbins = nbins, conf = conf,
               seed = seed, facet_ncol = facet_ncol)
  # i18n: the WHOLE label-building block under one language, as reg_model_lines() does.
  grobs <- with_legend_lang(lang, function(lg)
    purrr::compact(purrr::map(keys, function(k) reg_panel_build(k, ctxs, cols, opts))))
  if (!length(grobs)) {
    cli::cli_abort(c("Nothing could be drawn for this model.",
                     "i" = "A multinomial outcome has no residual panel (its residuals depend on the category order)."))
  }
  if (is.null(ncol)) ncol <- min(3L, length(grobs))
  gridExtra::grid.arrange(grobs = grobs, ncol = ncol)
}


# === SECTION: or_plot() -- odds-ratio forest plot ==================================================

# The OR columns of a tab_reg()/tab_logit() table (multiplicative interval, ci_type == "or").
or_plot_columns <- function(tabs) {
  nms <- names(tabs)[purrr::map_lgl(tabs, is_fmt)]
  nms[purrr::map_lgl(nms, ~ identical(get_ci_type(tabs[[.x]]), "or"))]
}

# The "2.34***" / "1/2.34***" text for the table panel (1/x for OR < 1, the console convention).
or_plot_text <- function(or, stars, ref) {
  txt <- ifelse(is.na(or), "",
                ifelse(or >= 1,
                       paste0(formatC(or, format = "f", digits = 2), stars),
                       paste0("1/", formatC(1 / or, format = "f", digits = 2), stars)))
  ifelse(ref, gettext("Ref."), txt)
}

#' Odds-ratio forest plot of a tabxplor regression table
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' A finalfit-style forest plot of the odds ratios in a [tab_logit()] / [tab_reg()] table: a log-scale
#' point-and-interval plot beside a text table of the estimates. It reads the stored `fmt` fields
#' (odds ratio, confidence interval, significance, count) directly -- no model is re-fitted.
#'
#' @param tabs A `tabxplor` table from [tab_logit()] / [multi_logit()] / [tab_reg()] (binomial /
#'   poisson / multinomial / ordinal -- any table with an odds-ratio-shaped column).
#' @param column Optional column name to plot when the table has several odds-ratio columns
#'   (default: the first one).
#' @param point_size Length-2 numeric, the `ggplot2` point-size range mapped to the cell counts.
#' @param title Optional plot title (default: the plotted column's name).
#' @param theme `"light"`, `"dark"` or `"print"` (greyscale). Defaults to `options("tabxplor.theme")`,
#'   like the table exporters and [reg_check_plots()].
#' @param lang Language of the axis label and the `Ref.` marker. Defaults to
#'   `options("tabxplor.lang")`.
#' @param ... Unused, for future extension.
#'
#' @return Invisibly, the assembled `gtable` (drawn on the current graphics device).
#'
#' @examples
#' data <- forcats::gss_cat |>
#'   dplyr::mutate(married = factor(dplyr::if_else(marital == "Married",
#'                                                 "Married", "Not married")))
#' if (requireNamespace("ggplot2", quietly = TRUE) &&
#'     requireNamespace("gridExtra", quietly = TRUE)) {
#'   or_plot(tab_logit(data, "married", c("race", "age")))
#' }
#' @export
or_plot <- function(tabs, column = NULL, point_size = c(1.5, 6), title = NULL,
                    theme = NULL, lang = NULL, ...) {
  reg_plot_deps()
  or_cols <- or_plot_columns(tabs)
  if (length(or_cols) == 0L) {
    cli::cli_abort(c("No odds-ratio column found in {.arg tabs}.",
                     "i" = "{.fn or_plot} expects a {.fn tab_logit} / {.fn tab_reg} odds-ratio table."))
  }
  # Default to a MODEL odds-ratio column, not its observed (crude) companion. Last Phase z13: read the
  # STORED `role` attribute (Phase 17c), not the column NAME. This matched "^Emp\\." -- a prefix Phase g
  # renamed to "Obs_" -- so every crude column had silently counted as a model one since, both for the
  # default pick and for the "Several odds-ratio columns" message. A role is exactly what 17c stored to
  # stop behaviour depending on a rendered label.
  roles      <- vapply(or_cols, function(nm) get_role(tabs[[nm]]), character(1))
  model_cols <- or_cols[roles != "emp"]
  default_col <- if (length(model_cols)) model_cols[[1]] else or_cols[[1]]
  col_nm <- if (!is.null(column)) column else default_col
  if (!col_nm %in% or_cols) {
    cli::cli_abort("{.arg column} {.val {col_nm}} is not an odds-ratio column of {.arg tabs}.")
  }
  if (length(model_cols) > 1L && is.null(column)) {
    cli::cli_inform(c("i" = paste0("Several odds-ratio columns; plotting {.val {col_nm}}. ",
                                   "Use {.arg column} to pick another.")))
  }

  d  <- dplyr::ungroup(tabs)
  oc <- d[[col_nm]]
  keep <- as.character(d$var) != "Constant" & !is.na(get_or(oc))
  d  <- d[keep, , drop = FALSE]; oc <- oc[keep]

  ref <- is_refrow(oc)
  cols <- reg_plot_colors(theme)
  df  <- with_legend_lang(lang, function(lg) tibble::tibble(
    variable = as.character(d$var),
    # z15: a graphics device has no block glyphs (see tx_spark_strip)
    level    = tx_spark_strip(as.character(d$levels)),
    or = get_or(oc), lo = get_ci_inf(oc), hi = get_ci_sup(oc),
    n  = get_n(oc), ref = ref,
    or_text = or_plot_text(get_or(oc), get_stars(oc), ref)))
  # the predictor name is shown once per block; rows are laid out top-to-bottom as in the table.
  df$var_label <- ifelse(df$variable == dplyr::lag(df$variable, default = ""), "", df$variable)
  df$order     <- rev(seq_len(nrow(df)))
  df$fit_id    <- factor(df$order)
  df$fill      <- ifelse(df$ref, "Reference", "Estimate")
  ttl          <- if (!is.null(title)) title else col_nm

  # log breaks around 1, symmetric (1/4 .. 4), trimmed to the data range.
  rng    <- range(c(df$or, df$lo, df$hi), na.rm = TRUE)
  ladder <- c(1/8, 1/4, 1/2, 1/1.5, 1, 1.5, 2, 4, 8)
  brks   <- ladder[ladder >= rng[1] / 1.2 & ladder <= rng[2] * 1.2]
  if (length(brks) < 2L) brks <- c(rng[1], 1, rng[2])
  lbls   <- ifelse(brks < 1, paste0("1/", formatC(1 / brks, format = "fg", digits = 2)),
                   formatC(brks, format = "fg", digits = 2))
  xlab   <- with_legend_lang(lang, function(lg)
    gettextf("Odds ratio (%s%% CI, log scale)", format(100 * get_conf_level(oc))))

  g_plot <- ggplot2::ggplot(df, ggplot2::aes(x = .data$or, y = .data$fit_id)) +
    ggplot2::geom_vline(xintercept = 1, linetype = "longdash", colour = cols$grey) +
    ggplot2::geom_linerange(ggplot2::aes(xmin = .data$lo, xmax = .data$hi),
                            na.rm = TRUE, colour = cols$grey) +
    ggplot2::geom_point(ggplot2::aes(size = .data$n, fill = .data$fill),
                        shape = 22, na.rm = TRUE) +
    ggplot2::scale_x_continuous(transform = "log10", breaks = brks, labels = lbls) +
    ggplot2::scale_size(range = point_size, guide = "none") +
    ggplot2::scale_fill_manual(values = c(Estimate = cols$point, Reference = cols$grey),
                               guide = "none") +
    ggplot2::labs(x = xlab, y = NULL, title = ttl) +
    ggplot2::theme_classic(base_size = 11) +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   axis.line.y = ggplot2::element_blank(),
                   text = ggplot2::element_text(colour = cols$text),
                   axis.text.x = ggplot2::element_text(colour = cols$grey),
                   plot.background  = ggplot2::element_rect(fill = cols$bg, colour = NA),
                   panel.background = ggplot2::element_rect(fill = cols$bg, colour = NA),
                   plot.title = ggplot2::element_text(face = "bold", size = 11),
                   plot.margin = ggplot2::margin(5, 5, 5, 0))

  g_tab <- ggplot2::ggplot(df, ggplot2::aes(y = .data$fit_id)) +
    ggplot2::geom_text(ggplot2::aes(x = 0, label = .data$var_label), hjust = 0,
                       fontface = "bold", size = 3.3, na.rm = TRUE, colour = cols$text) +
    ggplot2::geom_text(ggplot2::aes(x = 1, label = .data$level), hjust = 0, size = 3.3,
                       na.rm = TRUE, colour = cols$text) +
    ggplot2::geom_text(ggplot2::aes(x = 3, label = .data$or_text), hjust = 1, size = 3.3,
                       na.rm = TRUE, colour = cols$text) +
    ggplot2::scale_x_continuous(limits = c(0, 3.05)) +
    ggplot2::labs(x = NULL, y = NULL, title = "") +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = cols$bg, colour = NA),
                   plot.margin = ggplot2::margin(5, 0, 5, 5))

  gridExtra::grid.arrange(g_tab, g_plot, ncol = 2, widths = c(3, 2))
}
