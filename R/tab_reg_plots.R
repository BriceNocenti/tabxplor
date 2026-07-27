# PURPOSE: Regression plots for tabxplor -- lm_plots() (glm/lm diagnostics) + or_plot() (OR forest plot).
# ROLE: The display-phase companions to tab_reg() / tab_logit() (Phase 12h). lm_plots() works on a fitted
#   model object (diagnostics are about the fit); or_plot() works on a tabxplor_tab (reuses the stored fmt
#   fields -- no refit).
# KEY CONSTRAINTS:
#   - ggplot2 + gridExtra are Suggests -> every entry point guards with requireNamespace().
#   - or_plot() reads the fmt fields (get_or / get_ci_inf / get_ci_sup / get_stars / get_n / get_pct), so
#     it stays in sync with the console/exports for free; it never re-fits a model.
# See: CLAUDE.md 2.0.0 roadmap > Phase 12h.

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

# === SECTION: lm_plots() -- glm/lm diagnostic panel =================================================

# Fit a model for the data-frame convenience form (diagnostics only -- a direct lm()/glm(), not the
# tidy/CI machinery of reg_fit()).
lm_plots_fit <- function(data, dependent, predictors, family, wt) {
  form <- stats::reformulate(predictors, response = dependent)
  w    <- if (!is.null(wt)) data[[wt]] else NULL
  if (identical(family, "gaussian")) stats::lm(form, data = data, weights = w)
  else                               stats::glm(form, data = data, family = family, weights = w)
}

# The augmented diagnostics data frame (base-R accessors work for lm and glm; deviance residuals for glm).
lm_plots_augment <- function(fit) {
  tibble::tibble(
    .fitted   = as.numeric(stats::predict(fit)),          # fitted (lm) / linear predictor (glm)
    .resid    = as.numeric(stats::residuals(fit)),        # deviance residuals for glm
    .std      = as.numeric(stats::rstandard(fit)),
    .hat      = as.numeric(stats::hatvalues(fit)),
    .cook     = as.numeric(stats::cooks.distance(fit))
  )
}

lm_plot_theme <- function() {
  ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 11))
}

lm_plot_resid_fitted <- function(df) {
  ggplot2::ggplot(df, ggplot2::aes(x = .data$.fitted, y = .data$.resid)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    ggplot2::geom_point(alpha = 0.4, na.rm = TRUE, shape = 16) +
    ggplot2::geom_smooth(se = FALSE, method = "loess", formula = y ~ x,
                         colour = "#c00000", linewidth = 0.7, na.rm = TRUE) +
    ggplot2::labs(title = "Residuals vs Fitted", x = "Fitted values", y = "Residuals") +
    lm_plot_theme()
}

lm_plot_qq <- function(df) {
  ggplot2::ggplot(df, ggplot2::aes(sample = .data$.std)) +
    ggplot2::geom_qq(alpha = 0.4, na.rm = TRUE, shape = 16) +
    ggplot2::geom_qq_line(colour = "#c00000", linewidth = 0.7, na.rm = TRUE) +
    ggplot2::labs(title = "Normal Q-Q", x = "Theoretical quantiles",
                  y = "Standardized residuals") +
    lm_plot_theme()
}

lm_plot_scale_location <- function(df) {
  df$.sqrt_std <- sqrt(abs(df$.std))
  ggplot2::ggplot(df, ggplot2::aes(x = .data$.fitted, y = .data$.sqrt_std)) +
    ggplot2::geom_point(alpha = 0.4, na.rm = TRUE, shape = 16) +
    ggplot2::geom_smooth(se = FALSE, method = "loess", formula = y ~ x,
                         colour = "#c00000", linewidth = 0.7, na.rm = TRUE) +
    ggplot2::labs(title = "Scale-Location", x = "Fitted values",
                  y = expression(sqrt("|Standardized residuals|"))) +
    lm_plot_theme()
}

lm_plot_resid_leverage <- function(df, n_params) {
  # Cook's distance contours at D = 0.5 and 1: std_resid = +/- sqrt(D * p * (1 - h) / h).
  hseq  <- seq(max(min(df$.hat, na.rm = TRUE), 1e-4), max(df$.hat, na.rm = TRUE), length.out = 100)
  contour <- function(D) {
    tibble::tibble(.hat = c(hseq, rev(hseq)),
                   .std = c(sqrt(D * n_params * (1 - hseq) / hseq),
                            -rev(sqrt(D * n_params * (1 - hseq) / hseq))),
                   level = paste0("Cook ", D))
  }
  cont <- dplyr::bind_rows(contour(0.5), contour(1))
  ggplot2::ggplot(df, ggplot2::aes(x = .data$.hat, y = .data$.std)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    ggplot2::geom_path(data = cont, ggplot2::aes(group = .data$level),
                       linetype = "dotted", colour = "#c00000", na.rm = TRUE) +
    ggplot2::geom_point(alpha = 0.4, na.rm = TRUE, shape = 16) +
    ggplot2::geom_smooth(se = FALSE, method = "loess", formula = y ~ x,
                         colour = "#c00000", linewidth = 0.7, na.rm = TRUE) +
    ggplot2::coord_cartesian(ylim = range(df$.std, na.rm = TRUE)) +
    ggplot2::labs(title = "Residuals vs Leverage", x = "Leverage",
                  y = "Standardized residuals") +
    lm_plot_theme()
}

#' Diagnostic plots for a linear / generalized-linear model
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' A modern \pkg{ggplot2} version of the base `plot.lm()` 2x2 diagnostic panel: Residuals vs Fitted,
#' Normal Q-Q, Scale-Location, and Residuals vs Leverage (with Cook's-distance contours). Pass a fitted
#' model, or a data frame plus the variable names to fit one on the fly.
#'
#' @param object A fitted model (`lm` / `glm` / `svyglm`), OR a data frame (then supply `dependent`
#'   and `predictors`).
#' @param dependent,predictors When `object` is a data frame: the response and predictor column names.
#' @param family Model family for the data-frame form (default `"gaussian"`; e.g. `"binomial"`).
#' @param wt Optional weight column name for the data-frame form.
#' @param ... Unused, for future extension.
#'
#' @return Invisibly, the assembled `gtable` (drawn on the current graphics device).
#'
#' @examples
#' # \donttest: building the 2x2 ggplot grid costs 3-4 s of CPU (CRAN NOTEs any topic over 5 s).
#' \donttest{
#' m <- stats::lm(tvhours ~ age, data = forcats::gss_cat)
#' if (requireNamespace("ggplot2", quietly = TRUE) &&
#'     requireNamespace("gridExtra", quietly = TRUE)) {
#'   lm_plots(m)
#' }
#' }
#' @export
lm_plots <- function(object, dependent = NULL, predictors = NULL, family = "gaussian",
                     wt = NULL, ...) {
  reg_plot_deps()
  fit <- if (is.data.frame(object)) {
    if (is.null(dependent) || is.null(predictors)) {
      cli::cli_abort(c("With a data frame, supply {.arg dependent} and {.arg predictors}.",
                       "i" = "Or pass a fitted model as {.arg object}."))
    }
    lm_plots_fit(object, dependent, predictors, family, wt)
  } else {
    object
  }
  df <- lm_plots_augment(fit)
  n_params <- length(stats::coef(fit))
  grobs <- list(
    lm_plot_resid_fitted(df),
    lm_plot_qq(df),
    lm_plot_scale_location(df),
    lm_plot_resid_leverage(df, n_params)
  )
  gridExtra::grid.arrange(grobs = grobs, ncol = 2)
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
  ifelse(ref, "Ref.", txt)
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
or_plot <- function(tabs, column = NULL, point_size = c(1.5, 6), title = NULL, ...) {
  reg_plot_deps()
  or_cols <- or_plot_columns(tabs)
  if (length(or_cols) == 0L) {
    cli::cli_abort(c("No odds-ratio column found in {.arg tabs}.",
                     "i" = "{.fn or_plot} expects a {.fn tab_logit} / {.fn tab_reg} odds-ratio table."))
  }
  # default to a MODEL odds-ratio column, not the descriptive "Emp. OR" companion (empirical).
  model_cols <- or_cols[!grepl("^Emp\\.", or_cols)]
  default_col <- if (length(model_cols)) model_cols[[1]] else or_cols[[1]]
  col_nm <- if (!is.null(column)) column else default_col
  if (!col_nm %in% or_cols) {
    cli::cli_abort("{.arg column} {.val {col_nm}} is not an odds-ratio column of {.arg tabs}.")
  }
  if (length(model_cols) > 1L && is.null(column)) {
    cli::cli_inform(c("i" = "Several odds-ratio columns; plotting {.val {col_nm}}.
                             Use {.arg column} to pick another."))
  }

  d  <- dplyr::ungroup(tabs)
  oc <- d[[col_nm]]
  keep <- as.character(d$var) != "Constant" & !is.na(get_or(oc))
  d  <- d[keep, , drop = FALSE]; oc <- oc[keep]

  ref <- is_refrow(oc)
  df  <- tibble::tibble(
    variable = as.character(d$var),
    level    = as.character(d$levels),
    or = get_or(oc), lo = get_ci_inf(oc), hi = get_ci_sup(oc),
    n  = get_n(oc), ref = ref,
    or_text = or_plot_text(get_or(oc), get_stars(oc), ref)
  )
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

  g_plot <- ggplot2::ggplot(df, ggplot2::aes(x = .data$or, y = .data$fit_id)) +
    ggplot2::geom_vline(xintercept = 1, linetype = "longdash", colour = "grey40") +
    ggplot2::geom_linerange(ggplot2::aes(xmin = .data$lo, xmax = .data$hi),
                            na.rm = TRUE, colour = "grey30") +
    ggplot2::geom_point(ggplot2::aes(size = .data$n, fill = .data$fill),
                        shape = 22, na.rm = TRUE) +
    ggplot2::scale_x_continuous(transform = "log10", breaks = brks, labels = lbls) +
    ggplot2::scale_size(range = point_size, guide = "none") +
    ggplot2::scale_fill_manual(values = c(Estimate = "#33648c", Reference = "#b0b0b0"),
                               guide = "none") +
    ggplot2::labs(x = "Odds ratio (95% CI, log scale)", y = NULL, title = ttl) +
    ggplot2::theme_classic(base_size = 11) +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   axis.line.y = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(face = "bold", size = 11),
                   plot.margin = ggplot2::margin(5, 5, 5, 0))

  g_tab <- ggplot2::ggplot(df, ggplot2::aes(y = .data$fit_id)) +
    ggplot2::geom_text(ggplot2::aes(x = 0, label = .data$var_label), hjust = 0,
                       fontface = "bold", size = 3.3, na.rm = TRUE) +
    ggplot2::geom_text(ggplot2::aes(x = 1, label = .data$level), hjust = 0, size = 3.3, na.rm = TRUE) +
    ggplot2::geom_text(ggplot2::aes(x = 3, label = .data$or_text), hjust = 1, size = 3.3, na.rm = TRUE) +
    ggplot2::scale_x_continuous(limits = c(0, 3.05)) +
    ggplot2::labs(x = NULL, y = NULL, title = "") +
    ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(plot.margin = ggplot2::margin(5, 0, 5, 5))

  gridExtra::grid.arrange(g_tab, g_plot, ncol = 2, widths = c(3, 2))
}
