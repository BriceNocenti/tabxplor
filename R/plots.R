# PURPOSE: the package's data charts -- forest_plot() (the RESULTS: estimate + interval +
#   significance, for a tab() crosstab or a tab_reg() table) and reg_check_plots() (the model checks,
#   drawn). Plus the ONE model they and every future chart read: tab_estimates().
#   (tab_plot(), in R/tab_classes.R, is not a chart -- it renders the TABLE as an image.)
#
# THE MODEL (Phase 18z17). tab_estimates() is one long tibble: one row per (table row x plotted
#   column), carrying the estimate, its interval, its p, its scale (fmt_scale_of), its colour slot and
#   hex (the same accessors the printed table used), and its observed counterpart with the gap's
#   interval. It computes NOTHING -- so a chart over it agrees with the table by construction, and it
#   is testable without a graphics device (a tibble has a golden lock; a ggplot has none).
#
# TWO CHARTS, OPPOSITE CONTRACTS. forest_plot() reads the TABLE and never refits; reg_check_plots()
#   ALWAYS refits (diagnostics are about residuals, which no table carries). Both help pages say so.
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
#   - NOTHING here identifies a row or a column by its rendered label or its name prefix. The column
#     axis is `role` + `col_var`, the row axis is tab_render_vars() + tab_row_roles(), the scale is
#     fmt_scale_of(). (or_plot()'s `^Emp\\.` prefix match is the defect this rule exists to prevent.)
#   - NEVER geom_smooth(method = "auto") -- see the warning in R/reg-assumptions.R's primitives.
# See: dev/regression_effect_plots.md (forest_plot), dev/regression_assumptions_plots.md (the checks).

# Guard the Suggests packages a plot needs, with an install hint.
tx_plot_deps <- function(pkgs = c("ggplot2", "gridExtra")) {
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
tx_plot_colors <- function(theme = NULL) {
  th <- tx_theme_resolve(theme)          # ggplot bakes its colours: "auto" cannot be honoured
  ch <- tx_chrome_hex(th)
  list(theme = th, text = ch$text, grey = ch$grey, bg = ch$bg,
       # the accent: a hue under colour themes, pure black under `print` (a greyscale panel leans on
       # line TYPE, not on a hue that photocopies to the same grey as the data)
       accent = if (identical(th, "print")) "#000000" else "#c00000",
       point  = if (identical(th, "dark")) "#8fb8dd" else if (identical(th, "print")) "#000000"
                else "#33648c")
}

#' @keywords internal
tx_plot_theme <- function(cols) {
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


# === SECTION: tab_estimates() -- THE estimate model =================================================
#
# One long tibble, one row per (table row x plotted column). It COMPUTES NOTHING: every number comes
# from the accessor the printed table used, which is the whole no-drift claim -- a chart over this
# cannot disagree with the table it was made from.
#
# The three axes, each from a STORED fact and never from a rendered label:
#   * columns   `role` ("model" / "emp" / "n" / "") + `col_var` + `is_totcol`
#   * rows      tab_render_vars() (the DECLARED index columns, 19f) + tab_row_roles() (the row_kind field)
#   * scale     fmt_scale_of() (R/fmt_class.R), so the estimate, its neutral, its transform, its unit
#               and its ladder are one record and not four re-derivations
# and the colour comes from resolve_color_channel_plans() -> fmt_channel_codes(), the same two calls
# every exporter makes.

# The PLOTTED columns. A count column carries no estimate (it is the base, read only by `size = "n"`);
# a total column is dropped unless asked for; on a regression table the MODEL columns are the subject
# and the crude ones ride as `obs` (SS9), except where the reader asked to see their own interval.
#' @keywords internal
est_plot_columns <- function(x, columns = NULL, totals = FALSE, observed = "auto", what = "auto") {
  nm <- names(x)[vapply(x, is_fmt, logical(1))]
  if (!length(nm)) return(character(0))
  if (!is.null(columns)) {
    miss <- setdiff(columns, names(x))
    if (length(miss))
      cli::cli_abort(c("{.arg columns}: no such column{?s} in the table: {.val {miss}}.",
                       "i" = "Available: {.val {nm}}."))
    bad <- columns[!columns %in% nm]
    if (length(bad)) cli::cli_abort("{.arg columns}: {.val {bad}} {?is/are} not a value column.")
    return(columns)                                    # the reader's own order, honoured verbatim
  }
  role <- vapply(nm, function(n) as.character(get_role(x[[n]]))[1], character(1))
  typ  <- vapply(nm, function(n) fmt_var_kind(x[[n]]), character(1))
  keep <- role != "n" & typ != "count"
  if (!isTRUE(totals))
    keep <- keep & !vapply(nm, function(n) isTRUE(is_totcol(x[[n]])[1]), logical(1))
  nm <- nm[keep]; role <- role[keep]
  if (!any(role == "model")) return(nm)                # a crosstab: every value column is plotted
  # a regression table: the models are the subject. The crude block joins ONE column at a time and
  # always by a stored fact -- never "every Obs_* column", which would draw the same crude effect
  # twice (Obs_% and Obs_diff are field-identical) and pair an odds ratio with a percentage.
  out <- nm[role == "model"]
  # `observed = "ci"` is the only mode that needs the crude column ITSELF (it draws its own interval);
  # "band" / "point" read the `obs` field, which already rides on the model column.
  if (identical(observed, "ci")) {
    out <- unique(c(out, stats::na.omit(vapply(nm[role == "model"],
                                               function(n) est_crude_of(x, n), character(1)))))
  }
  if (identical(what, "level")) {
    # the observed-vs-adjusted percentages (SS11): the crude column that SHOWS a level. Obs_% and
    # Obs_diff are field-identical -- both carry the proportion AND the difference -- so the only
    # thing that tells them apart is `display`, which is a stored FIELD, not a rendered string (the
    # colour engine reads display_primary() for its own gates). Taking both would draw one series
    # twice and split the panel the pairing exists to make.
    lv <- nm[role == "emp"]
    lv <- lv[vapply(lv, function(n) {
      col <- x[[n]]
      d   <- display_primary(get_display(col))
      f   <- if (identical(fmt_var_kind(col), "mean")) get_mean(col) else get_pct(col)
      any(d %in% c("pct", "mean")) && any(is.finite(f))
    }, logical(1))]
    out <- unique(c(out, lv))
  }
  nm[nm %in% out]                                      # table order, not selection order
}

# The crude counterpart of a model column, by STORED facts: role "emp", the same estimand (`scale`),
# and -- when several qualify -- the same `col_var`. Never by the "Obs_" name prefix: that pairing is
# the defect or_plot() shipped with (`^Emp\\.`, silently dead after the Phase-g rename).
#' @keywords internal
est_crude_of <- function(x, col_nm) {
  nm  <- names(x)[vapply(x, is_fmt, logical(1))]
  emp <- nm[vapply(nm, function(n) identical(as.character(get_role(x[[n]]))[1], "emp"), logical(1))]
  if (!length(emp)) return(NA_character_)
  # Phase 19b: pair on the STORED SCALE -- "is this the crude twin of that model column" is exactly
  # "does it estimate the same thing", which is now one attribute instead of a coarser `ci_type`.
  scl <- get_scale(x[[col_nm]])
  emp <- emp[vapply(emp, function(n) identical(get_scale(x[[n]]), scl), logical(1))]
  if (!length(emp)) return(NA_character_)
  if (length(emp) > 1L) {
    cv  <- as.character(get_col_var(x[[col_nm]]))[1]
    hit <- emp[vapply(emp, function(n) identical(as.character(get_col_var(x[[n]]))[1], cv), logical(1))]
    if (length(hit)) emp <- hit
  }
  emp[1]
}

# The FACET key (ruling D7 -- derived once here, never stored: `col_var` is read by the exporters'
# header machinery and means "the span this column sits under").
#
# One panel per estimate column, except that columns which are the SAME estimand seen twice (a model
# and its crude twin: same col_var, different roles) share a panel. So: facet by `col_var`, unless a
# col_var holds several columns of the SAME role -- multinomial categories (two model columns under
# one "party3: OR"), or the levels of a crosstab's column variable, which is exactly the maintainer's
# layout ruling. A crude column whose col_var matches no model panel (comparison mode, where one
# observed block serves every model) is REPLICATED into each -- correct, since every model is compared
# against the same observed effect.
#' @keywords internal
est_facet_keys <- function(x, cols) {
  cv   <- vapply(cols, function(n) as.character(get_col_var(x[[n]]))[1], character(1))
  role <- vapply(cols, function(n) as.character(get_role(x[[n]]))[1],    character(1))
  key  <- cv
  for (v in unique(cv)) {
    i <- which(cv == v)
    if (any(table(role[i]) > 1L)) key[i] <- cols[i]    # several columns of one role: one panel each
  }
  stats::setNames(key, cols)
}

# The row axis, in ONE rule over the four label-block shapes tab_render_vars() distinguishes:
#   tab(d, race, party3)                 var = "race" (the variable), level = the race column
#   tab(d, c(race, relig), party3)       var = the `row_var` column,  level = the `levels` column
#   tab(d, race, party3, tab_vars = b)   var = "race",                level = race,  group = b
#   tab_reg(...)                         var = the `var` column,      level = the `levels` column
#' @keywords internal
est_row_axis <- function(x) {
  rv <- tab_render_vars(x)
  if (isTRUE(rv$degrade))
    cli::cli_abort(c("This table cannot be plotted: {rv$reason}.",
                     "i" = "It needs a factor row variable and at least one value column."))
  grp   <- dplyr::group_vars(x)
  lvl_c <- rv$row_var
  # Phase 19f: the DECLARED "var"-role column -- one rule for a merged crosstab and a regression,
  # where this had one clause per shape (and the regression one keyed on the grouping).
  var_c <- if (length(rv$var_col) == 1L && rv$var_col %in% names(x)) rv$var_col else NA_character_
  var   <- if (is.na(var_c)) rep(lvl_c, nrow(x)) else as.character(x[[var_c]])
  gcols <- setdiff(grp, c(var_c, lvl_c))
  grpv  <- if (length(gcols))
    do.call(paste, c(lapply(gcols, function(g) as.character(x[[g]])), list(sep = " / ")))
  else rep("", nrow(x))
  list(var = var, level = as.character(x[[lvl_c]]), group = grpv,
       roles = tab_row_roles(x), vars = rv)
}

#' The estimates of a table, one row per (table row x value column)
#'
#' The long model behind \code{\link{forest_plot}}: every plotted number, its interval, its p-value,
#' its scale and its colour, read from the table with the same accessors the printed table used.
#' Nothing is computed and no model is re-fitted, so it agrees with what the table shows by
#' construction. Reachable as \code{forest_plot(x, return_data = TRUE)}.
#'
#' @param x A table from \code{\link{tab}} or \code{\link{tab_reg}}.
#' @param columns Value columns to keep, by name. \code{NULL} (the default) keeps the model columns of
#'   a regression table and every value column of a cross-table.
#' @param what \code{"auto"} (the quantity the stored interval is centred on -- so a
#'   \code{ci = "cell"} table gives percentages, a \code{ci = "ref"} table differences, an
#'   odds-ratio table odds ratios), \code{"effect"} or \code{"level"}.
#' @param observed \code{"auto"}, \code{"band"}, \code{"point"}, \code{"ci"} or \code{"none"} --
#'   whether the observed (crude) counterpart of a regression estimate is included.
#' @param intercept Keep the regression \code{Constant} row.
#' @param totals Keep total rows and total columns.
#' @param theme Palette theme for the colour columns (\code{"light"} / \code{"dark"} / \code{"print"};
#'   \code{NULL} follows \code{getOption("tabxplor.export_theme")}).
#' @return A tibble with one row per plotted cell.
#' @keywords internal
tab_estimates <- function(x, columns = NULL, what = c("auto", "effect", "level"),
                          observed = c("auto", "band", "point", "ci", "none"),
                          intercept = FALSE, totals = FALSE, theme = NULL) {
  what     <- match.arg(what)
  observed <- match.arg(observed)
  if (!is.data.frame(x)) cli::cli_abort("{.arg x} must be a tabxplor table.")
  th    <- tx_plot_colors(theme)$theme
  tcols <- c(list(theme = th), tx_chrome_hex(th)[c("text", "grey", "grey2")])
  pp    <- fmt_point_palette(th, "text")
  cols  <- est_plot_columns(x, columns, totals = totals, observed = observed, what = what)
  if (!length(cols))
    cli::cli_abort(c("This table has no value column to plot.",
                     "i" = "{.fn forest_plot} needs a {.pkg tabxplor} table with {.cls fmt} columns."))
  ax    <- est_row_axis(x)
  facet <- est_facet_keys(x, cols)

  # which rows: the data rows, plus the totals when asked. A regression Constant is an intercept, not
  # an effect, and has no place on a forest axis (`intercept = TRUE` restores it, as ggstats does).
  keep <- ax$roles %in% if (isTRUE(totals)) c("data", "total") else "data"
  if (!isTRUE(intercept)) keep <- keep & ax$var != "Constant"   # the skeleton's own key, not a label
  if (!any(keep))
    cli::cli_abort("No row left to plot (every row is a total, or the intercept).")

  # the model facets, for the crude replication below
  role_of   <- vapply(cols, function(n) as.character(get_role(x[[n]]))[1], character(1))
  mod_facet <- unique(facet[role_of %in% c("model", "")])

  out <- list()
  for (nm in cols) {
    col  <- x[[nm]]
    role <- as.character(get_role(col))[1]
    # NOT inside the tibble() below: its arguments are evaluated sequentially, so `role` there would
    # already name the length-n COLUMN and the test would silently be FALSE for every row.
    ser  <- if (identical(role, "emp")) "observed" else "modelled"
    scl  <- fmt_scale_of(col, what)
    # the estimate and its interval. The interval belongs to the STORED scale, so a forced `what`
    # that moves off it keeps the point and drops the whisker rather than inventing one.
    stored <- fmt_scale_key(col)
    est    <- vctrs::field(col, scl$est_field)
    has_ci <- identical(scl$key, stored)
    lo     <- if (has_ci) get_ci_inf(col) else rep(NA_real_, length(col))
    hi     <- if (has_ci) get_ci_sup(col) else rep(NA_real_, length(col))
    pv     <- if (has_ci) get_pvalue(col) else rep(NA_real_, length(col))

    # colour: the two channel plans (for the measure / policy / ladder), then fmt_col_ann() -- the
    # EXPORTERS' own resolver, so the point's colour is the cell's colour down to the greys and the
    # never-greyed reference cell, and not a fourth re-derivation of the same case_when().
    pl  <- resolve_color_channel_plans(col)
    ann <- fmt_col_ann(col, tcols, want_colors = TRUE)
    # Phase 19c: "the channel carrying a GAP measure" is measure_own_ref() -- a measure whose baseline
    # is another column -- so the pair is read off MEASURES rather than written out here.
    gap_chan <- if (!is.null(pl$text) && measure_own_ref(pl$text$measure)) "text"
                else if (!is.null(pl$bg) && measure_own_ref(pl$bg$measure)) "bg"
                else NA_character_

    # the observed counterpart and the interval of the GAP (SS9.2: "the modelled point falls outside
    # the band" is exactly `fmt_gap_p(x) < 1 - conf_level`, because the band is obs (+/- | x/) z*gap_se
    # with the same z that fmt_gap_p() inverts).
    obs  <- get_obs(col)
    gse  <- get_gap_se(col)
    zc   <- conf_level_to_z(get_conf_level(col))
    half <- zc * gse
    band <- if (isTRUE(scl$mult)) list(lo = obs * exp(-half), hi = obs * exp(half))
            else                  list(lo = obs - half,       hi = obs + half)

    # what a LEVEL panel's reference line sits at. Crosstabs only: get_ref_field() broadcasts the
    # reference cell of each group from the group's END, which is where a crosstab's Total row is and
    # is meaningless on a regression skeleton (whose baseline is the FIRST row of each predictor
    # block) -- the same reason fmt_color_plan() refuses get_ref_var() for `type = "coef"`. A reg
    # panel needs no line anyway: its baseline level is drawn, marked, as a point of its own.
    ref_v <- if (!identical(scl$kind, "level")) rep(scl$neutral, length(col))
             else if (!identical(role, "")) rep(NA_real_, length(col))
             else if (identical(fmt_var_kind(col), "mean")) get_ref_means(col)
             else get_ref_pct(col)

    d <- tibble::tibble(
      row = seq_len(nrow(x)), var = ax$var, level = ax$level, group = ax$group,
      column = nm, role = role, col_var = as.character(get_col_var(col))[1],
      facet = unname(facet[[nm]]), series = ser,
      is_ref = is_refrow(col) %in% TRUE, is_total = ax$roles == "total",
      estimate = est, ci_inf = lo, ci_sup = hi, pvalue = pv,
      stars = get_stars(col), n = get_n(col), ref_value = ref_v,
      # the cell as the TABLE renders it -- format() is the package's only string producer (the
      # export-parity contract), so `labels = "estimate"` re-prints, never re-formats
      text = trimws(format(col)),
      kind = scl$kind, scale_key = scl$key, neutral = scl$neutral, trans = scl$trans,
      is_pct = scl$is_pct, unit = scl$unit, sd_y = scl$sd_y,
      obs = obs, gap_se = gse, gap = fmt_gap_raw(col), gap_lo = band$lo, gap_hi = band$hi,
      gap_p = fmt_gap_p(col), gap_tested = !all(is.na(gse)),
      measure = if (is.null(pl$text)) NA_character_ else pl$text$measure,
      policy  = if (is.null(pl$text)) NA_character_ else pl$text$policy,
      slot_text = ann$text_slot, slot_bg = ann$bg_slot,
      hex_text = ann$font, hex_bg = dplyr::na_if(ann$back, "none"),
      # what a MARK is painted with: the cell's own colour, except under `theme = "print"` where the
      # text palette is all black and a point borrows the grey ramp (fmt_point_palette). Identical to
      # `hex_text` under every colour theme.
      point_hex = ifelse(ann$text_slot > 0L, pp[pmax(ann$text_slot, 1L)], ann$font),
      bold = ann$face_bold, italic = ann$face_italic, underline = ann$face_underline,
      gap_slot = if (identical(gap_chan, "text")) ann$text_slot
                 else if (identical(gap_chan, "bg")) ann$bg_slot else rep(NA_integer_, length(col))
    )
    d$breaks    <- rep(list(scl$breaks),    nrow(d))
    d$break_dir <- rep(list(scl$break_dir), nrow(d))
    d <- d[keep, , drop = FALSE]

    # D7: one observed block serving several models is repeated in every model panel.
    if (identical(role, "emp") && !d$facet[1] %in% mod_facet && length(mod_facet)) {
      d <- vctrs::vec_rbind(!!!lapply(mod_facet, function(f) { d$facet <- f; d }))
    }
    out[[nm]] <- d
  }
  res <- vctrs::vec_rbind(!!!out)
  # `what = "level"` needs a stored level. A regression COEFFICIENT column has none -- only the
  # marginal path writes the adjusted probability into `pct` -- so say which argument produces it
  # rather than drawing an empty panel (SS11's honest scope).
  if (identical(what, "level") &&
      !any(is.finite(res$estimate[res$role %in% c("model", "")])))
    cli::cli_abort(c("{.code what = \"level\"} needs a percentage or a mean, and this table has none.",
                     "i" = paste("A regression stores an adjusted percentage only on the marginal",
                                 "path: build it with {.code effect = \"ame\"}.")))
  res$var    <- factor(res$var,   levels = unique(res$var))
  res$level  <- factor(res$level, levels = unique(res$level))
  res$facet  <- factor(res$facet, levels = unique(res$facet))
  res
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
                     family = reg_plot_family_of(x), outcome = reg_plot_dep_of(x),
                     predictors = reg_plot_preds_of(x), trials = NULL, wt = NULL, design = NULL,
                     label = gettext("Model"))))
  }
  # Phase 19m-i: TWO questions, so two messages. "Is this a regression table" is the STORED kind
  # (tab_is_reg); "does it still carry the recipe to refit from" is `spec$call$fit_spec`. They
  # diverge on a meta-stripped reg table (test-degraded-attrs.R builds exactly that state), where
  # the single conflated abort told the user their tab_reg() table was not one.
  if (!tab_is_reg(x))
    cli::cli_abort(c("{.arg x} is not a {.fn tab_reg} table.",
                     "i" = "Pass a {.fn tab_reg} result and its data, or a fitted model."))
  meta <- reg_call(x)
  fs   <- meta$fit_spec
  if (is.null(fs)) {
    cli::cli_abort(c("This {.fn tab_reg} table no longer carries its model record.",
                     "i" = "Rebuild it with {.fn tab_reg}, or pass the fitted model directly."))
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
      data, sp$outcome, sp$predictors, sp$fit_family, ds, isTRUE(sp$est$exp),
      reg_outcome_level_of(sp$outcome_level) %||% fs$outcome_level,
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
    list(fit = f$fit, data = f$data, family = sp$fit_family, outcome = sp$outcome,
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

# The (family, outcome, predictors) of a BARE fit -- the secondary form's only inference.
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
    ly <- rd_link_y(cx$data[[cx$outcome]], cx$family, cx$trials, cx$positive_level)
    w  <- if (!is.null(cx$wt) && cx$wt %in% names(cx$data)) cx$data[[cx$wt]] else NULL
    purrr::list_rbind(purrr::map(num, function(v) {
      # Phase 18z16-iv (W-G.4): the band takes the DESIGN variance when the user handed a
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
    tx_plot_theme(cols)
  facets <- if (length(unique(rows$model)) > 1L) ~ model + predictor else ~ predictor
  g + ggplot2::facet_wrap(facets, scales = "free_x", ncol = opts$facet_ncol)
}

# 2. RESIDUALS -- binned residuals against the fitted value. The classic lesson about why a RAW
# residual is useless for a binary outcome (it takes exactly two values given p-hat), and the reason
# every non-gaussian family here uses a randomised quantile residual instead.
reg_panel_residuals <- function(ctxs, cols, opts) {
  rows <- purrr::list_rbind(purrr::map(ctxs, function(cx) {
    r <- rd_resid(cx$fit, cx$family, cx$data[[cx$outcome]], cx$trials, opts$seed)
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
    tx_plot_theme(cols)
  reg_panel_facet(g, rows, opts$facet_ncol)
}

# 3. NORMALITY -- the Q-Q plot of the dispatched residual, against the ANALYTIC pointwise band.
reg_panel_normality <- function(ctxs, cols, opts) {
  rows <- purrr::list_rbind(purrr::map(ctxs, function(cx) {
    r <- rd_resid(cx$fit, cx$family, cx$data[[cx$outcome]], cx$trials, opts$seed)
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
    tx_plot_theme(cols)
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
    # Phase 19m-iii: the join key comes from `se` ITSELF. 19m-i left this as a length coincidence
    # between `se` and a SECOND, independent read (`names(coef(fit))`) -- but there was never a need
    # for a second read: reg_check_model_se() is `sqrt(diag(vcov(fit)))`, and vcov()'s dimnames are
    # carried straight through `diag()`, so `names(se)` names exactly the numbers in `se`, with the
    # same provenance and by construction the same length. (Reading summary(fit)$coefficients instead
    # would be WRONG twice over: it drops aliased rows, so it would no longer index the influence
    # closure built above, and on a quasipoisson its SEs are not vcov()'s -- see the WHY at the
    # head of reg_check_model_se().) Strictly better on multinom, where coef() is a MATRIX so
    # names() was NULL and this fell back to "1","2",... while vcov() is properly named.
    # The fallback stays for a fit whose variance matrix carries no dimnames (svy_vglm's $var), but
    # it is keyed on the NAMES being absent, not on two lengths happening to differ.
    nm <- names(se)
    tibble::tibble(term = if (length(nm)) nm else as.character(seq_along(se)),
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
    tx_plot_theme(cols)
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
    tx_plot_theme(cols)
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
    tx_plot_theme(cols)
  reg_panel_facet(g, rows, opts$facet_ncol)
}

# 7. PROPORTIONALITY (ordinal) -- the empirical cumulative logit of each cut, per predictor level. The
# proportional-odds assumption says these lines are PARALLEL; the Brant p in the footer tests it.
reg_panel_proportionality <- function(ctxs, cols, opts) {
  rows <- purrr::list_rbind(purrr::map(ctxs, function(cx) {
    if (cx$family != "ordinal") return(NULL)
    y  <- as.factor(cx$data[[cx$outcome]])
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
    tx_plot_theme(cols) +
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
#' @seealso [tab_reg()] and its `stats` argument (the same checks as footer rows), and
#'   [forest_plot()] for the RESULTS -- its opposite contract: it reads the finished table and never
#'   re-fits, where a model check always must.
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
  tx_plot_deps()
  ctxs <- reg_plot_fits(x, data)
  if (!length(ctxs)) cli::cli_abort("No model could be refitted from {.arg x}.")
  fam  <- ctxs[[1L]]$family
  weighted <- !is.null(ctxs[[1L]]$wt)
  keys <- reg_checks_for(fam, weighted, has_fit = TRUE, what = "panel")
  if (!identical(check, "auto") && !identical(check, "all")) {
    # Phase 19g (KEY 6): ONE vocabulary and ONE validator, shared with tab_reg(stats =) -- narrowed
    # here to the model CHECKS, which are the only things a panel can be drawn for.
    reg_validate_stat_keys(check, arg = "check", allowed = names(REG_CHECKS))
    keys <- intersect(check, keys)
    if (!length(keys)) {
      cli::cli_abort(c("None of those checks can be drawn for a {.val {fam}} model.",
                       "i" = "Available here: {.val {reg_checks_for(fam, weighted, what = 'panel')}}."))
    }
  }
  cols <- tx_plot_colors(theme)
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



# === SECTION: forest_plot() =========================================================================
#
# A renderer with no statistics in it. Everything it draws comes out of tab_estimates(); everything it
# names comes out of the legend's own producers. The three things worth stating here:
#
#   * THE LADDER IS THE COLOUR LADDER. The gridlines are the column's own break scale (fmt_scale_of),
#     labelled with legend_break_label() -- the glyph in the footer and the glyph on the axis are one
#     function. or_plot()'s private c(1/8, 1/4, ...) is what this replaces: it never moved when a user
#     called set_color_breaks().
#   * THE GAP BAND IS THE GAP TEST. Drawn around the OBSERVED point at obs (+/- | x/) z*gap_se, so
#     "the modelled point falls outside the bracket" is exactly `fmt_gap_p(x) < 1 - conf_level`, to
#     machine precision. Two correlated estimates' intervals must NOT be compared by overlap
#     (Schenker & Gentleman 2001) -- which is why the crude interval is not drawn by default.
#   * THE POLICIES BECOME GEOMETRY. `ignore` = where the point sits; `grey_non_signif` = whether the
#     whisker crosses the null line; `guaranteed_effect` = how far the near end of the whisker is from
#     it. One figure explains all three, which is the strongest reason to have it.

# The axis ladder for one scale: its own breaks, trimmed to the data and capped, plus the neutral.
# Cheap devices only, in order (SS7.3): trim -> cap, keeping the neutral then the roundest -> ggplot2's
# own check.overlap at the end, which is the only one that can see the panel width.
#' @keywords internal
fp_axis_breaks <- function(scl, rng, max_n = 7L) {
  b <- scl$breaks
  if (!length(b) || !all(is.finite(rng))) return(NULL)
  pad  <- if (isTRUE(scl$mult)) c(rng[1] / 1.2, rng[2] * 1.2) else rng + c(-1, 1) * 0.2 * diff(rng)
  keep <- is.finite(b) & b >= pad[1] & b <= pad[2]
  i    <- which(keep)
  if (length(i) < 2L) return(NULL)
  if (length(i) > max_n) {                       # keep the neutral, then the roundest magnitudes
    ntr  <- which(scl$break_dir[i] == 0L)
    ord  <- order(nchar(sub("0+$", "", format(scl$break_mag[i], trim = TRUE))))
    i    <- i[sort(unique(c(ntr, utils::head(ord, max_n - length(ntr)))))]
  }
  list(at = scl$breaks[i], dir = scl$break_dir[i], mag = scl$break_mag[i])
}

#' @keywords internal
fp_axis_labels <- function(scl, br, lang) {
  vapply(seq_along(br$at), function(k)
    if (br$dir[k] == 0L) legend_num(br$at[k] * if (isTRUE(scl$is_pct)) 100 else 1, lang)
    else legend_break_label(scl$label_meas, br$mag[k], br$dir[k], scl$is_pct, lang),
    character(1))
}

# The axis title. The unit is a KEY on the scale record; the words are gettext()'d HERE, at render, so
# `lang =` reaches them (a top-level gettext() would freeze the build locale -- z15's warning).
#' @keywords internal
fp_unit_word <- function(unit, eff_word = NA_character_, conf = NA_real_, outcome = NA_character_) {
  base <- switch(unit,
                 or         = if (!is.na(eff_word)) eff_word else gettext("Odds ratio"),
                 ratio      = gettext("Ratio"),
                 rate_ratio = gettext("Rate ratio"),
                 points     = gettext("Percentage points"),
                 pct        = gettext("Percentage"),
                 log        = gettext("Coefficient (log scale)"),
                 # a difference in the OUTCOME's own units: name the outcome, which is the only thing
                 # that makes the number readable ("hours", not "units")
                 units      = if (!is.na(outcome) && nzchar(outcome)) outcome
                              else gettext("Units of the outcome"),
                 "")
  if (!nzchar(base) || !is.finite(conf)) return(base)
  gettextf("%s (%s%% CI)", base, format(100 * conf))
}

#' Forest plot of a tabxplor table
#'
#' Draws every estimate of a table with its confidence interval, its significance and its colour --
#' for a cross-table from \code{\link{tab}} as much as for a regression table from
#' \code{\link{tab_reg}}. It **reads the table and never re-fits anything**: every number and every
#' colour comes from the cell it was printed from, so the figure and the table cannot disagree.
#' (Its sibling \code{\link{reg_check_plots}} is the opposite: model checks *always* re-fit, because
#' they are about residuals, which no table carries.)
#'
#' @details
#' **What is drawn.** By default, whatever the table's own \code{ci =} produced: cell percentages with
#' their intervals for \code{ci = "cell"}, differences from the reference with a zero line for
#' \code{ci = "ref"}, odds ratios on a log axis for \code{display = "{or}"}, and the model's effects for a
#' \code{\link{tab_reg}} table. \code{what = "level"} / \code{"effect"} override it.
#'
#' **The gridlines are the table's colour ladder** (\code{\link{set_color_breaks}}), labelled with the
#' same glyphs the footer uses, and the colour of each point is the colour of its cell.
#'
#' **The observed comparison.** With \code{empirical = TRUE}, a regression estimate carries its crude
#' counterpart. \code{observed = "band"} (the default when the gap was testable) draws a bracket around
#' the observed value at plus-or-minus the margin of error *of the difference*: the modelled point
#' falls outside it exactly when the gap test rejects. Two intervals must not be compared by overlap
#' when the estimators are correlated, which is why the crude interval is not drawn by default;
#' \code{observed = "ci"} restores that classic figure if you want it.
#'
#' @param x A table from \code{\link{tab}} or \code{\link{tab_reg}} (or a list
#'   of them, giving a list of plots).
#' @param columns Value columns to draw, by name. \code{NULL} (the default) draws the model columns of
#'   a regression table and every value column of a cross-table.
#' @param what \code{"auto"} (the quantity the table's own interval is centred on), \code{"effect"}
#'   (the contrast: difference, ratio or odds ratio) or \code{"level"} (the percentage or mean --
#'   for a regression table this needs \code{effect = "marginal"}).
#' @param observed For a regression table with \code{empirical = TRUE}: \code{"auto"}, \code{"band"}
#'   (the observed value with the margin of error of the gap), \code{"point"}, \code{"ci"} (the classic
#'   two-interval figure) or \code{"none"}.
#' @param facet \code{NULL} for one panel per estimate column, \code{FALSE} for a single panel.
#' @param color Set to \code{FALSE} for a plain plot with no colour measure.
#' @param guide \code{"gridlines"} (the default), \code{"bands"} (shade the panel between the colour
#'   breaks -- the teaching mode) or \code{"none"}.
#' @param labels Print the formatted estimate beside each point: \code{"none"}, \code{"estimate"}.
#' @param stars Show significance stars. \code{NULL} follows the table.
#' @param intercept Draw the regression \code{Constant} row.
#' @param totals Draw total rows and total columns.
#' @param size \code{NULL} for constant point size, or \code{"n"} to map the sample size.
#' @param theme \code{"light"}, \code{"dark"} or \code{"print"} (the black-and-white publication
#'   palette). \code{NULL} follows \code{getOption("tabxplor.export_theme")}.
#' @param lang Legend language: \code{NULL} (the locale), \code{"en"} or \code{"fr"}.
#' @param caption A caption. \code{NULL} keeps the table's own.
#' @param legend Print the colour legend (as a guide, or in the caption when several ladders apply).
#' @param subtext Include the table's subtext and footer lines in the caption.
#' @param return_data Return the long estimate tibble instead of the plot.
#' @param ... Unused.
#'
#' @return A \code{ggplot} (or a list of them), ready for \code{+ theme()} and \code{ggsave()}.
#' @seealso \code{\link{reg_check_plots}} for the model checks, \code{\link{tab_plot}} to render the
#'   table itself as an image.
#' @export
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   t <- tab(forcats::gss_cat, race, marital, pct = "row", ci = "ref", color = TRUE)
#'   forest_plot(t)
#' }
forest_plot <- function(x, columns = NULL, what = c("auto", "effect", "level"),
                        observed = c("auto", "band", "point", "ci", "none"),
                        facet = NULL, color = TRUE, guide = c("gridlines", "bands", "none"),
                        labels = c("none", "estimate"), stars = NULL,
                        intercept = FALSE, totals = FALSE, size = NULL,
                        theme = NULL, lang = NULL, caption = NULL, legend = TRUE, subtext = TRUE,
                        return_data = FALSE, ...) {
  what <- match.arg(what); observed <- match.arg(observed)
  guide <- match.arg(guide); labels <- match.arg(labels)
  if (!is.data.frame(x) && is.list(x))
    return(purrr::map(x, forest_plot, columns = columns, what = what, observed = observed,
                      facet = facet, color = color, guide = guide, labels = labels, stars = stars,
                      intercept = intercept, totals = totals, size = size, theme = theme,
                      lang = lang, caption = caption, legend = legend, subtext = subtext,
                      return_data = return_data))
  tx_plot_deps("ggplot2")
  cols  <- tx_plot_colors(theme)
  th    <- cols$theme

  e <- tab_estimates(x, columns = columns, what = what, observed = observed,
                     intercept = intercept, totals = totals, theme = th)
  if (identical(observed, "auto"))
    observed <- if (any(is.finite(e$gap_se))) "band"
                else if (any(is.finite(e$obs))) "point" else "none"
  if (return_data) return(e)

  # --- the y axis: table order, read top to bottom ---------------------------------------------------
  rows <- unique(e[c("row", "level")])
  e$ypos <- factor(e$row, levels = rev(sort(unique(e$row))))
  ylab   <- stats::setNames(as.character(rows$level), as.character(rows$row))

  # --- point size: constant, or the base. `n` is NA on a model column (Phase 14r moved the whole-model
  # N to the footer), so `size = "n"` reads the `add_n = TRUE` column when there is one -- the honest
  # replacement for or_plot()'s `point_size`, which mapped that all-NA field and silently did nothing.
  e$psize <- 1.9
  if (identical(size, "n")) {
    nv <- e$n
    if (all(is.na(nv))) {
      ncol_nm <- names(x)[vapply(x, function(c) is_fmt(c) &&
                                  identical(as.character(get_role(c))[1], "n"), logical(1))]
      if (length(ncol_nm)) nv <- get_n(x[[ncol_nm[1]]])[e$row]
    }
    if (any(is.finite(nv))) {
      r <- range(sqrt(nv), na.rm = TRUE, finite = TRUE)
      e$psize <- if (diff(r) > 0) 1.2 + 3.3 * (sqrt(nv) - r[1]) / diff(r) else 1.9
      e$psize[!is.finite(e$psize)] <- 1.2
    } else {
      cli::cli_inform(c("i" = paste("{.code size = \"n\"}: this table carries no per-level base.",
                                    "Build it with {.code add_n = TRUE}.")))
    }
  }

  # --- the x scale ----------------------------------------------------------------------------------
  scls  <- unique(e$scale_key)
  one   <- length(scls) == 1L
  ref   <- e[!duplicated(e$facet), c("facet", "neutral", "scale_key", "unit"), drop = FALSE]
  # the reference LINE of a level panel: only where it is one value for the whole panel (it is under
  # `pct = "row"` with `ref = "tot"`; under `pct = "col"` the reference is a COLUMN, i.e. a panel of
  # its own, and there is nothing to draw)
  rl <- do.call(rbind, lapply(split(e, e$facet), function(d) {
    v <- unique(d$ref_value[is.finite(d$ref_value)])
    if (length(v) == 1L) data.frame(facet = d$facet[1], at = v) else NULL
  }))
  nl <- ref[is.finite(ref$neutral), c("facet", "neutral"), drop = FALSE]

  # the axis follows the scale MOST of the plotted columns are on, not the first one's -- a reference
  # column carries no interval and would otherwise decide the axis for its whole panel row.
  main <- names(sort(table(e$scale_key), decreasing = TRUE))[1]
  mcol <- as.character(e$column[e$scale_key == main][1])
  scl  <- fmt_scale_of(x[[mcol]], what)
  conf <- get_conf_level(x[[mcol]])[1]
  rng  <- range(c(e$estimate, e$ci_inf, e$ci_sup, e$gap_lo, e$gap_hi, e$obs), na.rm = TRUE, finite = TRUE)
  br   <- fp_axis_breaks(scl, rng)

  p <- ggplot2::ggplot(e)

  # --- layers, back to front ------------------------------------------------------------------------
  if (identical(guide, "bands") && !is.null(br) && isTRUE(color)) {
    pal  <- fmt_point_palette(th, "bg")
    at   <- sort(unique(c(br$at, rng)))
    band <- data.frame(xmin = utils::head(at, -1), xmax = utils::tail(at, -1))
    mid  <- if (isTRUE(scl$mult)) sqrt(band$xmin * band$xmax) else (band$xmin + band$xmax) / 2
    lvl  <- vapply(seq_len(nrow(band)), function(k) {
      m <- if (isTRUE(scl$mult)) ifelse(mid[k] >= 1, mid[k], 1 / mid[k]) else abs(mid[k])
      s <- findInterval(m, sort(unique(scl$break_mag[scl$break_dir != 0L])))
      if (s == 0L) NA_integer_ else if (mid[k] >= scl$neutral) s else s + 4L
    }, integer(1))
    band$fill <- ifelse(is.na(lvl), NA_character_, pal[pmax(lvl, 1L)])
    band <- band[!is.na(band$fill), , drop = FALSE]
    if (nrow(band))
      p <- p + ggplot2::geom_rect(
        data = band, ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax, fill = .data$fill),
        ymin = -Inf, ymax = Inf, inherit.aes = FALSE, alpha = 0.55)
  }
  # the background colour channel: a band behind the row, the literal translation of a painted cell
  if (isTRUE(color) && any(!is.na(e$hex_bg))) {
    bg <- e[!is.na(e$hex_bg), , drop = FALSE]
    p <- p + ggplot2::geom_rect(
      data = bg, ggplot2::aes(ymin = as.numeric(.data$ypos) - 0.45,
                              ymax = as.numeric(.data$ypos) + 0.45, fill = .data$hex_bg),
      xmin = -Inf, xmax = Inf, inherit.aes = FALSE)
  }
  if (nrow(nl))
    p <- p + ggplot2::geom_vline(data = nl, ggplot2::aes(xintercept = .data$neutral),
                                 linetype = "longdash", colour = cols$grey, linewidth = 0.4)
  if (!is.null(rl) && nrow(rl))
    p <- p + ggplot2::geom_vline(data = rl, ggplot2::aes(xintercept = .data$at),
                                 linetype = "dotted", colour = cols$grey, linewidth = 0.4)

  mods <- e[e$series == "modelled", , drop = FALSE]
  crd  <- e[e$series == "observed", , drop = FALSE]
  dodge <- if (nrow(crd) && identical(observed, "ci")) ggplot2::position_dodge(width = 0.5)
           else ggplot2::position_identity()

  if (observed %in% c("band", "point")) {
    ob <- mods[is.finite(mods$obs), , drop = FALSE]
    if (nrow(ob)) {
      if (identical(observed, "band")) {
        bd <- ob[is.finite(ob$gap_lo), , drop = FALSE]
        if (nrow(bd))
          p <- p + ggplot2::geom_linerange(
            data = bd, ggplot2::aes(y = .data$ypos, xmin = .data$gap_lo, xmax = .data$gap_hi),
            linewidth = 2.4, alpha = 0.30, colour = cols$grey)
      }
      # solid where the gap was tested, dotted where it could not be (a conditional odds ratio, a
      # digest-path fit): the same predicate the colour engine reads, never a fourth encoding
      p <- p +
        ggplot2::geom_segment(
          data = ob, ggplot2::aes(x = .data$obs, xend = .data$estimate,
                                  y = .data$ypos, yend = .data$ypos,
                                  linetype = .data$gap_tested),
          colour = cols$grey, linewidth = 0.4) +
        ggplot2::geom_point(data = ob, ggplot2::aes(x = .data$obs, y = .data$ypos),
                            shape = 21, fill = cols$bg, colour = cols$grey, size = 1.8)
    }
  }
  if (nrow(crd) && identical(observed, "ci")) {
    p <- p +
      ggplot2::geom_linerange(
        data = crd, ggplot2::aes(y = .data$ypos, xmin = .data$ci_inf, xmax = .data$ci_sup,
                                 group = .data$series),
        colour = cols$grey, position = dodge) +
      ggplot2::geom_point(
        data = crd, ggplot2::aes(x = .data$estimate, y = .data$ypos, group = .data$series),
        shape = 21, fill = cols$bg, colour = cols$grey, size = 1.8, position = dodge)
  }

  # the model estimates: whisker, then the point in the cell's own colour
  p <- p + ggplot2::geom_linerange(
    data = mods, ggplot2::aes(y = .data$ypos, xmin = .data$ci_inf, xmax = .data$ci_sup,
                              group = .data$series),
    colour = cols$grey, position = dodge)
  p <- p + if (isTRUE(color))
    ggplot2::geom_point(data = mods,
                        ggplot2::aes(x = .data$estimate, y = .data$ypos, colour = .data$point_hex,
                                     shape = .data$is_ref, size = .data$psize, group = .data$series),
                        position = dodge)
  else
    ggplot2::geom_point(data = mods,
                        ggplot2::aes(x = .data$estimate, y = .data$ypos, shape = .data$is_ref,
                                     size = .data$psize, group = .data$series),
                        colour = cols$point, position = dodge)

  want_stars <- (is.null(stars) || isTRUE(stars)) && any(nzchar(mods$stars))
  if (want_stars)
    p <- p + ggplot2::geom_text(
      data = mods[nzchar(mods$stars), , drop = FALSE],
      ggplot2::aes(x = .data$ci_sup, y = .data$ypos, label = .data$stars),
      hjust = -0.2, vjust = 0.75, size = 2.6, colour = cols$text)
  if (identical(labels, "estimate"))
    p <- p + ggplot2::geom_text(
      data = mods, ggplot2::aes(x = Inf, y = .data$ypos, label = .data$text),
      hjust = 1.05, size = 2.6, colour = cols$grey)

  # --- scales ---------------------------------------------------------------------------------------
  sec <- ggplot2::waiver()
  if (identical(scl$sec, "exp"))
    sec <- ggplot2::sec_axis(transform = exp, name = gettext("Odds ratio"))
  else if (identical(scl$sec, "sd") && is.finite(scl$sd_y)) {
    sd_y <- scl$sd_y
    sec  <- ggplot2::sec_axis(transform = ~ . / sd_y, name = gettext("SD of the outcome"))
  }
  lab_fun <- if (isTRUE(scl$is_pct)) function(v) paste0(round(100 * v), "%") else ggplot2::waiver()
  xsc <- if (!is.null(br))
    ggplot2::scale_x_continuous(transform = scl$trans, breaks = br$at,
                                labels = fp_axis_labels(scl, br, lang), sec.axis = sec,
                                guide = ggplot2::guide_axis(check.overlap = TRUE))
  else
    ggplot2::scale_x_continuous(transform = scl$trans, labels = lab_fun, sec.axis = sec,
                                guide = ggplot2::guide_axis(check.overlap = TRUE))
  p <- p + xsc +
    ggplot2::scale_y_discrete(labels = ylab) +
    ggplot2::scale_linetype_manual(values = c("TRUE" = "solid", "FALSE" = "dotted"), guide = "none") +
    ggplot2::scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 5), guide = "none") +
    ggplot2::scale_size_identity(guide = "none")

  # --- the colour guides ----------------------------------------------------------------------------
  gtxt <- if (isTRUE(color) && isTRUE(legend))
    legend_guide_spec(x, unique(as.character(e$column)), "text", th, lang) else NULL
  gbg  <- if (isTRUE(color) && isTRUE(legend) && any(!is.na(e$hex_bg)))
    legend_guide_spec(x, unique(as.character(e$column)), "bg", th, lang) else NULL
  if (isTRUE(color)) {
    p <- p + if (!is.null(gtxt))
      ggplot2::scale_colour_identity(
        name = gtxt$title, guide = "legend",
        breaks = c(gtxt$keys$hex, gtxt$grey_hex), labels = c(gtxt$keys$label, gtxt$grey_label))
    else ggplot2::scale_colour_identity()
    p <- p + if (!is.null(gbg))
      ggplot2::scale_fill_identity(
        name = gbg$title, guide = "legend",
        breaks = gbg$keys$hex, labels = gbg$keys$label)
    else ggplot2::scale_fill_identity()
  }

  # --- facets ---------------------------------------------------------------------------------------
  nvar <- nlevels(e$var); nfac <- nlevels(e$facet)
  ngrp <- length(unique(e$group[nzchar(e$group)]))            # sub-tables (tab_vars / a tab_vars)
  if (!identical(facet, FALSE) && (nvar > 1L || nfac > 1L || ngrp > 1L)) {
    if (max(nvar, 1L) * max(nfac, 1L) * max(ngrp, 1L) > 12L)
      cli::cli_inform(c("!" = "{max(nvar, 1L) * max(nfac, 1L) * max(ngrp, 1L)} panels.",
                        "i" = "Use {.arg columns} to keep fewer estimate columns."))
    rw <- if (ngrp > 1L && nvar > 1L) ggplot2::vars(.data$group, .data$var)
          else if (ngrp > 1L) ggplot2::vars(.data$group)
          else if (nvar > 1L) ggplot2::vars(.data$var) else NULL
    p <- p + ggplot2::facet_grid(
      rows = rw, cols = if (nfac > 1L) ggplot2::vars(.data$facet) else NULL,
      scales = if (one) "free_y" else "free", space = "free_y", switch = "y")
  }

  # --- labels ---------------------------------------------------------------------------------------
  # when the columns do not share a scale, the unit moves into the strip and the axis title goes: one
  # ggplot object still comes back (no ggh4x, no gtable), which is what keeps `+ theme()` working.
  rm_  <- reg_call(x)
  outn <- if (!is.null(rm_)) paste(unique(rm_$outcome), collapse = " / ")
          else as.character(e$col_var[1])
  xt <- if (one) with_legend_lang(lang, function(lg)
    fp_unit_word(scl$unit, reg_eff_word_of(x, mcol), conf, outn)) else NULL
  cap <- fp_caption(x, unique(as.character(e$column)), caption, subtext,
                    isTRUE(legend) && is.null(gtxt), th, lang)
  p + tx_plot_theme(cols) +
    ggplot2::labs(x = xt, y = NULL, title = cap$title, caption = cap$caption) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   strip.placement = "outside",
                   legend.key = ggplot2::element_rect(fill = cols$bg, colour = NA))
}

# the effect word a regression column's own legend uses (OR / IRR / RR / AME / beta), so the axis title
# and the footer name the same thing. NA on a cross-table, where the unit word stands alone.
# Phase 19m-i: the gate is the STORED KIND, not "does it still carry the recipe". The column's own
# `model_family` / `scale` are what legend_reg_eff_word() reads first, so a meta-stripped reg table
# keeps its axis word instead of silently losing it -- the same split the legend itself made in 19l.
#' @keywords internal
reg_eff_word_of <- function(x, col_nm) {
  if (!tab_is_reg(x) || is.null(x[[col_nm]])) return(NA_character_)
  legend_reg_eff_word(x[[col_nm]], reg_call(x))
}

# The caption: the table's whole footer EXCEPT the colour ladder, which the guide now carries (ruling
# D6 -- it must never be printed twice). `want_legend` is TRUE only when no guide could be built, in
# which case the whole prose legend comes back and nothing has to be added.
#
# What the guide CANNOT say, and the prose legend did: which interval was computed, and any caveat the
# measure carries. Those come back as one line each, from the legend's own producers -- so the figure
# still names its method, in the same words the table would.
#' @keywords internal
fp_caption <- function(x, cols, caption, subtext, want_legend, theme, lang) {
  ttl  <- caption %||% get_caption(x) %||%
    with_legend_lang(lang, function(lg) reg_title(reg_call(x)))
  sub  <- if (isTRUE(subtext)) get_subtext(x) else character(0)
  foot <- suppressWarnings(rd_footer(x, "plain", theme = theme, want_legend = want_legend,
                                     subtext = if (length(sub)) sub else character(0), lang = lang))
  meth <- if (want_legend) character(0) else fp_method_line(x, cols, lang)
  out  <- c(meth, foot)
  list(title = if (is.null(ttl) || !nzchar(ttl)) NULL else ttl,
       caption = if (!length(out)) NULL else paste(out, collapse = "\n"))
}

#' @keywords internal
fp_method_line <- function(x, cols, lang) {
  with_legend_lang(lang, function(lg) {
    sp <- Filter(function(s) s$col_name %in% cols, legend_specs(x))
    if (!length(sp)) return(character(0))
    sp <- lapply(sp, function(s) legend_resolve_spec(s, lg))
    ph <- unique(stats::na.omit(vapply(sp, function(s) s$method_phrase %||% NA_character_,
                                       character(1))))
    ph <- ph[nzchar(ph)]
    if (!length(ph)) return(character(0))
    legend_ucfirst(paste(ph, collapse = "; "))
  })
}
