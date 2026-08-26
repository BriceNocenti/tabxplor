# PURPOSE: the package's data charts -- forest_plot() (the RESULTS: estimate + interval +
#   significance, for a tab() crosstab or a tab_reg() table) and reg_check_plots() (the model checks,
#   drawn). Plus the ONE model they and every future chart read: tab_estimates().
#   (An exporter renders the TABLE; these render its NUMBERS.)
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
# ONE AXIS PER PANEL (Phase 22e-ii). A table may hold an odds ratio (log axis) beside a mean
#   difference (identity), and ggplot has ONE scale per aesthetic. So x is pre-transformed into each
#   scale's own space, the plot's scale is identity, and breaks AND labels are resolved per panel
#   through a lookup keyed on that panel's forced limits -- which ggplot resolves once per panel under
#   free scales. The forcing frame (a geom_blank + expand = 0) is what makes the key exact, so no layer
#   may reach past it: the gap band is clamped in data space instead. One RANGE per scale key, not per
#   panel -- panels measuring the same thing must stay comparable.
#
# WHAT A FOREST PLOT DRAWS IS A DEVIATION, AND ONLY ONE. The position is the effect (or, on a
#   crosstab, whatever `color =` grades); the LEVEL it sits on is printed above the whisker, so
#   position and number say two different things. The whisker takes the cell's colour whole --
#   significance is read off it, which is why the figure has no stars. A table's SECOND colour channel
#   is not drawn: it has no interval, no neutral and nowhere positional to go.
#
# THE READING AXIS IS TRAINED FIRST, ON PURPOSE. A discrete scale's order is the order it is trained
#   in, so the first layer maps the whole model; anything that must sit ON a row maps `ypos` itself
#   and lets ggplot place it. A row NUMBER is a coordinate on the full level set, which in a panel
#   holding four of eighteen levels is far outside its range.
#
# ROLE. reg_check_plots() is TEACHING ONLY, and its documentation says so in the first sentence: every
#   decision-grade number is already a footer row of the table, for every model column, in every export
#   (R/reg-assumptions.R). This function exists to show a class what a violation LOOKS like, and to let
#   a careful reader look closer. Nothing in the workflow requires calling it.
#
# ONE ENGINE, TWO ENTRY FORMS (ruling R1). A tab_reg() table, or a bare fit. Both reduce to the same
#   context (fit, frame, family, weights, shapes), so the panel builders never branch on the form.
#   The table form REFITS through reg_fit() itself, from the ~4 KB recipe stored in reg_meta$fit_spec:
#   a 60 ms teaching cost, against the ~10 MB per retained fit that was the measured cause of the
#   Phase-o jamovi freeze. There is no second fitting path to keep in sync. The microdata it refits on
#   is normally found from the NAME the table was built with (fit_spec$data_expr), so `data =` is
#   written once; the N guard is what makes that safe.
#
# ONE GRID PER MODEL (ruling R10, revised in 22e-i). A grid carries one model's heading and the panel
#   set of ITS OWN family -- faceting several models into one panel could only ever apply the first
#   model's family to all of them, which is wrong the moment a table mixes outcomes.
#
# THE PANEL SET IS REG_CHECKS. A panel and a footer row are the same check, so their titles, their
#   applicable families, their thresholds (`panel_marks`), whether the default grid draws them
#   (`panel_default`) and the `check =` vocabulary all come from that one table; two of its rows are
#   TAUGHT BUT NEVER SCORED (residuals, normality -- measured non-discriminating as verdicts, canonical
#   as lessons) and say so by carrying no discriminator.
#
# THE PANEL HEADLINE IS PLOTMATH. One bold assumption word, one plain question, one line -- and the
#   linearity y axis is a real formula. bquote() buys both without a third Suggests package; the cost
#   is that the title THEME element must stay plain, or bold() would spread over the whole line.
#   ⚠ AND that a label may use only what plotmath draws with a RULE or as ordinary text: a math-mode
#   space, a call's parentheses and `=` / `<` / `>` come from the Adobe SYMBOL font, which `ragg` --
#   Positron's and RStudio's device -- draws as missing-glyph boxes. The rule, its measurements and
#   the safe substitutes are in rd_link_expr()'s WARNING (R/reg-assumptions.R); it is locked by
#   test-tab_reg-plots.R.
#
# KEY CONSTRAINTS:
#   - ggplot2 + gridExtra are Suggests -> every entry point guards with requireNamespace().
#   - NOTHING here identifies a row or a column by its rendered label or its name prefix. The column
#     axis is `role` + `col_var`, the row axis is tab_render_vars() + tab_row_roles(), the scale is
#     fmt_scale_of(). (or_plot()'s `^Emp\\.` prefix match is the defect this rule exists to prevent.)
#   - NEVER geom_smooth(method = "auto") -- see the warning in R/reg-assumptions.R's primitives.
# See: dev/regression_effect_plots.md (forest_plot), dev/regression_assumptions_plots.md (the checks).

# Guard the Suggests packages a plot needs.
tx_plot_deps <- function(pkgs = c("ggplot2", "gridExtra")) {
  tx_need_pkg(pkgs, "This plot")
}

# === SECTION: the shared theme seam =================================================================

# THE plot theme, from the same `tx_chrome_hex()` vocabulary the tables use (z11: light / dark /
# print), so a diagnostic panel beside a table is the same object in the same clothes. It replaced the
# five hard-coded "#c00000" literals: a publication palette matters, because a diagnostic panel is exactly
# what ends up in a thesis appendix in greyscale.
#' @keywords internal
tx_plot_colors <- function(theme = NULL) {
  th <- tx_theme_resolve(theme)          # ggplot bakes its colours: "auto" cannot be honoured
  ch <- tx_chrome_hex(th)
  list(theme = th, text = ch$text, grey = ch$grey, grey2 = ch$grey2 %||% ch$text, bg = ch$bg,
       # `grey` is the INK of a band or a gridline; `subtle` is the ink of WORDS that must stay
       # readable while sitting back from the title -- the table's own grey is too light for that on
       # white, and axis labels are as hard to read as a subtitle.
       subtle = if (identical(th, "dark")) "#BEBEBE"
                else if (tx_is_print(th))  ch$grey2 %||% "#3F3F3F" else "#555555",
       # the accent: a hue under colour themes, pure black under `print` (a greyscale panel leans on
       # line TYPE, not on a hue that photocopies to the same grey as the data)
       accent = if (tx_is_print(th)) "#000000" else "#c00000",
       point  = if (identical(th, "dark")) "#8fb8dd" else if (tx_is_print(th)) "#000000"
                else "#33648c")
}

#' @keywords internal
tx_plot_theme <- function(cols) {
  ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold", size = 10, colour = cols$text),
      plot.subtitle = ggplot2::element_text(size = 8.5, colour = cols$subtle, lineheight = 1.15),
      text          = ggplot2::element_text(colour = cols$text),
      axis.text     = ggplot2::element_text(colour = cols$subtle),
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
  # twice and pair an odds ratio with a percentage.
  out <- nm[role == "model"]
  # `observed = "ci"` is the only mode that needs the crude column ITSELF (it draws its own interval);
  # "band" / "point" read the `obs` field, which already rides on the model column.
  if (identical(observed, "ci")) {
    out <- unique(c(out, stats::na.omit(vapply(nm[role == "model"],
                                               function(n) est_crude_of(x, n), character(1)))))
  }
  if (identical(what, "level")) {
    # the observed-vs-adjusted levels: the crude column beside each model one. There is exactly one
    # per model column now, and it carries its level in the field its scale names -- so the test is
    # simply "does it hold a level", never a guess from what the cell happens to display.
    lv <- nm[role == "emp"]
    out <- unique(c(out, lv[vapply(lv, function(n) any(is.finite(est_level_of(x[[n]]))),
                                   logical(1))]))
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
#' @param theme Palette theme for the colour columns (\code{"light"} / \code{"dark"} / a publication
#'   palette; \code{NULL} follows \code{getOption("tabxplor.export_theme")}).
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
  # ...plus the Total row when IT is the reference (`ref = "tot"`): it is not a summary there, it is
  # the baseline every deviation is measured from, and a forest plot without its anchor row is
  # missing the one number the whole panel is about.
  ref_row <- Reduce(`|`, lapply(cols, function(n) is_refrow(x[[n]])), rep(FALSE, nrow(x)))
  keep <- keep | (ax$roles %in% "total" & ref_row)
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
    # ⚠ zscore_formula(), NOT conf_level_to_z(): the latter ROUNDS to 1.96 (it exists to name colour
    # BREAKS), and the band has to be the very interval fmt_gap_bounds() draws and fmt_gap_p() inverts.
    half <- zscore_formula(get_conf_level(col)) * gse
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
      # what a MARK is painted with: the cell's own colour, except under a publication palette where
      # the text ink is all black and a point borrows the grey ramp (fmt_point_palette). Identical to
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
  # `what = "level"` needs a stored level. Every model column now carries its adjusted prediction, so
  # the only tables left without one are those whose scale names no level at all (a link-scale
  # coefficient, a cumulative odds ratio).
  if (identical(what, "level") &&
      !any(is.finite(res$estimate[res$role %in% c("model", "")])))
    cli::cli_abort(c("{.code what = \"level\"} needs a percentage or a mean, and this table has none.",
                     "i" = paste("These estimates sit on no single level --- a log-scale coefficient,",
                                 "or a cumulative odds ratio, has none to plot.")))
  res$var    <- factor(res$var,   levels = unique(res$var))
  res$level  <- factor(res$level, levels = unique(res$level))
  res$facet  <- factor(res$facet, levels = unique(res$facet))
  res
}


# === SECTION: getting a fit ==========================================================================

# The (fit, frame, family, label, ...) contexts a call is about. A bare model gives one; a tab_reg()
# table gives one PER MODEL COLUMN, and each becomes its own titled grid (ruling R10, revised in
# 22e-i: faceting by model could only ever draw ONE family's panel set, which is wrong the moment a
# table mixes outcomes).
#' @keywords internal
reg_plot_fits <- function(x, data = NULL, caller = parent.frame()) {
  if (!inherits(x, "tbl_df") && !is.data.frame(x)) {
    # the secondary form: a bare lm / glm / svyglm / polr / multinom / svyolr
    fr <- tryCatch(stats::model.frame(x), error = function(e) NULL)
    return(list(list(fit = x, data = if (is.null(data)) fr else data,
                     family = reg_plot_family_of(x), outcome = reg_plot_dep_of(x),
                     predictors = reg_plot_preds_of(x), trials = NULL, wt = NULL, design = NULL,
                     nobs = tryCatch(stats::nobs(x), error = function(e) NA_integer_),
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
  if (is.null(data)) data <- reg_plot_recover_data(fs$data_expr, caller)
  svy  <- svy_unwrap_data(data, "reg_check_plots")
  if (!is.null(svy)) data <- svy$data
  ds   <- list(design = if (is.null(svy)) NULL else svy$spec$design,
               wt = if (is.null(svy)) fs$wt else svy$spec$wt)
  if (!is.null(ds$wt) && is.na(ds$wt)) ds$wt <- NULL
  if (!is.null(ds$wt) && !ds$wt %in% names(data)) ds$wt <- NULL
  # the same preparation the table was built on -- the `shape` recodes and the `ref` anchors -- so
  # the refit below is the SAME MODEL, not a look-alike on raw columns.
  data <- reg_prepare_replay(data, fs$prep)
  if (!is.null(ds$design)) ds$design$variables <- data
  nobs_tab <- reg_plot_nobs(x)
  why <- NULL                        # the first refit's own error, kept to explain an empty result
  out <- purrr::imap(fs$specs, function(sp, i) {
    f <- tryCatch(suppressMessages(suppressWarnings(reg_fit(
      data, sp$outcome, sp$predictors, sp$fit_family, ds, isTRUE(sp$est$exp),
      reg_outcome_level_of(sp$outcome_level) %||% fs$outcome_level,
      fs$conf_level, fs$method, trials = sp$trials, formula = sp$formula,
      multiplier = fs$multiplier, drop_extra = fs$na_shared_vars,
      add_terms = c(reg_shape_add(fs$shape_terms, sp$predictors),
                    reg_cross_add(fs$crosses, sp$cross))))),
      error = function(e) { if (is.null(why)) why <<- conditionMessage(e); NULL })
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
    list(fit = f$fit, digest = f$digest, data = f$data, family = sp$fit_family, outcome = sp$outcome,
         predictors = sp$predictors, trials = sp$trials, wt = ds$wt, design = ds$design,
         positive_level = f$positive_level, label = sp$label, nobs = f$nobs,
         # a model COMPARISON is the only case where the label says something the outcome does not
         compare = sum(vapply(fs$specs, function(z) z$outcome, character(1)) == sp$outcome) > 1L,
         anchors = fs$prep$anchors, shapes = fs$prep$shapes,
         shape_terms = reg_shape_keep(fs$shape_terms, sp$predictors))
  }) |> purrr::compact()
  if (!length(out))
    cli::cli_abort(c("No model could be refitted from {.arg x}.",
                     if (!is.null(why)) c("x" = why),
                     "i" = "Check that {.arg data} is the data the table was built from."))
  out
}

# The quadratic terms of ONE model, keyed by variable -- reg_shape_add()'s named twin, which the
# panels need in order to name a term and to know which predictor is fitted as a curve.
#' @keywords internal
reg_shape_keep <- function(shape_terms, predictors) {
  if (is.null(shape_terms) || !length(shape_terms)) return(character(0))
  shape_terms[intersect(names(shape_terms), predictors)]
}

# The data a table was built from, found again. A tab_reg() table records the EXPRESSION its `data`
# argument was written as; a bare NAME is re-resolvable (cheap, side-effect-free, and the guard below
# catches a name that now holds something else), so it is the only form followed. Anything else --
# a pipeline, a subset, `.` -- is not re-run behind the user's back.
#' @keywords internal
reg_plot_recover_data <- function(expr, caller) {
  hint <- c("i" = "Diagnostics need the microdata; the table stores only the recipe.",
            "x" = "e.g. {.code reg_check_plots(t, gss_simple)}.")
  ok   <- !is.null(expr) && nzchar(expr) && expr != "." && make.names(expr) == expr
  if (!ok)
    cli::cli_abort(c("{.arg data} is required with a {.fn tab_reg} table.",
                     if (!is.null(expr) && nzchar(expr))
                       c("i" = "It was built from {.code {expr}}, which is not a name to look up."),
                     hint))
  d <- tryCatch(get(expr, envir = caller), error = function(e) NULL)
  if (!is.data.frame(d) && !inherits(d, "survey.design"))
    cli::cli_abort(c("{.arg data} is required with a {.fn tab_reg} table.",
                     "i" = "{.code {expr}}, the data it was built from, is not here any more.", hint))
  d
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
# which families; this switch says HOW each is drawn. Every builder takes ONE context -- a grid
# belongs to one model (ruling R10, revised in 22e-i) -- and returns a ggplot, or NULL when the data
# cannot support it.
#' @keywords internal
reg_panel_build <- function(key, cx, cols, opts) {
  switch(key,
         linearity       = reg_panel_linearity(cx, cols, opts),
         residuals       = reg_panel_residuals(cx, cols, opts),
         normality       = reg_panel_normality(cx, cols, opts),
         dispersion      = reg_panel_dispersion(cx, cols, opts),
         influence       = reg_panel_influence(cx, cols, opts),
         collinearity    = reg_panel_collinearity(cx, cols, opts),
         proportionality = reg_panel_proportionality(cx, cols, opts),
         NULL)
}

# The headline of a panel: the ASSUMPTION word from REG_CHECKS in bold, then the question the reader
# should ask, plain, on the same line -- so the word the footer row and `check =` use stays striking
# and the panel still says what to look at. plotmath keeps this dependency-free; the element's own
# face must therefore be plain, or the whole line would embolden.
#' @keywords internal
reg_panel_head <- function(key, question)
  bquote(bold(.(gettext(REG_CHECKS[[key]]$noun))) * .(paste0(": ", question)))

# The shared skin: the theme, the plain title face plotmath needs, and the wrapped subtitle.
#' @keywords internal
reg_panel_skin <- function(g, key, question, subtitle, cols)
  g + ggplot2::labs(title = reg_panel_head(key, question), subtitle = rd_wrap(subtitle)) +
    tx_plot_theme(cols) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "plain"))

# The reference line(s) a panel draws, as a LAYER: one row per declared mark (REG_CHECKS$panel_marks),
# the linetype carried in the data through an identity scale.
# WARNING: never pass a vector `linetype =` as a geom PARAM. ggplot2 replicates a layer's data across
# facets but not its params, so `geom_hline(yintercept = c(5, 10), linetype = c(...))` aborts the
# moment the panel is facetted (measured on ggplot2 4.0.3).
#' @keywords internal
reg_panel_mark_layers <- function(key, cols) {
  m <- reg_panel_marks(key)
  if (!length(m)) return(NULL)
  d <- data.frame(mark = m, lt = c("dashed", "dotted")[pmin(seq_along(m), 2L)])
  list(ggplot2::geom_hline(data = d,
                           ggplot2::aes(yintercept = .data$mark, linetype = .data$lt),
                           colour = cols$accent, linewidth = 0.6, inherit.aes = FALSE),
       ggplot2::scale_linetype_identity())
}

# 1. LINEARITY -- the observed binned curve of each continuous predictor against the shape the MODEL
# fits (rd_comparator: a straight line, a parabola where `shape = "quadratic"` put a curvature term
# in). Never a smoother: the assumption IS the shape, so a smoother would trace the very departure
# the panel exists to show. An ordinal or multinomial outcome gives one curve per cut / per category
# (rd_link_cuts), which is the standard reading and the only one that shows a proportional-odds
# departure for a NUMERIC predictor.
reg_panel_linearity <- function(cx, cols, opts) {
  num <- reg_numeric_preds(cx$data, cx$predictors)
  if (!is.null(opts$predictors)) num <- intersect(num, opts$predictors)
  if (!length(num)) return(NULL)
  ly <- rd_link_cuts(cx$data[[cx$outcome]], cx$family, cx$trials, cx$positive_level, cx$outcome)
  if (is.null(ly)) return(NULL)
  w   <- if (!is.null(cx$wt) && cx$wt %in% names(cx$data)) cx$data[[cx$wt]] else NULL
  drw <- cx$data[[svy_row_col]]
  sq  <- names(cx$shape_terms %||% character(0))
  rows <- purrr::list_rbind(purrr::map(num, function(v) {
    # the x axis is the ONE reading of a predictor's own values a plot makes, so it is the second
    # place a `ref` anchor must be added back (the first is reg_spec_tips_num()'s tooltip). The
    # curve's SHAPE is invariant under a location shift; only the axis labels are not.
    x <- cx$data[[v]] + reg_anchor_of(cx$anchors, v)
    purrr::list_rbind(purrr::map(ly$curves, function(cu) {
      # Phase 18z16-iv (W-G.4): the band takes the DESIGN variance when the user handed a
      # svydesign to reg_check_plots(), the exact flat closed form on a plain weight column, and is
      # unchanged (n) unweighted.
      b <- rd_bin(x[cu$keep], cu$y, w[cu$keep], opts$nbins, ly$link,
                  design = cx$design, des_rows = drw[cu$keep])
      if (is.null(b)) return(NULL)
      dplyr::mutate(b, fit = rd_comparator(b$x, b$y, v %in% sq),
                    predictor = reg_pred_facet(v, cx), cut = cu$cut)
    }))
  }))
  if (is.null(rows) || !nrow(rows)) return(NULL)
  # one curve = the panel's own blue; several = the cut is what the colour says.
  many <- !all(is.na(rows$cut))
  if (!many) rows$cut <- ""
  curve <- if (many)
    list(ggplot2::geom_line(ggplot2::aes(colour = .data$cut), linewidth = 0.7, na.rm = TRUE),
         ggplot2::geom_point(ggplot2::aes(colour = .data$cut, size = .data$n), na.rm = TRUE))
  else
    list(ggplot2::geom_line(colour = cols$point, linewidth = 0.7, na.rm = TRUE),
         ggplot2::geom_point(ggplot2::aes(size = .data$n), colour = cols$point, na.rm = TRUE))
  g <- ggplot2::ggplot(rows, ggplot2::aes(x = .data$x, y = .data$y, group = .data$cut)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$y - 2 * .data$se, ymax = .data$y + 2 * .data$se),
                         fill = cols$grey, alpha = 0.25, colour = NA, na.rm = TRUE) +
    ggplot2::geom_line(ggplot2::aes(y = .data$fit), colour = cols$accent,
                       linetype = "dashed", linewidth = 0.6, na.rm = TRUE) +
    curve +
    ggplot2::scale_size(range = c(0.6, 2.4), guide = "none") +
    ggplot2::labs(x = NULL, y = ly$expr, colour = NULL) +
    ggplot2::facet_wrap(~ predictor, scales = "free_x",
                        ncol = opts$facet_ncol %||% min(2L, length(num)))
  reg_panel_skin(
    g, "linearity",
    gettext("does the observed curve follow the model's shape?"),
    gettextf("%d bins, +/-2 SE. The dashed line is what the model fits; a bending curve asks for shape=\"quadratic\".", opts$nbins),
    cols) +
    ggplot2::theme(legend.position = if (many) "bottom" else "none",
                   strip.text = ggplot2::element_text(face = "bold", size = 9))
}

# The facet label of a numeric predictor: the terms the MODEL carries for it, so a cured predictor is
# not read against a line the model no longer fits. `log`/`sqrt` recoded the column itself
# (reg_prepare_replay), so their mark IS the label; a quadratic added a second term beside it.
#' @keywords internal
reg_pred_facet <- function(v, cx) {
  mk <- shape_mark(cx$shapes[[v]]$kind %||% NA_character_, v)
  if (!is.na(mk)) return(mk)
  if (v %in% names(cx$shape_terms %||% character(0)))
    return(paste0(v, " + ", reg_shape_sq_level(v)))
  v
}

# 2. RESIDUALS -- binned residuals against the fitted value, or against the expected CATEGORY where
# the fit has one per level rather than one per row (rd_fitted_1d()). The classic lesson about why a
# RAW residual is useless for a binary outcome (it takes exactly two values given p-hat), and the
# reason every non-gaussian family here uses a randomised quantile residual instead.
reg_panel_residuals <- function(cx, cols, opts) {
  r <- rd_resid(cx$fit, cx$family, cx$data[[cx$outcome]], cx$trials, opts$seed)
  # WARNING: NOT `as.numeric(fitted())` -- an ordinal fit's is the n x K probability matrix, so that
  # gave n*K values, the length guard fired, and a panel REG_CHECKS DECLARES was silently dropped.
  f <- rd_fitted_1d(cx$fit, cx$family)
  if (is.null(r) || is.null(f) || length(f) != length(r)) return(NULL)
  ordinal <- identical(reg_check_family_of(cx$family), "ordinal")
  rows <- rd_bin(f, r, NULL, max(5L, min(60L, floor(sqrt(length(r))))), "identity")
  if (is.null(rows) || !nrow(rows)) return(NULL)
  g <- ggplot2::ggplot(rows, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = cols$grey) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = -2 * .data$se, ymax = 2 * .data$se),
                         fill = cols$grey, alpha = 0.2, na.rm = TRUE) +
    ggplot2::geom_point(colour = cols$point, size = 1.2, na.rm = TRUE) +
    ggplot2::labs(x = if (ordinal) gettext("Expected category") else gettext("Fitted value"),
                  y = gettext("Mean residual"))
  reg_panel_skin(
    g, "residuals",
    gettext("are fewer than 5 % of the points outside the band?"),
    gettext("Mean residual per bin of the fitted value: a trend, or many points out, means the model is missing something."),
    cols)
}

# 3. NORMALITY -- the Q-Q plot of the dispatched residual, against the ANALYTIC pointwise band.
reg_panel_normality <- function(cx, cols, opts) {
  r <- rd_resid(cx$fit, cx$family, cx$data[[cx$outcome]], cx$trials, opts$seed)
  rows <- if (is.null(r)) NULL else rd_qq(r, opts$conf, min(opts$max_points, 400L))
  if (is.null(rows) || !nrow(rows)) return(NULL)
  g <- ggplot2::ggplot(rows, ggplot2::aes(x = .data$theoretical, y = .data$sample)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lo, ymax = .data$hi),
                         fill = cols$grey, alpha = 0.2, na.rm = TRUE) +
    ggplot2::geom_abline(slope = 1, intercept = 0, colour = cols$accent,
                         linetype = "dashed", linewidth = 0.6) +
    ggplot2::geom_point(colour = cols$point, size = 0.7, alpha = 0.6, na.rm = TRUE) +
    ggplot2::labs(x = gettext("Theoretical quantiles"), y = gettext("Quantile residuals"))
  reg_panel_skin(
    g, "normality",
    gettext("do the points follow the diagonal?"),
    gettext("The band is pointwise: about 5 % of points fall outside it even when the model is right."),
    cols)
}

# 4. DISPERSION -- the model's own standard errors against the robust (sandwich) ones, coefficient by
# coefficient. It is exactly the footer row, un-maximised: the row prints the largest of these points'
# distance from the diagonal.
reg_panel_dispersion <- function(cx, cols, opts) {
  rows <- local({
    se <- reg_check_model_se(cx$fit)
    cif <- reg_coef_if_maker(reg_model_of(cx), cx$data)
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
                   model_se = se, robust_se = rb)
  })
  if (is.null(rows) || !nrow(rows) || all(is.na(rows$robust_se))) return(NULL)
  g <- ggplot2::ggplot(rows, ggplot2::aes(x = .data$model_se, y = .data$robust_se)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, colour = cols$accent,
                         linetype = "dashed", linewidth = 0.6) +
    ggplot2::geom_point(colour = cols$point, size = 1.6, na.rm = TRUE) +
    ggplot2::labs(x = gettext("Model SE"), y = gettext("Robust SE"))
  reg_panel_skin(
    g, "dispersion",
    gettext("do the two standard errors agree?"),
    gettext("One point per coefficient: off the line, the family's variance assumption fails."),
    cols)
}

# 5. INFLUENCE -- the per-observation version of the footer row: max_j |dfbeta_ij| / SE_j, i.e. how far
# one respondent moves the coefficient it moves most, in that coefficient's own standard errors.
reg_panel_influence <- function(cx, cols, opts) {
  rows <- local({
    se  <- reg_check_model_se(cx$fit)
    cif <- reg_coef_if_maker(reg_model_of(cx), cx$data)
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
    tibble::tibble(index = keep, dfbeta = m[keep])
  })
  if (is.null(rows) || !nrow(rows)) return(NULL)
  g <- ggplot2::ggplot(rows, ggplot2::aes(x = .data$index, y = .data$dfbeta)) +
    reg_panel_mark_layers("influence", cols) +
    ggplot2::geom_point(colour = cols$point, size = 0.7, alpha = 0.5, na.rm = TRUE) +
    ggplot2::labs(x = gettext("Observation"), y = gettext("max |dfbetas|"))
  reg_panel_skin(
    g, "influence",
    gettext("does one respondent carry a result?"),
    gettext("How far each respondent moves the coefficient it moves most, in that coefficient's standard errors."),
    cols)
}

# 6. COLLINEARITY -- the VIF of every term, on the 5 / 10 ladder every textbook uses.
reg_panel_collinearity <- function(cx, cols, opts) {
  rows <- local({
    v <- tryCatch(tx_vif(cx$fit), error = function(e) NULL)
    if (is.null(v) || !length(v)) return(NULL)
    val <- if (is.matrix(v)) { if (ncol(v) >= 3L) v[, 3]^2 else v[, 1] } else as.numeric(v)
    nm  <- if (is.matrix(v)) rownames(v) else names(v)
    tibble::tibble(term = if (is.null(nm)) as.character(seq_along(val))
                          else reg_term_label(nm, cx$shape_terms),
                   vif = as.numeric(val))
  })
  if (is.null(rows) || !nrow(rows)) return(NULL)
  g <- ggplot2::ggplot(rows, ggplot2::aes(x = stats::reorder(.data$term, .data$vif), y = .data$vif)) +
    ggplot2::geom_col(fill = cols$point, width = 0.6) +
    reg_panel_mark_layers("collinearity", cols) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = gettext("VIF"))
  reg_panel_skin(
    g, "collinearity",
    gettext("can the data tell the predictors apart?"),
    gettext("Variance inflation per term (5 and 10 are the usual thresholds). It widens intervals; it biases nothing."),
    cols)
}

# 7. PROPORTIONALITY (ordinal) -- the empirical cumulative logit of each cut, per predictor level. The
# proportional-odds assumption says these lines are PARALLEL; the Brant p in the footer tests it.
reg_panel_proportionality <- function(cx, cols, opts) {
  if (cx$family != "ordinal") return(NULL)
  y  <- as.factor(cx$data[[cx$outcome]])
  lv <- levels(y)
  if (length(lv) < 3L) return(NULL)
  fp <- reg_factor_preds(cx$data, cx$predictors)
  if (!length(fp)) return(NULL)
  w  <- if (!is.null(cx$wt) && cx$wt %in% names(cx$data)) cx$data[[cx$wt]] else rep(1, nrow(cx$data))
  rows <- purrr::list_rbind(purrr::map(fp, function(v) {
    g <- as.factor(cx$data[[v]])
    purrr::list_rbind(purrr::map(seq_len(length(lv) - 1L), function(k) {
      above <- as.integer(as.integer(y) > k)
      num <- as.numeric(tapply(w * above, g, sum))
      den <- as.numeric(tapply(w, g, sum))
      p   <- (num + 0.5) / (den + 1)
      tibble::tibble(level = levels(g), logit = log(p / (1 - p)),
                     cut = gettextf("> %s", lv[[k]]), predictor = v)
    }))
  }))
  if (is.null(rows) || !nrow(rows)) return(NULL)
  g <- ggplot2::ggplot(rows, ggplot2::aes(x = .data$level, y = .data$logit,
                                          group = .data$cut, colour = .data$cut)) +
    ggplot2::geom_line(na.rm = TRUE) + ggplot2::geom_point(size = 1.2, na.rm = TRUE) +
    ggplot2::labs(x = NULL, y = gettext("Empirical cumulative logit"), colour = NULL) +
    ggplot2::facet_wrap(~ predictor, scales = "free_x",
                        ncol = opts$facet_ncol %||% min(4L, length(fp)))
  reg_panel_skin(
    g, "proportionality",
    gettext("are the lines parallel?"),
    gettext("One line per cut of the outcome: parallel means one odds ratio fits every cut."),
    cols) +
    # the legend takes half the panel's width at the right, and it is one short row of words
    ggplot2::theme(legend.position = "bottom",
                   axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
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
#' One call diagnoses **every model** in the table (each outcome, each compared model): one titled
#' grid per model, each drawing the panels its own family allows. Pass a [tab_reg()] table --- the
#' data it was built from is usually found on its own --- or a fitted model directly.
#'
#' @param x A [tab_reg()] table, or a fitted model (`lm` / `glm` / `svyglm` / `polr` / `multinom` /
#'   `svyolr`).
#' @param data The data frame or `survey::svydesign` the table was built from. **Usually unnecessary**:
#'   a table records the name it was called with, and when that name still holds a data set the same
#'   size, it is used --- otherwise the call stops rather than draw the wrong model. Give `data`
#'   explicitly when the table was built from an expression rather than a named object
#'   (`tab_reg(gss |> dplyr::filter(...), ...)`), or when the name has since changed. Ignored with a
#'   bare model. (A table stores a ~4 KB recipe and refits from it; it never keeps the fitted models.)
#' @param check Which panels to draw. `"auto"` (default) draws the panels that *decide* something the
#'   footer cannot say in one number --- linearity, residuals, normality, influence, and
#'   proportionality for an ordinal outcome. `"all"` adds dispersion and collinearity, whose footer
#'   row is normally enough. Or name them: any of `"linearity"`, `"residuals"`, `"normality"`,
#'   `"dispersion"`, `"influence"`, `"collinearity"`, `"proportionality"` --- the same words the
#'   footer rows and [tab_reg()]'s `stats` argument use.
#' @param predictors Optional: restrict the linearity panel to these continuous predictors.
#' @param ncol Number of panel columns in the assembled grid (default: as square as it can be, 3 at
#'   most).
#' @param facet_ncol Number of facet columns *inside* a panel (default: 2 for linearity, 4 for
#'   proportionality).
#' @param theme `"light"`, `"dark"`, or a black-and-white publication palette (`"print_ready"` and
#'   friends -- greyscale, for a thesis appendix). Defaults to
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
#' @return Invisibly, the assembled `gtable` --- or, with several models, the named list of them, one
#'   per model, all drawn on the current graphics device.
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
#'   reg_check_plots(t)
#' }
#' }
#' @export
reg_check_plots <- function(x, data = NULL, check = "auto", predictors = NULL,
                            ncol = NULL, facet_ncol = NULL, theme = NULL, lang = NULL,
                            max_points = 2000L, nbins = 10L, conf = 0.95,
                            seed = 20260810, ...) {
  tx_plot_deps()
  ctxs <- reg_plot_fits(x, data, caller = parent.frame())
  cols <- tx_plot_colors(theme)
  opts <- list(predictors = predictors, max_points = max_points, nbins = nbins, conf = conf,
               seed = seed, facet_ncol = facet_ncol)
  # ONE GRID PER MODEL. A grid carries one model's title, and its panel set comes from THAT model's
  # own family -- which is what a table mixing a binomial and an ordinal outcome needs, and what
  # faceting by model could never give.
  # i18n: the WHOLE label-building block under one language, as reg_model_lines() does.
  out <- with_legend_lang(lang, function(lg) purrr::map(ctxs, function(cx) {
    keys <- reg_panel_keys(cx, check)
    grobs <- purrr::compact(purrr::map(keys, function(k) reg_panel_build(k, cx, cols, opts)))
    if (!length(grobs)) return(NULL)
    gridExtra::grid.arrange(grobs = grobs, top = reg_check_top(cx, cols),
                            ncol = ncol %||% min(3L, ceiling(sqrt(length(grobs)))))
  }))
  names(out) <- vapply(ctxs, function(cx) cx$label %||% "", character(1))
  out <- purrr::compact(out)
  if (!length(out)) {
    cli::cli_abort(c("Nothing could be drawn for this model.",
                     "i" = "A multinomial outcome has no residual panel (its residuals depend on the category order)."))
  }
  invisible(if (length(out) == 1L) out[[1L]] else out)
}

# The panels ONE model gets: its own family's, narrowed to the default set, to everything, or to what
# the user named. Phase 19g (KEY 6): ONE vocabulary and ONE validator, shared with tab_reg(stats =)
# -- narrowed here to the model CHECKS, which are the only things a panel can be drawn for.
#' @keywords internal
reg_panel_keys <- function(cx, check = "auto") {
  weighted <- !is.null(cx$wt)
  all_keys <- reg_checks_for(cx$family, weighted, what = "panel")
  if (identical(check, "all")) return(all_keys)
  if (identical(check, "auto")) return(reg_panels_default(cx$family, weighted))
  reg_validate_stat_keys(check, arg = "check", allowed = names(REG_CHECKS))
  keys <- intersect(check, all_keys)
  if (!length(keys))
    cli::cli_abort(c("None of those checks can be drawn for a {.val {cx$family}} model.",
                     "i" = "Available here: {.val {all_keys}}."))
  keys
}

# The heading of one model's grid: what was fitted, then how. The formula is the model's OWN, with its
# terms passed through reg_term_label() so a curvature term reads "age2" and not its frozen literal;
# the second line takes the family's name from the footer's own vocabulary, so a plot and a table
# cannot name one model two ways.
#' @keywords internal
reg_check_top <- function(cx, cols) {
  terms <- reg_term_label(attr(stats::terms(stats::formula(cx$fit)), "term.labels"),
                          cx$shape_terms)
  head  <- gettextf("Assumption checks: %s ~ %s", cx$outcome,
                    paste(terms, collapse = " + "))
  fam   <- tryCatch(reg_family_display_name(cx$family), error = function(e) cx$family)
  # a plain space, not the table's narrow no-break one: a graphics device may be single-byte, and
  # U+202F then fails conversion with a warning (measured on the pdf device under testthat).
  sub   <- gettextf("%s, n = %s", fam, format(cx$nobs %||% NA_integer_, big.mark = " "))
  if (isTRUE(cx$compare) && !is.null(cx$label) && nzchar(cx$label))
    sub <- gettextf("%s - %s", cx$label, sub)
  gridExtra::arrangeGrob(
    grid::textGrob(head, gp = grid::gpar(fontface = "bold", fontsize = 11, col = cols$text)),
    grid::textGrob(sub,  gp = grid::gpar(fontsize = 9, col = cols$subtle)),
    ncol = 1, heights = grid::unit(c(1.3, 1), "lines"))
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

# === SECTION: the forest plot's axis =================================================================
#
# DESIGN: ggplot has ONE scale per aesthetic, so one `scale_x_continuous()` cannot carry two
# transforms -- and a table may hold an odds ratio (log) beside a mean difference (identity). The
# resolution: x is pre-transformed into each scale's own space and the plot's scale is identity, while
# breaks AND labels are resolved PER PANEL. With free scales ggplot calls both functions once per
# panel, so a lookup keyed on the panel's own limits dispatches them exactly.
# WARNING: the key is the limits, so they are forced (a `geom_blank` frame + `expand = expansion(0)`)
# and NO layer may reach past them -- the gap band is clamped in data space rather than allowed to
# widen a panel.

# The ladder, continued as far as the data goes. The colour rungs are exact (a gridline IS a
# threshold); a CONTINUATION rung is not, so it is rounded to a readable number. One rule for both
# geometries -- every ladder steps ~x2 per rung in its own metric (COLOR_SCALES' K2 check) -- so
# x2 -> x4 -> x8 and +0.3 -> +0.6 -> +1.2 are the same rule.
#' @keywords internal
fp_ladder <- function(scl, need, extra_max = 4L) {
  mag <- sort(unique(scl$break_mag[scl$break_dir > 0L]))
  if (!length(mag)) return(numeric(0))
  k <- 0L
  while (is.finite(need) && max(mag) < need && k < extra_max) {
    mag <- c(mag, signif(max(mag) * 2, 2)); k <- k + 1L
  }
  mag
}

# An axis label carries ONE decimal. The rounding happens in the scale the label is READ in: a
# percentage-point ladder is stored as 0.05 and printed as 5, so rounding the stored value first would
# turn +5 into +10.
#' @keywords internal
fp_round_mag <- function(m, is_pct) {
  k <- if (is_pct) 100 else 1
  r <- round(m * k, 1) / k
  if (isTRUE(r == 0) && m != 0) signif(m, 2) else r
}

# One record per FACET, but ONE RANGE PER SCALE KEY: panels measuring the same thing must be directly
# comparable -- that is what small multiples are for -- while panels on different units keep their own
# axis, which is the whole point of resolving the scale per panel.
#' @keywords internal
fp_scale_records <- function(e, x, what = "auto", lang = NULL, pad = 0.04, max_n = 9L) {
  recs <- list()
  for (key in unique(as.character(e$scale_key))) {
    d    <- e[as.character(e$scale_key) == key, , drop = FALSE]
    col  <- as.character(d$column[1])
    scl  <- fmt_scale_of(x[[col]], what)
    mult <- isTRUE(scl$mult); ntr <- scl$neutral
    tr   <- if (mult) function(v) log10(v) else function(v) v

    # the range the reader must see: the estimates, their intervals and the observed point. NOT the
    # gap band, which is background and would otherwise decide the axis.
    rng <- range(c(d$estimate, d$ci_inf, d$ci_sup, d$obs), na.rm = TRUE, finite = TRUE)
    if (!all(is.finite(rng))) rng <- if (is.finite(ntr)) c(ntr, ntr) else c(0, 1)
    need <- if (mult) max(rng[2] / ntr, ntr / rng[1], na.rm = TRUE) else max(abs(rng - ntr))
    mag  <- fp_ladder(scl, need)
    if (length(mag)) {
      pos <- if (mult) c(rev(ntr / mag), ntr, ntr * mag) else c(rev(ntr - mag), ntr, ntr + mag)
      dir <- c(rep(-1L, length(mag)), 0L, rep(1L, length(mag)))
      mg  <- c(rev(mag), 0, mag)
      # the first rung on each side is always in view: a plot where every whisker sits inside the
      # first colour threshold must SAY so, and a lone neutral line cannot.
      rng <- range(c(rng, max(pos[dir < 0L]), min(pos[dir > 0L])), na.rm = TRUE, finite = TRUE)
    } else { pos <- if (is.finite(ntr)) ntr else numeric(0); dir <- integer(0); mg <- numeric(0) }

    lim <- tr(rng)
    if (!all(is.finite(lim)) || diff(lim) <= 0) lim <- c(min(lim), max(lim)) + c(-0.5, 0.5)
    lim  <- lim + c(-1, 1) * pad * diff(lim)
    at   <- tr(pos)
    keep <- which(is.finite(at) & at >= lim[1] & at <= lim[2])
    all_ <- keep
    if (!length(mag)) {
      # a LEVEL panel grades no deviation, so it has no ladder: round numbers are the only case where
      # the axis is not the colour ladder.
      pos  <- pretty(rng, n = 5L); at <- tr(pos)
      k    <- at >= lim[1] & at <= lim[2]
      pos  <- pos[k]; at <- at[k]
      keep <- seq_along(at); all_ <- integer(0); dir <- rep(0L, length(at)); mg <- pos
    } else if (length(keep) > max_n) {
      # thin the LABELS from the outside in, whole pairs at a time: a log ladder crowds near the
      # neutral, and a one-sided ladder would misread as an asymmetric scale.
      lab_i <- keep[dir[keep] == 0L]
      for (m in sort(unique(mg[keep][dir[keep] != 0L]), decreasing = TRUE)) {
        pair <- keep[dir[keep] != 0L & mg[keep] == m]
        if (length(lab_i) + length(pair) > max_n) break
        lab_i <- c(lab_i, pair)
      }
      keep <- sort(lab_i)
    }
    labs <- vapply(keep, function(k)
      if (dir[k] == 0L) legend_num(pos[k] * if (isTRUE(scl$is_pct)) 100 else 1, lang)
      else legend_break_label(scl$label_meas, fp_round_mag(mg[k], isTRUE(scl$is_pct)), dir[k],
                              isTRUE(scl$is_pct), lang),
      character(1))
    if (isTRUE(scl$is_pct)) labs <- paste0(labs, "%")
    rv <- unique(d$ref_value[is.finite(d$ref_value)])
    recs[[key]] <- list(col = col, scl = scl, mult = mult, tr = tr, neutral = ntr, key = key,
                        ref = if (length(rv) == 1L) tr(rv) else NA_real_, lim = lim,
                        at = at[keep], dir = dir[keep], mag = mg[keep], labels = labs,
                        at_all = at[all_], dir_all = dir[all_], mag_all = mg[all_], unit = scl$unit)
  }
  # the limits ARE the lookup key, so two SCALES must never share them (two facets on one scale do)
  ks <- vapply(recs, function(z) paste(format(z$lim, digits = 15), collapse = "|"), character(1))
  while (anyDuplicated(ks)) {
    i <- anyDuplicated(ks)
    recs[[i]]$lim[2] <- recs[[i]]$lim[2] * (1 + 1e-9) + 1e-12
    ks <- vapply(recs, function(z) paste(format(z$lim, digits = 15), collapse = "|"), character(1))
  }
  out <- list()
  for (f in levels(droplevels(e$facet))) {
    i   <- which(e$facet == f)
    key <- names(sort(table(as.character(e$scale_key[i])), decreasing = TRUE))[1]
    # the SCALE is shared, the COLUMN is not: a multinomial risk ratio and an ordinal win ratio live
    # on one ladder but are two quantities, and the strip word comes from the facet's own column.
    # (assigned, not c()'d -- `c()` would append a second `col` and `$` would read the scale's.)
    r <- recs[[key]]; r$facet <- f; r$col <- as.character(e$column[i][1])
    out[[f]] <- r
  }
  out
}

# The two closures ggplot calls once per panel.
#' @keywords internal
fp_axis_fns <- function(scales) {
  kb <- function(v) paste(format(v, digits = 15), collapse = "|")
  s  <- scales[!duplicated(vapply(scales, function(z) z$key, character(1)))]
  bmap <- stats::setNames(lapply(s, `[[`, "at"),     vapply(s, function(z) kb(z$lim), character(1)))
  lmap <- stats::setNames(lapply(s, `[[`, "labels"), vapply(s, function(z) kb(z$at),  character(1)))
  list(breaks = function(lim) bmap[[kb(lim)]] %||% numeric(0),
       labels = function(br)  lmap[[kb(br)]]  %||% rep("", length(br)))
}

# fp_layout() -- which axis is READ and which is faceted.
#
# DESIGN: both orientations show the same numbers -- a cell's deviation is a stored field, not a
# property of where it is drawn -- so this is a legibility choice, and `pct` does not decide it
# (measured: a 3x6 table and a 7x3 table, both `pct = "row"`, want opposite orientations). The legible
# one is MORE levels down the side and FEWER panels across, which is what `"auto"` picks. It is NOT
# the default: a plot whose rows are the table's rows is the one a reader can check against the table
# in front of them, and silently swapping the two axes costs more than a cramped panel. The reference
# level of the panel axis carries no deviation, so it would be an empty panel: it is dropped, and the
# K-1 panels left are each one group's whole profile. On the reading axis it stays, as the anchor row
# at the neutral.
#' @keywords internal
fp_layout <- function(e, x, layout) {
  if (identical(layout, "keep") || !nrow(e)) return(e)
  if (identical(layout, "auto")) {
    if (tab_is_reg(x)) return(e)
    n_row <- length(unique(e$row[!e$is_ref]))
    n_col <- length(unique(as.character(e$facet)))
    if (n_col <= n_row) return(e)
  }
  e <- e[!e$is_ref, , drop = FALSE]
  if (!nrow(e)) return(e)
  new_var <- droplevels(factor(as.character(e$col_var)))
  new_lvl <- factor(as.character(e$column), levels = levels(e$facet))
  e$facet <- if (nlevels(droplevels(e$var)) <= 1L)
    droplevels(factor(as.character(e$level), levels = levels(e$level)))
  else droplevels(interaction(e$var, e$level, sep = ": ", drop = TRUE, lex.order = TRUE))
  e$var <- new_var; e$level <- new_lvl
  e$row <- as.integer(new_lvl)
  e
}

# The strip suffix and the axis title name the measure the SAME way, so a single-scale plot and a
# mixed one cannot disagree about what a column measures. `fp_unit_word("units")` names the OUTCOME,
# which the strip already carries, so a regression column is named by its own header word instead.
#' @keywords internal
fp_unit_strip <- function(x, s) {
  ew <- reg_eff_word_of(x, s$col)
  b  <- reg_word_base(ew)
  if (!is.na(b) && !is.null(REG_WORDS[[b]])) REG_WORDS[[b]]$long() else fp_unit_word(s$unit, ew)
}

#' @keywords internal
fp_axis_title <- function(x, s) {
  base <- legend_ucfirst(fp_unit_strip(x, s))
  cf   <- get_conf_level(x[[s$col]])[1]
  if (!nzchar(base) || !is.finite(cf)) base else gettextf("%s (%s%% CI)", base, format(100 * cf))
}

# The number printed above a whisker is the cell's OWN primary token -- the effect on a model column,
# the level it sits on for a crosstab (whose axis is already the deviation). `format()` stays the one
# string producer; only the display is swapped to that token first.
#' @keywords internal
fp_primary_text <- function(x, d, display = NULL) {
  out <- rep(NA_character_, nrow(d))
  for (nm in unique(as.character(d$column))) {
    i   <- which(as.character(d$column) == nm)
    dsp <- display %||% display_primary(get_display(x[[nm]]))
    txt <- trimws(format(set_display(x[[nm]], dsp)))
    out[i] <- txt[d$trow[i]]
  }
  out
}

# How many rows each block of panels holds -- the reading axis is free per BLOCK (the facet rows).
#
# WARNING: this gives the COUNT, never a position. A discrete scale's positions are the panel's own
# and are not the factor's level order once free scales have dropped levels, so anything that must sit
# ON a row maps `ypos` itself and lets ggplot place it. The count is safe, and it is all a rule
# stopping short of the panel edge needs.
#' @keywords internal
fp_ypos_index <- function(e) {
  e$blk   <- paste(as.character(e$var), as.character(e$group), sep = "\r")
  e$n_blk <- NA_integer_
  for (b in unique(e$blk)) {
    i <- which(e$blk == b)
    e$n_blk[i] <- nlevels(droplevels(e$ypos[i]))
  }
  e
}

# The slot a break stands for: rung 1..4 out from the neutral, 5..8 below it (the fmt slot map).
#' @keywords internal
fp_break_slots <- function(dir, mag) {
  o <- integer(length(dir))
  for (s in c(-1L, 1L)) {
    i <- which(dir == s); if (!length(i)) next
    o[i] <- pmin(as.integer(rank(abs(mag[i]), ties.method = "first")), 4L) + if (s < 0L) 4L else 0L
  }
  o
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
#' **What is drawn.** Always a \strong{deviation}: the effect a regression estimates, or, for a
#' cross-table, the comparison its \code{color =} grades -- a difference from the reference in
#' percentage points, a ratio or an odds ratio on a log axis. The \strong{level} the deviation sits on
#' (the percentage, the mean, the adjusted probability) is printed above each whisker instead, so the
#' position and the number say two different things. \code{what = "level"} swaps them.
#'
#' **The gridlines are the table's colour ladder** (\code{\link{set_color_breaks}}), one dashed rule
#' per rung in that rung's own colour, labelled with the same glyphs the footer uses -- and continued
#' beyond the last rung (each step twice the one before) as far as the data goes. The whisker takes
#' the colour of its cell, whole: significance is read off the whisker, so there are no stars.
#'
#' **A table that mixes units** (an odds ratio beside a mean difference) gets one axis per panel, each
#' in its own transform, and the unit moves into the strip. Panels that measure the SAME thing share
#' one range, so their effect sizes stay comparable.
#'
#' **The observed comparison.** With \code{empirical = TRUE}, a regression estimate carries its crude
#' counterpart. \code{observed = "band"} (the default when the gap was testable) draws a bracket around
#' the observed value at plus-or-minus the margin of error *of the difference*: the modelled point
#' falls outside it exactly when the gap test rejects. Two intervals must not be compared by overlap
#' when the estimators are correlated, which is why the crude interval is not drawn by default;
#' \code{observed = "ci"} restores that classic figure if you want it.
#'
#' @eval tab_args_rd("forest_plot")
#' @param columns Value columns to draw, by name. \code{NULL} (the default) draws the model columns of
#'   a regression table and every value column of a cross-table.
#' @param what \code{"auto"} (the quantity the table's own interval is centred on), \code{"effect"}
#'   (the contrast: difference, ratio or odds ratio) or \code{"level"} (the percentage or mean --
#'   for a regression table this needs \code{effect = "marginal"}).
#' @param observed For a regression table with \code{empirical = TRUE}: \code{"auto"}, \code{"band"}
#'   (the observed value with the margin of error of the gap), \code{"point"}, \code{"ci"} (the classic
#'   two-interval figure) or \code{"none"}.
#' @param center What marks the estimate: \code{"n"} (the default) a square whose area is the level's
#'   own base, with the value printed just above it; \code{"estimate"} the value alone; \code{"none"}
#'   a constant square and no value, for a plot with many panels.
#' @param display What that value prints -- a \code{\{\}} display template, as
#'   \code{\link{set_display}} takes (\code{"\{est\} (\{base\})"}, \code{"est_ci"}, ...).
#'   \code{NULL} (the default) prints the cell's own primary token.
#' @param offset How far below the estimate the observed value sits, as a fraction of a row. ggplot
#'   has no absolute-unit nudge, so what reads well depends on the viewport: raise it for a tall
#'   figure with few rows. Under an adjustment colour the arrow takes this row and the observed value
#'   drops one further.
#' @param label_offset How far above the estimate its value is printed, as a fraction of a row.
#' @param max_size Area of the largest marker, when \code{center = "n"} maps the base to it.
#' @param footer_width Characters per footer line. A ggplot caption does not wrap, and the plot
#'   cannot measure the device at build time, so a wide figure wants a larger number and a narrow one
#'   a smaller.
#' @param layout Which axis is read and which is faceted: \code{"keep"} (the default) reads the
#'   table's rows, \code{"transpose"} reads its columns, and \code{"auto"} picks whichever has more
#'   levels -- more levels down the side, fewer panels across, which is the more legible of the two
#'   whenever the table is much wider than it is tall. A regression table is never transposed.
#' @param facet \code{NULL} for one panel per estimate column, \code{FALSE} for a single panel.
#' @param guide \code{"gridlines"} (the default), \code{"bands"} (shade the panel between the colour
#'   breaks -- the teaching mode, which makes a cell's colour and its position one statement) or
#'   \code{"none"}.
#' @param intercept Draw the regression \code{Constant} row.
#' @param totals Draw total rows and total columns.
#' @param footer \code{"short"} (the default) the console's own footer, \code{"full"} the exports'
#'   longer one, or \code{"none"}. Both are wrapped and set flush left.
#' @param theme \code{"light"}, \code{"dark"} or one of the black-and-white publication palettes
#'   (\code{"print_ready"} and friends; a plotted mark then reads its
#'   magnitude off a grey ramp, since a point has no typography).
#'   \code{NULL} follows \code{getOption("tabxplor.export_theme")}.
#' @param caption A caption. \code{NULL} keeps the table's own.
#' @param legend Where the colour legend goes: \code{"auto"} (the bottom), \code{"right"},
#'   \code{"left"}, \code{"top"} or \code{"none"}. When several ladders apply it cannot be a guide and
#'   goes to the caption instead.
#' @param subtext Include the table's subtext and footer lines in the caption.
#' @param return_data Return the long estimate tibble instead of the plot.
#' @param ... Unused.
#'
#' @return A \code{ggplot} (or a list of them), ready for \code{+ theme()} and \code{ggsave()}.
#' @seealso \code{\link{reg_check_plots}} for the model checks, \code{\link{tab_export}} to export
#'   the table itself.
#' @export
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   t <- tab(forcats::gss_cat, race, marital, pct = "row", ci = "ref", color = TRUE)
#'   forest_plot(t)
#'   forest_plot(t, guide = "bands")          # the teaching mode
#' }
forest_plot <- function(x, columns = NULL, what = c("auto", "effect", "level"),
                        observed = c("auto", "band", "point", "ci", "none"),
                        center = c("n", "estimate", "none"), display = NULL,
                        layout = c("keep", "auto", "transpose"),
                        facet = NULL, color = TRUE, guide = c("gridlines", "bands", "none"),
                        intercept = FALSE, totals = FALSE, offset = 0.25, label_offset = 0.30,
                        max_size = 6, footer = c("short", "full", "none"), footer_width = 130L,
                        legend = "auto",
                        theme = NULL, lang = NULL, caption = NULL, subtext = TRUE,
                        return_data = FALSE, ...) {
  what   <- match.arg(what);   observed <- match.arg(observed)
  guide  <- match.arg(guide);  center   <- match.arg(center)
  layout <- match.arg(layout); footer   <- match.arg(footer)
  if (!is.data.frame(x) && is.list(x))
    return(purrr::map(x, forest_plot, columns = columns, what = what, observed = observed,
                      center = center, display = display, layout = layout, facet = facet,
                      color = color, guide = guide, intercept = intercept, totals = totals,
                      offset = offset, label_offset = label_offset, max_size = max_size,
                      footer = footer, footer_width = footer_width, legend = legend, theme = theme,
                      lang = lang, caption = caption, subtext = subtext,
                      return_data = return_data))
  tx_plot_deps("ggplot2")
  cols <- tx_plot_colors(theme)
  th   <- cols$theme
  lgd  <- fp_legend_position(legend)

  # DESIGN: a crosstab is plotted on the DEVIATION its `color =` grades -- the quantity the reference
  # line is the neutral of. The level it sits on is printed above the whisker instead, so the position
  # and the number say two different things.
  if (identical(what, "auto") && !tab_is_reg(x)) what <- "effect"
  e <- tab_estimates(x, columns = columns, what = what, observed = observed,
                     intercept = intercept, totals = totals, theme = th)
  if (identical(observed, "auto"))
    observed <- if (any(is.finite(e$gap_se))) "band"
                else if (any(is.finite(e$obs))) "point" else "none"
  if (!any(is.finite(e$ci_inf)) && !tab_is_reg(x))
    cli::cli_inform(c("i" = "No confidence interval in this table: the plot shows points only.",
                      "i" = "Build it with {.code ci = \"ref\"} to get whiskers."))

  e$trow <- e$row
  e <- fp_layout(e, x, layout)     # `return_data` comes AFTER: it must describe what is DRAWN
  if (return_data) return(e)
  sc <- fp_scale_records(e, x, what, lang, pad = if (identical(center, "none")) 0.04 else 0.10)
  fn <- fp_axis_fns(sc)
  keys <- unique(vapply(sc, function(z) z$key, character(1)))
  # the axis title speaks for every panel, so it may only exist where they all measure the same thing
  # AND call it the same thing: a multinomial risk ratio beside an ordinal win ratio shares the scale
  # but not the word, and one title would name the other panel's quantity.
  wrds <- with_legend_lang(lang, function(lg) vapply(sc, function(z) fp_unit_strip(x, z), character(1)))
  one_word <- length(unique(wrds)) == 1L && length(keys) == 1L

  # --- into the common space ------------------------------------------------------------------------
  trf <- function(v) {
    o <- rep(NA_real_, length(v))
    for (f in names(sc)) { i <- which(as.character(e$facet) == f); o[i] <- sc[[f]]$tr(v[i]) }
    o
  }
  for (nm in c("estimate", "ci_inf", "ci_sup", "obs", "gap_lo", "gap_hi"))
    e[[paste0("x_", nm)]] <- trf(e[[nm]])
  lo <- vapply(as.character(e$facet), function(f) sc[[f]]$lim[1], numeric(1))
  hi <- vapply(as.character(e$facet), function(f) sc[[f]]$lim[2], numeric(1))
  e$x_gap_lo <- pmax(e$x_gap_lo, lo); e$x_gap_hi <- pmin(e$x_gap_hi, hi)

  # --- the y axis: table order, read top to bottom, the reference row in bold -----------------------
  e$ypos <- factor(e$row, levels = rev(sort(unique(e$row))))
  e      <- fp_ypos_index(e)                    # the WITHIN-PANEL position, for anything continuous
  rows   <- unique(e[c("row", "level", "is_ref")])
  ylab   <- stats::setNames(as.character(rows$level), as.character(rows$row))
  yref   <- stats::setNames(rows$is_ref, as.character(rows$row))
  ylab_fn <- function(b) {
    lv <- unname(ylab[b]); lv[is.na(lv)] <- ""
    if (!any(yref[b], na.rm = TRUE)) return(lv)
    lapply(seq_along(b), function(k) if (isTRUE(yref[[b[k]]])) bquote(bold(.(lv[k]))) else lv[k])
  }
  e$psize <- fp_point_sizes(e, center, max_size)
  mods <- if (any(e$series == "modelled")) e[e$series == "modelled", , drop = FALSE] else e
  wsk  <- mods[is.finite(mods$x_ci_inf) & is.finite(mods$x_ci_sup) & !mods$is_ref, , drop = FALSE]

  pal_bg <- fmt_point_palette(th, "bg")
  pal_tx <- fmt_point_palette(th, "text")
  # the adjustment is the one second measure a forest plot CAN draw: it is the gap between the two
  # marks already on the row, and it has an interval. When a channel grades it, that gap gets its own
  # geometry and its own colour -- and if the MAIN colour is the gap, the model whisker goes neutral,
  # so colour still grades exactly one thing.
  adj_on   <- isTRUE(color) && any(is.finite(e$gap_slot))
  adj_main <- adj_on && any(vapply(unique(as.character(e$measure)), measure_own_ref, logical(1)))
  # neutral, and light: not `cols$point` (a muted BLUE, which would read as a rung of the very ladder
  # it stands outside of) and not `grey2` (the ink of a secondary token -- too dark to recede).
  # `grey` is the table's own "uncoloured cell", which is exactly what this whisker now is.
  ink      <- function(v) if (!isTRUE(color)) cols$point else if (adj_main) cols$grey else v
  gap_hex  <- function(d, pal) ifelse(is.finite(d$gap_slot) & d$gap_slot > 0L,
                                      pal[pmax(d$gap_slot, 1L)], cols$grey)
  # ggplot has no absolute-unit nudge (a position is data space; a millimetre needs the panel height,
  # known only at draw time), so every companion row sits a FRACTION of a row from the model's, and
  # the caller -- who knows the viewport -- can move them. `offset` measured: 0.15 is 1.3-2.4 mm
  # across the sizes such a table is drawn at, against the 1.8 mm two whisker linewidths come to.
  off      <- ggplot2::position_nudge(y = -offset)
  # the rules stop short of the panel edge, so the gap between two predictor blocks reads as a break
  # rather than as one continuous grid: they run from just under the bottom row to just over the top.
  # WARNING: a rule frame must carry the ROW-facet variables (`var` / `group`) as well as `facet`, or
  # facet_grid replicates every block's rules into every panel -- and a `yend` from a nine-level block
  # then stretches a three-level one to reach it.
  rule_y <- unique(e[c("var", "group", "facet", "n_blk")])
  rule   <- function(d) merge(d, rule_y, by = "facet", all.x = TRUE)

  # --- layers, back to front -----------------------------------------------------------------------
  # WARNING: this FIRST layer is what fixes the reading axis. A discrete scale's order is the order it
  # is trained in, and a later layer holding one row (a coloured row band, the limits frame) would
  # otherwise put its own level first and scramble every panel -- measured. Training on the whole
  # model, up front, is the only way the axis is the table's order whatever else is drawn.
  p <- ggplot2::ggplot(e) +
    ggplot2::geom_blank(ggplot2::aes(x = .data$x_estimate, y = .data$ypos))
  if (identical(guide, "bands") && isTRUE(color)) {
    bd <- do.call(rbind, lapply(sc, function(z) {
      if (length(z$at_all) < 2L) return(NULL)
      sl <- fp_break_slots(z$dir_all, z$mag_all)
      at <- c(-Inf, z$at_all, Inf); sl <- c(sl[1], sl, sl[length(sl)])
      k  <- seq_len(length(at) - 1L)
      ms <- ifelse(sl[k] == 0L, sl[k + 1L], sl[k])
      data.frame(facet = factor(z$facet, levels = levels(e$facet)),
                 xmin = at[k], xmax = at[k + 1L],
                 fill = ifelse(ms > 0L, pal_bg[pmax(ms, 1L)], NA_character_))
    }))
    bd <- if (is.null(bd)) NULL else bd[!is.na(bd$fill), , drop = FALSE]
    if (!is.null(bd) && nrow(bd))
      p <- p + ggplot2::geom_rect(data = bd, ggplot2::aes(xmin = .data$xmin, xmax = .data$xmax,
                                                          fill = .data$fill),
                                  ymin = -Inf, ymax = Inf, alpha = 0.55, inherit.aes = FALSE,
                                  show.legend = FALSE)
  }
  # DESIGN: a table's SECOND colour channel is not drawn. A forest plot has one position axis and one
  # ladder; a second measure has no interval, no neutral and nowhere positional to go, and rendering
  # it as a band behind every row floods the figure without adding a comparison. (The gap under
  # `color = "adjustment"` is not an exception to this: there the adjustment IS the main measure.)

  # the ladder: one dashed rule per rung, IN ITS OWN COLOUR. Drawn for every in-range rung, while the
  # axis labels are thinned -- so the scale stays complete even where its text cannot fit.
  rl <- if (guide != "none" && isTRUE(color)) do.call(rbind, lapply(sc, function(z) {
    sl <- fp_break_slots(z$dir_all, z$mag_all); i <- which(sl > 0L)
    if (!length(i)) return(NULL)
    data.frame(facet = factor(z$facet, levels = levels(e$facet)), at = z$at_all[i], col = pal_tx[sl[i]])
  }))
  if (!is.null(rl) && nrow(rl)) {
    rl <- rule(rl)
    p <- p + ggplot2::geom_segment(data = rl,
      ggplot2::aes(x = .data$at, xend = .data$at, y = 0.62, yend = .data$n_blk + 0.38,
                   colour = .data$col),
      linetype = "dashed", linewidth = 0.4, alpha = 0.5, show.legend = FALSE)
  }
  nl <- do.call(rbind, lapply(sc, function(z) if (!is.finite(z$neutral)) NULL else
    data.frame(facet = factor(z$facet, levels = levels(e$facet)), at = z$tr(z$neutral))))
  if (!is.null(nl) && nrow(nl)) {
    nl <- rule(nl)
    p <- p + ggplot2::geom_segment(data = nl,
      ggplot2::aes(x = .data$at, xend = .data$at, y = 0.62, yend = .data$n_blk + 0.38),
      linetype = "longdash", colour = cols$text, linewidth = 0.45)
  }
  rf <- do.call(rbind, lapply(sc, function(z) if (!is.finite(z$ref)) NULL else
    data.frame(facet = factor(z$facet, levels = levels(e$facet)), at = z$ref)))
  if (!is.null(rf) && nrow(rf)) {
    rf <- rule(rf)
    p <- p + ggplot2::geom_segment(data = rf,
      ggplot2::aes(x = .data$at, xend = .data$at, y = 0.62, yend = .data$n_blk + 0.38),
      linetype = "dotted", colour = cols$text, linewidth = 0.45)
  }
  # one rule per block, above its rows: what separates two predictors from each other
  p <- p + ggplot2::geom_hline(data = rule_y, ggplot2::aes(yintercept = .data$n_blk + 0.5),
                               colour = cols$grey, linewidth = 0.3)

  # the observed value and the gap, one row BELOW the model's: a thin black capped whisker (the gap's
  # own interval -- the estimate falls outside it exactly when the gap test rejects) and a filled
  # black point at the crude value. Never a wide band: it was the biggest ink on the page and it
  # located nothing.
  # what adjustment DID: an arrow from the observed value to the model's, in the gap's own colour.
  # Its length is the adjustment, to scale -- a small one is meant to look small; the colour is what
  # says whether the move is a real one. It shares the companion row with the bracket and the observed
  # point, which are drawn OVER it: both are thin or small enough not to cover the colour.
  if (adj_on && observed %in% c("band", "point")) {
    ar <- mods[is.finite(mods$x_obs) & is.finite(mods$x_estimate) & !mods$is_ref &
                 mods$x_obs != mods$x_estimate, , drop = FALSE]
    if (nrow(ar)) p <- p + ggplot2::geom_segment(data = ar,
      ggplot2::aes(x = .data$x_obs, xend = .data$x_estimate, y = .data$ypos, yend = .data$ypos),
      colour = gap_hex(ar, pal_tx), linewidth = 1.3, position = off, show.legend = FALSE,
      arrow = grid::arrow(type = "closed", angle = 22, length = grid::unit(0.042, "inches")))
  }
  if (identical(observed, "band")) {
    # the ACCEPTANCE REGION, around the observed value: the model estimate falls outside it exactly
    # when the gap test rejects. Drawn only where the gap IS testable -- on a non-collapsible measure
    # the movement is real arithmetic but no test exists, and a bracket would claim one.
    bd <- mods[is.finite(mods$x_gap_lo) & is.finite(mods$x_gap_hi) & mods$gap_tested, , drop = FALSE]
    if (nrow(bd)) {
      span <- vapply(as.character(bd$facet), function(f) diff(sc[[f]]$lim), numeric(1))
      bd   <- bd[(bd$x_gap_hi - bd$x_gap_lo) < 0.97 * span, , drop = FALSE]
    }
    if (nrow(bd)) p <- p + ggplot2::geom_segment(data = bd,
      ggplot2::aes(x = .data$x_gap_lo, xend = .data$x_gap_hi, y = .data$ypos, yend = .data$ypos),
      colour = cols$text, linewidth = 0.25, position = off,
      arrow = grid::arrow(angle = 90, ends = "both", length = grid::unit(0.02, "inches")),
      show.legend = FALSE)
  }
  if (observed %in% c("band", "point")) {
    ob <- mods[is.finite(mods$x_obs) & !mods$is_ref, , drop = FALSE]
    if (nrow(ob)) p <- p + ggplot2::geom_point(data = ob,
      ggplot2::aes(x = .data$x_obs, y = .data$ypos), shape = 21, fill = cols$text,
      colour = cols$text, size = 1.6, stroke = 0.5, show.legend = FALSE, position = off)
  }
  # a crude SERIES carries its own interval: the same thin black whisker, at the same offset
  oci <- e[e$series == "observed" & is.finite(e$x_ci_inf) & is.finite(e$x_ci_sup), , drop = FALSE]
  if (nrow(oci)) p <- p +
    ggplot2::geom_segment(data = oci,
      ggplot2::aes(x = .data$x_ci_inf, xend = .data$x_ci_sup, y = .data$ypos, yend = .data$ypos),
      colour = cols$text, linewidth = 0.35, position = off,
      arrow = grid::arrow(angle = 90, ends = "both", length = grid::unit(0.02, "inches")),
      show.legend = FALSE) +
    ggplot2::geom_point(data = oci, ggplot2::aes(x = .data$x_estimate, y = .data$ypos),
      colour = cols$text, fill = cols$text, shape = 21, size = 1.6, stroke = 0.5,
      show.legend = FALSE, position = off)

  # the whisker: ONE segment, capped, wholly in the cell's colour -- significance is read off it,
  # which is why there are no stars. No layer draws a key: the guide has its own frame below, so a
  # rung nothing happens to land on still gets its glyph.
  if (nrow(wsk)) p <- p + ggplot2::geom_segment(data = wsk,
    ggplot2::aes(x = .data$x_ci_inf, xend = .data$x_ci_sup, y = .data$ypos, yend = .data$ypos,
                 colour = ink(.data$point_hex)), linewidth = 0.9, show.legend = FALSE,
    arrow = grid::arrow(angle = 90, ends = "both", length = grid::unit(0.045, "inches")))
  # under `guaranteed_effect` the SCORE is the bound nearest the neutral, so that cap is the quantity
  # the colour grades: it is drawn twice the size, and the policy becomes visible rather than explained
  if (nrow(wsk) && any(wsk$policy %in% "guaranteed_effect")) {
    gw <- wsk[wsk$policy %in% "guaranteed_effect", , drop = FALSE]
    ntr <- vapply(as.character(gw$facet), function(f) sc[[f]]$tr(sc[[f]]$neutral), numeric(1))
    gw$x_near <- ifelse(is.finite(ntr) & abs(gw$x_ci_inf - ntr) > abs(gw$x_ci_sup - ntr),
                        gw$x_ci_sup, gw$x_ci_inf)
    p <- p + ggplot2::geom_point(data = gw,
      ggplot2::aes(x = .data$x_near, y = .data$ypos, colour = ink(.data$point_hex)),
      shape = 124, size = 3.6, stroke = 1, show.legend = FALSE)
  }
  if (!identical(center, "estimate")) {
    # under an adjustment colour the whisker recedes to grey and the SQUARE carries the measure, in
    # the arrow's own colour: the mark and the movement it made are then one statement.
    fill_hex <- if (adj_main) gap_hex(mods, pal_tx) else ink(mods$point_hex)
    mods$fill_hex <- ifelse(mods$is_ref, cols$text, fill_hex)
    p <- p + ggplot2::geom_point(data = mods,
      ggplot2::aes(x = .data$x_estimate, y = .data$ypos, size = .data$psize, fill = .data$fill_hex),
      shape = 22, colour = cols$text, stroke = 0.35, show.legend = FALSE)
  }
  if (!identical(center, "none")) {
    # never on a reference row: its cell reads "1" or "0", which the neutral line already says, and
    # the two glyphs land on top of each other
    lb <- mods[!mods$is_ref, , drop = FALSE]
    lb$lab <- fp_primary_text(x, lb, display)
    lb <- lb[!is.na(lb$lab), , drop = FALSE]
    if (nrow(lb)) p <- p + ggplot2::geom_label(data = lb,
      ggplot2::aes(x = .data$x_estimate, y = .data$ypos, label = .data$lab), size = 2.6,
      colour = cols$text, fill = paste0(substr(cols$bg, 1, 7), "DD"), nudge_y = label_offset,
      linewidth = 0, label.r = grid::unit(0.12, "lines"), show.legend = FALSE,
      label.padding = grid::unit(0.10, "lines"))
  }

  # WARNING: this frame FORCES each panel's limits, which is what makes the break/label lookup exact.
  # It is added LAST, never first: the first layer to touch a discrete scale fixes its level order,
  # and this frame carries one level.
  blank <- do.call(rbind, lapply(sc, function(z)
    data.frame(facet = factor(z$facet, levels = levels(e$facet)), x = z$lim,
               ypos = e$ypos[1], var = e$var[1], group = e$group[1])))
  p <- p + ggplot2::geom_blank(data = blank, ggplot2::aes(x = .data$x, y = .data$ypos))

  # --- scales ---------------------------------------------------------------------------------------
  # a secondary axis is per SCALE, not per panel: it survives only where the whole plot is on one.
  sec <- ggplot2::waiver()
  if (length(keys) == 1L) {
    scl <- sc[[1]]$scl
    if (identical(scl$sec, "exp"))
      sec <- ggplot2::sec_axis(transform = exp, name = gettext("Odds ratio"))
    else if (identical(scl$sec, "sd") && is.finite(scl$sd_y)) {
      sd_y <- scl$sd_y
      sec  <- ggplot2::sec_axis(transform = ~ . / sd_y, name = gettext("SD of the outcome"))
    }
  }
  p <- p +
    ggplot2::scale_x_continuous(breaks = fn$breaks, labels = fn$labels, sec.axis = sec,
                                expand = ggplot2::expansion(0),
                                guide = ggplot2::guide_axis(check.overlap = TRUE)) +
    ggplot2::scale_y_discrete(labels = ylab_fn, expand = ggplot2::expansion(add = 0.55)) +
    ggplot2::scale_size_identity(guide = "none") +
    ggplot2::scale_fill_identity(guide = "none")

  # two ladders are drawn when the arrows grade a different measure from the whiskers: no guide then,
  # and the caption carries both in prose -- one rule, never a ladder without a key.
  gtxt <- if (!isTRUE(color) || identical(lgd, "none")) NULL
          else legend_guide_spec(x, unique(as.character(e$column)),
                                 if (adj_on && !adj_main) "bg" else "text", th, lang)
  if (!is.null(gtxt)) {
    # DESIGN: the keys come from a frame of their own, drawn at alpha 0 over the neutral. Keyed off
    # the DATA, a rung that no cell happens to fall in gets a label with no glyph -- measured on both
    # ends of a `guaranteed_effect` ladder.
    kf <- data.frame(hex = c(gtxt$keys$hex, gtxt$grey_hex),
                     facet = factor(levels(droplevels(e$facet))[1], levels = levels(e$facet)),
                     var = e$var[1], group = e$group[1], ypos = e$ypos[1],
                     at = sc[[1]]$tr(sc[[1]]$neutral))
    p <- p + ggplot2::geom_segment(data = kf,
      ggplot2::aes(x = .data$at, xend = .data$at, y = .data$ypos, yend = .data$ypos,
                   colour = .data$hex), alpha = 0, linewidth = 0.9, show.legend = TRUE)
  }
  if (!is.null(gtxt)) {
    # the keys run left to right as the axis does -- the deepest UNDER slot first, the grey where the
    # ladder is silent, then the OVER side deepening. One row, or ggplot fills by column and the
    # ladder reads in an order nothing on the page has.
    k <- gtxt$keys
    o <- c(rev(which(k$slot > 4L)), which(k$slot <= 4L))
    o <- o[order(match(k$slot[o], c(8:5, 1:4)))]
    p <- p +
      ggplot2::scale_colour_identity(
        name = gtxt$title, guide = "legend",
        breaks = c(k$hex[o][k$slot[o] > 4L], gtxt$grey_hex, k$hex[o][k$slot[o] <= 4L]),
        labels = c(k$label[o][k$slot[o] > 4L], gtxt$grey_label, k$label[o][k$slot[o] <= 4L])) +
      ggplot2::guides(colour = ggplot2::guide_legend(
        nrow = 1L, override.aes = list(alpha = 1, linewidth = 0.9)))
  } else p <- p + ggplot2::scale_colour_identity(guide = "none")

  # --- facets ---------------------------------------------------------------------------------------
  nvar <- nlevels(droplevels(e$var)); nfac <- nlevels(droplevels(e$facet))
  ngrp <- length(unique(e$group[nzchar(e$group)]))
  if (identical(facet, FALSE) && length(keys) > 1L) {
    cli::cli_inform(c("!" = "{length(keys)} different units in this table: it cannot share one panel."))
    facet <- NULL
  }
  if (!identical(facet, FALSE) && (nvar > 1L || nfac > 1L || ngrp > 1L)) {
    if (max(nvar, 1L) * max(nfac, 1L) * max(ngrp, 1L) > 12L)
      cli::cli_inform(c("!" = "{max(nvar, 1L) * max(nfac, 1L) * max(ngrp, 1L)} panels.",
                        "i" = "Use {.arg columns} to keep fewer estimate columns."))
    rw <- if (ngrp > 1L && nvar > 1L) ggplot2::vars(.data$group, .data$var)
          else if (ngrp > 1L) ggplot2::vars(.data$group)
          else if (nvar > 1L) ggplot2::vars(.data$var) else NULL
    # the unit cannot be an axis title when the panels do not share one: it moves into the strip,
    # through a labeller -- never by relabelling the data, which is every layer's facet key.
    lbl <- ggplot2::label_value
    if (!one_word)
      lbl <- ggplot2::labeller(facet = function(v)
        ifelse(v %in% names(wrds), paste0(v, " - ", wrds[v]), v))
    p <- p + ggplot2::facet_grid(rows = rw, cols = if (nfac > 1L) ggplot2::vars(.data$facet) else NULL,
                                 scales = "free", space = "free_y", switch = "y", labeller = lbl)
  }

  # --- labels ---------------------------------------------------------------------------------------
  # the method belongs to the axis when there IS one axis, and to the footer otherwise: a
  # many-outcome regression must not carry a statistical clause it can only state per panel.
  xt <- if (!one_word) NULL else with_legend_lang(lang, function(lg) {
    ti <- fp_axis_title(x, sc[[1]])
    # the interval's NAME only: its confidence level is already the title's own "(95% CI)", and a
    # column that colours without an interval has no name to add -- only the level, which would then
    # be printed twice. Under an adjustment colour the spec names the GAP's test, which is not the
    # interval this axis draws: it stays in the footer, where it describes the colour.
    me <- if (adj_main) character(0) else
      sub("[,;] [0-9.]+% .*$", "", fp_method_line(x, unique(as.character(e$column)), lang))
    if (!length(me) || !nzchar(me) || grepl("^[0-9.]+\\s*%", me) || !nzchar(ti)) ti
    else sub("\\)$", gettextf(", %s)", me), ti)
  })
  cap <- fp_caption(x, unique(as.character(e$column)), caption, subtext, footer,
                    isTRUE(color) && is.null(gtxt), th, lang, width = footer_width,
                    drop_method = one_word && !adj_main)
  p + fp_plot_theme(cols) +
    ggplot2::labs(x = xt, y = NULL, title = cap$title, caption = cap$caption) +
    ggplot2::theme(legend.position = lgd)
}

# `legend =` is a POSITION; TRUE / FALSE are still accepted. "auto" is the bottom, where a ladder of
# six keys costs one line instead of a column of width -- and where reg_check_plots() puts its own.
#' @keywords internal
fp_legend_position <- function(legend) {
  if (isTRUE(legend))  return("bottom")
  if (isFALSE(legend)) return("none")
  legend <- match.arg(as.character(legend),
                      c("auto", "bottom", "right", "left", "top", "none"))
  if (identical(legend, "auto")) "bottom" else legend
}

# The area of a marker is the level's own base, so a small base and a wide interval say the same thing
# twice -- which is what makes a fragile estimate impossible to overlook.
#' @keywords internal
fp_point_sizes <- function(e, center, max_size = 6) {
  if (!identical(center, "n") || !any(is.finite(e$n))) return(rep(min(2.2, max_size), nrow(e)))
  r <- range(sqrt(e$n), na.rm = TRUE, finite = TRUE)
  s <- if (diff(r) > 0) 1.5 + (max_size - 1.5) * (sqrt(e$n) - r[1]) / diff(r) else max_size * 0.6
  s[!is.finite(s)] <- 1.5
  s
}

# The forest plot's own skin: theme_minimal, the table's ink for every word, dashed rules, bold strips.
# The assumption panels keep tx_plot_theme()'s theme_bw -- a chart of the RESULTS and a chart of the
# DIAGNOSTICS are read differently, and only this one has to sit beside the table it comes from.
#' @keywords internal
fp_plot_theme <- function(cols) {
  ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold", size = 11, colour = cols$text),
      text          = ggplot2::element_text(colour = cols$text),
      axis.text     = ggplot2::element_text(colour = cols$text),
      axis.text.y   = ggplot2::element_text(colour = cols$text, hjust = 0),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank(),
      panel.spacing.x    = grid::unit(0.9, "lines"),
      panel.spacing.y    = grid::unit(0.35, "lines"),
      plot.background   = ggplot2::element_rect(fill = cols$bg, colour = NA),
      panel.background  = ggplot2::element_rect(fill = cols$bg, colour = NA),
      legend.background = ggplot2::element_rect(fill = cols$bg, colour = NA),
      legend.key        = ggplot2::element_rect(fill = cols$bg, colour = NA),
      strip.background  = ggplot2::element_blank(),
      strip.text        = ggplot2::element_text(colour = cols$text, size = 9, face = "bold"),
      strip.text.y.left = ggplot2::element_text(angle = 90),
      legend.text       = ggplot2::element_text(size = 7.5),
      legend.title      = ggplot2::element_text(size = 8.5),
      legend.key.width  = grid::unit(0.9, "lines"),
      legend.key.height = grid::unit(0.7, "lines"),
      strip.placement   = "outside",
      plot.caption          = ggplot2::element_text(hjust = 0, size = 7.5, colour = cols$subtle),
      plot.caption.position = "plot",
      plot.title.position   = "plot")
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
fp_caption <- function(x, cols, caption, subtext, footer, want_legend, theme, lang,
                       width = 130L, drop_method = FALSE) {
  # reg_title() is NA on a cross-table (it names a MODEL), and NA is not NULL: without the is.na()
  # guard a crosstab's figure is headed "NA".
  ttl <- caption %||% get_caption(x) %||%
    with_legend_lang(lang, function(lg) reg_title(reg_call(x)))
  ttl <- if (is.null(ttl) || is.na(ttl) || !nzchar(ttl)) NULL else ttl
  if (identical(footer, "none")) return(list(title = ttl, caption = NULL))
  sub  <- if (isTRUE(subtext)) purrr::discard(get_subtext(x), \(s) s == "") else character(0)
  st   <- suppressWarnings(tab_footer_streams(
    x, style = if (identical(footer, "short")) "terse" else "prose", lang = lang,
    subtext = sub, legend = want_legend, theme = theme))
  foot <- render_footer(st, medium = "plain", theme = theme)
  meth <- if (want_legend || drop_method) character(0) else fp_method_line(x, cols, lang)
  out  <- c(meth, foot)
  list(title = ttl,
       caption = if (!length(out)) NULL
                 else paste(vapply(out, rd_wrap, character(1), width = width), collapse = "\n"))
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
