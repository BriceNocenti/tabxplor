# PURPOSE: The crosstab API and its build pipeline -- tab() and the staged tab_build() it drives.
# ROLE: tab() is a thin, user-facing wrapper: it defuses the NSE variable arguments, routes the
#   retired 1.x spellings, and hands a typed context to tab_build(). The five build stages, the
#   settings spine, the spread / transpose reshapes and the variable-model readers live here.
# KEY CONSTRAINTS:
#   - FIVE STAGES over one typed ctx: tab_setup (arguments -> the settings spine) -> tab_prepare_pop
#     (prepare the population ONCE: filter, NA, lump, relabel, weights) -> tab_aggregate (the tier-1
#     count and moment-sum aggregates) -> tab_build_tables (per row_var: tab_transform ->
#     tab_assemble_tables) -> tab_assemble_output (the cross-row_var output shape).
#   - ONE CARRIER for the settings: `ctx$settings`, the rows / cols / pairs star schema tab_setup()
#     builds and tab_prepare_pop() completes. Each stage projects it into the bare names it reads
#     with ctx_settings_locals(); nothing writes those names back into the ctx, and the raw inputs
#     the spine owns leave it once tab_setup() has consumed them. That is what keeps two parallel
#     argument vectors from recycling against each other.
#   - The population is prepared ONCE for the whole database. Do only per-table work per row_var.
#   - THE AGGREGATE CORE IS THE LEAF (R/tab-leaf.R): it computes the cells, their interval and the
#     whole-table test in one pass. Nothing here re-reads a built table to add them.
#   - Levels drop AFTER the tests: non-first levels are removed only in assembly, so chi2 and the
#     intervals see the full level set. The ordering is structural, not incidental.
#   - The row_var axis is globalised on tab() (pct / color / comp / ci / chi2 / ref2 are scalar);
#     `ref` is a named, ordered per-row_var vector; the col_var axis stays flexible.
#   - All public signatures are CRAN API -- soft-deprecate before changing one.
#   - tab.R sorts AFTER every tab-*.R in R's C collation, so those files may read tab.R's top-level
#     objects, never the reverse.
# See: CLAUDE.md § tabxplor architecture (the calculation pipeline).

#' Internal data.table methods
#' @import data.table
#' @keywords internal
#' @name tabxplor-data.table
NULL

# MAIN USER-FRIENDLY FUNCTIONS ###########################################################


# WARNING: tab()'s @param blocks are GENERATED. Every argument -- its producers, its legal values,
# its option twin and its prose -- is ONE row of TAB_ARGS (R/tab-args.R). Edit the row, not this
# file: tab_args_rd() orders by formals(), and a load-time check refuses a formal with no row.
#' Cross-table with color helpers
#'
#' @description
#' `tab()` builds a cross-table of one or several row variables by one or several column
#' variables, and colors the cells so the table is easy to read at a glance --- in the R
#' console, or exported to Excel, HTML or Word. Cells can show counts, row or column
#' percentages, or (for a numeric column variable) means, optionally with differences,
#' confidence intervals and statistical tests.
#'
#' The result is a `tibble` (of class `tabxplor_tab`), so you can keep working on it with the
#' usual \pkg{dplyr} verbs ([dplyr::select()], [dplyr::filter()], [dplyr::arrange()],
#' [dplyr::mutate()]).
#'
#' New to the package? Start with `vignette("tabxplor")` and with just four arguments ---
#' `data`, `row_vars`, `col_vars` and `pct` --- then add `color` when you want reading helpers.
#'
#' @details
#' `tab()` has many arguments, but you only need a handful to begin. They fall into groups:
#' \itemize{
#'   \item **The table**: `data`, `row_vars`, `col_vars`, `tab_vars` (one sub-table per group),
#'     `wt` (a weight variable).
#'   \item **What each cell shows**: `pct` (row or column percentages, or leave counts), `digits`.
#'   \item **Colors (reading helpers)**: `color`, and `color_signif` (whether statistical
#'     significance gates the color). Thresholds and palettes are set once for the whole session
#'     with [set_color_breaks()] and [set_color_palette()]; a color legend prints automatically.
#'   \item **Comparisons**: `ref` / `ref2` / `comp` (which cell is the baseline for differences),
#'     and `display` when you want odds ratios shown.
#'   \item **Statistics**: `test` (chi-squared or Welch's F), and `ci` + `conf_level` + `stars`
#'     (confidence intervals). `ci_method` picks the engine for each kind of interval.
#'   \item **Totals & missing values**: `tot`, `total_names`, `totaltab`, `na`, `levels`.
#'   \item **Advanced / output**: `display`, `n_min`, `output_list`, `parallel`, `spread_vars`,
#'     `filter`.
#' }
#' The package-wide display, color and statistics defaults are `options()`, listed at
#' [tabxplor-options].
#'
#' @eval tab_args_rd("tab")
#' @param ... The arguments retired in 2.0.0, caught by name: the nine deprecated formals
#'   (`sup_cols`, `OR`, `chi2`, `method_cell`, `method_diff`, `names_prefix`, `names_sort`,
#'   `row_var`, `col_var`), the three total-label ones now carried by
#'   `options(tabxplor.total_names)` (`total_names`, `totaltab_name`, `other_level`), and the five
#'   jamovi-internal ones (`.cache`, `.defer_level_merge`, `.return_armed`, `.levels_order`,
#'   `.levels_collapse`).
#'   Everything else is refused with a suggestion, and an UNNAMED argument here is refused outright
#'   -- past the variable roles, every argument must be named.
#'
#' @details
#' \strong{Ordered factors.} An \code{ordered} factor stays ordered through the whole pipeline,
#' which is what lets \code{ref2 = "cumulative"} pick its column variables by class. One
#' consequence is worth knowing: the synthetic \code{"Total"} / \code{"Ensemble"} / \code{"NA"}
#' levels are appended \emph{after} the real ones, so on an ordered grouping column they compare as
#' the greatest levels. They are labels, not points on the scale.
#'
#' \strong{Weighted data.} With a weight (\code{wt}), the default confidence interval treats the
#' weighted percentage as if it came from the unweighted number of cases. That carries no design
#' effect, so under unequal weights it is \strong{usually too narrow} --- and the table's footer
#' says so. \code{design_effect = TRUE} (or \code{options(tabxplor.design_effect = TRUE)} for a
#' whole session) corrects it exactly, in every descriptive interval and every colour threshold.
#' Because it is the exact variance and not an upper bound, it can also make an interval
#' \emph{narrower}: that is correct, not a bug.
#'
#' \strong{Survey designs.} Pass a \code{survey::svydesign} as \code{data} and strata, clusters,
#' \code{fpc} \emph{and} calibration reach every interval, star and colour threshold, each referred
#' to the design's own degrees of freedom. It is exact for a cell and mildly conservative for a
#' cell-versus-reference difference, so it never produces a star the design does not support, and
#' sometimes withholds one it would. A design-based table costs roughly three times a weighted one.
#' Pre-aggregated counts (\code{\link{tab_counts}}) cannot carry either correction, and their
#' footer says so rather than claiming one.
#'
#' \code{vignette("tabxplor")} works through when each of these matters.
#'
#' @section Significance stars:
#' With \code{stars = TRUE} and an interval anchored on the comparison (see \code{ci}), each cell
#' shows how sure we can be that its difference from the reference is real and not just sampling
#' noise: \code{*} means significant at the 10\% level (p < 0.10), \code{**} at 5\% (p < 0.05),
#' \code{***} at 1\% (p < 0.01). The exact p-value is stored per cell in the \code{pvalue} field of
#' the \code{fmt} vectors, readable with \code{$pvalue} or \code{get_pvalue()}.
#'
#' There is no separate statistical test run behind the scenes: the significance is read straight
#' from the confidence interval that is displayed. A cell is significant at a given level exactly
#' when its interval at that confidence level no longer contains zero, so the stars and the printed
#' \code{[inf; sup]} bracket can never contradict each other. Which test this amounts to depends on
#' the interval:
#' \itemize{
#'   \item \strong{percentage difference} (the default, \code{ci_method = c(diff = "newcombe")}):
#'     inverting the Newcombe hybrid-score interval. This is, to a very close approximation, the
#'     classical two-sample test of proportions (the score / "N-1" chi-squared test).
#'   \item \strong{percentage difference} with \code{ci_method = c(diff = "ac")} or
#'     \code{c(diff = "wald")}: inverting the Agresti-Caffo (adjusted Wald) or the Wald interval ---
#'     an (adjusted) two-proportion z-test.
#'   \item \strong{mean difference}: the \strong{Welch two-sample t-test} (for groups with unequal
#'     variances); inverting the Welch t interval is exactly this well-known test.
#'   \item \code{ci = "cell"} (an absolute cell interval, not a difference) is purely descriptive,
#'     so it carries no stars and its \code{pvalue} is \code{NA}.
#' }
#' On weighted data the estimate is weighted but the sample size used is the real (unweighted)
#' number of cases, unless you opt in to the weighting's own design effect with
#' \code{options("tabxplor.design_effect" = TRUE)}.
#'
#' @eval display_tokens_rd(user_only = TRUE)
#'
#' @return A \code{tibble} of class \code{tab}, possibly with colored reading helpers.
#' All non-text columns are of class \code{\link{fmt}}, storing all
#' the data necessary to print formats and colors. Columns with \code{row_var} and
#' \code{tab_vars} are of class \code{factor} : every added \code{factor} will be
#' considered as a \code{tab_vars} and used for grouping. To add text columns without
#' using them in calculations, be sure they are of class \code{character}.
#' @export
#'
#' @examples # A simple cross-table:
#' tab(forcats::gss_cat, marital, race)
#'
#'
#' # With more variables provided, `tab` makes a subtables for each combination of levels:
#' \donttest{
#' tab(forcats::gss_cat, marital, tab_vars = c(year, race))
#'}
#'
#' # You can add several col_vars, mixing factors and numeric (means) ; `levels = "first"`
#' # keeps only the first level of each factor col_var for compact summary tables:
#' \donttest{
#' tab(dplyr::storms, category, c(status, pressure, wind))
#'}
#'
#' # Colors to help the user read the table:
#' data <- forcats::gss_cat |>
#'   dplyr::filter(year %in% c(2000, 2006, 2012), !marital %in% c("No answer", "Widowed"))
#' gss  <- "Source: General social survey 2000-2014"
#' gss2 <- "Source: General social survey 2000, 2006 and 2012"
#'
#' # Differences between the cell and it's subtable's total cell:
#' \donttest{
#' tab(data, race, marital, year, subtext = gss2, pct = "row", color = "difference")
#' }
#'
#' # Differences between the cell and the whole table's general total cell:
#' \donttest{
#' tab(data, race, marital, year, subtext = gss2, pct = "row", color = "difference",
#'   comp = "all")
#' }
#'
#' # Historical differences:
#' \donttest{
#' data2 <- data |> dplyr::mutate(year = as.factor(year))
#' tab(data2, year, marital, race, subtext = gss2, pct = "row",
#'     color = "difference", ref = "first", tot = "col")
#'
#'
#' # Differences with the total, except if their confidences intervals are superior to them:
#' tab(forcats::gss_cat, race, marital, subtext = gss, pct = "row",
#'     color = "difference", color_signif = "grey_non_signif")
#'
#' # Same differences, minus their confidence intervals:
#' tab(forcats::gss_cat, race, marital, subtext = gss, pct = "row",
#'     color = "difference", color_signif = "guaranteed_effect")
#'
#' # Contribution of cells to table's variance, like in a correspondence analysis:
#' tab(forcats::gss_cat, race, marital, subtext = gss, color = "contrib")
#'}
#'
#' # Since the result is a tibble, you can use all dplyr verbs to modify it :
#' \donttest{
#' library(dplyr)
#' tab(dplyr::storms, category, c(status, pressure, wind)) |>
#'   dplyr::filter(category != "-1") |>
#'   dplyr::select(-`tropical depression`) |>
#'   dplyr::arrange(is_totrow(pick(everything())), desc(category))
#'}
#'
#'\donttest{
#' # With `dplyr::arrange`, don't forget to keep the order of tab variables and total rows:
#' tab(data, race, marital, year, pct = "row") |>
#'   dplyr::arrange(year, is_totrow(dplyr::pick(dplyr::everything())), desc(Married))
#'   }
#'
#' @seealso
#'   [tab_reg()] (regression tables), and the variants [tab_num()] (numeric variables),
#'   [tab_counts()] (pre-aggregated counts) and [tab_plain()] (one bare cross-table).
#'   [set_color_breaks()] / [set_color_palette()] customise the colours,
#'   [tab_shape()] reports what a finished table is and what accepts it.
#'   Export a table with [tab_xl()] (Excel), [tab_kable()] (HTML), [tab_md()] (Markdown) or
#'   [tab_plot()], and CHART it with [forest_plot()] (every cell's estimate, interval and colour --
#'   `tab_plot()` renders the table as an image, `forest_plot()` is the real chart).
#'   Package-wide defaults live in [tabxplor-options].
#'
#'   `color = "contrib"` shows each cell's departure from the **log-linear model of independence**
#'   (that is what the chi-squared is), so it reads as a heatmap of the association pattern. For the
#'   specialist contingency-table models built on top of it --- quasi-independence, Goodman's RC
#'   association models, UNIDIFF --- see the \pkg{logmult} package
#'   (\url{https://cran.r-project.org/package=logmult}), which also supports complex survey designs.
tab <- function(data, row_vars, col_vars, tab_vars, wt, ...,
                pct = "no", color = "no", color_signif = "ignore", test = FALSE,
                na = "keep", levels = "all",
                cleannames = NULL, other_if_less_than = 0,
                ref = "auto", ref2 = "first", comp = "tab",
                ci = "auto", conf_level = NULL, stars = NULL,
                ci_method = NULL, anova = NULL, design_effect = NULL,
                totaltab = "line", common_totrow = FALSE, tot = c("row", "col"),
                add_n = TRUE, add_pct = FALSE,
                subtext = "", digits = 0, n_min = 0, display = NULL,
                color_breaks = NULL,
                output_list = FALSE, parallel = NULL,
                spread_vars, filter) {

  .dots <- rlang::enquos(..., .ignore_empty = "all")
  tab_check_dots(.dots, "tab")
  OR          <- dots_value(.dots, "OR", "no")
  chi2        <- dots_value(.dots, "chi2", lifecycle::deprecated())
  method_cell <- dots_value(.dots, "method_cell")
  method_diff <- dots_value(.dots, "method_diff")
  names_prefix <- dots_value(.dots, "names_prefix")
  names_sort   <- dots_value(.dots, "names_sort", FALSE)
  .cache             <- dots_value(.dots, ".cache")
  .defer_level_merge <- dots_value(.dots, ".defer_level_merge", FALSE)
  .return_armed      <- dots_value(.dots, ".return_armed", FALSE)
  .levels_order      <- dots_value(.dots, ".levels_order")
  .levels_collapse   <- dots_value(.dots, ".levels_collapse")

  # WARNING: the argument boundary tab_resolve_common_args() (R/tab-resolve.R) must run BEFORE the
  #   tidy-select block below: it routes `chi2` -> `test` and the `OR` spelling the build call reads.
  .a <- tab_resolve_common_args(
    "tab", test = test, chi2 = chi2, color = color, color_signif = color_signif,
    ci = ci, stars = stars, conf_level = conf_level,
    ci_method = ci_method, method_cell = method_cell, method_diff = method_diff,
    cleannames = cleannames, OR = OR, display = display, ref = ref, ref2 = ref2,
    tot = tot, na = na, levels = levels, pct = pct,
    total_names   = dots_value(.dots, "total_names"),
    totaltab_name = dots_value(.dots, "totaltab_name"),
    other_level   = dots_value(.dots, "other_level"),
    comp = comp, totaltab = totaltab, n_min = n_min, anova = anova,
    user_env = rlang::caller_env())
  test <- .a$test ; cleannames <- .a$cleannames ; stars <- .a$stars ; ci_method <- .a$ci_method
  display <- .a$display ; ref <- .a$ref ; ref2 <- .a$ref2
  color_spec <- .a$color_spec ; color <- .a$color
  total_names <- .a$total_names ; tot <- .a$tot
  totaltab_name <- .a$totaltab_name ; other_level <- .a$other_level
  conf_level <- .a$conf_level

  # WARNING: THE PARTIAL-MATCHING TRAP. R matches a partial argument name against the formals sitting
  #   BEFORE `...`, so `row_var` -- a PREFIX of `row_vars` -- binds there and never reaches `.dots`.
  .said <- names(sys.call())
  if ("row_var" %in% .said)
    lifecycle::deprecate_soft("2.0.0", "tab(row_var = )", "tab(row_vars = )",
                              user_env = rlang::caller_env())
  if ("col_var" %in% .said)
    lifecycle::deprecate_soft("2.0.0", "tab(col_var = )", "tab(col_vars = )",
                              user_env = rlang::caller_env())
  row_var_quo <- rlang::enquo(row_vars)
  col_var_quo <- rlang::enquo(col_vars)

  svy <- svy_unwrap_data(data, "tab")
  if (!is.null(svy)) data <- svy$data


  if (quo_miss_na_null_empty_no(row_var_quo)) {
    data <- data |> dplyr::mutate(no_row_var = factor("no_row_var")) # "n"
    row_var <- "no_row_var"
  } else {
    row_var <- names(tidyselect::eval_select(row_var_quo, data))
  }

  if (quo_miss_na_null_empty_no(col_var_quo)) {
    data <- data |> dplyr::mutate(no_col_var = factor("n"))
    col_var <- "no_col_var"
  } else {
    col_var <- names(tidyselect::eval_select(col_var_quo, data))
  }

  tab_vars <- rlang::enquo(tab_vars)
  if (quo_miss_na_null_empty_no(tab_vars)) {
    tab_vars <- character()
  } else {
    tab_vars <- names(tidyselect::eval_select(tab_vars, data))
  }

  sup_cols_quo <- .dots$sup_cols
  if (is.null(sup_cols_quo) || quo_miss_na_null_empty_no(sup_cols_quo)) {
    sup_cols <- character()
  } else {
    lifecycle::deprecate_soft(
      "2.0.0", "tab(sup_cols = )",
      details = "Pass these columns in `col_vars` and set `levels = \"first\"`.",
      user_env = rlang::caller_env()
    )
    sup_cols <- names(tidyselect::eval_select(sup_cols_quo, data))
  }

  spread_vars_quo <- rlang::enquo(spread_vars)
  if (quo_miss_na_null_empty_no(spread_vars_quo)) {
    spread_vars <- character()
  } else {
    spread_vars <- names(tidyselect::eval_select(spread_vars_quo, data))
    if (!all(spread_vars %in% tab_vars)) {
      cli::cli_abort(c("{.arg spread_vars} must be among the {.arg tab_vars}.",
                       "i" = "Got {.val {setdiff(spread_vars, tab_vars)}}, tab_vars are {.val {tab_vars}}."))
    }
  }

  # forwarded to the engine as a VALUE (a quosure or NULL), never as an NSE argument.
  filter_quo <- rlang::enquo(filter)
  if (rlang::quo_is_missing(filter_quo) || rlang::quo_is_null(filter_quo)) filter_quo <- NULL

  wt_quo <- rlang::enquo(wt)
  if (quo_miss_na_null_empty_no(wt_quo)) {
    wt <- character()
  } else {
    wt <- rlang::ensym(wt)
  }

  # WARNING: a design carries its own weights, so they ARE the weight column. Passing a design AND
  #   `wt` is a contradiction: it aborts rather than silently dropping the user's column.
  if (!is.null(svy)) {
    svy_abort_wt_design(length(wt) != 0L)
    wt <- rlang::sym(svy$spec$wt)
  }
  else if (length(wt) && identical(as.character(wt)[1], svy_wt_col))
    cli::cli_abort(c("{.val {svy_wt_col}} is a name tabxplor reserves for a survey design.",
                     "i" = "Rename that column, or pass a {.fn survey::svydesign} as {.arg data}."))

  test_on     <- test
  design_spec <- svy$spec

  if (is.list(pct))
    cli::cli_abort(c(
      "{.arg pct} is per {.arg col_vars}, so it must be a character vector, not a list.",
      "i" = "The row-variable axis is global in {.fn tab}: for different percentages per row
             variable, build one {.fn tab} per variable."))
  vctrs::vec_assert(ref2, size = 1)
  # DESIGN: the row_var axis is globalised -- pct / color / comp / ci / test / ref2 apply to ALL row
  #   variables. `ref` is the exception: one reference per row_var, named or ordered.
  vctrs::vec_assert(ci  , size = 1)
  vctrs::vec_assert(test, size = 1)

  na_drop_all <- switch(na,
                        "keep"        = character(),
                        "drop"        = character(),
                        "drop_all"    = character(),
                        "common_base" = c(row_var, col_var[1], tab_vars))
  na_effective <- if (na == "common_base") "keep" else na

  sup <- tab_deprecate_sup_cols(sup_cols, col_var, levels, pct)

  result <- tab_build(data = data,
           row_vars = tidyselect::all_of(row_var),
           col_vars = tidyselect::all_of(sup$col_vars),
           tab_vars = tidyselect::all_of(tab_vars),
           wt = !!wt,
           levels = sup$levels,
           na = na_effective, na_drop_all = tidyselect::all_of(na_drop_all),
           # a VALUE, never NSE -- see tab_build()'s filter WARNING
           filter = filter_quo,
           digits = digits,
           cleannames = cleannames,
           output = if (isTRUE(output_list)) "list" else "single", #pvalue_line = pvalue_line,
           other_if_less_than = other_if_less_than, other_level = other_level,
           totaltab = totaltab, totaltab_name = totaltab_name,
           common_totrow = common_totrow,
           totrow = .a$totrow,
           totcol = .a$totcol,
           total_names = total_names,
           pct  = sup$pct,
           ref = ref, ref2 = ref2, #c(ref, rep(ref , length(sup_cols))),
           comp = comp,
           chi2 = test_on,
           anova = anova,
           design_spec = design_spec,
           ci = ci,
           conf_level = conf_level,
           stars = stars,
           ci_method = ci_method, design_effect = design_effect,
           color = color,
           color_signif = color_spec$signif,
           color_ratio_ci = color_pct_text_is_ratio(color_spec),
           display = display,
           add_n = add_n, add_pct = add_pct,
           subtext = subtext, n_min = n_min, parallel = parallel,
           spread_vars = spread_vars, names_prefix = names_prefix, names_sort = names_sort,
           .cache = .cache, .defer_level_merge = .defer_level_merge,
           .levels_order = .levels_order, .levels_collapse = .levels_collapse)

  if (isTRUE(.return_armed)) return(result)

  result <- finalize_color_tail(result, color_spec, color_breaks, display)

  if (isTRUE(getOption("tabxplor.output_kable"))) return(tab_html(result))

  as_tabxplor_tabs(result)
}




# === SECTION: the colour-spec grammar ======================================================

# The tail every public entry point runs on the PRE-finalise table: the two-channel colour +
# significance attributes, then the display recipe. WARNING: color_breaks is stored LAST.
#' @keywords internal
#' @noRd
finalize_color_tail <- function(result, color_spec, color_breaks = NULL, display = NULL) {
  result <- finalize_color_spec(result, color_spec)
  result <- tab_apply_display(result, display)
  set_color_breaks_attr(result, resolve_color_breaks_arg(color_breaks))
}



#' @keywords internal
color_decode_legacy <- function(color) {
  a <- COLOR_ALIASES[[color]]
  if (is.null(a)) list(measure = color, policy = NULL) else a
}

# The `color` grammar: POSITION = channel (1st text, 2nd background), NAMES = column type
# (pct / mean). `legacy` is the scalar CLEAN measure the build pipeline reads for its side effects.
#' @keywords internal
normalize_color_spec <- function(color, color_signif = "ignore", deprecate = TRUE) {
  signif <- if (length(color_signif) == 0L) "ignore" else color_signif[1]
  if (is.na(signif) || signif %in% c("", "no")) signif <- "ignore"
  if (identical(signif, "color_all_signif")) {
    lifecycle::deprecate_soft("2.0.0", I('color_signif = "color_all_signif"'),
                              with = I('color_signif = "guaranteed_effect"'),
                              user_env = rlang::caller_env(2))
    signif <- "guaranteed_effect"
  }
  ok_signif <- COLOR_SIGNIF_VALUES          # the ONE vocabulary (R/fmt_class.R)
  if (!signif %in% ok_signif) {
    cli::cli_abort(c("Unknown {.arg color_signif} value {.val {signif}}.",
                     "i" = "Valid: {.val {ok_signif}}."))
  }
  uenv       <- rlang::caller_env(2)
  # WARNING: normalising must run AFTER the alias decode -- measure_key() resolves a policy-carrying
  #   alias to its MEASURE, so normalising first discards the policy half. "auto" passes through.
  norm       <- function(m) {
    if (is.na(m) || identical(m, "no")) return("")
    if (identical(m, "auto")) return("auto")
    k <- measure_key(m); if (is.na(k)) as.character(m) else if (!nzchar(k)) "" else k
  }

  # WARNING: `deprecate = FALSE` is not a convenience -- it is REQUIRED on the internal seam.
  #   legacy_union() MANUFACTURES the legacy spellings and the pipeline hands one back here to be
  #   re-parsed; deprecating would blame the user for a string the pipeline wrote.
  deprecate_old <- function(text) {
    if (!deprecate) return(invisible(NULL))
    if (text %in% color_legacy_spellings()) {
      lifecycle::deprecate_soft(
        "2.0.0",
        I(paste0("The `color = \"", text, "\"` mode")),
        with = I("`color = \"diff\"` with the `color_signif` argument"),
        user_env = uenv)
    }
  }

  parse_channels <- function(v) {
    v   <- unname(as.character(v))
    raw <- if (length(v) >= 1L) v[1] else NA_character_
    deprecate_old(raw)
    dec  <- if (is.na(raw)) list(measure = raw, policy = NULL) else color_decode_legacy(raw)
    if (!is.null(dec$policy)) signif <<- dec$policy
    text <- norm(dec$measure)
    bg   <- if (length(v) >= 2L) v[2] else NA_character_
    # DESIGN: a combined string is a (measure, policy) pair and the policy is scalar for the whole
    #   spec, so it cannot describe a second channel -- refuse it, never keep its measure half.
    if (!is.na(bg) && !is.null(COLOR_ALIASES[[bg]]$policy)) {
      cli::cli_abort(c("{.val {bg}} cannot go on the background channel.",
                       "i" = "It also names a significance policy; set that with {.arg color_signif}."))
    }
    bg <- if (is.na(bg)) NA_character_ else norm(bg)
    if (!is.na(bg) && bg == "") bg <- NA_character_
    # DESIGN: ONE validator, called with producer = "tab", so a measure only tab_reg() can build is
    #   refused HERE. `auto` is this boundary's own sentinel and is exempt.
    if (identical(text, "auto")) {
      if (!is.na(bg)) measure_validate(c("", bg), producer = "tab", call = rlang::caller_env())
    } else {
      measure_validate(c(text, if (is.na(bg)) NULL else bg), producer = "tab",
                       call = rlang::caller_env())
    }
    c(text, if (is.na(bg)) NA_character_ else bg)
  }

  legacy_union <- function(ms) {
    ms <- ms[!is.na(ms) & ms != ""]
    if ("auto" %in% ms) return("auto")   # resolved per column type downstream (tab_resolve_settings)
    builds <- vapply(ms, measure_builds, character(1), USE.NAMES = FALSE)
    for (b in COLOR_BUILD_ORDER) if (b %in% builds) return(measure_of_build(b))
    "no"
  }

  # ---- FALSE / TRUE ----
  if (is.logical(color)) {
    if (isTRUE(color)) {
      return(list(mode = "auto", legacy = "auto", text = "auto", bg = NA_character_,
                  types = NULL, signif = signif))
    }
    return(list(mode = "off", legacy = "no", text = "", bg = NA_character_,
                types = NULL, signif = "ignore"))
  }

  cnms <- names(color)
  if (!is.null(cnms) && length(setdiff(cnms[nzchar(cnms)], c("text", "background", "bg"))) == 0L &&
      any(cnms %in% c("text", "background", "bg"))) {
    lifecycle::deprecate_soft("2.0.0", I('color = c(text = , background = )'),
                              with = I('a positional color = c("diff", "ratio")'),
                              user_env = rlang::caller_env(2))
    cc   <- as.character(color)
    tval <- if ("text" %in% cnms) cc[cnms == "text"][1] else ""
    bval <- if ("background" %in% cnms) cc[cnms == "background"][1]
            else if ("bg" %in% cnms) cc[cnms == "bg"][1] else NA_character_
    color <- if (is.na(bval)) tval else c(tval, bval)   # -> positional; falls through to the flat path
  }

  # ---- list(pct =, mean =) or a NAMED vector : per column TYPE ----
  is_typed <- (is.list(color) && !is.null(names(color)) && all(nzchar(names(color)))) ||
    (!is.null(names(color)) && any(nzchar(names(color))))
  if (is_typed) {
    nms <- names(color)
    if (is.null(nms) || !all(nzchar(nms)) || !all(nms %in% c("pct", "mean"))) {
      cli::cli_abort(c("A per-type {.arg color} must be named by column type ({.field pct} / {.field mean}).",
                       "i" = 'e.g. {.code list(pct = c("diff", "ratio"), mean = "ratio")}.',
                       "i" = "For two channels on every column use positions: {.code c(\"diff\", \"ratio\")}."))
    }
    entries <- if (is.list(color)) color else as.list(color)
    types   <- purrr::map(entries, parse_channels)
    legacy  <- legacy_union(unlist(types, use.names = FALSE))
    return(list(mode = "by_type", legacy = legacy, text = NA_character_, bg = NA_character_,
                types = types, signif = signif))
  }

  # ---- unnamed scalar / positional vector : the SAME measure(s) on every column ----
  ch     <- parse_channels(color)
  text   <- ch[1]; bg <- ch[2]
  legacy <- if (text %in% c("", "no") && !is.na(bg)) "diff" else legacy_union(ch)
  list(mode = "flat", legacy = legacy, text = text, bg = bg, types = NULL, signif = signif)
}

#' @keywords internal
finalize_color_spec <- function(x, spec) {
  if (is.list(x) && !is.data.frame(x)) return(purrr::map(x, ~ finalize_color_spec(., spec)))
  rewrite <- spec$mode %in% c("auto", "by_type") || !is.na(spec$bg) ||
    spec$signif != "ignore" || identical(spec$text, "ratio")
  if (!rewrite) return(x)
  dplyr::mutate(x, dplyr::across(dplyr::where(is_fmt), ~ finalize_one_col(.x, spec)))
}

#' @keywords internal
color_pct_text_is_ratio <- function(spec) {
  if (is.null(spec) || is.null(spec$mode)) return(FALSE)
  m <- switch(spec$mode,
              "flat"    = spec$text,
              "by_type" = c(spec$types[["pct"]][1], spec$types[["mean"]][1]),
              NA_character_)   # "auto" -> a column's text channel is resolved later; "off" -> no colour
  "ratio" %in% unname(m)
}

#' @keywords internal
resolve_col_measures <- function(spec, numeric_col, pct_col, built) {
  kind <- if (numeric_col) "num" else if (pct_col) "pct" else NA_character_
  if (spec$mode == "auto") {                                # color = TRUE smart per-kind default
    if (!measure_kind_keyed(built))
      return(if (identical(measure_builds(built), "or")) "odds_ratio" else NULL)
    if (is.na(kind)) return(NULL)
    m <- c(measure_auto(kind, "text"), measure_auto(kind, "bg"))
    m <- m[nzchar(m)]
    return(if (length(m) == 0L) NULL else unname(m))
  }
  if (spec$mode == "by_type") {
    if (!measure_kind_keyed(built)) return(NULL)            # keep what the pipeline built
    key <- if (numeric_col) "mean" else if (pct_col) "pct" else NA_character_
    if (is.na(key) || is.null(spec$types[[key]])) return(NULL)
    m <- spec$types[[key]]
    return(if (is.na(m[2])) m[1] else m)
  }
  text <- spec$text
  if (identical(text, "auto")) {
    if (is.na(kind)) return(NULL)
    text <- measure_auto(kind, "text")
    if (!nzchar(text)) return(NULL)
    if (is.na(spec$bg)) {
      bg <- measure_auto(kind, "bg")
      return(if (nzchar(bg)) c(text, bg) else text)
    }
  }
  if (text == "" && is.na(spec$bg)) return(NULL)
  if (is.na(spec$bg)) text else c(text, spec$bg)
}

#' @keywords internal
finalize_one_col <- function(col, spec) {
  built <- get_color(col)
  if (built %in% c("", "no")) return(col)                  # the pipeline did not color this column
  measures <- resolve_col_measures(spec, fmt_var_kind(col) != "pct", get_pct_type(col) != "none",
                                   built)
  if (is.null(measures)) return(col)
  if (length(measures) == 1L && measures %in% c("", "no")) return(col)
  set_color_signif(set_color(col, measures), spec$signif)
}






# === SECTION: the typed build context ======================================================

# Update `ctx` with what a stage produced. Single-bracket `[<-` so a NULL value is PRESERVED as a
# list element (`ctx$x <- NULL` deletes it, breaking the list2env() unpack) and a tibble is whole.
#' @keywords internal
#' @noRd
ctx_update <- function(ctx, updates) {
  ctx[names(updates)] <- updates
  ctx
}


# Project the SETTINGS SPINE into the bare names one stage reads. CTX_SETTINGS_LOCALS declares
# them: codetools cannot see the bindings, and a column with no projection must fail the assert.
# WARNING: `col_vars_num` / `col_vars_text` are NAMED logicals downstream but the spine stores them
#   unnamed -- the names are restored here; dropping them silently returns NULL from that read.
#' @keywords internal
#' @noRd
CTX_SETTINGS_LOCALS <- c(
  # settings$rows, minus its key (na_num is added by tab_prepare_pop)
  "color", "comparison", "or_ci", "chi2", "ref", "ref2", "comp", "ci", "ci_scale",
  "totaltab", "totrow", "na_num",
  # settings$cols (lv1 added by tab_prepare_pop)
  "lvs", "lv1", "digits", "col_vars_num", "col_vars_text",
  # settings$pairs (na added by tab_prepare_pop)
  "pct_vect", "ref_vect", "ref2_vect", "na_text"
)

#' @keywords internal
#' @noRd
ctx_settings_locals <- function(ctx) {
  s <- ctx$settings
  if (is.null(s)) return(list())
  # WARNING: `[[`, never `$` -- the spine is filled in TWO stages, and tibble's `$` warns on a column
  #   the earlier stage has not written yet.
  out <- c(as.list(s$rows[setdiff(names(s$rows), "row_var")]),
           list(lvs           = s$cols[["lvs"]],
                lv1           = s$cols[["lv1"]],
                digits        = s$cols[["digits"]],
                col_vars_num  = stats::setNames(s$cols[["is_num"]] , s$cols[["col_var"]]),
                col_vars_text = stats::setNames(s$cols[["is_text"]], s$cols[["col_var"]]),
                pct_vect      = s$pairs[["pct"]],
                ref_vect      = s$pairs[["ref"]],
                ref2_vect     = s$pairs[["ref2"]],
                na_text       = s$pairs[["na"]][s$pairs[["is_text"]]]))
  stopifnot(all(names(out) %in% CTX_SETTINGS_LOCALS))
  out
}


# The TYPED ctx constructor: every field gets ONE default here, so every ctx carries the full field
# set. DESIGN: `[<-` writes an explicit NULL as a PRESENT-but-NULL key, which list2env() needs.
#' @keywords internal
#' @noRd
new_ctx <- function(...) {
  defaults <- list(
    data = NULL, filter_expr = NA_character_,
    row_vars_quo = NULL, col_vars_quo = NULL, tab_vars_quo = NULL,
    wt_quo = NULL, na_drop_all_quo = NULL,
    # inputs (= each formal's current default)
    pct = "no", color = "no", color_signif = "ignore", color_ratio_ci = FALSE,
    anova = NULL,
    display = NULL, chi2 = FALSE, design_spec = NULL,
    agg_only = FALSE,
    na = "keep", levels = "all",
    cleannames = NULL, output = "single",
    other_if_less_than = 0, other_level = "Others", levels_collapse = NULL,
    ref = "auto", ref2 = "first", comp = "tab",
    ci = "auto", conf_level = 0.95, stars = NULL,
    ci_method = default_ci_method(), design_effect = NULL,
    inference = new_inference(),
    totaltab = "line", totaltab_name = "Ensemble", totrow = TRUE, totcol = "last",
    total_names = "Total", add_n = TRUE, add_pct = FALSE, common_totrow = FALSE, digits = 0,
    subtext = "", n_min = 0, by_table = FALSE, parallel = NULL,
    spread_vars = character(), names_prefix = NULL, names_sort = FALSE,
    cache_env = NULL, defer_level_merge = FALSE, levels_order = NULL,

    # --- STAGE PRODUCTS: written by one stage, read by a later one -----------------------------
    # tab_setup:        the resolved variable roles + the arg products no grain fits
    settings = NULL, row_vars = NULL, col_vars = NULL, tab_vars = NULL, wt = NULL,
    tab_row_names = NULL, na_drop_all = NULL, tot_cols_type = NULL, cache_keys = NULL,
    # tab_prepare_pop:  the non-first levels dropped at display time (NULL = nothing to drop)
    remove_levels = NULL,
    # tab_aggregate:    the tier-1 aggregates + the two jmvtab cache products
    fine_num = NULL, fine_fused = NULL, cached_tests = NULL, tier2_keys = NULL,
    # tab_transform:    this row_var's built tables + its whole-table tests
    tabs_text = NULL, tabs_num = NULL, chi2_num = NULL,
    robust_tests = NULL,
    # tab_*_tables:     the finished per-row_var tab(s) + the tier-2 test store
    tabs = NULL, tests = NULL,
    var_labels = character()
  )
  ctx_update(defaults, list(...))
}
# (the derived globalVariables() declaration for these fields is at the END of this file)


#' @keywords internal
#' @noRd
resolve_stars <- function(stars, call = rlang::caller_env()) {
  if (is.null(stars)) {
    v <- tx_option("stars")
    return(if (is.numeric(v)) length(v) > 0L else isTRUE(v))
  }
  if (is.numeric(stars))
    cli::cli_abort(c("{.arg stars} is TRUE or FALSE; the star LADDER is a global option.",
                     "i" = 'Set it with {.code options(tabxplor.stars = c("*" = 0.05, "**" = 0.01))}
                            -- it is read when the table is printed, so it applies to every table.'),
                   call = call)
  stars
}
#' @keywords internal
#' @noRd
force_comp <- function(comp, tab_vars) {
  if (length(tab_vars) == 0 && all(comp == "all")) "tab" else comp
}

#' @keywords internal
#' @noRd
resolve_cleannames <- function(cleannames) {
  if (is.null(cleannames)) tx_option("cleannames") else cleannames
}
#' @keywords internal
#' @noRd
conf_level_default <- function() tx_option("conf_level")


# === SECTION: the build engine and the row axis ============================================

#' @keywords internal
#' @noRd
tab_build <- function(data, row_vars, col_vars, tab_vars, wt,
                      pct = "no", color = "no", color_signif = "ignore",
                      color_ratio_ci = FALSE,
                      display = NULL, chi2 = FALSE, anova = NULL, design_spec = NULL,
                      na = "keep", levels = "all", na_drop_all,
                      cleannames = NULL, output = "single", #pvalue_line = NULL,
                      other_if_less_than = 0, other_level = "Others",
                      ref = "auto", ref2 = "first", comp = "tab",
                      ci = "auto", conf_level = 0.95, stars = NULL, #ci_visible = FALSE,
                      ci_method = default_ci_method(), design_effect = NULL,
                      totaltab = "line", totaltab_name = "Ensemble",
                      totrow = TRUE, totcol = "last", total_names = "Total",
                      add_n = TRUE, add_pct = FALSE, common_totrow = FALSE,
                      digits = 0, subtext = "", n_min = 0,
                      parallel = NULL,
                      .by_table = FALSE,
                      spread_vars = character(), names_prefix = NULL, names_sort = FALSE,
                      .cache = NULL, .defer_level_merge = FALSE,
                      .levels_order = NULL, .levels_collapse = NULL,

                      # a VALUE (quosure / string / NULL), not NSE -- see the WARNING below
                      filter = NULL #, listed = FALSE,
) {

  filter_expr <- NA_character_
  # WARNING: `filter` reaches this INTERNAL engine already DEFUSED -- a quosure, a character string
  #   (the tribble idiom, parsed below), or NULL. `{{ }}` inside an `if` defuses the `if` itself.
  if (!is.null(filter) && !(rlang::is_quosure(filter) && rlang::quo_is_null(filter))) {
    # WARNING: rlang gives a CONSTANT quosure the EMPTY environment, so re-quoting the parsed call with
    #   quo_get_env() leaves it unable to find even `%in%`. Fall back to this frame's caller.
    if (rlang::is_quosure(filter)) {
      fx <- rlang::quo_get_expr(filter)
      if (is.character(fx)) {
        env <- rlang::quo_get_env(filter)
        if (identical(env, rlang::empty_env())) env <- rlang::caller_env()
        filter <- rlang::new_quosure(str2lang(fx), env)
      }
    } else if (is.character(filter)) {
      filter <- rlang::new_quosure(str2lang(filter), rlang::caller_env())
    }
    data <- data |> dplyr::mutate(.filter = !!filter)
    filter_expr <- paste(rlang::as_label(filter), collapse = "")
  }

  ctx <- new_ctx(
    data = data, filter_expr = filter_expr,
    row_vars_quo = rlang::enquo(row_vars), col_vars_quo = rlang::enquo(col_vars),
    tab_vars_quo = rlang::enquo(tab_vars), wt_quo = rlang::enquo(wt),
    na_drop_all_quo = rlang::enquo(na_drop_all),
    pct = pct, color = color, color_signif = color_signif,
    color_ratio_ci = color_ratio_ci, display = display, chi2 = chi2, anova = anova,
    design_spec = design_spec,
    na = na, levels = levels,
    cleannames = cleannames, output = output,
    other_if_less_than = other_if_less_than, other_level = other_level,
    levels_collapse = new_lvl_collapse(.levels_collapse),
    ref = ref, ref2 = ref2, comp = comp, ci = ci, conf_level = conf_level, stars = stars,
    ci_method = ci_method, design_effect = design_effect,
    totaltab = totaltab, totaltab_name = totaltab_name, totrow = totrow, totcol = totcol,
    total_names = total_names, add_n = add_n, add_pct = add_pct, common_totrow = common_totrow,
    digits = digits,
    subtext = subtext, n_min = n_min, by_table = .by_table,
    parallel = parallel,
    spread_vars = spread_vars, names_prefix = names_prefix, names_sort = names_sort,
    cache_env = .cache, defer_level_merge = .defer_level_merge,
    levels_order = .levels_order
  )

  ctx <- tab_setup(ctx)          # resolve per-row_var + per-col_var arg vectors + colour cascade + keys
  ctx <- tab_prepare_pop(ctx)    # prepare the whole DB once (na/lump/levels; the global drop_all drop)
  ctx <- tab_aggregate(ctx)      # tier-1 aggregates (fine_num per-rv + shared fine_fused); jmvtab hook
  tab_build_tables(ctx)          # the OUTER map over row_vars + the cross-row_var output shape
}


#' @keywords internal
#' @noRd
tab_build_tables <- function(ctx) {
  workers <- tab_parallel_workers(ctx$parallel, ctx$cache_env)
  units   <- tab_rowvar_ctxs(ctx)
  rv_names <- as.character(ctx$row_vars)
  built   <- tab_pmap(list(ctx_i = units), "tab_build_one",
                      .ship = list(data = ctx$data, fine_fused = ctx$fine_fused,
                                   design = ctx$inference$design),
                      .names = rv_names, workers = workers)
  tabs  <- purrr::set_names(purrr::map(built, "tab"),  rv_names)
  tests <- purrr::set_names(purrr::map(built, "test"), rv_names)
  ctx   <- ctx_update(ctx, list(tabs = tabs, tests = tests))

  if (!is.null(ctx$cache_env)) jmv_cache_store_tests(ctx)

  tab_assemble_output(ctx)
}


#' @keywords internal
#' @noRd
tab_rowvar_ctxs <- function(ctx) {
  rows  <- ctx$settings$rows
  pairs <- ctx$settings$pairs
  n     <- nrow(rows)
  per_rv <- c("row_vars", "settings", "tab_row_names", "fine_num")
  shared <- ctx[setdiff(names(ctx), c(per_rv, "data", "fine_fused", "design_spec"))]
  shared$inference["design"] <- list(NULL)
  shared <- shared[!grepl("_quo$", names(shared))]
  shared$parallel  <- FALSE     # THE NESTING RULE -- stated once, in tab_pmap()'s everywhere() block
  shared$cache_env <- NULL

  lapply(seq_len(n), function(i) {
    rv   <- rows$row_var[i]
    keep <- pairs$row_var == rv
    u <- list()
    u$row_vars      <- ctx$row_vars[i]                             # keep as a length-1 sym list
    u$tab_row_names <- as.character(c(ctx$tab_vars, ctx$row_vars[i]))
    u$settings      <- list(rows = rows[i, ], cols = ctx$settings$cols, pairs = pairs[keep, ])
    u$fine_num      <- ctx$fine_num[[rv]]                          # by NAME (NULL when no numeric cols)
    c(shared, u)
  })
}


# THE `ref2 = "cumulative"` eligibility rule: a cumulative odds ratio dichotomises a col_var at
# each cut point, so it needs an ORDERED scale with 3+ levels and `pct = "row"`. DESIGN: an
# ineligible pair DEGRADES to "first" rather than aborting -- a table can mix ordered and nominal.
#' @keywords internal
#' @noRd
or_cum_ok <- function(x) is.ordered(x) && nlevels(x) >= 3L

#' @keywords internal
#' @noRd
ref2_resolve_cum <- function(ref2, pct, col_vars_cumor) {
  v <- vctrs::vec_recycle(as.character(ref2), length(col_vars_cumor))
  if (!any(v == "cumulative")) return(v)
  want <- v == "cumulative"
  bad_class <- want & !col_vars_cumor
  bad_pct   <- want &  col_vars_cumor & pct != "row"
  if (any(bad_class)) cli::cli_inform(c(
    "i" = paste0("{.code ref2 = \"cumulative\"} needs an {.cls ordered} col_var with 3+ levels; ",
                 "{cli::qty(sum(bad_class))} {?it is/they are} skipped here."),
    "i" = "{.code data |> dplyr::mutate(x = factor(x, levels = c(...), ordered = TRUE))}"
  ))
  if (any(bad_pct)) cli::cli_inform(c(
    "i" = paste0("{.code ref2 = \"cumulative\"} cumulates each row's distribution, so it needs ",
                 "{.code pct = \"row\"}; skipped here.")
  ))
  v[bad_class | bad_pct] <- "first"
  v
}


# === STAGE 1/5: tab_setup() -- resolve arguments + build the settings spine (no cache tier) ==
#' @keywords internal
#' @noRd
tab_setup <- function(ctx) {
  list2env(ctx, environment())

  stopifnot(output %in% c("single", "list"))

  cleannames <-
    resolve_cleannames(cleannames)

  stars <- resolve_stars(stars)


  stopifnot(levels %in% c("first", "all", "auto"))
  lvs <- levels

  row_vars <- row_vars_quo
  if (quo_miss_na_null_empty_no(row_vars)) {
    data     <- data |> dplyr::mutate(no_row_var = factor("no_row_var")) # "n"
    row_vars <- rlang::syms("no_row_var")
    pos_row_vars <- tidyselect::eval_select("no_row_var", data)
  } else {
    pos_row_vars <- tidyselect::eval_select(row_vars, data)
    row_vars     <- rlang::syms(names(pos_row_vars))
  }

  col_vars <- col_vars_quo
  if (quo_miss_na_null_empty_no(col_vars)) {
    data     <- data |> dplyr::mutate(no_col_var = factor("n"))
    col_vars <- rlang::syms("no_col_var")
    pos_col_vars <- tidyselect::eval_select("no_col_var", data)
  } else {
    pos_col_vars <- tidyselect::eval_select(col_vars, data)
    col_vars     <- rlang::syms(names(pos_col_vars))
  }
  tab_vars <- tab_vars_quo
  if (quo_miss_na_null_empty_no(tab_vars)) {
    tab_vars <- character() #rlang::syms("no_tab_vars")
  } else {
    pos_tab_vars <- tidyselect::eval_select(tab_vars, data)
    tab_vars     <- rlang::syms(names(pos_tab_vars))
  }

  # WARNING: capture the variable labels BEFORE the labelled conversion, which strips them. Both run
  #   before the numeric / text classification, so a labelled categorical reads as a factor.
  sel_vars   <- unique(c(as.character(row_vars), as.character(col_vars), as.character(tab_vars)))
  var_labels <- capture_var_labels(data, sel_vars)
  data       <- data |> tab_apply_val_labels(sel_vars)

  # WARNING: an NA factor LEVEL is a real category whose label is NA. Convert it to an NA VALUE so the
  #   `na=` machinery handles it -- else it poisons the total-row mask and crashes every render.
  for (v in sel_vars) {
    if (is.factor(data[[v]]) && anyNA(levels(data[[v]])))
      data[[v]] <- forcats::fct_na_level_to_value(data[[v]])
  }

  # WARNING: a logical col_var is a natural 2-level cross-tab variable, but the masks below cover
  #   neither it nor a Date: coerce the logical, abort cleanly on any other unsupported type.
  for (p in pos_col_vars) {
    nm <- names(data)[[p]]
    v  <- data[[p]]
    if (is.logical(v)) {
      data[[nm]] <- forcats::as_factor(v)
    } else if (!is.numeric(v) && !is.factor(v) && !is.character(v)) {
      cli::cli_abort(c(
        "Column variable {.val {nm}} must be a factor, character or numeric.",
        "x" = "Got a {.cls {class(v)}} column.",
        "i" = "Convert it first \u2014 bin a date or continuous variable into groups, or use {.code as.factor()}."
      ))
    }
  }

  # WARNING: extract by POSITION with `[[`, never `data[<int vector>]` -- that is column-subsetting on
  #   a data.frame but ROW-subsetting on a data.table, which silently mis-classified the col_vars.
  col_vars_num  <- purrr::map_lgl(pos_col_vars, ~ is.numeric(data[[.x]]))
  col_vars_text <- purrr::map_lgl(pos_col_vars,
                                  ~ is.factor(data[[.x]]) || is.character(data[[.x]]))
  col_vars_cumor <- purrr::map_lgl(pos_col_vars, ~ or_cum_ok(data[[.x]]))

  if (quo_miss_na_null_empty_no(wt_quo)) {
    wt <- character() #rlang::sym("no_weight")
  } else {
    wt <- rlang::sym(rlang::as_name(wt_quo))
  }
  conf_level <- vctrs::vec_recycle(conf_level, 1)
  inference <- new_inference(wt, design_spec, conf_level, ci_method, agg_only,
                             design_effect = design_effect)
  if (length(wt) != 0L &&
      as.character(wt) %in% c(as.character(row_vars), as.character(col_vars), as.character(tab_vars))) {
    cli::cli_abort(c(
      "The weight variable {.val {as.character(wt)}} is also used as a row, column or tab variable.",
      "i" = "A weight cannot be a table variable at the same time \u2014 pick a different weight column."
    ))
  }
  tab_dup <- intersect(as.character(tab_vars),
                       c(as.character(row_vars), as.character(col_vars)))
  if (length(tab_dup) != 0L) {
    cli::cli_abort(c(
      "{cli::qty(tab_dup)}The variable{?s} {.val {tab_dup}} {?is/are} used both as a tab variable \\
       and as a row or column variable.",
      "i" = "A variable cannot be a tab variable and a row/column variable at the same time \u2014 \\
             pick a different variable for one of the two roles."
    ))
  }

  if (rlang::quo_is_missing(na_drop_all_quo) || rlang::quo_is_null(na_drop_all_quo)) {
    na_drop_all <- character()
  } else {
    na_drop_all <- names(tidyselect::eval_select(na_drop_all_quo, data))
  }

  tab_row_names  <- as.character(c(tab_vars, row_vars))




  #Arguments vectorised over row : tested in tab_plain/tab_num
  nrowvars    <- length(row_vars)
  totaltab    <- vctrs::vec_recycle(totaltab, nrowvars)
  totrow      <- vctrs::vec_recycle(totrow  , nrowvars)
  # WARNING: detect a per-COL_VAR reference BEFORE resolve_ref_vector(row_vars), which would warn on
  #   the col_var names. Under col% `ref` is vectorised over col_vars, each item picking a column.
  pct_flat      <- unlist(pct)
  col_regime    <- any(pct_flat == "col") && !any(pct_flat == "row")
  ref_by_colvar <- NULL
  named_colvar   <- !is.null(names(ref)) && any(nzchar(names(ref))) &&
                    any(names(ref) %in% as.character(col_vars))
  positional_colvar <- col_regime && is.null(names(ref)) && length(ref) > 1 &&
                       length(ref) == length(col_vars)
  if (col_regime && (named_colvar || positional_colvar)) {
    ref_by_colvar <- resolve_ref_vector(ref, as.character(col_vars), what = "col_var")
    ref <- "auto"   # scalar unset: tab_num / settings / the row% path behave as no per-row ref
  }
  ref_is_vector <- length(ref) > 1
  ref         <- resolve_ref_vector(ref, as.character(row_vars))
  if (ref_is_vector && col_regime) {
    cli::cli_inform(c("i" = paste0("With {.code pct = \"col\"}, {.arg ref} is vectorised over the ",
                                   "col_vars (length {length(col_vars)}); this ref did not match, so it ",
                                   "is collapsed to a single column reference (its first value).")))
    ref <- vctrs::vec_recycle(ref[1], nrowvars)
  }
  ref2        <- vctrs::vec_recycle(ref2    , nrowvars)
  comp        <- vctrs::vec_recycle(comp    , nrowvars)
  color       <- vctrs::vec_recycle(color   , nrowvars)

  #Arguments vectorised over row : tested here or in tab_num (not in tab_plain)
  ci          <- vctrs::vec_recycle(ci      , nrowvars)
  chi2        <- vctrs::vec_recycle(chi2    , nrowvars)

  #Arguments vectorised over columns : tested here
  ncolvars    <- length(col_vars)
  lvs         <- vctrs::vec_recycle(lvs   , ncolvars)
  digits      <- vctrs::vec_recycle(digits, ncolvars)
  if (as.character(totcol)[1] %in% c("last", "all_col_vars", "each")) {
    totcol <- col_vars_text[col_vars_text] |> names() |> dplyr::last()
    if (all(lvs == "first") & all(pct == "row") & ncolvars > 1) {
      totcol <- NULL
    }
  } else if (as.character(totcol)[1] %in% c("no", "")) {
    totcol <- col_vars[0]                                       # no total column
  } else {
    cli::cli_abort(c('{.arg totcol} must be {.val last} or {.val no}.',
                     "i" = "Through {.fn tab}, say {.code tot = \"col\"} or drop it from {.arg tot}."))
  }
  # tot_cols_type says what to do with the total columns downstream (consumed in tab_assemble_tables):
  #   "one"          = keep the ONE requested total column (the last text col_var's), drop the rest
  #   "no_delete"    = none requested, but one is needed internally (pct/ci/chi2/OR need a
  #                    reference total) -> build it, drop only at the very end
  #   "no_no_create" = no total col at all
  tot_cols_type <- if (length(totcol) != 0) {
    "one"
  } else if (any(chi2 != FALSE) | any(pct != "no") | any(ci != "no")) {
    "no_delete"
  } else {
    "no_no_create"
  }





  #Arguments vectorised over columns or rows : test in tab_plain/tab_num
  stopifnot(length(pct) >= 1)
  pct_vect <-
    if (is.character(pct) & length(pct) == 1) {
      rep(list(
        rep(pct, length(col_vars))
      ),
      length(row_vars),
      )
    } else if (is.character(pct) & length(row_vars) == 1) {
      list(vctrs::vec_recycle(pct, length(col_vars)))
    } else if (is.character(pct) & length(col_vars) == 1) {
      as.list(vctrs::vec_recycle(pct, length(row_vars)))
    } else if (is.character(pct) & length(pct) == length(col_vars)) {
      rep(list(pct), length(row_vars))
    } else if (is.list(pct) & length(pct) == length(row_vars) &
               all(purrr::map_int(pct, length) == length(col_vars))) {
      pct
    } else {
      stop("pct can't be recycled to the lengths of row_vars and col_vars (see documentation `?tab_many`)")
    }

  ref_vect <-
    if (!is.null(ref_by_colvar)) {
      rep(list(ref_by_colvar), length(row_vars))
    } else {
      purrr::map(ref, ~ rep(.x, length(col_vars)))
    }

  ref2_vect <- purrr::map2(ref2, pct_vect, ~ ref2_resolve_cum(.x, .y, col_vars_cumor))


  #Unique arguments :
  total_names <- vctrs::vec_recycle(total_names, 2)
  na          <- vctrs::vec_recycle(na , 1)


  .settings     <- tab_resolve_settings(color = color, ci = ci, chi2 = chi2,
                                         ref = ref, pct_vect = pct_vect,
                                         display_measure = display_comparison(display),
                                         col_vars_text = col_vars_text, totrow = totrow,
                                         color_signif = color_signif,
                                         color_ratio_ci = color_ratio_ci, stars = stars,
                                         na = na, wt_name = as.character(wt),
                                         other_if_less_than = other_if_less_than, comp = comp,
                                         tab_vars = as.character(tab_vars),
                                         row_vars = as.character(row_vars),
                                         col_vars = as.character(col_vars),
                                         filter_expr = filter_expr)
  color         <- .settings$color         # ONE resolved measure
  chi2          <- .settings$chi2
  ci            <- .settings$ci
  ci_scale      <- .settings$ci_scale     # "diff" / "ratio" (the Katz interval)
  comparison    <- .settings$comparison
  or_ci         <- .settings$or_ci
  color_signif  <- .settings$color_signif
  stars         <- .settings$stars
  totrow        <- .settings$totrow
  cache_keys    <- .settings$cache_keys

  # THE SETTINGS SPINE: a star schema built ONCE here, three typed tibbles at their natural grain --
  #   rows = one per row_var, cols = one per col_var, pairs = one per (row_var x col_var), carrying
  #   pct + ref + ref2 plus the `na` policy tab_prepare_pop adds. Expansion is ROW-MAJOR.
  # It carries SETTINGS only, never built OBJECTS: those ride the ctx.
  rv_chr <- as.character(row_vars) ; cv_chr <- as.character(col_vars)
  settings <- list(
    rows = tibble::tibble(
      row_var = rv_chr, color = color, comparison = comparison, or_ci = or_ci, chi2 = chi2,
      ref = ref, ref2 = ref2,
      comp = comp, ci = ci, ci_scale = ci_scale, totaltab = totaltab, totrow = totrow
    ),
    cols = tibble::tibble(
      col_var = cv_chr, is_num = unname(col_vars_num), is_text = unname(col_vars_text),
      lvs = lvs, digits = digits
    ),
    pairs = tibble::tibble(
      row_var = rep(rv_chr, each  = length(cv_chr)),
      col_var = rep(cv_chr, times = length(rv_chr)),
      is_text = rep(unname(col_vars_text), times = length(rv_chr)),
      pct     = unlist(pct_vect, use.names = FALSE),
      ref     = unlist(ref_vect , use.names = FALSE),
      ref2    = unlist(ref2_vect, use.names = FALSE)
    )
  )

  ctx <- ctx_update(ctx, list(
    data = data, settings = settings,
    row_vars = row_vars, col_vars = col_vars, tab_vars = tab_vars, wt = wt,
    tab_row_names = tab_row_names, na_drop_all = na_drop_all,
    cleannames = cleannames, stars = stars, color_signif = color_signif,
    inference = inference,
    total_names = total_names, na = na,
    totcol = totcol, tot_cols_type = tot_cols_type,
    cache_keys = cache_keys,
    var_labels = var_labels
  ))
  ctx[SPINE_OWNED_INPUTS] <- NULL
  ctx
}

# The tab_build() inputs whose resolved form is a SETTINGS SPINE column, and which therefore stop
# existing as ctx fields once tab_setup() has run (see there). `levels` resolves to `cols$lvs`.
#' @keywords internal
#' @noRd
SPINE_OWNED_INPUTS <- c("pct", "color", "chi2", "ci", "ref", "ref2", "comp",
                        "totaltab", "totrow", "digits", "levels")

# The assert CTX_SETTINGS_LOCALS promises: deleting an input is only safe if the spine projects it
# back, so every name here must reappear there (`pct` as `pct_vect`, `levels` as `lvs`).
stopifnot("every spine-owned input is projected back by ctx_settings_locals()" =
            all(c(setdiff(SPINE_OWNED_INPUTS, c("pct", "levels")), "pct_vect", "lvs")
                %in% CTX_SETTINGS_LOCALS))


# === STAGE 2/5: tab_prepare_pop() -- prepare the population ONCE (cache tier 0) ==============
#' @keywords internal
#' @noRd
tab_prepare_pop <- function(ctx) {
  list2env(ctx, environment())
  list2env(ctx_settings_locals(ctx), environment())   # lvs / col_vars_text, from the spine

  data <- data |> dplyr::select(!!!tab_vars, !!!row_vars, !!wt, !!!col_vars,
                                 tidyselect::any_of(c(svy_row_col, ".filter"))) |>
    relabel_levels_in_varnames(as.character(col_vars))


  if (!is.na(filter_expr)) data <- data |> dplyr::filter(.data$.filter) |>
    dplyr::select(-".filter")

  if (na == "drop_all") {
    na_drop_all <- as.character(c(row_vars, col_vars, tab_vars))
    na_text <- rep(list(rep("keep", sum(col_vars_text))), length(row_vars))
    na_num  <- rep(list("keep"), length(row_vars))

  } else {
    na_drop_all <- names(tidyselect::eval_select(rlang::enquo(na_drop_all), data))

    na_text <-
      purrr::map(as.character(row_vars),
                 ~ purrr::map2_lgl(., as.character(col_vars[col_vars_text]),
                                   ~ all(c(.x, .y, as.character(tab_vars)) %in% na_drop_all)
                 ) ) |>
      purrr::map(~ dplyr::if_else(., "keep", na))

    na_num <-
      purrr::map(as.character(row_vars),
                 ~ all(c(., as.character(tab_vars)) %in% na_drop_all)
      ) |>
      purrr::map(~ dplyr::if_else(., "keep", na))
  }

  data <- data |>
    tab_prepare(
      as.character(c(row_vars, col_vars, tab_vars)),
      na_drop_all = tidyselect::all_of(na_drop_all),
      cleannames = cleannames,
      other_if_less_than = other_if_less_than, other_level = other_level,
      levels_collapse = levels_collapse
    )


  if (other_if_less_than > 0 & length(tab_vars) != 0) {
    data <- data |>
      dplyr::group_by(!!!tab_vars) |>
      dplyr::mutate(dplyr::across(tidyselect::all_of(as.character(row_vars)),
                                  ~ forcats::fct_lump_min(., other_if_less_than,
                                                          other_level = other_level))) |>
      dplyr::ungroup() |>
      # WARNING: no nested lambda referencing `.x` here -- dplyr >= 1.2 inlines across() functions, which
      #   breaks the closure (`object '.x' not found`). Keep `.x` in the direct body only.
      dplyr::mutate(dplyr::across(tidyselect::all_of(as.character(row_vars)), function(.x) {
        lvs <- unique(append(levels(dplyr::pull(data, dplyr::cur_column())), other_level))
        forcats::fct_relevel(.x, lvs[lvs %in% levels(.x)])
      }))
  }


  if (length(wt) != 0) {
    zero_weight <- dplyr::pull(data, !!wt)
    zero_weight <- is.na(zero_weight) | zero_weight == 0
    if (nrow(data) != 0L && all(zero_weight)) {
      cli::cli_abort(c(
        "Every row has a zero or missing weight ({.val {as.character(wt)}}) \u2014 nothing to tabulate.",
        "i" = "Check the weight variable {.val {as.character(wt)}} for all-zero or all-NA values."
      ))
    }
    if (any(zero_weight)) {
      rlang::inform(paste0(sum(zero_weight), " rows with zero or NA weights were removed"))
      data <- data |> dplyr::filter(!zero_weight)
    }
  }


  if(any(lvs == "auto")) {
    lvs <- purrr::map2_chr(
      lvs,
      dplyr::select(data, !!!col_vars),
      ~ if (.x == "auto") {
        if(!(is.factor(.y) | is.character(.y))) {"first"} else {
          if(nlevels(forcats::fct_drop(.y)) == 2L) "first" else "all"
        }
      } else {
        .x
      }
    )

  }

  lv1 <- lvs == "first" & col_vars_text
  if (any(lv1)) {
    if (!isTRUE(defer_level_merge)) {
      col_vars_3levels <-
        purrr::map_lgl(dplyr::select(data, !!!col_vars),
                       ~ is.factor(.) & nlevels(.) >= 3) & lv1

      if (any(col_vars_3levels)) {

        rm_levels_by_col_vars <- dplyr::select(data, !!!col_vars[col_vars_3levels]) |>
          purrr::map(~ purrr::set_names(levels(.)[-1], "remove_levels"))

        data <- data |>
          dplyr::mutate(dplyr::across(
            tidyselect::all_of(as.character(col_vars[col_vars_3levels])),
            ~ suppressWarnings(forcats::fct_recode(., rlang::splice(rm_levels_by_col_vars[[dplyr::cur_column()]] )))
          ))
      }
    }

    remove_levels <- purrr::map(dplyr::select(data, !!!col_vars[lv1]), ~ c(levels(.)[-1], "NA"))
  }



  ctx$settings$cols$lvs <- lvs
  ctx$settings$cols$lv1 <- lv1
  ctx$settings$rows$na_num <- purrr::map_chr(na_num, 1L)
  ctx$settings$pairs$na <- unlist(
    purrr::map2(na_text, na_num, function(nt, nn) {
      v <- rep(nn[[1]], length(col_vars_text))
      v[col_vars_text] <- nt
      v
    }), use.names = FALSE)
  ctx_update(ctx, list(
    data = data,
    remove_levels = if (any(lv1)) remove_levels else NULL
  ))
}


# === STAGE 3/5: tab_aggregate() -- the tier-1 count / moment-sum aggregates ==================
#' @keywords internal
#' @noRd
tab_aggregate <- function(ctx) {
  if (!is.null(ctx$cache_env)) return(jmv_cache_aggregate(ctx))

  list2env(ctx, environment())
  list2env(ctx_settings_locals(ctx), environment())   # col_vars_num / na_num, from the spine
  .by_table <- by_table

  # The numeric tier-1 is HOISTED out of tab_num()'s pmap, so it is a first-class cache object.
  # WARNING: NEVER fused across row_vars -- a shared scan cannot reproduce a per-row_var na.omit(),
  #   and would change the float summation order.
  fine_num <- NULL
  if (sum(col_vars_num) != 0) {
    fine_num <- if (.by_table) {
      rep(list(NULL), length(row_vars))
    } else {
      purrr::map2(row_vars, na_num, ~ tab_aggregate_num(
        data, !!.x,
        as.character(col_vars)[col_vars_num],
        as.character(tab_vars),
        wt = !!wt, na = .y
      ))
    }
    fine_num <- purrr::set_names(fine_num, as.character(row_vars))
  }

  # Factor tier-1: NONE on the tab() path -- the `.fine` / fine_for_pair() seam in tab_plain() is
  # EXCLUSIVELY the jamovi cache seam. DESIGN: `fine_fused = NULL` is kept as an EXPLICIT ctx element
  # (ctx_update()'s single-bracket assignment), so tab_transform()'s list2env() finds the key.
  ctx_update(ctx, list(fine_num = fine_num, fine_fused = NULL))
}


#' @keywords internal
#' @noRd
fine_for_pair <- function(fine, row_var, col_var) {
  if (is.null(fine) || data.table::is.data.table(fine)) return(fine)
  fine[[paste(as.character(row_var), as.character(col_var), sep = "\r")]]
}


# === STAGE 4/5: tab_transform() -- pct/diff/ratio/or/CI + fmt + the tier-2 test =============
# Aggregate -> the per-cell fmt fields AND the whole-table test, both from the UNCHANGED leaves.
# The leaf owns the test, which is what makes the ordering invariant STRUCTURAL: it necessarily
# sees the FULL level set, because it runs before the non-first-level drop can exist.
#' @keywords internal
#' @noRd
tab_transform <- function(ctx) {
  list2env(ctx, environment())
  list2env(ctx_settings_locals(ctx), environment())
  .by_table <- by_table
  .fine     <- fine_fused
  row_var   <- as.character(row_vars)                 # this ctx describes exactly ONE row_var
  rv        <- rlang::sym(row_var)
  wt_sym    <- if (length(wt) == 0L) wt else rlang::sym(as.character(wt))

  if (is.null(pct_vect))  pct_vect  <- rep(pct , length(col_vars))
  if (is.null(ref_vect))  ref_vect  <- rep(ref , length(col_vars))
  if (is.null(ref2_vect)) ref2_vect <- rep(ref2, length(col_vars))
  cached_test <- if (is.null(cached_tests)) NULL else cached_tests[[row_var]]

  robust_tests <- NULL
  if (!identical(inference$basis, "n") && isTRUE(chi2)) {
    robust_tests <- svy_omnibus_grid(
      data, row_var, as.character(col_vars),
      stats::setNames(as.logical(col_vars_num), as.character(col_vars)),
      as.character(tab_vars), wt, inference$basis, inference$design, comp[1],
      totaltab_name = if (identical(totaltab, "table")) totaltab_name else NULL)
  }

  tv_syms <- rlang::syms(as.character(tab_vars))

  tabs_num <- NULL
  chi2_num <- NULL
  if (sum(col_vars_num) != 0) {
    num_col_syms <- rlang::syms(as.character(col_vars)[col_vars_num])
    num_digits   <- vctrs::vec_recycle(vctrs::vec_cast(digits[col_vars_num], integer()),
                                       length(num_col_syms))
    total_names2 <- vctrs::vec_recycle(total_names, 2)
    ref_num_vec <- unlist(ref_vect, use.names = FALSE)[col_vars_num]
    ref_num     <- if (length(ref_num_vec)) ref_num_vec[[1]] else ref
    if (length(unique(ref_num_vec)) > 1L)
      cli::cli_inform(c("i" = paste0("Several numeric col_vars with different references: the first ",
                                     "({.val {ref_num}}) applies to all mean columns.")))
    color_num <- if (identical(color, "auto") || measure_applies(color, "num")) color else "no"
    r_num <- num_resolve(color_num, ref_num, ci, dplyr::if_else(totrow, "row", "no"),
                         comp[1], totaltab, rv, num_col_syms, tv_syms)
    tabs_num <- num_core(
      data, rv, num_col_syms, tv_syms, wt_sym,
      color = r_num$color, na = na_num[1], ref = r_num$ref, comp = r_num$comp, ci = r_num$ci,
      ci_visible = r_num$ci_visible, stars = stars,
      ci_scale = ci_scale[1], totaltab = r_num$totaltab, totaltab_name = totaltab_name,
      tot = r_num$tot, total_names = total_names2, subtext = "", digits = num_digits,
      num = FALSE, df = FALSE, .fine = fine_num, .by_table = .by_table,
      inference = inference
    )
    if (isTRUE(chi2)) chi2_num <- leaf_chi2_num(tabs_num, comp, rv, num_col_syms, tv_syms)
  }

  tabs_text <- NULL
  tests     <- chi2   # logical placeholder; assemble's is.logical() fallback handles a numeric-only tab
  if (sum(col_vars_text) != 0) {
    want_ctr  <- identical(measure_builds(color), "contrib")
    test_leaf <- if (!isTRUE(chi2)) "no"
                 else if (!is.null(cached_test) && !want_ctr) "no"
                 else if (want_ctr) "ctr" else "p"
    text <- purrr::pmap(
      list(col_vars[col_vars_text], digits[col_vars_text], na_text,
           pct_vect[col_vars_text], ref_vect[col_vars_text], ref2_vect[col_vars_text],
           lv1[col_vars_text]),
      function(.col_var, .digits, .na, .pct, .ref, .ref2, .lv1) {
        color_leaf <- if (want_ctr) "no" else color
        r_pl <- plain_resolve(.pct, .ref, .ref2, .na, totaltab_name, total_names,
                              c("row", "col"), comp, color_leaf, .digits, totaltab, tv_syms,
                              comparison = comparison)
        plain_core(
          data, rv, .col_var, tv_syms, wt_sym,
          pct = r_pl$pct, color = color_leaf, na = r_pl$na, ref = r_pl$ref,
          ref2 = r_pl$ref2, comp = r_pl$comp, totaltab = r_pl$totaltab, totaltab_name = totaltab_name,
          tot = r_pl$tot, total_names = r_pl$total_names, subtext = "", digits = r_pl$digits,
          num = FALSE, df = FALSE, stars = stars,
          comparison = comparison, or_ci = or_ci, dichotomise = isTRUE(.lv1),
          ci = ci, ci_scale = ci_scale[1], test = test_leaf, deff = robust_tests,
          color_signif = color_signif, .fine = fine_for_pair(.fine, row_var, .col_var),
          .by_table = .by_table, inference = inference
        )
      }
    ) |> purrr::set_names(as.character(col_vars[col_vars_text]))

    lvl_names <- text |>
      purrr::map(~ purrr::discard(names(.), names(.) %in% c(row_var, as.character(tab_vars)))) |>
      purrr::flatten_chr()
    duplicated_levels <- unique(lvl_names[duplicated(lvl_names)])
    if (length(duplicated_levels) != 0) {
      text <- purrr::imap(text, ~ dplyr::rename_with(.x, function(.names)
        dplyr::if_else(.names %in% duplicated_levels, paste0(.names, "_", .y), .names)))
    }

    leaf_tests <- purrr::map(text, get_test) |> purrr::compact()
    text       <- purrr::map(text, ~ set_test(.x, NULL))

    tabs_text <- purrr::reduce(text, dplyr::full_join, by = c(as.character(tab_vars), row_var))

    tests <- if (!isTRUE(chi2)) chi2
             else if (!is.null(cached_test) && !want_ctr) cached_test           # tier-2 hit
             else if (length(leaf_tests) == 0L) new_test_tibble()
             else {
               tt <- vctrs::vec_rbind(!!!leaf_tests)
               if (nrow(tt) == 0L) new_test_tibble() else
                 dplyr::arrange(tt, dplyr::across(tidyselect::any_of(
                   c(as.character(tab_vars), "col", "test"))))
             }
    if (isTRUE(chi2)) tabs_text <- set_test(tabs_text, tests)
  }

  ctx_update(ctx, list(
    tabs_text = tabs_text, tabs_num = tabs_num, tests = tests, chi2_num = chi2_num,
    robust_tests = robust_tests
  ))
}


# === STAGE 5/5: tab_assemble() -- join, totals, wrap, output shape, render prep (tier 4) ====
# Built tables -> the final tab or list, in two halves: tab_assemble_tables() finishes ONE row_var
# (the same table alone or inside an integrated build), tab_assemble_output() the cross-row_var shape.

#' @keywords internal
#' @noRd
tab_assemble_tables <- function(ctx) {
  list2env(ctx, environment())
  list2env(ctx_settings_locals(ctx), environment())   # lv1 / col_vars_* / totrow / ref
  row_var <- as.character(row_vars)

  if (sum(col_vars_text) != 0) {

    if (any(lv1)) {
      rm_levels <- purrr::imap(remove_levels, ~ c(.x, paste0(.x, "_", .y))) |> purrr::flatten_chr()
      tabs_text <- dplyr::select(tabs_text, -tidyselect::any_of(rm_levels))
    }


    if (tot_cols_type == "no_delete")
      tabs_text <- dplyr::select(tabs_text, -where(is_totcol))
    if (tot_cols_type == "one")
      tabs_text <- dplyr::select(tabs_text, -(where(~ is_totcol(.) & !get_col_var(.) %in% totcol)))

    # A lone total column is renamed "Total" with no col_var name; a genuinely multi-total table keeps
    # the qualified names. WARNING: found through the STORED `totcol` flag, never a regex built from
    #   `total_names[2]` -- that string is the USER's, so "Total (n)" or "Ensemble." would BE a regex.
    totnames <- unique(names(tabs_text)[purrr::map_lgl(tabs_text, ~ is_fmt(.) && is_totcol(.))])
    if (length(totnames) == 1)
      tabs_text <- dplyr::rename(
        tabs_text,
        tidyselect::any_of(purrr::set_names(totnames, rep(total_names[2], length(totnames)))))
  }

  if (sum(col_vars_num) != 0 & sum(col_vars_text) != 0) {
    tab <- dplyr::full_join(tabs_text, tabs_num, by = c(as.character(tab_vars), row_var))

    col_vars_order <- tab |>
      purrr::map(~ purrr::map(get_col_var(.), ~ which(as.character(col_vars) == .))) |>
      purrr::flatten()
    col_vars_order <- col_vars_order |>
      purrr::map_if(names(col_vars_order) %in% tab_row_names, ~ 0L) |>
      purrr::map_int(~ if (length(.) == 0) length(col_vars) + 1L else .) |>
      sort() |> names()

    tab <- dplyr::select(tab, tidyselect::any_of(col_vars_order))

  } else if (sum(col_vars_num) != 0) {
    tab <- tabs_num
  } else {
    tab <- tabs_text
  }

  no_totrow <- (totrow == FALSE)
  if (no_totrow) {
    totrows     <- is_totrow(tab)
    tottab_rows <- is_tottab(tab)
    tottab_line <- length(tottab_rows[tottab_rows]) == 1 & tottab_rows
    tab <- tab |>
      tibble::add_column(totrows = totrows, tottab_line = tottab_line) |>
      dplyr::filter(!.data$totrows | .data$tottab_line) |>
      dplyr::select(-"totrows", -"tottab_line")
  }

  if (is.logical(tests)) tests <- new_test_tibble()
  if (!is.null(chi2_num)) tests <- dplyr::bind_rows(tests, chi2_num)

  if (!is.null(robust_tests) && nrow(tests) > 0) {
    tests <- tab_robust_overlay(tests, robust_tests, as.character(tab_vars))
  }

  # store the add_n / add_pct DISPLAY intent.
  # WARNING: they only make sense beside a col_var, since they fold the base `n` INTO the crosstab. On
  #   a no-col_var table those columns ARE the content -- else the fold would drop the real `n`.
  fmt_here        <- purrr::map_lgl(tab, is_fmt)
  has_real_colvar <- any(fmt_here & is_real_col_var(get_col_var(tab)))
  render_extras <- list(add_n  = isTRUE(add_n)  && has_real_colvar,
                        add_pct = isTRUE(add_pct) && has_real_colvar)
  if (isTRUE(common_totrow)) {
    render_extras$common_totrow     <- TRUE
    render_extras$common_totrow_ref <- any(ref == "tot")
  }
  if (!is.null(anova)) render_extras$anova <- as.character(anova)[[1]]
  vars_attr <- new_vars_attr(
    wt = if (length(wt) == 0L) NA_character_ else as.character(wt)[1],
    var_labels = var_labels)
  meta <- list(render_extras = render_extras, spec = new_spec("crosstab", vars = vars_attr))
  # WARNING: project the call's confidence LEVEL onto every fmt column. The colour engine is per
  #   COLUMN and never sees the table, so a table built at conf_level = 0.99 would otherwise print
  #   99% intervals while greying at 95%. The level ONLY: each core stamps its own df and basis.
  tab <- tab_stamp_inference(tab, inference$conf_level)
  if (!lv1_group_vars(tab)) {
    tab    <- dplyr::group_by(tab, !!!tab_vars)
    groups <- dplyr::group_data(tab)
    tab    <- new_grouped_tab(tab, groups = groups, subtext = subtext, test = tests, meta = meta)
  } else {
    tab <- new_tab(tab, subtext = subtext, test = tests, meta = meta)
  }

  ctx_update(ctx, list(tabs = tab, tests = tests))
}

#' @keywords internal
#' @noRd
tab_assemble_output <- function(ctx) {
  list2env(ctx, environment())

  merge_now <- output == "single"
  if (merge_now &
      !(is.list(tabs) & !is.data.frame(tabs) & length(tabs) == 1 ) ) {
    tabs <- tabs |> tab_compact() # pvalue_lines = FALSE
  }



  if (length(n_min) > 0 && any(n_min > 0, na.rm = TRUE)) {
    tabs <- if (is.data.frame(tabs)) tab_apply_n_min(tabs, n_min)
            else purrr::map(tabs, tab_apply_n_min, n_min = n_min)
  }


  if (length(spread_vars) != 0) {
    .spread_one <- function(t) {
      if (is.null(names_prefix)) {
        tab_spread(t, spread_vars = tidyselect::all_of(spread_vars),
                   names_sort = names_sort, totname = total_names[1])
      } else {
        tab_spread(t, spread_vars = tidyselect::all_of(spread_vars),
                   names_prefix = names_prefix, names_sort = names_sort,
                   totname = total_names[1])
      }
    }
    tabs <- if (is.data.frame(tabs)) .spread_one(tabs) else purrr::map(tabs, .spread_one)
  }

  if (output != "list" &
      is.list(tabs) & !is.data.frame(tabs) & length(tabs) == 1) tabs <- tabs[[1]]

  tabs
}














# === SECTION: the spread / transpose reshapes ==============================================

#' Spread a tab, passing a tab variable to column
#'
#' @param tabs A \code{tibble} of class \code{tab}, made with \code{\link{tab}},
#' \code{\link{tab_reg}} or \code{\link{tab_plain}}.
#' @param spread_vars <\link[tidyr:tidyr_tidy_select]{tidy-select}>  The tab variables
#' to pass to column, with a syntax of type \code{c(var1, var2, ...)}.
#' @param names_prefix String added to the start of every variable name.
#' @param names_sort If no \code{names_prefix} is given, new names takes the form
#'  \code{spread_var}_\code{col_var_level}. Should then the column names be sorted ?
#'  If \code{FALSE}, the default, column names are ordered by first appearance.
#' @param totname The new name of the total rows, as a single string.
#'
#' @return A \code{tibble} of class \code{tab}, with less rows and more columns.
#' @export
#'
#' @examples
#' \donttest{ data <- forcats::gss_cat |> dplyr::filter(year %in% c(2000, 2014))
#'
#' tabs <-
#'   tab(data, relig, marital, c(year, race), pct = "row", totaltab = "no",
#'       color = "difference", tot = "row", other_if_less_than = 30)
#'
#' tabs |>
#'   dplyr::select(year, race, relig, Married) |>
#'   tab_spread(race)
#'   }
tab_spread <- function(tabs, spread_vars, names_prefix, names_sort = FALSE,
                       totname = "Total" #, recalculate = TRUE
) {
  spread_vars     <- rlang::enquo(spread_vars)
  pos_spread_vars <- tidyselect::eval_select(spread_vars, tabs)
  spread_vars     <- names(pos_spread_vars)
  NA_spread_vars  <- purrr::map_lgl(spread_vars,
                                    ~ as.character(.) %in% c("NA", "NULL", "no"))
  if (all(NA_spread_vars) ) return(tabs)

  subtext <- get_subtext(tabs)
  test    <- get_test(tabs)
  # WARNING: capture `meta` HERE, while `tabs` is still a tab -- pivot_wider() returns a plain tibble
  #   with no table attributes, and a fresh `meta = list(...)` literal loses every sub-field.
  meta_in  <- get_meta(tabs)

  get_vars   <- tab_get_vars(tabs)
  col_levels <- get_vars$col_vars_levels |> purrr::flatten_chr()
  row_var    <- get_vars$row_var
  tab_vars   <- get_vars$tab_vars
  tab_vars_new <- tab_vars[!tab_vars %in% spread_vars]
  # WARNING: captured BEFORE the pivot -- the last moment the spread variables exist as columns. Every
  #   new column name ends with one of these levels, which is how spread_relabel() pairs them back.
  spread_levels <- unique(unlist(lapply(
    spread_vars, function(v) as.character(unique(dplyr::pull(dplyr::ungroup(tabs), v))))))
  spread_levels <- spread_levels[!is.na(spread_levels) & nzchar(spread_levels)]
  spec_out <- get_spec(tabs)

  na_values <- purrr::map(dplyr::ungroup(tabs)[col_levels],
                          ~ fmt0(scale = get_scale(.x), display = get_display(.x[1]))) |>
    purrr::set_names(col_levels)


  totrows <- is_totrow(tabs)
  if (any(totrows)) {
    tabs <- tabs |> dplyr::group_by(!!!rlang::syms(tab_vars))
    groups <- dplyr::group_vars(tabs)

    tottab_rows <- is_tottab(tabs)
    tottab_line <- length(tottab_rows[tottab_rows]) == 1 & tottab_rows & totrows

    tabs <- tabs |> tibble::add_column(totrows, tottab_rows, tottab_line)

    if (length(tab_vars_new) != 0 & any(tottab_rows)) {
      tabs <- tabs |> dplyr::filter(!tottab_line)
    }

    new_levels <- tabs |>
      dplyr::filter(.data$totrows & !.data$tottab_line) |>
      dplyr::select(!!!tab_vars, !!row_var) |>
      dplyr::arrange(!!!rlang::syms(tab_vars_new), .by_group = FALSE,
                     .by_totals = FALSE, .only_main_display = FALSE) |>
      dplyr::mutate(
        new_levels = paste(totname, paste(!!!rlang::syms(tab_vars_new), sep = " / ")) |>
          stringi::stri_trans_toupper()
      )
    new_levels <- purrr::set_names(as.character(dplyr::pull(new_levels, row_var)),
                                   new_levels$new_levels)


    tabs <- tabs |> dplyr::mutate(
      !!rlang::sym(row_var) := forcats::fct_recode(!!rlang::sym(row_var),
                                                   !!!new_levels) |>
        forcats::fct_relevel(unique(names(new_levels)), after = Inf)
    ) |>
      dplyr::select(-"totrows", -"tottab_rows", -"tottab_line")
  }

  if ( !missing(names_prefix) ) {
    tabs <- tidyr::pivot_wider(tabs,
                               names_from   = tidyselect::all_of(spread_vars),
                               values_from  = tidyselect::all_of(col_levels),
                               names_prefix = names_prefix,
                               values_fill  = na_values,
                               names_sort   = names_sort
    )
  } else {
    tabs <- tidyr::pivot_wider(tabs,
                               names_from   = tidyselect::all_of(spread_vars),
                               values_from  = tidyselect::all_of(col_levels),
                               values_fill  = na_values,
                               names_sort   = names_sort
    )
  }

  tabs <- tabs |>
    dplyr::arrange(!!!rlang::syms(tab_vars_new), !!rlang::sym(row_var),
                   .only_main_display = FALSE)

  tabs <- complete_partial_totals(tabs)

  spread <- spread_relabel(tabs, spread_vars, spread_levels, test, get_vars$col_vars)
  tabs   <- spread$tabs ; test <- spread$test

  meta_out <- tab_meta_merge(list(meta_in), spec = spec_out)

  if (lv1_group_vars(tabs)) {
    new_tab(tabs, subtext = subtext, test = test, meta = meta_out)
  } else {

    group_dat <- dplyr::group_data(tabs)
    new_grouped_tab(tabs, groups = group_dat, subtext = subtext, test = test, meta = meta_out)
  }

}


# spread_relabel() -- THE post-spread repair, for both producers. pivot_wider() moves the data and
# nothing else, so two facts go stale: a new column's `col_var` still names the ORIGINAL column
# variable (which level of the spread variable it belongs to is stored beside it, in `col_group` --
# the pair is the block identity), and a `test` row keyed on the spread variable now points at a
# set of COLUMNS, not a ROW group. Matching is by column NAME, so the longest matching level wins.
# WARNING: `test$col` holds TWO kinds of entity, hence the discriminator: a crosstab row names a
#   COL_VAR, a regression row names a COLUMN. One rule, two lookups.
#' @keywords internal
#' @noRd
spread_relabel <- function(tabs, spread_vars, spread_levels, test, col_vars = character(0)) {
  if (length(spread_levels) == 0L) return(list(tabs = tabs, test = test))
  col_of_group <- stats::setNames(rep(NA_character_, length(spread_levels)), spread_levels)
  for (nm in names(tabs)[vapply(tabs, is_fmt, logical(1))]) {
    hits <- spread_levels[vapply(spread_levels,
                                 function(g) nm == g || endsWith(nm, paste0("_", g)), logical(1))]
    if (!length(hits)) next
    g <- hits[which.max(nchar(hits))]
    tabs[[nm]] <- set_col_group(tabs[[nm]], g)
    if (fmt_has_role(tabs[[nm]], "n")) next
    if (is.na(col_of_group[[g]])) col_of_group[[g]] <- nm
  }

  if (!is.null(test) && nrow(test) > 0) {
    for (sv in spread_vars) {
      key <- test_key_col(test, sv)
      known <- which(key %in% spread_levels)
      if (!length(known)) next
      lv   <- key[known]
      old  <- test_key_col(test, "col")[known]
      is_cv <- old %in% col_vars
      newc <- ifelse(is_cv, old, unname(col_of_group[lv]))
      newg <- ifelse(is_cv, lv,  "")
      test$col[known[!is.na(newc)]] <- newc[!is.na(newc)]
      test$col_group <- test_key_col(test, "col_group")
      test$col_group[known[!is.na(newc)]] <- newg[!is.na(newc)]
      # WARNING: a tab_var key column is a FACTOR, and `[<-` on a factor with an unknown level gives
      # NA plus a warning. Blank it as character.
      test[[sv]] <- test_key_col(test, sv)
      test[[sv]][known] <- ""
      if (anyNA(newc)) test <- test[-known[is.na(newc)], , drop = FALSE]
    }
  }
  list(tabs = tabs, test = test)
}


#' Transpose a cross-table (swap its rows and columns)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `tab_transpose()` is **soft-deprecated** since tabxplor 2.0.0. It flips the *object* (the
#' `tabxplor_fmt` fields), which cannot carry a transposed column's mixed cell types, so a table with
#' several row variables or numeric columns transposes incorrectly (numeric cells mis-coloured,
#' duplicated total columns). Use the exporters' `transpose = TRUE` argument instead --- it flips the
#' finished render model after colours are computed, and handles several row variables and numeric
#' columns:
#'
#' ```r
#' tab(data, row_vars, col_vars, pct = "row") |> tab_kable(transpose = TRUE)   # or tab_md() / tab_xl()
#' ```
#'
#' The function is kept (unchanged) for the single-row-variable round-trip it always supported.
#'
#' @param tabs A single table made with \code{\link{tab}} (one row variable, one column variable; not
#'   a subtabled table with `tab_vars`, and at most one total row and one total column).
#' @param name The name to give the new first (label) column, holding the old column-variable levels.
#'   `NULL` (default) uses the old column-variable name.
#'
#' @return A transposed `tabxplor_tab`.
#' @export
#'
#' @examples
#' \donttest{
#' # build marital x race as row percentages, then display it as race x marital:
#' tab(forcats::gss_cat, marital, race, pct = "row") |>
#'   tab_kable(transpose = TRUE)
#' }
tab_transpose <- function(tabs, name = NULL) {
  lifecycle::deprecate_soft(
    "2.0.0", "tab_transpose()",
    details = 'Use the `transpose = TRUE` argument of tab_kable() / tab_md() / tab_xl() / tab_export().')
  if (!is.data.frame(tabs)) {
    cli::cli_abort("{.arg tabs} must be a {.pkg tabxplor} table.")
  }
  tabs <- dplyr::ungroup(tabs)

  tab_check_shape(tabs, "transpose_object")

  vars    <- tab_get_vars(tabs)
  row_var <- vars$row_var

  fmt_mask <- purrr::map_lgl(tabs, is_fmt)
  fmtc     <- names(tabs)[fmt_mask]
  if (length(fmtc) == 0) {
    cli::cli_abort("{.arg tabs} has no {.pkg tabxplor} formatted columns to transpose.")
  }

  dvars       <- tab_declared_vars(tabs)
  merged      <- isTRUE(dvars$compacted)
  var_col_nm  <- intersect(dvars$var_col, names(tabs))
  src_of      <- function() as.character(tabs[[var_col_nm[[1]]]])
  src_row_var <- NULL
  if (merged) {
    src   <- src_of()
    lvl   <- as.character(tabs[[row_var]])
    dup   <- lvl %in% names(which(tapply(src, lvl, function(s) length(unique(s))) > 1))
    key   <- ifelse(dup, paste0(lvl, "_", src), lvl)
    if (anyDuplicated(key)) {
      cli::cli_abort(c("{.fn tab_transpose} cannot name the transposed columns uniquely.",
                       "i" = "Two rows share the same variable and level."))
    }
    src_row_var <- stats::setNames(src, key)
    tabs[[".tx_key"]] <- factor(key, levels = key)
    row_var <- ".tx_key"
  }

  # --- capture the axis roles BEFORE the pivot (row_kind / in_refrow are uniform across fmt cols) ---
  totrow_lgl   <- is_totrow(tabs[[fmtc[1]]])
  refrow_lgl   <- vctrs::field(tabs[[fmtc[1]]], "in_refrow")
  totcol_names <- fmtc[purrr::map_lgl(tabs[fmtc], is_totcol)]
  refcol_names <- fmtc[purrr::map_lgl(tabs[fmtc], is_refcol)]
  # WARNING: base `[[`, NOT dplyr::pull(all_of(row_var)) -- tidyselect evaluates `row_var` in the DATA
  #   MASK first, and a merged table has a column literally NAMED `row_var`.
  labels        <- as.character(tabs[[row_var]])
  totrow_labels <- labels[totrow_lgl]
  refrow_labels <- labels[refrow_lgl]
  max_per_sub <- function(x) if (merged) max(c(0L, table(src_of()[x])))
                             else sum(x)
  if (max_per_sub(totrow_lgl) > 1) {
    cli::cli_abort("{.fn tab_transpose} does not work (yet) with more than one total row.")
  }
  if (length(totcol_names) > 1) {
    cli::cli_abort("{.fn tab_transpose} does not work (yet) with more than one total column.")
  }

  real_col_vars <- vars$col_vars[is_real_col_var(vars$col_vars)]
  old_col_var <- if (length(real_col_vars) > 0) real_col_vars[[1]] else NA_character_
  rep_name <- fmtc[purrr::map_lgl(tabs[fmtc], ~ identical(get_col_var(.), old_col_var))]
  rep_name <- if (length(rep_name) > 0) rep_name[[1]] else fmtc[[1]]
  rep_attrs <- purrr::set_names(
    lapply(fmt_col_attrs, function(a) attr(tabs[[rep_name]], a, exact = TRUE)), fmt_col_attrs)
  old_base <- if (is.null(rep_attrs$pct_type)) "row" else rep_attrs$pct_type
  new_base <- switch(old_base, row = "col", col = "row", old_base)

  if (is.null(name)) name <- if (!is.na(old_col_var)) old_col_var else "variables"
  if (merged) tabs <- tabs[, setdiff(names(tabs), c("row_var", "levels")), drop = FALSE]
  long <- tabs |>
    tidyr::pivot_longer(cols = tidyselect::all_of(fmtc),
                        names_to = name, values_to = "value")
  long[[name]] <- factor(long[[name]], levels = fmtc)          # keep the col_var-level order as rows
  wide <- long |>
    tidyr::pivot_wider(names_from = tidyselect::all_of(row_var),
                       values_from = "value", names_sort = FALSE)

  new_fmtc   <- setdiff(names(wide), name)                     # = the old row_var levels
  new_labels <- as.character(wide[[name]])                     # = fmtc (the old column names)

  for (nm in new_fmtc) {
    col <- wide[[nm]]
    for (a in fmt_col_attrs) attr(col, a) <- rep_attrs[[a]]    # restore uniform col_var attributes
    col <- set_pct_type(col, new_base)                        # row % <-> col %
    col <- set_col_var(col, if (merged) unname(src_row_var[[nm]]) else row_var)
    col <- as_totcol(col, FALSE)
    col <- as_refcol(col, FALSE)
    col <- as_totrow(col, new_labels %in% totcol_names)       # old total COLUMN -> new total ROW
    col <- as_refrow(col, new_labels %in% refcol_names)       # old reference COLUMN -> new ref ROW
    wide[[nm]] <- col
  }
  for (lab in intersect(totrow_labels, new_fmtc)) {
    wide[[lab]] <- as_totcol(wide[[lab]], TRUE)
  }
  ref_targets <- if (length(refrow_labels) >= 1) refrow_labels else totrow_labels
  for (lab in intersect(ref_targets, new_fmtc)) {
    wide[[lab]] <- as_refcol(wide[[lab]], TRUE)
  }

  wide[[name]] <- new_lvl(factor(new_labels, levels = new_labels), "level", name)

  test <- get_test(tabs)
  if (is.data.frame(test) && nrow(test) > 0) {
    rv <- test[["var"]]; cv <- test[["col"]]
    test[["var"]] <- cv
    test[["col"]] <- rv
  }

  attrs <- tab_attrs(tabs)
  attrs$test <- test
  attrs$meta <- tab_meta_merge(
    list(attrs$meta),                                    # the weight survives the transpose
    spec = new_spec("crosstab", vars = new_vars_attr(wt = get_vars_attr(tabs)$wt)))
  rlang::exec(new_tab, wide, !!!attrs)
}





# === SECTION: the variable-model readers ===================================================

# The variable roles come from the COLUMNS that declare them (tab_declared_vars(), R/row-model.R),
# never a stored triple a dplyr chain could leave stale. CONTRACT: `row_var` / `tab_vars` are
# COLUMN names, NOT source variable names -- on a merged table those differ.


tab_last_factor_row_var <- function(fct_names, groups = character(0)) {
  non_group <- setdiff(fct_names, groups)
  if (!length(groups) || !length(non_group)) utils::tail(fct_names, 1L)
  else                                        utils::tail(non_group, 1L)
}


#' The variables of a tabxplor table
#' @description
#' Which variable plays which role in a finished table: the row variable, the column variable(s) and
#' the sub-table variable(s). Read off the table's own declared model (the index columns' stored
#' roles and the `fmt` columns' `col_var`), never guessed from a column name --- so it survives
#' renaming, `dplyr` verbs and a merge of several row variables.
#' @param tabs A \code{tibble} of class \code{tab}, made with \code{\link{tab}},
#' \code{\link{tab_reg}} or \code{\link{tab_plain}}.
#' @param vars A character vector naming the roles you want:
#' \code{"row_var"}, \code{"col_vars"} or \code{"tab_vars"}.
#'
#' @return A list with the variables names.
#' @seealso [tab_shape()], which reports the table's SHAPE (merged / grouped / list) and which
#'   operations accept it.
#' @export
#'
tab_get_vars <- function(tabs, vars = c("row_var", "col_vars", "tab_vars")) {
  stopifnot(is.data.frame(tabs))
  rec <- tab_declared_vars(tabs)

  if ("col_vars" %in% vars) {
    fmtc <- purrr::map_lgl(tabs, is_fmt)
    col_vars       <- get_col_var(tabs[fmtc]) |> purrr::discard(~ is.na(.))
    col_vars_names <- col_vars |> unique()

    col_vars_levels <-
      purrr::map(col_vars_names, ~ names(col_vars[col_vars == .])) |>
      purrr::set_names(col_vars_names)

    col_vars <- col_vars_names
  }

  fct_cols <- purrr::map_lgl(tabs, is.factor)

  if ("row_var" %in% vars)
    row_var <- if (!is.null(rec)) rec$row_var
               else tab_last_factor_row_var(names(fct_cols)[fct_cols])

  if ("tab_vars" %in% vars) tab_vars <-
    if (!is.null(rec))            rec$tab_vars
    else if (length(row_var) == 0) names(fct_cols[fct_cols])
    else names(fct_cols[fct_cols & names(fct_cols) != row_var])



  ls(pattern = "^row_var$|^col_vars$|^col_vars_levels$|^tab_vars$") |>
    purrr::set_names() |>
    purrr::map(~ rlang::sym(.) |> rlang::eval_tidy())
}


#' @keywords internal
tab_row_roles <- function(tab) {
  n <- nrow(tab)
  kinds <- fmt_row_kind(tab)
  if (length(kinds) == n) return(kinds)
  rep("data", n)
}

# The ROBUST render-time variable detector: it degrades instead of letting a consumer crash.
# DESIGN: row_var / tab_vars are placed from dplyr::group_vars(), which survives rename / select /
#   relocate, so a factor moved AFTER the fmt columns is not miswritten.
tab_render_vars <- function(tabs) {
  if (!is.data.frame(tabs))
    return(list(degrade = TRUE, reason = "the object is not a data frame"))

  fmt_mask <- purrr::map_lgl(tabs, is_fmt)
  if (!any(fmt_mask))
    return(list(degrade = TRUE,
                reason = "the table has no tabxplor_fmt columns (not a tabxplor table)"))

  fct_names <- names(tabs)[purrr::map_lgl(tabs, is.factor)]
  if (length(fct_names) == 0)
    return(list(degrade = TRUE,
                reason = "the table has no factor column to use as the row variable"))

  col_vars <- get_col_var(tabs[fmt_mask]) |> purrr::discard(~ is.na(.))
  col_vars_names  <- unique(col_vars)
  col_vars_levels <- purrr::map(col_vars_names, ~ names(col_vars[col_vars == .])) |>
    purrr::set_names(col_vars_names)

  rec <- tab_declared_vars(tabs)
  if (!is.null(rec)) {
    row_var  <- rec$row_var
    tab_vars <- rec$tab_vars
  } else {
    row_var  <- tab_last_factor_row_var(fct_names, intersect(dplyr::group_vars(tabs), fct_names))
    tab_vars <- setdiff(fct_names, row_var)
  }

  if (length(row_var) == 0 || is.na(row_var) || !row_var %in% fct_names)
    return(list(degrade = TRUE, reason = "could not identify the row variable"))

  list(degrade = FALSE, row_var = row_var, tab_vars = tab_vars,
       row_vars = if (!is.null(rec)) rec$row_vars else row_var,
       compacted = !is.null(rec) && isTRUE(rec$compacted),
       var_col = if (!is.null(rec)) rec$var_col else character(0),
       col_vars = col_vars_names, col_vars_levels = col_vars_levels)
}


#' @keywords internal
tab_degrade_inform <- function(reason) {
  cli::cli_inform(c(
    "!" = "tabxplor formatting and colors skipped: {reason}.",
    "i" = "Rendering the plain table instead."
  ))
}





# === SECTION: labelled-data (haven/labelled) interop =================================

# Convert ONE haven/labelled column to a factor from its value labels, with no haven dependency.
# Converts ONLY when the labels are COMPLETE; otherwise it strips the class, so a coded numeric
# keeps its means path. WARNING: it drops the `label` attribute -- capture variable labels first.
val_labels_to_factor <- function(x) {
  labs <- attr(x, "labels", exact = TRUE)
  if (is.null(labs) || length(labs) == 0L) return(x)

  raw <- x
  attributes(raw) <- NULL                       # bare atomic values, drops labelled/label/class

  observed <- unique(raw[!is.na(raw)])
  if (!all(observed %in% unname(labs))) return(raw)   # incomplete -> underlying numeric/character

  f <- factor(raw, levels = unname(labs), labels = names(labs))
  forcats::fct_drop(f)
}

# Apply val_labels_to_factor() across the labelled columns among `vars`; a no-op when none is.
# WARNING: column access by `[[` (name), never `data[vars]` -- the latter ROW-subsets a data.table.
tab_apply_val_labels <- function(data, vars) {
  vars <- intersect(unique(vars), names(data))
  for (v in vars) {
    if (!is.null(attr(data[[v]], "labels", exact = TRUE)))
      data[[v]] <- val_labels_to_factor(data[[v]])
  }
  data
}

capture_var_labels <- function(data, vars) {
  vars <- intersect(unique(vars), names(data))
  if (length(vars) == 0L) return(character())
  labs <- vapply(vars, function(v) {
    l <- attr(data[[v]], "label", exact = TRUE)
    if (is.null(l) || !nzchar(as.character(l)[[1]])) NA_character_ else as.character(l)[[1]]
  }, character(1))
  names(labs) <- vars
  labs[!is.na(labs)]
}

tab_lump_others <- function(data, vars_not_numeric, other_if_less_than = 0,
                            other_level = "Others") {
  if (other_if_less_than > 0 && length(vars_not_numeric) != 0) {
    data <- data |>
      dplyr::mutate(dplyr::across(
        tidyselect::all_of(vars_not_numeric),
        ~ forcats::fct_lump_min(., other_if_less_than, other_level = other_level)
      ))
  }
  data
}

# Merge chosen factor levels into one, PRE-AGGREGATE (the spec is R/row-model.R's declared level
# operation). DESIGN: a collapse commutes with the aggregate, so it COULD run on the cached counts.
#   It does not, because the design-based n_eff (R/survey-variance.R) reads MICRODATA: doing it here
#   makes the result identical to tab() on a frame the user collapsed himself, and the pct bases,
#   tot_n, n_eff and chi2 all follow with no code. The price is one tier-1 jamovi cache miss.
# WARNING: it runs BEFORE tab_lump_others(), so a merged level's COMBINED count faces
#   `other_if_less_than`, and before tab_cleannames_relabel(), so the spec keys on RAW labels.
# fct_collapse() preserves `ordered`, places the merged level at its FIRST constituent's position,
# and WARNS on an absent level -- so drift is filtered here.
tab_collapse_levels <- function(data, spec) {
  if (length(spec) == 0L) return(data)
  for (v in intersect(names(spec), names(data))) {
    f <- data[[v]]
    if (!is.factor(f)) next                     # a numeric column has no levels to merge
    lv     <- levels(f)
    groups <- lapply(spec[[v]], function(g) g[g %in% lv])
    groups <- groups[lengths(groups) >= 2L]     # level drift: a group down to one level is a no-op
    if (length(groups) == 0L) next
    data[[v]] <- forcats::fct_collapse(f, !!!groups)
  }
  data
}

tab_cleannames_relabel <- function(data, vars_not_numeric) {
  if (length(vars_not_numeric) != 0) data <- data |>
    dplyr::mutate(dplyr::across(
      tidyselect::all_of(vars_not_numeric),
      ~ forcats::fct_relabel(., ~ stringi::stri_replace_all_regex(., cleannames_condition(), ""))
    ))
  data
}

#' Prepare data for \code{\link{tab_plain}}.
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' An internal step of the build, exported before the pipeline had one. Every one of its jobs is
#' now an argument of [tab()] — `na_drop_all` is `filter = !is.na(...)`, and `cleannames`,
#' `other_if_less_than` and `other_level` are formals of [tab()] itself — so calling it by hand
#' prepares data for a function that would prepare it again. It will be made internal in 2.1.0.
#'
#' @param data A dataframe.
#' @param ... Variables then to be passed in \code{\link{tab_plain}}.
#' @param na_drop_all <\link[tidyr:tidyr_tidy_select]{tidy-select}> Removes all
#' observation with a `NA` in any of the chosen variables.
#' @param cleannames Set to \code{TRUE} to clean levels names, by removing
#' prefix numbers like \code{"1-"}, and text in parentheses.
#' @param other_if_less_than When set to a positive integer, levels with less count
#' than it will be merged into an "Others" level.
#' @param other_level The name of the "Other" level, as a character vector of length one.
#' @param levels_collapse A named list, one element per variable, each a named list of
#'   character vectors: the levels to merge, named by the merged level's label (the shape
#'   \code{\link[forcats:fct_collapse]{forcats::fct_collapse}} takes). Applied before
#'   \code{other_if_less_than}. \code{NULL} merges nothing.
#'
#' @return A modified data.frame.
#' @keywords internal
#' @export
#' @examples \donttest{data <- dplyr::starwars |>
#' tab_prepare(sex, hair_color, gender, other_if_less_than = 5,
#'             na_drop_all = sex)
#' data
#' }
tab_prepare <-
  function(data, ..., na_drop_all,
           cleannames = NULL, other_if_less_than = 0,
           other_level = "Others", levels_collapse = NULL) {
    if (tx_user_call()) lifecycle::deprecate_soft("2.0.0", "tab_prepare()", details = paste0(
      "Its work is done by tab() itself: `na_drop_all` is `filter = !is.na(...)`, and ",
      "`cleannames` / `other_if_less_than` / `other_level` are tab() arguments."))

    cleannames <-
      resolve_cleannames(cleannames)

    variables     <- rlang::expr(c(...))
    pos_variables <- tidyselect::eval_select(variables, data)
    variables     <- names(pos_variables)

    if (missing(na_drop_all)) {
      na_drop_all <- character()
    } else{
      na_drop_all <- names(tidyselect::eval_select(rlang::enquo(na_drop_all), data))
    }



    if (length(na_drop_all) != 0) {
      data.table::setDT(data)
      data <- tibble::as_tibble(stats::na.omit(data, na_drop_all))
    }

    data <- data |> tab_apply_val_labels(variables)

    vars_not_numeric <-
      dplyr::select(data[pos_variables], where(~ !is.numeric(.))) |>
      colnames()

    # WARNING (public surface): ordered factors survive the whole pipeline, so a table's grouping
    #   columns come back `ordered`, with "NA" and "Total" / "Ensemble" appended as the GREATEST
    #   levels. They are labels, not points on the scale.
    data <- data |> tab_collapse_levels(levels_collapse)
    data <- data |> tab_lump_others(vars_not_numeric, other_if_less_than, other_level)
    if (cleannames == TRUE) data <- data |> tab_cleannames_relabel(vars_not_numeric)

    # LAST, on the levels that reach the leaf, so a collision a recode above created is caught too. See
    # lvl_check_reserved() (R/row-model.R) for why a source level named "Total" aborts, not warns.
    lvl_check_reserved(data, vars_not_numeric)

    data
  }







# === SECTION: the plain fmt carrier ========================================================
# A carrier COLUMN = list(frame, meta):
#   frame : the per-cell FIELDS, each length nrow and CORRECTLY TYPED -- n / digits integer,
#           in_tottab / in_refrow logical, row_kind / display character, the rest double. new_fmt()
#           does NO casting, so the carrier owns the types.
#   meta  : the per-column ATTRIBUTES (`fmt_col_attrs`). The name ORDER is the new_fmt() contract.

# WARNING: pass `comp_all` by EXACT name, never `comp` -- it PARTIAL-MATCHES the `comp_all` formal.
# fmt_materialize_col() is the ONE new_fmt() call: do.call by exact names, so no partial-match drift.
fmt_materialize_col <- function(frame, meta) do.call(new_fmt, c(frame, meta))

fmt_unwrap <- function(tab) {
  cols <- unclass(tab)                                     # the data columns (fmt + factor), by name
  is_f <- vapply(cols, is_fmt, logical(1))
  fmt  <- lapply(cols[is_f], function(col) list(
    frame = as.list(vctrs::vec_data(col)),
    meta  = purrr::set_names(lapply(fmt_col_attrs, function(a) attr(col, a, exact = TRUE)),
                             fmt_col_attrs)
  ))
  list(is_fmt = is_f, factors = cols[!is_f], fmt = fmt, attrs = attributes(tab))
}

fmt_wrap <- function(carrier) {
  cols <- vector("list", length(carrier$is_fmt))
  names(cols) <- names(carrier$is_fmt)
  cols[!carrier$is_fmt] <- carrier$factors
  cols[ carrier$is_fmt] <- lapply(carrier$fmt, function(cc) fmt_materialize_col(cc$frame, cc$meta))
  attributes(cols) <- carrier$attrs                       # class/names/row.names/subtext/test/groups
  cols
}

fmt_stack_frames <- function(frames, meta) {
  frames   <- unname(frames)                     # else vec_c() takes the list names as outer names
  fields   <- names(frames[[1]])
  combined <- purrr::set_names(
    lapply(fields, function(f) do.call(vctrs::vec_c, lapply(frames, `[[`, f))),
    fields)
  fmt_materialize_col(combined, meta)
}




























# === SECTION: shared leaf and reference helpers ============================================

#' @keywords internal
quo_miss_na_null_empty_no <- function(quo) {
  if (rlang::quo_is_missing(quo)) return (TRUE)
  if (rlang::quo_is_null(quo)) return(TRUE)
  base_quo <- quo
  quo <- rlang::get_expr(quo) |> as.character()
  all(is.na(quo) | quo %in% c("", "no")) |
    (quo[1] %in% c("all_of", "any_of") &
       !is.na(quo[2]) & quo[2] %in% c("", "no", "no_row_var", "no_col_var"))
}


#' @keywords internal
as_df_merge_rownames <- function(tabs, row_var) {
  text_cols <- !purrr::map_lgl(tabs, is.numeric)
  text_cols <- names(text_cols)[which(text_cols)]
  new_rownames  <- paste0(text_cols, collapse = "_")

  if (length(text_cols) >= 2) {
    tabs <- tabs |>
      tibble::as_tibble() |>
      dplyr::mutate(!!new_rownames :=
                      paste(!!!purrr::map(text_cols, rlang::sym), sep = "_")) |>
      dplyr::select(-tidyselect::all_of(text_cols)) |>
      dplyr::relocate(where(is.character), .before = 1) |>
      tibble::column_to_rownames(var = new_rownames)
  } else {
    rnames <- as.character(tabs[[row_var]])
    tabs[, eval(row_var) := NULL]
    data.table::setDF(tabs, rownames = rnames)
  }

  tabs
}


#' @keywords internal
leaf_totrow_tottab <- function(tabs, row_var, tab_vars) {
  # WARNING: `%in%`, not `==`, so an NA row or tab label yields FALSE and never NA -- an NA in
  #   in_totrow / in_tottab poisons is_totrow() and crashes the masked assignments in format().
  totrow_vector <- dplyr::pull(tabs, !!row_var) %in% "Total"
  tottab_vector <- if (length(tab_vars) == 0) {
    rep(FALSE, nrow(tabs))
  } else {
    dplyr::transmute(tabs, tottab = dplyr::if_all(
      tidyselect::all_of(as.character(tab_vars)),
      ~ . %in% "Total"
    )) |>
      tibble::deframe()
  }
  list(totrow = totrow_vector, tottab = tottab_vector,
       kind   = dplyr::if_else(totrow_vector, "total", "data"))
}


#' @keywords internal
leaf_rename_totals <- function(tabs, row_var, tab_vars, tot, total_names, totaltab, totaltab_name,
                               tottab_vector, totrow_vector) {
  # DESIGN: both renames are MASK-ASSIGNMENTS on the expanded factor, not dplyr::if_else(), whose
  #   fresh `true =` branch cannot be combined with an ORDERED input.
  # WARNING: `sort(unique(.))` below is load-bearing, NOT tidying -- the old character branch made
  #   factor() sort the labels alphabetically, and dropping it reorders every grouped table's totals.
  if (totaltab %in% c("line", "table") &  totaltab_name != "Total") {
    tabs <- tabs |> dplyr::mutate(dplyr::across(
      tidyselect::all_of(as.character(tab_vars)),
      ~ {
        z <- forcats::fct_expand(., totaltab_name)
        z[tottab_vector] <- totaltab_name
        forcats::fct_drop(z)
      }
    ))
  }

  if (length(tab_vars) == 0) {

    # WARNING: `!!!`, not `!!` -- forcats::fct_recode() takes its pairs through `...` as NAMED
    #   arguments, so a named vector handed over as ONE positional argument is a hard error.
    if ("row" %in% tot & total_names[1] != "Total") tabs <- tabs |>
        dplyr::mutate(!!row_var := forcats::fct_recode(!!row_var,
                                                       !!!purrr::set_names("Total", total_names[1])))
  } else {
    tabs <- tabs |>
      tidyr::unite(col = "tabs_tot_names", !!!tab_vars, sep = " ", remove = FALSE)
    totrow_labels <- paste(total_names[1], tabs$tabs_tot_names)
    tabs <- tabs |>
      dplyr::mutate(
        !!row_var := {
          z <- forcats::fct_expand(!!row_var, sort(unique(totrow_labels)))
          z[totrow_vector] <- totrow_labels[totrow_vector]
          forcats::fct_drop(z)
        }
      ) |>
      dplyr::select(-"tabs_tot_names")
  }

  if ("col" %in% tot & total_names[2] != "Total") tabs <- tabs |>
    dplyr::rename(tidyselect::any_of(purrr::set_names("Total", total_names[2])))

  tabs
}


# The df= / num= escape hatch: build the NORMAL fmt table, then pull the displayed number per cell.
# WARNING: it takes `num` only -- `df` is the implicit else, so the caller's gate is `if (df || num)`.
#' @keywords internal
leaf_extract_raw <- function(result, num, row_var) {
  fmt_cols <- names(result)[purrr::map_lgl(result, is_fmt)]
  nums <- dplyr::mutate(result, dplyr::across(tidyselect::all_of(fmt_cols), get_num))
  if (num) return(nums)
  out <- as_df_merge_rownames(data.table::as.data.table(nums), rlang::as_name(row_var))
  for (a in c("subtext", "test", "meta")) attr(out, a) <- NULL
  out
}

#' @keywords internal
# WARNING: examine ONLY the col_vars targets, never every column -- a `where()` predicate over all
#   columns coerces a numeric weight column's whole vector to strings, which is very slow.
relabel_levels_in_varnames <- function(data, col_vars) {
  nms      <- names(data)
  col_vars <- intersect(col_vars, nms)
  needs <- purrr::map_lgl(col_vars, function(v) {
    col <- data[[v]]
    (is.factor(col)    && any(levels(col) %in% nms)) ||
      (is.character(col) && any(unique(col) %in% nms))
  })
  targets <- col_vars[needs]
  if (length(targets) == 0) return(data)
  data |>
    dplyr::mutate(dplyr::across(
      tidyselect::all_of(targets),
      ~ forcats::fct_relabel(., ~ dplyr::if_else(. %in% nms, paste0(., "_lv"), .))
    ))
}

#' @keywords internal
diff_index <-  function(ref, row_var, num_names, pct, is_total = FALSE) {
  if (ref == "tot"   ) return(-1L)
  if (ref == "first" ) return(1L )
  if (is.numeric(ref) | !is.na(suppressWarnings(as.integer(ref)))
  ) {
    return(as.integer(ref[1]))
  }

  targets <- switch(pct, "row" = row_var, "col" = num_names)

  # DESIGN: "last" is the mirror of "first", and the only sentinel needing `targets`. ONE meaning on
  #   both axes -- the last LEVEL -- because a total is not a level: `ref = "tot"` names it, and
  #   "last" must not become a synonym. On the col axis `targets` IS the column set, so exclude the
  #   totals and take the last index; on the row axis it is stacked over EVERY sub-table while the
  #   caller compares row_number() within one, so -1L is the sentinel calculate_refrows() resolves.
  #   Falling through to the regex matcher matched nothing, and first(integer(0)) -> replace_na(0)
  #   gave index 0 -- the "no columns were found as reference" warning.
  # WARNING: like "tot" / "first", the sentinel wins over a level LITERALLY named "last" -- select
  #   such a level by its integer index. `is_total` is the leaf's OWN naming, never a user label.
  if (identical(ref, "last")) {
    if (identical(pct, "row")) return(-1L)
    keep <- which(!vctrs::vec_recycle(is_total, length(targets)))
    return(if (length(keep)) max(keep) else length(targets))
  }

  exact <- which(targets == ref)
  index <- if (length(exact) >= 1L) exact else which(stringi::stri_detect_regex(targets, ref))
  if (length(index) >= 2) {
    switch(pct,
           "row" = warning(paste0(
             "with ref = '", ref, "' , several rows were found as ",
             "reference for comparison ; only the first was kept ; ",
             "to remove this warning, precise the value of ref ",
             "until there is only one row_var level matched"
           )),

           "col" = warning(paste0(
             "with ref = '", ref, "' , several columns were found as ",
             "reference for comparison ; only the first was kept ; ",
             "to remove this warning, precise the value of ref ",
             "until there is only one column matched"
           ))
    )
  }
  index <- tidyr::replace_na(dplyr::first(index), 0)

  if (length(index) == 0) index <- 0

  index
}

#' @keywords internal
calculate_refrows <- function(tabs, ref, comp, tab_row_names, tab_vars,
                              row_var, tottab_vector, totrow_vector, # pct,
                              num_names) {
  if (ref != "tot") {
    # WARNING: diff_index() stays INSIDE the transmute. `!!row_var` is tidy-eval, so each grouped call
    #   sees its OWN sub-table's labels; hoisting it out makes `!!row_var` an invalid argument.
    # -1L is `ref = "last"`: the last LEVEL of each sub-table, not its last ROW, hence last_lvl().
    last_lvl <- function(is_tot) {
      keep <- which(!is_tot)
      if (length(keep)) max(keep) else length(is_tot)   # a sub-table of nothing but totals
    }
    refrows <-
      if(comp == "tab") {
        tibble::as_tibble(tabs[, tab_row_names, with = FALSE]) |>
          dplyr::mutate(totrow_vector = totrow_vector) |>
          dplyr::group_by(!!!tab_vars) |>
          dplyr::transmute(
            var =
              dplyr::row_number() == if (diff_index(ref, !!row_var,
                                                    num_names = num_names,
                                                    pct = "row") == -1) {
                last_lvl(.data$totrow_vector)
              } else {
                diff_index(ref, !!row_var, num_names = num_names, pct = "row")
              }
          ) |>
          dplyr::pull("var")

      } else {
        tibble::as_tibble(tabs[, tab_row_names, with = FALSE]) |>
          dplyr::mutate(tottab_vector = tottab_vector, totrow_vector = totrow_vector) |>
          dplyr::group_by(!!!tab_vars) |>
          dplyr::transmute(
            var = dplyr::if_else(
              condition = .data$tottab_vector,
              true  = dplyr::row_number() == if (diff_index(ref, !!row_var,
                                                            num_names = num_names,
                                                            pct = "row") == -1) {
                last_lvl(.data$totrow_vector)
              } else {
                diff_index(ref, !!row_var, num_names = num_names, pct = "row")
              },
              false = FALSE
            )
          ) |>
          dplyr::pull("var")
      }

    if (!any(refrows)) {
      warning(paste0(
        "in ref = '", ref, "' , no rows were found as reference for comparison ; ",
        "to remove this warning, precise the value of ref ",
        "until there is one row_var level matched"
      ))
    }
  } else {
    refrows <- if (comp == "tab") { totrow_vector } else { totrow_vector & tottab_vector }
  }
  refrows <- tidyr::replace_na(refrows, FALSE)

  return(refrows)
}


resolve_ref_vector <- function(ref, row_vars_chr, what = "row_var") {
  n <- length(row_vars_chr)
  if (length(ref) == 1L && is.null(names(ref))) return(vctrs::vec_recycle(ref, n))
  nms <- names(ref)
  if (!is.null(nms) && any(nzchar(nms))) {
    unknown <- setdiff(nms[nzchar(nms)], row_vars_chr)
    if (length(unknown)) {
      cli::cli_warn(paste0(
        "{cli::qty(unknown)}Unknown {.arg ref} name{?s} {.val {unknown}}: ",
        "{cli::qty(unknown)}{?it matches/they match} no {what} and {cli::qty(unknown)}{?is/are} ignored."
      ))
    }
    out  <- rlang::set_names(rep("auto", n), row_vars_chr)
    keep <- intersect(nms, row_vars_chr)
    out[keep] <- as.character(ref[keep])
    unname(out)
  } else {
    vctrs::vec_recycle(ref, n)
  }
}







# --- codetools: the tab_build() ctx fields ----------------------------------------------------
# Every stage starts with list2env(ctx, ...) + list2env(ctx_settings_locals(ctx), ...), binding every
# field as a local: correct at run time, invisible to codetools. DERIVED from the declarations, so
# it cannot go stale the way a hand-mirrored list does.
# WARNING: this call must stay at the END of this file -- new_ctx()'s defaults call
#   conf_level_default(), defined further down, and top-level code runs in source order.
utils::globalVariables(c(names(new_ctx()), CTX_SETTINGS_LOCALS,
                         names(leaf_inference_setup(new_inference(), NULL, FALSE))))
