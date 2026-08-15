# PURPOSE: THE AGGREGATE CORE -- the two leaves that turn microdata into cells, and everything that
#   serves only them. `tab_plain()` (factor col_vars) and `tab_num()` (numeric col_vars) are the
#   public superseded entry points; `plain_core()` / `num_core()` are their resolved-args compute
#   cores, which tab_build() calls directly.
# ROLE: Carved out of R/tab.R by Phase 19l (2582 lines, whole functions, no behaviour change). tab.R
#   had grown to 7918 lines holding four unrelated subsystems; this is the one 2.0.0's keystone made
#   the centre of the package, so it gets its own file.
# KEY CONSTRAINTS:
#   - 17f: WRAPPER/CORE split. The public leaf defuses NSE + validates + normalises colour, a shared
#     `*_resolve()` resolves the arguments, the `*_core()` builds the fmt cells and returns
#     PRE-FINALISE. tab_transform() calls the CORES, so forcing runs once and colour finalises once.
#   - 19j (KEY 5): the leaf computes the cells, THEIR interval (leaf_ci_plain / ci_dispatch) and the
#     whole-table TEST (leaf_chi2 / leaf_chi2_num), because that is where the plan is. There is no
#     second pass; tab_apply_tests() is gone.
#   - 19i: both leaves share one head (leaf_inference_setup) and one tail (leaf_finish).
#   - build_total_rows() and num_rollup() are deliberately NOT merged: base::sum over split() vs
#     data.table gforce is a 1-ULP contract on both sides. See their headers.
# See: CLAUDE.md Repository Map > R/tab-leaf.R; dev/tabxplor_architecture.md.


# DESIGN: tab_plain() is the core aggregation function. Internal sequence:
#   1. data.table dcast (row_var ~ col_var, fun = sum of weights) for speed
#   2. Wrap counts into fmt vectors via new_fmt()
#   3. Add total rows/cols, then chain to tab_pct/tab_ci/tab_chi2 as requested
#   Column names are temporarily prefixed to avoid DT reserved name conflicts.
#' Plain single cross-table
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' One bare cross-table of counts or percentages, from ONE row variable and ONE column variable.
#' Superseded by [tab()], which does the same and everything around it (several variables, colours,
#' totals, tests) -- but it stays the smallest entry point into the aggregate core, and takes the
#' same `ci` / `ci_method` / `conf_level` / `stars` / `display` arguments, resolved by the same
#' rules, so its numbers agree with `tab()`'s cell for cell.
#' @param data A data frame.
#' @param row_var,col_var The row variable, which will be printed with one level per line,
#'  and the column variable, which will be printed with one level per column. Numeric
#'  variables will be used as factors. To calculate means, use \code{\link{tab_num}}.
#' @param tab_vars  <\link[tidyr:tidyr_tidy_select]{tidy-select}> Tab variables :
#' a subtable is made for each combination of levels of the
#' selected variables. Leave empty to make a simple cross-table. All tab variables
#' are converted to factor.
#' @param wt A weight variable, of class numeric. Leave empty for unweighted results.
#' @param digits The number of digits to print, as a single integer, or an integer vector the
#' same length as \code{col_vars}.
#' @param na The policy to adopt with missing values, as a single string.
#'  \itemize{
#'   \item \code{"keep"}: by default, \code{NA}'s of row, col and tab variables
#'   are printed as explicit "NA" level.
#'   \item \code{"drop"}: removes NA of row, col and tab variables.
#'   }
#' @param totaltab The total table,
#' if there are subtables/groups (i.e. when \code{tab_vars} is provided) :
#'  \itemize{
#'   \item \code{"line"}: by default, add a general total line (necessary for
#'   calculations with \code{comp = "all"})
#'   \item \code{"table"}: add a complete total table
#'  (i.e. \code{row_var} by \code{col_vars} without \code{tab_vars}).
#'   \item \code{"no"}: not to draw any total table.
#'  }
#' @param totaltab_name The name of the total table, as a single string.
#' @param tot The totals :
#'  \itemize{
#'   \item \code{c("col", "row")} or \code{"both"} : by default, both total rows and total
#'   columns.
#'   \item \code{"row"}: only total rows.
#'   \item \code{"col"}: only total column.
#'   \item \code{"no"}: remove all totals (after calculations if needed).
#'  }
#' @param total_names The names of the totals, as a character vector of length one or two.
#' Use syntax of type \code{c("Total row", "Total column")} to set different names for
#' rows and cols.
#' @param pct The type of percentages to calculate :
#'  \itemize{
#'   \item \code{"row"}: row percentages.
#'   \item \code{"col"}: column percentages.
#'   \item \code{"all"}: frequencies for each subtable/group, if there is \code{tab_vars}.
#'   \item \code{"all_tabs"}: frequencies for the whole (set of) table(s).
#' }
#' @param ref The reference cell to calculate differences and ratios
#'  (used to print \code{colors}) :
#'  \itemize{
#'   \item \code{"auto"}: by default, cell difference from the corresponding total
#'   (rows or cols depending on \code{pct = "row"} or \code{pct = "col"}) is
#'   used for `diff` ; cell ratio from the first line (or col) is use for `OR`
#'   (odds ratio/relative risks ratio).
#'   \item \code{"tot"}: totals are always used.
#'   \item \code{"first"}: calculate cell difference or ratio from the first cell
#' of the row or column (useful to color temporal developments).
#'   \item \code{"last"}: the mirror of \code{"first"} — the **last level** of the row (or column)
#' variable. A total row or column is not a level and is never selected: use \code{"tot"} for that.
#' Resolved inside each subtable when there are \code{tab_vars}.
#'   \item \code{n}: when `ref` is an integer, the nth row (or column) is used for comparison.
#'   \item \code{"regex"}: when `ref` is a string, it it used as a regular expression,
#'   to match with the names of the rows (or columns). Be precise enough to match only one
#'   column or row, otherwise you get a warning message.
#'   \item \code{"no"}: not use ref and not calculate diffs to gain calculation time.
#' }
#' @param ref2 The second reference level for odds ratios (or relative risk ratios), needed
#' only for a factor with **3 levels or more** (the "OR of each level versus \code{ref2}"). The
#' first level is used by default. For a **binary** factor \code{ref2} is ignored: each level's
#' OR is computed against the *other* level, so both levels show a value (reciprocals of one
#' another) instead of one being forced to \code{1}. See `ref` above for the list of possible values.
#' @param comp Comparison level. When \code{tab_vars} are present, should the
#' contributions to variance be calculated for each subtable/group (by default,
#'  \code{comp = "tab"}) ? Should they be calculated for the whole table
#'  (\code{comp = "all"}) ?
#'  \code{comp} must be set once and for all the first time you use \code{\link{tab_plain}},
#'  \code{\link{tab_num}} or \code{\link{tab_chi2}} with rows, or \code{\link{tab_ci}}.
#' @param subtext A character vector to print rows of legend under the table.
#' @param num Set to \code{TRUE} to obtain a table with normal numeric vectors (not fmt).
#' @param df  Set to \code{TRUE} to obtain a plain data.frame (not a tibble),
#' with normal numeric vectors (not fmt). Useful, for example, to pass the table to
#' correspondence analysis with \pkg{FactoMineR}.
#' @param design_effect See \code{\link{tab}}: whether a \strong{weighted} table's intervals account
#' for the weighting's own design effect. \code{NULL} (default) takes
#' \code{options("tabxplor.design_effect")}.
#' @param display A \code{{}} display template applied to the built table -- the same grammar
#'   \code{\link{tab}} takes (e.g. \code{"{or}"}, \code{"{pct} {ci}"}, \code{"{pct} (n={n})"}),
#'   or the type-adaptive alias \code{"num_ci"}. Display only: it never changes what is computed.
#' @param color_signif How significance interacts with `color`:
#' \code{"ignore"} (default), \code{"grey_non_signif"} or \code{"guaranteed_effect"}.
#' See \code{\link{tab}}.
#' @param .fine,.by_table Internal. `.fine` is a pre-computed count-aggregate to roll up from
#' instead of scanning the raw data (used by \code{\link{tab_counts}} and the scan-fusion path);
#' `.by_table` forces the table-by-table path.
#'
#' @inheritParams tab
#'
#' @return A \code{tibble} of class \code{tabxplor_tab}. If \code{...} (\code{tab_vars})
#'  are provided, a \code{tab} of class \code{tabxplor_grouped_tab}.
#' All non-text columns are \code{\link{fmt}} vectors of class \code{tabxplor_fmt},
#' storing all the data necessary to print formats and colors. Columns with \code{row_var}
#' and \code{tab_vars} are of class \code{factor} : every added \code{factor} will be
#' considered as a \code{tab_vars} and used for grouping. To add text columns without
#' using them in calculations, be sure they are of class \code{character}.
#' @export
#'
#' @examples
#' \donttest{
#' data <- dplyr::starwars |> tab_prepare(sex, hair_color)
#'
#' # the leaf builds the cells AND their intervals (2.0.0): `ci` is resolved here exactly as in
#' # tab(), so tab_plain(ci = "ref") and tab(ci = "ref") agree cell for cell.
#' data |>
#'   tab_plain(sex, hair_color, tot = c("row", "col"), pct = "row",
#'             ci = "ref", color = "difference", color_signif = "grey_non_signif")
#'
#' # the whole-table test is still a step (superseded, but supported)
#' data |>
#'   tab_plain(sex, hair_color, tot = c("row", "col"), pct = "row") |>
#'   tab_chi2()
#' }
tab_plain <- function(data, row_var, col_var, tab_vars, wt,
                      pct = "no", color = "no", display = NULL, OR = "no",
                      na = "keep",
                      ref = "auto", ref2 = "first", comp = "tab",
                      totaltab = "line", totaltab_name = "Ensemble",
                      tot = NULL, total_names = "Total",
                      subtext = "", digits = 0,
                      num = FALSE, df = FALSE,
                      ci = "auto", conf_level = conf_level_default(), stars = NULL,
                      ci_method = NULL, design_effect = NULL, color_signif = "ignore",
                      .fine = NULL, .by_table = FALSE
) {
  # Phase 18z14-i: a survey design as `data` is unwrapped FIRST -- tidyselect must see a data frame.
  # On the tab() pipeline path `data` is already a frame, so this is a single inherits() and a no-op.
  # The design itself is not used here yet (tab_plain has no test); its weights are, which is what
  # makes tab_plain(design, ...) return the same estimates as tab(design, ...).
  svy   <- svy_unwrap_data(data, "tab_plain")
  if (!is.null(svy)) data <- svy$data
  # Phase 19i: the shared argument boundary (see tab()). Phase 18z16-iiiii (D7): the two leaves
  # hard-coded conf_level = 0.95 and stars = FALSE while ?tabxplor-options promised both options are
  # honoured everywhere; they resolve like tab() -- now literally, through the same call.
  .a <- tab_resolve_common_args(
    "tab_plain", color = color, color_signif = color_signif, stars = stars,
    conf_level = conf_level, OR = OR, display = display, ref = ref, ref2 = ref2,
    tot = tot, total_names = total_names, na = na, pct = pct, comp = comp, totaltab = totaltab,
    ci = ci, ci_method = ci_method, user_env = rlang::caller_env())
  stars <- .a$stars ; display <- .a$display ; ref <- .a$ref ; ref2 <- .a$ref2
  total_names <- .a$total_names ; ci_method <- .a$ci_method

  # Phase 19l: THE shared NSE preamble (leaf_defuse_vars, below) -- one rule for the three producers.
  .v <- leaf_defuse_vars(data, rlang::enquo(row_var), rlang::enquo(col_var),
                         rlang::enquo(tab_vars), rlang::enquo(wt), svy = svy, plural = FALSE)
  data <- .v$data ; row_var <- .v$row_var ; col_var <- .v$col ; tab_vars <- .v$tab_vars ; wt <- .v$wt



  # Phase 17f: resolve the leaf's validation + forcing cascade ONCE (shared with tab_transform),
  # then hand the resolved bundle to the compute core. tab_plain never finalises colour -- the outer
  # tab()/tab_many() wrapper is the sole finaliser -- so the core returns the built table directly.
  # Phase 19d/19e: the ONE `OR` retirement route ran at the boundary above. The leaf carries a real
  # `display` of its own, so it is LOSSLESS here as on the pipeline (`OR = "OR"` -> `display =
  # "{or}"`): the leaf and the wrapper speak one grammar.
  comparison <- tab_leaf_comparison(color, display, pct, ref)
  # Phase 19j (KEY 5): the leaf computes EVERY per-cell interval now, so `ci` is a real argument here,
  # in tab()'s own anchor vocabulary, resolved by the rule both leaves share (D28/D29 included).
  # `or_ci` / `ci` / `ci_scale` are the same triple tab_resolve_settings() derives for the pipeline,
  # so tab_plain(ci = "cell") and tab(ci = "cell") agree by construction rather than by mirroring.
  r_ci  <- resolve_leaf_ci(ci, color, color_signif, stars, ref)
  stars <- r_ci$stars ; color_signif <- r_ci$color_signif
  or_ci <- identical(comparison, "odds_ratio") && identical(r_ci$ci, "ref")
  ci_leaf  <- if (or_ci) "no" else if (identical(r_ci$ci, "ref")) "diff" else r_ci$ci
  ci_scale <- if (identical(comparison, "ratio")) "ratio" else "diff"
  r <- plain_resolve(pct, ref, ref2, na, totaltab_name, total_names, tot, comp, color,
                     digits, totaltab, tab_vars, comparison = comparison)
  tab_apply_display(plain_core(
    data, row_var, col_var, tab_vars, wt,
    pct = r$pct, color = color, na = r$na, ref = r$ref, ref2 = r$ref2, comp = r$comp,
    totaltab = r$totaltab, totaltab_name = totaltab_name, tot = r$tot, total_names = r$total_names,
    subtext = subtext, digits = r$digits, num = num, df = df,
    stars = stars, color_signif = color_signif, .fine = .fine, .by_table = .by_table,
    comparison = comparison, ci = ci_leaf, ci_scale = ci_scale,
    or_ci = or_ci,
    # Phase 18z14-ii: tab_plain(design, ...) gets the design-based intervals too -- through the
    # same inference object tab_setup() builds for the pipeline (no design -> "weights"/"n" from
    # `design_effect` or its option, byte-identical to the leaf's former inline read).
    inference = new_inference(wt, svy$spec, conf_level, ci_method, design_effect = design_effect)
  ), display)
}


# plain_resolve() -- Phase 17f: the factor leaf's argument validator + forcing cascade (pct ->
# tot -> comp -> ref="auto" -> digits -> totaltab), shared by the public tab_plain() wrapper and
# tab_transform() so the pipeline resolves the SAME way instead of the leaf re-deriving. ref = "auto"
# is type-specific here (the measure's declared `ref_auto`, else the total row), differing from the
# numeric leaf (num_resolve) for a mixed table. Returns the resolved bundle.

# THE leaf's NSE preamble (Phase 19l). One rule, three callers.
#
# WHY IT EXISTS. This cascade -- capture the quosure, treat missing/NA/NULL/""/"no" as "the user named
# nothing" and synthesise a constant column for it, else resolve to symbol(s) -- was written THREE
# times: plain_core(), num_core() and tab_aggregate_num() (R/tab-agg.R). ~30 lines each, differing in
# exactly one thing: whether `col_var` is a single symbol or a tidyselect of several. That made it the
# largest verbatim duplication left in the package after Phase 19.
#
# The quosures are captured BY THE CALLER (`rlang::enquo(row_var)` in its own frame) and handed in, so
# this is an ordinary function -- no NSE forwarding, no caller_env() games.
#
# `plural = FALSE` returns `col_var` (one sym); `plural = TRUE` returns `col_vars` (a list of syms)
# plus `pos_col_vars`, the tidyselect positions num_core() needs to keep `digits` aligned.
# `svy`: a survey design already unwrapped at the boundary -- its own weight column REPLACES `wt`,
# and passing both aborts (W10, svy_abort_wt_design). Pass NULL where there is no design.
#' @keywords internal
#' @noRd
leaf_defuse_vars <- function(data, row_var_quo, col_quo, tab_vars_quo, wt_quo,
                             svy = NULL, plural = FALSE) {
  if (quo_miss_na_null_empty_no(row_var_quo)) {
    data    <- data |> dplyr::mutate(no_row_var = factor("no_row_var")) # "n"
    row_var <- rlang::sym("no_row_var")
  } else {
    row_var <- rlang::sym(rlang::as_name(row_var_quo))
  }

  pos_col_vars <- NULL
  if (quo_miss_na_null_empty_no(col_quo)) {
    data <- data |> dplyr::mutate(no_col_var = factor("n"))
    col  <- if (plural) rlang::syms("no_col_var") else rlang::sym("no_col_var")
    if (plural) pos_col_vars <- tidyselect::eval_select("no_col_var", data)
  } else if (plural) {
    pos_col_vars <- tidyselect::eval_select(col_quo, data)
    col          <- rlang::syms(names(pos_col_vars))
  } else {
    col <- rlang::sym(rlang::as_name(col_quo))
  }

  if (quo_miss_na_null_empty_no(tab_vars_quo)) {
    tab_vars <- character()
  } else {
    tab_vars <- rlang::syms(names(tidyselect::eval_select(tab_vars_quo, data)))
  }

  if (quo_miss_na_null_empty_no(wt_quo)) {
    wt <- character()
  } else {
    wt <- rlang::sym(rlang::as_name(wt_quo))
  }
  if (!is.null(svy)) {
    svy_abort_wt_design(length(wt) != 0L)
    wt <- rlang::sym(svy$spec$wt)
  }

  list(data = data, row_var = row_var, col = col, pos_col_vars = pos_col_vars,
       tab_vars = tab_vars, wt = wt)
}


#' @keywords internal
#' @noRd
plain_resolve <- function(pct, ref, ref2, na, totaltab_name, total_names, tot, comp, color,
                          digits, totaltab, tab_vars, comparison = NA_character_) {
  # Phase 19i: the VOCABULARY checks (pct / na / tot / comp) moved to the one argument boundary,
  # tab_resolve_common_args() -- this resolver's job is the FORCING cascade. What stays is the
  # normalisation the cascade itself needs.
  vctrs::vec_assert(ref, size = 1)
  ref <- stringi::stri_trim_both(stringi::stri_replace_all_regex(ref, "\\s+", " "))
  vctrs::vec_assert(ref2, size = 1)
  ref2 <- stringi::stri_trim_both(stringi::stri_replace_all_regex(ref2, "\\s+", " "))
  vctrs::vec_assert(totaltab_name, size = 1)

  #pct
  if (pct == "all_tabs" & length(tab_vars) == 0) pct <- "all"

  if (color != "no" & ref == "no") {
    warning("since color is ", color, " ref can't be `no` and was set to `tot`")
    ref <- "tot"
  }

  #tot
  # WARNING (Phase 19a): the `else` arm below -- the six forcings and their warnings -- is UNREACHABLE
  # from tab() / tab_many() / tab_counts(). tab_transform() hard-codes `tot = c("row", "col")` in its
  # plain_resolve() call, so `tot` is non-NULL, neither guard `!"col" %in% tot` / `!"row" %in% tot`
  # can be TRUE, and the whole arm is a 34-line identity. It is LIVE, however, through the exported
  # `tab_plain(tot = "row")` -- which is why study §5's "delete it, 6 unreachable warnings" was NOT
  # applied here: tab_plain() stays public, so these forcings are public behaviour. Revisit in 19h
  # (the entry-point item) rather than deleting them as dead code.
  if (is.null(tot)) {
    tot <- switch(pct,
                  "no"  = "no",
                  "row" = , #switch(ref, "tot" = c("row", "col"), "col"),
                  "col" = , #switch(ref, "tot" = c("row", "col"), "row"),
                  "all" = ,
                  "all_tabs" = c("row", "col"),
    )

  } else {
    # the vocabulary is checked at the boundary (TAB_ARG_VALUES); the EXPANSION stays here, because
    # it means different things on the two leaves -- both totals on a crosstab, ...
    if (tot[1] == "both") tot <- c("row", "col")

    if (!"col" %in% tot) {
      if (pct == "row") {
        warning("since pct == 'row', a total column was added")
        tot <- c(tot, "col")
      }
      if (color != "no" & pct == "col" & ref == "tot") {
        warning("since color == ", color, " and pct == 'col' and ref == 'tot', a total column was added")
        tot <- c(tot, "col")
      }
      if (pct %in% c("all", "all_tabs")) {
        warning("since pct == 'all' or 'all_tabs', a total column was added")
        tot <- c(tot, "col")
      }
    }

    if (!"row" %in% tot) {
      if (pct == "col") {
        # Phase 19a: the message said "pct == 'row'" under the guard `pct == "col"` -- the wrong
        # orientation word, mirroring the total-COLUMN block above instead of stating its own rule.
        warning("since pct == 'col', total rows were added")
        tot <- c(tot, "row")
      }
      if (color != "no" & pct == "row" & ref == "tot") {
        warning("since color == ", color, " and pct == 'row' and ref == 'tot', total rows were added")
        tot <- c(tot, "row")
      }
      if (pct %in% c("all", "all_tabs")) {
        warning("since pct == 'all' or 'all_tabs', total rows were added")
        tot <- c(tot, "row")
      }
    }
  }

  #comp
  vctrs::vec_assert(comp, size = 1)
  stopifnot(comp %in% c("tab", "all", "") | is.na(comp) | is.null(comp))

  comp <- force_comp(comp, tab_vars)

  #ref
  # LEAF resolution (Phase 7b): ref = "auto" is type-specific and intentionally stays here, NOT
  # in tab_resolve_settings() -- for a mixed table it must differ between this factor leaf and the
  # numeric leaf (tab_num). WHICH reference is the MEASURE's own declared `ref_auto` (Phase 19d: one
  # lookup instead of the `OR != "no" | color %in% c("or","OR")` literal) -- the odds ratio compares
  # to the first level; every other measure to the total row. See the map doc, § static-vs-data line.
  if (ref == "auto") {
    ra  <- measure_ref_auto(if (is.na(comparison) || !nzchar(comparison)) color else comparison)
    ref <- if (!is.na(ra)) ra else "tot"
  }

  #digits
  vctrs::vec_assert(digits, size = 1)
  digits <- vctrs::vec_cast(digits, integer())

  #totaltab
  if (length(tab_vars) == 0) totaltab <- "no"

  if (((comp[1] == "all" & ref == "tot") | pct == "all_tabs") &
      !totaltab %in% c("table", "line")) {
    warning("since comp = 'all', a total table was added to compare with")
    totaltab <-  "line"
  }

  if (comp[1] == "all" & !ref %in% c("tot", "no", "") & totaltab != "table") {
    warning("since comp = 'all', a full total table was added to compare with")
    totaltab <- "table"
  }

  list(pct = pct, ref = ref, ref2 = ref2, na = na, total_names = total_names,
       tot = tot, comp = comp, digits = digits, totaltab = totaltab)
}


# plain_core() -- Phase 17f: the factor leaf's compute core. Consumes ALREADY-RESOLVED scalar settings
# (from plain_resolve) + the resolved NSE syms; does the count aggregate + pct/diff/ratio/OR + fmt build
# + totals + reference + the tab_var_1lv wrap, and returns the built table. Colour is NOT finalised here
# (tab_plain never was) -- the outer tab()/tab_many() wrapper finalises once.
#' @keywords internal
#' @noRd
plain_core <- function(data, row_var, col_var, tab_vars, wt, pct, color, na, ref, ref2, comp,
                       totaltab, totaltab_name, tot, total_names, subtext, digits, num, df,
                       stars, color_signif, .fine, .by_table, inference,
                       comparison = NA_character_, or_ci = FALSE, dichotomise = FALSE,
                       ci = "no", ci_scale = "diff", test = "no", deff = NULL) {
  # Phase 19d (KEY 8a): `OR` is gone from the leaf. The odds ratio is computed on EVERY row/col-%
  # table (measured free: the 2x2 it needs is four numbers the wide table already holds, in the same
  # tab_apply_reference() sweep that produces diff and ratio), so nothing here switches it on.
  # What the two new arguments carry is the ONE fact the resolver settled: `comparison` = which
  # geometry this table compares on, `or_ci` = whether the LEAF owns the interval (the Woolf log-OR
  # one) rather than tab_ci(). `ref2` alone picks the 2x2 -- a level, or "cumulative".
  # Phase 19j (KEY 5): ... and `ci` / `ci_scale` are the rest of that one settled fact -- the leaf owns
  # EVERY per-cell interval now, the Woolf one and the cell/contrast one alike (they are mutually
  # exclusive: `or_ci` is TRUE only where `ci` is "no"). They come straight off the settings spine.
  # `test` ("no" | "p" | "ctr") and `deff` are the same for the WHOLE-TABLE test (see leaf_chi2()).
  or_compare <- identical(comparison, "odds_ratio")
  # Phase 19a: `inference` is REQUIRED (it was `= new_inference()`). A lazy default could only
  # fire on a caller that forgot to thread the build-time object, and would then silently
  # re-read the global option instead of failing -- the "re-derived downstream" bug the
  # inference object exists to end. Every call site passes it explicitly.

  # Phase 18z16-iiiii: ONE resolved inference object (new_inference(), built in tab_setup) instead
  # of the four flat formals conf_level / design_spec / inference_basis / degf.
  # Phase 19i: the six statements both leaves share are leaf_inference_setup(); what stays local to
  # each is what genuinely differs (here `can_neff`/`has_w2`, in num_core the per-col_var
  # `num_served` and the two `method_mean_*`).
  list2env(leaf_inference_setup(inference, .fine, .by_table), environment())
  des_rows <- NULL

  if (use_raw) {
    # Phase k: convert labelled (haven/labelled) row/col/tab columns to value-label factors for the
    # DIRECT tab_plain() entry (no tab_prepare upstream). Idempotent on the tab()/tab_many() path
    # (already converted -> no-op); the weight is excluded (numeric).
    data <- data |> tab_apply_val_labels(as.character(c(tab_vars, row_var, col_var)))
    data <- data |>
      dplyr::select(!!!tab_vars, !!row_var, !!col_var, !!wt,
                    tidyselect::any_of(if (design_on) svy_row_col else character())) |>
      dplyr::mutate(dplyr::across(!!wt & !where(is.numeric), as.numeric)) |>
      # DESIGN: REQUIRED for the direct tab_plain() entry (the public no-total escape hatch).
      # tab_many() also relabels once upstream (~L889), so this is redundant ONLY on the tab()/
      # tab_many() path; the op is idempotent and cheap (post the short-circuit fix, see CLAUDE.md
      # § Discovered bugs). Keep it -- removing it would break a bare tab_plain() call for a
      # negligible perf gain.
      relabel_levels_in_varnames(as.character(col_var))
    #Vars are not changed to factors here, but after data.table
  }






  tab_row_names  <- as.character(c(tab_vars, row_var))

  # DESIGN: data.table name round-trip (how user column names survive dcast). We (1) rename
  # the col_var to the fixed internal name "col_var" (~L2239) so the dcast formula is stable,
  # and (2) when a col_var ALSO appears among row/tab vars (self cross-tab), duplicate it as
  # "<var>_colvarbis" so one column can be both an aggregation key and the spread variable.
  # The internal names ("col_var", "_colvarbis", and dcast's "n_"/"wn_" value prefixes) are
  # all stripped later (~L2317 setnames, ~L2437 prefix removal) to restore the user's names.
  # Phase 19m-iii: **"Total" is the fourth of them** -- the pre-rename key of every total ROW
  # (build_total_rows / finalize_total_rows), total TAB and the total COLUMN this leaf mints, read
  # back as such by leaf_wide_pct(), num_rollup() and the survey variance producers
  # (svy_group_map / svy_var_prop), and swapped for the user's `total_names` only at the very end,
  # in leaf_rename_totals(). So a `total_names[1]` anywhere upstream of that rename is a BUG, and
  # a consumer that runs AFTER it must be handed a declared vector instead (`totcol_vector` /
  # `totrow_vector` / `tottab_vector`) -- which is exactly the 19m-i defect in tab_apply_reference().
  #If variables are in double in cols and rows, duplicate them and manage data.table
  col_var_in_row_var <- tab_row_names %in% as.character(col_var)
  if (any(col_var_in_row_var)) {
    in_col_vars <- tab_row_names[col_var_in_row_var]

    data <- data |>
      dplyr::mutate(dplyr::across(tidyselect::all_of(in_col_vars), ~ ., .names = "{.col}_colvarbis"))
    tabs_vars2 <-
      if (length(tab_vars) != 0) {
        dplyr::recode(as.character(tab_vars),
                      !!!purrr::set_names(paste0(in_col_vars, "_colvarbis"),
                                          in_col_vars))
      } else {
        character()
      }

    row_var2 <- dplyr::recode(as.character(row_var),
                              !!!purrr::set_names(paste0(in_col_vars, "_colvarbis"),
                                                  in_col_vars))
    tab_row_names2 <- c(tabs_vars2, row_var2)
  } else {
    tab_row_names2 <- tab_row_names
  }



  #Make all calculations with data.table to gain time
  if (use_raw) {
    # Phase 18z14-ii: lift `.svy_row` (each prepared row's position in the ORIGINAL design) out of
    # the frame BEFORE data.table takes over, so the aggregate scan below sees exactly the columns it
    # saw before this phase. plain_core never filters rows, so `des_rows` stays aligned with `data`.
    if (design_on) { des_rows <- data[[svy_row_col]]; data[[svy_row_col]] <- NULL }

    data.table::setDT(data)
    data.table::setnames(data, as.character(col_var), "col_var", skip_absent = TRUE)

    if (nrow(data) == 0) stop("data is of length 0 (possibly after filter or na = 'drop_all')")
  } else if (nrow(.fine) == 0) {
    stop("data is of length 0 (possibly after filter or na = 'drop_all')")
  }

  # DESIGN: aggregation source for the default (factor x factor) path. `use_raw` -> table-by-table
  # (one raw scan per row_var x col_var, current behaviour, kept verbatim). Otherwise roll up the
  # shared finest-grain aggregate `.fine` (built once in tab_many) for this pair. Both feed the
  # SAME dcast below, so everything downstream is byte-identical. Fused runs only when col_var is a
  # factor and there is no col_var/row_var overlap (both guaranteed by tab_many).
  # Phase 18s: the effective sample size of the weighted factor CIs (opt-in). Sigma w^2 is a per-cell
  # sufficient statistic accumulated ONLY on the microdata `use_raw` scan (pre-aggregated `.fine` data
  # has no per-observation weights, so it is genuinely unrecoverable there -> n_eff stays NA -> the CI
  # falls back to the raw unweighted base tot_n). Gated on `weighted`, and USED per the basis.
  # Phase 18z14-ii: the basis is RESOLVED (svy_inference_basis(), tab_setup()), never re-read from
  # the option here -- one rule governs the omnibus test and every cell interval.
  # Phase 18z16-i (ruling 8): Sigma w^2 is accumulated whenever the table is WEIGHTED, not only when
  # the option is on, so the aggregate has ONE shape -- toggling tabxplor.design_effect is then a
  # jamovi cache HIT instead of a full re-aggregate. Whether it is USED is the basis (`want_neff`).
  weighted <- length(wt) != 0
  if (use_raw) {
    long <- data[, list(n  = .N,
                        wn = if(weighted) { sum(eval(wt), na.rm = TRUE) } else {double()},
                        w2 = if(weighted) { sum(eval(wt)^2, na.rm = TRUE) } else {double()}),
                 keyby = eval(c(tab_row_names2, "col_var"))]
  } else {
    ocv  <- as.character(col_var)
    # Phase 18z16-iiiii: Sigma w^2 is ADDITIVE, so a pre-aggregate that carries it rolls up like
    # `wn` and the leaf gets the exact flat-design variance from it. That is what makes the jamovi
    # `design_effect` checkbox reach a PERCENTAGE (its cached factor aggregate is the only `.fine`
    # producer that can supply it -- tab_counts() genuinely cannot, and correctly does not).
    keep_w2 <- weighted && "w2" %in% names(.fine)
    long <- if (keep_w2) {
      .fine[, list(n = as.integer(sum(n)), wn = sum(wn), w2 = sum(w2)),
            keyby = eval(c(tab_row_names, ocv))]
    } else if (weighted) {
      .fine[, list(n = as.integer(sum(n)), wn = sum(wn)), keyby = eval(c(tab_row_names, ocv))]
    } else {
      .fine[, list(n = as.integer(sum(n))),              keyby = eval(c(tab_row_names, ocv))]
    }
    if (ocv != "col_var") data.table::setnames(long, ocv, "col_var")
  }

  # The flat design's nPSU: the number of observations the table is built from, i.e. survey's own n
  # for `svydesign(ids = ~1, data = <this data>)`. It feeds only the finite-sample factor n/(n-1).
  # Phase 18z16-iiiii: read off the AGGREGATE, on both branches -- byte-identical to the former
  # `nrow(data)` on the raw path (`.N` partitions the frame), and the only definition that also works
  # when the leaf was handed a pre-aggregate. It is the convention num_core() already used.
  n_obs <- sum(as.double(long$n))

  # Phase 18s: Sigma w^2 comes from the microdata scan, or (z16-iiiii) from a pre-aggregate that
  # carries it. The unweighted scan produces an EMPTY `w2` column like the empty `wn`, so a bare
  # `"w2" %in% names` would be a false positive: `has_w2` gates on weighted AND actual presence.
  has_w2 <- weighted && "w2" %in% names(long)
  # W9: the weighted basis was asked for but this input cannot serve it (pre-aggregated counts carry
  # no per-observation Sum(w^2)) -> the table states basis "n" (leaf_inference(unserved =)).
  # z16-iiiii: a LOCAL of this build, read by this build's own stamp at the tail -- it can no longer
  # leak into another table's footer, and it needs no reset.
  unserved <- identical(inference_basis, "weights") && !has_w2
  degraded <- FALSE
  # Phase 18z16-iv (W-G.2): TWO facts, each said once, replacing the near-synonymous `use_w2` and
  # the four hand-written `(use_w2 || design_on)` sites -- the basis is a single RESOLVED value
  # (svy_inference_basis()) and must not be re-encoded in five booleans. `want_neff` (from
  # leaf_inference_setup) = the basis asks for an effective base; `can_neff` = this input can supply
  # one. num_core() uses the same pair (`num_served` is its per-col_var `can_neff`, the moment
  # triples rather than one Sigma w^2 column). leaf_neff() below still gates its FLAT arm on `has_w2`
  # alone, which is correct and deliberate: a non-flat design whose variance degrades falls THROUGH.
  can_neff  <- has_w2 || design_on

  tabs <-
    data.table::dcast(
      long,
      formula = ... ~ col_var,
      value.var = if (has_w2) {c("n", "wn", "w2")} else if (length(wt) != 0) {c("n", "wn")} else {"n"},
      fill = 0
    )

  # Phase 18s: when Sigma w^2 is NOT a value.var (unweighted, or the .fine path), the empty `w2`
  # column leaks into the dcast as a constant id column -- exactly like the empty `wn` does when
  # unweighted (dropped a few lines below). Drop it so it never reaches the output.
  if (!has_w2 && "w2" %in% names(tabs)) tabs[, "w2" := NULL]


  if (any(col_var_in_row_var)) {
    colvarbis <- names(tabs)[stringi::stri_detect_regex(names(tabs), "_colvarbis$")]
    data.table::setnames(tabs, colvarbis, stringi::stri_replace_first_regex(colvarbis, "_colvarbis$", ""),
                         skip_absent = TRUE)
  }

  not_fct <- !purrr::map_lgl(dplyr::select(tabs, tidyselect::all_of(tab_row_names)), is.factor)
  if (any(not_fct)) {
    tabs[, names(not_fct)[not_fct] := purrr::map(.SD, forcats::as_factor),
         .SDcols = names(not_fct)[not_fct]]
  }


  na_cols <- names(tabs) %in% c("n_NA", "wn_NA", "w2_NA", "NA")
  if (any(na_cols)) {
    if (na == "drop") {
      suppressWarnings(tabs[, `:=`(n_NA = NULL, wn_NA = NULL, w2_NA = NULL, `NA` = NULL)])
    } else {
      data.table::setcolorder(tabs, c(names(tabs)[!na_cols], names(tabs)[na_cols]))
    }
  }

  na_rows <- tabs |>
    dplyr::select(!!!tab_vars, !!row_var) |>
    dplyr::mutate(na_rows = dplyr::if_any(.cols = dplyr::everything(), .fns = is.na)) |>
    dplyr::pull(.data$na_rows)

  if (any(na_rows)) {
    if (na == "drop") {
      tabs <- tabs[-which(na_rows), ]
    } else {
      data.table::setorderv(
        tabs, tab_row_names, na.last = TRUE
      )[, paste0(tab_row_names) := lapply(.SD, forcats::fct_na_value_to_level, level = "NA"),
        .SDcols = tab_row_names]
    }
  }

  num_cols <- tabs |> purrr::map_lgl(is.numeric)
  num_cols <- names(num_cols)[num_cols]

  # Region B (Phase 9d): total-TABLE row(s) via base-R group-sum. "table" = one total row per row_var
  # level (tab_vars set to "Total"); "line" = one grand total row (all tab_row_names "Total").
  if (totaltab %in% c("table", "line")) {
    if (totaltab[1] == "table") { bt_keys <- as.character(row_var); bt_totvars <- as.character(tab_vars) }
    else                        { bt_keys <- character();           bt_totvars <- tab_row_names }
    tabs_totaltab <- build_total_rows(tabs, bt_keys, bt_totvars, tab_row_names, num_cols)
    tabs <- finalize_total_rows(tabs, tabs_totaltab, bt_totvars, tab_row_names)
  }



  # Region C (Phase 9d): total ROWS via base-R group-sum, one build_total_rows() per tab_vars
  # accumulation level (subtable totals + grand total), deduped (identical duplicate rows collapse,
  # order-independent -> the final setorderv dominates), then the totaltab=="line" grand-line drop.
  if ("row" %in% tot) {
    if (length(tab_vars) != 0) {
      group_vars <- rev(purrr::accumulate(as.character(tab_vars) , ~ c(.x, .y)))
      total_vars <- purrr::map(group_vars,
                               ~ c(as.character(tab_vars)[!as.character(tab_vars) %in% .],
                                   as.character(row_var)))
    } else {
      group_vars <- list(character())
      total_vars <- list(as.character(row_var))
    }

    parts    <- purrr::map2(group_vars, total_vars,
                            ~ build_total_rows(tabs, .x, .y, tab_row_names, num_cols))
    tabs_tot <- do.call(rbind, parts)
    tabs_tot <- tabs_tot[do.call(order, tabs_tot[tab_row_names]), , drop = FALSE]
    tabs_tot <- tabs_tot[!duplicated(tabs_tot), , drop = FALSE]

    if (totaltab == "line") {
      keep     <- Reduce(`|`, lapply(tab_row_names, function(v) as.character(tabs_tot[[v]]) != "Total"))
      tabs_tot <- tabs_tot[keep, , drop = FALSE]
    }

    tabs <- finalize_total_rows(tabs, tabs_tot, unique(unlist(total_vars)), tab_row_names)
  }

  tt <- leaf_totrow_tottab(tabs, row_var, tab_vars)
  totrow_vector <- tt$totrow; tottab_vector <- tt$tottab; kind_vector <- tt$kind




  # THE OPTIONAL LEAF TABLES. Each is produced only on some paths (weighted / percentage / reference
  # / OR-interval / design-based), so the twelve reads below have to ask "was this one produced?".
  # Phase 19l declares them ONCE instead: they were bare locals, and every read asked the ENVIRONMENT
  # through `exists(<name>, rlang::current_env(), inherits = FALSE)` -- 29 of them, four of which
  # spelled the call differently. That is the same disease `new_ctx()` cured for the build context in
  # 19i: an undeclared name is indistinguishable from a mistyped one, and a typo reads as "absent"
  # instead of erroring. This list IS the documentation of what the leaf may or may not compute.
  tabs_wn <- tabs_w2 <- tabs_pct <- tabs_totn <- tabs_neff <- NULL
  tabs_diff <- tabs_mean <- tabs_rr <- tabs_or <- NULL
  tabs_or_ci_inf <- tabs_or_ci_sup <- tabs_or_pvalue <- NULL
  refcols_vector <- refrows <- NULL

  # Phase 17f: df/num build the normal table like any other and extract get_num() at the very end
  # (leaf_extract_raw), so this is now the SINGLE aggregation-shaping path (the former df/num early
  # return + count-only branch are gone).
  if (length(wt) == 0) {
    if ("wn" %in% names(tabs)) tabs[, "wn" := NULL]

    text_vars <- !purrr::map_lgl(tabs, is.numeric)
    text_vars <- text_vars[text_vars]

    if ("col" %in% tot) {
      tabs[, "Total" := as.integer(rowSums(tabs[, -text_vars, with = FALSE]))] #Problems if not integer.
    }
    tabs_n <- tabs

  } else {
    text_vars <- !purrr::map_lgl(tabs, is.numeric)
    n_index  <- stringi::stri_detect_regex(names(tabs), "^n_")  | text_vars
    wn_index <- stringi::stri_detect_regex(names(tabs), "^wn_") | text_vars
    w2_index <- stringi::stri_detect_regex(names(tabs), "^w2_") | text_vars

    text_vars <- text_vars[text_vars]

    tabs_n  <- data.table::setnames(tabs[, n_index, with = FALSE] ,
                                    function(.x) stringi::stri_replace_first_regex(.x, "^n_" , ""))
    tabs_wn <- data.table::setnames(tabs[, wn_index, with = FALSE],
                                    function(.x) stringi::stri_replace_first_regex(.x, "^wn_", ""))

    tabs_wn[, (names(tabs_wn)) := purrr::map(.SD, as.double)]

    # Phase 18s: the per-cell Σw² wide table (present only on the microdata scan), reshaped like
    # tabs_wn and rolled up for the "col" total identically (Σw² is additive across a partition, like Σw).
    if (has_w2) {
      tabs_w2 <- data.table::setnames(tabs[, w2_index, with = FALSE],
                                      function(.x) stringi::stri_replace_first_regex(.x, "^w2_", ""))
      tabs_w2[, (names(tabs_w2)) := purrr::map(.SD, as.double)]
    }

    if ("col" %in% tot) {
      tabs_n [, "Total" := as.integer(rowSums(tabs_n[, -names(text_vars), with = FALSE] ))] #Problems if not integer.
      tabs_wn[, "Total" := rowSums(tabs_wn[, -names(text_vars), with = FALSE])]
      if (has_w2) tabs_w2[, "Total" := rowSums(tabs_w2[, -names(text_vars), with = FALSE])]
    }

  }
  tabs_text <- tabs[, names(text_vars), with = FALSE] #tibble::as_tibble()
  cols <- purrr::map_lgl(tabs_n, is.numeric)
  cols <- cols[cols]


  #Percentages
  # DESIGN: copy() before each in-place := derivation below (tabs_pct/diff/mean/rr/or). The
  # aggregated table is shared by reference; without copy() a := would mutate the source and
  # every other derived table too (data.table reference semantics).
  # THE per-cell inference base, `n_eff` (Phase 18z14-ii Route A, generalised in z16-ii).
  # ONE definition -- n_eff = p(1-p) / Var_design(p), Korn & Graubard's own device -- with the
  # IMPLEMENTATION selected by the resolved basis:
  #   "weights", or a FLAT svydesign(ids = ~1), or a design whose variance could not be computed
  #        -> the closed form in the per-cell Sigma w^2 the aggregate already carries (O(cells),
  #           no microdata, no ceiling, no fallback that can silently degrade)
  #   "design"  -> survey::svyrecvar on each cell's influence function (R/survey-variance.R)
  # It is written into the SAME `n_eff` field, so every interval, star and colour threshold
  # downstream becomes basis-aware through the one field they all already read (tab_ci,
  # tab_apply_reference's OR interval, chi2_write_contrib) -- no new field, no engine change. `p` is
  # the DISPLAYED proportion, so the interval provably inverts the number printed.
  # z4: a COUNTS table (pct = "no") never reaches leaf_wide_pct(), so its base is computed on the
  # "all" selector -- the subtable itself, which is exactly the base its chi2 cell residual needs.
  neff_dt <- function(Ne) {
    Ne[!is.finite(Ne)] <- NA_real_
    out <- data.table::copy(tabs_n)
    out[, (names(cols)) := lapply(seq_len(ncol(Ne)), function(j) Ne[, j])]
    out
  }
  leaf_neff <- function(res, base) {
    Pm <- as.matrix(res$pct[, names(cols), with = FALSE]) * 1.0
    if (design_on && !design_flat && !is.null(des_rows)) {
      vres <- svy_var_prop(
        prep      = svy_var_prep(inference$design, des_rows),
        keys      = lapply(tab_row_names,  function(v) svy_key_chr(tabs_n[[v]])),
        n_tab     = length(tab_vars),
        mkeys     = lapply(tab_row_names2, function(v) svy_key_chr(data[[v]])),
        mcol      = svy_key_chr(data[["col_var"]]),
        col_names = names(cols), base = base)
      if (!is.null(vres$v)) {
        Ne <- Pm * (1 - Pm) / vres$v
        Ne[!is.finite(Ne) | Ne <= 0] <- NA_real_
        # A DEGENERATE cell (p = 0 or 1 -> Var = 0 -> 0/0) has no base of its own; it falls back to
        # its base domain's B^2/S, exactly as the closed form does (z16-ii). Without this the Total
        # column of a percentage table came back NA under a design, and `color = "contrib"` then
        # silently read the RAW n there -- measured p 1.6e-11 where the counts table said 0.052 (W3).
        if (has_w2 && anyNA(Ne)) {
          M_w2 <- as.matrix(tabs_w2[, names(cols), with = FALSE]) * 1.0
          fb <- svy_flat_base_neff(res$dmat(res$m_pct), res$dmat(M_w2))
          Ne[is.na(Ne)] <- fb[is.na(Ne)]
        }
        return(neff_dt(Ne))
      }
      # -> basis "design_partial" (a local of plain_core, stamped at its tail); the weights still
      # apply below, so the cell keeps the exact flat closed form rather than the raw n.
      degraded <<- svy_var_degraded(vres$reason)
    }
    if (!has_w2) return(NULL)
    M_w2 <- as.matrix(tabs_w2[, names(cols), with = FALSE]) * 1.0
    neff_dt(svy_flat_neff_prop(P = Pm, A = M_w2, S = res$dmat(M_w2), B = res$dmat(res$m_pct),
                               n_obs = n_obs))
  }

  if (pct == "no" && want_neff && can_neff) {
    res_0 <- leaf_wide_pct(tabs_n, tabs_wn, "all", as.character(tab_vars), cols)
    ne_0  <- leaf_neff(res_0, "all")
    if (!is.null(ne_0)) tabs_neff <- ne_0
  }

  if (pct != "no") {
    # Phase 9d: percentages + the tot_n base on a numeric matrix (base-R) via leaf_wide_pct(),
    # replacing the copy() + switch(pct) + purrr::map(.SD, ~ ./eval(sym("Total"))) per column.
    # `tot_n` (Phase 2, 2.0.0) = each cell's OWN unweighted percentage base (row / column / grand
    # total, per `pct`), BROADCAST from the UNWEIGHTED tabs_n so the built table is self-sufficient
    # for exact statistics (retires detect_totcols() on built tables, decisions §2, §11). Byte-
    # identical to the former per-cell path (dev/benchmarks/phase9d_leaf_math_parity.R).
    res_e     <- leaf_wide_pct(tabs_n, if (length(wt) == 0) NULL else tabs_wn,
                               pct, as.character(tab_vars), cols)
    tabs_pct  <- res_e$pct
    tabs_totn <- res_e$tot_n
    # the per-cell effective base, on the base the table displays (see leaf_neff()); NULL on basis "n".
    if (want_neff && can_neff) {
      ne_e <- leaf_neff(res_e, pct)
      if (!is.null(ne_e)) tabs_neff <- ne_e
    }


    #Differences and odds ratio
    if (ref != "no" & pct %in% c("row", "col")) {
      # Phase 7f: the reference step is the shared tab_apply_reference() (used verbatim here and by the
      # jmvtab tier-3 re-ref). It returns diff / ratio(=tabs_mean) and, when OR/color needs them, rr /
      # or + the ref-col vector; refrows is the ref-row marker. Assign each only when produced; the
      # optional-table declaration at the top of this function leaves the rest NULL.
      # 14z: compute the OR interval only when a colour policy or stars needs it (else a NULL tabs_totn
      # skips it in tab_apply_reference -> no ci_type/bounds change, so existing ignore-OR tables stay
      # byte-identical). color_signif reads the bounds; stars read the (want_p-gated) pvalue.
      # 19d: the interval is the leaf's only when the odds ratio IS the comparison; whether one is
      # wanted at all was settled by the resolver (or_ci), so the leaf no longer re-reads the policy.
      or_want_ci <- isTRUE(or_ci)
      ref_res <- tab_apply_reference(
        tabs = tabs, tabs_pct = tabs_pct, ref = ref, ref2 = ref2, comp = comp,
        or_compare = or_compare, pct = pct, tab_row_names = tab_row_names, tab_vars = tab_vars,
        row_var = row_var, tottab_vector = tottab_vector, totrow_vector = totrow_vector, cols = cols,
        # 19m-i: the leaf MINTS this column a few hundred lines above (`tabs[, "Total" := ...]`), so
        # here the literal IS the declaration -- which is exactly why it may not live inside the
        # shared function, whose other caller works on renamed columns.
        totcol_vector = names(cols) == "Total",
        tabs_totn = if (or_want_ci) tabs_totn else NULL,
        # Phase 18s: the OR colour interval honours the effective base too, so color = "OR"
        # significance/stars on a weighted crosstab widen consistently with the % CI brackets.
        # z14-ii: keyed on the object existing rather than on the basis, since it also carries the
        # DESIGN base -- byte-identical, `tabs_neff` having only ever existed under one of the two.
        tabs_neff = if (or_want_ci && !is.null(tabs_neff)) tabs_neff else NULL,
        conf_level = conf_level, stars = stars, degf = inference$degf,
        dichotomise = dichotomise
      )
      tabs_diff <- ref_res$diff
      tabs_mean <- ref_res$ratio
      if (!is.null(ref_res$rr))             tabs_rr        <- ref_res$rr
      if (!is.null(ref_res$or))             tabs_or        <- ref_res$or
      if (!is.null(ref_res$or_ci_inf))      tabs_or_ci_inf <- ref_res$or_ci_inf
      if (!is.null(ref_res$or_ci_sup))      tabs_or_ci_sup <- ref_res$or_ci_sup
      if (!is.null(ref_res$or_pvalue))      tabs_or_pvalue <- ref_res$or_pvalue
      if (!is.null(ref_res$refcols_vector)) refcols_vector <- ref_res$refcols_vector
      if (!is.null(ref_res$refrows))        refrows        <- ref_res$refrows
    }
  }



  #Make the final table with fmt vectors
  # remove(list = c("tabs_n", "tabs_wn", "tabs_pct", "tabs_diff", "tabs_ci", "refcols_vector", "refrows"))
  tabs_n [, names(text_vars) := NULL]
  if (!is.null(tabs_wn)) tabs_wn  [, names(text_vars) := NULL]
  if (!is.null(tabs_pct)) tabs_pct [, names(text_vars) := NULL]
  if (!is.null(tabs_diff)) tabs_diff[, names(text_vars) := NULL]
  if (!is.null(tabs_mean)) tabs_mean[, names(text_vars) := NULL]
  if (!is.null(tabs_rr)) tabs_rr  [, names(text_vars) := NULL]
  if (!is.null(tabs_or)) tabs_or  [, names(text_vars) := NULL]
  if (!is.null(tabs_or_ci_inf)) tabs_or_ci_inf[, names(text_vars) := NULL]
  if (!is.null(tabs_or_ci_sup)) tabs_or_ci_sup[, names(text_vars) := NULL]
  if (!is.null(tabs_or_pvalue)) tabs_or_pvalue[, names(text_vars) := NULL]
  if (!is.null(tabs_totn)) tabs_totn[, names(text_vars) := NULL]
  if (!is.null(tabs_neff)) tabs_neff[, names(text_vars) := NULL]

  totcol_vector <- names(tabs_n) == "Total"
  NA_reals <- rep(NA_real_, nrow(tabs_n))

  if (ref == "tot") refrows <- rep(FALSE, nrow(tabs_n))

  if (is.null(refrows)) refrows <- rep(FALSE, nrow(tabs_n))

  # Phase 19j (KEY 5): THE cell / contrast interval, computed here -- where the plan is -- instead of
  # 1 500 lines later by tab_ci() from reconstructed markers. Everything it needs is a clean matrix at
  # this point (the text columns are dropped just above, so each data.table's columns ARE `cols`).
  # `ref = "tot"` zeroes `refrows` above (the total row is not a "reference row" marker), which is
  # exactly the distinction tab_ci()'s ref_mask() drew by reading `ref` back off the column.
  ci_res <- leaf_ci_plain(
    P     = if (!is.null(tabs_pct))
              as.matrix(tabs_pct)  * 1.0 else matrix(NA_real_, nrow(tabs_n), ncol(tabs_n)),
    tot_n = if (!is.null(tabs_totn))
              as.matrix(tabs_totn) * 1.0 else matrix(NA_real_, nrow(tabs_n), ncol(tabs_n)),
    n_eff = if (!is.null(tabs_neff))
              as.matrix(tabs_neff) * 1.0 else NULL,
    ci = ci, pct = pct, ci_scale = ci_scale,
    # tab_ci() ungroups for a ROW contrast under comp = "all" and for nothing else -- so a column
    # contrast and a cell interval keep their sub-table grouping even there. Reproduced literally.
    grp = if (identical(comp, "all") || length(tab_vars) == 0L) rep(1L, nrow(tabs_n)) else
      do.call(paste, c(lapply(as.character(tab_vars), function(v) as.character(tabs_text[[v]])),
                       sep = "\r")),
    ref_row = if (identical(as.character(ref), "tot")) totrow_vector else refrows,
    totrow  = totrow_vector,
    refcol  = if (!is.null(refcols_vector) &&
                  any(refcols_vector)) which(refcols_vector)[1] else NA_integer_,
    totcol  = totcol_vector,
    conf_level = conf_level, stars = stars,
    ci_method = inference$method, degf = inference$degf)

  # Phase 7f-1: display / colour / type / ref / comp / col_var and the digits recycle are
  # column-INVARIANT here (they read only tab_plain-scope scalars/symbols -- pct/OR/wt/color/ref/
  # ref2/row_var/col_var/comp/digits -- never the per-column pmap args ..N), yet the old code
  # recomputed each once per output column inside the closure. Compute them ONCE. new_fmt()
  # recycles the scalar `display` to length(n) (fmt_class.R), so this is byte-identical to the
  # former per-column case_when/if_else/switch. NA_reals (built above at length nrow(tabs_n)) is
  # reused for every all-NA field (identical values, one allocation instead of ~6 per column).
  # Phase 19d: the leaf builds the CELL, not a chosen geometry -- `display` is the tail's job now
  # (tab_apply_display), and the `or`/`or_pct` arms went with the `OR` argument.
  # Phase 19j: ... except the one display a CELL interval implies -- "show the bracket you asked for"
  # (tab_ci()'s `visible` argument, which existed only because the interval arrived after the leaf).
  display_1 <- dplyr::case_when(
    isTRUE(ci_res$visible)                           ~ "pct_ci",
    pct != "no"                                      ~ "pct",
    length(wt) != 0                                  ~ "wn" ,
    TRUE                                             ~ "n"
  )
  color_1 <- dplyr::case_when(
    color %in% c("", "no")                            ~ "",
    is_placeholder_var(row_var) | is_placeholder_var(col_var) ~ "",
    or_compare & pct %in% c("row", "col") & ref != "no" & ref2 != "no" ~ "odds_ratio",
    pct %in% c("row", "col") & ref != "no"            ~ "difference",
    TRUE                                              ~ ""
  )
  # Phase 19b (KEY 2): the leaf STAMPS what its columns estimate. `pct_base` is the percentage's own
  # base ("none" for a count column); `scale` is the estimate's scale -- a level here, since the leaf
  # builds cells. tab_ci() upgrades it to `points` / `odds_ratio` / `pct_ratio` when it computes a
  # contrast interval, and tab_apply_reference() stamps `odds_ratio` where it builds the Woolf one.
  base_1   <- dplyr::if_else(pct != "no", pct, "none")
  # An odds-ratio table's columns estimate an ODDS RATIO -- all of them, including the reference one,
  # whose own OR bounds are NA by construction (D19). Phase 19d keys that on the COMPARISON, not on
  # the rendered display: `scale` says which estimate the column's INTERVAL belongs to, and the
  # display is free (a `{or}` template on a difference-tested table shows odds ratios over a
  # difference interval -- which is exactly what the D23 bracket rule refuses to print together).
  # Phase 19j: and a CONTRAST interval makes it a difference (`points`) or a ratio of proportions --
  # the leaf's own CI_GEOMS row says which, so the scale, the method name and the bounds all come from
  # one lookup. A NA scale_key is a cell interval: the level scale stands. (The odds-ratio arm wins by
  # position; the two cannot co-occur, `or_ci` being TRUE only where `ci` is "no".)
  scale_1  <- dplyr::case_when(or_compare & pct %in% c("row", "col") & ref != "no" ~ "odds_ratio",
                               !is.na(ci_res$scale)             ~ ci_res$scale,
                               pct != "no"                      ~ "level_pct",
                               TRUE                             ~ "level_n")
  ref_1    <- switch(as.character(ref), "no" = "", "tot" = "tot", as.character(ref))
  comp_1   <- dplyr::if_else(pct != "no" & ref != "no", comp == "all", NA)
  colvar_1 <- rlang::as_name(col_var)
  digits_v <- vctrs::vec_recycle(as.integer(digits), nrow(tabs_n))

  # Phase 19j: ONE SLOT, ONE INTERVAL. The Woolf log-OR bounds when the odds ratio IS the comparison
  # (`or_ci`), this leaf's cell/contrast bounds otherwise -- the resolver guarantees the two are
  # mutually exclusive, which is the whole reason the geometry had to be settled before either was
  # asked for. `or_from_leaf` also keeps the ci_method stamp honest: under `or_ci` a column whose own
  # 2x2 was degenerate carries all-NA bounds and therefore names no method (D19).
  or_from_leaf <- !is.null(tabs_or_ci_inf)
  mat_cols     <- function(M) lapply(seq_len(ncol(M)), function(j) M[, j])
  ci_inf_1     <- if (or_from_leaf) tabs_or_ci_inf else
                  if (!is.null(ci_res$inf))    mat_cols(ci_res$inf)    else list(NA_reals)
  ci_sup_1     <- if (or_from_leaf) tabs_or_ci_sup else
                  if (!is.null(ci_res$sup))    mat_cols(ci_res$sup)    else list(NA_reals)
  ci_pvalue_1  <- if (or_from_leaf) tabs_or_pvalue else
                  if (!is.null(ci_res$pvalue)) mat_cols(ci_res$pvalue) else list(NA_reals)

  tabs <-
    list(tabs_n,
         if (!is.null(tabs_wn)) { tabs_wn   } else { list(NA_reals) },
         if (!is.null(tabs_pct)) { tabs_pct  } else { list(NA_reals) },
         if (!is.null(tabs_diff)) { tabs_diff } else { list(NA_reals) },
         if (!is.null(tabs_mean)) { tabs_mean } else { list(NA_reals) },
         if (!is.null(tabs_rr)) { tabs_rr   } else { list(NA_reals) },
         if (!is.null(tabs_or)) { tabs_or   } else { list(NA_reals) },

         totcol_vector,
         if (!is.null(refcols_vector)) { refcols_vector } else {
           rep(FALSE, length(cols)) },
         if (!is.null(tabs_totn)) { tabs_totn } else { list(NA_reals) },
         ci_inf_1, ci_sup_1, ci_pvalue_1,
         if (!is.null(tabs_neff)) { tabs_neff } else { list(NA_reals) }
    ) |>
    # Phase 9b-3: build the plain carrier column (frame + meta) then materialize via the single
    # fmt_materialize_col() (== the former inline new_fmt, byte-identical). pmap_dfc is KEPT so the
    # output columns keep their exact col_var-cell names/order. `..6` (tabs_rr) is unused, as before.
    # Phase 5 (§3): `ratio` (= tabs_mean = ..5) is the REFERENCE-RELATIVE ratio (the "x2 rule" / colour
    # ratio measure); `mean` is NA for pct columns (the old mean-overload is gone; colour reads ratio).
    purrr::pmap_dfc(function(...) {
      a <- list(...)
      # 14z: a[[11..13]] carry the empirical-OR interval (all-NA unless a colour policy/stars asked for
      # it). Phase 19b: the scale is column-INVARIANT (scale_1 above) -- the ref2/Total columns' NA
      # bounds are what makes them carry no interval and no significance, which is a data fact, not a
      # second vocabulary (D19).
      fmt_materialize_col(
        frame = list(
          n         = as.integer(a[[1]]), display = display_1, digits = digits_v,
          wn        = a[[2]], pct = a[[3]], mean = NA_reals, diff = a[[4]], ratio = a[[5]],
          ctr       = NA_reals, var = NA_reals, ci_inf = a[[11]], ci_sup = a[[12]],
          pvalue    = a[[13]], or = a[[7]], tot_n = a[[10]], n_eff = a[[14]],
          row_kind  = kind_vector, in_tottab = tottab_vector, in_refrow = refrows),
        meta  = list(
          scale     = scale_1, comp_all = comp_1, ref = ref_1,
          # WHICH engine built these bounds (D8), from the same source that built them. Under `or_ci`
          # it is Woolf's log-OR one -- and a column whose own 2x2 was degenerate carries all-NA
          # bounds, which is the data fact saying "no interval here", so it names no method.
          # Otherwise it is the CI_GEOMS row leaf_ci_plain() used, column-invariant like the scale.
          ci_method = if (or_from_leaf) { if (!all(is.na(a[[11]]))) "woolf" else "" }
                      else ci_res$method,
          pct_base  = base_1, col_var = colvar_1,
          totcol    = a[[8]], refcol = a[[9]], color = color_1, color_signif = "ignore")
      )
    })

  tabs <- dplyr::bind_cols(tibble::as_tibble(tabs_text), tabs)

  tabs <- leaf_rename_totals(tabs, row_var, tab_vars, tot, total_names, totaltab, totaltab_name,
                             tottab_vector, totrow_vector)


  # with no col_var
  no_col_vars_cols <- get_col_var(tabs) == "no_col_var" #& pct %in% c("row", "col", "all", "all_tabs")
  if (any(no_col_vars_cols) ) {
    tabs <- tabs |>
      dplyr::mutate(n = set_display(.data$n, "n") |> set_count_col() |> as_totcol(FALSE)) |>
      dplyr::relocate("n", .after = tidyselect::last_col())

    if (pct %in% c("row", "col", "all", "all_tabs")) {
      tabs <- tabs |>
        dplyr::rename(tidyselect::any_of(c("pct" = total_names[2]))) |> # if (total_names[2] == "Total")
        dplyr::mutate(pct = as_totcol(pct, FALSE))
         } else {
      tabs <- tabs |> dplyr::select(-dplyr::where(is_totcol))
    }

    if (length(wt) != 0) tabs <- tabs |>
        dplyr::mutate(wn = set_display(.data$n, "wn") |> set_count_col()) |>
        dplyr::relocate("wn", .after = tidyselect::last_col() )
  }

  # Phase 19j (KEY 5): the WHOLE-TABLE TEST, here, on this leaf's own col_var -- which is its natural
  # grain (`chi2_compute_test()` already produces one row per subtable x col_var, and the residual is
  # a property of ONE contingency table). See leaf_chi2().
  leaf_test <- NULL
  if (!identical(test, "no")) {
    lt        <- leaf_chi2(tabs, test, comp, row_var, col_var, tab_vars, deff)
    tabs      <- lt$tabs
    leaf_test <- lt$test
  }

  leaf_finish(tabs, row_var, tab_vars, wt, subtext, inference, unserved, degraded, df, num,
              test = leaf_test)
}


# leaf_chi2() -- Phase 19j (KEY 5): the leaf's OWN whole-table test + contribution pass.
#
# DESIGN: it calls the SAME chi2_write_contrib() / chi2_compute_test() the superseded tab_chi2() step
#   calls -- no second implementation, no matrix rewrite of a 180-line function carrying a byte-identity
#   lock. What moves is not the arithmetic but the QUESTION: the step had to reconstruct the metadata
#   from fmt markers (tab_get_vars, detect_totcols, tab_validate_comp) and MUTATE the table to make its
#   own preconditions true (tab_match_groups_and_totrows / tab_add_totcol_if_no / tab_match_comp_and_-
#   tottab, five warning branches between them); the leaf simply knows all of it, and built the totals
#   itself. `col_vars_levels` is this leaf's value columns, `tot_cols` is its "Total", `is_a_mean` is
#   FALSE (a factor leaf), and the reference/total structure is the one it just wrote.
#
# WARNING: `comp = "all"` is applied as a LOCAL ungrouping, not a table mutation. tab_chi2() ungrouped
#   the table it returned, so whether a comp = "all" table came back GROUPED depended on whether a test
#   happened to run -- and the jamovi tier-2 test cache, which skips the step, therefore returned a
#   different CLASS from a fresh build (measured; masked until 19j only because tab_ci() ungrouped too).
#   A computation step must not decide the table's shape.
#' @keywords internal
#' @noRd
leaf_chi2 <- function(tabs, test, comp, row_var, col_var, tab_vars, deff = NULL) {
  # `test` is the leaf's plan value ("p" | "ctr"); chi2_write_contrib() speaks tab_chi2()'s own
  # pre-2.0.0 colour vocabulary ("no"/"auto"/"all"/"all_pct"), so it is derived here, once -- exactly
  # as the deleted post-join test pass derived it.
  do_ctr  <- identical(test, "ctr")
  calc    <- if (do_ctr) c("ctr", "p") else "p"
  color   <- if (do_ctr) "all" else "no"
  cv      <- rlang::as_name(col_var)
  lev_all <- names(tabs)[purrr::map_lgl(tabs, is_fmt)]
  if (length(lev_all) == 0L || identical(cv, "no_col_var"))
    return(list(tabs = tabs, test = new_test_tibble()))

  col_vars_levels        <- stats::setNames(list(rlang::syms(lev_all)), cv)
  is_tot                 <- purrr::map_lgl(lev_all, ~ any(is_totcol(tabs[[.x]])))
  col_vars_levels_no_tot <- stats::setNames(list(rlang::syms(lev_all[!is_tot])), cv)
  tot_nm                 <- if (any(is_tot)) lev_all[is_tot][[1]] else lev_all[[length(lev_all)]]
  tot_cols               <- stats::setNames(rlang::syms(rep(tot_nm, length(lev_all))), lev_all)

  keep  <- dplyr::group_vars(tabs)
  work  <- leaf_test_view(tabs, comp, tab_vars)

  # the contribution reads the col_var's own TOTAL column (the chi2 marginals live there), which the
  # measure's declared `requires` forces on -- so its absence means the caller did not ask for contrib.
  if (do_ctr && any(is_tot))
    work <- chi2_write_contrib(work, calc, comp, color, col_vars_levels,
                               col_vars_levels_no_tot, is_a_mean = FALSE, all_col_tot = FALSE,
                               tot_cols = tot_cols, deff = deff)

  test_tbl <- chi2_compute_test(work, comp, as.character(rlang::as_name(row_var)),
                                col_vars_levels, col_vars_levels_no_tot,
                                is_a_mean = FALSE, all_col_tot = FALSE)

  work <- dplyr::ungroup(work)
  if (length(keep)) work <- dplyr::group_by(work, dplyr::across(dplyr::all_of(keep)))
  list(tabs = work, test = test_tbl)
}


# leaf_test_view() -- the grouping the whole-table test is computed ON. `comp = "all"` means "one test
# for the whole table", so the sub-table grouping is dropped -- for the COMPUTATION only (see the
# WARNING on leaf_chi2()). Shared by both leaves so they cannot answer it differently.
#' @keywords internal
#' @noRd
leaf_test_view <- function(tabs, comp, tab_vars) {
  gv <- as.character(tab_vars)
  if (identical(comp, "all") || length(gv) == 0L) dplyr::ungroup(tabs)
  else dplyr::group_by(tabs, dplyr::across(dplyr::all_of(gv)))
}


# leaf_chi2_num() -- the numeric leaf's twin: the one-way ANOVA (Welch + classic F) over its mean
# columns, via the same chi2_compute_test(). A numeric col_var IS its own single "level" column, and
# there is no contribution to write -- so this is the metadata, and nothing else.
#' @keywords internal
#' @noRd
leaf_chi2_num <- function(tabs, comp, row_var, col_vars, tab_vars) {
  cvs <- as.character(col_vars)
  cvs <- cvs[cvs %in% names(tabs)]
  if (length(cvs) == 0L) return(new_test_tibble())
  cvl <- stats::setNames(lapply(cvs, function(v) rlang::syms(v)), cvs)
  chi2_compute_test(leaf_test_view(tabs, comp, tab_vars), comp,
                    as.character(rlang::as_name(row_var)), cvl, cvl,
                    is_a_mean = rep(TRUE, length(cvs)), all_col_tot = rep(FALSE, length(cvs)))
}


# leaf_finish() -- Phase 19i: the RESULT TAIL both leaves run, in one place. Declare the row-index
# columns, decide whether the tab_vars survive as groups, wrap in the class with the table's own
# identity, stamp the inference facts on every fmt column, and hand back either the fmt table or the
# extracted raw numbers.
#
# It replaces two ~30-line blocks that were structurally identical and differed in ONE thing: the
# factor leaf passed `meta = list(spec = new_spec("crosstab", ...))` and the numeric leaf passed no
# `meta` at all -- so a table built by a direct `tab_num()` carried no `spec$kind` (`tab_kind()` fell
# back to its degraded guess) and no `vars$wt` (the "Weighted by" footer line had nothing to read).
# `tab()` masked it by setting the meta itself at assemble. Sharing the tail is what fixes it.
#
# @param tabs      the assembled tibble, totals renamed, before any class
# @param row_var,tab_vars  symbols (tab_vars a list of symbols)
# @param wt        the weight column name (symbol or character(0))
# @param inference the build-time inference object; `unserved`/`degraded` are what THIS build found
#                  out about it (see leaf_inference()) and only this build can know
# @param df,num    the two raw-extraction modes of the public leaves
#' @keywords internal
#' @noRd
leaf_finish <- function(tabs, row_var, tab_vars, wt, subtext, inference,
                        unserved = FALSE, degraded = FALSE, df = FALSE, num = FALSE,
                        test = NULL, anova = NULL) {
  tab_var_1lv <- all(purrr::map_lgl(dplyr::select(tabs, !!!tab_vars),
                                    ~ length(unique(.)) == 1))

  # Phase 18z16-i: the leaf records its own inference basis, so a DIRECT tab_plain()/tab_num() (the
  # exported step path) carries the fact its footer and its tab_ci() need.
  inf <- leaf_inference(inference, unserved, degraded)

  # Phase 19f (KEY 1): the leaf DECLARES its row-index columns, in one call, where the truth is known
  # -- instead of assembling a `vars` list that consumers then had to validate against the real
  # columns. `tab_render_vars()` reads the declaration; nothing guesses "the last factor column".
  tabs <- tab_stamp_index(tabs, level = rlang::as_name(row_var),
                          var = rlang::as_name(row_var),
                          tab_vars = purrr::map_chr(tab_vars, rlang::as_name))
  # Phase 19g (KEY 6): the producer STATES the kind. `vars` holds only what no column can carry --
  # after 19f that is the weight name (and the variable labels, added at assemble).
  meta <- list(spec = new_spec("crosstab", vars = new_vars_attr(
    wt = if (length(wt) == 0L) NA_character_ else as.character(wt)[1])))
  # Phase 19k: `anova` is display intent (which of the two stored F rows the p-value line shows), so
  # it rides render_extras -- stored only when the caller stated it, else the option decides.
  if (!is.null(anova)) meta$render_extras <- list(anova = as.character(anova)[[1]])

  # WARNING: `test` defaults to new_test_tibble() in new_tab(), never NULL -- so a leaf with no test
  # must let the default stand, not pass NULL (that would DROP the empty-tibble attribute every table
  # has carried, on every numeric golden).
  tst <- if (is.null(test)) new_test_tibble() else test
  result <- if (tab_var_1lv) {
    new_tab(tabs, subtext = subtext, test = tst, meta = meta) |>
      dplyr::select(-tidyselect::any_of(purrr::map_chr(tab_vars, as.character)))
  } else {
    tabs <- tabs |> dplyr::group_by(!!!tab_vars)
    new_grouped_tab(tabs, dplyr::group_data(tabs), subtext = subtext, test = tst, meta = meta)
  }

  # Phase 18z13 (D3) + z16-iiiii: the level, the design df and the basis on every fmt COLUMN, for
  # the per-column colour engine and for tab_ci() -- see tab_stamp_inference().
  result <- tab_stamp_inference(result, inference$conf_level, inf$degf, inf$basis)

  # Phase 17f: df/num -> pull the displayed number per cell (leaf_extract_raw); else the fmt table.
  if (df || num) leaf_extract_raw(result, df, num, row_var) else result
}


# leaf_inference() -- the inference facts of ONE built table (Phase 18z16-i): the resolved basis,
# plus the design's degrees of freedom. Shared by both leaves and by tab_reg(), so the fact cannot
# differ between the pipeline and the exported step path.
# The basis is downgraded by what the build FOUND OUT, which only the build can know, and which
# Phase 18z16-iiiii turned from a process-global environment into two locals passed in here:
#   `unserved` -- the weighted basis was asked for and this input cannot serve it (a pre-aggregated
#                 `.fine` / tab_counts carry no per-observation Sum(w^2)) -> the table states "n" and
#                 its footer says the intervals use the counts' own n, rather than claiming a
#                 correction the numbers do not have (W9).
#   `degraded` -- the design's variance producer had to fall back (svy_var_degraded()) -> the claim
#                 becomes "design_partial", so no export can assert a design the numbers do not carry
#                 (W4).
# It FEEDS tab_stamp_inference() -- the facts are stamped on every fmt COLUMN, not stored in a table
# attribute, so they survive every rebuild that keeps the columns. An unweighted table gets the
# defaults ("n", NA) and is byte-unchanged.
#' @keywords internal
#' @noRd
# num_total_postprocess() -- Phase 19i: the tail num_core() runs after EACH of its two num_rollup()s
# (the total rows, then the total table). It was written out twice, differing only in the key set --
# the tab_vars there, the row_var here. In place (data.table `:=`), so the caller's own object is
# updated and nothing is returned.
#
# Two steps: (1) a rollup's key columns come back as plain character, so coerce them to factors in
# APPEARANCE order (`forcats::as_factor`, never `base::as.factor`, which would sort them and move
# "Total"); (2) under na = "keep", order NAs last and give them an explicit "NA" level, so the total
# rows carry the same NA level the cells do.
#
# WARNING: this is NOT the same as the coercion of the MAIN aggregate a hundred lines above, which
# uses `base::as.factor`. That one runs on data.table's own keyed output, already in sorted order.
#' @keywords internal
#' @noRd
num_total_postprocess <- function(dt, keys, na, tab_row_names) {
  not_fct <- !purrr::map_lgl(dplyr::select(dt, tidyselect::any_of(tab_row_names)), is.factor)
  if (any(not_fct)) {
    dt[, names(not_fct)[not_fct] := purrr::map(.SD, forcats::as_factor),
       .SDcols = names(not_fct)[not_fct]]
  }
  if (identical(na, "keep") && length(keys) != 0) {
    data.table::setorderv(dt, keys, na.last = TRUE
    )[, (keys) := lapply(.SD, forcats::fct_na_value_to_level, level = "NA"), .SDcols = keys]
  }
  invisible(dt)
}


# leaf_inference_setup() -- Phase 19i: the inference PREAMBLE both leaves open with, list2env()'d
# into each core so the bodies below read exactly as before. Six statements, not the ~45 lines of
# comment they were buried in -- the rest of each preamble is genuinely leaf-specific and stays put
# (plain_core's `has_w2` / `can_neff`, num_core's per-col_var `num_served` and its two `method_mean_*`).
#
#   conf_level / inference_basis  -- the two facts unpacked from the ONE build-time object
#   design_on / design_flat       -- a FLAT svydesign(ids = ~1) has the closed form as its EXACT
#                                    answer, so it takes the algebraic path: same number, no
#                                    influence matrix, no 400 MB ceiling (z16-ii)
#   want_neff                     -- the basis asks for an effective base (W-G.2)
#   use_raw                       -- the aggregate-injection seam. `.by_table` or no `.fine` forces
#                                    the raw scan; so does ANY design, because a design-based
#                                    variance is a function of the OBSERVATIONS (svyrecvar on
#                                    per-cell influence vectors) and even the flat one needs the
#                                    per-cell Sigma w^2, which a count aggregate cannot carry.
#' @keywords internal
#' @noRd
leaf_inference_setup <- function(inference, .fine, .by_table) {
  basis     <- inference$basis
  design_on <- identical(basis, "design")
  list(conf_level      = inference$conf_level,
       inference_basis = basis,
       design_on       = design_on,
       design_flat     = design_on && svy_design_is_flat(inference$design),
       want_neff       = !identical(basis, "n"),
       use_raw         = .by_table || is.null(.fine) || design_on)
}


leaf_inference <- function(inf, unserved = FALSE, degraded = FALSE) {
  if (!svy_weighted(inf)) return(list(basis = NULL, degf = NULL))
  basis <- inf$basis
  if (identical(basis, "weights") && isTRUE(unserved)) basis <- "n"
  if (identical(basis, "design")  && isTRUE(degraded)) basis <- "design_partial"
  list(basis = basis, degf = inf$degf)
}


# leaf_wide_pct() -- Phase 9d: tab_plain()'s Region E (percentages + the tot_n base) on a numeric
# matrix (base-R) instead of copy() + switch(pct) + purrr::map(.SD, ~ ./eval(rlang::sym("Total")))
# per column. `pct` = the value matrix / denominator matrix `D` (row -> the row's Total; col -> the
# tab_vars-group's last (= total) row; all/all_tabs -> that row's / the grand Total), then NA/NaN ->
# 0 (== tidyr::replace_na). `tot_n` = D built on the UNWEIGHTED tabs_n, broadcast (not divided).
# `grp_last <- ave(seq_len(n), grp, max)` reproduces dplyr::last(.) = the group's total row exactly.
# Byte-identical to the former per-cell path (dev/benchmarks/phase9d_leaf_math_parity.R).
#' @keywords internal
#' @noRd
# Phase 18z16-ii: it no longer carries a SECOND meaning -- it computes percentages and `tot_n`, and
# the variance module computes variances. `dmat` (the base-domain broadcast) and `grp_last` are
# returned so plain_core can apply exactly the same selector to the Sigma w^2 matrix.
leaf_wide_pct <- function(tabs_n, tabs_wn, pct, tab_vars, cols) {
  nm <- names(cols); n <- nrow(tabs_n); k <- length(nm)
  grp <- if (length(tab_vars) == 0) rep(1L, n) else {
    key <- do.call(paste, c(lapply(tab_vars, function(v) as.character(tabs_n[[v]])), sep = "\r"))
    match(key, unique(key))
  }
  grp_last <- stats::ave(seq_len(n), grp, FUN = max)
  M_pct  <- if (!is.null(tabs_wn)) as.matrix(tabs_wn[, nm, with = FALSE]) else
                                   as.matrix(tabs_n[,  nm, with = FALSE]) * 1.0
  M_totn <- as.matrix(tabs_n[, nm, with = FALSE]) * 1.0
  Dmat <- function(M) leaf_dmat(M, pct, grp_last, n, k)
  P <- M_pct / Dmat(M_pct); P[is.na(P)] <- 0
  Tn <- Dmat(M_totn)
  wb <- function(src, M2) {
    dt <- data.table::copy(src)
    dt[, (nm) := lapply(seq_len(k), function(j) M2[, j])]
    dt
  }
  list(pct   = wb(if (!is.null(tabs_wn)) tabs_wn else tabs_n, P),
       tot_n = wb(tabs_n, Tn),
       grp_last = grp_last, m_pct = M_pct, dmat = Dmat)
}

# THE percentage-base broadcast: which denominator each cell divides by, per `pct`. Extracted in Last
# Phase z16-ii so leaf_wide_pct() and the flat design variance provably use the SAME base -- row -> the
# row's Total, col -> the tab_vars group's last (= total) row, all/all_tabs -> that row's / the grand
# Total. (`grp_last <- ave(seq_len(n), grp, max)` reproduces dplyr::last(.) = the group's total row.)
#' @keywords internal
#' @noRd
leaf_dmat <- function(M, pct, grp_last, n, k) switch(
  pct,
  "row"      = matrix(M[, "Total"],         n, k),
  "col"      = M[grp_last, , drop = FALSE],
  "all"      = matrix(M[grp_last, "Total"], n, k),
  "all_tabs" = matrix(M[n,        "Total"], n, k))


# build_total_rows() / finalize_total_rows() -- Phase 9d: tab_plain()'s total-TABLE (Region B) and
# total-ROW (Region C) group-sums via base-R instead of data.table `keyby`. DECISIVE: sum with
# base::sum() per split() group -- NOT rowsum()/data.table-gforce, whose plain-double accumulator
# drifts 1 ULP from the `purrr::map(.SD, sum, na.rm=TRUE)` (long-double accumulator) the old code
# used, breaking identical(). finalize_total_rows() appends the "Total" level to exactly the columns
# that receive it (totvars) before rbind + setorderv, matching data.table's factor-union. Byte-
# identical across 648 shapes (dev/benchmarks/phase9d_leaf_math_parity.R).
#' @keywords internal
#' @noRd
build_total_rows <- function(tabs, keys, totvars, tab_row_names, num_cols) {
  n <- nrow(tabs)
  if (length(keys) == 0) { idx <- list(seq_len(n)); kf <- NULL } else {
    key <- do.call(paste, c(lapply(keys, function(v) as.character(tabs[[v]])), sep = "\r"))
    f   <- factor(key, levels = unique(key))
    idx <- split(seq_len(n), f)
    kf  <- as.data.frame(do.call(rbind, strsplit(levels(f), "\r", fixed = TRUE)),
                         stringsAsFactors = FALSE)
    names(kf) <- keys
  }
  summ <- lapply(num_cols, function(cc) {
    col <- tabs[[cc]]; fv <- if (is.integer(col)) integer(1) else numeric(1)
    vapply(idx, function(ii) sum(col[ii], na.rm = TRUE), fv)
  })
  names(summ) <- num_cols
  lab <- lapply(tab_row_names, function(v)
    if (!is.null(kf) && v %in% names(kf)) kf[[v]] else rep("Total", length(idx)))
  names(lab) <- tab_row_names
  # check.names = FALSE: value-cell / key names carry special chars (e.g. "$25000 or more") that the
  # default as.data.frame() would mangle, breaking the c(tab_row_names, num_cols) reselect below.
  out <- cbind(as.data.frame(lab,  stringsAsFactors = FALSE, check.names = FALSE),
               as.data.frame(summ, stringsAsFactors = FALSE, check.names = FALSE))
  out[, c(tab_row_names, num_cols), drop = FALSE]
}

#' @keywords internal
#' @noRd
finalize_total_rows <- function(tabs, extra, cols_get_total, tab_row_names) {
  for (v in cols_get_total) if (v %in% names(tabs))
    tabs[[v]] <- factor(tabs[[v]], levels = unique(c(levels(tabs[[v]]), "Total")))
  for (v in tab_row_names)
    extra[[v]] <- factor(extra[[v]], levels = levels(tabs[[v]]))
  out <- rbind(tabs, data.table::as.data.table(extra))
  data.table::setorderv(out, tab_row_names)
  out[]
}


# tab_apply_reference() -- the reference step (Phase 7f carve): from the pct data.table + a reference
# selector, derive the reference-relative fields diff (cell - ref), ratio (cell / ref, the "x2 rule")
# and, when OR/color needs it, rr / or; plus the ref-row / ref-col markers. Extracted VERBATIM from
# tab_plain()'s inline block so the FRESH build stays byte-identical AND the jmvtab tier-3 re-ref
# (jmv_tab3_reref) can recompute exactly these ref-dependent fields from a cached table's ref-
# INDEPENDENT pct base, without a new_fmt() rebuild -- one implementation, no forked math.
# Returns a list; elements not computed for the given `pct` are NULL, so the caller's guards behave
# identically to the former inline locals. Phase 19d: the odds ratio is computed UNCONDITIONALLY on
# a row/col-percentage table (measured free -- its 2x2 is four numbers this sweep already holds), so
# `or` and `rr` are always produced. `or_compare` says whether the odds ratio is the comparison the
# table is TESTED on, which is what still gates the two BASELINE markers (`refcols_vector` on the
# row path, `refrows` on the col one) -- a marker means "this is the reference of the comparison in
# force", never "some comparison could use it". `ref2` picks the 2x2: a level, or "cumulative".
#' @keywords internal
#' @noRd
tab_apply_reference <- function(tabs, tabs_pct, ref, ref2, comp, or_compare, pct,
                                tab_row_names, tab_vars, row_var, tottab_vector, totrow_vector, cols,
                                totcol_vector = names(cols) == "Total",
                                tabs_totn = NULL, tabs_neff = NULL, conf_level = 0.95, stars = FALSE,
                                degf = Inf, dichotomise = FALSE) {
  # Phase 9d: the reference arithmetic (diff = cell - ref, ratio = cell / ref, rr / or) runs on a
  # plain numeric matrix via base-R sweep instead of the former per-cell data.table `:=` +
  # purrr::map_if -- byte-identical, ~100x faster on the isolated block (proven across 648 shapes:
  # dev/benchmarks/phase9d_leaf_math_parity.R). Index/name resolution (calculate_refrows, diff_index)
  # and the RETURN SHAPE (diff/ratio/rr/or frames indexable by col name + `refrows` logical) are
  # unchanged, so tab_plain() and the jmvtab tier-3 re-ref (jmv_tab3_reref) are unaffected.
  nm <- names(cols)
  n  <- nrow(tabs_pct)
  k  <- length(nm)
  P  <- as.matrix(tabs_pct[, nm, with = FALSE]) * 1.0
  # Phase 19m-i: which column is the TOTAL is a DECLARED fact, the sibling of `totrow_vector` /
  # `tottab_vector` this function already takes. It used to be re-derived here from the literal
  # `nm == "Total"` -- the leaf's own pre-rename convention, which holds for the leaf and NOT for the
  # jamovi re-reference (jmv_tab3_reref passes POST-leaf_rename_totals() names, and was correct only
  # because po/R-fr.po happens to translate "Total" -> "Total"; with total_names = "Ensemble" the
  # reference 2x2 was built against the wrong column). The default reproduces the old expression
  # exactly, so a caller cannot silently regress -- but every caller passes it.
  is_tot_col <- as.logical(totcol_vector)
  if (length(is_tot_col) != k) is_tot_col <- rep_len(FALSE, k)

  tabs_diff <- data.table::copy(tabs_pct)
  tabs_mean <- data.table::copy(tabs_pct)
  refrows   <- NULL
  # Phase 18z16-iv (W-G.5): the OR-branch locals are DECLARED here, absent = NULL, so the guards
  # below read `is.null()` -- the Phase 17e typed-default idiom -- instead of `exists(inherits =
  # FALSE)`, which asks the environment a question the function can simply answer.
  # 19l: `or_refrows` joins them -- it was the one local of this block still asked for with exists().
  tabs_rr <- NULL; tabs_or <- NULL; or_cells <- NULL; refcols_vector <- NULL; or_refrows <- NULL

  # write a derived matrix M2 (columns aligned to `nm`) into a data.table's value columns in place
  set_cols <- function(dt, M2) dt[, (nm) := lapply(seq_len(k), function(j) M2[, j])]

  # per-comp-group first reference-row absolute index (NA -> P[NA, ] is an all-NA row, reproducing
  # `x - dplyr::nth(x, replace_na(which(ref_rows)[1], 0))` = x - NA). comp_group = tab_vars (comp
  # "tab") or none (comp "all" / no tab_vars) -- the plain form of the former `by = eval(comp_group)`.
  comp_group <- if (comp == "tab") as.character(tab_vars) else character()
  grp_comp   <- if (length(comp_group) != 0) {
    do.call(paste, c(lapply(comp_group, function(v) as.character(tabs[[v]])), sep = "\r"))
  } else rep(1L, n)
  ref_abs <- function(refr) {
    out <- rep(NA_integer_, n)
    for (rows in split(seq_len(n), grp_comp)) {
      p <- which(refr[rows])[1]
      if (!is.na(p)) out[rows] <- rows[p]
    }
    out
  }

  if (pct == "row") {

    refrows <- tabs |>
      calculate_refrows(ref           = ref,
                        comp          = comp,
                        tab_row_names = tab_row_names,
                        tab_vars      = tab_vars,
                        row_var       = row_var,
                        tottab_vector = tottab_vector,
                        totrow_vector = totrow_vector,
                        num_names     = names(cols)
      )

    ra   <- ref_abs(refrows)
    Pref <- P[ra, , drop = FALSE]
    set_cols(tabs_diff, P - Pref)
    set_cols(tabs_mean, P / Pref)   # with pct, tabs_mean is the *2 rule ratio, not a difference


    # Phase 18z10: CUMULATIVE odds ratio -- one cut point per column ("at or below level j"), for
    # row i against the reference row. A k-level ordered col_var has k-1 cuts, so the last column is
    # empty by construction (P(Y <= last) == 1 -> infinite odds), which is exactly how it fits the
    # cell grid with nothing left over. Everything comes from the AGGREGATE (no microdata pass), and
    # it reuses ci_or() + the `odds_ratio` break scale unchanged -- a new DICHOTOMISATION, not a new
    # measure. The spread across a row IS the proportional-odds diagnostic, visible and free.
    # WARNING: the `na = "keep"` column is excluded from the cumulation. It is appended AFTER the real
    # levels by fct_na_value_to_level(), and "at or below NA" is not a cut point.
    if (ref2 == "cumulative") {
      lv <- which(!is_tot_col & nm != "NA")
      Pc <- matrix(NA_real_, n, k)
      if (length(lv) >= 2L) {
        U <- upper.tri(matrix(0, length(lv), length(lv)), diag = TRUE) * 1     # the cumulator
        Pc[, lv] <- P[, lv, drop = FALSE] %*% U
        Pc[, lv[length(lv)]] <- NA_real_                       # the degenerate last cut
      }
      Oc <- Pc / (1 - Pc)                                      # cumulative odds
      tabs_rr <- data.table::copy(tabs_pct)
      set_cols(tabs_rr, Oc)
      tabs_or <- data.table::copy(tabs_pct)
      set_cols(tabs_or, Oc / Oc[ra, , drop = FALSE])
      refcols_vector <- rep(FALSE, k)      # no reference COLUMN: every column is its own cut, ref2 unused
      or_cells <- function(N) {
        A <- Pc * N; B <- (1 - Pc) * N
        list(a = A, b = B, c = A[ra, , drop = FALSE], d = B[ra, , drop = FALSE])
      }

    } else {

      # Phase 16c: PER-COLUMN reference index. For a BINARY col_var (exactly 2 non-Total level columns)
      # each level's OR is computed against the OTHER level (the two columns are reciprocals, neither is
      # forced to "1", and ref2 is unused). For 3+ levels every column references the single ref2 column
      # (which then shows OR = 1) -- byte-identical to the former `P / P[, refcols]`. tab_plain has ONE
      # factor col_var, so the non-total columns ARE its levels.
      ridx0   <- diff_index(ref2, row_var = dplyr::pull(tabs_pct, !!row_var),
                            num_names = nm, pct = "col", is_total = is_tot_col)
      ok_ref2 <- length(ridx0) != 0 && !is.na(ridx0) && ridx0 >= 1L && ridx0 <= k
      lv      <- which(!is_tot_col)
      binary  <- length(lv) == 2L
      # Phase 19d-tail: `levels = "first"` SHOWS one level against the merged rest, so the reader's
      # col_var is a dichotomy and its odds ratio is the TRUE binary one -- that level against
      # everything else -- which is the whole reason showing a single column makes sense. tab() merges
      # before the leaf, so `binary` already catches it there; the jamovi path DEFERS the merge (the
      # aggregate and the whole-table test must see every level) and the surviving level is ALSO
      # `ref2`, so every column referenced itself and `or` came out 1 everywhere -- invisible until
      # 19d made the odds ratio unconditional. `dichotomise` says the col_var is shown dichotomised;
      # the length test picks which realisation applies, so the pre-merged path is untouched.
      dich <- isTRUE(dichotomise) && !binary && length(lv) >= 3L

      if (binary || ok_ref2 || dich) {
        ref_col_idx <- rep(if (ok_ref2) as.integer(ridx0) else NA_integer_, k)
        if (binary) { ref_col_idx[lv[1]] <- lv[2]; ref_col_idx[lv[2]] <- lv[1] }
        Pref_col <- P[, ref_col_idx, drop = FALSE]
        # The merged "rest" column does not exist yet on the deferred path -- it is materialised by the
        # display drop. Build it: within a row base the complement of a level IS 1 - p, which is
        # exactly the column the pre-merge would have produced (proven by the byte-identity lock
        # against a plain tab(levels = "first")). The Total column keeps its ref2 index, as there.
        if (dich) {
          Pref_col[, lv] <- 1 - P[, lv, drop = FALSE]
          # ... and those columns no longer reference a COLUMN, exactly as in the binary case. The
          # index survives only to feed `refcols_vector` below, and leaving it pointing at ref2 would
          # mark the kept level as the baseline where the pre-merged path does not.
          ref_col_idx[lv] <- NA_integer_
        }
        RR <- P / Pref_col
        or_cells <- function(N) {
          PN <- P * N; PrefN <- Pref_col * N
          list(a = PN, b = if (dich) PrefN else PN[, ref_col_idx, drop = FALSE],
               c = (P * N)[ra, , drop = FALSE],
               d = if (dich) PrefN[ra, , drop = FALSE]
                   else ((P * N)[ra, , drop = FALSE])[, ref_col_idx, drop = FALSE])
        }
      } else {
        warning(paste0(
          "in ref2 = '", ref2, "' , no columns were found as reference for comparison ; ",
          "to remove this warning, precise the value of ref ",
          "until there is one column matched"
        ))
        ref_col_idx <- rep(NA_integer_, k)
        RR <- matrix(NA_real_, n, k)
      }
      # Self-referencing columns show OR = 1 by construction: the ref2 column for 3+ levels, none for
      # binary. Phase 19d: the odds ratio is computed on every row-% table now, but `refcol` means
      # "this column is THE reference of the comparison in force" -- which the ref2 column is only
      # when the odds ratio IS that comparison. Marking it otherwise would tell every exporter to
      # dress the first level as a baseline on an ordinary difference table.
      refcols_vector <- or_compare & !is.na(ref_col_idx) & ref_col_idx == seq_len(k)

      tabs_rr <- data.table::copy(tabs_pct)
      set_cols(tabs_rr, RR)

      # Odds ratio (binary complement, or per-level vs ref2 for 3+ levels) : rr / reference ROW
      tabs_or <- data.table::copy(tabs_pct)
      set_cols(tabs_or, RR / RR[ra, , drop = FALSE])
    }

  }


  if (pct == "col") {
    refcols <- dplyr::nth(names(cols), diff_index(ref,
                                                  num_names = nm,
                                                  pct       = pct,
                                                  is_total  = is_tot_col))
    refcols_vector <- names(cols) == refcols

    if (length(refcols) != 0 & !is.na(refcols)) {
      set_cols(tabs_diff, P - P[, refcols])
      set_cols(tabs_mean, P / P[, refcols])   # *2 rule ratio
    } else {
      warning(paste0(
        "in ref = '", ref, "' , no columns were found as reference for comparison ; ",
        "to remove this warning, precise the value of ref ",
        "until there is one column matched"
      ))
      set_cols(tabs_diff, matrix(NA_real_, n, k))
      set_cols(tabs_mean, matrix(NA_real_, n, k))
    }


    # Odds ratio (when pct = "col") -- Phase 19d: unconditional, like its pct = "row" twin.
    {

      # Relative risks : cell / reference ROW. Phase 19d: the ref2 ROW is the odds ratio's own
      # baseline, so it is exported as `refrows` (-> the `in_refrow` field) only when the odds ratio
      # IS the comparison -- on a col% DIFFERENCE table the reference is a column, and marking a row
      # as the baseline would dress the wrong cells as one. The local `or_refrows` still drives the
      # arithmetic.
      or_refrows <- tabs |>
        calculate_refrows(ref           = ref2,
                          comp          = comp,
                          tab_row_names = tab_row_names,
                          tab_vars      = tab_vars,
                          row_var       = row_var,
                          tottab_vector = tottab_vector,
                          totrow_vector = totrow_vector,
                          num_names     = names(cols)
        )
      if (or_compare) refrows <- or_refrows
      ra <- ref_abs(or_refrows)
      RR <- P / P[ra, , drop = FALSE]
      tabs_rr <- data.table::copy(tabs_pct)
      set_cols(tabs_rr, RR)

      # Per-level OR vs the reference COLUMN. Phase 16c: the pct="col" binary-row mirror is deferred
      # (see decisions doc); the reference is the single `refcols` column (level 1 shows OR = 1), as
      # before. `ref_col_idx` (all pointing at the ref column) feeds the shared Woolf block below, keeping
      # the pct="col" interval byte-identical.
      tabs_or <- data.table::copy(tabs_pct)
      if (length(refcols) != 0 & !is.na(refcols)) {
        set_cols(tabs_or, RR / RR[, refcols])
      } else {
        set_cols(tabs_or, matrix(NA_real_, n, k))
      }
      ref_col_idx <- rep(which(refcols_vector)[1], k)
      if (!is.na(ref_col_idx[1])) or_cells <- function(N) {
        PN <- P * N
        list(a = PN, b = PN[, ref_col_idx, drop = FALSE],
             c = (P * N)[ra, , drop = FALSE],
             d = ((P * N)[ra, , drop = FALSE])[, ref_col_idx, drop = FALSE])
      }
    }
  }

  # 14z: Woolf log-OR Wald interval for the empirical odds ratio, computed ONLY when the caller needs
  # it (a colour policy / stars) -- signalled by a non-NULL `tabs_totn`, the UNWEIGHTED base. The 2x2 is
  # CONDITIONAL on {level j, its reference level} x {row i, ref row `ra`}, built the §14 way (WEIGHTED
  # proportion P x UNWEIGHTED base N, so the totals cancel). Phase 16c: `ref_col_idx` is the PER-COLUMN
  # reference index -- the ref2 column for 3+ levels (that column's OR = 1), the COMPLEMENT level for a
  # binary col_var (both levels get a real, reciprocal interval; only the ref row is NA'd), or the single
  # ref column for pct="col". For 3+ / pct="col" `ref_col_idx` is constant, so B/D are byte-identical to
  # the former single-`ridx` broadcast. The gate now fires whenever the OR was computed (any non-NA
  # reference), not only when a self-referencing `refcols_vector` column exists -- which for binary is
  # empty, so the old gate silently skipped its intervals. ci_or() (R/tab-agg.R) is the shared engine and
  # gives the CI-inversion pvalue (want_p = stars) that duals with the bracket, like the modelled OR.
  or_ci_inf <- or_ci_sup <- or_pvalue <- NULL
  # Phase 19d: `or_refrows` is the odds ratio's OWN row baseline -- `refrows` on the pct = "row" path
  # (the same row), the ref2 row on the pct = "col" one, where the exported `refrows` may be NULL.
  # 19l: the branch above declares it (NULL when it did not run), so this reads a value, not an env.
  if (is.null(or_refrows)) or_refrows <- refrows
  if (!is.null(tabs_totn) && !is.null(tabs_or) && !is.null(or_cells) && !is.null(or_refrows)) {
    N  <- as.matrix(tabs_totn[, nm, with = FALSE]) * 1.0
    # Phase 18s: swap in the effective base (n_eff) where it is finite (opt-in); on basis "n"
    # tabs_neff is NULL -> N is the unweighted base, byte-identical.
    if (!is.null(tabs_neff)) {
      Ne <- as.matrix(tabs_neff[, nm, with = FALSE]) * 1.0
      N[is.finite(Ne)] <- Ne[is.finite(Ne)]
    }
    # z10: the 2x2 is supplied BY THE ARM that built the odds ratio (a closure over its own
    # reference structure), so this block has no branch at all -- one ci_or() call for three OR
    # flavours. `a`/`b` = the level's (positive, negative) legs in this row, `c`/`d` the ref row's.
    cl <- or_cells(N)
    oc <- ci_or(as.vector(cl$a), as.vector(cl$b), as.vector(cl$c), as.vector(cl$d),
                conf_level = conf_level, want_p = isTRUE(stars), df = degf)
    OINF <- matrix(oc$inf, n, k); OSUP <- matrix(oc$sup, n, k); OPV <- matrix(oc$pvalue, n, k)
    # No interval on a reference position (OR = 1 there by construction): the ref row and any
    # self-referencing column (the ref2/ref column; none for a binary col_var).
    rrm <- !is.na(or_refrows) & or_refrows
    OINF[rrm, ] <- NA_real_; OSUP[rrm, ] <- NA_real_; OPV[rrm, ] <- NA_real_
    if (!is.null(refcols_vector) && any(refcols_vector)) {
      OINF[, refcols_vector] <- NA_real_; OSUP[, refcols_vector] <- NA_real_; OPV[, refcols_vector] <- NA_real_
    }
    or_ci_inf <- data.table::copy(tabs_pct); set_cols(or_ci_inf, OINF)
    or_ci_sup <- data.table::copy(tabs_pct); set_cols(or_ci_sup, OSUP)
    or_pvalue <- data.table::copy(tabs_pct); set_cols(or_pvalue, OPV)
  }

  list(
    diff           = tabs_diff,
    ratio          = tabs_mean,
    rr             = tabs_rr,
    or             = tabs_or,
    or_ci_inf      = or_ci_inf,
    or_ci_sup      = or_ci_sup,
    or_pvalue      = or_pvalue,
    refcols_vector = refcols_vector,
    refrows        = refrows
  )
}


# leaf_ci_plain() -- Phase 19j (KEY 5): THE factor leaf's confidence interval, on matrices, FROM THE
# PLAN. It is tab_ci()'s per-cell arithmetic with the plan RECONSTRUCTION removed: the leaf knows
# `pct`, `ci`, `ci_scale`, `comp`, its reference row and its reference column, so nothing here re-reads
# an fmt marker, re-recycles a per-column vector or re-detects a total column. Shared verbatim by
# plain_core() and by the jamovi tier-3 re-reference (jmv_tab3_reref), so those two cannot fork.
#
# DESIGN: it lives BESIDE tab_apply_reference(), not inside it. `ci = "cell"` needs no reference at all
#   and must run when ref == "no", which is outside that function's own gate; and the Woolf log-OR
#   interval is genuinely internal there (three arms build three different 2x2s through the `or_cells`
#   closure). "One cell, one interval" still holds: the RULE is one (CI_GEOMS, R/tab-agg.R), and
#   `or_ci` / `ci` are mutually exclusive by construction (tab_resolve_settings()).
#
# @param P,tot_n   n x k matrices: the WEIGHTED proportion and its RAW unweighted base
# @param n_eff     n x k effective base, or NULL on inference basis "n"
# @param ci        the RESOLVED step value: "no" | "cell" | "diff"
# @param pct       "row" | "col" | "all" | "all_tabs" | "no"
# @param ci_scale  "diff" | "ratio"
# @param grp       integer/character(n): each row's comparison group (the tab_vars key, or one group)
# @param ref_row   logical(n): the reference ROW mask (the total rows when ref == "tot")
# @param totrow    logical(n): the total rows (diff_col reads the reference column's base THERE)
# @param refcol    integer(1): the reference COLUMN index, NA when there is none (pct == "col" only)
# @param totcol    logical(k): the total columns
# @return list(kind, inf, sup, pvalue, scale, method, visible) -- three n x k matrices plus the three
#   COLUMN-INVARIANT stamps (`scale` NA = the level scale stands).
leaf_ci_plain <- function(P, tot_n, n_eff = NULL, ci, pct, ci_scale = "diff",
                          grp, ref_row, totrow, refcol = NA_integer_, totcol,
                          conf_level = 0.95, stars = FALSE,
                          ci_method = default_ci_method(), degf = Inf) {
  n <- nrow(P); k <- ncol(P)
  none <- list(kind = "no", inf = NULL, sup = NULL, pvalue = NULL,
               scale = NA_character_, method = "", visible = FALSE)

  # (a) THE DIRECTION. tab_ci()'s eight-branch case_when collapses to this, because in a factor leaf
  # `pct_base` and `var_kind` are COLUMN-INVARIANT: `ci_able` is `pct != "no"` (a count column carries
  # no cell interval), `is_rm` is `pct == "row"`, and "all"/"all_tabs" can only take a cell interval.
  # "auto" never arrives -- the resolver settles it (tab_resolve_settings / resolve_leaf_ci).
  kind <- if (length(ci) == 0L || is.na(ci[1]) || identical(ci[1], "no") || identical(pct, "no")) "no"
          else if (identical(ci[1], "cell"))                                          "cell"
          else if (identical(pct, "row"))                                             "diff_row"
          else if (identical(pct, "col"))                                             "diff_col"
          else                                                                        "no"
  if (identical(kind, "no")) return(none)

  kind_base <- if (identical(kind, "cell")) "cell" else "diff"

  # (b) THE REFERENCE ROW, per comparison group.
  # WARNING: this is group_last_pos()'s LAST-in-group semantics, deliberately, NOT the FIRST that
  # tab_apply_reference()'s own ref_abs() takes. The two coincide on every reachable shape (a
  # calculate_refrows() mask holds exactly one TRUE per group), but the CI and the difference have
  # always been written that way and re-implementing it here costs four lines and removes the class.
  grp_last <- function(mask) {
    pos <- rep(NA_integer_, n)
    for (g in unique(grp)) { r <- which(grp == g); w <- which(mask[r])
                             if (length(w)) pos[r] <- r[[w[[length(w)]]]] }
    pos
  }
  rp    <- grp_last(ref_row)
  rtona <- !is.na(rp) & (seq_len(n) == rp)     # the cell's own reference row: no interval against itself

  # (c) THE BASES. `fmt_base()`'s coalesce, on matrices: the effective n where it is populated, the raw
  # percentage base otherwise. `n_raw` stays the RAW base -- ci_beta's df rescale needs it beside the
  # effective one, and it is neither coalesced nor NA'd.
  B <- tot_n * 1.0
  if (!is.null(n_eff)) { ok <- is.finite(n_eff); B[ok] <- n_eff[ok] }
  # Phase 19m-i: whether the reference cell keeps its OWN interval is ONE declared fact
  # (CI_GEOMS$ref_cell). A `ci = "cell"` interval compares each cell to 0 %, not to a reference, so
  # the total row's own percentage interval is exactly as descriptive as any other cell's -- this
  # leaf used to blank it under both kinds, while num_core() blanked it only under a contrast.
  X <- B
  if (identical(ci_geom_ref_cell(kind_base, "pct", ci_scale[1]), "na")) X[rtona, ] <- NA_real_

  REF <- REF_N <- NULL
  if (identical(kind, "diff_row")) {
    REF   <- P[rp, , drop = FALSE]
    REF_N <- B[rp, , drop = FALSE]
  } else if (identical(kind, "diff_col")) {
    if (is.na(refcol)) refcol <- 1L      # detect_refcol()'s fallback: the group's FIRST column
    # WARNING: the reference column's PROPORTION is read in the cell's own row, but its BASE at the
    # group's TOTAL row (tab_ci()'s `fmt_base(rcol)[group_last_pos(is_totrow(col))]`). Under pct = "col"
    # the raw base is constant down a column, so this is invisible unweighted -- and wrong on every
    # design-based col-% table, where `n_eff` varies per cell.
    tr    <- grp_last(totrow)
    REF   <- P[, rep(refcol, k), drop = FALSE]
    REF_N <- B[tr, rep(refcol, k), drop = FALSE]
  }

  res <- ci_dispatch(
    kind = kind_base, var_kind = "pct", ci_scale = ci_scale[1],
    est = as.vector(P), base = as.vector(X),
    ref = if (!is.null(REF)) as.vector(REF), ref_n = if (!is.null(REF_N)) as.vector(REF_N),
    n_raw = as.vector(tot_n),
    conf_level = conf_level, want_p = isTRUE(stars), method = ci_method, degf = degf)

  INF <- matrix(res$inf, n, k); SUP <- matrix(res$sup, n, k); PV <- matrix(res$pvalue, n, k)

  # (d) THE MASKING -- a total column has no reference row to differ from, a reference column no
  # reference column. Applied to the RESULTS, after the stamps are decided: the scale and the method
  # describe the whole col_var, and NA bounds are the data fact saying "no interval in this cell" (D19).
  if (identical(kind, "diff_row") && any(totcol)) {
    INF[, totcol] <- NA_real_; SUP[, totcol] <- NA_real_; PV[, totcol] <- NA_real_
  }
  if (identical(kind, "diff_col")) {
    INF[, refcol] <- NA_real_; SUP[, refcol] <- NA_real_; PV[, refcol] <- NA_real_
  }

  list(kind = kind, inf = INF, sup = SUP, pvalue = PV,
       scale   = ci_geom_scale( kind_base, "pct", ci_scale[1]),
       method  = ci_geom_method(kind_base, "pct", ci_scale[1], ci_method),
       visible = identical(kind, "cell"))
}






#' Means table
#' @description Cross categorical variables with numeric variables, and get a table
#' of means and standard deviations.
#' @param data A data frame.
#' @param row_var The row variable, which will be printed with one level per line. If
#' numeric, it will be used as a factor.
#' @param col_vars The numeric variables, which will appear in columns :
#' means and standard deviation are calculated for each levels of `row_var` and `tab_vars`.
#' @param tab_vars  <\link[tidyr:tidyr_tidy_select]{tidy-select}> Tab variables :
#' a subtable is made for each combination of levels of the
#' selected variables. Leave empty to make a simple cross-table. All tab variables
#' are converted to factor.
#' @param wt A weight variable, of class numeric. Leave empty for unweighted results.
#' @param digits The number of digits to print, as a single integer, or an integer vector the
#' same length as \code{col_vars}.
#' @param na The policy to adopt for missing values in row and tab variables (factors),
#' as a single string.
#'  \itemize{
#'   \item \code{"keep"}: by default, \code{NA}'s of row and tab variables
#'   are printed as an explicit `"NA"` level.
#'   \item \code{"drop"}: remove `NA`'s in row and tab variables.
#'   }
#' `NA`s in numeric variables are always removed when calculating means. For that reason
#' the `n` field of each resulting \code{\link{fmt}} column, used to calculate confidence
#' intervals, only takes into account the complete observations (without `NA`).
#' To drop all rows with `NA` in any numeric variable first, use \code{\link{tab_prepare}}
#' or the superseded \code{\link{tab_many}}'s `na_drop_all` argument.
#' @param totaltab The total table,
#' if there are subtables/groups (i.e. when \code{tab_vars} is provided) :
#'  \itemize{
#'   \item \code{"line"}: by default, add a general total line (necessary for
#'   calculations with \code{comp = "all"})
#'   \item \code{"table"}: add a complete total table
#'  (i.e. \code{row_var} by \code{col_vars} without \code{tab_vars}).
#'   \item \code{"no"}: not to draw any total table.
#'  }
#' @param totaltab_name The name of the total table, as a single string.
#' @param tot The totals :
#'  \itemize{
#'   \item \code{c("col", "row")} or \code{"both"} : by default, both total rows and total
#'   columns.
#'   \item \code{"row"}: only total rows.
#'   \item \code{"col"}: only total column.
#'   \item \code{"no"}: remove all totals (after calculations if needed).
#'  }
#' @param total_names The names of the totals, as a character vector of length one or two.
#' Use syntax of type \code{c("Total row", "Total column")} to set different names for
#' rows and cols.
#' @param ref The reference cell to calculate differences and ratios
#'  (used to print \code{colors}) :
#'  \itemize{
#'   \item \code{"auto"}: by default, cell difference from the corresponding total
#'   (rows or cols depending on \code{pct = "row"} or \code{pct = "col"}) is
#'   used for `diff` ; cell ratio from the first line (or col) is use for `OR`
#'   (odds ratio/relative risks ratio).
#'   \item \code{"tot"}: totals are always used.
#'   \item \code{"first"}: calculate cell difference or ratio from the first cell
#' of the row or column (useful to color temporal developments).
#'   \item \code{"last"}: the mirror of \code{"first"} — the **last level** of the row (or column)
#' variable. A total row or column is not a level and is never selected: use \code{"tot"} for that.
#' Resolved inside each subtable when there are \code{tab_vars}.
#'   \item \code{n}: when `ref` is an integer, the nth row (or column) is used for comparison.
#'   \item \code{"regex"}: when `ref` is a string, it it used as a regular expression,
#'   to match with the names of the rows (or columns). Be precise enough to match only one
#'   column or row, otherwise you get a warning message.
#'   \item \code{"no"}: not use ref and not calculate diffs to gain calculation time.
#' }
#' @param comp Comparison level. When \code{tab_vars} are present, should the
#' contributions to variance be calculated for each subtable/group (by default,
#'  \code{comp = "tab"}) ? Should they be calculated for the whole table
#'  (\code{comp = "all"}) ?
#'  \code{comp} must be set once and for all the first time you use \code{\link{tab_plain}},
#'  \code{\link{tab_num}} or \code{\link{tab_chi2}} with rows, or \code{\link{tab_ci}}.
#' @param color Which measure(s) to color, on which channel -- see \code{\link{tab}} for the full
#'   grammar (\code{FALSE}/\code{TRUE}, a measure name, or a positional two-channel
#'   \code{c("difference", "ratio")} vector). For numeric means the useful measures are
#'   \code{"difference"} (standardized, Glass's \eqn{\Delta}) and \code{"ratio"} (mean ratio);
#'   \code{TRUE} uses \code{"ratio"}. Default \code{"auto"} keeps the historical behavior.
#' @param display A \code{{}} display template applied to the built table -- the same grammar
#'   \code{\link{tab}} takes (e.g. \code{"{or}"}, \code{"{pct} {ci}"}, \code{"{pct} (n={n})"}),
#'   or the type-adaptive alias \code{"num_ci"}. Display only: it never changes what is computed.
#' @param color_signif How significance gates the color (\code{"ignore"} / \code{"grey_non_signif"}
#'   / \code{"guaranteed_effect"}) -- see \code{\link{tab}}.
#' @param color_breaks A per-table colour-threshold override -- see \code{\link{tab}}.
#' @param subtext A character vector to print rows of legend under the table.
#' @param conf_level The confidence level for the confidence intervals,
#'  as a single numeric between 0 and 1. Default to 0.95 (95%).
#' @param ci_method,design_effect See \code{\link{tab}}. Only the \code{mean_diff} / \code{mean_ratio}
#'  slots of \code{ci_method} are meaningful here (a numeric table has no proportion interval).
#' @param stars Logical (opt-in; default \code{FALSE}, or `options("tabxplor.stars")` when \code{NULL}).
#' With \code{ci = "ref"}, print per-cell Welch t significance stars for the difference from the
#' reference row; the mean-diff interval then uses the Welch t quantile (z when \code{FALSE}).
#' @param num Set to \code{TRUE} to obtain a table with normal numeric vectors (not `fmt`).
#' @param df  Set to \code{TRUE} to obtain a plain data.frame (not a `tibble`),
#' with normal numeric vectors (not `fmt`). Useful, for example, to pass the table to
#' correspondence analysis with \pkg{FactoMineR}.
#' @param .fine,.by_table Internal. `.fine` is a pre-computed moment-sum aggregate (from
#' \code{tab_aggregate_num()}) to adopt instead of scanning the raw data; `.by_table` forces
#' the table-by-table path (a fresh scan). Both default to the fresh-scan behaviour.
#'
#' @inheritParams tab
#'
#' @return A \code{tibble} of class \code{tabxplor_tab}. If \code{...} (\code{tab_vars})
#'  are provided, a \code{tab} of class \code{tabxplor_grouped_tab}.
#' All non-text columns are \code{\link{fmt}} vectors of class \code{tabxplor_fmt},
#' storing all the data necessary to print formats and colors. Columns with \code{row_var}
#' and \code{tab_vars} are of class \code{factor} : every added \code{factor} will be
#' considered as a \code{tab_vars} and used for grouping. To add text columns without
#' using them in calculations, be sure they are of class \code{character}.
#' @export
#'
#' @examples
#' \donttest{
#' data <- dplyr::storms |> tab_prepare(category, wind, na_drop_all = wind)
#' tab_num(data, category, wind, tot = "row",
#'         color = "difference", color_signif = "guaranteed_effect")
#' }
tab_num <- function(data, row_var, col_vars, tab_vars, wt,
                    color = "auto", display = NULL, color_signif = "ignore",
                    anova = NULL,
                    na = c("keep", "drop"),
                    ref = "tot", comp = c("tab", "all"),
                    ci = "auto", conf_level = conf_level_default(), stars = NULL, #ci_visible = FALSE,
                    ci_method = NULL, design_effect = NULL,
                    totaltab = "line", totaltab_name = "Ensemble",
                    tot = NULL, total_names = "Total",
                    subtext = "", digits = 0, num = FALSE, df = FALSE,
                    color_breaks = NULL,
                    .fine = NULL, .by_table = FALSE
) {
  # Phase 18z14-i: unwrap a survey design FIRST -- see tab_plain(); a no-op on the pipeline path.
  svy       <- svy_unwrap_data(data, "tab_num")
  if (!is.null(svy)) data <- svy$data
  # Phase 19i: the shared argument boundary (see tab()). It is also where `stars` is resolved now:
  # this leaf used to hand a possibly-NULL `stars` to resolve_leaf_ci() -- which tests
  # `isTRUE(stars)` -- and resolve it only much later inside num_core(), so
  # options(tabxplor.stars = TRUE) built a reference interval through tab_plain() and none here.
  # (`na` and `comp` carry a match.arg-style vector default, so take the first entry before the
  # boundary, which asks for one value.)
  .a <- tab_resolve_common_args(
    "tab_num", color = color, color_signif = color_signif, ci = ci, stars = stars,
    conf_level = conf_level, ci_method = ci_method, display = display, ref = ref,
    tot = tot, total_names = total_names, na = na[1], comp = comp[1], totaltab = totaltab,
    anova = anova, user_env = rlang::caller_env())
  ci_method <- .a$ci_method ; stars <- .a$stars ; display <- .a$display ; ref <- .a$ref
  total_names <- .a$total_names ; na <- .a$na ; comp <- .a$comp
  color_spec <- .a$color_spec ; color <- .a$color

  # Phase 19l: THE shared NSE preamble (leaf_defuse_vars, above). `plural = TRUE` -> several col_vars
  # + their tidyselect positions, which is the ONE thing this leaf needs that the factor one does not.
  .v <- leaf_defuse_vars(data, rlang::enquo(row_var), rlang::enquo(col_vars),
                         rlang::enquo(tab_vars), rlang::enquo(wt), svy = svy, plural = TRUE)
  data <- .v$data ; row_var <- .v$row_var ; col_vars <- .v$col ; tab_vars <- .v$tab_vars
  wt   <- .v$wt   ; pos_col_vars <- .v$pos_col_vars

  #forbid the level to have the name of the variable, othewise problems ----

  vctrs::vec_assert(ref, size = 1)
  digits <- vctrs::vec_recycle(vctrs::vec_cast(digits, integer()), length(col_vars))
  vctrs::vec_assert(totaltab_name, size = 1)
  # Phase 19c: a mean column can carry only a measure whose declared `applies_to` includes "num"
  # ("auto" is the resolver's own sentinel, resolved by resolve_color_auto_num just below). The
  # legacy composites cannot arrive -- normalize_color_spec decodes them into a clean measure plus
  # `color_signif` -- so what is left to check is exactly this one declared fact.
  stopifnot(color %in% c("auto", "no", "") || measure_applies(color, "num"))

  # Phase 17f: resolve the leaf's forcing cascade ONCE (shared with tab_transform), then hand the
  # resolved bundle to the compute core. Colour is finalised ONCE, here, after the core returns.
  # Phase 19d: `ci_scale` is CUT -- it was a pure duplicate of `color = "ratio"` (used 0 times
  # anywhere), and which scale the interval rides is the resolved comparison's to say. D29: the
  # gated forcing (a `color_signif` policy needs the interval it gates on) is applied HERE too, not
  # only inside tab_resolve_settings() -- without it the policy greyed every cell of a directly-built
  # numeric table. Same shared rule, so the two paths cannot drift again.
  r_ci  <- resolve_leaf_ci(ci, color, color_spec$signif, stars, ref)
  stars <- r_ci$stars ; color_spec$signif <- r_ci$color_signif
  ci    <- if (identical(r_ci$ci, "ref")) "diff" else r_ci$ci
  # The SAME comparison chain the pipeline resolves (`color` -> `display` -> the difference); a mean
  # column has no odds ratio, so the only geometry the chain can move here is the ratio.
  ci_scale <- if (identical(measure_key(color_spec$text), "ratio") ||
                  identical(measure_key(color), "ratio") ||
                  identical(display_comparison(display), "ratio")) "ratio" else "diff"
  r <- num_resolve(color, ref, ci, tot, comp, totaltab, row_var, col_vars, tab_vars)
  result <- num_core(
    data, row_var, col_vars, tab_vars, wt,
    color = r$color, na = na, ref = r$ref, comp = r$comp, ci = r$ci, ci_visible = r$ci_visible,
    stars = stars, ci_scale = ci_scale, totaltab = r$totaltab,
    totaltab_name = totaltab_name, tot = r$tot, total_names = total_names, subtext = subtext,
    digits = digits, num = num, df = df, .fine = .fine, .by_table = .by_table,
    # Phase 18z14-ii: tab_num(design, ...) gets the design-based mean intervals too; through the
    # same inference object tab_setup() builds for the pipeline.
    inference = new_inference(wt, svy$spec, conf_level, ci_method, design_effect = design_effect),
    anova = anova
  )

  # Phase 17f: df/num returns plain numbers (no fmt), so skip the colour finalise entirely.
  if (df || num) return(result)

  # The shared wrapper tail (a no-op finalise for a plain scalar colour passed straight through, e.g.
  # when tab_many() drives tab_num()). Phase 19e: the leaf carries a real `display` too, so the tail
  # is the SAME one tab()/tab_counts() run -- one grammar, one applier, no leaf-only degradation.
  finalize_color_tail(result, color_spec, color_breaks, display)
}


# num_resolve() -- Phase 17f: the numeric leaf's argument resolver (colour-auto -> ci -> ref -> tot ->
# comp -> totaltab forcing), shared by the public tab_num() wrapper and tab_transform() so the pipeline
# resolves the SAME way instead of the leaf re-deriving. Takes the already-normalised colour legacy +
# the raw ref/ci/tot/comp/totaltab; returns the resolved bundle. ref = "auto" is type-specific here
# (a mean's reference is its total row -> "tot"), the byte-identical counterpart of tab_plain's rule.
#' @keywords internal
#' @noRd
num_resolve <- function(color, ref, ci, tot, comp, totaltab, row_var, col_vars, tab_vars) {
  # Phase 7b: the numeric color = "auto" resolution is the means arm of the shared cascade,
  # in resolve_color_auto_num() (R/tab-resolve.R). A mean has no contrib/OR notion, so it keys
  # only on whether a real difference is possible (a `ref`, and ci != "cell"). Under tab_build()
  # this receives the resolved measure filtered by `applies_to` (never "OR"/"contrib"); direct
  # tab_num() callers also land here.
  color <- resolve_color_auto_num(color, ref, ci, row_var, col_vars)

  if (is_placeholder_var(row_var) | any(is_placeholder_var(col_vars))) color <- ""

  # Phase 19c: the FIFTH copy of "a comparison colour needs a reference" -- the measure's own
  # declared `requires["ref"]`. The numeric leaf warns and repairs where tab_resolve_settings()
  # aborts (a difference against the total row is always available on a mean table); 19d unifies the
  # two, which is why the rule is stated once even though it is applied twice.
  needs_ref <- measure_forces(color, "ref")
  if (needs_ref & ref %in% c("no", "")) {
    warning("since color = 'diff', ref must be provided and was set to 'tot'")
    ref <- "tot"
  }

  # Phase 19c: the `diff_ci` / `after_ci` arms that stood here are gone with the composite spellings
  # themselves. They forced `ci = "diff"` for a colour string that also named a significance policy;
  # the policy is `color_signif` now, and the forcing it needs is applied once, in
  # tab_resolve_settings() (the `requires["ci"] == "gated"` rule).
  if (is.null(ci)) ci <- "no"

  if (ci == "diff" & ref %in% c("no", "")) {
    warning("since ci = 'diff', a diff was added with ref = 'tot'")
    ref <- "tot"
  }

  ci_visible <- ci == "cell"


  if (is.null(tot)) {
    tot <- if (ref == "tot" & needs_ref) {"row"} else {"no"}

  } else {
    # ... and only the total ROW on the numeric leaf, which has no total-column notion. Two rules,
    # deliberately; one vocabulary, at the boundary.
    if (tot[1] == "both") tot <- "row"

    if ((needs_ref | ref == "tot") & !tot %in% "row") {
      #warning("since color = '", color, "' and ref = 'tot', a total row was added")
      tot <- "row"
    }
  }

  # LEAF resolution (Phase 7b): ref = "auto" is type-specific and intentionally stays here, NOT
  # in tab_resolve_settings(). A mean's reference is always its total row ("tot"); tab_num() has
  # no OR mode, so the factor rule's "first" branch (tab_plain, below) can never fire for means.
  # This is the documented (byte-identical) counterpart of tab_plain()'s ref = "auto" rule.
  if (ref == "auto") {
    ref <- "tot"  # ref <- if (OR != "no") {"first"} else {"tot"}
  }

  comp <- force_comp(comp, tab_vars)

  if (length(tab_vars) == 0) totaltab <- "no"

  if (comp[1] == "all" & ref == "tot" & !totaltab %in% c("table", "line")) {
    warning("since comp = 'all', a total table was added to compare with")
    totaltab <-  "line"
  }

  if (comp[1] == "all" & !ref %in% c("tot", "no", "") & totaltab != "table") {
    warning("since comp = 'all', a full total table was added to compare with")
    totaltab <- "table"
  }

  list(color = color, ref = ref, ci = ci, ci_visible = ci_visible,
       tot = tot, comp = comp, totaltab = totaltab)
}


# num_digits_floor() -- THE mean-magnitude digits floor: a column of small means needs more decimals
# than the user's `digits` asks for, or it prints as a wall of zeroes. ONE rule, two callers -- the
# numeric leaf (num_core, where the column is built) and the jamovi tier-4 re-paint
# (jmv_reapply_digits, which rewrites `digits` on a cached carrier and must reproduce the leaf
# exactly). Phase 19k: it was byte-duplicated in the two places.
# Phase 18p bug-fix: an all-NA numeric col_var makes every mean NA, so max(., na.rm = TRUE) leaks a
# base "no non-missing arguments to max" warning and returns -Inf -> coerce to 0 (the m <= 1 branch,
# which keeps the digits sane).
# @param digits The requested digits (a scalar).
# @param means  The column's mean cells (any length), or the already-computed max as a scalar.
#' @keywords internal
#' @noRd
num_digits_floor <- function(digits, means) {
  m <- suppressWarnings(max(means, na.rm = TRUE))
  if (!is.finite(m)) m <- 0
  if      (m <= 1 ) max(digits, 2L)
  else if (m <= 10) max(digits, 1L)
  else              digits
}


# num_core() -- Phase 17f: the numeric leaf's compute core. Consumes ALREADY-RESOLVED scalar settings
# (from num_resolve) + the resolved NSE syms; does the moment aggregate + mean/diff/ratio/CI + fmt build
# + totals + the tab_var_1lv wrap, and RETURNS THE PRE-FINALISE table. Colour is finalised ONCE by the
# caller (the public tab_num() wrapper, or tab()/tab_many()); num_core never finalises -> no double pass.
#' @keywords internal
#' @noRd
num_core <- function(data, row_var, col_vars, tab_vars, wt,
                     color, na, ref, comp, ci, ci_visible, stars, ci_scale,
                     totaltab, totaltab_name, tot, total_names,
                     subtext, digits, num, df, .fine, .by_table,
                     inference,                            # REQUIRED -- see plain_core()
                     anova = NULL) {

  # Phase 18z16-iiiii: ONE resolved inference object -- see plain_core().
  # Phase 19i: the six statements both leaves share are leaf_inference_setup() (which see: it also
  # settles `use_raw`, the aggregate-injection seam, whose rule "a design-based variance reads the
  # OBSERVATIONS, so the raw scan is mandatory" is the same on both sides). What stays here is what
  # this leaf alone needs: the design df and `tab_row_names`. (Phase 19j: the two numeric interval
  # METHODS are no longer unpacked -- `inference$method` travels whole and CI_GEOMS names its slot.)
  list2env(leaf_inference_setup(inference, .fine, .by_table), environment())
  des_rows          <- NULL
  # Phase 18z16-i (W7): the DESIGN's degrees of freedom (#PSU - #strata), Inf/NA otherwise. It
  # REPLACES the sample-based df of every mean pivot -- survey refers a design-based mean interval to
  # t(degf), never to t(n_eff - 1). df_or_design() is the no-op when there is no design df.
  degf      <- inference$degf

  tab_row_names <- purrr::map_chr(c(tab_vars, row_var), rlang::as_name)

  # Phase 18s: the effective n applies to the weighted mean CIs (already) AND is surfaced into the
  # per-cell `n_eff` FIELD, symmetric with the factor side. Phase 18z16-ii: that effective n is the
  # EXACT flat closed form (svy_flat_neff_mean) or the design variance, never Kish -- which survives
  # only as the degenerate-cell limit inside those producers. `want_neff`'s "can this input supply
  # one" twin is `num_served` below (per-col_var moment triples, not one Sigma w^2 column).
  # WARNING: unlike the factor leaf, `use_raw` under a design is a real change of PATH here -- the
  # numeric aggregate `fine_num` is normally adopted -- but not of VALUES: tab_aggregate_num() and
  # the scan branch call the same num_moment_scan(), which test-num-fuse-parity.R locks.

  if (use_raw) {
    # Phase k: convert labelled (haven/labelled) GROUPING columns (row_var/tab_vars) to value-label
    # factors for the direct tab_num() entry. The numeric col_vars are zapped to their codes by the
    # as.numeric() coercion just below, so they are left out here. Idempotent on the tab() path.
    data <- data |> tab_apply_val_labels(purrr::map_chr(c(tab_vars, row_var), rlang::as_name))
    data <- data |>
      dplyr::select(!!!tab_vars, !!row_var, !!!col_vars, !!wt,
                    tidyselect::any_of(if (design_on) svy_row_col else character())) |>
      dplyr::mutate(dplyr::across((!!wt | tidyselect::all_of(as.character(col_vars))) &
                                    !where(is.numeric), as.numeric)
      )

    #Faster with data.table
    data.table::setDT(data)

    # Remove NA's in factors here, otherwise they are kept in totals after
    if (na == "drop") data <- stats::na.omit(data, tab_row_names) # 0.5 sec

    # Phase 18z14-ii: `.svy_row` rides THROUGH na.omit (it is what keeps the design positions
    # aligned with the surviving rows), then leaves before num_moment_scan -- whose .SD would
    # otherwise scan it as one more numeric col_var.
    if (design_on) { des_rows <- data[[svy_row_col]]; data[, (svy_row_col) := NULL] }

    if (nrow(data) == 0) stop("data is of length 0 (possibly after filter or na = 'drop')")
  } else if (nrow(.fine) == 0) {
    stop("data is of length 0 (possibly after filter or na = 'drop')")
  }

  if (!use_raw) {
    # Adopt the prebuilt moment aggregate. copy(): the factor-key coercion + na-order relabel just
    # below mutate `tabs` by reference, so a reused/cached `.fine` must not be corrupted.
    tabs <- data.table::copy(.fine)

  } else {
    # Phase 2/7d: sufficient moment sums (n [, wn], s1 = Sigma[w]x, s2 = Sigma[w]x^2) in ONE grouped
    # pass; mean/var are derived afterwards by num_derive_stats() (R/tab-agg.R), replacing the old
    # weighted.var double scan. The scan itself lives in num_moment_scan() (R/tab-agg.R) so tab_num()
    # and tab_aggregate_num() share it verbatim. (The moment sums are ADDITIVE, so the total-row and
    # total-table blocks below are num_rollup()s of this aggregate, not extra N-scans.)
    tabs <- num_moment_scan(data, tab_row_names, col_vars, wt)
  }

  # The flat design's nPSU (Phase 18z16-ii), PER col_var: the number of observations that variable
  # is estimated from. It only feeds the finite-sample factor n/(n-1). Read off the AGGREGATE (which at
  # this point is the finest grain, with no total rows yet) rather than nrow(data), so the raw scan and
  # an adopted `.fine` give the same number -- test-num-fuse-parity.R measured the two disagreeing by
  # the NA count, which moves every interval by ~1e-6.
  n_obs_v <- vapply(as.character(col_vars),
                    function(v) sum(as.double(tabs[[paste0(v, "_n")]]), na.rm = TRUE), numeric(1))

  not_fct <- !purrr::map_lgl(dplyr::select(tabs, tidyselect::any_of(tab_row_names)), is.factor)
  if (any(not_fct)) {
    tabs[, names(not_fct)[not_fct] := purrr::map(.SD, as.factor),
         .SDcols = names(not_fct)[not_fct]]
  }

  if (na == "keep") {
    data.table::setorderv(
      tabs, tab_row_names, na.last = TRUE
    )[, paste0(tab_row_names) := lapply(.SD, forcats::fct_na_value_to_level, level = "NA"),
      .SDcols = tab_row_names]
  }







  #Calculate means and variances for all totals and subtotals
  # Phase 2 rollup: the total rows and the total table below are subtotals of the main aggregate.
  # Its moment sums (n [, wn], s1, s2) are ADDITIVE, so both are computed as ROLLUPS of a captured
  # copy of the main aggregate via num_rollup() (R/tab-agg.R) instead of two extra N-row re-scans.
  moment_cols <- setdiff(names(tabs), tab_row_names)
  main_agg    <- data.table::copy(tabs)

  if ("row" %in% tot | totaltab %in% c("line", "table")) {
    if (length(tab_vars) != 0) {
      group_vars <- c(as.character(tab_vars)) |> purrr::accumulate(~ c(.x, .y))
      group_vars <- c(rev(group_vars), list(character()))
    } else {
      group_vars <- list(character())
    }
    # Phase 6e KNOWN-BUG fix: when tot="no" but a total table is still built, keep ONLY the
    # grand total -- but as a length-1 LIST (the grand-total key `character()`), not the bare
    # `character()` that `dplyr::last()` returned. The bare vector made `map_dfr()` iterate zero
    # times -> an empty `tabs_tot` -> the `setorderv()`/`rbind()` below crashed with tab_vars
    # (and silently dropped the total table without). Now the grand total is actually computed.
    if (!"row" %in% tot) group_vars <- group_vars[length(group_vars)]


    # Phase 2 rollup: the total rows are subtotals of the main aggregate (moment sums are
    # additive), so sum them by each group_vars subset instead of re-scanning N rows. One path
    # for weighted and unweighted -- moment_cols carries _wn only when weighted.
    tabs_tot <- purrr::map_dfr(
      group_vars,
      ~ num_rollup(
        main_agg,
        by           = .,
        drop_keys    = as.character(c(tab_vars[!tab_vars %in% .], row_var)),
        moment_cols  = moment_cols,
        tab_vars_chr = as.character(tab_vars)
      )
    )

    # Fixed in Phase 6e (the grand-total grouping-set is now a length-1 LIST, see above) and
    # golden-locked by n_ci_tabvars / n_ci_tabvars_all; num_rollup() guarantees every tab_var is
    # present in tabs_tot. Phase 7d belt-and-suspenders: restrict the reorder/relabel to the
    # tab_vars actually present, so it is byte-identical in every real case (intersect == the full
    # set) and can only differ on the genuinely-absent-column path that used to crash.
    num_total_postprocess(tabs_tot, intersect(as.character(tab_vars), names(tabs_tot)),
                          na, tab_row_names)

    tabs <- rbind(tabs, tabs_tot)
    data.table::setorderv(tabs, tab_row_names)


  }

  #Calculate means and variances for total table
  if (totaltab == "table") {

    # Phase 2 rollup: the total table is the main aggregate summed by row_var (its tab_vars
    # collapsed to "Total"), reusing the additive moment sums instead of a third N-row re-scan.
    tabs_totaltab <- num_rollup(
      main_agg,
      by           = as.character(row_var),
      drop_keys    = as.character(tab_vars),
      moment_cols  = moment_cols,
      tab_vars_chr = as.character(tab_vars)
    )

    num_total_postprocess(tabs_totaltab, as.character(row_var), na, tab_row_names)

    tabs <- rbind(tabs, tabs_totaltab)
    data.table::setorderv(tabs, tab_row_names)


  }

  # Phase 2 (2.0.0): derive per-col_var mean and variance from the moment sums (<v>_n [, _wn],
  # _s1, _s2) the aggregate + totals scans produced, in ONE pass over the small bound table.
  # Reproduces the pre-2.0.0 stats::var (unweighted) / weighted.var (weighted) definitions
  # exactly and removes the old weighted.var double scan. See R/tab-agg.R.
  tabs <- num_derive_stats(tabs, col_vars, weighted = length(wt) != 0)

  # --- the per-cell effective base `_en` (Phase 18z16-i, W13) ------------------------------------
  # `n_eff` is a PROPERTY OF THE CELL -- "the effective sample size used for this cell's confidence
  # interval" -- so it is written whenever the basis asks for one, NOT (as before) only inside
  # `if (ci %in% c("cell","diff"))`. That gate is why the exported step path disagreed with itself:
  # tab_num(design) |> tab_ci("cell") returned intervals 1.6x too narrow while tab_plain(design) |>
  # tab_ci("cell") returned design-based ones, and the footer claimed the design in both. The factor
  # leaf always treated n_eff as a base (leaf_wide_pct); this makes the numeric leaf agree.
  # Basis "design" -> n_eff = s^2 / Var_design(x_bar), the mean twin of the factor leaf's
  # p(1-p)/Var_design(p) (R/survey-variance.R). Basis "weights" -> the Sigma w^2 form. Basis "n" ->
  # nothing written, exactly as before (`_en` absent -> the raw count downstream).
  cvs_all <- as.character(col_vars)
  # W9: what the table can actually carry (see plain_core). The moment sums are always there when the
  # scan produced them; a hand-supplied `.fine` without them cannot climb, and must not claim to.
  num_served <- design_on ||
    (want_neff && all(paste0(rep(cvs_all, each = 3L), c("_w2", "_w2s1", "_w2s2")) %in% names(tabs)))
  # z16-iiiii: LOCALS of this build, read by its own stamp at the tail -- see plain_core / leaf_inference.
  unserved <- identical(inference_basis, "weights") && !num_served
  degraded <- FALSE
  if (want_neff) {
    Vres <- if (design_on && !design_flat)
      svy_var_mean(prep  = svy_var_prep(inference$design, des_rows),
                   keys  = lapply(tab_row_names, function(v) svy_key_chr(tabs[[v]])),
                   n_tab = length(tab_vars),
                   mkeys = lapply(tab_row_names, function(v) svy_key_chr(data[[v]])),
                   xs    = stats::setNames(lapply(cvs_all, function(v) data[[v]]), cvs_all))
    Vm <- Vres$v
    if (design_on && !design_flat && is.null(Vm)) degraded <- svy_var_degraded(Vres$reason)
    for (v in cvs_all) {
      raw_n <- as.double(tabs[[paste0(v, "_n")]])
      has_m <- all(paste0(v, c("_w2", "_w2s1", "_w2s2")) %in% names(tabs))
      data.table::set(
        tabs, j = paste0(v, "_en"),
        value = if (!is.null(Vm)) {
          en <- tabs[[paste0(v, "_var")]] / Vm[, match(v, cvs_all)]
          ifelse(is.finite(en) & en > 0, en, raw_n)
        } else if (has_m) {
          # the flat closed form, exactly as on the factor side: the cell IS its own domain here, so
          # B = Sum(w) over the cell's rows and the three moment sums are the cell's own.
          en <- svy_flat_neff_mean(M = tabs[[paste0(v, "_mean")]], s2 = tabs[[paste0(v, "_var")]],
                                   W2 = tabs[[paste0(v, "_w2")]], W2X = tabs[[paste0(v, "_w2s1")]],
                                   W2X2 = tabs[[paste0(v, "_w2s2")]], B = tabs[[paste0(v, "_wn")]],
                                   n_obs = n_obs_v[[v]])
          ifelse(is.finite(en) & en > 0, en, raw_n)
        } else {
          raw_n
        })
    }
  }

  tt <- leaf_totrow_tottab(tabs, row_var, tab_vars)
  totrow_vector <- tt$totrow; tottab_vector <- tt$tottab; kind_vector <- tt$kind
  comp_group <- if (comp == "tab") { as.character(tab_vars) } else { character() }

  #Differences and confidence intervals
  if (!ref %in% c("no", "") | ci %in% c("cell", "diff")) {

    # Phase 17f: the ref-row derivation is the SHARED calculate_refrows() / diff_index() -- the same
    # executor tab_plain uses -- replacing tab_num's former inline copy + its diff_index_mean() twin.
    # diff_index(pct = "row") keys on row_var and ignores num_names, so col_vars is a placeholder.
    refrows <- calculate_refrows(
      tabs, ref = ref, comp = comp, tab_row_names = tab_row_names, tab_vars = tab_vars,
      row_var = row_var, tottab_vector = tottab_vector, totrow_vector = totrow_vector,
      num_names = col_vars
    )

    tabs[, "ref_rows___" := refrows]

    #Differences and ratios
    # Phase 2 (2.0.0): the numeric `diff` field is now a real DIFFERENCE (cell_mean - ref_mean);
    # the cell/ref RATIO (the old `diff` value) moves to the `ratio` field. Numeric coloring keeps
    # reading `ratio` against mean_breaks until Phase 5 (D3 interim). See decisions doc §3, §Phasing.
    if (!ref %in% c("no", "") ) {
      tabs[, paste0(col_vars, "_diff") := purrr::map(
        rlang::syms(paste0(col_vars, "_mean")),
        ~ eval(.) - dplyr::nth(eval(.), tidyr::replace_na(which(eval(rlang::sym("ref_rows___")))[1], 0) )
      ),
      by = eval(comp_group)]
      tabs[, paste0(col_vars, "_ratio") := purrr::map(
        rlang::syms(paste0(col_vars, "_mean")),
        ~ eval(.) / dplyr::nth(eval(.), tidyr::replace_na(which(eval(rlang::sym("ref_rows___")))[1], 0) )
      ),
      by = eval(comp_group)]
    }




    # Confidence intervals (Phase 3a): store real bounds (<v>_ci_inf / <v>_ci_sup) + the
    # per-cell significance <v>_pvalue, via the ci_pivot() engine (R/tab-agg.R). Means use the
    # z pivot for cell CIs and the Welch-t pivot for diff CIs when stars are on; the pvalue is
    # the Welch-t inversion p (universal CI-inclusion) -- NA for cell CIs and when stars are
    # opted out (one interval eval). See dev/tabxplor_2.0.0_decisions.md §20.
    if (ci %in% c("cell", "diff")) {
      stars_on <- resolve_stars(stars)
      want_p   <- isTRUE(stars_on) && ci == "diff"
      cvs      <- as.character(col_vars)

      # The per-cell effective base `_en` is written ABOVE, once, as a property of the cell (W13);
      # here it is only read. On basis "n" it is absent -> the raw count.
      if (!all(paste0(cvs, "_en") %in% names(tabs)))
        for (v in cvs) data.table::set(tabs, j = paste0(v, "_en"),
                                       value = as.double(tabs[[paste0(v, "_n")]]))

      if (ci == "diff") {
        # Broadcast the reference row's mean / var / effective-n within each comparison group
        # (the same `nth(., ref index within group)` idiom the diff/ratio block above uses).
        tabs[, paste0(cvs, "_refm") := purrr::map(
          rlang::syms(paste0(cvs, "_mean")),
          ~ dplyr::nth(eval(.), tidyr::replace_na(which(eval(rlang::sym("ref_rows___")))[1], 0))
        ), by = eval(comp_group)]
        tabs[, paste0(cvs, "_refv") := purrr::map(
          rlang::syms(paste0(cvs, "_var")),
          ~ dplyr::nth(eval(.), tidyr::replace_na(which(eval(rlang::sym("ref_rows___")))[1], 0))
        ), by = eval(comp_group)]
        tabs[, paste0(cvs, "_refn") := purrr::map(
          rlang::syms(paste0(cvs, "_en")),
          ~ dplyr::nth(eval(.), tidyr::replace_na(which(eval(rlang::sym("ref_rows___")))[1], 0))
        ), by = eval(comp_group)]
      }

      for (v in cvs) {
        m  <- tabs[[paste0(v, "_mean")]]
        vv <- tabs[[paste0(v, "_var")]]
        nn <- tabs[[paste0(v, "_en")]]
        # Phase 19j (KEY 5): ONE lookup in CI_GEOMS (R/tab-agg.R) instead of this leaf's own copy of
        # the rule -- a mean cell interval is Rule B's one-sample Student t(n-1) (14v-ii, §48), and a
        # contrast follows the measure the reader sees (ci_scale: a real ratio-of-means interval, else
        # the mean-difference one; the old path always used the difference bounds, so a ratio-coloured
        # mean showed them mislabelled as a ratio, decisions §48).
        res <- ci_dispatch(
          kind = ci, var_kind = "mean", ci_scale = ci_scale[1],
          est = m, base = nn, var = vv,
          ref     = if (ci == "diff") tabs[[paste0(v, "_refm")]],
          ref_var = if (ci == "diff") tabs[[paste0(v, "_refv")]],
          ref_n   = if (ci == "diff") tabs[[paste0(v, "_refn")]],
          conf_level = conf_level, want_p = want_p, method = inference$method, degf = degf)
        # A reference row has no CI/test AGAINST ITSELF -- but a `ci = "cell"` interval is not a
        # comparison, so it keeps its own. Phase 19m-i: that decision is ONE declared fact
        # (CI_GEOMS$ref_cell), shared with leaf_ci_plain() and tab_ci(); only the MECHANISM stays
        # local (this leaf NAs the RESULTS, the other two the BASE -- not equivalent on a mean cell).
        if (identical(ci_geom_ref_cell(if (ci == "diff") "diff" else "cell", "mean", ci_scale[1]),
                      "na")) {
          res$inf[refrows] <- NA_real_
          res$sup[refrows] <- NA_real_
          res$pvalue[refrows] <- NA_real_
        }
        data.table::set(tabs, j = paste0(v, "_ci_inf"), value = res$inf)
        data.table::set(tabs, j = paste0(v, "_ci_sup"), value = res$sup)
        data.table::set(tabs, j = paste0(v, "_pvalue"), value = res$pvalue)
      }

      # Basis "n" writes no effective base -- drop the raw-count scratch built just above so it never
      # reaches the `n_eff` field at the reshape below (unchanged behaviour on the classic basis).
      if (!want_neff) data.table::set(tabs, j = paste0(cvs, "_en"), value = NULL)
      if (ci == "diff")
        data.table::set(tabs, j = paste0(rep(cvs, each = 3L),
                                         c("_refm", "_refv", "_refn")), value = NULL)
    }

    tabs[, "ref_rows___" := NULL]
  }

  # G1: drop the Sigma(w^2) / Sigma(w^2 x) / Sigma(w^2 x^2) scratch (accumulated whenever weighted
  # since z16-i) before the reshape, so it never leaks into the fmt columns.
  w2_cols <- names(tabs)[stringi::stri_detect_regex(names(tabs), "_w2$|_w2s1$|_w2s2$")]
  if (length(w2_cols) > 0) data.table::set(tabs, j = w2_cols, value = NULL)





  #Make the final table with fmt vectors
  # remove(list = c("tabs_n", "tabs_wn", "tabs_pct", "tabs_diff", "tabs_ci", "refcols_vector", "refrows"))

  text_vars <- !purrr::map_lgl(tabs, is.numeric)
  NA_reals <- rep(NA_real_, nrow(tabs))


  tabs_n  <-
    data.table::setnames(tabs[, stringi::stri_detect_regex(names(tabs), "_n$"), with = FALSE] ,
                         function(.x) stringi::stri_replace_first_regex(.x, "_n$" , ""))

  tabs_wn  <-
    if (length(wt) != 0) {
      data.table::setnames(tabs[, stringi::stri_detect_regex(names(tabs), "_wn$"), with = FALSE] ,
                           function(.x) stringi::stri_replace_first_regex(.x, "_wn$" , ""))
    } else {
      list(NA_reals)
    }

  tabs_mean  <-
    data.table::setnames(tabs[, stringi::stri_detect_regex(names(tabs), "_mean$"), with = FALSE] ,
                         function(.x) stringi::stri_replace_first_regex(.x, "_mean$" , ""))

  #Nan to NA
  tabs_mean <- tibble::as_tibble(tabs_mean) |>
    dplyr::mutate(dplyr::across(
      where(~ any(is.nan(.))),
      ~ dplyr::if_else(is.nan(.), NA_real_, .)
    )) |>
    data.table::as.data.table()


  # WARNING: tab_num reshapes by column-name SUFFIX (_n/_wn/_mean/_var/_diff/_ci) — fragile.
  # "no_row_var" ends in "_var" and would be mis-detected as a variance column, hence the
  # explicit exclusion. A numeric col_var whose name ends in one of these suffixes would
  # likewise be mis-parsed.
  tabs_var  <-
    data.table::setnames(tabs[, stringi::stri_detect_regex(names(tabs), "_var$") &
                                !is_placeholder_var(names(tabs)),
                              with = FALSE],
                         function(.x) stringi::stri_replace_first_regex(.x, "_var$" , ""))


  are_diff <- stringi::stri_detect_regex(names(tabs), "_diff$")
  tabs_diff  <-
    if (any(are_diff)) {
      data.table::setnames(tabs[, are_diff, with = FALSE] ,
                           function(.x) stringi::stri_replace_first_regex(.x, "_diff$" , ""))
    } else {
      list(NA_reals)
    }

  are_ratio <- stringi::stri_detect_regex(names(tabs), "_ratio$")
  tabs_ratio  <-
    if (any(are_ratio)) {
      data.table::setnames(tabs[, are_ratio, with = FALSE] ,
                           function(.x) stringi::stri_replace_first_regex(.x, "_ratio$" , ""))
    } else {
      list(NA_reals)
    }

  # Phase 3a: reshape the real CI bounds + per-cell pvalue (were a single symmetric half-width).
  reshape_suffix <- function(sfx) {
    hit <- stringi::stri_detect_regex(names(tabs), paste0(sfx, "$"))
    if (any(hit)) {
      data.table::setnames(tabs[, hit, with = FALSE],
                           function(.x) stringi::stri_replace_first_regex(.x, paste0(sfx, "$"), ""))
    } else {
      list(NA_reals)
    }
  }
  tabs_ci_inf <- reshape_suffix("_ci_inf")
  tabs_ci_sup <- reshape_suffix("_ci_sup")
  tabs_pvalue <- reshape_suffix("_pvalue")
  # Phase 18s: surface the kept per-cell effective n into the n_eff field (selected by
  # EXACT scratch names to avoid the reshape-by-suffix collision the WARNING above flags, then dropped).
  tabs_neff <-
    if (want_neff && all(paste0(as.character(col_vars), "_en") %in% names(tabs))) {
      data.table::setnames(tabs[, paste0(as.character(col_vars), "_en"), with = FALSE],
                           as.character(col_vars))
    } else { list(NA_reals) }

  tabs_text <- tabs[, text_vars, with = FALSE]

  if (ref %in% c("tot", "no", "")) refrows <- rep(FALSE, nrow(tabs))


  # Phase 7f-1: display / ref / comp are column-invariant (scalars for this tab_num call) -- compute
  # once. `digits` and `col_var` stay per-column (digits reads the per-column mean magnitude ..3);
  # the per-column case_when becomes a base if/else (scalar conditions, only one branch evaluated) --
  # byte-identical. NA_reals is reused for the always-NA fields (pct/ctr/tot_n/or) new_fmt defaults.
  display_1 <- if (ci_visible) { "mean_ci" } else { "mean" }
  # Phase 19b (KEY 2): what these columns estimate. A cell interval leaves the column a LEVEL (a mean
  # with its own interval is still a mean); a contrast interval makes it a mean DIFFERENCE, or a
  # ratio of means when that is the scale asked for.
  # ... and WHICH engine built its bounds (D8). A one-sample cell interval on a mean is a Student t
  # pivot -- which the legend used to announce as "Welch t", because it had to pick a slot back out
  # of a table-wide vector by measure.
  # Phase 19j: both are the CI_GEOMS row that chose the engine above. A NA scale_key means the level
  # scale stands, which covers `ci = "no"` and `ci = "cell"` alike.
  scale_num  <- ci_geom_scale(ci, "mean", ci_scale[1])
  if (is.na(scale_num)) scale_num <- "level_mean"
  method_num <- ci_geom_method(ci, "mean", ci_scale[1], inference$method)
  ref_1     <- switch(as.character(ref), "no" = "", "tot" = "tot", as.character(ref))
  comp_1    <- dplyr::if_else(ref != "no" | ci != "no", comp == "all", NA)
  NA_reals  <- rep(NA_real_, nrow(tabs_n))

  tabs <-
    list(tabs_n, tabs_wn, tabs_mean, tabs_var, tabs_diff, tabs_ci_sup, as.character(col_vars),
         digits, tabs_ratio, tabs_ci_inf, tabs_pvalue, tabs_neff) |>
    # Phase 9b-3: build the plain carrier column (frame + meta) then materialize via the single
    # fmt_materialize_col() (== the former inline new_fmt, byte-identical). The digits mean-magnitude
    # floor stays per-column (reads this column's means ..3). Phase 3a: real asymmetric CI bounds +
    # per-cell significance (mean CIs symmetric around the estimate, stored as absolute bounds).
    purrr::pmap_dfc(function(...) {
      a <- list(...)
      digits_col <- vec_recycle(num_digits_floor(a[[8]], a[[3]]), length(a[[1]]))
      fmt_materialize_col(
        frame = list(
          n         = a[[1]], display = display_1, digits = digits_col,
          wn        = a[[2]], pct = NA_reals, mean = a[[3]], diff = a[[5]], ratio = a[[9]],
          ctr       = NA_reals, var = a[[4]], ci_inf = a[[10]], ci_sup = a[[6]],
          pvalue    = a[[11]], or = NA_reals, tot_n = NA_reals, n_eff = a[[12]],
          row_kind  = kind_vector, in_tottab = tottab_vector, in_refrow = refrows),
        meta  = list(
          # Phase 19b: the numeric leaf computes its OWN interval (unlike the factor one, which waits
          # for tab_ci()), so it stamps the finished scale here. 14v-ii: a ratio-scale mean interval
          # lives on `mean_ratio` (neutral 1), so ci_center()/format()/the colour gate read the ratio
          # bounds, not a difference mislabelled as a ratio.
          scale     = scale_num, comp_all = comp_1, ref = ref_1,
          pct_base  = "none", ci_method = method_num,
          col_var   = a[[7]],
          totcol    = FALSE, refcol = FALSE, color = color, color_signif = "ignore")
      )
    })

  tabs <- dplyr::bind_cols(tibble::as_tibble(tabs_text), tabs)


  tabs <- leaf_rename_totals(tabs, row_var, tab_vars, tot, total_names, totaltab, totaltab_name,
                             tottab_vector, totrow_vector)







  # Phase 19i: the shared result tail. It is what closes the numeric leaf's `meta` gap -- this leaf
  # recorded NO meta at all, so a direct tab_num() had no `spec$kind` (tab_kind() fell back to its
  # degraded guess) and no `vars$wt` (nothing for the "Weighted by" footer to read); tab() masked it
  # by setting the meta itself at assemble. Also: the inference stamp here is the ONLY one of the two
  # (the assembler no longer overwrites the leaves' basis), so a factor block whose design variance
  # succeeded keeps "design" beside a numeric block that fell back -- the table-level answer being
  # the weakest of its columns (tab_inference_basis()).
  leaf_finish(tabs, row_var, tab_vars, wt, subtext, inference, unserved, degraded, df, num,
              anova = anova)
}
