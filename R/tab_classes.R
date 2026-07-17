# PURPOSE: S3 class definitions for tabxplor_tab/grouped_tab, dplyr method dispatch,
#   print methods, tab_kable(), tab_plot(), tab_compact(), color palettes and breaks.
# ROLE: Ensures tabxplor tables survive dplyr operations and print with colors.
# KEY CONSTRAINTS:
#   - Every dplyr verb needs an S3 method for tabxplor_grouped_tab. Missing one = silent
#     class downgrade to plain tbl_df (attributes lost, printing breaks).
#   - The dplyr_row_slice/dplyr_col_modify/dplyr_reconstruct trio is the core mechanism.
#     Each calls lv1_group_vars() to decide: downgrade to tabxplor_tab or keep grouped.
#   - Color palettes (6 sets) and break logic live here, shared with fmt_class.R and tab_xl.R.
# See: CLAUDE.md § Design Decisions > dplyr Integration.

# Special tibble class needed for printing, even if the most meaningful attributes
#  where passed to fmt class variables (only chi2 and subtext remains at tab level) :
#  the implementation relies on "grouped_df" class structure, and to manage it, it is
#  necessary to add one method for class "tabxplor_grouped_tab" for each dplyr function...
#  (Thank to Giulia Pais, Davis Vaughan and Hadley Wickham,
#   https://github.com/tidyverse/dplyr/issues/5480).

# grouped_tab class still don't handle [] ----

# Problem with methods for dplyr::filter, because it replaces base::filter,
# which cannot be detached in namespace

# #Import dplyr in NAMESPACE :
# # dplyr is imported as a "Depends" package, otherwise dplyr::filter, needed for methods,
# # cannot be found by roxygen2 because it replaces base::filter.
#
# #' Internal dplyr methods
# #' @rawNamespace import(dplyr, except = data_frame)
# #  otherwise, conflict with vctrs. Thanks to Thomas :
# #  https://stackoverflow.com/questions/51899220/import-all-the-functions-of-a-package-except-one-when-building-a-package
# #' @keywords internal
# #' @name tabxplor-dplyr
# NULL

# #' To allow dplyr::filter to be used for methods
# #' @rawNamespace import(base, except = filter)
# #' @keywords internal
# #' @name no_base_filter
# NULL


# Create class tabxplor_tab --------------------------------------------------------------
# sloop::s3_methods_class("tbl")
# sloop::s3_get_method(print.tbl)
# cli::cat_line()
# sloop::s3_get_method(format.tbl)
# tibble::trunc_mat #Gives classes :
# c("trunc_mat_single_tab", "trunc_mat_tbl_df", "trunc_mat_tbl", "trunc_mat_data.frame", "trunc_mat")
# sloop::s3_methods_class("tibble::trunc_mat")
# sloop::s3_get_method(format.tibble::trunc_mat)
# sloop::s3_get_method(print.tibble::trunc_mat)
# sloop::s3_methods_class("pillar_colonnade")
# sloop::s3_get_method(format.pillar_colonnade)
# sloop::s3_get_method(print.pillar_colonnade)
# pillar::squeeze
# sloop::s3_methods_class("single_tab")

#' A constructor for class tabxplor_tab
#'
#' @param tabs A table, stored into a \code{\link[tibble]{tibble}} data.frame.
#' It is generally made with \code{\link{tab}}, \code{\link{tab_many}}
#' or \code{\link{tab_plain}}.
#' @param subtext A character vector to print legend lines under the table.
#' @param test A tidy tibble storing whole-table test results (Chi2 for factor columns,
#' ANOVA F for mean columns), filled by \code{\link{tab_chi2}}. Renamed from \code{chi2}
#' in tabxplor 1.4.0.
#' @param chi2 `r lifecycle::badge("deprecated")` Soft-deprecated alias of \code{test}.
#' @param render_extras Display-only intent for the \code{add_n} / \code{add_pct} extras, as
#' \code{list(add_n =, add_pct =)}. Since tabxplor 1.4.0 those rows/columns are no longer baked
#' into the table: they are materialised at print/export time from this attribute. \code{NULL}
#' (the default) means no extras.
#' @param ci_settings Display-only metadata for the colour legend, as
#' \code{list(conf_level =, method_cell =, method_diff =)}: which confidence level and confidence
#' interval methods were actually used. \code{NULL} (the default) makes the legend fall back to
#' the package defaults.
#' @param vars The table's variable roles, as
#' \code{list(row_vars =, col_vars =, tab_vars =, compacted =)}, recorded when the table is built
#' rather than guessed back from it afterwards. \code{NULL} (the default) makes
#' \code{tab_get_vars()} fall back to detecting them from the column types.
#' @param ... Needed to implement subclasses.
#' @param class Needed to implement subclasses.
#'
#' @return A \code{tibble} of class \code{tabxplor_tab}.
#' @export
#  @examples
new_tab <-
  function(tabs = tibble::tibble(), subtext = "",
           test = new_test_tibble(), chi2 = NULL,
           render_extras = NULL, ci_settings = NULL, vars = NULL,
           ..., class = character()) {
    stopifnot(is.data.frame(tabs))
    #vec_assert(subtext    , character())

    # Soft-deprecated `chi2` arg (renamed `test` in 1.4.0): if supplied, it feeds `test`.
    if (!is.null(chi2)) test <- chi2

    out <- tibble::new_tibble(tabs, subtext = subtext, test = test, ...,
                              nrow = nrow(tabs), class = c(class, "tabxplor_tab"))
    # Phase 10i-B: `render_extras` (list(add_n=, add_pct=)) is the display-only intent for the add_n /
    # add_pct extras -- set only when supplied (a NULL attribute would be dropped anyway), so tables
    # never given it (raw tab_plain, older objects) simply have no attribute -> materialiser no-op.
    if (!is.null(render_extras)) attr(out, "render_extras") <- render_extras
    # Phase 13b: `ci_settings` (list(conf_level=, method_cell=, method_diff=)) is display-only metadata
    # for the colour legend (which CI method / confidence level was actually used). Carried like
    # `render_extras`; absent -> the legend falls back to package defaults.
    if (!is.null(ci_settings)) attr(out, "ci_settings") <- ci_settings
    # Phase 14d: `vars` (list(row_vars=, col_vars=, tab_vars=, compacted=)) is the table's own record
    # of its variable roles. Absent -> tab_get_vars()/tab_render_vars() fall back to the column-type
    # heuristic (hand-built tables, tab_plain(), older objects).
    if (!is.null(vars)) attr(out, "vars") <- vars
    out
  }

#' @param groups The grouping data.
#' @rdname new_tab
#' @return A \code{tibble} of class \code{tabxplor_grouped_tab}.
#' @export
new_grouped_tab <-
  function(tabs = tibble::tibble(), groups,
           subtext = "",
           test = new_test_tibble(), chi2 = NULL,
           render_extras = NULL, ci_settings = NULL, vars = NULL,
           ..., class = character()) {
    if (missing(groups)) groups <- attr(tabs, "groups")
    class <- c(class, c("tabxplor_grouped_tab", "grouped_df"))

    # Soft-deprecated `chi2` arg (renamed `test` in 1.4.0): if supplied, it feeds `test`.
    if (!is.null(chi2)) test <- chi2

    new_tab(tabs, groups = groups,
            subtext = subtext, test = test, render_extras = render_extras,
            ci_settings = ci_settings, vars = vars,
            ...,
            class = class)
  }



# Functions to work with class tabxplor_tab ----------------------------------------------

# Useful test fonction :
#' @describeIn tab_many a test function for class tabxplor_tab
#' @param x A object to test with \code{\link{is_tab}}.
#' @return A single logical.
#' @export
is_tab <- function(x) {
  inherits(x, "tabxplor_tab")
}

get_subtext <- purrr::attr_getter("subtext")

# Phase 3b: the whole-table test results (Chi2 for factor col_vars, ANOVA F for mean col_vars,
# future tests) live in the `test` table attribute -- a TIDY tibble, one row per
# (subtable x col_var x test-type). Renamed from the pre-1.4.0 `chi2` attribute (§16/§17).
# get_test() reads `test`, FALLING BACK to the old `chi2` attribute name (older objects /
# robustness); get_chi2() is kept as a working back-compat alias so pre-1.4.0 user code runs.
get_test <- function(x) {
  out <- attr(x, "test", exact = TRUE)
  if (is.null(out)) out <- attr(x, "chi2", exact = TRUE)
  out
}
get_chi2 <- function(x) get_test(x)

# set_test() -- write the whole-table `test` tibble attribute on a built table. Used by the
# jmvtab tier-2 cache (Phase 7e) to inject a cached chi2/ANOVA result instead of recomputing it.
set_test <- function(x, test) {
  attr(x, "test") <- test
  x
}

# Phase 10i-B: `render_extras` -- the DISPLAY-only intent for the add_n / add_pct extras, a small
# table-level list `list(add_n = <lgl>, add_pct = <lgl>)`. The built tab() no longer carries the add_n
# `n` column / add_pct `col_pct` column-or-rows; it stores this intent (born in tab_assemble_tables)
# and tab_materialize_extras() re-creates the rows/cols at display. Carried through dplyr verbs exactly
# like `subtext`/`test` (every S3 method threads `render_extras = get_render_extras(...)`); the vctrs
# reconcilers take x's (a scalar intent, not row-bound like `test`). NULL -> no extras.
get_render_extras <- purrr::attr_getter("render_extras")
set_render_extras <- function(x, render_extras) {
  attr(x, "render_extras") <- render_extras
  x
}

# Phase 13b: `ci_settings` -- display-only metadata for the colour legend, a small table-level list
# `list(conf_level = <num>, method_cell = <chr>, method_diff = <chr>)` recording which CI method /
# confidence level tab()/tab_ci() actually used, so tab_color_legend() can name it accurately (e.g.
# "Newcombe score interval, 95% confidence"). Born in tab_assemble_tables(); carried through dplyr
# verbs exactly like `render_extras`. Distinct attribute (NOT folded into render_extras) so it
# survives tab_materialize_extras()'s `set_render_extras(NULL)` clear. get_ci_settings() falls back to
# the package defaults when absent (heavy dplyr chains / raw tab_plain / older objects).
get_ci_settings <- purrr::attr_getter("ci_settings")
set_ci_settings <- function(x, ci_settings) {
  attr(x, "ci_settings") <- ci_settings
  x
}

# Phase 14d: `vars` -- the table's OWN record of its variable roles,
# `list(row_vars = <chr>, col_vars = <chr>, tab_vars = <chr>, compacted = <lgl>)`, written where the
# truth is known (tab_assemble_tables / tab_compact / tab_counts / tab_reg) and read by
# tab_get_vars() / tab_render_vars().
# WHY: the roles CANNOT be recovered from a built table. tab_compact() renames column 1 to the literal
# "levels" and stores the row-variable names only as factor LEVELS of a synthetic column named
# "row_var" -- so the "last factor column is the row_var, the others are tab_vars" heuristic reports
# `row_var = "levels", tab_vars = "row_var"` on a merged table that has no tab_vars at all. That is why
# tab_transpose() aborted with a message about tab_vars that were never there, and why a tab_xl title
# read "levels by multi (tabbed by row_var)". Sniffing for a column NAMED "row_var" would be the
# ad-hoc layer this replaces: record the roles instead of inferring them.
# `compacted` = several row_vars were merged into one table (so `row_vars` has length > 1 and the
# row-variable name lives in the `row_var` column's values, not in a column name).
# The heuristic stays as the fallback for hand-built tables (tab_plain(), a raw tibble of fmt columns,
# an object from an older version), so nothing user-facing breaks.
get_vars_attr <- purrr::attr_getter("vars")
set_vars_attr <- function(x, vars) {
  attr(x, "vars") <- vars
  x
}
new_vars_attr <- function(row_vars = character(0), col_vars = character(0),
                          tab_vars = character(0), compacted = FALSE) {
  list(row_vars = as.character(row_vars), col_vars = as.character(col_vars),
       tab_vars = as.character(tab_vars), compacted = isTRUE(compacted))
}
# The package CI defaults (mirror tab()'s formals), used when a table carries no `ci_settings`.
default_ci_settings <- function() {
  list(conf_level = 0.95, method_cell = "wilson", method_diff = "newcombe")
}

# === SECTION: the ONE table-attribute carry (Phase 14d) ============================================
# Every table-level attribute is listed HERE, once. Before this, each of the ~34 dplyr S3 methods /
# vctrs reconcilers named all of them by hand, so `subtext`/`test`/`render_extras`/`ci_settings` each
# paid the same ~34-site edit; a table that lost an attribute lost it silently, in one verb only.
# Adding an attribute is now: a `new_tab()` formal, a getter/setter, and one line in tab_attrs().
# WARNING: `test` is ROW-BOUND (one row per subtable x col_var), so a bind must vec_rbind it -- that
# is why the vctrs reconcilers still name it explicitly and only take tab_attrs() for the rest.
#' @keywords internal
tab_attrs <- function(from) {
  list(subtext       = get_subtext(from),
       test          = get_test(from),
       render_extras = get_render_extras(from),
       ci_settings   = get_ci_settings(from),
       vars          = get_vars_attr(from))
}

# Rebuild `out` as the right tab class, carrying every table attribute of `from`. `lv1_group_vars()`
# is the auto-downgrade: one grouping level left -> a plain tab.
#' @keywords internal
tab_restore <- function(out, from, attrs = tab_attrs(from)) {
  if (lv1_group_vars(out)) {
    rlang::exec(new_tab, out, !!!attrs)
  } else {
    rlang::exec(new_grouped_tab, out, dplyr::group_data(out), !!!attrs)
  }
}

# The attribute reconcile for a BIND of two tables (the vctrs ptype2/cast pair). `subtext` unions;
# `test` is ROW-BOUND (one row per subtable x col_var) so it rbinds; everything else is a scalar
# intent -> x's, falling back to the other's.
#' @keywords internal
tab_bind_attrs <- function(x, other) {
  a <- tab_attrs(x)
  b <- tab_attrs(other)
  for (nm in setdiff(names(a), c("subtext", "test"))) if (is.null(a[[nm]])) a[[nm]] <- b[[nm]]
  subtext <- unique(vctrs::vec_c(get_subtext(x), get_subtext(other)))
  if (length(subtext) > 1) subtext <- subtext[subtext != ""]
  a$subtext <- subtext
  a$test    <- vctrs::vec_rbind(get_test(x), get_test(other))
  a
}


# Phase 10i-B back-compat shim -- `tabs$n` / `tabs[["n"]]` / `pull(tabs, "n")` (and `col_pct`). add_n /
# add_pct are now DISPLAY-only, so the built tab has no `n` / `col_pct` column; old user code reading
# that column would get NULL. When the column is ABSENT but WAS requested (the render_extras intent),
# reconstruct it from the Total column -- byte-identical to the old add_n/add_pct column -- with a
# soft-deprecation (removed in a future version). Only a genuine COLUMN reconstruction applies (pct=
# "row"); under pct="col" add_n/add_pct were ROWS, so there was never an `n`/`col_pct` column -> NULL.
# The accessors below GATE on `%in% names(x)`, so the normal fast path pays nothing.
#' @keywords internal
tabxplor_deprecated_column <- function(x, name, user_env = rlang::caller_env(2)) {
  if (length(name) != 1L || is.na(name) || !name %in% c("n", "col_pct")) return(NULL)
  re   <- get_render_extras(x)
  want <- (name == "n" && isTRUE(re$add_n)) || (name == "col_pct" && isTRUE(re$add_pct))
  if (!want) return(NULL)
  hyd  <- tryCatch(tab_materialize_extras(x, backend = "xl", pvalue = FALSE),
                   error = function(e) NULL)
  # `nrow` guard: pct="col" add_n/add_pct is a ROW (name not a column) -> fall through to NextMethod.
  if (is.null(hyd) || !name %in% names(hyd) || nrow(hyd) != nrow(x)) return(NULL)
  lifecycle::deprecate_soft(
    "1.4.0", I(paste0("`$", name, "` on a tabxplor tab")),
    details = c(
      paste0("The `", name, "` column is now added only at display; it is reconstructed here from ",
             "the Total column and will stop being reconstructed in a future version."),
      i = paste0("Read it from the printed / exported table, or use `get_n()` on the `Total` column.")),
    user_env = user_env)
  hyd[[name]]
}

#' Extract a column of a tabxplor tab (with the Phase 10i-B add_n/add_pct back-compat shim)
#' @param x A \code{tabxplor_tab}.
#' @param i A column name.
#' @param name For \code{$}, a column name. For \code{\link[dplyr:pull]{dplyr::pull}}, the column
#' to use to name the result -- see its documentation.
#' @param ... Passed on.
#' @return The column, or the reconstructed add_n/add_pct column (deprecated), or the base method's value.
#' @method $ tabxplor_tab
#' @export
`$.tabxplor_tab` <- function(x, name) {
  if (name %in% names(x)) return(.subset2(x, name))          # fast path (exact, no partial matching)
  shim <- tabxplor_deprecated_column(x, name, user_env = rlang::caller_env())
  if (!is.null(shim)) return(shim)
  NextMethod()
}

#' @rdname cash-.tabxplor_tab
#' @method [[ tabxplor_tab
#' @export
`[[.tabxplor_tab` <- function(x, i, ...) {
  if (...length() == 0L && is.character(i) && length(i) == 1L && !i %in% names(x)) {
    shim <- tabxplor_deprecated_column(x, i, user_env = rlang::caller_env())
    if (!is.null(shim)) return(shim)
  }
  NextMethod()
}

#' @rdname cash-.tabxplor_tab
#' @importFrom dplyr pull
#' @param var See \code{\link[dplyr:pull]{dplyr::pull}}.
#' @param .data A \code{tabxplor_tab}.
#' @method pull tabxplor_tab
#' @export
pull.tabxplor_tab <- function(.data, var = -1, name = NULL, ...) {
  # Capture `var` as a quosure and inspect its name. Only a bare/`"..."` name of a DEPRECATED, ABSENT
  # display-only column is intercepted; everything else DELEGATES to dplyr's pull on the DECLASSED
  # tibble with the quosure RE-INJECTED (`!!vq`) -- this preserves tidy-select's NSE, which a plain
  # NextMethod() would break (the quosure environment gets rebound).
  vq  <- rlang::enquo(var)
  lbl <- tryCatch(rlang::as_name(vq), error = function(e) NULL)
  if (!is.null(lbl) && lbl %in% c("n", "col_pct") && !lbl %in% names(.data)) {
    shim <- tabxplor_deprecated_column(.data, lbl, user_env = rlang::caller_env())
    if (!is.null(shim)) return(shim)
  }
  dplyr::pull(tibble::as_tibble(.data), !!vq, name = {{ name }}, ...)
}

#' @rdname cash-.tabxplor_tab
#' @method pull tabxplor_grouped_tab
#' @export
pull.tabxplor_grouped_tab <- pull.tabxplor_tab

# The empty-placeholder `test` tibble (used before any test has run). Tidy schema: adding a new
# test type = adding rows (never a schema change); tab_var columns are added when populated.
# Phase 9b-3: memoized -- tibble() validation is ~1.4 ms/call and this placeholder is built several
# times per table (~3% of the build). The empty tibble is STATELESS, so the cached copy is shared
# safely (R copy-on-modify: any caller edit -- bind_rows / mutate / attr<- -- copies first, never
# touching the base). Byte-identical: same object tibble() produced.
new_test_tibble <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) {
      cached <<- tibble::tibble(row_var   = character(), col_var   = character(), test = character(),
                                statistic = double()   , df1       = double()   ,
                                df2       = double()   , pvalue    = double()   ,
                                n         = double()   , variance  = double()   , min_e = double())
    }
    cached
  }
})

# Pick the DISPLAYED test row per (subtable x col_var): chi2 for factor col_vars, and for mean
# col_vars the option-selected ANOVA F (Welch by default). Both F rows are stored; this chooses one.
test_display_rows <- function(test_tbl, anova = getOption("tabxplor.anova", "welch")) {
  keep_f <- paste0("F_", anova)
  dplyr::filter(test_tbl, .data$test == "chi2" | .data$test == keep_f)
}

# Build the fmt "pvalue" cells for a p-value display row, reproducing the pre-1.4.0 cell fields
# (display "pvalue"; pct = p; diff drives the >=5% flag; n cleared). Vectorised over p.
# Phase 12f: `label` (per col_var, e.g. "Chi2" / "F, Welch") turns the cell into the composite
# display "{pvalue} (<label>)" -- the in-cell test label that self-documents a mixed factor/mean row.
# NA / "" leaves the bare "pvalue" token (byte-identical to the pre-12f cell). The label is a text-
# backend suffix only (Excel keeps the raw p-value number).
pvalue_line_fmt <- function(p, label = NA_character_) {
  disp <- ifelse(is.na(label) | !nzchar(label), "pvalue", paste0("{pvalue} (", label, ")"))
  fmt(display = disp, type = "n", n = NA_integer_,
      var = p, pct = p, ci_inf = 0, ci_sup = 0, ctr = 0,
      diff = dplyr::if_else(p > 0.05, -0.5, 0), digits = 2L, col_var = "chi2_cols")
}

# The label shown in a crosstab p-value cell for each test type (Phase 12f). NULL -> no in-cell label.
test_cell_label <- function(test) {
  switch(test, "chi2" = "Chi2", "F_welch" = "F, Welch", "F_classic" = "F", NA_character_)
}


# === Regression model-summary footer (Phase 12f) =================================================
# GOF stats travel in the whole-table `test` attribute with reg-specific discriminators (built by
# reg_gof_tibble() / reg_compare_rows() in R/tab_reg.R). This section renders them as a console block
# (print_reg_footer, parallel to print_chi2) or appended export rows (reg_footer_lines, parallel to
# tab_pvalue_lines). The discriminators are DISJOINT from the crosstab "chi2"/"F_*", so print_chi2 /
# tab_pvalue_lines no-op on a regression table and these renderers no-op on a crosstab -- one `test`
# attribute, two rendering paths, no crosstab byte-identity impact.

# One entry per footer stat: its row label + how the cell renders. kind "gof" -> a plain number (the
# "gof" display token reading the `statistic` value); kind "pvalue" -> a p-value cell (from `pvalue`).
# `digits` applies to gof cells. Order here = the display / fallback order.
reg_footer_spec <- function() list(
  n                    = list(label = "N",                    kind = "gof",    digits = 0L),
  lr_null              = list(label = "LR vs null",           kind = "pvalue"),
  wald_null            = list(label = "Wald vs null",         kind = "pvalue"),
  f_model              = list(label = "F",                    kind = "pvalue"),
  r2                   = list(label = "R2",              kind = "gof",   digits = 3L),
  r2_adj               = list(label = "Adjusted R2",     kind = "gof",   digits = 3L),
  mcfadden_r2          = list(label = "McFadden R2",     kind = "gof",   digits = 3L),
  nagelkerke_r2        = list(label = "Nagelkerke R2",   kind = "gof",   digits = 3L),
  cox_snell_r2         = list(label = "Cox-Snell R2",    kind = "gof",   digits = 3L),
  sigma                = list(label = "Residual SD",          kind = "gof",   digits = 2L),
  aic                  = list(label = "AIC",                  kind = "gof",   digits = 0L),
  bic                  = list(label = "BIC",                  kind = "gof",   digits = 0L),
  dispersion           = list(label = "Dispersion",           kind = "gof",   digits = 2L),
  compare_baseline     = list(label = "LR vs baseline",       kind = "pvalue"),
  compare_baseline_f   = list(label = "F vs baseline",        kind = "pvalue"),
  compare_baseline_wald = list(label = "Wald vs baseline",    kind = "pvalue"),
  compare_baseline_aic = list(label = "Delta-AIC vs baseline", kind = "gof",  digits = 0L),
  compare_seq          = list(label = "LR vs previous",       kind = "pvalue"),
  compare_seq_f        = list(label = "F vs previous",        kind = "pvalue"),
  compare_seq_wald     = list(label = "Wald vs previous",     kind = "pvalue"),
  compare_seq_aic      = list(label = "Delta-AIC vs previous", kind = "gof",  digits = 0L)
)
reg_footer_test_types <- function() names(reg_footer_spec())
reg_footer_labels     <- function() unname(vapply(reg_footer_spec(), `[[`, character(1), "label"))
is_reg_footer <- function(test_tbl)
  !is.null(test_tbl) && nrow(test_tbl) > 0 && any(test_tbl$test %in% reg_footer_test_types())

# A single footer cell (one fmt value), for the appended export rows. gof -> the "gof" token (value in
# `diff`); pvalue -> the pvalue_line_fmt shape (no in-cell label: the reg row label already names the
# stat). A missing stat -> a "blank" cell (renders "").
reg_gof_cell   <- function(value, digits) fmt(display = "gof", type = "n", n = NA_integer_,
                                              diff = value, digits = as.integer(digits))
reg_pvalue_cell <- function(p) pvalue_line_fmt(p)
reg_blank_cell  <- function() fmt(display = "blank", type = "n", n = NA_integer_)

# # In doc exemple they do :
#  df_colour <- function(x) {
# if (inherits(x, "my_tibble")) {
#   attr(x, "colour")
# } else {
#   NULL
# }
# }


# as_tab <- function(x, ...) {
#   UseMethod("as_tab")
# }
# as_tab.default <- function(x, ...) {
#   #vctrs::vec_cast(x, tab())
# }

#' @keywords internal
untab <- function(tabs) {
  if (lv1_group_vars(tabs)) {
    `class<-`(tabs, class(tabs) %>% purrr::discard(. == "tabxplor_tab"))
  } else {
    `class<-`(tabs, class(tabs) %>%
                purrr::discard(. %in% c("tabxplor_grouped_tab", "tabxplor_tab")))
  }
}


#Methods to print class tabxplor_tab -----------------------------------------------------

#' Printing method for class tabxplor_tab
#' @param x Object to format or print.
#' @param ... Passed on to \code{tbl_format_setup()}.
#' @param n Number of rows to show.
#' @param width Width of text output to generate.
#' @param max_extra_cols Number of extra columns to print abbreviated information for,
#'   if the width is too small for the entire tibble.
#' @param max_footer_lines Maximum number of footer lines.
#' @param min_row_var Minimum number of characters for the row variable. Default to 30.
#' @param get_text Set to `TRUE` to get the text as a character vector
#' instead of a printed output.
#' @export
#' @return A printed table.
#' @method print tabxplor_tab
print.tabxplor_tab <- function(x, width = NULL, ..., n = 100, max_extra_cols = NULL,
                               max_footer_lines = NULL, min_row_var = 30, get_text = FALSE) {
  # Phase 13a: install this table's per-table color_breaks override for the render (no-op otherwise).
  .cb <- push_color_breaks(x); on.exit(pop_color_breaks(.cb), add = TRUE)
  if (getOption("tabxplor.print") == "kable") {
    x <- tab_kable(x)
    print(x)
    return(invisible(x))
  }

  # Phase 10i-B: materialise the add_n (in-cell {pct} (n={n})) / add_pct (col_pct) display extras for
  # the console (backend "text"); p-value stays the print_chi2() block (pvalue = FALSE), NOT rows.
  x <- tab_materialize_extras(x, backend = "text", pvalue = FALSE)

  # Phase 10i-B (decision 2): the console shows the compact chi2/F test block (print_chi2), NOT
  # p-value body rows. It sits AFTER the kable branch so `print = "kable"` renders p-value ROWS (via
  # tab_kable -> tab_export_prep materialize) rather than the block. print_chi2() now fires for a
  # normal tab() too, because the `test` attribute is no longer dropped at build (Phase 10i-B).
  print_chi2(x, width = width)
  print_reg_footer(x, width = width)  # Phase 12f: regression GOF block (no-op on crosstabs)

  # Use pillar::char() on row_var to control truncation. Phase 10c: robust, position-independent
  # detection (degrade -> no min-width fixup, prints the plain tibble without crashing).
  rv        <- tab_render_vars(x)
  row_var   <- if (isTRUE(rv$degrade)) character(0) else rv$row_var
  n_row_var <- which(names(x) == row_var)

  out <- dplyr::mutate(x, dplyr::across(
    tidyselect::all_of(row_var),
    ~ pillar::char(as.character(.), min_chars = min_row_var)
  ))

  # out <- format(out, width = NULL)
  out <- format(out, width = width, ..., n = n, max_extra_cols = max_extra_cols,
                max_footer_lines = max_footer_lines)

  # DESIGN: pillar::char(min_chars=) above is used only to force a minimum width on the
  # row_var column, but it makes pillar print that column's type as <char>. Rewrite it back
  # to <fct> in the header line so the displayed type stays correct. out[3] is the type-tag
  # line for tabxplor_tab (out[4] in the grouped method, which has one extra header line).
  if (length(n_row_var) != 0) {
    regular_ex <-
      paste0("^(", paste0(rep("[^<]+<", n_row_var), collapse = ""), ")<char>") %>%
      stringr::str_replace("<\\)<", ")<")

    out[3] <- out[3] %>% stringr::str_replace(regular_ex, "\\1<fct> ")
  }


  # writeLines(format(x, width = width, ..., n = n, max_extra_cols = max_extra_cols,
  #                   max_footer_lines = max_footer_lines))
  if (get_text) {
    out
  } else {
    writeLines(out)
    invisible(x)
  }
}

#' Printing method for class tabxplor_grouped_tab
#' @param x Object to format or print.
#' @param ... Passed on to \code{tbl_format_setup()}.
#' @param n Number of rows to show.
#' @param width Width of text output to generate.
#' @param max_extra_cols Number of extra columns to print abbreviated information for,
#'   if the width is too small for the entire tibble.
#' @param max_footer_lines Maximum number of footer lines.
#' @param min_row_var Minimum number of characters for the row variable. Default to 30.
#' @param get_text Set to `TRUE` to get the text as a character vector
#' instead of a printed output.
#'
#' @export
#' @return A printed grouped table.
#' @method print tabxplor_grouped_tab
print.tabxplor_grouped_tab <- function(x, width = NULL, ..., n = 100,
                                       max_extra_cols = NULL,max_footer_lines = NULL,
                                       min_row_var = 30, get_text = FALSE) {
  # Phase 13a: install this table's per-table color_breaks override for the render (no-op otherwise).
  .cb <- push_color_breaks(x); on.exit(pop_color_breaks(.cb), add = TRUE)
  if (getOption("tabxplor.print") == "kable") {
    x <- tab_kable(x)
    print(x)
    return(invisible(x))
  }

  # Phase 10i-B: materialise add_n / add_pct for the console (backend "text"); p-value stays the block.
  x <- tab_materialize_extras(x, backend = "text", pvalue = FALSE)

  # Phase 10i-B (decision 2): see print.tabxplor_tab -- the console shows the chi2/F block, not
  # p-value rows; placed after the kable branch so `print = "kable"` renders p-value rows instead.
  print_chi2(x, width = width)
  print_reg_footer(x, width = width)  # Phase 12f: regression GOF block (no-op on crosstabs)

  # Use pillar::char() on row_var to control truncation. Phase 10c: robust, position-independent
  # detection (degrade -> no min-width fixup, prints the plain tibble without crashing).
  rv        <- tab_render_vars(x)
  row_var   <- if (isTRUE(rv$degrade)) character(0) else rv$row_var
  n_row_var <- which(names(x) == row_var)

  out <- dplyr::mutate(x, dplyr::across(
    tidyselect::all_of(row_var),
    ~ pillar::char(as.character(.), min_chars = min_row_var)
  ))

  # out <- format(out, width = NULL)
  out <- format(out, width = width, ..., n = n, max_extra_cols = max_extra_cols,
                max_footer_lines = max_footer_lines)

  # Same <char>-back-to-<fct> workaround as print.tabxplor_tab; the type-tag line is out[4]
  # here (a grouped_tab prints one extra header line).
  if (length(n_row_var) != 0) {
    regular_ex <-
      paste0("^(", paste0(rep("[^<]+<", n_row_var), collapse = ""), ")<char>") %>%
      stringr::str_replace("<\\)<", ")<")

    out[4] <- out[4] %>% stringr::str_replace(regular_ex, "\\1<fct> ")
  }

  # writeLines(format(x, width = width, ..., n = n, max_extra_cols = max_extra_cols,
  #                   max_footer_lines = max_footer_lines))
  if (get_text) {
    out
  } else {
    writeLines(out)
    invisible(x)
  }

  }


# === SECTION: tabxplor_tabs -- the multi-table list class (Phase 13c-iv) =========================

# A lightweight S3 wrapper over the LIST that tab()/tab_many() return for a multi-table result (>= 2
# row_vars, tab_vars present, or output_list = TRUE). It IS a list (inherits "list") -- is.list(),
# `[[`, length(), lapply(), purrr::map() all keep working -- and only adds a print / knit_print that
# renders like a single tab (kable -> Viewer, or the tibble list) plus `[` / `c` that keep the class.
# A single tab is returned bare (a tabxplor_tab), never wrapped, so the common case is unchanged.
#' @keywords internal
new_tabxplor_tabs <- function(x) {
  structure(x, class = c("tabxplor_tabs", "list"))
}

# Wrap a multi-table list; no-op on a single tab (data.frame) or an already-wrapped / kable object.
#' @keywords internal
as_tabxplor_tabs <- function(x) {
  if (is.list(x) && !is.data.frame(x) && !inherits(x, "tabxplor_tabs")) new_tabxplor_tabs(x) else x
}

#' Printing method for a list of tabxplor tables
#'
#' @param x A \code{tabxplor_tabs} object (the list returned by \code{\link{tab}} /
#'   \code{\link{tab_many}} for a multi-table result).
#' @param ... Passed to the per-table print method.
#' @return \code{x}, invisibly.
#' @export
print.tabxplor_tabs <- function(x, ...) {
  # Mirror print.tabxplor_tab: honour options("tabxplor.print"). "kable" renders all tables joined
  # (routed to the Viewer, like a single tab); otherwise print each element's tibble in sequence.
  if (getOption("tabxplor.print") == "kable") {
    print(tab_kable(x))
    return(invisible(x))
  }
  for (i in seq_along(x)) {
    print(x[[i]], ...)
    if (i < length(x)) cat("\n")
  }
  invisible(x)
}

#' @export
`[.tabxplor_tabs` <- function(x, ...) new_tabxplor_tabs(NextMethod())

#' @export
c.tabxplor_tabs <- function(...) new_tabxplor_tabs(NextMethod())

# knit_print so a `tabxplor_tabs` embedded in an Rmd/Quarto chunk renders as the joined kable.
#' @exportS3Method knitr::knit_print
knit_print.tabxplor_tabs <- function(x, ...) {
  knitr::knit_print(tab_kable(x), ...)
}


#' @keywords internal
# Phase 3b: render the tidy `test` attribute (Chi2 + ANOVA F) as a readable, colored block above
# the table. In the normal tab() flow the p-values are materialised as body rows by
# tab_pvalue_lines() (which drops the attribute), so this block mainly shows for tables that keep
# the attribute (e.g. a manual tab_plain() |> tab_chi2()). One line per (subtable x col_var).
print_chi2 <- function(x, width = NULL) {
  test_tbl <- get_test(x)
  if (is.null(test_tbl) || nrow(test_tbl) == 0) return(NULL)
  disp <- test_display_rows(test_tbl)
  disp <- dplyr::filter(disp, !is.na(.data$pvalue))
  if (nrow(disp) == 0) return(NULL)

  cs  <- get_color_style()
  tvs <- purrr::map_chr(tab_get_vars(x)$tab_vars, rlang::as_name)
  tvs <- intersect(tvs, names(disp))

  lines <- purrr::pmap_chr(disp, function(...) {
    r <- list(...)
    stat_lbl <- if (r$test == "chi2") "Chi2" else "F"
    df_txt   <- if (r$test == "chi2") paste0("df=", r$df1)
                else                  paste0("df=", r$df1, ",", round(r$df2, 1))
    prefix   <- if (length(tvs) > 0) {
      paste0(paste(purrr::map_chr(tvs, ~ as.character(r[[.x]])), collapse = " / "), " - ")
    } else ""
    p_txt <- paste0(formatC(r$pvalue * 100, format = "g", digits = 3), "%")
    p_txt <- if (isTRUE(r$pvalue >= 0.05)) cs[[8]](p_txt) else cs[[4]](p_txt)
    paste0("# ", prefix, r$col_var, ": ", stat_lbl, "=",
           formatC(r$statistic, format = "g", digits = 3), " (", df_txt, ") p=", p_txt)
  })

  cli::cat_line(lines)
  cli::cat_line()
}

#' @keywords internal
# Phase 12f: render the regression GOF footer as a console block (one line per model column), from the
# reg-specific rows of the `test` attribute. Parallel to print_chi2(); no-op on a crosstab (no reg
# discriminators). A gof stat shows "<label>=<value>"; a p-value stat shows "<label> p=<p%>" (coloured
# red/green by >=0.05, like print_chi2). Called from both print methods, after print_chi2().
print_reg_footer <- function(x, width = NULL) {
  test_tbl <- get_test(x)
  if (!is_reg_footer(test_tbl)) return(NULL)
  spec <- reg_footer_spec()
  reg  <- test_tbl[test_tbl$test %in% names(spec), , drop = FALSE]
  if (nrow(reg) == 0) return(NULL)
  cs <- get_color_style()

  fmt_val <- function(v, digits) prettyNum(formatC(v, format = "f", digits = digits),
                                           big.mark = " ")
  # One footer line per model column, and -- under split_var (Phase 12g) -- per split group (the group
  # level is tagged in `row_var`): "# <col_var> | <group>: ..." so the per-group N / GOF stay separate
  # instead of being concatenated onto one line. row_var = "" (no split) -> just "# <col_var>: ...".
  reg$row_var <- if (is.null(reg$row_var)) "" else ifelse(is.na(reg$row_var), "", reg$row_var)
  keys  <- unique(reg[, c("col_var", "row_var"), drop = FALSE])
  lines <- purrr::pmap_chr(keys, function(col_var, row_var) {
    sub   <- reg[reg$col_var == col_var & reg$row_var == row_var, , drop = FALSE]
    sub   <- sub[order(match(sub$test, names(spec))), , drop = FALSE]
    parts <- purrr::pmap_chr(sub, function(...) {
      r  <- list(...)
      sp <- spec[[r$test]]
      if (identical(sp$kind, "gof")) {
        paste0(sp$label, "=", fmt_val(r$statistic, sp$digits))
      } else {
        p_txt <- if (isTRUE(r$pvalue < 0.0001)) "<0.01%"
                 else paste0(formatC(r$pvalue * 100, format = "g", digits = 3), "%")
        p_txt <- if (isTRUE(r$pvalue >= 0.05)) cs[[8]](p_txt) else cs[[4]](p_txt)
        paste0(sp$label, " p=", p_txt)
      }
    })
    lbl <- if (nzchar(row_var)) paste0(col_var, " | ", row_var) else col_var
    paste0("# ", lbl, ": ", paste(parts, collapse = "  "))
  })
  cli::cat_line(lines)
  cli::cat_line()
}


#' Table headers for class tab
#' @importFrom pillar tbl_sum
#' @param x An object of class tabxplor_tab
#' @param ... Other parameters.
#' @return A table header
#' @export
#' @method tbl_sum tabxplor_tab
tbl_sum.tabxplor_tab <- function(x, ...) {
  tbl_header <- NextMethod()
  names(tbl_header)[1] <- "A tabxplor tab"
  tbl_header
}
#' Table headers for class grouped tab
#' @return A table header
#' @param x An object of class tabxplor_tab
#' @param ... Other parameters.
#' @export
#' @method tbl_sum tabxplor_grouped_tab
tbl_sum.tabxplor_grouped_tab <- function(x, ...) {
  grouped_tbl_header <- NextMethod()
  names(grouped_tbl_header)[1] <- "A tabxplor tab"
  grouped_tbl_header
}


#' Table footer for class tab
#' @importFrom pillar tbl_format_footer
#' @param x An object of class tabxplor_tab
#' @param setup A setup object from the table
#' @param ... Other parameters.
#' @return A character vector.
#' @export
#' @method tbl_format_footer tabxplor_tab
tbl_format_footer.tabxplor_tab <- function(x, setup, ...) {
  default_footer <- NextMethod()

  print_colors <- suppressWarnings(tab_color_legend(x))
  subtext <- get_subtext(x) %>% purrr::discard(. == "")
  if (length(print_colors) != 0) print_colors <- paste0(
    pillar::style_subtle("# "), print_colors
  )
  if (length(subtext) != 0) subtext <- pillar::style_subtle( paste0("# ", subtext) )

  c(default_footer, print_colors, subtext)
}


#' Table body for class tab
#' @importFrom pillar tbl_format_body
#' @param x An object of class tabxplor_tab
#' @param setup A setup object from the table
#' @param ... Other parameters.
#' @return A character vector.
#' @export
#' @method tbl_format_body tabxplor_tab
tbl_format_body.tabxplor_tab <- function(x, setup, ...) {
  default_body <- NextMethod()

  body_data  <- default_body[-(1:2)]
  ind   <- dplyr::group_indices(setup$x)[1:length(body_data)]
  ind   <- tidyr::replace_na(ind != dplyr::lag(ind, default = 1L), FALSE)
  body_data <- body_data %>%
    purrr::map2(ind, function(.x, .y) if (.y) {c("", .x)} else {.x}) %>%
    purrr::flatten_chr()

  c(default_body[1:2], body_data) %>% `class<-`("pillar_vertical")
}



#' Print a tabxplor table in html
#'
#' @param tabs A table made with \code{\link{tab}} or \code{\link{tab_many}},
#'   or a `list` of tab with the same `col_vars` and no `tab_vars`.
#' @param color_type `r lifecycle::badge("deprecated")` Inert since 1.4.0: the text channel always uses
#' the text palette. The colour CHANNEL is chosen by `color = c(text, background)` (see \code{\link{tab}}).
#' @param theme By default (\code{"light"}) a white table with black text; \code{"dark"} for a black
#' table with white text; \code{"auto"} (opt-in) to follow whoever is **reading** the table:
#' \itemize{
#'   \item in a file or a knitted document, the reader's browser decides -- their operating system,
#'     plus any dark-mode toggle of the host page (Quarto, Bootstrap 5.3, Tailwind);
#'   \item printed to the **Viewer**, your editor decides. Its webview reports the operating system
#'     rather than the editor's colour theme, so the theme is resolved in R instead (RStudio's, or
#'     Positron's, best-effort).
#' }
#' \code{"auto"} needs `engine = "html"` (kableExtra's themes are baked at render time); asking it of
#' the kableExtra engine renders light with a message. Defaults to \code{getOption("tabxplor.theme")},
#' i.e. \code{"light"} -- a dark table is always a deliberate choice.
#' @param html_24_bit `r lifecycle::badge("deprecated")` Inert since 1.4.0: exports are always
#' 24-bit (the OKLCH palettes). Kept only so old calls do not error.
#' @param css `engine = "html"` only: inline the stylesheet with the table, so the output is
#' self-contained (default, from \code{getOption("tabxplor.kable_css")}). Set `FALSE` in a many-table
#' document that emits \code{\link{tab_css}} once at the top -- the stylesheet is table-independent,
#' so one copy styles every table. With `FALSE` and no \code{\link{tab_css}} call, tables render
#' uncoloured.
#' @param tooltips By default, html tooltips are used to display additional informations
#' at mouse hover. Set to \code{FALSE} to discard.
#' @param popover By default, takes \code{getOption("tabxplor.kable_popover")}. When
#' `FALSE`, html tooltips are of the base kind : they can't be used with floating table of
#' content in \pkg{rmarkdown} documents. Set to `TRUE` to use \pkg{kableExtra} html
#' popovers instead, which are compatible with floating toc. Remember
#' to enable the `popover` module by copying the following code into your document :
#' \code{<script>
#' $(document).ready(function(){
#'   $('[data-toggle="popover"]').popover();
#' });
#' </script>
#'}
#' @param color Set to \code{FALSE} to render the table without colours (monochrome).
#' @param color_legend Print colors legend below the table ?
#' @param lang Colour-legend language: \code{NULL} (auto from the R/OS locale, English fallback), \code{"en"} or \code{"fr"}.
#' You can then use a `css` chunk in rmarkdown to change popovers colors.
#' @param transpose Set to \code{TRUE} to transpose the table before export (rows become columns) --
#' the col-percentages-with-several-row-variables use case.
#' @param full_width A TRUE or FALSE variable controlling whether the HTML table
#' should have the preferable format for full_width. If not specified, a HTML
#' table will have full width by default but this option will be set to FALSE for
#' a LaTeX table.
#' @param html_font A string for HTML css font. By default, it uses
#'  `'"DejaVu Sans", "Arial", arial, helvetica, sans-serif'`. Set another
#'  default by setting `options("tabxplor.kable_html_font" = )`.
#' @param caption The table caption. For formatting, you need to use a `css`
#' with `caption{}`in rmarkdown.
#' @param wrap_rows By default, rownames are wrapped when larger than 30 characters.
#' @param wrap_cols By default, colnames are wrapped when larger than 12 characters.
#' @param whitespace_only Set to `FALSE` to wrap also on non whitespace characters.
# @param unbreakable_spaces Set to `FALSE` to keep normal spaces in text (auto-break).
#' @param var_names Which variable names to write beside the table: `"both"` (the default),
#'  `"rows"`, `"cols"` or `"none"`. The row-variable name is the leading column a table with
#'  several `row_vars` uses to name each block (written once per block, vertically); the
#'  column-variable names are the spanning row above their level columns. Level headers always
#'  keep their name. Defaults to \code{getOption("tabxplor.var_names", "both")}.
#' @param get_data Get the transformed data instead of the html table.
#' @param engine The HTML render engine. `"html"` (default) is a dependency-free `<table>` renderer:
#'  faster, and every look is a CSS class you can restyle (see [tab_css()]), which is what makes
#'  `theme = "auto"` possible. `"kableExtra"` is the legacy engine (\pkg{kableExtra}); it bakes its own
#'  theme, so it cannot follow the reader's colour scheme. Defaults to
#'  \code{getOption("tabxplor.tab_kable_engine", "html")}.
#' @param ... Other arguments to pass to \code{\link[kableExtra:kable_styling]{kableExtra::kable_styling}}.

#' @return A html table. Printing it opens it in the Viewer, on a page painted to match the table --
#' so a `theme = "dark"` table no longer sits in a white pane. Differences from totals, confidence
#' intervals, contribution to variance, and unweighted counts, are available in an html tooltip at
#' cells hover.
#' @export
#'
#' @examples
#' \donttest{
#' tabs <- tab(forcats::gss_cat, race, marital, year, pct = "row", color = "diff")
#' tab_kable(tabs, theme = "light")
#' }
tab_kable <- function(tabs,
                      theme = NULL, color_type = lifecycle::deprecated(), html_24_bit = NULL,
                      color = TRUE, tooltips = TRUE, popover = NULL, color_legend = TRUE,
                      lang = NULL,
                      caption = knitr::opts_current$get("tab.cap"),
                      transpose = FALSE,
                      var_names = NULL,
                      html_font = NULL,
                      get_data = FALSE,
                      full_width = FALSE,
                      wrap_rows = 35, wrap_cols = 15,
                      whitespace_only = TRUE,
                      engine = NULL, css = NULL,
                      ...) {
  if (lifecycle::is_present(color_type)) lifecycle::deprecate_soft("1.4.0", "tab_kable(color_type)")
  # Phase 13a: install a per-table color_breaks override for the render (no-op otherwise).
  .cb <- push_color_breaks(tabs); on.exit(pop_color_breaks(.cb), add = TRUE)
  # Phase 10j: the theme/color/color_legend preamble is the shared resolver. `html_24_bit` is inert
  # (Phase 13a): exports are always 24-bit, kept only so old calls do not error.
  o <- resolve_export_opts(theme = theme, color = color, color_legend = color_legend,
                           transpose = transpose, var_names = var_names, allow_auto = TRUE)
  theme <- o$theme
  color_legend <- o$color_legend
  compute <- c("refs", "bold", "range")
  if (o$color) compute <- c(compute, "colors")
  html_font <-
    if (is.null(html_font)) {getOption("tabxplor.kable_html_font")} else {html_font}
  popover <- if (is.null(popover)) {getOption("tabxplor.kable_popover")} else {popover}
  engine  <- if (is.null(engine)) {getOption("tabxplor.tab_kable_engine", "html")} else {engine}
  engine  <- match.arg(engine, c("kableExtra", "html"))
  css     <- if (is.null(css)) {getOption("tabxplor.kable_css", TRUE)} else {isTRUE(css)}

  # Phase 13d: "auto" (follow the reader's colour scheme) needs a stylesheet we control. kableExtra's
  # themes are baked at render time (kable_classic / kable_material_dark) and its HTML is not ours to
  # restyle, so downgrade rather than pretend.
  if (identical(theme, "auto") && !identical(engine, "html")) {
    cli::cli_inform(
      c("!" = 'theme = "auto" needs {.code engine = "html"}; rendering {.val light}.',
        "i" = "The kableExtra engine's themes are static."),
      .frequency = "once", .frequency_id = "tabxplor_theme_auto_kableextra")
    theme <- "light"
  }

  # --- Phase 10d: shared exporter prep (list/compact, degrade, roles, two-channel colours, bold). ---
  # The block-A "canonical col_vars -> validate -> compact", the graceful-degrade check, the role
  # detection, the per-column colour codes (fmt_channel_codes) and the bold-row set are the ONE shared
  # tab_export_prep(). `list_method = TRUE`: a non-mergeable list (several row_vars / tab_vars) is
  # rendered table-after-table instead of erroring (Phase 10e, like tab_md()).
  prep <- tab_export_prep(
    tabs, backend = "kable", list_method = TRUE, compute = compute, transpose = o$transpose,
    wrap = list(rows = wrap_rows, cols = wrap_cols, exdent = 2,
                whitespace_only = whitespace_only, unbreakable_spaces = TRUE, brk = "<br>"),
    theme = theme, var_names = o$var_names,
    color_legend = color_legend, what = "tab_kable()"
  )

  # Phase 10e: render each prepared table through the engine seam. The colour legend is CONTENT (a
  # measure summary), so it is prepended per table to `subtext` here; the seam styles everything else.
  in_knitr <- !is.null(knitr::opts_knit$get("out.format"))
  parts <- purrr::map(prep$tables, function(rd) {
    subtext <- character(0)
    if (!isTRUE(rd$vars$degrade)) {
      subtext <- rd$subtext
      if (color_legend && length(rd$roles$color_cols) != 0) {
        subtext <- c(
          suppressWarnings(tab_color_legend(
            rd$tab, medium = "html", style = "prose", lang = lang,
            theme = theme[1],
            # Phase 13d: the html engine ships a tabxplor stylesheet, so the legend uses the same slot
            # classes as the cells and follows any theme toggle with them. kableExtra does not.
            classes = identical(engine, "html"))),
          subtext)
      }
    }
    render_kable_html(rd, prep$meta, engine = engine, subtext = subtext, caption = caption,
                      tooltips = tooltips, popover = popover, html_font = html_font,
                      full_width = full_width, get_data = get_data, in_knitr = in_knitr, ...)
  })

  if (get_data) return(if (length(parts) == 1L) parts[[1]] else parts)

  # Phase 13d: the html engine's cells carry slot CLASSES, so the theme lives entirely here. The
  # stylesheet is table-independent (see tab_css()), hence built once per call -- or not at all, when a
  # document emitted tab_css() itself (options("tabxplor.kable_css" = FALSE)). kableExtra styles inline.
  style <- if (css && identical(engine, "html")) {
    tab_css(theme = theme, chrome = TRUE, style_tag = FALSE)
  } else ""
  # Phase 14k: `theme` rides along as an attribute so print.tabxplor_kable() can paint the Viewer's
  # page to match -- and, under "auto", resolve it from the editor (the browser cannot see Positron).
  tab_kable_join(parts, engine, css = style, theme = theme)
}



#' Print a tabxplor table in html
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Superseded by [tab_kable()], which renders any table -- `tabxplor_tab` or plain data.frame --
#' through the shared exporter prep. This function predates it and never shared its machinery: it
#' detects total rows/columns by matching the literal strings `"Total"`/`"Ensemble"` against names and
#' values, so it is hardcoded to English and French, and it renders no colours, tooltips or spanning
#' headers. Nothing in the package has ever called it.
#'
#' @param tabs A data.frame.
#' @param theme By default, a white table with black text, Set to \code{"dark"} for a
#' black table with white text.
#' @param total_in_bold Should rows and cols with "Total" string be set in bold ?
#' @param all_column_borders Put a vertical border around each column ?
#' @param html_font A string for HTML css font. By default, it uses
#'  `'"DejaVu Sans", "Arial", arial, helvetica, sans-serif'`. Set another
#'  default by setting `options("tabxplor.kable_html_font" = )`.
#' @param caption The table caption. For formatting, you need to use a `css`
#' with `caption{}`in rmarkdown.
#' @param full_width A TRUE or FALSE variable controlling whether the HTML table
#' should have the preferable format for full_width. If not specified, a HTML
#' table will have full width by default but this option will be set to FALSE for
#' a LaTeX table.
#' @param wrap_rows By default, rownames are wrapped when larger than 30 characters.
#' @param wrap_cols By default, colnames are wrapped when larger than 12 characters.
#' @param whitespace_only Set to `FALSE` to wrap also on non whitespace characters.
# @param unbreakable_spaces Set to `FALSE` to keep normal spaces in text (auto-break).
#' @param subtext A character vector to print rows of legend under the table.
#' @param ... Other arguments to pass to \code{\link[kableExtra:kable_styling]{kableExtra::kable_styling}}.


#' @return A html table (opened in the viewer in RStudio). Differences from totals,
#' confidence intervals, contribution to variance, and unweighted counts,
#' are available in an html tooltip at cells hover.
#' @export
#'
#' @examples
#' \donttest{
#' tabs <- tibble::tibble(nm      = c("First", "Second", "Total"),
#'                        column1 = c(1, 2, 3),
#'                        column2 = c(4, 5, 6)                    )
#' kable_tabxplor_style(tabs)
#' }
kable_tabxplor_style <- function(tabs,
                                 caption = knitr::opts_current$get("tab.cap"),
                                 theme = c("light", "dark"),
                                 total_in_bold = TRUE, all_column_borders = FALSE,
                                 html_font = NULL,
                                 full_width = FALSE,
                                 wrap_rows = 35, wrap_cols = 15,
                                 whitespace_only = TRUE, # unbreakable_spaces = TRUE,
                                 subtext = "",
                                 ...) {
  lifecycle::deprecate_soft("1.4.0", "kable_tabxplor_style()", "tab_kable()")

  html_font <-
    if (is.null(html_font)) {getOption("tabxplor.kable_html_font")} else {html_font}


  tabs <- tabs %>% dplyr::ungroup()

  tabs <- tabs |>
    tab_wrap_text(wrap_rows = wrap_rows,
                  wrap_cols = wrap_cols,
                  exdent = 2,
                  whitespace_only = whitespace_only,
                  unbreakable_spaces = TRUE,
                  brk = "<br>")

  alignement <- tabs |>
    purrr::map_chr(
      ~ dplyr::if_else(condition = is_fmt(.) | is.numeric(.),
                       true      = "r",
                       false     = "l")
    )

  out <- tabs |> knitr::kable(escape = FALSE, format = "html", align = alignement,
                              #table.attr = "style=\"border-top: 0; border-bottom: 0; cellspacing: -10pt\"",
                              caption = caption)
  # table.attr changes css style of table_classic (no upper and lower big lines)

  if (theme[1] == "light") {
    out <- out %>% kableExtra::kable_classic(
      lightable_options = "hover", # "striped", ?
      #bootstrap_options = c("hover", "condensed", "responsive", "bordered"), #"striped",
      full_width = full_width,
      html_font = html_font, # "DejaVu Sans Condensed", # row_label_position
      #fixed_thead = TRUE,
      ...
    )

  } else {
    out <- out %>% kableExtra::kable_material_dark(
      lightable_options = "hover",
      bootstrap_options = c("hover", "condensed", "responsive"), #"striped",
      full_width = full_width,
      html_font = html_font, # "DejaVu Sans Condensed", # row_label_position
      #fixed_thead = TRUE,
      ...
    )

  }

  # `if (subtext != "")` on a length-2 subtext is an error since R 4.2 ("the condition has length > 1")
  # -- unreachable so far only because nothing calls this. any() is what the sibling engine does.
  if (any(nzchar(subtext))) {
    out <- out %>% kableExtra::add_footnote(subtext, notation = "none", escape = FALSE)
  }

  totcols <- which(stringr::str_detect(names(tabs), "^Total|^Ensemble"))
  totrows <- which(stringr::str_detect(tabs[[1]], "^Total|^Ensemble"))

  out <- out %>%
    kableExtra::row_spec(
      0, bold = TRUE, # color = "black"
      extra_css = "border-top: 0px solid ; border-bottom: 1px solid ;font-size: 90%;vertical-align: bottom;line-height: 0.9;padding: 3px;text-align: center;" #
    ) %>%
    #kableExtra::row_spec(refs2, bold = TRUE) %>%
    kableExtra::row_spec(
      nrow(tabs), extra_css = "border-bottom: 1px solid ;"
    ) %>%
     #kableExtra::column_spec(fmt_cols, extra_css = "white-space: nowrap;") %>%
    #kableExtra::column_spec(unique(c(new_col_var, ncol(tabs))), border_right = TRUE) %>%
    #kableExtra::column_spec(other_cols, border_left = TRUE) %>%
    kableExtra::column_spec(1, width_min = 20, border_left = TRUE, border_right = TRUE) %>%
    kableExtra::column_spec(ncol(tabs), border_right = TRUE) %>%
     #kableExtra::row_spec(new_group, extra_css = "border-bottom: 1px solid;") %>%
    #kableExtra::row_spec(nrow(tabs), extra_css = "border-bottom: 1px solid;") |>
    kableExtra::row_spec(
      1:nrow(tabs),
      extra_css = "vertical-align: top; line-height: 0.85;padding: 3px;white-space: nowrap;"
    )

  if (total_in_bold) {
    out <- out |>
      kableExtra::row_spec(
        totrows, bold = TRUE,
        extra_css = "border-top: 1px solid ; border-bottom: 1px solid ;"
      ) |>
      kableExtra::column_spec(totcols, bold = TRUE, width_min = 11, border_left = TRUE)

    } else {
      out <- out |>
        kableExtra::row_spec(
          totrows,
          extra_css = "border-top: 1px solid ; border-bottom: 1px solid ;"
        ) |>
        kableExtra::column_spec(totcols, width_min = 11, border_left = TRUE)
    }

  if (all_column_borders) {
    out <- out |> kableExtra::column_spec(1:ncol(tabs), border_left = TRUE)
  }


  if (getOption("tabxplor.always_add_css_in_tab_kable") | interactive()) {
    out <- paste0(

      htmltools::includeCSS(system.file("tab.css", package = "tabxplor")),
      "\n",
      # "<script type=\"text/x-mathjax-config\">MathJax.Hub.Config({tex2jax: {inlineMath: [[\"$\",\"$\"]]}})</script>",
      # "<script async src=\"https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\"></script>",
      # "\n",
      as.character(out) #|>
      #stringr::str_replace_all("<td style", '<td class = "align-top"; style')
    ) |>
      vctrs::vec_restore(out)
  }


  out
}




# Why: tab_compact() promotes a merged sub-table's total row to its reference row when that
# sub-table has no explicit reference (so each stacked sub-table colours against its OWN
# total). Byte-identical to if_else(is_totrow & !any(is_refrow), as_refrow(.), .) but writes
# the in_refrow field DIRECTLY, skipping the per-column vec_case_when ptype2/cast round-trip
# that was tab_compact()'s single biggest cost (Phase 9b-1; decisions.md §29).
promote_totrow_to_refrow <- function(col) {
  in_refrow <- vctrs::field(col, "in_refrow")
  if (any(in_refrow)) return(col)             # sub-table already has a reference row
  totrow <- vctrs::field(col, "in_totrow")
  if (!any(totrow)) return(col)
  in_refrow[totrow] <- TRUE
  vctrs::field(col, "in_refrow") <- in_refrow
  col
}

# tab_stack_tables() -- Phase 9b-6 (Boundary B): row-bind a list of prepared per-row_var tables (same
# columns, the tab_compact() same-col_vars contract) on PLAIN field-frames, byte-identical to
# purrr::imap_dfr() / vec_rbind but without the per-row tabxplor_fmt reconstruction. Per column name:
#   - non-fmt (the "levels" / "row_var" factors): vctrs::vec_c() -> factor level union, like bind_rows.
#   - fmt: vctrs::vec_ptype_common() across the tables reconciles the 9 attrs via the SAME
#     vec_ptype2.tabxplor_fmt reduce vec_rbind would use (L3: differing attr -> neutral) but is
#     O(#tables x #attrs), not O(#rows) (a ptype is length-0). promote_totrow_to_refrow runs per table
#     (L4, per subtable) before the field read.
# Column order = tables[[1]]'s (all tables share columns after the same-col_vars check); row order =
# tables stacked in list order.
tab_stack_tables <- function(tables) {
  nms  <- names(tables[[1]])
  cols <- purrr::map(purrr::set_names(nms, nms), function(nm) {
    # unname: the table (list) names would otherwise be taken by vec_c()/vec_ptype_common() as outer
    # names and error on length > 1 vectors ("Can't merge the outer name ...").
    pieces <- unname(purrr::map(tables, ~ .[[nm]]))
    if (is_fmt(pieces[[1]])) {
      frames <- purrr::map(pieces, function(col) {
        col   <- promote_totrow_to_refrow(col)   # L4, per subtable (one in_refrow field write, cheap)
        fr    <- as.list(vctrs::vec_data(col))
        # The old imap_dfr / vec_rbind cast each column via vec_cast.tabxplor_fmt.tabxplor_fmt, which
        # reads fields through the GETTERS. get_wn() is the only getter with a fallback (NA -> the n
        # field), so it MATERIALISES wn -- reproduce it here (raw vec_data keeps NA). All other getters
        # are raw field reads, so the rest of the frame already matches.
        fr$wn <- get_wn(col)
        fr
      })
      common <- do.call(vctrs::vec_ptype_common, pieces)   # L3 reconcile via ptype2, O(#tables)
      meta   <- purrr::set_names(
        lapply(fmt_col_attrs, function(a) attr(common, a, exact = TRUE)), fmt_col_attrs)
      fmt_stack_frames(frames, meta)
    } else {
      do.call(vctrs::vec_c, pieces)                        # factor level union / plain concat
    }
  })
  tibble::new_tibble(cols, nrow = sum(purrr::map_int(tables, nrow)))
}

#' Bind a list of tabs with the same col_vars (and no tab_vars) into a single tab
#'
#' @param tabs A `list` of `tabxplor_tab` (or a `tabxplor_tab`)
# @param pvalue_lines Set to `TRUE` to add a line with chi2 pvalues under each table.
#'
#' @returns A `tabxplor_tab`
#' @export
#'
#' @examples
#' \donttest{
#' forcats::gss_cat |>
#'   tab_many(c(race, rincome), marital, pct = "row", color = "diff") |>
#'   tab_compact()
#' }
tab_compact <- function(tabs) { # pvalue_lines = FALSE
  tabs_base <- tabs

  if (is.data.frame(tabs)) {tabs <- list(tabs) |> purrr::set_names(names(tabs)[1]) }

  # Phase 14d: an already-merged table is a no-op. It used to be caught by accident -- the heuristic
  # read its synthetic `row_var` meta column as a tab_var and took the bail below. Now that the roles
  # are recorded, that table truthfully reports NO tab_vars, so the guard has to be explicit or it
  # would merge a second time (col 1 "row_var" -> "levels", a new `row_var` on top).
  if (any(purrr::map_lgl(tabs, ~ isTRUE(get_vars_attr(.)$compacted)))) return(tabs_base)

  if (any(purrr::map_lgl(tabs, ~ length(tab_get_vars(.)$tab_vars) > 0 )) ) {
    # Merging across row_vars WITH tab_vars is deferred (§7): keep the multi-table structure.
    message("since some tab_vars were provided, tab_compact() was not used")
    return(tabs_base)
    #stop("tab_compact() can't be used with tab_vars")
  }

  same_col_vars <- purrr::map(tabs, ~ tab_get_vars(.)$col_vars)
  same_col_vars <- same_col_vars |>
    purrr::map(~ .[!. %in% c("all_col_vars", "", "no") & !is.na(.)])
  longest_col_vars <- purrr::map_int(same_col_vars, length)
  longest_col_vars <-
    dplyr::first(which(longest_col_vars == max(longest_col_vars, na.rm = TRUE)))
  longest_col_vars <- same_col_vars[[longest_col_vars]]
  same_col_vars <- same_col_vars |> purrr::map_lgl(~ all(. %in% longest_col_vars))
  if(!all(same_col_vars)) {
    stop("tab_compact() can only be used with the same col_vars in each tab")
  }


  subtext <- get_subtext(tabs[[1]])
  render_extras_first <- get_render_extras(tabs[[1]])
  ci_settings_first   <- get_ci_settings(tabs[[1]])

  # Phase 14d: the ONE place the row-variable names must be harvested per-tab, not from tabs[[1]] --
  # the merge is about to destroy them (col 1 -> the literal "levels"; the names survive only as
  # levels of the synthetic `row_var` factor). Recorded, so no consumer has to guess them back.
  vars_merged <- new_vars_attr(
    row_vars  = purrr::map_chr(tabs, ~ {
      v  <- get_vars_attr(.)
      rv <- dplyr::first(if (is.null(v)) tab_get_vars(.)$row_var else v$row_vars)
      # Phase 14i: was `%||% NA_character_`. Base `%||%` is R >= 4.4 only and the package supports
      # R >= 4.1, importing it from nowhere (cf. resolve_export_opts()) -- so this errored on 4.1-4.3.
      if (is.null(rv)) NA_character_ else rv
    }),
    col_vars  = longest_col_vars,
    tab_vars  = character(0),          # guaranteed: the tab_vars bail above returned already
    compacted = TRUE
  )

  tabs_chi2 <- purrr::map_df(tabs, ~get_test(.) )

  # var_type <- tabs |> map(get_type) |> first()
  # var_type <- first(unique(type[!type %in% c("", "n")]))
  #
  # color_type <- tabs |> map(get_color) |> first()
  # color_type <- first(unique(color_type[!color_type %in% c("", "no") &
  #                                         !names(color_type) %in% ("n")]))




  # DESIGN: when a merged sub-table has no explicit reference row, promote its total row to
  # reference so each stacked sub-table colors its cells against its OWN total (Phase 9b-1's
  # promote_totrow_to_refrow, a direct in_refrow field write). Phase 9b-6 (Boundary B): the
  # per-row_var tables are row-bound on PLAIN field-frames via tab_stack_tables() instead of an
  # imap_dfr / vec_rbind over the tabxplor_fmt records -- the promotion is folded onto each table's
  # field frame there (still per sub-table, so `any(in_refrow)` stays grouped per row_var), and the
  # cross-table attribute reconcile reuses vec_ptype_common (L3). The per-tab prep (rename col 1 ->
  # "levels", add the row_var meta factor) is cheap (no row-reconstruction).
  prepped <- tabs |> purrr::imap(
    ~ dplyr::rename_with(.x, ~"levels", .cols =  1) |>
      dplyr::mutate(row_var = as.factor(.y), .before = 1)
  )
  tabs <- tab_stack_tables(prepped)

  # tabs$Danser |> vctrs::vec_data()
  # tabs |> tab_kable()


  # col_vars <- get_col_var(tabs)[ get_col_var(tabs) != "" &
  #                                  names(get_col_var(tabs)) != "n" &
  #                                  !str_detect(names(get_col_var(tabs)), "^Total") ]

  if (sum(stringr::str_detect(names(tabs), "^Total_")) == 1) {
    tabs <- tabs |>
      dplyr::rename_with(~ "Total", .cols = tidyselect::starts_with("Total_"))
  }

  # Phase 10i-B: carry the add_n/add_pct intent through the merge (all per-row_var tabs share it).
  tabs <- new_tab(tabs, subtext = subtext, test = tabs_chi2,
                  render_extras = render_extras_first, ci_settings = ci_settings_first,
                  vars = vars_merged) |>
    dplyr::group_by(!!rlang::sym("row_var"))

  # if (pvalue_lines) {
  #   tabs <- tabs |> tab_pvalue_lines()
  # }

  tabs
}


# tab_materialize_extras() -- the SINGLE display-time materializer for the synthetic table extras
# (Phase 10i-B). The built tab() is the "core" table: no add_n / add_pct columns, no p-value rows. It
# carries only the INTENT -- the `test` attribute (whole-table chi2/ANOVA) and, from Increment 2, a
# `render_extras` attribute (the add_n/add_pct flags). This helper hydrates a core table into the
# rendered shape and is the ONE place the extras are built, called by every DISPLAY path:
# tab_export_prep() (kable/md/plot/xl), tab_xl() before tab_transpose(), and -- for add_n/add_pct
# only, NOT p-value -- the console print methods (the console shows the print_chi2() block instead of
# p-value body rows; Phase 10i-B decision 2).
#
# `backend`: "text" (console/kable/md) folds add_n into the Total cell (in-cell {pct} (n={n})); "xl"
# emits a real `n` column (Increment 2). `pvalue`: when TRUE, bake the p-value rows from the kept
# `test` attribute (reused via tab_pvalue_lines(), which drops the attribute after baking).
#
# IDEMPOTENT: tab_pvalue_lines() early-returns on an empty/absent `test` and drops it on success (and,
# Increment 2, the add_n/add_pct arm clears `render_extras` after consuming), so a second call is a
# no-op -- tab_xl can materialize before tab_transpose() while tab_export_prep()'s later re-materialize
# stays inert.
#' @keywords internal
#' @noRd
tab_materialize_extras <- function(tab, backend = c("text", "xl"), pvalue = TRUE) {
  backend <- match.arg(backend)

  # --- add_n / add_pct (from the render_extras intent) -----------------------------------------
  re      <- get_render_extras(tab)
  add_n   <- isTRUE(re$add_n)
  add_pct <- isTRUE(re$add_pct)
  if (add_n || add_pct) {
    # Reuse tab_add_n_pct() verbatim (byte-identical field construction; its grouped outer-mutate
    # reproduces the per-subtable scoping on the final merged / tab_vars-grouped table -- proven).
    # It yields the "xl-style" output: a real `n` COLUMN (pct="row" add_n) / `n` ROW (pct="col"),
    # the add_pct `col_pct` column / `row_pct` row.
    tab <- tab_add_n_pct(list(tab), add_n = add_n, add_pct = add_pct)[[1]]
    # TEXT backends fold the add_n `n` COLUMN into the Total cell (`{pct} (n={n})`); Excel keeps the
    # real numeric column. The pct="col" add_n ROW and add_pct col_pct/row_pct are backend-invariant.
    if (identical(backend, "text") && add_n && "n" %in% names(tab)) {
      tab <- tab_fold_addn_incell(tab)
    }
    tab <- set_render_extras(tab, NULL)          # consumed -> a second call is a no-op (idempotent)
  }

  # --- Excel-only: a mean + sd twin column (Phase 13c-v) ---------------------------------------
  # Console / kable / md show the sd inline as "mean (sigma sd)" (special_formatting); Excel cannot, so
  # for each numeric mean column insert an uncoloured sibling "<var>_sd" holding sd = sqrt(var) (display
  # "var" -> get_num() IS the sd; the sigma prefix is added by tab_xl's numFmt). Purely an Excel layout
  # concern: the built table + the text backends (inline sd) are untouched.
  if (identical(backend, "xl")) {
    is_mean_col <- function(col) is_fmt(col) && identical(get_type(col), "mean") &&
      any(get_display(col) %in% c("mean", "mean_ci"))
    means <- names(tab)[purrr::map_lgl(tab, is_mean_col)]
    for (nm in means) {
      sdc <- tab[[nm]]
      tab[[paste0(nm, "_sd")]] <-
        set_color(set_display(set_var(sdc, suppressWarnings(sqrt(get_var(sdc)))), "var"), "no")
    }
    if (length(means) > 0) {                     # place each _sd directly after its mean column
      ord <- names(tab)
      for (nm in rev(means)) {
        sd_nm <- paste0(nm, "_sd")
        rest  <- ord[ord != sd_nm]
        ord   <- append(rest, sd_nm, after = which(rest == nm))
      }
      tab <- tab[ord]
    }
  }

  # --- p-value rows (from the kept `test` attribute) -------------------------------------------
  # tab_pvalue_lines no-ops on a regression table (no chi2/F rows), so the order is safe: a crosstab
  # gets the chi2 p-value row, a regression table gets its GOF footer rows (Phase 12f).
  if (pvalue) tab <- tab_pvalue_lines(tab)
  if (pvalue && is_reg_footer(get_test(tab))) tab <- reg_footer_lines(tab)
  tab
}


#' Transform chi2 attribute table of a tabxplor_tab into rows with pvalues.
#'
#' @param tabs A tabxplor_tab (with chi2 table as attribute).
#'
#' @return A tabxplor_tab.
# @export
#
# @examples
# \donttest{
# forcats::gss_cat |>
#   tab_many(race, marital, pct = "row", color = "diff", add_n = FALSE) |>
#   tab_chi2() |>
#   tab_pvalue_lines()
# }
tab_pvalue_lines <- function(tabs) {
  subtext  <- get_subtext(tabs)
  render_extras <- get_render_extras(tabs)
  ci_settings   <- get_ci_settings(tabs)
  test_tbl <- get_test(tabs)
  if (is.null(test_tbl) || nrow(test_tbl) == 0) return(tabs)

  groups   <- dplyr::groups(tabs)
  gv       <- tab_get_vars(tabs)
  row_var  <- gv$row_var
  tab_vars <- purrr::map_chr(gv$tab_vars, rlang::as_name)
  tab_vars <- intersect(tab_vars, names(test_tbl))

  # first-level column of each col_var (where the p-value cell is placed): col_var -> column name
  first_lv  <- gv$col_vars_levels |> purrr::map_chr(~ rlang::as_name(dplyr::first(.)))
  cv_to_col <- purrr::set_names(unname(first_lv), names(first_lv))

  # one displayed test per (subtable x col_var): chi2 (factors) / chosen F (means)
  disp <- test_display_rows(test_tbl)
  disp <- dplyr::filter(disp, .data$col_var %in% names(cv_to_col), !is.na(.data$pvalue))
  if (nrow(disp) == 0) return(tabs)

  # one p-value row per subtable, the p-value fmt cell placed under each col_var's first-level col.
  # Phase 12f: the cell embeds the test label ("Chi2" / "F, Welch") so a mixed factor/mean p-value row
  # self-documents which test each column ran (composite "{pvalue} (<label>)" display).
  tabs_pvalue_lines <- disp |>
    dplyr::mutate(.col  = unname(cv_to_col[.data$col_var]),
                  .cell = pvalue_line_fmt(.data$pvalue,
                                          label = purrr::map_chr(.data$test, test_cell_label))) |>
    dplyr::select(tidyselect::any_of(tab_vars), ".col", ".cell") |>
    tidyr::pivot_wider(names_from = ".col", values_from = ".cell") |>
    dplyr::mutate(!!rlang::sym(row_var) := forcats::as_factor("pvalue"))

  # Phase 9b-6 (Boundary B): append the p-value row(s) on PLAIN field-frames instead of
  # map2_df(bind_rows(tabs, pvalue), tabs, vec_restore) + a masked fill -- BOTH were full tabxplor_fmt
  # record reconstructions (the ~9% pass-4 residue). Row ORDER + the non-fmt columns come from the SAME
  # bind_rows + group_by + arrange, run on a fmt-FREE skeleton; each fmt column is then rebuilt ONCE:
  # origin cells ++ the appended cells (the pvalue_line_fmt cell where present, else the fill
  # fmt0(first(display), type) with n = NA -- subsuming the pass-3 masked fill), sliced to the arranged
  # order, materialized with tabs' OWN meta (the old vec_restore(., tabs) discarded the added row's
  # attrs, so there is no L3 reconcile). The fill's first(display)/type are column-uniform, so tabs'
  # global first == the old grouped mutate's per-group first (byte-identical, locked by test-golden).
  n0      <- nrow(tabs)
  k       <- nrow(tabs_pvalue_lines)
  fmt_nms <- names(tabs)[purrr::map_lgl(tabs, is_fmt)]
  skel_df <- function(x, nms, src) tibble::new_tibble(
    c(purrr::set_names(lapply(nms, function(nm) x[[nm]]), nms), list(.src = src)),
    nrow = length(src))

  # 1. row order + the combined non-fmt columns: the IDENTICAL bind_rows + group_by + arrange, but on
  # the fmt-free projection. `.src` tags each final row's source (positive -> origin row of `tabs`;
  # negative -> p-value row of `tabs_pvalue_lines`).
  skel <- dplyr::bind_rows(
    skel_df(tabs,              setdiff(names(tabs),              fmt_nms),  seq_len(n0)),
    skel_df(tabs_pvalue_lines, setdiff(names(tabs_pvalue_lines), fmt_nms), -seq_len(k))
  ) |>
    dplyr::group_by(!!!rlang::syms(groups)) |>
    dplyr::arrange(.by_group = TRUE) |>
    dplyr::ungroup()
  src <- skel$.src
  pos <- dplyr::if_else(src > 0, src, n0 - src)   # index into c(origin 1..n0, appended n0+1..n0+k)
  skel <- dplyr::select(skel, -".src")

  # 2. rebuild each fmt column once: origin fields ++ the k appended fields, sliced to `pos`.
  build_col <- function(nm) {
    of    <- as.list(vctrs::vec_data(tabs[[nm]]))
    of$wn <- get_wn(tabs[[nm]])                    # the vec_cast wn fallback (as in tab_stack_tables)
    fill  <- fmt0(dplyr::first(get_display(tabs[[nm]])), type = get_type(tabs[[nm]]))
    vctrs::field(fill, "n") <- NA_integer_
    af    <- lapply(as.list(vctrs::vec_data(fill)), function(v) rep(v, k))   # k fill rows
    pv    <- tabs_pvalue_lines[[nm]]
    if (!is.null(pv) && is_fmt(pv)) {
      present <- !is.na(get_display(pv))           # subtables that got a displayed test in this col_var
      if (any(present)) {
        pvd <- as.list(vctrs::vec_data(pv))
        for (f in names(af)) af[[f]][present] <- pvd[[f]][present]
      }
    }
    frame <- purrr::set_names(
      lapply(names(of), function(f) vctrs::vec_c(of[[f]], af[[f]])[pos]), names(of))
    meta  <- purrr::set_names(
      lapply(fmt_col_attrs, function(a) attr(tabs[[nm]], a, exact = TRUE)), fmt_col_attrs)
    fmt_materialize_col(frame, meta)
  }

  out  <- purrr::set_names(lapply(names(tabs), function(nm)
    if (nm %in% fmt_nms) build_col(nm) else skel[[nm]]), names(tabs))
  tabs <- tibble::new_tibble(out, nrow = n0 + k)

  new_tab(tabs, subtext = subtext, render_extras = render_extras, ci_settings = ci_settings) |>
    dplyr::group_by(!!!rlang::syms(groups))
}

# Phase 12f: materialise the regression GOF footer as appended rows (one row per stat, a "Model fit"
# group), the export analogue of tab_pvalue_lines(). Each stat cell is placed under its model column
# (the first output column of the fit; MNL/ordinal blank the other category columns), and the row-label
# column carries the stat label. Reuses the tab_pvalue_lines fmt-frame append (fmt_stack_frames on plain
# field-vectors). Idempotent: the `test` attribute is dropped, so a second call no-ops. Renders nothing
# on a crosstab (is_reg_footer FALSE).
reg_footer_lines <- function(tabs) {
  test_tbl <- get_test(tabs)
  if (!is_reg_footer(test_tbl)) return(tabs)
  spec <- reg_footer_spec()
  reg  <- test_tbl[test_tbl$test %in% names(spec), , drop = FALSE]
  if (nrow(reg) == 0) return(tabs)

  subtext   <- get_subtext(tabs)
  groups    <- dplyr::groups(tabs)
  group_chr <- purrr::map_chr(groups, rlang::as_name)

  fmt_nms <- names(tabs)[purrr::map_lgl(tabs, is_fmt)]
  nonfmt  <- setdiff(names(tabs), fmt_nms)
  # the row-label column = the non-grouping factor (reg groups by `var`; the label column is `levels`).
  rlc     <- setdiff(nonfmt, group_chr)
  row_lab_col <- if (length(rlc) >= 1L) rlc[length(rlc)] else nonfmt[length(nonfmt)]

  stats_present <- names(spec)[names(spec) %in% unique(reg$test)]
  K  <- length(stats_present)
  if (K == 0) return(tabs)
  footer_labels <- unname(vapply(stats_present, function(s) spec[[s]]$label, character(1)))

  # split_var (Phase 12h): a split table carries per-group GOF (the group level tagged in `reg$row_var`;
  # split_var is the FIRST grouping column). It gets one "Model fit" footer block PER group, placed at
  # the end of that group's rows; a plain table gets one block at the end (a single pseudo-group ""). The
  # rebuild iterates groups in row order, interleaving [group data | group footer], so order is preserved.
  reg_rv    <- if (is.null(reg$row_var)) rep(NA_character_, nrow(reg)) else reg$row_var
  is_split  <- any(nzchar(reg_rv[!is.na(reg_rv)]))
  split_col <- if (is_split) group_chr[[1]] else NA_character_
  grp_of    <- if (is_split) as.character(tabs[[split_col]]) else rep("", nrow(tabs))
  grp_lv    <- unique(grp_of)

  cell_for <- function(nm, s, g) {
    sel <- reg$col_var == nm & reg$test == s &
      (if (is_split) (!is.na(reg_rv) & reg_rv == g) else TRUE)
    r <- reg[sel, , drop = FALSE]
    if (nrow(r) == 0) return(reg_blank_cell())
    sp <- spec[[s]]
    if (identical(sp$kind, "gof")) reg_gof_cell(r$statistic[1], sp$digits) else reg_pvalue_cell(r$pvalue[1])
  }
  footer_frame <- function(nm, g) {
    fcol  <- do.call(vctrs::vec_c, lapply(stats_present, function(s) cell_for(nm, s, g)))
    fr    <- as.list(vctrs::vec_data(fcol)); fr$wn <- get_wn(fcol); fr
  }
  # per fmt column: interleave [group field-frame, group footer-frame] over groups, then stack once.
  build_col <- function(nm) {
    meta   <- purrr::set_names(
      lapply(fmt_col_attrs, function(a) attr(tabs[[nm]], a, exact = TRUE)), fmt_col_attrs)
    frames <- unlist(lapply(grp_lv, function(g) {
      idx <- which(grp_of == g)
      of  <- as.list(vctrs::vec_data(tabs[[nm]][idx])); of$wn <- get_wn(tabs[[nm]][idx])
      list(of, footer_frame(nm, g))
    }), recursive = FALSE)
    fmt_stack_frames(frames, meta)
  }
  # non-fmt column: each group's original values then its K footer values (labels / group / "Model fit").
  build_nonfmt <- function(nm) {
    orig <- tabs[[nm]]
    combined <- unlist(lapply(grp_lv, function(g) {
      base <- as.character(orig)[grp_of == g]
      foot <- if (nm == row_lab_col)          footer_labels
              else if (identical(nm, split_col)) rep(g, K)
              else                             rep("Model fit", K)
      c(base, foot)
    }))
    if (is.factor(orig)) {
      lv <- levels(orig)
      factor(combined, levels = c(lv, setdiff(unique(combined), lv)))
    } else combined
  }

  out <- purrr::set_names(lapply(names(tabs), function(nm) {
    if (nm %in% fmt_nms) build_col(nm) else build_nonfmt(nm)
  }), names(tabs))
  tabs2 <- tibble::new_tibble(out, nrow = length(out[[1]]))

  new_tab(tabs2, subtext = subtext) |>            # `test` dropped -> idempotent
    dplyr::group_by(!!!rlang::syms(group_chr))
}







#' Print a tabxplor table as plot
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' Superseded (1.4.0): `tab_plot()` renders a \pkg{tabxplor} table as a \pkg{ggpubr} image, but its
#' display is limited and it is no longer actively developed. It keeps working and is retained for a
#' future redesign; prefer \code{\link{tab_kable}} (HTML), \code{\link{tab_md}} (markdown) or
#' \code{\link{tab_xl}} (Excel).
#'
#' @param tabs A table made with \code{\link{tab}} or \code{\link{tab_many}}.
#' @param color_type `r lifecycle::badge("deprecated")` Inert since 1.4.0: the text channel always uses
#' the text palette. The colour CHANNEL is chosen by `color = c(text, background)` (see \code{\link{tab}}).
#' @param theme By default, a white table with black text, Set to \code{"dark"} for a
#' black table with white text.
#' @param html_24_bit `r lifecycle::badge("deprecated")` Inert since 1.4.0: exports are always
#' 24-bit (the OKLCH palettes). Kept only so old calls do not error.
#' @param color Set to \code{FALSE} to render the table without colours (monochrome).
#' @param color_legend Print colors legend below the table ?
#' @param lang Colour-legend language: \code{NULL} (auto from the R/OS locale, English fallback), \code{"en"} or \code{"fr"}.
#' @param transpose Set to \code{TRUE} to transpose the table before export (rows become columns).
#' @param caption The table caption.
#' @param var_names Which variable names to write beside the table: `"both"` (the default),
#'  `"rows"`, `"cols"` or `"none"`. See \code{\link{tab_kable}}.
#' @param wrap_rows By default, rownames are wrapped when larger than 30 characters.
#' @param wrap_cols By default, colnames are wrapped when larger than 12 characters.
#' @param whitespace_only Set to `FALSE` to wrap also on non whitespace characters.
# @param unbreakable_spaces Set to `FALSE` to keep normal spaces in text (auto-break).
#' @return A \code{\link[ggplot2]{ggplot}} object to be printed in the
#' `RStudio` Plots pane or exported as image, using \code{\link[ggpubr]{ggtexttable}}.
#' @export
#'
#' @examples
#' \donttest{
#' # ggpubr / gtable / ggplot2 are Suggests-only and tab_plot() stops without them, so guard the
#' # example: \donttest{} does NOT exempt it from R CMD check --as-cran, which CRAN also runs
#' # without Suggests installed.
#' if (requireNamespace("ggpubr", quietly = TRUE) &&
#'     requireNamespace("gtable", quietly = TRUE) &&
#'     requireNamespace("ggplot2", quietly = TRUE)) {
#'   tab(forcats::gss_cat, race, marital, pct = "row", color = "diff") |>
#'     tab_plot()
#' }
#' }
#'
tab_plot <- function(tabs,
                     theme = NULL, color_type = lifecycle::deprecated(), html_24_bit = NULL,
                     color = TRUE, color_legend = TRUE, lang = NULL, caption = NULL, transpose = FALSE,
                     var_names = NULL,
                     wrap_rows = 35, wrap_cols = 14, # unbreakable_spaces = TRUE
                     whitespace_only = TRUE) {
  if (lifecycle::is_present(color_type)) lifecycle::deprecate_soft("1.4.0", "tab_plot(color_type)")
  # Phase 13a: install a per-table color_breaks override for the render (no-op otherwise).
  .cb <- push_color_breaks(tabs); on.exit(pop_color_breaks(.cb), add = TRUE)
  if (!requireNamespace("ggpubr", quietly = TRUE)) {
    stop(paste0("Package \"ggpubr\" needed for this function to work. ",
                "You can install it with : install.packages('ggpubr')"),
         call. = FALSE)
  }
  if (!requireNamespace("gtable", quietly = TRUE)) {
    stop(paste0("Package \"gtable\" needed for this function to work. ",
                "You can install it with : install.packages('gtable')"),
         call. = FALSE)
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(paste0("Package \"ggplot2\" needed for this function to work. ",
                "You can install it with : install.packages('ggplot2')"),
         call. = FALSE)
  }
  if (!requireNamespace("cowplot", quietly = TRUE)) {
    stop(paste0("Package \"cowplot\" needed for this function to work. ",
                "You can install it with : install.packages('cowplot')"),
         call. = FALSE)
  }
  # Phase 10j: list-method parity. A list renders each table as its OWN plot (matching
  # tab_kable/tab_md/tab_xl, which render a list table-after-table), returning a list of ggplots.
  # Phase 14d: a list is never merged at export any more (see tab_resolve_tables), so `length > 1` is
  # the whole condition -- the mergeable probe it used to run is gone.
  if (is.list(tabs) && !is.data.frame(tabs) && length(tabs) > 1L) {
    return(purrr::map(tabs, tab_plot, theme = theme,
                      color = color, color_legend = color_legend,
                      caption = caption, transpose = transpose, wrap_rows = wrap_rows,
                      wrap_cols = wrap_cols, whitespace_only = whitespace_only))
  }

  # Phase 10j: shared option resolver (theme/color/color_legend/transpose). `html_24_bit` is inert
  # (Phase 13a).
  o <- resolve_export_opts(theme = theme, color = color, color_legend = color_legend,
                           transpose = transpose, var_names = var_names)
  theme <- o$theme
  color_legend <- o$color_legend
  compute <- c("refs", "bold", "range")
  if (o$color) compute <- c(compute, "colors")

  # --- Phase 10d: shared exporter prep (degrade, roles, two-channel colours, bold rows/cols). ---
  # tab_plot has no list->compact preamble; everything else (role detection, refs2/refs3, the colour
  # loop) is the ONE shared tab_export_prep(). Plot drops tab_vars, wraps with exdent = 1 /
  # unbreakable_spaces = FALSE (the "\n" break). Output is a ggplot -> no golden lock; A/B-verified.
  prep <- tab_export_prep(
    tabs, backend = "plot", compute = compute, transpose = o$transpose,
    wrap = list(rows = wrap_rows, cols = wrap_cols, exdent = 1,
                whitespace_only = whitespace_only, unbreakable_spaces = FALSE, brk = "\n"),
    theme = theme, var_names = o$var_names,
    color_legend = color_legend, what = "tab_plot()"
  )
  rd <- prep$tables[[1]]

  if (isTRUE(rd$vars$degrade)) {
    tab_degrade_inform(rd$vars$reason)
    return(invisible(tibble::as_tibble(tabs)))
  }

  tabs        <- rd$tab
  row_var     <- rd$vars$row_var
  tab_vars    <- rd$vars$tab_vars
  subtext     <- rd$subtext
  new_group   <- rd$roles$new_group
  color_cols  <- rd$roles$color_cols
  fmt_cols    <- rd$roles$fmt_cols
  other_cols  <- rd$roles$other_cols
  totcols     <- rd$roles$totcols
  totrows     <- rd$roles$totrows
  no_totrows  <- rd$roles$no_totrows
  new_col_var <- rd$roles$new_col_var
  any_bg      <- rd$roles$any_bg

  refs2 <- rd$bold_rows   # bold rows (reference/total in every discriminating column)
  refs3 <- rd$bold_cols   # bold columns (all-reference columns)

  text_color  <- prep$meta$theme_cols$text
  grey_color  <- prep$meta$theme_cols$grey
  grey_color2 <- prep$meta$theme_cols$grey2

  # Per-fmt-column colour vectors (derive-once) from the prep's `ann`, keyed by column name.
  color_selection <- purrr::map(rd$ann, "font")
  bg_selection    <- purrr::map(rd$ann, "back")

  if (length(other_cols) != 0) {
    other_font <- as.list(dplyr::mutate(tabs[other_cols],
                                        dplyr::across(tidyselect::everything(), ~ text_color)))
    other_none <- as.list(dplyr::mutate(tabs[other_cols],
                                        dplyr::across(tidyselect::everything(), ~ "none")))
    color_selection <- dplyr::bind_cols(other_font, color_selection)
    bg_selection    <- dplyr::bind_cols(other_none, bg_selection)
  } else {
    color_selection <- color_selection |> dplyr::bind_cols()
    bg_selection    <- bg_selection    |> dplyr::bind_cols()
  }

  face_selection <- color_selection |>
    dplyr::mutate(dplyr::across(
      dplyr::everything(),
      ~ dplyr::if_else(
        !. %in% c(text_color, grey_color, grey_color2) |
          #dplyr::cur_column() %in% names(totcols) |
          dplyr::row_number() %in% refs2 | dplyr::cur_column() %in% refs3,
        true  = "bold",
        false = "plain")
    ))

  # Phase 14i: name each block once (the prep's shared run model, as md blanks and html rowspans). No
  # rotation: a ggtexttable cell is a grob, not a table cell. `var_names` (the name column's drop and
  # the col_var span suppression) is already honoured upstream, in the prep.
  for (cl in names(rd$roles$label_cols)) {
    if (!cl %in% names(tabs)) next
    show <- rd$roles$label_runs[[cl]]$show
    tabs[[cl]] <- as.character(tabs[[cl]])
    tabs[[cl]][!show] <- ""
  }

  # Phase 14m-ii: a monospace body font so the numbers, significance stars and "(n=...)" composites
  # line up. WARNING: ggpubr 1.0.0 exposes no per-COLUMN font (table_cell_font() takes no family and
  # replaces the cell gpar), so this applies to the WHOLE body -- the row labels turn monospace too, a
  # small deviation from "text stays Condensed" that only affects the superseded tab_plot(). Revert
  # with options("tabxplor.plot_num_font" = "") (-> the ggpubr default). "mono" is a device-portable
  # graphics family, so it resolves to a monospace font on any device.
  plot_num_font <- getOption("tabxplor.plot_num_font", "mono")
  tbody_args <- list(color = "black", size = 11, fill = "white", linewidth = 0,
                     linecolor = "black", hjust = 0.98, x = 0.95) # x/hjust = right-adjust
  if (nzchar(plot_num_font)) tbody_args$fontfamily <- plot_num_font

  tabs_gg <- tabs |>
    dplyr::mutate(
      dplyr::across(
        where(is_fmt),
        ~ format(., special_formatting = TRUE,
                 .ref = ann_ref(rd$ann[[dplyr::cur_column()]]))
      ),
      dplyr::across( # otherwise, unbreakable spaces fail in some graphic devices
        where(is.factor),
        ~ forcats::fct_relabel(., ~ stringr::str_replace_all(., unbrk, " "))
      ),
      dplyr::across( # otherwise, unbreakable spaces fail in some graphic devices
        where(is.character),
        ~ stringr::str_replace_all(., unbrk, " ")
      ),
      # # unbreakable space at the starting of names, otherwise doesn't fit with hjust = "right"
      # dplyr::across(
      #   1,
      #   ~ forcats::fct_relabel(., ~ paste0(paste0(rep(unbrk, 4), collapse = ""),
      #                                      .))
      # )
    ) |>

    ggpubr::ggtexttable(
      rows = NULL, # base_size = 11,
      theme = ggpubr::ttheme("blank",
                             padding = grid::unit(c(4, 3), "mm"), # c(h, v)
                             tbody.style = do.call(ggpubr::tbody_style, tbody_args)),
    )

  # tabs |>
  #   dplyr::mutate(dplyr::across(where(is_fmt), format)) |>
  #   ggpubr::ggtexttable(
  #     rows = NULL, theme = ggpubr::ttheme("blank"),
  #   )

  # c("default", "blank", "classic", "minimal", "light",
  #   "lBlack", "lBlue", "lRed", "lGreen", "lViolet", "lCyan", "lOrange", "lBlackWhite", "lBlueWhite", "lRedWhite", "lGreenWhite", "lVioletWhite", "lCyanWhite", "lOrangeWhite",
  #   "mBlack", "mBlue", "mRed", "mGreen", "mViolet", "mCyan", "mOrange", "mBlackWhite", "mBlueWhite", "mRedWhite", "mGreenWhite", "mVioletWhite", "mCyanWhite", "mOrangeWhite"
  #   )




  # Phase 5: unified per-cell rendering. Text channel -> font colour; background channel -> cell
  # fill (only applied where a bg-channel colour exists; other cells keep the ggtexttable default).
  for(j in 1:ncol(tabs)) {
    for(i in 1:nrow(tabs)) {
      tabs_gg <- tabs_gg |> ggpubr::table_cell_font(
        row    = i + 1,
        column = j,
        color  = color_selection[[j]][[i]],
        face   = face_selection[[j]][[i]]
      )
      if (any_bg) {
        fillv <- bg_selection[[j]][[i]]
        if (!is.na(fillv) && fillv != "none") {
          tabs_gg <- tabs_gg |> ggpubr::table_cell_bg(
            row = i + 1, column = j, fill = fillv, linewidth = 0
          )
        }
      }
    }
  }
# tabs_gg



  tabs_gg <- tabs_gg |>
    ggpubr::tab_add_border(from.row = 1, linetype = 1, linewidth = 2, linecolor = "black") |>
    #ggpubr::thead_add_border(linetype = 1, linewidth = 2, linecolor = "black") |>
    ggpubr::tab_add_hline(
      at.row = unique(c(1, totrows, totrows + 1, new_group)), row.side = "bottom",
      linetype = 1, linewidth = 2, linecolor = "black",
    ) |>
    # ggpubr::tab_add_hline(
    #   at.row = totrows, row.side = "top",
    #   linetype = 1, linewidth = 2, linecolor = "black",
    # ) |>
    ggpubr::tab_add_vline(
      at.column = unique(c(new_col_var, totcols - 1)), column.side = "right",
      linetype = 1, linewidth = 2, linecolor = "black",
    ) |>
    ggpubr::tab_add_vline(
      at.column = unique(c(other_cols, totcols)), column.side = "left",
      linetype = 1, linewidth = 2, linecolor = "black",
     ) #|>
    # ggpubr::tab_add_vline(
    #   at.column = totcols - 1L, column.side = "right",
    #   linetype = 1, linewidth = 2, linecolor = "black",
    # )

    ## bold
    # kableExtra::row_spec(refs2, bold = TRUE) %>%

    ## wrap
    # kableExtra::column_spec(fmt_cols, extra_css = "white-space: nowrap;") %>%



if (color_legend & length(color_cols) != 0) {

  # Phase 14c: read the legend's RUN stream (text + hex per token) directly. It used to be scraped back
  # out of the html rendering with regexes that had silently stopped matching (Phase 13b replaced
  # kableExtra's `color: rgba(...)` spans with inline hex), so every legend token rendered as a raw
  # html fragment in black. "runs" is the medium built for exactly this: draw-as-text, no fill (a
  # background break-word borrows the darker bg_legend palette, as in Excel).
  color_legend <- suppressWarnings(tab_color_legend(tabs,
                                   medium     = "runs", style = "prose", lang = lang,
                                   theme      = theme[1])) |>
    purrr::map(function(line) {
      text  <- purrr::map_chr(line, "text")
      color <- purrr::map_chr(line, "color")
      color[is.na(color)] <- text_color
      # one ggtexttable column per token is wasteful (and the separators are their own tokens): fold
      # each run of same-coloured tokens into one cell.
      grp <- cumsum(color != dplyr::lag(color, default = ""))
      tibble::tibble(
        text  = vapply(split(text, grp), paste0, character(1), collapse = ""),
        color = color[!duplicated(grp)]
      ) |>
        # otherwise, unbreakable spaces fail in some graphic devices
        dplyr::mutate(text = stringr::str_replace_all(.data$text, unbrk, " "))
    })



  # color_legend <- color_legend |>
  #   purrr::map_dfr(
  #     ~ purrr::map_dfr(unique(.$color), function(.color)
  #       . |>
  #         dplyr::mutate(
  #           in_color = color %in% .color,
  #           group    = cumsum(in_color != dplyr::lag(in_color, default = FALSE))
  #         ) |>
  #         dplyr::group_by(group) |>
  #         dplyr::summarise(
  #           in_color = dplyr::first(in_color),
  #           text     = paste(.data$text, collapse = " "),
  #           .groups  = "drop"
  #         ) |>
  #         dplyr::mutate(text = dplyr::if_else(in_color,
  #                                             true  = paste0('"', .data$text, '"'),
  #                                             false = paste0('phantom("', .data$text, '")' ))
  #         ) |>
  #         dplyr::summarise(
  #           text = paste0("bold(", paste(.data$text, collapse = " * "),")") |>
  #             stringr::str_squish(),
  #         ) |>
  #         dplyr::mutate(
  #           color = .color,
  #           n     = dplyr::first(.$n),
  #           .before = 1
  #         )
  #
  #     )
  #   )

  # if (length(subtext) != 0) {
  #   color_legend <- list(
  #     color_legend,
  #     tibble::tibble(color = text_color,
  #                    text  = subtext, # paste0('"', subtext, '"'),
  #                    # n     = 1:length(subtext)
  #     ) |>
  #       dplyr::rowwise() |>
  #       dplyr::group_split()
  #   ) |>
  #     purrr::flatten()
  # }


# # If no color legend, just subtext
#   } else if (length(subtext) != 0) {
#     color_legend <-
#         tibble::tibble(color = text_color,
#                        text  = subtext, # paste0('"', subtext, '"'),
#                        # n     = 1:length(subtext)
#         ) |>
#       dplyr::rowwise() |>
#       dplyr::group_split()

  } else {
    color_legend <- NULL
  }

  if (length(color_legend) != 0) {
    #if (nrow(color_legend) != 0) {

      # color_legend_plot <- color_legend |>
      #   dplyr::group_by(!!rlang::sym("n")) |>
      #   dplyr::group_split() |>
      #   purrr::map(
      #     ~ dplyr::mutate(., n = max(.data$n) - .data$n) |>
      #       ggplot2::ggplot(ggplot2::aes(y     = .data$n,
      #                                    label = .data$text,
      #                                    color = .data$color)) +
      #       ggplot2::geom_text(x = 0, parse = TRUE, hjust = 0, size = 3.5) +
      #       ggplot2::scale_color_identity() +
      #       ggplot2::theme_void() #+
      #       #ggplot2::theme()
      #   )

      tab_legend <- color_legend |>
        purrr::map_dfr(
          ~ dplyr::select(., "text") |>
            dplyr::mutate(name = dplyr::row_number()) |>
            tidyr::pivot_wider( names_from = "name", values_from = "text")
        )

      tab_legend_color <- color_legend |>
        purrr::map_dfr(
          ~ dplyr::select(., "color") |>
            dplyr::mutate(name = dplyr::row_number()) |>
            tidyr::pivot_wider( names_from = "name", values_from = "color")

        )

      tab_legend_plot <- tab_legend |>
        ggpubr::ggtexttable(
          rows = NULL,
          theme = ggpubr::ttheme("blank",
                                 padding = grid::unit(c(7, 4), "mm"), # c(h, v)
                                 colnames.style = ggpubr::colnames_style(
                                   color = "white",
                                   size = 0,
                                   fill = "white",
                                   linewidth = 0
                                 ),
                                 tbody.style = ggpubr::tbody_style(
                                   color     = "black", #face = "plain", #parse = TRUE,
                                   size      = 8,
                                   fill      = "white", #c("grey95", "grey90"),
                                   linewidth = 0,
                                   linecolor = "black",

                                   hjust = 0.98, x = 0.95 # right ajust
                                 )),
        )


      for(i in 1:nrow(tab_legend)) {
        for(j in 1:ncol(tab_legend)) {
          tab_legend_plot <- tab_legend_plot |> ggpubr::table_cell_font(
            row    = i + 1,
            column = j,
            color  = tab_legend_color[[j]][[i]],
            face   = "bold"
          )
        }
      }


      cowplot::set_null_device("png") # "pdf", "png", "cairo", "agg"

      tabgrob    <- get_tablegrob(tabs_gg) |> justify_grob()
      legendgrob <- get_tablegrob(tab_legend_plot) |> justify_grob()

      tabgrob <- gtable::gtable_add_rows(
        tabgrob,
        heights = grid::grobHeight(legendgrob), #+
        #ggplot2::unit(1, "line"),
        pos = -1
      )
      tabgrob <- gtable::gtable_add_grob(tabgrob, legendgrob,
                                         t = nrow(tabgrob),
                                         b = nrow(tabgrob),
                                         l = 1,
                                         r = ncol(tabgrob))
      tabs_gg <- tab_return_same_class_as_input(tabgrob, input = tabs_gg)


      # dim_gg     <- tab_get_wrapped_dimensions(tabs)
      # dim_legend <- tab_get_wrapped_dimensions(tab_legend)
      #
      # tabgrob    <- get_tablegrob(tabs_gg) |> justify_grob()
      # legendgrob <- get_tablegrob(tab_legend_plot) |> justify_grob()
      #
      # tabs_gg    <- tab_return_same_class_as_input(tabgrob, input = tabs_gg)
      # tab_legend_plot <- tab_return_same_class_as_input(legendgrob, input = tab_legend_plot)
      #
      # tabs_gg <-
      #   ggpubr::ggarrange(
      #     tabs_gg,
      #     tab_legend_plot,
      #     ncol = 1L,
      #     #align = "v",
      #     heights = c(dim_gg[2], dim_legend[2] - 1L)
      #   )



      # for (i in 1:length(color_legend_plot)) {
      #   # ggpubr::tab_add_footnote
      #   tabgrob    <- get_tablegrob(tabs_gg)
      #   legendgrob <- cowplot::as_grob(color_legend_plot[[i]])
      # #
      #   tabgrob <- gtable::gtable_add_rows(
      #     tabgrob,
      #     heights = grid::grobHeight(legendgrob) +
      #       ggplot2::unit(1 + dplyr::if_else(i == 1, 0.5, 0), "line"),
      #     pos = -1
      #   )
      #   tabgrob <- gtable::gtable_add_grob(tabgrob, legendgrob, t = nrow(tabgrob),
      #                                      b = nrow(tabgrob), l = 1, r = ncol(tabgrob))
      #   tabs_gg <- tab_return_same_class_as_input(tabgrob, input = tabs_gg)
      # }
    #}
  }


  # Align the whole plot top left
  tabgrob <- get_tablegrob(tabs_gg)
  tabgrob <- justify_grob(tabgrob)
  tabs_gg <- tab_return_same_class_as_input(tabgrob, input = tabs_gg)

  # if (length(color_legend) != 0) {
  #   if (nrow(color_legend) != 0) {
  #     tabs_gg$color_palette <- color_palette
  #   }
  # }

  # tabs_gg$height <- grid::grobHeight(tabgrob) |> grid::convertHeight(unitTo = "points") # |> as.double()
  # tabs_gg$width  <- grid::grobWidth(tabgrob)  |> grid::convertWidth (unitTo = "points") # |> as.double()
  # # seem not ok...

  return(tabs_gg)
}










#' @keywords internal
# Phase 10e: each `out_*` fragment is now any()-gated -- the expensive format(set_display(x, ...)) pass
# runs ONLY when at least one cell of the column carries that field (a pct column has no or/mean/sd; a
# mean column has no rr/or/pct). When the gate fails the fragment is rep("", n), which is exactly what
# the original if_else/case_when produced (all-FALSE condition), so the tooltip string is BYTE-IDENTICAL
# -- only the discarded format() calls are skipped. `.ref` (the prep's precomputed ref_cells) skips the
# get_reference() re-derivation.
# Phase 14b: TEXT only -- it used to also wrap its output in kableExtra::spec_popover() when
# `popover = TRUE`, i.e. return HTML attributes from a text builder. The html engine passed that
# through and wrapped it AGAIN, so `tab_kable(engine = "html", popover = TRUE)` rendered the escaped
# attribute string as its own popover content. Attributes now live in tab_tooltip_attrs() alone.
tab_kable_print_tooltip <- function(x, .ref = NULL) {

  n       <- length(x)
  blank   <- rep("", n)
  ref     <- if (!is.null(.ref)) .ref else get_reference(x, mode = "cells")
  totcol  <- is_totcol(x)
  totrows <- is_totrow(x)
  tottabs <- is_tottab(x)
  type    <- get_type(x)
  digits  <- get_digits(x)
  # Phase 10i-A: a composite cell ("{pct} (n={n})") suppresses the tooltip line for its PRIMARY
  # field just like a plain "pct" cell would (the field-suppression guards below read `disp`).
  disp    <- display_primary(get_display(x))

  # Phase 14b: format() right-pads a column to its widest cell so the numbers align in the TABLE; in
  # a prose tooltip that pad is noise ("ratio:   x1"). Every interpolated value goes through this.
  tip_num <- function(v) stringr::str_trim(format(v))

  # Phase 14b: diff and ratio are ONE comparison group -- one gate, one "ref" token.
  # `comparable` is the exclusion the diff line always had (a Total-column / total-row cell that IS
  # its own base has nothing to compare itself to); it now gates the ratio line too, which used to
  # print a vacuous "ratio: x1" down every Total column. NA-safe: a contrib table writes onto the
  # Total column, whose pct is NA -- and an NA pct is not a 100% base (mirrors cond_pct / cond_ctr).
  comparable <- !((totcol | totrows) & !is.na(get_pct(x)) & get_pct(x) == 1)
  ok_diff    <- !is.na(get_diff(x))  & comparable
  # `type == "mean"` was excluded, so a mean column showed no ratio line at all -- though under the
  # default color = TRUE the ratio is exactly what colours it.
  ok_rr      <- !is.na(get_ratio(x)) & comparable & !disp == "rr" &
    type %in% c("col", "row", "mean")
  # A reference cell's whole comparison group collapses to ONE "ref": its diff is 0 and its ratio 1
  # by construction, so "diff: ref ; ratio: x1" said nothing, twice. The cell already prints
  # "ref:38%" -- the tooltip only has to name the role, and keep the load-bearing "n:".
  ref_grp    <- ref & (ok_diff | ok_rr)
  show_rr    <- ok_rr & !ref_grp

  out_diff <- if (any(ok_diff | ref_grp)) {
    dplyr::case_when(
      ref_grp ~ "ref",
      ok_diff ~ paste0("diff: ", tip_num(set_display(x, "diff"))),
      TRUE    ~ ""
    )
  } else blank

  # Phase 14b: a mean column is coloured by the sd-standardized difference (Glass's delta =
  # diff / sd_ref) against the mean_diff breaks, but the cell shows the RAW difference in the
  # variable's own units -- so the legend's "+0.2; +0.5; +0.8 standard deviations" had no per-cell
  # counterpart. Surface it on hover, next to the `sd:` it is measured in. Only where sd_ref
  # resolves: an absent / zero-variance reference row leaves the ratio undefined (and the cell
  # uncoloured), so it earns no line.
  ok_std  <- ok_diff & !ref_grp & type == "mean"
  out_std <- if (any(ok_std)) {
    std <- get_diff(x) / suppressWarnings(sqrt(get_ref_var(x)))
    std[!is.finite(std)] <- NA_real_
    dplyr::if_else(ok_std & !is.na(std),
                   paste0("std diff: ", sprintf("%+.2f", std), "sd"), "")
  } else blank

  ci_type  <- get_ci_type(x)
  ci_start <- switch(ci_type, "cell" = "ci: ", "")
  has_ci   <- !is.na(get_ci(x))
  out_ci   <- if (any(has_ci)) {
    dplyr::if_else(
      condition = has_ci,
      true      = paste0(ci_start, tip_num(set_display(x, "ci") %>%
                                             set_digits(dplyr::if_else(digits == 0L,
                                                                       digits + 1L,
                                                                       digits))) ),
      false     = ""
    )
  } else blank

  # str_trim: on a reference cell out_diff is the bare "ref" and out_ci is empty (a reference is
  # never compared to itself -> NA bounds), which would otherwise leave a trailing space.
  out_diff <- switch(ci_type,
                     "diff"  = ,
                     "ratio" = stringr::str_trim(paste0(out_diff, " ",
                                                        stringr::str_remove(out_ci, "%$"))),
                     out_diff)
  out_ci   <- switch(ci_type, "cell" = out_ci, "")

  cond_pct <- type %in% c("col", "row", "all", "all_tabs") &
    !is.na(get_pct(x)) & !disp %in% c("pct", "pct_ci")
  out_pct <- if (any(cond_pct)) {
    dplyr::if_else(cond_pct, tip_num(set_display(x, "pct")), "")
  } else blank

  cond_mean <- type == "mean" & !is.na(get_mean(x)) & !disp %in% c("mean", "mean_ci")
  out_mean <- if (any(cond_mean)) {
    dplyr::if_else(cond_mean, tip_num(set_display(x, "mean")), "")
  } else blank

  cond_sd <- type == "mean" & !is.na(get_var(x)) & !disp == "var"
  out_sd <- if (any(cond_sd)) {
    vr <- get_var(x)                                   # get_var()/get_digits(), not the `$` proxy pull
    dplyr::if_else(
      cond_sd,
      dplyr::if_else(
        vr >= 0,
        true  = paste0("sd: ", tip_num(set_display(set_digits(set_var(x, suppressWarnings(sqrt(vr))),
                                                              get_digits(x) + 1L), "var"))),
        false = ""),
      "")
  } else blank

  # Phase 13c-i: the ratio tooltip line was mislabelled ("rr:") and formatted the OR field, so a
  # color = c("diff","ratio") table (a ratio but no OR) showed an empty value. Format the rr field
  # (the ×/÷ ratio display) under a clearer "ratio:" label. Gate: `show_rr` (Phase 14b, above).
  out_rr <- if (any(show_rr)) {
    dplyr::if_else(show_rr, paste0("ratio: ", tip_num(set_display(x, "rr")) ), "")
  } else blank

  cond_or <- type %in% c("col", "row") & !is.na(get_or(x)) &
    !disp %in% c("or", "OR", "or_pct", "OR_pct")
  out_or <- if (any(cond_or)) {
    dplyr::if_else(cond_or, paste0("OR: ", tip_num(set_display(x, "or")) ), "")
  } else blank

  # `comparable` (Phase 14b) is the same base-cell exclusion this line had spelled out for itself.
  cond_ctr <- !is.na(get_ctr(x)) & !(get_ctr(x) == Inf) & comparable
  out_ctr <- if (any(cond_ctr)) {
    mctr      <- if (get_comp_all(x)) { totrows & tottabs & !totcol } else { totrows & !totcol }
    ctr_start <- dplyr::if_else(mctr, "mean_ctr: ", "contrib: ")
    dplyr::if_else(cond_ctr,
                   paste0(ctr_start, tip_num(set_display(x, "ctr")) %>% stringr::str_remove("^-")),
                   "")
  } else blank

  cond_n <- !is.na(get_n(x)) & !disp == "n"
  out_n <- if (any(cond_n)) {
    dplyr::if_else(cond_n, paste0("n: ", tip_num(set_display(x, "n")) ), "")
  } else blank

  # Phase 14b: join the NON-EMPTY fragments per cell. The old chain pasted all of them with a fixed
  # " ; " separator and then rewrote the result to collapse the empty slots -- str_replace_all(";  ; ",
  # "; ") three times, plus head/tail trims and an "NA ;" scrub. Non-overlapping matching means one
  # pass cannot collapse adjacent empties, which is why it was repeated: it silently assumed no cell
  # ever leaves >4 in a row. Adding a 10th fragment would have broken that assumption. This is exact,
  # for any number of fragments, and drops the NA scrub (an NA fragment is simply not joined).
  frags <- list(out_pct, out_mean, out_sd, out_diff, out_std, out_rr, out_or,
                out_ci, out_ctr, out_n)
  out <- rep("", n)
  for (f in frags) {
    k <- !is.na(f) & nzchar(f)
    if (!any(k)) next
    out[k] <- paste0(out[k], ifelse(nzchar(out[k]), " ; ", ""), f[k])
  }

  out
}



#' Wrap column names and character/factor variables.
#' @param tabs A `tabxplor_tab` or a `tibble` .
#' @param wrap_rows By default, rownames are wrapped when larger than 30 characters.
#' @param wrap_cols By default, colnames are wrapped when larger than 12 characters.
#' @param exdent On the second lines or more, the number or characters to use for indentation.
#' @param whitespace_only Set to `FALSE` to wrap also on non whitespace characters.
#' @param unbreakable_spaces Set to `FALSE` to keep normal spaces in text (auto-break).
#' @param brk The string to use for linebreak : `\n` in text, but `<br>` in html.

#' @return The same `tabxplor_tab` or `tibble`.
#' @export
#'
#' @examples
#' \donttest{
#' tab(forcats::gss_cat, race, marital, pct = "row", color = "diff") |>
#'   tab_wrap_text(wrap_rows = 5L, wrap_cols = 8L)
#' }
#'
tab_wrap_text <- function(tabs, wrap_rows = 35L, wrap_cols = 15L, exdent = 1,
                          whitespace_only = TRUE, unbreakable_spaces = TRUE,
                          brk = "\n") {
  if (wrap_rows == Inf & wrap_cols == Inf) return(tabs)

  tabs <- tabs |>
    dplyr::rename_with(
      ~ stringr::str_wrap(., wrap_cols, exdent = 0, whitespace_only = whitespace_only) |>
        stringr::str_replace_all("\n", brk)
    ) |>
    dplyr::mutate(
      dplyr::across(
        where(is.factor),
        ~ forcats::fct_relabel(
          ., ~ stringr::str_wrap(.,
                                 width           = wrap_rows,
                                 exdent          = exdent,
                                 whitespace_only = whitespace_only) |>
            stringr::str_replace_all("\n", brk)
        )
      ),
      dplyr::across(
        where(is.character),
        ~ stringr::str_wrap(.,
                            width           = wrap_rows,
                            exdent          = exdent,
                            whitespace_only = whitespace_only) |>
          stringr::str_replace_all("\n", brk)
      )
    )

  if (unbreakable_spaces) {
    tabs <- tabs |>
      dplyr::rename_with(
        ~ stringr::str_replace_all(., " ", unbrk)
      ) |>
      dplyr::mutate(
        dplyr::across(
          where(is.factor),
          ~ forcats::fct_relabel(., ~ stringr::str_replace_all(., " ", unbrk) )
        ),
        dplyr::across(
        where(is.character),
        ~ stringr::str_replace_all(., " ", unbrk)
      ),

      )
  }

  return(tabs)
}



#' Get the number of actual rows and the max character length of a table after
#' being wrapped (count `\n` as a linebreak).
#' @param tabs A data.frame.
#' @param no_tab_vars For data.frame of class `tabxplor_tab`, remove `tab_vars`.
#' @param width_pad Number of characters lengths between columns.
#' @export
tab_get_wrapped_dimensions <- function(tabs, no_tab_vars = FALSE,
                                       width_pad = 4L) {

  if (no_tab_vars & is_tab(tabs)) {
    tab_vars <- tab_get_vars(tabs)$tab_vars
    tabs <- tabs |> dplyr::ungroup() |> dplyr::select(-tidyselect::all_of(tab_vars))
  }

  tabs_with_colnames <-
    dplyr::bind_rows(
      tibble::tibble(!!!purrr::set_names(names(tabs), names(tabs))),
      tabs |> # heigth depend on the number of line breaks in each column
        #dplyr::select(tidyselect::where(~ is.character(.) | is.factor(.))) |>
        dplyr::ungroup() |>
        dplyr::mutate(dplyr::across(
          tidyselect::everything(),
          format
        )),
      )

  height <- tabs_with_colnames |>
    dplyr::mutate(dplyr::across(
      tidyselect::everything(),
      ~ 1L + stringr::str_count(., "\n")
    )) |>
    dplyr::rowwise() |>
    dplyr::mutate(n = max(dplyr::c_across(cols = tidyselect::everything()))) |>
    dplyr::pull("n") |> sum()

  #length(get_subtext(tabs)) +

  #length(unique(get_color(tabs)[!get_color(tabs) %in% c("", "no")])) # color legend length

  width <- tabs_with_colnames |>
    purrr::map(
      ~ stringr::str_split(., "\n") |>
        purrr::flatten_chr() |>
        stringr::str_length() |>
        max()
    ) |>
    purrr::map_int(
      ~ max(. + width_pad)
    ) |>
    sum()

  c("width" = width, "height" = height)
}










#Methods for class tabxplor_tab ----------------------------------------------------------

# importFrom not needed when tabxplor import dplyr as a "Depends" package

#' group_by method for class tabxplor_tab
#' @importFrom dplyr group_by
#' @param .data A tibble of class \code{tabxplor_tab}.
#' @param ... Variables or computations to group by.
#' @param .add When \code{FALSE}, the default, \code{group_by()} will
#'   override existing groups. To add to the existing groups, use
#'   \code{.add = TRUE}.
#' @param .drop Drop groups formed by factor levels that don't appear in the
#'   data? The default is \code{TRUE} except when \code{.data} has been previously
#'   grouped with \code{.drop = FALSE}.

#' @method group_by tabxplor_tab
#' @return A tibble of class \code{tabxplor_grouped_tab}.
#' @export
group_by.tabxplor_tab <- function(.data,
                                  ...,
                                  .add = FALSE,
                                  .drop = dplyr::group_by_drop_default(.data)) {
  out <- NextMethod()
  rlang::exec(new_grouped_tab, out, dplyr::group_data(out), !!!tab_attrs(.data))
}



#' arrange method for class tabxplor_tab
#' @importFrom dplyr arrange
#' @param .data A tibble of class tabxplor_tab.
#' @param ... <[`data-masking`][rlang::args_data_masking]> Variables, or
#'   functions of variables. Use `desc()` to sort a variable in descending
#'   order.
#' @param .by_group By default, will sort first by grouping variable.
#'   Set to `FALSE` to avoid this behaviour.
#' @param .by_totals By default, will put totals at the end of their group.
#'   Set to `FALSE` to avoid this behaviour.
#' @param .only_main_display By default, only the rows with the same display
#'   than the first row are arranged : if the first row of the group displays
#'   percentages, rows with n or pvalues are kept at the same place
#'   (typically, at the end of the group). Rows with the text `"row_pct"`, `"n"`
#'   or `"pvalue"` in the `row_var` name are also kept at the same place.
#'   Set to `FALSE` to avoid this behaviour.
#' @param .locale The locale to sort character vectors in.
#' @method arrange tabxplor_tab
#' @return A tibble of class \code{tabxplor__tab} or \code{tabxplor_grouped_tab}.
#' @export
 arrange.tabxplor_tab <-
  function(.data, ..., .by_group = TRUE, .by_totals = TRUE,
           .only_main_display = TRUE, .locale = NULL) {

    dots <- rlang::enquos(...)
    groups <- dplyr::groups(.data) #dplyr::group_data(.data)

    if (.by_totals) {
      .totrows <- is_totrow(.data)
      .data <- .data |>
        dplyr::select(-tidyselect::any_of(".totrows")) |>
        tibble::add_column(.totrows = .totrows)
      dots <- c(rlang::quo(.totrows), dots)

    }

    if (.only_main_display) {
      row_var <- tab_get_vars(.data)$row_var

      several_displays <- purrr::map_lgl(
        dplyr::select(dplyr::ungroup(.data), dplyr::where(is_fmt)),
        ~ length(unique(get_display(.))) > 1
      )
      several_displays <- names(several_displays)[several_displays]


      if (length(several_displays) > 1) {
        .secondary_display <-
          dplyr::select(.data, !!!groups, tidyselect::all_of(c(row_var)),
                        tidyselect::all_of(several_displays)) |>
          dplyr::transmute(
            secondary_display = dplyr::if_any(
              tidyselect::all_of(several_displays),
              ~ get_display(.) != dplyr::first(get_display(.))
            ) | !!rlang::sym(row_var) %in% c("row_pct", "n", "pvalue"),

            secondary_display = dplyr::if_else(.data$secondary_display,
                                               true  = dplyr::row_number(),
                                               false = 0L
            )
          ) |>
          dplyr::pull("secondary_display")

      } else {
        .secondary_display <-
          dplyr::select(.data, !!!groups, tidyselect::all_of(c(row_var)),
                        tidyselect::all_of(several_displays)) |>
          dplyr::transmute(
            secondary_display = dplyr::if_else(
              !!rlang::sym(row_var) %in% c("row_pct", "n", "pvalue"),
              true  = dplyr::row_number(),
              false = 0L
            )
          ) |>
          dplyr::pull("secondary_display")
      }

      .data <- .data |>
        dplyr::select(-tidyselect::any_of(".secondary_display")) |>
        tibble::add_column(.secondary_display = .secondary_display)
      dots <- c(rlang::quo(.secondary_display), dots)
    }

    if (.by_group) {
      dots <- c(rlang::quos(!!!dplyr::groups(.data)), dots)
    }

    out <-
      dplyr::arrange(.data = tibble::as_tibble(.data),
                     ... = !!!dots,
                     .by_group = FALSE,
                     .locale = .locale
      )

    if (.by_totals | .only_main_display) {
      out <- out |>
        dplyr::select(-tidyselect::any_of(c(".totrows", ".secondary_display")))
    }


    # out <- NextMethod()

    if (length(groups) > 0) out <- out |> dplyr::group_by(!!!groups)

    if (lv1_group_vars(out)) {
      rlang::exec(new_tab, out, !!!tab_attrs(.data))

    } else {
      groups <- dplyr::group_data(out)
      rlang::exec(new_grouped_tab, out, groups, !!!tab_attrs(.data))
    }

}
# tabs <- tab(forcats::gss_cat, race, marital, year, pct = "row", color = "diff")
# arrange(tabs, `Never married`)
# arrange(tabs, `Never married`, .by_group = FALSE)
# arrange(tabs, `Never married`, .by_totals = FALSE)
# arrange(tabs, `Never married`, .by_group = FALSE, .by_totals = FALSE)
# ungroup_tabs <- tab(forcats::gss_cat, race, marital, pct = "row", color = "diff")
# arrange(ungroup_tabs, `Never married`)
# arrange(ungroup_tabs, `Never married`, .by_group = FALSE)
# arrange(ungroup_tabs, `Never married`, .by_totals = FALSE)
# arrange(ungroup_tabs, `Never married`, .by_group = FALSE, .by_totals = FALSE)

#' rowwise method for class tabxplor_tab
#' @importFrom dplyr rowwise
#' @param data A tibble of class \code{tabxplor_tab}.
#' @param ... Variables to be preserved
#'   when calling \code{summarise()}. This is typically a set of variables whose
#'   combination uniquely identify each row.
#' @method rowwise tabxplor_tab
#' @return A tibble of class \code{tabxplor_grouped_tab} and \code{rowwise_df}.
#' @export
rowwise.tabxplor_tab <- function(data, ...) {
  out <- NextMethod()
  out <- rlang::exec(new_grouped_tab, out, dplyr::group_data(out), !!!tab_attrs(data))
  `class<-`(out, stringr::str_replace(class(out), "grouped_df", "rowwise_df"))
}




# (from vctrs documentation)
# The coercion methods for data frames operate in two steps:
# They check for compatible subclass attributes. In our case the tibble colour has to
# be the same, or be undefined.
# They call their parent methods, in this case tib_ptype2() and tib_cast() because we
# have a subclass of tibble. This eventually calls the data frame methods df_ptype2() and
# tib_ptype2() which match the columns and their types.

#' Coercion between two tab
#' @param x,y,to Subclasses of data frame.
#' @param ... For future extensions.
#' @param x_arg Argument names for x and y. These are used in error messages to inform
#' the user about the locations of incompatible types.
#' @param y_arg Argument names for x and y. These are used in error messages to inform
#' the user about the locations of incompatible types.
#' @param to_arg Argument names for x and to. These are used in error messages to inform
#' the user about the locations of incompatible types.
#'
#' @return A tibble of class \code{tabxplor_tab}.
#' @keywords internal
# @export
tab_cast <- function(x, to, ..., x_arg = "", to_arg = "") {
  out <- vctrs::tib_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)

  rlang::exec(new_tab, out, !!!tab_bind_attrs(x, to))
}

#' @rdname tab_cast
#' @return A tibble of class \code{tabxplor_tab}.
#' @keywords internal
# @export
tab_ptype2 <- function(x, y, ..., x_arg = "", y_arg = "") {
  out <- vctrs::tib_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)
  #colour <- df_colour(x) %||% df_colour(y)
  rlang::exec(new_tab, out, !!!tab_bind_attrs(x, y))
}


#Let's now implement the coercion methods, starting with the self-self methods.
#' @return A tibble of class \code{tabxplor_tab}.
#' @describeIn tab_cast find common ptype between tabxplor_tab and tabxplor_tab
#' @export
vec_ptype2.tabxplor_tab.tabxplor_tab <- function(x, y, ...) {
  tab_ptype2(x, y, ...)
}
#' @describeIn tab_cast convert tabxplor_tab to tabxplor_tab
#' @return A tibble of class \code{tabxplor_tab}.
#' @export
vec_cast.tabxplor_tab.tabxplor_tab <- function(x, to, ...) {
  tab_cast(x, to, ...)
}

# The methods for combining our class with tibbles follow the same pattern.
# For ptype2 we return our class in both cases because it is the richer type

#' @describeIn tab_cast find common ptype between tabxplor_tab and tbl_df
#' @export
#' @return A tibble of class \code{tabxplor_tab}.
vec_ptype2.tabxplor_tab.tbl_df <- function(x, y, ...) {
  tab_ptype2(x, y, ...)
}
#' @describeIn tab_cast find common ptype between tbl_df and tabxplor_tab
#' @return A tibble.
#' @export
vec_ptype2.tbl_df.tabxplor_tab <- function(x, y, ...) {
  tab_ptype2(x, y, ...)
}
#' @describeIn tab_cast convert tbl_df to tabxplor_tab
#' @return A tibble of class \code{tabxplor_tab}.
#' @export
vec_cast.tabxplor_tab.tbl_df <- function(x, to, ...) {
  tab_cast(x, to, ...)
}
#' @describeIn tab_cast convert tabxplor_tab to tbl_df
#' @return A tibble.
#' @export
vec_cast.tbl_df.tabxplor_tab <- function(x, to, ...) {
  vctrs::tib_cast(x, to, ...)
}

#' @describeIn tab_cast find common ptype between tabxplor_tab and data.frame
#' @return A tibble of class \code{tabxplor_tab}.
#' @export
vec_ptype2.tabxplor_tab.data.frame <- function(x, y, ...) {
  tab_ptype2(x, y, ...)
}
#' @describeIn tab_cast find common ptype between data.frame and tabxplor_tab
#' @return A data.frame.
#' @export
vec_ptype2.data.frame.tabxplor_tab <- function(x, y, ...) {
  tab_ptype2(x, y, ...)
}
#' @describeIn tab_cast convert data.frame to tabxplor_tab
#' @return A tibble of class \code{tabxplor_tab}.
#' @export
vec_cast.tabxplor_tab.data.frame <- function(x, to, ...) {
  tab_cast(x, to, ...)
}
#' @describeIn tab_cast convert tabxplor_tab to data.frame
#' @return A data.frame.
#' @export
vec_cast.data.frame.tabxplor_tab <- function(x, to, ...) {
  vctrs::df_cast(x, to, ...)
}




#Methods for class grouped_tab------------------------------------------------------------

# just modify the methodes currently used by dplyr class "grouped_df" (not relative to groups)
# .S3methods(class = "grouped_df")

# dplyr_col_modify      dplyr_reconstruct     dplyr_row_slice
# ungroup               distinct_        rename_     select_     summarise
# [                     [<-          [[<-
# cbind                 rbind  rowwise

#' ungroup method for class tabxplor_grouped_tab
#' @importFrom dplyr ungroup
#' @param x A tibble of class \code{tabxplor_grouped_tab}.
#' @param ... Variables to remove from the grouping.
#' @method ungroup tabxplor_grouped_tab
#' @return An object of class \code{tabxplor_tab} or \code{tabxplor_grouped_tab}.
#' @export
ungroup.tabxplor_grouped_tab <- function (x, ...)
{
  if (missing(...)) {
    rlang::exec(new_tab, x, !!!tab_attrs(x))
  }
  else {
    old_groups <- dplyr::group_vars(x)
    to_remove  <- tidyselect::vars_select(names(x), ...)
    new_groups <- setdiff(old_groups, to_remove)
    dplyr::group_by(x, !!!rlang::syms(new_groups))
  }
}

#' @keywords internal
lv1_group_vars <- function(tabs) {
  # TRUE when at most one group remains -> caller downgrades grouped_tab to plain tab. Uses
  # n_groups()<=1 (simple); the commented alternative counted single-level group vars.
  dplyr::n_groups(tabs) <= 1

  #groupvars <- dplyr::group_vars(tabs)
  # all(purrr::map_lgl(groupvars,
  #                ~ nlevels(forcats::fct_drop(dplyr::pull(tabs, .))) == 1)) |
  #   length(groupvars) == 0
}


# DESIGN: dplyr_row_slice + dplyr_col_modify + dplyr_reconstruct form the core trio.
#   Each: (1) calls NextMethod() for the actual operation, (2) checks lv1_group_vars()
#   to decide if result has enough groups to stay grouped_tab or must downgrade to tab,
#   (3) re-attaches subtext and chi2 attributes from the original data.
# WARNING: every dplyr verb a user might call needs its own S3 method following this same
#   pattern (see the group_by/select/rename/relocate/summarise/[/[<- clones below and in
#   NAMESPACE). A missing method silently downgrades the table to a plain tbl_df, losing the
#   class, subtext, chi2 and colored printing. See CLAUDE.md § dplyr Integration.
#' dplyr_row_slice method for class tabxplor_grouped_tab
#' @importFrom dplyr dplyr_row_slice
#' @method dplyr_row_slice tabxplor_grouped_tab
#' @param data A data frame.
#' @param i A numeric or logical vector that indexes the rows of \code{.data}.
#' @param ... Future parameters.
#' @return An object of class \code{tabxplor_grouped_tab}.
#' @export
dplyr_row_slice.tabxplor_grouped_tab <- function(data, i, ...) {
  out <- NextMethod()
  tab_restore(out, data)
}
# dplyr:::dplyr_row_slice.grouped_df

#' dplyr_col_modify method for class tabxplor_grouped_tab
#' @importFrom dplyr dplyr_col_modify
#' @method dplyr_col_modify tabxplor_grouped_tab
#' @param data A data frame.
#' @param cols A named list used modify columns. A \code{NULL} value should remove
#'   an existing column.
#' @return An object of class \code{tabxplor_grouped_tab}.
#' @export
dplyr_col_modify.tabxplor_grouped_tab <- function(data, cols) {
  out <- NextMethod()
  tab_restore(out, data)
}
# dplyr:::dplyr_col_modify.grouped_df

#' dplyr_reconstruct method for class tabxplor_grouped_tab
#' @importFrom dplyr dplyr_reconstruct
#' @method dplyr_reconstruct tabxplor_grouped_tab
#' @param data A data frame.
#' @param template Template to use for restoring attributes
#' @return An object of class \code{tabxplor_grouped_tab}.
#' @export
dplyr_reconstruct.tabxplor_grouped_tab <- function(data, template) {
  out <- NextMethod()
  tab_restore(out, data)
}
# dplyr:::dplyr_reconstruct.grouped_df

#' subset method for class tabxplor_grouped_tab
#' @param x A tabxplor_grouped_tab object.
#' @param i,j,... Indices
#' @param drop For matrices and arrays. If TRUE the result is coerced to the lowest
#' possible dimension (see the examples). This only works for extracting elements,
#' not for the replacement.
#' @usage "x[i]  ;  x[i, j, ... , drop = TRUE]"
#' @method `[` tabxplor_grouped_tab
#' @return An object of class \code{tabxplor_grouped_tab}.
#' @export
`[.tabxplor_grouped_tab` <- function(x, i, j, drop = FALSE) {
  out <- NextMethod()
  tab_restore(out, x)
}
# dplyr:::`[.grouped_df`

# #' @rdname `[.tabxplor_grouped_tab`
# `[` <- `[.tabxplor_grouped_tab`


#' set subset method for class tabxplor_grouped_tab
#' @param x A tabxplor_grouped_tab object.
#' @param i,j,... Indices.
#' @param value The new value.
#' @usage "x[i] <- value  ;   x[i, j, ...] <- value"
#' @method `[<-` tabxplor_grouped_tab
#' @return An object of class \code{tabxplor_grouped_tab}.
#' @export
`[<-.tabxplor_grouped_tab` <- function(x, i, j, ..., value) {
  out <- NextMethod()
  tab_restore(out, x)
}
# dplyr:::`[<-.grouped_df`

# #' @rdname `[<-.tabxplor_grouped_tab`
# `[<-` <- `[<-.tabxplor_grouped_tab`

#' set sub-subset method for class tabxplor_grouped_tab
#' @param x A tabxplor_grouped_tab object.
#' @param ... Indices
#' @param value The new value.
#' @usage "x[[...]] <- value"
#' @method `[[<-` tabxplor_grouped_tab
#' @return An object of class \code{tabxplor_grouped_tab}.
#' @export
`[[<-.tabxplor_grouped_tab` <- function(x, ..., value) {
  out <- NextMethod()
  tab_restore(out, x)
}
# dplyr:::`[[<-.grouped_df`

# #' @rdname `[[<-.tabxplor_grouped_tab`
# `[[<-` <- `[[<-.tabxplor_grouped_tab`

#' rowwise method for class tabxplor_grouped_tab
#' @importFrom dplyr rowwise
#' @method rowwise tabxplor_grouped_tab
#' @param data A tibble of class \code{tabxplor_tab}.
#' @param ... Variables to be preserved
#'   when calling summarise(). This is typically a set of variables whose
#'   combination uniquely identify each row.
#' @return An object of class \code{tabxplor_grouped_tab} and \code{rowwise_df}.
#' @export
rowwise.tabxplor_grouped_tab <- function(data, ...) {
  out <- NextMethod()
  groups <- dplyr::group_data(out)

  out <- rlang::exec(new_grouped_tab, out, groups, !!!tab_attrs(data))
  `class<-`(out, stringr::str_replace(class(out), "grouped_df", "rowwise_df"))
}

# #' @method rbind tabxplor_grouped_tab
# #' @export
# rbind.tabxplor_grouped_tab <- function(...) {
#   out <- NextMethod()
#   groups <- dplyr::group_data(out)
#   if (lv1_group_vars(out)) {
#     new_tab(out, subtext = get_subtext(.data), test = get_test(.data), render_extras = get_render_extras(.data), ci_settings = get_ci_settings(.data))
#   } else {
#     new_grouped_tab(out, groups, subtext = get_subtext(.data), test = get_test(.data), render_extras = get_render_extras(.data), ci_settings = get_ci_settings(.data))
#   }
# }
# # dplyr:::rbind.grouped_df
#
# #' @method cbind tabxplor_grouped_tab
# #' @export
# cbind.tabxplor_grouped_tab <- function(...) {
#   out <- NextMethod()
#   groups <- dplyr::group_data(out)
#   if (lv1_group_vars(out)) {
#     new_tab(out, subtext = get_subtext(.data), test = get_test(.data), render_extras = get_render_extras(.data), ci_settings = get_ci_settings(.data))
#   } else {
#     new_grouped_tab(out, groups, subtext = get_subtext(.data), test = get_test(.data), render_extras = get_render_extras(.data), ci_settings = get_ci_settings(.data))
#   }
# }
# # dplyr:::cbind.grouped_df

#' summarise method for class tabxplor_grouped_tab
#' @importFrom dplyr summarise
#' @method summarise tabxplor_grouped_tab
#' @param .data A tibble of class \code{tabxplor_tab}.
#' @param ... Name-value pairs of summary functions. The name will be the name of the
#' variable in the result.
#' @param .groups Grouping structure of the result.
#' @return An object of class \code{tabxplor_grouped_tab}.
#' @export
summarise.tabxplor_grouped_tab <- function(.data, ..., .groups = NULL) {
  out <- NextMethod()
  groups <- dplyr::group_data(out)
  if (lv1_group_vars(out)) {
    rlang::exec(new_tab, out, !!!tab_attrs(.data))
  } else {
    rlang::exec(new_grouped_tab, out, groups, !!!tab_attrs(.data))
  }
}


#' select method for class tabxplor_grouped_tab
#' @importFrom dplyr select
#' @method select tabxplor_grouped_tab
#' @param .data A tibble of class \code{tabxplor_tab}.
#' @param ... One or more unquoted expressions separated by commas. Variable names can be
#' used as if they were positions in the data frame, so expressions like \code{x:y} can
#'   be used to select a range of variables.
#' @return An object of class \code{tabxplor_grouped_tab}.
#' @export
select.tabxplor_grouped_tab <- function(.data, ...) {
  out <- NextMethod()
  groups <- dplyr::group_data(out)
  if (lv1_group_vars(out)) {
    rlang::exec(new_tab, out, !!!tab_attrs(.data))
  } else {
    rlang::exec(new_grouped_tab, out, groups, !!!tab_attrs(.data))
  }
}

#' rename method for class tabxplor_grouped_tab
#' @importFrom dplyr rename
#' @method rename tabxplor_grouped_tab
#' @param .data A tibble of class \code{tabxplor_tab}.
#' @param ... Use \code{new_name = old_name} to rename selected variables.
#' @return An object of class \code{tabxplor_grouped_tab}.
#' @export
rename.tabxplor_grouped_tab <- function(.data, ...) {
  out <- NextMethod()
  groups <- dplyr::group_data(out)
  if (lv1_group_vars(out)) {
    rlang::exec(new_tab, out, !!!tab_attrs(.data))
  } else {
    rlang::exec(new_grouped_tab, out, groups, !!!tab_attrs(.data))
  }
}

#' rename_with method for class tabxplor_grouped_tab
#' @importFrom dplyr rename_with
#' @method rename_with tabxplor_grouped_tab
#' @param .data A tibble of class \code{tabxplor_tab}.
#' @param ... Additional arguments passed onto \code{.fn}.
#' @param .fn A function used to transform the selected \code{.cols}. Should
#'   return a character vector the same length as the input.
#' @param .cols Columns to rename; defaults to all columns.
#' @return An object of class \code{tabxplor_grouped_tab}.
#' @export
rename_with.tabxplor_grouped_tab <- function(.data, .fn, .cols = dplyr::everything(), ...) {
  # `.cols` is a tidyselect selection, so it cannot go through NextMethod(): that forwards it as the
  # bare symbol `.cols`, dplyr's enquo(.cols) captures THAT, and tidyselect then resolves it as an
  # external vector -- deprecated since tidyselect 1.1.0 (and a future error). Re-inject the quosure
  # and dispatch by dropping our own class, the same fix pull.tabxplor_tab() uses for `var`.
  # `.data` keeps its grouped_df class, so the next method sees exactly what NextMethod() gave it.
  cols_quo <- rlang::enquo(.cols)
  bare     <- .data
  class(bare) <- setdiff(class(bare), "tabxplor_grouped_tab")
  out <- dplyr::rename_with(bare, .fn, !!cols_quo, ...)
  groups <- dplyr::group_data(out)
  if (lv1_group_vars(out)) {
    rlang::exec(new_tab, out, !!!tab_attrs(.data))
  } else {
    rlang::exec(new_grouped_tab, out, groups, !!!tab_attrs(.data))
  }
}


#' relocate method for class tabxplor_grouped_tab
#' @importFrom dplyr relocate
#' @method relocate tabxplor_grouped_tab
#' @param .data A tibble of class \code{tabxplor_tab}.
#' @param ... Columns to move.
# @param .before,.after Destination of columns selected by \code{...}. Supplying neither
#'  will move columns to the left-hand side; specifying both is an error.
#' @return An object of class \code{tabxplor_grouped_tab}.
#' @export
relocate.tabxplor_grouped_tab <- function(.data, ...) { #.before = NULL, .after = NULL
  out <- NextMethod()
  groups <- dplyr::group_data(out)
  if (lv1_group_vars(out)) {
    rlang::exec(new_tab, out, !!!tab_attrs(.data))
  } else {
    rlang::exec(new_grouped_tab, out, groups, !!!tab_attrs(.data))
  }
} # dplyr:::relocate.grouped_df

# #' distinct_ method for class tabxplor_grouped_tab
# #' @importFrom dplyr distinct_
# #' @method distinct_ tabxplor_grouped_tab
# #' @param .data A tibble of class \code{tabxplor_tab}.
# #' @return An object of class \code{tabxplor_grouped_tab}.
# #' @export
# distinct_.tabxplor_grouped_tab <- function(.data, ..., .dots = list(), .keep_all = FALSE) {
#   out <- NextMethod()
#   groups <- dplyr::group_data(out)
#   if (lv1_group_vars(out)) {
#     new_tab(out, subtext = get_subtext(.data), test = get_test(.data), render_extras = get_render_extras(.data), ci_settings = get_ci_settings(.data))
#   } else {
#     new_grouped_tab(out, groups, subtext = get_subtext(.data), test = get_test(.data), render_extras = get_render_extras(.data), ci_settings = get_ci_settings(.data))
#   }
# }
# # dplyr:::distinct_.grouped_df













#' @rdname tab_cast
#' @keywords internal
# @export
gtab_cast <- function(x, to, ..., x_arg = "", to_arg = "") {
  #based upon vctrs:::gdf_cast()
  df <- vctrs::df_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)
  vars <- dplyr::group_vars(to)
  drop <- dplyr::group_by_drop_default(to)
  gdf <- dplyr::grouped_df(df, vars, drop = drop)

  groups <- dplyr::group_data(gdf)
  rlang::exec(new_grouped_tab, gdf, groups, !!!tab_attrs(to))
}

#' @rdname tab_cast
#' @keywords internal
# @export
gtab_ptype2 <- function(x, y, ..., x_arg = "", y_arg = "") {
  #based upon vctrs:::gdf_ptype2
  common <- vctrs::df_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)
  x_vars <- dplyr::group_vars(x)
  y_vars <- dplyr::group_vars(y)
  vars <- union(x_vars, y_vars)
  drop <- dplyr::group_by_drop_default(x) && dplyr::group_by_drop_default(y)
  gdf <-  dplyr::grouped_df(common, vars, drop = drop)

  groups <- dplyr::group_data(gdf)
  rlang::exec(new_grouped_tab, gdf, groups, !!!tab_attrs(x))
}

#Self-self
#' @describeIn tab_cast find common ptype between tabxplor_grouped_tab and tabxplor_grouped_tab
#' @return An object of class \code{tabxplor_grouped_tab}.
#' @export
vec_ptype2.tabxplor_grouped_tab.tabxplor_grouped_tab <- function(x, y, ...) {
  gtab_ptype2(x, y, ...)
}
#' @describeIn tab_cast convert tabxplor_grouped_tab to tabxplor_grouped_tab
#' @return An object of class \code{tabxplor_grouped_tab}.
#' @export
vec_cast.tabxplor_grouped_tab.tabxplor_grouped_tab <- function(x, to, ...) {
  gtab_cast(x, to, ...)
}

#grouped_tab / grouped_df
#' @describeIn tab_cast find common ptype between tabxplor_grouped_tab and grouped_df
#' @return An object of class \code{tabxplor_grouped_tab}.
#' @export
vec_ptype2.tabxplor_grouped_tab.grouped_df <- function(x, y, ...) {
  gtab_ptype2(x, y, ...)
}
#' @describeIn tab_cast find common ptype between grouped_df and tabxplor_grouped_tab
#' @return An object of class \code{grouped_df}.
#' @export
vec_ptype2.grouped_df.tabxplor_grouped_tab <- function(x, y, ...) {
  gtab_ptype2(x, y, ...)
}
#' @describeIn tab_cast convert grouped_df to tabxplor_grouped_tab
#' @return An object of class \code{tabxplor_grouped_tab}.
#' @export
vec_cast.tabxplor_grouped_tab.grouped_df <- function(x, to, ...) {
  gtab_cast(x, to, ...)
}
#' @describeIn tab_cast convert tabxplor_grouped_tab to grouped_df
#' @return An object of class \code{grouped_df}.
#' @export
vec_cast.grouped_df.tabxplor_grouped_tab <- function(x, to, ...) {
  #vctrs:::gdf_cast
  df <- vctrs::df_cast(x, to, ...)
  vars <- dplyr::group_vars(to)
  drop <- dplyr::group_by_drop_default(to)
  dplyr::grouped_df(df, vars, drop = drop)
}

#grouped_tab / tab
#' @describeIn tab_cast find common ptype between tabxplor_grouped_tab and tabxplor_tab
#' @return An object of class \code{tabxplor_grouped_tab}.
#' @export
vec_ptype2.tabxplor_grouped_tab.tabxplor_tab <- function(x, y, ...) {
  gtab_ptype2(x, y, ...)
}
#' @describeIn tab_cast find common ptype between tabxplor_tab and tabxplor_grouped_tab
#' @return An object of class \code{tabxplor_tab}.
#' @export
vec_ptype2.tabxplor_tab.tabxplor_grouped_tab <- function(x, y, ...) {
  gtab_ptype2(x, y, ...)
}
#' @describeIn tab_cast convert tabxplor_tab to tabxplor_grouped_tab
#' @return An object of class \code{tabxplor_grouped_tab}.
#' @export
vec_cast.tabxplor_grouped_tab.tabxplor_tab <- function(x, to, ...) {
  gtab_cast(x, to, ...)
}
#' @describeIn tab_cast convert tabxplor_grouped_tab to tabxplor_tab
#' @return An object of class \code{tabxplor_tab}.
#' @export
vec_cast.tabxplor_tab.tabxplor_grouped_tab <- function(x, to, ...) {
  tab_cast(x, to, ...)
}

#grouped_tab / tbl_df
#' @describeIn tab_cast find common ptype between tabxplor_grouped_tab and tbl_df
#' @return An object of class \code{tabxplor_grouped_tab}.
#' @export
vec_ptype2.tabxplor_grouped_tab.tbl_df <- function(x, y, ...) {
  gtab_ptype2(x, y, ...)
}
#' @describeIn tab_cast find common ptype between tbl_df and tabxplor_grouped_tab
#' @return An object of class \code{tbl_df}.
#' @export
vec_ptype2.tbl_df.tabxplor_grouped_tab <- function(x, y, ...) {
  gtab_ptype2(x, y, ...)
}
#' @describeIn tab_cast convert tbl_df to tabxplor_grouped_tab
#' @return An object of class \code{tabxplor_grouped_tab}.
#' @export
vec_cast.tabxplor_grouped_tab.tbl_df <- function(x, to, ...) {
  gtab_cast(x, to, ...)
}
#' @describeIn tab_cast convert tabxplor_grouped_tab to tbl_df
#' @return An object of class \code{tbl_df}.
#' @export
vec_cast.tbl_df.tabxplor_grouped_tab <- function(x, to, ...) {
  vctrs::tib_cast(x, to, ...)
}

#grouped_tab / data.frame
#' @describeIn tab_cast find common ptype between tabxplor_grouped_tab and data.frame
#' @return An object of class \code{tabxplor_grouped_tab}.
#' @export
vec_ptype2.tabxplor_grouped_tab.data.frame <- function(x, y, ...) {
  gtab_ptype2(x, y, ...)
}
#' @describeIn tab_cast find common ptype between data.frame and tabxplor_grouped_tab
#' @return An data.frame.
#' @export
vec_ptype2.data.frame.tabxplor_grouped_tab <- function(x, y, ...) {
  gtab_ptype2(x, y, ...)
}
#' @describeIn tab_cast convert data.frame to tabxplor_grouped_tab
#' @return An object of class \code{tabxplor_grouped_tab}.
#' @export
vec_cast.tabxplor_grouped_tab.data.frame <- function(x, to, ...) {
  gtab_cast(x, to, ...)
}
#' @describeIn tab_cast convert tabxplor_grouped_tab to data.frame
#' @return An data.frame.
#' @export
vec_cast.data.frame.tabxplor_grouped_tab <- function(x, to, ...) {
  vctrs::df_cast(x, to, ...)
}


#Colors for printing fmt in tabs -------------------------------------------------------

# # Test function to see how colors print
# #' @keywords internal
# color_graph <- function(former = NULL, new = NULL, new2 = NULL, new3 = NULL) {
#   HCLformer <- tibble::as_tibble(t(round(jamba::col2hcl(former)[-4,], 0)))
#   HCLnew    <- tibble::as_tibble(t(round(jamba::col2hcl(new   )[-4,], 0)))
#   HCLnew2   <- tibble::as_tibble(t(round(jamba::col2hcl(new2  )[-4,], 0)))
#   HCLnew3   <- tibble::as_tibble(t(round(jamba::col2hcl(new3  )[-4,], 0)))
#
#   colors <- tibble::tibble(
#     color = rep(c(former, new, new2, new3), 4),
#     text = c(former, new, new2, new3,
#              HCLformer$H, HCLnew$H, HCLnew2$H, HCLnew3$H,
#              HCLformer$C, HCLnew$C, HCLnew2$C, HCLnew3$C,
#              HCLformer$L, HCLnew$L, HCLnew2$L, HCLnew3$L ),
#     x = rep(c(if(length(former) != 0){1:length(former)} else {NULL},
#               if(length(new   ) != 0){1:length(new)   } else {NULL},
#               if(length(new2  ) != 0){1:length(new2)  } else {NULL},
#               if(length(new3  ) != 0){1:length(new3)  } else {NULL} ), 4),
#     y = c(rep(1 , length(former)),
#           rep(0 , length(new   )),
#           rep(-1, length(new2  )),
#           rep(-2, length(new3  )),
#
#           rep(-4, length(former)),
#           rep(-5, length(new   )),
#           rep(-6, length(new2  )),
#           rep(-7, length(new3  )),
#
#           rep(-9, length(former)),
#           rep(-10, length(new   )),
#           rep(-11, length(new2  )),
#           rep(-12, length(new3  )),
#
#           rep(-14, length(former)),
#           rep(-15, length(new   )),
#           rep(-16, length(new2  )),
#           rep(-17, length(new3  ))
#     )
#   )
#   color_scale <- c(if(length(former) != 0){former} else {NULL},
#                    if(length(new   ) != 0){new   } else {NULL},
#                    if(length(new2  ) != 0){new2  } else {NULL},
#                    if(length(new3  ) != 0){new3  } else {NULL}
#   ) %>% purrr::set_names(.)
#   color_scale <- color_scale[!duplicated(names(color_scale))]
#
#   ggplot2::ggplot(colors, ggplot2::aes(x = x, y = y, color = color, label = text)) +
#     ggplot2::geom_text(fontface = "bold") +
#     ggplot2::scale_color_manual(values = color_scale) +
#     ggplot2::theme_minimal() +
#     ggplot2::theme(panel.grid = ggplot2::element_line(colour = "white")) +
#     ggplot2::ylim(-18, 3) +
#     ggplot2::annotate("text", x = 1, y =   2, label = "Colors :") +
#     ggplot2::annotate("text", x = 1, y =  -3, label = "Hue :") +
#     ggplot2::annotate("text", x = 1, y =  -8, label = "Chroma :") +
#     ggplot2::annotate("text", x = 1, y = -13, label = "Luminance :")
#
# }


## 8-BIT FALLBACK PALETTES (RStudio console only) ----
# The console default is the 24-bit OKLCH palette (below). RStudio's console does not render
# 24-bit truecolor, so there we fall back to these curated 256-colour palettes -- 4 over + 4 neg
# each, trimmed from the historical tabxplor palettes (the old faint pos1/neg1 and the ratio slot
# dropped, to match the 4-intensity model). Positron / modern terminals get the 24-bit palette.
#' @keywords internal
palette_8bit <- list(
  text_light = c("#33FFFF", "#00CCFF", "#0066FF", "#0000FF",   # over (faint -> strong)
                 "#FF9933", "#FF6600", "#FF3333", "#FF0000"),  # under
  text_dark  = c("#CCFF33", "#99FF33", "#33FF33", "#00FF00",
                 "#FF9933", "#FF6633", "#FF3300", "#FF0000"),
  bg_light   = c("#F6F3FF", "#E9E3FF", "#DED3FF", "#D2C3FF",
                 "#fff8e6", "#ffeab1", "#fddb7c", "#ffce2d"),
  bg_dark    = c("#000066", "#000099", "#0000CC", "#0000FF",
                 "#660000", "#990000", "#CC0000", "#FF0000")
)


## NEW COLOR PALETTES (to wire to the code) ----

# OKLCH Chroma Peaks
# - Blue            H265 / L45  ; H180 to 265
# - to Orange Red   H28 / L62   ; H90 to 28   (avoid true red ?)
# 
# - Green           H142 / L86  ; H110 to H160
# - to Violet Red   H325 / L70  ; H285 to H25

### Light palette ----
#### Text colors ----
default_text_colors <- c(
  "#02a5b3", # oklch(0.66 0.1124 205) # better for color blindness
  #"#03ab86", # oklch(0.66 0.13 167)   # "#20a89b", # oklch(0.66 0.11 185)  "#0ba6ba", # "oklch(0.67 0.11 210)",
  "#0891c9", # oklch(0.62 0.13 235)   # "#0890a2", # "oklch(0.6 0.1 210)",
  "#0267c7", # oklch(0.52 0.17 255)   # "#027ad2", # "oklch(0.57 0.16 250)",
  "#300dfd"  # oklch(0.47 0.30 270)   # "#265aff"  # "#2f60ee"  # "oklch(0.55 0.22 265)",
)
default_text_colors_neg <- c( 
  # more ligthness differences for color blinds
  "#dca331", # oklch(0.75 0.1400 80)   # "#d6a54d", # oklch(0.75 0.1197 80)    
  "#de7c01", # oklch(0.68 0.1596 60)   # "#de7c01", # oklch(0.68 0.1596 60)
  "#dd5301", # oklch(0.62 0.1868 42)   # "#dd5301", # oklch(0.62 0.1868 42)
  "#d60103"#,# oklch(0.55 0.2253 29)   # "#d60103"#,# oklch(0.55 0.2253 29) 
  # "#b58629", # oklch(0.65 0.12 80)    # neg2 = "#c38c46", # oklch(0.68 0.11  70)   # "#d08747", # "oklch(0.69 0.12 60)",
  # "#c46d02", # oklch(0.62 0.1449 60)  # neg3 = "#d26c28", # oklch(0.64 0.15  50)   # "#c8692d", # "oklch(0.62 0.14 50)",
  # "#cf4e01", # oklch(0.59 0.1775 42)  # neg4 = "#da4c01", # oklch(0.61 0.19  40)   # "#d04b0c", # "oklch(0.59 0.18 40)",
  # "#dc0204"#,# oklch(0.56 0.23  29)   # neg5 = "#dc0204"#,# oklch(0.56 0.23  29)   # "#e61301"  # "#d10f00"  # "oklch(0.54 0.22 30)",
)

#### Background colors ----
default_background_colors <-  c(
  "#dffcff", # oklch(0.97 0.0304 205)  # better for color blindness
  # "#e3fcf1", # oklch(0.97 0.0300 167) # "#e3fcf1", # oklch(0.97 0.0300 167)   # "#F6F3FF", # oklch(0.97 0.016 295)
  "#d7efff", # oklch(0.94 0.0336 235) # "#d4f0ff", # oklch(0.94 0.0358 230)   # "#E9E3FF", # oklch(0.93 0.038 295)
  "#cee3ff", # oklch(0.91 0.0439 255) # "#d3e2ff", # oklch(0.91 0.0429 265)   # "#DED3FF", # oklch(0.89 0.060 295)
  "#bbccff"  # oklch(0.85 0.0733 270) # "#c8c7ff"  # oklch(0.85 0.0771 285)   # "#D2C3FF"# # oklch(0.85 0.084 295)
)
default_background_colors_neg <- c(
  "#fff4e1", # oklch(0.97 0.0271 80)   # "#ffeccd", # oklch(0.95 0.0456 80)    # "#ffe9e5", # oklch(0.95 0.0249 30)  # "#fff8e6", # oklch(0.98 0.025 90)
  "#ffe6d3", # oklch(0.94 0.0374 60)   # "#ffddc3", # oklch(0.92 0.051 60)     # "#ffdad3", # oklch(0.92 0.0461 30)  # "#ffeab1", # oklch(0.94 0.076 90)
  "#ffd7c8", # oklch(0.91 0.0488 42)   # "#ffcebc", # oklch(0.89 0.0608 42)    # "#ffcdc5", # oklch(0.89 0.0575 30)  # "#fddb7c", # oklch(0.90 0.12  90)
  "#ffbaaf"#,# oklch(0.85 0.082 29)    # "#ffbfb5"#,# oklch(0.86 0.0754 29.01) # "#ffbfb4"#,# oklch(0.86 0.0754 30)  # "#ffce2d"#,# oklch(0.87 0.168 90)
)

#### Background-legend colors (Phase 14c, rebaked 14l) ----
# WHY: a colour legend break-word that describes the BACKGROUND channel cannot be drawn with a fill in
# every medium -- an Excel rich-text run and a ggpubr text label carry a font colour only. Drawn as
# text, the background palette above (L 0.85-0.97) is invisible on a white sheet. These are the same
# hues at -0.30 OKLCH lightness and 2x chroma (capped to gamut), so the ladder and the visual link to
# the fills survive. Produced by dev/color_palette_tools.R::darken_for_legend(); regenerate there.
# DESIGN (Phase 14l): the 14c bake was -0.2 L at 1x chroma and read as faint. The fix needed BOTH
# levers, and the reason is measured (see darken_for_legend's header): APCA Lc is driven by lightness
# almost alone, so chroma alone would have fixed the greyness and left Lc at 39.6-60.8 -- 3 of the 4
# slots below the >= 60 bar these palettes were designed against. -0.30/2x gives Lc 55.3-75.3 and is
# fully in-gamut on all 8 slots, so the chroma proportions inherited from the fills survive exactly
# (a bigger boost caps the strong slots out and flattens the ladder instead).
# NOTE: light only. The dark background palette (L 0.20-0.35) already reads as text on the white sheet
# an Excel legend cell sits on, and darkening would collapse it to black -- build_palettes() uses it
# as-is.
default_bg_legend_colors <- c(
  "#67A1A7", # oklch(0.67 0.0611 204)  <- #dffcff
  "#6492B0", # oklch(0.64 0.0674 238)  <- #d7efff
  "#5E85B8", # oklch(0.61 0.0896 255)  <- #cee3ff
  "#5169C7"  # oklch(0.55 0.1481 270)  <- #bbccff
)
default_bg_legend_colors_neg <- c(
  "#A7936F", # oklch(0.67 0.0553 82)   <- #fff4e1
  "#AE815E", # oklch(0.64 0.0741 59)   <- #ffe6d3
  "#B56E53", # oklch(0.61 0.0989 41)   <- #ffd7c8
  "#BE4034"  # oklch(0.55 0.1639 29)   <- #ffbaaf
)


### Dark palette ----
#### Dark text colors ----
default_dark_text_colors <- c(
  "#028282", # oklch(0.55 0.0934 195) # better for color blindness
  "#018bc1", # oklch(0.60 0.1270 235)
  "#4687d8", # oklch(0.62 0.1400 255)
  "#6987ff"#,# oklch(0.66 0.1797 270)
  # "#288463", # oklch(0.55 0.1000 165)   "#03ab86", # oklch(0.66 0.13 167)   
  # "#0190a3", # oklch(0.60 0.1037 210)   "#0891c9", # oklch(0.62 0.13 235)   
  # "#078fd1", # oklch(0.62 0.1406 240)   "#0267c7", # oklch(0.52 0.17 255)   
  # "#5b8bff"#,# oklch(0.66 0.1798 265)   "#300dfd"  # oklch(0.47 0.30 270)   
)
default_dark_text_colors_neg <- c(
  # more ligthness differences for color blinds
  "#867002", # oklch(0.55 0.1124 95)
  "#b87501", # oklch(0.62 0.1341 70)
  "#ec6f02", # oklch(0.68 0.1792 50)  
  "#ff626b"# # oklch(0.70 0.1906 20)  
  # "#977e05", # oklch(0.60 0.1221 95)  "#7a6001", # oklch(0.50 0.102 90),  # "#b58629", # oklch(0.65 0.12 80)    
  # "#c17b01", # oklch(0.64 0.1384 70)  "#a65c01", # oklch(0.55 0.129 60),  # "#c46d02", # oklch(0.62 0.1449 60)  
  # "#ec6f02", # oklch(0.68 0.1792 50)  "#d74b01", # oklch(0.60 0.188 40),  # "#cf4e01", # oklch(0.59 0.1775 42)  
  # "#ff626b"# # oklch(0.70 0.1906 20)  "#fe4a36"#,# oklch(0.67 0.220 30),  # "#dc0204"#,# oklch(0.56 0.23  29)   
)


#### Dark background colors ----
default_dark_background_colors <-  c(
  "#001b1b", # oklch(0.20 0.0336 195)  # better for color blindness
  "#002537", # oklch(0.25 0.0526 235)
  "#132d5c", # oklch(0.30 0.0900 261)
  "#17226d"#,# oklch(0.30 0.1300 270)
  #"#001c11", # oklch(0.20 0.0418 165)  # "#002115", # oklch(0.22 0.0461 165) # "#001c12", # oklch(0.20 0.0407 167)    # "#e3fcf1", # oklch(0.97 0.0300 167) 
  #"#00272d", # oklch(0.25 0.0429 210)  # "#00272d", # oklch(0.25 0.0429 210) # "#002538", # oklch(0.25 0.0543 236.97)   # "#d7efff", # oklch(0.94 0.0336 235) 
  #"#00314c", # oklch(0.30 0.0684 240)  # "#002c45", # oklch(0.28 0.0640 240) # "#002d5c", # oklch(0.30 0.0961 254.26)   # "#cee3ff", # oklch(0.91 0.0439 255) 
  #"#0d246e"#,# oklch(0.30 0.1300 265)  # "#0d246e"#,# oklch(0.30 0.1300 265) # "#243278"#,# oklch(0.35 0.12 270.4)   # "#bbccff"  # oklch(0.85 0.0733 270) 
)
default_dark_background_colors_neg <- c(
  "#1c1600", # oklch(0.20 0.0407 95) # "#211a00", # oklch(0.22 0.045 95) # "#1f1400", # oklch(0.2 0.0412 81.48)   # "#fff4e1", # oklch(0.97 0.0271 80) 
  "#321c00", # oklch(0.25 0.0537 70) # "#321c00", # oklch(0.25 0.0537 70) # "#2f1d0e", # oklch(0.25 0.0374 59.56)   # "#ffe6d3", # oklch(0.94 0.0374 60) 
  "#4c1f00", # oklch(0.30 0.0792 50) # "#441b00", # oklch(0.28 0.0738 50) # "#511900", # oklch(0.3 0.0906 41.62)   # "#ffd7c8", # oklch(0.91 0.0488 42) 
  "#6b141f"# # oklch(0.35 0.1200 20) # "#6b141f"# # oklch(0.35 0.12 19.39) # "#6c1610"#,# oklch(0.35 0.12 29)   # "#ffbaaf"#,# oklch(0.85 0.082 29)  
)

# ### Color palettes visual tests, with color blind mode ----
# source("d:/Statistiques/github/tabxplor/dev/color_palette_tools.R", encoding = "UTF-8")
# # Light palette
# light_text_palette <- c(plain= "#9f9f9f", default_text_colors, default_text_colors_neg)
# light_bg_palette   <- c(plain= "#ffffff",default_background_colors, default_background_colors_neg)
# preview_color_grid(light_text_palette, light_bg_palette) # #show_contrast = FALSE  
# #    Lc ≥ 75 for body text ; ≥ 60 for larger/heavier text ; ≥ 45 for large headlines ; below ~30 is decorative-only.


# #   color blindness
# preview_color_grid(simulate_cvd_farver(light_text_palette, type = "deutan", severity = 1), 
#                    simulate_cvd_farver(light_bg_palette, type = "deutan", severity = 1),
#                    table_bg = lcd_simulate_oklch("#ffffff")
#                    )
# preview_color_grid(simulate_cvd_farver(light_text_palette, type = "deutan", severity = 0.5), 
#                    simulate_cvd_farver(light_bg_palette, type = "deutan", severity = 0.5),
#                    table_bg = lcd_simulate_oklch("#ffffff")
#                    )
# preview_color_grid(simulate_cvd_farver(light_text_palette, type = "protan"), 
#                    simulate_cvd_farver(light_bg_palette, type = "protan"),
#                    table_bg = lcd_simulate_oklch("#ffffff")
#                    )
# preview_color_grid(simulate_cvd_farver(light_text_palette, type = "protan", severity = 0.5), 
#                    simulate_cvd_farver(light_bg_palette, type = "protan", severity = 0.5),
#                    table_bg = lcd_simulate_oklch("#ffffff")
#                    )

# #   bad LCD approximation
# preview_color_grid(lcd_simulate_oklch(light_text_palette), 
#                    lcd_simulate_oklch(light_bg_palette),
#                    table_bg = lcd_simulate_oklch("#ffffff")
#                    )

# # default_text_colors |> farver::decode_colour(to = "oklch") # Inspect OKLCH coordinates



# # Dark palette
# dark_text_palette <- c(plain= "#707070", default_dark_text_colors, default_dark_text_colors_neg)
# dark_bg_palette   <- c(plain= "#111111", default_dark_background_colors, default_dark_background_colors_neg)
# preview_color_grid(dark_text_palette, dark_bg_palette,  table_bg = "#111111")

# #   color blindness
# preview_color_grid(simulate_cvd_farver(dark_text_palette, type = "deutan", severity = 0.5), 
#                    simulate_cvd_farver(dark_bg_palette, type = "deutan"),
#                    table_bg = lcd_simulate_oklch("#111111")
#                    )
# preview_color_grid(simulate_cvd_farver(dark_text_palette, type = "deutan"), 
#                    simulate_cvd_farver(dark_bg_palette, type = "deutan"),
#                    table_bg = lcd_simulate_oklch("#111111")
#                    )

# preview_color_grid(simulate_cvd_farver(dark_text_palette, type = "protan"), 
#                    simulate_cvd_farver(dark_bg_palette, type = "protan"),
#                    table_bg = lcd_simulate_oklch("#111111")
#                    )

# #   bad LCD approximation
# preview_color_grid(lcd_simulate_oklch(dark_text_palette), 
#                   lcd_simulate_oklch(dark_bg_palette),
#                   table_bg = lcd_simulate_oklch("#111111")
#                   )
                   

# # Simuler une palette normale et une palette color blind cote-à-cote
# plot_oklch_hue_strip_cvd(L = 0.65,type = "deutan", severity = 1, C=0.16) # chroma_mode = "max"
# plot_oklch_hue_strip_cvd(L = 0.65,type = "deutan", severity = 0.5, C=0.16) 
# plot_oklch_hue_strip_cvd(L = 0.65,type = "protan", severity = 1, C=0.16)
# plot_oklch_hue_strip_cvd(L = 0.65,type = "tritan", severity = 1, C=0.16)


# # preview_color_grid(diff_colors, set_luminance(background_colors, 0.99)) 
# # set_luminance(background_colors, 0.99) |> farver::get_channel("l", space = "oklch")
# # # set_luminance(background_colors, c(0.99, 0.90, 0.85, 0.80, 0.72))

# # preview_color_grid(diff_colors, set_luminance(background_colors2, 0.95)) 

# # preview_color_grid(diff_colors, set_luminance(diff_colors, 0.95)) 
# # preview_color_grid(diff_colors, set_luminance(diff_colors, 0.8) |> set_chroma(0.12)) 


# # preview_luminance_grid("#59c5bf", "#b9c653")                  # fixed source chroma, capped to gamut
# # preview_luminance_grid("#59c5bf", "#b9c653", chroma = "max")  # most vivid shade at each L
# # preview_luminance_grid("#0185e4", "#68b430", l_values = seq(0.40, 0.90, by = 0.10)) # custom lightness ramp
# # # Lc ≥ 75 for body text ; ≥ 60 for larger/heavier text ; ≥ 45 for large headlines ; below ~30 is decorative-only.







## Color functions ----


# PURPOSE: the render-time colour palettes (Phase 13a). Ten OKLCH base palettes -- eight being one per
# (light/dark theme x text/background channel x over-/under-represented side), plus the two Phase-14c
# bg_legend sides (the font stand-in for the fills, light only) -- each 4 hex codes
# (faint -> strong), position-based (no pos1..neg5 names, no ratio slot). They are composed into
# 8-element slot vectors (4 over + 4 under) and pre-built once into crayon style functions, stored
# in an internal env and only rebuilt by set_color_palette(). The engine indexes them by the
# integer slot from fmt_color_slots() (1:4 = over intensities, 5:8 = under). See dev/new_colors_UI.md.
#' @keywords internal
tabxplor_palette_env <- new.env(parent = emptyenv())

# The ten OKLCH defaults (defined above as default_*_colors), as the seed base palette.
#' @keywords internal
default_palette_base <- function() {
  list(
    text_colors                = default_text_colors,
    text_colors_neg            = default_text_colors_neg,
    background_colors          = default_background_colors,
    background_colors_neg      = default_background_colors_neg,
    dark_text_colors           = default_dark_text_colors,
    dark_text_colors_neg       = default_dark_text_colors_neg,
    dark_background_colors     = default_dark_background_colors,
    dark_background_colors_neg = default_dark_background_colors_neg,
    bg_legend_colors           = default_bg_legend_colors,
    bg_legend_colors_neg       = default_bg_legend_colors_neg
  )
}

# Compose the base palettes into the 8-slot hex vectors + pre-built crayon functions. The console
# uses 24-bit OKLCH, except in the RStudio console (no truecolor) where the curated 8-bit fallback
# is used; exports (mode = "color_code") always use the 24-bit hex.
#' @keywords internal
build_palettes <- function() {
  e <- tabxplor_palette_env
  if (is.null(e$base)) e$base <- default_palette_base()
  b <- e$base
  e$hex <- list(
    text_light = c(b$text_colors,            b$text_colors_neg),
    text_dark  = c(b$dark_text_colors,       b$dark_text_colors_neg),
    bg_light   = c(b$background_colors,       b$background_colors_neg),
    bg_dark    = c(b$dark_background_colors,  b$dark_background_colors_neg),
    # Phase 14c: the FONT stand-in for the background palette, used where a fill is impossible (an
    # Excel rich-text run / a ggpubr text label -> the colour legend). See default_bg_legend_colors.
    # There is no dark variant to bake: the legend cell's page is white whatever the theme, and the
    # dark fills already read there.
    bg_legend_light = c(b$bg_legend_colors,        b$bg_legend_colors_neg),
    bg_legend_dark  = c(b$dark_background_colors,  b$dark_background_colors_neg)
  )
  bit8 <- isTRUE(Sys.getenv("RSTUDIO") == "1")
  ncol <- if (bit8) 256L else crayon::num_colors()
  mk <- function(key, is_bg) {
    src <- if (bit8) palette_8bit[[key]] else e$hex[[key]]
    purrr::map(src, ~ crayon::make_style(., bg = is_bg, colors = ncol))
  }
  e$crayon <- list(
    text_light = mk("text_light", FALSE), text_dark = mk("text_dark", FALSE),
    bg_light   = mk("bg_light",   TRUE),  bg_dark   = mk("bg_dark",   TRUE)
  )
  invisible()
}

#' Define the color palette used to print \code{\link{tab}}
#' @describeIn tab_many customise the color palette used to print \code{\link{tab}}. Each palette
#' is 4 hex codes ordered faint -> strong. Provide only the ones you want to change; the OKLCH
#' defaults are used otherwise. The crayon styles are (re)built once, not per cell.
#' @param text_colors,text_colors_neg,background_colors,background_colors_neg Light-theme palettes
#' (4 hex each): the text (font) and background (fill) colours for the over- (\code{*_colors}) and
#' under-represented (\code{*_colors_neg}) sides.
#' @param dark_text_colors,dark_text_colors_neg,dark_background_colors,dark_background_colors_neg
#' The dark-theme counterparts (4 hex each).
#' @param bg_legend_colors,bg_legend_colors_neg (4 hex each) The FONT stand-in for
#' \code{background_colors} in the colour legend of media that cannot fill a run (Excel,
#' \code{\link{tab_plot}}); the defaults are the background colours at -0.2 OKLCH lightness. Setting
#' \code{background_colors} without these makes them follow it unchanged (readable only if your fills
#' already are). There is no dark counterpart: an Excel legend cell is on a white page whatever the
#' theme, and the dark fills read there as-is.
#' @param theme \code{"light"} or \code{"dark"} for the console / exports, or \code{"auto"} to detect
#' the console's colour scheme now (the RStudio theme, the Positron theme, or \code{COLORFGBG};
#' \code{"light"} when it cannot be told). Defaults to the current setting. Detection is best-effort
#' and resolved ONCE: call again after changing your editor's theme. (This is the console only ---
#' \code{\link{tab_css}} / \code{\link{tab_kable}} take their own \code{theme = "auto"}, which follows
#' the reader's browser.)
#' @return Sets the internal color palettes (invisibly) and the option
#' \code{"tabxplor.color_style_theme"}.
#' @export
#' @examples set_color_palette(text_colors = c("#02a5b3", "#0891c9", "#0267c7", "#300dfd"))
set_color_palette <- function(text_colors = NULL, text_colors_neg = NULL,
                              background_colors = NULL, background_colors_neg = NULL,
                              dark_text_colors = NULL, dark_text_colors_neg = NULL,
                              dark_background_colors = NULL, dark_background_colors_neg = NULL,
                              bg_legend_colors = NULL, bg_legend_colors_neg = NULL,
                              theme = NULL) {
  e <- tabxplor_palette_env
  if (is.null(e$base)) e$base <- default_palette_base()

  set1 <- function(nm, val) {
    if (is.null(val)) return(invisible())
    if (!is.character(val) || length(val) != 4L) {
      cli::cli_abort(c("{.arg {nm}} must be 4 hex color codes (faint -> strong).",
                       "x" = "{length(val)} were given."))
    }
    e$base[[nm]] <- unname(val)
  }
  set1("text_colors", text_colors)
  set1("text_colors_neg", text_colors_neg)
  set1("background_colors", background_colors)
  set1("background_colors_neg", background_colors_neg)
  set1("dark_text_colors", dark_text_colors)
  set1("dark_text_colors_neg", dark_text_colors_neg)
  set1("dark_background_colors", dark_background_colors)
  set1("dark_background_colors_neg", dark_background_colors_neg)
  set1("bg_legend_colors", bg_legend_colors)
  set1("bg_legend_colors_neg", bg_legend_colors_neg)
  # A custom background palette must not keep the DEFAULT legend hues (a green fill described by a
  # blue break-word). Deriving them would need an OKLCH gamut mapper (farver, dev-only), so the
  # honest fallback is the fills themselves -- set bg_legend_colors explicitly for a readable one.
  if (!is.null(background_colors)     && is.null(bg_legend_colors))
    e$base$bg_legend_colors <- unname(background_colors)
  if (!is.null(background_colors_neg) && is.null(bg_legend_colors_neg))
    e$base$bg_legend_colors_neg <- unname(background_colors_neg)

  # Phase 14g: `theme = "auto"` detects the console's colour scheme (tx_detect_theme(): RStudio's
  # getThemeInfo(), Positron's cached settings, COLORFGBG; anything unresolved -> "light"). The
  # RESOLVED value is stored, so no per-print cost -- and so a mid-session theme switch needs another
  # set_color_palette(theme = "auto"). NULL keeps the current setting, detecting only if there is none
  # (what .onLoad does).
  if (is.null(theme)) {
    if (is.null(getOption("tabxplor.color_style_theme"))) {
      options("tabxplor.color_style_theme" = tx_detect_theme())
    }
  } else {
    stopifnot(length(theme) == 1L, theme %in% c("dark", "light", "auto"))
    options("tabxplor.color_style_theme" = if (identical(theme, "auto")) tx_detect_theme() else theme)
  }

  build_palettes()
  invisible()
}

# === COMPAT (Phase 13a) — deprecated colour surface wired to the new behaviour, no error ==========
# Thin shims mapping the removed 1.3.x/Phase-5 API onto set_color_palette() / the new options / the
# new grammar, each with lifecycle::deprecate_soft(). Grep "COMPAT (Phase 13a)" to find/remove them.

#' Set the color style (deprecated)
#' @describeIn tab_many `r lifecycle::badge("deprecated")` Superseded by \code{set_color_palette()}.
#' Kept as a back-compat shim: \code{type}/\code{theme} still take effect (as options);
#' \code{custom_palette} maps its over/under colours onto the new 4+4 palette; \code{html_24_bit}
#' is inert (exports are always 24-bit).
#' @param custom_palette `r lifecycle::badge("deprecated")` A former 10/11-slot palette; its 4
#' over- and 4 under-represented colours are mapped onto \code{set_color_palette()}.
#' @param html_24_bit `r lifecycle::badge("deprecated")` Inert since 1.4.0 (exports are always 24-bit).
#' @export
set_color_style <- function(type = c("text", "bg"), theme = NULL,
                            html_24_bit = NULL, custom_palette = NULL) {
  lifecycle::deprecate_soft("1.4.0", "set_color_style()", "set_color_palette()")
  # Phase 14l: the `tabxplor.color_style_type` option is deprecated (it never chose a family; it
  # repointed the text channel into the fill palette). `type` stays LOAD-BEARING below -- it routes
  # `custom_palette` to the text vs background slot -- but it no longer writes the option.
  if (!is.null(theme)) set_color_palette(theme = theme[1])
  if (length(custom_palette) >= 10L) {
    # old order pos1..pos5, neg1..neg5[, ratio] -> new 4 over + 4 under (drop the faintest pos1/neg1)
    cp   <- unname(custom_palette)
    over <- cp[2:5]; under <- cp[7:10]
    if (identical(type[1], "bg")) {
      set_color_palette(background_colors = over, background_colors_neg = under)
    } else {
      set_color_palette(text_colors = over, text_colors_neg = under)
    }
  }
  invisible()
}
# === end COMPAT (Phase 13a) ======================================================================

#' @describeIn tab_many get the color palette as \pkg{crayon} functions or html codes: an 8-element
#' vector (4 over-represented intensities then 4 under-represented), indexed by the engine slot.
#' @param mode By default, \code{get_color_style} returns a list of \pkg{crayon} coloring
#' functions. Set to \code{"color_code"} to return html color codes.
#' @param type \code{"text"} (font colour), \code{"bg"} (background fill), or \code{"bg_legend"}
#' (\code{mode = "color_code"} only): the darker FONT stand-in for the background palette, for the
#' media that cannot fill (an Excel rich-text run, a \pkg{ggpubr} text label) -- see the colour legend.
#' @param theme \code{"light"} or \code{"dark"}; defaults to the current setting. (A palette is always
#' one or the other: the export theme \code{"auto"} resolves to \code{"light"} here.)
#' @param ... Absorbs deprecated arguments (e.g. \code{html_24_bit}); ignored.
#' @return A list of 8 crayon color functions, or a vector of 8 color html codes.
#' @export
get_color_style <- function(mode = c("crayon", "color_code"), type = NULL, theme = NULL, ...) {
  # Phase 14l: `type` (the palette-FAMILY selector) stays; the OPTION tabxplor.color_style_type is
  # deprecated -- it never chose a family, it globally repointed the TEXT channel into the FILL
  # palette, i.e. fill-coloured font (the CHANNEL is chosen by `color = c(text, background)`). Warn
  # once per session (deprecate_warn dedups and fires from these nested internal frames; deprecate_soft
  # keys on the USER frame, so it would be silent from pillar_shaft). Only fires for someone who set
  # the option to a non-default value -- for everyone else it is NULL now the seed write is gone.
  opt_type <- getOption("tabxplor.color_style_type")
  if (!is.null(opt_type) && !identical(opt_type, "text")) {
    lifecycle::deprecate_warn(
      "1.4.0", I('The option "tabxplor.color_style_type"'),
      details = 'The colour CHANNEL is chosen by `color = c(text, background)` (see `?tab`).')
  }
  theme <- if (is.null(theme)) getOption("tabxplor.color_style_theme") else theme
  if (is.null(type)  || is.na(type[1]))  type  <- "text"
  if (is.null(theme) || is.na(theme[1])) theme <- "light"
  # Phase 13d: a palette is always light/dark. "auto" is an EXPORT render intent (`theme = "auto"`
  # means "follow the reader"), and it reaches here whenever a caller forwards its own theme -- e.g.
  # the exported fmt_get_color_code(theme = "auto"). Without this it would build the key "text_auto",
  # find NULL, and error on a length-0 vector further down. Resolve at the one chokepoint.
  theme <- tx_palette_theme(theme)
  fam <- switch(type[1], "bg" = "bg", "bg_legend" = "bg_legend", "text")
  key <- paste0(fam, "_", theme[1])

  e <- tabxplor_palette_env
  if (is.null(e$hex)) build_palettes()
  if (identical(mode[1], "crayon")) {
    # bg_legend exists only to substitute for a fill in media that have no fill; a console HAS one.
    if (identical(fam, "bg_legend")) {
      cli::cli_abort('{.arg type} {.val bg_legend} has no crayon styles: use {.code mode = "color_code"},
                      or {.arg type} {.val bg} for a real background.')
    }
    e$crayon[[key]]
  } else e$hex[[key]]
}




# cat_style <- function(styles = tabxplor_color_style) cat("\n",
#                                            styles$pos1("42%" ), styles$neg1("42%\n" ),
#                                            styles$pos2("42%" ), styles$neg2("42%\n" ),
#                                            styles$pos3("42%" ), styles$neg3("42%\n" ),
#                                            styles$pos4("42%" ), styles$neg4("42%\n" ),
#                                            styles$pos5("42%" ), styles$neg5("42%\n" ) )
#
# set_color_style(n = 5) %>%
#   purrr::map(~ crayon::make_style(., colors = 256)) %>% cat_style()
#
# set_color_style(console_theme = "light", n = 5) %>%
#   purrr::map(~ crayon::make_style(., colors = 256)) %>% cat_style()
#
# set_color_style(type = "bg", n = 5) %>%
#   purrr::map(~ crayon::make_style(., bg = TRUE, colors = 256)) %>% cat_style()
#
# set_color_style(type = "bg", console_theme = "light", n = 5)  %>%
#   purrr::map(~ crayon::make_style(., bg = TRUE, colors = 256)) %>% cat_style()

#crayon::show_ansi_colors()



#Color breaks for printing fmt in tabs ------------------------------------------------

# PURPOSE: the canonical color-break representation (Phase 13a) and its accessors.
# The stored option "tabxplor.color_breaks" is a named list of the five measure scales
#   pct_diff, pct_ratio, mean_diff, mean_ratio, contrib
# each a list(center, strict, std, over = list(breaks, slots), under = list(breaks, slots)):
#   - over/under : the two sides, each a list(breaks = <ascending POSITIVE magnitudes>,
#                  slots = <intensities 1:4 into the 4-colour palette>). An empty side is off.
#                  Both are magnitudes: the engine folds every cell to a magnitude >= the neutral
#                  (abs for additive, 1/x for multiplicative) and picks the side by direction.
#   - center : 0 for additive scales (pct_diff, mean_diff, contrib), 1 for multiplicative
#              (pct_ratio, mean_ratio) -- the neutral value each break is measured from.
#   - strict : TRUE reproduces a strict `>`/`<` comparison, FALSE an inclusive `>=`/`<=`
#              (contrib). On-break cells fall in the lower band when strict.
#   - std    : mean_diff only -- TRUE colors the sd-standardized difference (Glass's delta),
#              FALSE colors the raw difference in data units.
# The findInterval engine (fmt_color_plan/fmt_color_slots) reads this shape directly.
# See: dev/new_colors_UI.md ; CLAUDE.md > 1.4.0 roadmap > Phase 13a.

# Default intensity-slot selection for k thresholds on one side, mapped into the 4 palette
# intensities. Fewer than 4 breaks drop the 2nd intensity first, then the 4th, then the 1st,
# so a single break lands on the medium-strong colour (intensity 3).
#' @keywords internal
intensity_slots <- function(k) {
  switch(as.character(k),
         "0" = integer(0),
         "1" = 3L,
         "2" = c(1L, 3L),
         "3" = c(1L, 3L, 4L),
         "4" = c(1L, 2L, 3L, 4L),
         cli::cli_abort(c("At most 4 color breaks per side (there are 4 palette intensities).",
                          "x" = "{k} were given.")))
}

# Parse one side (over- or under-represented) given as POSITIVE magnitudes, possibly with NA
# marking a skipped intensity slot. Returns list(breaks = <ascending magnitudes>, slots = <1:4>).
# Without NA the slots come from intensity_slots(); with NA the non-NA positions ARE the intensities.
#' @keywords internal
parse_color_side <- function(v, name) {
  if (length(v) == 0) return(list(breaks = numeric(0), slots = integer(0)))
  if (length(v) > 4) {
    cli::cli_abort(c("Color breaks {.arg {name}} accept at most 4 values per side.",
                     "x" = "{length(v)} were given."))
  }
  if (anyNA(v)) {
    slots  <- which(!is.na(v))
    breaks <- as.double(v[slots])
  } else {
    slots  <- intensity_slots(length(v))
    breaks <- as.double(v)
  }
  if (length(breaks) > 1 && is.unsorted(breaks, strictly = TRUE)) {
    cli::cli_abort("Color break magnitudes {.arg {name}} must be strictly increasing.")
  }
  if (any(breaks <= 0)) {
    cli::cli_abort("Color break magnitudes {.arg {name}} must be > 0.")
  }
  list(breaks = breaks, slots = as.integer(slots))
}

# Validate one user scale and wrap it into the canonical
#   list(center, strict, std, over = list(breaks, slots), under = list(breaks, slots)).
# Input forms (see dev/new_colors_UI.md / Phase 13a):
#   - a plain numeric vector of SIGNED / RECIPROCAL literals: negatives (additive) or values < 1
#     (multiplicative) are the under-represented side; a one-sided vector auto-mirrors, a two-sided
#     one is used as-is. `NA` entries skip an intensity slot (one-sided vectors only).
#   - list(over =, under =): explicit per-side magnitudes, NO mirror; omit a side to switch it off
#     (e.g. list(over = 2) = the "only x2" rule).
#   - NULL / empty: drop the measure for its column type -- except mean_diff = NULL, which restores
#     the standardized (Glass's delta) default.
#' @keywords internal
mk_color_scale <- function(name, values) {
  valid <- c("pct_diff", "pct_ratio", "mean_diff", "mean_ratio", "contrib")
  if (!name %in% valid) {
    cli::cli_abort(c("Unknown color-break scale {.val {name}}.",
                     "i" = "Valid scales: {.val {valid}}."))
  }
  center <- if (name %in% c("pct_ratio", "mean_ratio")) 1 else 0
  strict <- name != "contrib"

  # NULL / empty: drop the measure, except mean_diff -> standardized default.
  if (is.null(values) || (is.numeric(values) && length(values) == 0L)) {
    if (name == "mean_diff") {
      side <- parse_color_side(c(0.2, 0.5, 0.8), name)
      return(list(center = 0, strict = TRUE, std = TRUE, over = side, under = side))
    }
    empty <- list(breaks = numeric(0), slots = integer(0))
    return(list(center = center, strict = strict, std = FALSE, over = empty, under = empty))
  }

  # over/under list form: explicit per-side magnitudes, no mirror.
  if (is.list(values)) {
    nms <- names(values)
    if (is.null(nms) || !all(nzchar(nms)) || !all(nms %in% c("over", "under"))) {
      cli::cli_abort(c("A color scale given as a list must use {.field over} / {.field under}.",
                       "i" = 'e.g. {.code list(over = c(1.5, 2, 4))} for the over-represented side only.'))
    }
    over  <- parse_color_side(if (is.null(values$over))  numeric(0) else values$over,  name)
    under <- parse_color_side(if (is.null(values$under)) numeric(0) else values$under, name)
    return(list(center = center, strict = strict, std = FALSE, over = over, under = under))
  }

  if (!is.numeric(values)) {
    cli::cli_abort(c("Color breaks {.arg {name}} must be numeric or a {.code list(over =, under =)}.",
                     "x" = "Got {.cls {class(values)}}."))
  }

  nonna <- values[!is.na(values)]
  if (center == 1) {
    if (any(nonna == 1)) cli::cli_abort("Ratio breaks {.arg {name}} cannot equal 1 (the neutral value).")
    if (any(nonna <= 0)) cli::cli_abort("Ratio breaks {.arg {name}} must be > 0.")
    over_sel  <- values > 1
    under_sel <- values < 1
  } else {
    if (any(nonna == 0)) cli::cli_abort("Breaks {.arg {name}} cannot equal 0 (the neutral value).")
    over_sel  <- values > 0
    under_sel <- values < 0
  }
  has_over  <- any(over_sel,  na.rm = TRUE)
  has_under <- any(under_sel, na.rm = TRUE)

  # magnitude of one side (NA preserved as a slot-skip marker)
  to_mag <- function(x, side) {
    if (center == 1) { if (side == "over") x else 1 / x } else { if (side == "over") x else -x }
  }

  if (has_over && has_under) {                      # two-sided: use as-is, no mirror, no NA
    if (anyNA(values)) {
      cli::cli_abort(c("Color breaks {.arg {name}}: NA slot-skips are only allowed on a one-sided vector.",
                       "i" = "Use the {.code list(over =, under =)} form for asymmetric scales with skips."))
    }
    over  <- parse_color_side(sort(to_mag(values[over_sel],  "over")),  name)
    under <- parse_color_side(sort(to_mag(values[under_sel], "under")), name)
  } else {                                          # one-sided: mirror to both, keep NA positions
    side <- if (has_under && !has_over) "under" else "over"
    parsed <- parse_color_side(to_mag(values, side), name)
    over <- parsed; under <- parsed
  }
  list(center = center, strict = strict, std = FALSE, over = over, under = under)
}

#' @keywords internal
default_color_scales <- function() {
  list(
    pct_diff   = mk_color_scale("pct_diff",   c(0.05, 0.1, 0.2, 0.3)),
    pct_ratio  = mk_color_scale("pct_ratio",  list(over = 2)),
    mean_diff  = mk_color_scale("mean_diff",  NULL),
    mean_ratio = mk_color_scale("mean_ratio", list(over = c(1.15, 1.5, 2, 4), under = c(1.5, 2, 4))),
    contrib    = mk_color_scale("contrib",    c(1, 2, 5, 10))
  )
}


#' Set the breaks used to print colors
#' @describeIn tab_many set the breaks used to print colors.
#' @description Color breaks are a named list of the five measure scales \code{pct_diff},
#' \code{pct_ratio}, \code{mean_diff}, \code{mean_ratio} and \code{contrib}. Each is a vector
#' of positive-only thresholds (the under-represented side is mirrored automatically), 1 to 5
#' values, one per color step: \code{pct_diff} colors percentage-point differences,
#' \code{pct_ratio} the relative risk (the "x2 rule"), \code{mean_diff} the standardized mean
#' difference (Glass's delta) by default (supply data-unit values for absolute coloring),
#' \code{mean_ratio} the mean ratio, \code{contrib} the chi2 contribution. An empty/\code{NULL}
#' scale drops that measure for its column type.
#' @param breaks A named list of scales to set, e.g.
#' \code{list(pct_diff = c(0.05, 0.1, 0.2, 0.3), pct_ratio = list(over = 2))}. Unset scales keep
#' their current value.
#' @param ... Scales passed individually and named, e.g.
#' \code{set_color_breaks(pct_diff = c(0.05, 0.1, 0.2), mean_ratio = c(1.15, 1.5, 2, 4))}. Each
#' value is either a plain vector of signed / reciprocal literals (negatives, or ratios < 1, are
#' the under-represented side; a one-sided vector auto-mirrors; \code{NA} skips an intensity slot)
#' or a \code{list(over =, under =)} of magnitudes (no mirror; omit a side to switch it off, e.g.
#' \code{list(over = 2)} for the "only x2" rule). The old \code{pct_breaks} / \code{mean_breaks} /
#' \code{contrib_breaks} arguments are soft-deprecated but still work (mapped onto the new scales).
#'
#' @return Sets the global option "tabxplor.color_breaks" (a named list of scales) and returns
#' it invisibly.
#' @export
#' @examples set_color_breaks(
#'   pct_diff   = c(0.05, 0.15, 0.3),
#'   pct_ratio  = list(over = 2),
#'   mean_ratio = c(1.15, 2, 4),
#'   contrib    = c(1, 2, 5)
#' )
set_color_breaks <- function(breaks = NULL, ...) {
  cur <- getOption("tabxplor.color_breaks")
  if (is.null(cur) || is.null(cur$pct_diff)) cur <- default_color_scales()

  dots <- list(...)
  # COMPAT (Phase 13a): the old flat args pct_breaks / mean_breaks / contrib_breaks, mapped onto the
  # new scales (pct_breaks splits <=1 -> pct_diff, >1 -> pct_ratio) with a soft-deprecation.
  old_args <- intersect(names(dots), c("pct_breaks", "mean_breaks", "contrib_breaks"))
  if (length(old_args)) {
    lifecycle::deprecate_soft("1.4.0", I(paste0("set_color_breaks(", old_args[1], ")")),
                              with = I("set_color_breaks(pct_diff = , pct_ratio = , mean_ratio = , contrib = )"))
    if (!is.null(dots$pct_breaks)) {
      pb <- dots$pct_breaks
      cur$pct_diff  <- mk_color_scale("pct_diff",  sort(pb[pb <= 1]))
      rr <- pb[pb > 1]
      cur$pct_ratio <- mk_color_scale("pct_ratio", if (length(rr)) list(over = sort(rr)) else numeric())
    }
    if (!is.null(dots$mean_breaks))    cur$mean_ratio <- mk_color_scale("mean_ratio", sort(dots$mean_breaks))
    if (!is.null(dots$contrib_breaks)) cur$contrib    <- mk_color_scale("contrib",    sort(dots$contrib_breaks))
    dots <- dots[setdiff(names(dots), old_args)]
  }

  combined <- c(if (is.null(breaks)) list() else breaks, dots)
  if (length(combined) == 0L) {
    options("tabxplor.color_breaks" = cur)
    return(invisible(cur))
  }
  nms <- names(combined)
  if (is.null(nms) || any(!nzchar(nms))) {
    cli::cli_abort(c("Color scales must be named.",
                     "i" = "e.g. {.code set_color_breaks(pct_ratio = list(over = 2))} or",
                     "i" = "{.code set_color_breaks(list(pct_diff = c(0.05, 0.1, 0.2)))}."))
  }
  combined <- combined[!duplicated(nms, fromLast = TRUE)]   # a later value overrides an earlier one
  for (nm in names(combined)) cur[[nm]] <- mk_color_scale(nm, combined[[nm]])

  options("tabxplor.color_breaks" = cur)
  invisible(cur)
}


# --- Per-table color_breaks override (Phase 13a) --------------------------------------------------
# `tab(color_breaks = list(...))` validates the user scales into a PARTIAL canonical list and stores
# it as the table attribute "color_breaks" (set at the very END of tab(), so no dplyr verb strips it
# before the user gets it). At render time, push_color_breaks() merges that partial list OVER the live
# global option for the duration of the render, then pop restores. Robust by design: a missing / NULL /
# malformed attribute simply falls back to the global breaks. A heavy dplyr chain between build and
# render drops the attribute -> global fallback (documented; the global set_color_breaks() still works).

#' @keywords internal
resolve_color_breaks_arg <- function(color_breaks) {
  if (is.null(color_breaks)) return(NULL)
  if (!is.list(color_breaks) || is.null(names(color_breaks)) || any(!nzchar(names(color_breaks)))) {
    cli::cli_abort(c("{.arg color_breaks} must be a named list of color scales.",
                     "i" = "e.g. {.code list(pct_ratio = list(over = 2), pct_diff = c(0.05, 0.1, 0.2))}."))
  }
  purrr::imap(color_breaks, ~ mk_color_scale(.y, .x))
}

#' @keywords internal
set_color_breaks_attr <- function(x, cb) {
  if (is.null(cb)) return(x)
  if (is.list(x) && !is.data.frame(x)) return(purrr::map(x, set_color_breaks_attr, cb))
  attr(x, "color_breaks") <- cb
  x
}

# Install a table's color_breaks attribute as the transient global option; returns a state to restore
# with pop_color_breaks() (NULL when there is no override -> nothing to restore). Each render entry
# point calls: st <- push_color_breaks(tabs); on.exit(pop_color_breaks(st), add = TRUE).
#' @keywords internal
push_color_breaks <- function(tabs) {
  tb <- if (is.list(tabs) && !is.data.frame(tabs)) {
    if (length(tabs) >= 1L) attr(tabs[[1]], "color_breaks", exact = TRUE) else NULL
  } else attr(tabs, "color_breaks", exact = TRUE)
  if (is.null(tb) || !is.list(tb) || length(tb) == 0L) return(NULL)
  old  <- getOption("tabxplor.color_breaks")
  base <- if (is.null(old) || is.null(old$pct_diff)) default_color_scales() else old
  for (nm in names(tb)) base[[nm]] <- tb[[nm]]
  options("tabxplor.color_breaks" = base)
  list(old = old)
}

#' @keywords internal
pop_color_breaks <- function(state) {
  if (!is.null(state)) options("tabxplor.color_breaks" = state$old)
  invisible()
}



#calculate pct breaks based on the number of levels ? ----

# pct_breaks      <- c(0.05, 0.1, 0.2, 0.3)
# mean_breaks     <- c(1.15, 1.5, 2, 4)
# contrib_breaks  <- c(1, 2, 5, 10)
#
# pct_ci_breaks   <- pct_breaks - pct_breaks[1]
# mean_ci_breaks  <- mean_breaks / mean_breaks[1]
#
# pct_brksup      <- c(pct_breaks    [2:length(pct_breaks)    ], Inf)
# mean_brksup     <- c(mean_breaks   [2:length(mean_breaks)   ], Inf)
# contrib_brksup  <- c(contrib_breaks[2:length(contrib_breaks)], Inf)
# pct_ci_brksup   <- c(pct_ci_breaks [2:length(pct_ci_breaks) ], Inf)
# mean_ci_brksup  <- c(mean_ci_breaks[2:length(mean_ci_breaks)], Inf)
#
# pct_breaks         <- pct_breaks     %>% c(., -.)
# mean_breaks        <- mean_breaks    %>% c(., 1/.)
# contrib_breaks     <- contrib_breaks %>% c(., -.)
# pct_ci_breaks      <- pct_ci_breaks  %>% c(., -.)
# mean_ci_breaks     <- mean_ci_breaks %>% c(., -.) #then - again
#
# pct_brksup      <- pct_brksup     %>% c(., -.)
# mean_brksup     <- mean_brksup    %>% c(., 1/.)
# contrib_brksup  <- contrib_brksup %>% c(., -.)
# pct_ci_brksup   <- pct_ci_brksup  %>% c(., -.)
# mean_ci_brksup  <- mean_ci_brksup %>% c(., -.) #then - again


#' Get the breaks currently used to print colors
#' @describeIn tab_many get the color breaks currently in use, in the canonical Phase-5 shape.
#' @param brk When missing, return the full named list of break scales (\code{pct_diff},
#' \code{pct_ratio}, \code{mean_diff}, \code{mean_ratio}, \code{contrib}) -- the same shape
#' \code{\link{set_color_breaks}} accepts, so it round-trips. Specify one scale name to return
#' only its breaks. The old aliases \code{"pct"} (-> \code{pct_diff}) and \code{"mean"} (->
#' \code{mean_ratio}) are still accepted.
#' @param type Default \code{"positive"} returns a readable form: a plain vector of magnitudes
#' when the scale is symmetric, or a \code{list(over =, under =)} of magnitudes otherwise. Set to
#' \code{"all"} to get the signed / reciprocal thresholds the engine compares against
#' (\code{c(-x, x)} for additive scales, \code{c(1/x, x)} for multiplicative ones).
#'
#' @return The color breaks as a double vector or a \code{list(over =, under =)}, or a named list
#' of these.
#' @export
get_color_breaks <- function(brk, type = c("positive", "all")) {
  scales <- getOption("tabxplor.color_breaks")
  if (is.null(scales) || is.null(scales$pct_diff)) scales <- default_color_scales()

  lit_under <- function(sc) if (isTRUE(sc$center == 1)) 1 / sc$under$breaks else -sc$under$breaks

  as_form <- function(sc) {
    ob <- sc$over$breaks; ub <- sc$under$breaks
    if (identical(type[1], "all")) return(c(rev(lit_under(sc)), ob))
    if (length(ob) == 0L && length(ub) == 0L) return(numeric(0))
    if (length(ub) == 0L) return(list(over = ob))
    if (length(ob) == 0L) return(list(under = ub))
    if (identical(ob, ub) && identical(sc$over$slots, sc$under$slots)) return(ob)
    list(over = ob, under = ub)
  }

  if (missing(brk)) return(purrr::map(scales, as_form))

  brk <- switch(brk, "pct" = "pct_diff", "mean" = "mean_ratio", brk)
  if (!brk %in% names(scales)) {
    cli::cli_abort(c("Unknown color break {.val {brk}}.",
                     "i" = "Valid scales: {.val {names(scales)}} (aliases {.val pct}, {.val mean})."))
  }
  as_form(scales[[brk]])
}

# get_color_breaks()
#
# set_color_breaks(pct_breaks = c(0.05, 0.10, 0.15, 0.25, 0.35))

# get_full_color_breaks()

# pct_breaks     = c(0.05, 0.10, 0.15, 0.25, 0.35)
# mean_breaks    = c(1.15, 1.25, 1.5 , 2   , 4   )
# contrib_breaks = c(0.5 , 1   , 2   , 5   , 10  )








# Tests -----
# new_tab() %>% get_chi2()
# new_tab() %>% get_total_table()
# new_tab() %>% get_subtext()

# vec_ptype2(new_tab(), new_tab()) %>% attributes()
#
# vec_rbind(red, red)
# vec_rbind(green, green)
# vec_rbind(green, red)
#
# vec_rbind(red, tibble::tibble(x = 10:12))
# vec_rbind(red, data.frame(x = 10:12))



# vctrs documentation --------------------------------------------------------------------

# howto-faq-coercion-data-frame
# FAQ - How to implement ptype2 and cast methods? (Data frames)
# Description
# This guide provides a practical recipe for implementing vec_ptype2() and vec_cast() methods
# for coercions of data frame subclasses. Related topics:
#  - For an overview of the coercion mechanism in vctrs, see ?theory-faq-coercion.
#  - For an example of implementing coercion methods for simple vectors, see ?howto-faq-coercion.
# Coercion of data frames occurs when different data frame classes are combined in some way. The
# two main methods of combination are currently row-binding with vec_rbind() and col-binding
# with vec_cbind() (which are in turn used by a number of dplyr and tidyr functions). These functions
# take multiple data frame inputs and automatically coerce them to their common type.
# vctrs is generally strict about the kind of automatic coercions that are performed when combining
# inputs. In the case of data frames we have decided to be a bit less strict for convenience. Instead of
# throwing an incompatible type error, we fall back to a base data frame or a tibble if we don't know
# how to combine two data frame subclasses. It is still a good idea to specify the proper coercion
# behaviour for your data frame subclasses as soon as possible.
# We will see two examples in this guide. The first example is about a data frame subclass that has
# no particular attributes to manage. In the second example, we implement coercion methods for a
# tibble subclass that includes potentially incompatible attributes.

# Roxygen workflow:
#   To implement methods for generics, first import the generics in your namespace and redocument:
#   #' @importFrom vctrs vec_ptype2 vec_cast
#   NULL
# Note that for each batches of methods that you add to your package, you need to export the
# methods and redocument immediately, even during development. Otherwise they won't be in
# scope when you run unit tests e.g. with testthat.
# Implementing double dispatch methods is very similar to implementing regular S3 methods. In
# these examples we are using roxygen2 tags to register the methods, but you can also register the
# methods manually in your NAMESPACE file or lazily with s3_register().

# Parent methods:
#   Most of the common type determination should be performed by the parent class. In vctrs, double
# dispatch is implemented in such a way that you need to call the methods for the parent class manually.
# For vec_ptype2() this means you need to call df_ptype2() (for data frame subclasses) or
# tib_ptype2() (for tibble subclasses). Similarly, df_cast() and tib_cast() are the workhorses
# for vec_cast() methods of subtypes of data.frame and tbl_df. These functions take the union
# of the columns in x and y, and ensure shared columns have the same type.
# These functions are much less strict than vec_ptype2() and vec_cast() as they accept any
# subclass of data frame as input. They always return a data.frame or a tbl_df. You will probably
# want to write similar functions for your subclass to avoid repetition in your code. You may want
# to export them as well if you are expecting other people to derive from your class.

# A data.tabxplor_tab le example:
# [...]

# #A tibble example:
# #  In this example we implement coercion methods for a tibble subclass that carries a colour as a
# #scalar metadata:
#
#   # User constructor
#   my_tibble <- function(colour = NULL, ...) {
#     new_my_tibble(tibble::tibble(...), colour = colour)
#   }
# # Developer constructor
# new_my_tibble <- function(x, colour = NULL) {
#   stopifnot(is.data.frame(x))
#   tibble::new_tibble(
#     x,
#     colour = colour,
#     class = "my_tibble",
#     nrow = nrow(x)
#   )
# }
# df_colour <- function(x) {
#   if (inherits(x, "my_tibble")) {
#     attr(x, "colour")
#   } else {
#     NULL
#   }
# }
# #'@export
# print.my_tibble <- function(x, ...) {
#   cat(sprintf("<%s: %s>\n", class(x)[[1]], df_colour(x)))
#   cli::cat_line(format(x)[-1])
# }
# #This subclass is very simple. All it does is modify the header.
# red <- my_tibble("red", x = 1, y = 1:2)
# red
# #> <my_tibble: red>
# #> x y
# #> <dbl> <int>
# #> 1 1 1
# #> 2 1 2
# red[2]
# #> <my_tibble: red>
# #> y
# #> <int>
# #> 1 1
# #> 2 2
# green <- my_tibble("green", z = TRUE)
# green
# #> <my_tibble: green>
# #> z
#
# #> <lgl>
# #> 1 TRUE
# #Combinations do not work properly out of the box, instead vctrs falls back to a bare tibble:
#   vec_rbind(red, tibble::tibble(x = 10:12))
# #> # A tibble: 5 x 2
# #> x y
# #> <dbl> <int>
# #> 1 1 1
# #> 2 1 2
# #> 3 10 NA
# #> 4 11 NA
# #> 5 12 NA
# # Instead of falling back to a data frame, we would like to return a <my_tibble> when combined
# # with a data frame or a tibble. Because this subclass has more metadata than normal data frames
# # (it has a colour), it is a supertype of tibble and data frame, i.e. it is the richer type. This is similar
# # to how a grouped tibble is a more general type than a tibble or a data frame. Conceptually, the
# # latter are pinned to a single constant group.
