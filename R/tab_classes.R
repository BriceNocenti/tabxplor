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
#' in tabxplor 2.0.0.
#' @param chi2 `r lifecycle::badge("deprecated")` Soft-deprecated alias of \code{test}.
#' @param meta The table's metadata, as a single named list gathering (all optional, \code{NULL}
#' when unset):
#' \itemize{
#'   \item \code{render_extras} -- display-only intent for the \code{add_n} / \code{add_pct} extras,
#'   \code{list(add_n =, add_pct =)}. Since tabxplor 2.0.0 those rows/columns are materialised at
#'   print/export time from this attribute rather than baked into the table.
#'   \item \code{ci_settings} -- display-only metadata for the colour legend,
#'   \code{list(conf_level =, method_cell =, method_diff =, ...)}: which confidence level and
#'   confidence-interval methods were actually used. Absent makes the legend fall back to defaults.
#'   \item \code{vars} -- the table's variable roles,
#'   \code{list(row_vars =, col_vars =, tab_vars =, compacted =, wt =, caption =)}, recorded at build
#'   rather than guessed back afterwards (see \code{\link{set_caption}} for \code{caption}).
#'   \item \code{empirical_tips} -- multinomial crude-companion tooltip data (a \code{tibble} keyed by
#'   column, predictor and level), set by \code{tab_reg(empirical = TRUE)}.
#'   \item \code{reg_meta} -- a regression table's model record (family, effect, dependent, reference
#'   level, predictors, ...), set by \code{\link{tab_reg}}; drives the reg title/caption, the "Model:"
#'   legend line and the colour-legend wording.
#'   \item \code{assumptions} -- the observed curve of each continuous predictor (weighted quantile
#'   bins of the outcome on the family's link scale), set by \code{\link{tab_reg}}: the data behind
#'   the sparkline in a continuous predictor's row label and behind
#'   \code{\link{reg_check_plots}}'s linearity panel.
#'   \item \code{color_breaks} -- a per-table override of the colour break scales (see
#'   \code{\link{set_color_breaks}}), merged over the global option at render time.
#' }
#' \code{meta} sub-fields left \code{NULL} are dropped, so a table given nothing carries no attribute.
#' @param ... Needed to implement subclasses.
#' @param class Needed to implement subclasses.
#'
#' @return A \code{tibble} of class \code{tabxplor_tab}.
#' @export
#  @examples
new_tab <-
  function(tabs = tibble::tibble(), subtext = "",
           test = new_test_tibble(), chi2 = NULL,
           meta = NULL,
           ..., class = character()) {
    stopifnot(is.data.frame(tabs))
    #vec_assert(subtext    , character())

    # Soft-deprecated `chi2` arg (renamed `test` in 2.0.0): if supplied, it feeds `test`.
    if (!is.null(chi2)) test <- chi2

    out <- tibble::new_tibble(tabs, subtext = subtext, test = test, ...,
                              nrow = nrow(tabs), class = c(class, "tabxplor_tab"))
    # Phase 17b: every 2.0.0-new table attribute (render_extras / ci_settings / vars / empirical_tips /
    # reg_meta / color_breaks) is now ONE `meta` named list -- one formal, one attribute, one tab_attrs()
    # line, one bind reconcile (was six of each). Sub-fields left NULL are dropped, so a table given
    # nothing carries no `meta` attribute at all (raw tab_plain / hand-built / older objects stay clean).
    # The former per-field prose lives on the `@param meta` roxygen; the accessors below (get_vars_attr,
    # get_ci_settings, ...) keep their names and read straight into this list.
    if (!is.null(meta)) meta <- meta[!vapply(meta, is.null, logical(1))]
    if (length(meta)) attr(out, "meta") <- meta
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
           meta = NULL,
           ..., class = character()) {
    if (missing(groups)) groups <- attr(tabs, "groups")
    class <- c(class, c("tabxplor_grouped_tab", "grouped_df"))

    # Soft-deprecated `chi2` arg (renamed `test` in 2.0.0): if supplied, it feeds `test`.
    if (!is.null(chi2)) test <- chi2

    new_tab(tabs, groups = groups,
            subtext = subtext, test = test, meta = meta,
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
# (subtable x col_var x test-type). Renamed from the pre-2.0.0 `chi2` attribute (§16/§17: the old
# `chi2` attribute is an accepted break -- 2.0.0 tabs are re-created from code, never deserialized).
# get_chi2() is kept as a working back-compat ALIAS so pre-2.0.0 user code that CALLS it still runs.
get_test <- function(x) attr(x, "test", exact = TRUE)
get_chi2 <- function(x) get_test(x)

# set_test() -- write the whole-table `test` tibble attribute on a built table. Used by the
# jmvtab tier-2 cache (Phase 7e) to inject a cached chi2/ANOVA result instead of recomputing it.
set_test <- function(x, test) {
  attr(x, "test") <- test
  x
}

# Phase 17b: the `meta` table attribute -- ONE named list gathering every 2.0.0-new table attribute
# (render_extras / ci_settings / vars / empirical_tips / reg_meta / color_breaks). get_meta() returns
# NULL when absent, so every get_meta(x)[["field"]] yields NULL exactly like the old attr_getter did.
get_meta <- function(x) attr(x, "meta", exact = TRUE)

# set_meta_field() -- write ONE meta sub-field, preserving the others. Assigning NULL REMOVES the field
# (base-R list semantics), and an emptied meta drops the whole attribute -- this is what keeps the
# "absent when unset" property (a table given nothing carries no `meta` attribute) AND makes
# set_render_extras(x, NULL) (tab_materialize_extras) clear render_extras WITHOUT touching ci_settings /
# vars. So every set_* below is one call, and byte-identity at the attribute level is preserved.
set_meta_field <- function(x, field, value) {
  m <- get_meta(x)
  if (is.null(m)) m <- list()
  m[[field]] <- value
  m <- m[!vapply(m, is.null, logical(1))]
  attr(x, "meta") <- if (length(m)) m else NULL
  x
}

# Phase 10i-B: `render_extras` -- the DISPLAY-only intent for the add_n / add_pct extras, a small
# list `list(add_n = <lgl>, add_pct = <lgl>)`. The built tab() no longer carries the add_n `n` column /
# add_pct `col_pct` column-or-rows; it stores this intent (born in tab_assemble_tables) and
# tab_materialize_extras() re-creates the rows/cols at display. NULL -> no extras.
get_render_extras <- function(x) get_meta(x)[["render_extras"]]
set_render_extras <- function(x, render_extras) set_meta_field(x, "render_extras", render_extras)

# Phase 13b: `ci_settings` -- display-only metadata for the colour legend, a small list
# `list(conf_level = <num>, method_cell = <chr>, method_diff = <chr>, ...)` recording which CI method /
# confidence level tab()/tab_ci() actually used, so tab_color_legend() can name it accurately (e.g.
# "Newcombe score interval, 95% confidence"). get_ci_settings() falls back to the package defaults when
# absent (heavy dplyr chains / raw tab_plain / older objects).
get_ci_settings <- function(x) get_meta(x)[["ci_settings"]]
set_ci_settings <- function(x, ci_settings) set_meta_field(x, "ci_settings", ci_settings)

# Phase 14d: `vars` -- the table's OWN record of its variable roles,
# `list(row_vars = <chr>, col_vars = <chr>, tab_vars = <chr>, compacted = <lgl>, wt =, caption =)`,
# written where the truth is known (tab_assemble_tables / tab_compact / tab_counts / tab_reg / tab_plain)
# and read by tab_get_vars() / tab_render_vars().
# WHY: the roles CANNOT be recovered from a built table. tab_compact() renames column 1 to the literal
# "levels" and stores the row-variable names only as factor LEVELS of a synthetic column named
# "row_var" -- so the "last factor column is the row_var, the others are tab_vars" heuristic reports
# `row_var = "levels", tab_vars = "row_var"` on a merged table that has no tab_vars at all. That is why
# tab_transpose() aborted with a message about tab_vars that were never there, and why a tab_xl title
# read "levels by multi (tabbed by row_var)". Sniffing for a column NAMED "row_var" would be the
# ad-hoc layer this replaces: record the roles instead of inferring them.
# `compacted` = several row_vars were merged into one table (so `row_vars` has length > 1 and the
# row-variable name lives in the `row_var` column's values, not in a column name).
# The heuristic stays as the fallback for hand-built tables (a raw tibble of fmt columns, an object from
# an older version), so nothing user-facing breaks.
get_vars_attr <- function(x) get_meta(x)[["vars"]]
set_vars_attr <- function(x, vars) set_meta_field(x, "vars", vars)

# Phase 17c: the DISPLAY-time positional row-role vector (values "data"/"total"/"n"/"row_pct"/"pvalue"/
# "gof"/"blank"), stored in meta$vars$row_roles. Seeded by tab_materialize_extras(), extended by the
# row-adding materialisers (tab_add_n_pct via tab_append_pctcol_rows, and tab_append_footer), sliced by
# tab_collapse_total_rows -- so every synthetic-row consumer reads the stored kind instead of matching
# an English row label. It is never persisted in the user-facing built table (materialise is display-only).
# The RESOLVER with the hand-built-table fallback is tab_row_roles() (R/tab.R); these are the raw store.
set_row_roles <- function(x, roles) {
  v <- get_vars_attr(x); if (is.null(v)) v <- new_vars_attr()
  v$row_roles <- roles                 # NULL clears it (base-R list semantics)
  set_vars_attr(x, v)
}
get_row_roles_raw <- function(x) get_vars_attr(x)[["row_roles"]]

# Last Phase z16-i: `inference` -- THE stored fact "how were this table's intervals and tests
# computed", `list(basis = , degf = , note = )`. `wt` says how the ESTIMATE is computed; this says how
# the INFERENCE is. Before, the basis was resolved at build (svy_inference_basis) and thrown away, so
# the footer could not name it, a degraded design could not be recorded, and tab_weight_line() had to
# re-derive "this is design-based" by string-sniffing the internal `.svy_weights` column name.
#   basis  "n" | "weights" | "design" | "design_partial"   (see R/survey-design.R)
#   degf   the design's degrees of freedom (NA otherwise) -- the critical value of every interval
#   note   why a design degraded, only on "design_partial": "size" | "unsupported" | "failed"
# Stored only when the table is weighted, so an unweighted table carries no `inference` and no golden
# moves ("absent when unset", Phase 17b).
get_inference <- function(x) get_meta(x)[["inference"]]
set_inference  <- function(x, inference) set_meta_field(x, "inference", inference)

new_inference_attr <- function(basis = "n", degf = NA_real_, note = NULL) {
  out <- list(basis = basis)
  degf <- suppressWarnings(as.double(degf)[1])
  if (length(degf) == 1L && !is.na(degf) && is.finite(degf)) out$degf <- degf
  if (!is.null(note) && nzchar(note)) out$note <- note
  out
}

# The BASIS of a table, resolved for display: the stored fact, else "n" (a hand-built table, an older
# object, a table whose metadata a pipeline dropped -- all of which mean "no design effect claimed").
tab_inference_basis <- function(x) get_inference(x)[["basis"]] %||% "n"

# Phase 14v: `empirical_tips` -- the multinomial crude-companion tooltip data (see new_tab()).
get_empirical_tips <- function(x) get_meta(x)[["empirical_tips"]]
set_empirical_tips <- function(x, empirical_tips) set_meta_field(x, "empirical_tips", empirical_tips)

# Last Phase z15: `assumptions` -- the observed curve of each continuous predictor (see new_tab()),
# the data behind the row sparklines and behind reg_check_plots()' linearity panel.
get_assumptions <- function(x) get_meta(x)[["assumptions"]]
set_assumptions <- function(x, assumptions) set_meta_field(x, "assumptions", assumptions)

# Phase 14w: `reg_meta` -- a regression table's model record (see new_tab()).
get_reg_meta <- function(x) get_meta(x)[["reg_meta"]]
set_reg_meta <- function(x, reg_meta) set_meta_field(x, "reg_meta", reg_meta)

#' Store a caption on a tabxplor table
#'
#' Records a caption/title on a \code{tabxplor_tab} that survives a dplyr pipeline (it is kept in the
#' table's \code{meta$vars$caption}, carried through every verb) and is read by the exporters
#' (\code{\link{tab_md}}, \code{\link{tab_kable}}, \code{\link{tab_xl}}, \code{\link{tab_plot}}) as the
#' table title, ahead of a regression table's auto-title, when the exporter's own \code{caption=}
#' argument is not supplied. \code{get_caption()} reads it back (\code{NULL} when none is stored).
#'
#' @param x A \code{tabxplor_tab} (or a \code{tabxplor_tabs} list of them).
#' @param caption A single string, or \code{NULL} / \code{NA} to remove any stored caption.
#' @return \code{x}, with its stored caption set (\code{set_caption}) ; the caption or \code{NULL}
#'   (\code{get_caption}).
#' @export
set_caption <- function(x, caption) {
  if (is.list(x) && !is.data.frame(x)) return(purrr::map(x, set_caption, caption))
  if (!is.null(caption) && (length(caption) != 1L || is.na(caption) || !nzchar(caption)))
    caption <- NULL
  v <- get_vars_attr(x)
  if (is.null(v)) v <- new_vars_attr()
  v$caption <- caption          # NULL removes the sub-field (base-R list semantics)
  set_vars_attr(x, v)
}

#' @rdname set_caption
#' @export
get_caption <- function(x) get_meta(x)[["vars"]][["caption"]]
new_vars_attr <- function(row_vars = character(0), col_vars = character(0),
                          tab_vars = character(0), compacted = FALSE, wt = NA_character_,
                          var_labels = character(0)) {
  out <- list(row_vars = as.character(row_vars), col_vars = as.character(col_vars),
              tab_vars = as.character(tab_vars), compacted = isTRUE(compacted))
  # Phase 16d: the weight column NAME drives the footer "Weighted by <wt>." line. It is stored ONLY when
  # there IS a weight -- an unweighted table's `vars` attribute is unchanged (no field), so no golden /
  # serialized table churns and get_vars_attr(x)$wt is simply NULL. (get_weight_name reads it either way.)
  wt <- if (length(wt)) as.character(wt)[1] else NA_character_
  if (!is.na(wt) && nzchar(wt)) out$wt <- wt
  # Phase k: variable labels (name -> label, haven/labelled) for the opt-in name display-swap. Stored
  # ONLY when non-empty (same absent-when-unset rule as `wt`), so a label-free table churns nothing.
  if (length(var_labels) && !is.null(names(var_labels))) {
    keep       <- !is.na(var_labels) & nzchar(names(var_labels))
    var_labels <- var_labels[keep]
    if (length(var_labels)) out$var_labels <- var_labels
  }
  out
}
# The package CI defaults, used when a table carries no `ci_settings`. DERIVED from tab()'s formals
# (Phase 17a) rather than hand-mirrored, so the two can never drift: each default is the tab() formal
# evaluated (conf_level resolves getOption("tabxplor.conf_level", 0.95), exactly as tab() would).
default_ci_settings <- function() {
  fm <- formals(tab)
  ce <- environment()
  lapply(fm[c("conf_level", "method_cell", "method_diff",
              "method_ratio", "method_mean_diff", "method_mean_ratio")],
         eval, envir = ce)
}

# === SECTION: the ONE table-attribute carry (Phase 14d / 17b) ======================================
# Every table-level attribute is listed HERE, once. Before this, each of the ~34 dplyr S3 methods /
# vctrs reconcilers named all of them by hand, so each attribute paid the same ~34-site edit; a table
# that lost an attribute lost it silently, in one verb only. Phase 17b collapsed the six 2.0.0-new
# attrs into ONE `meta` list, so tab_attrs() now carries just THREE things.
# WARNING: `test` is ROW-BOUND (one row per subtable x col_var), so a bind must vec_rbind it -- that
# is why the vctrs reconcilers still name it explicitly and only take tab_attrs() for the rest.
#' @keywords internal
tab_attrs <- function(from) {
  list(subtext = get_subtext(from),
       test    = get_test(from),
       meta    = get_meta(from))
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
# `test` is ROW-BOUND (one row per subtable x col_var) so it rbinds; the `meta` sub-fields reconcile
# element-wise (x wins, other fills a NULL), EXCEPT `color_breaks` which merges per named scale (so a
# partial override on either side survives -- matching push_color_breaks() precedence).
#' @keywords internal
tab_meta_bind <- function(mx, my) {
  if (is.null(mx) && is.null(my)) return(NULL)
  if (is.null(mx)) mx <- list()
  if (is.null(my)) my <- list()
  out <- list()
  for (nm in union(names(mx), names(my))) out[[nm]] <- mx[[nm]] %||% my[[nm]]
  cbx <- mx[["color_breaks"]]; cby <- my[["color_breaks"]]
  if (!is.null(cbx) || !is.null(cby)) {
    merged <- if (is.null(cby)) list() else cby
    if (!is.null(cbx)) for (s in names(cbx)) merged[[s]] <- cbx[[s]]
    out[["color_breaks"]] <- merged
  }
  out <- out[!vapply(out, is.null, logical(1))]
  if (length(out)) out else NULL
}

#' @keywords internal
tab_bind_attrs <- function(x, other) {
  subtext <- unique(vctrs::vec_c(get_subtext(x), get_subtext(other)))
  if (length(subtext) > 1) subtext <- subtext[subtext != ""]
  list(subtext = subtext,
       test    = vctrs::vec_rbind(get_test(x), get_test(other)),
       meta    = tab_meta_bind(get_meta(x), get_meta(other)))
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
    "2.0.0", I(paste0("`$", name, "` on a tabxplor tab")),
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
#' @keywords internal
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
# Last Phase j: two COMPANION columns `effect_size` (double) + `es_type` (character, e.g. "cramer_v"/
# "phi"/"eta2") ride each omnibus row -- an effect size belongs ON its test's row, so it is a column,
# not a separate row. Reg-footer / older rows carry NA/"" there (vec_rbind fills, but the uniform
# schema keeps binds clean).
# Last Phase z15: the 13th column `term` -- WHICH PREDICTOR a reg-footer row is about ("" = the whole
# model). It is a new DIMENSION, not a new test type, and it could not ride `row_var`: on a reg footer
# row `row_var` already means the SPLIT-GROUP LEVEL, in reg_footer_lines() (the `is_split` switch + the
# cell key), in test_grid_reg() (the group key) and in reg_spread_models() (which re-keys by it and
# DROPS the misses). A predictor name there flipped a plain table into "split" mode and silently
# deleted the rows on a spread one -- measured. It backs the per-predictor Linearity + "global" rows
# (reg_footer_plan() renders `label: term`) and the interaction/global LINES, which used to overload
# `row_var` and printed the split level, repeated, instead of the predictors.
# Phase 9b-3: memoized -- tibble() validation is ~1.4 ms/call and this placeholder is built several
# times per table (~3% of the build). The empty tibble is STATELESS, so the cached copy is shared
# safely (R copy-on-modify: any caller edit -- bind_rows / mutate / attr<- -- copies first, never
# touching the base). Byte-identical: same object tibble() produced.
new_test_tibble <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) {
      cached <<- tibble::tibble(row_var   = character(), col_var     = character(), test = character(),
                                term      = character(),
                                statistic = double()   , df1         = double()   ,
                                df2       = double()   , pvalue      = double()   ,
                                n         = double()   , min_e       = double()   ,
                                effect_size = double() , es_type     = character(),
                                pvalue_exact = double(),
                                # Last Phase z16-i (W8): `n` is ALWAYS the raw count; `deff` is the
                                # mean design effect this row's test corrected by (NA on basis "n").
                                deff       = double())
    }
    cached
  }
})

# Phase 16a: test_display_rows / pvalue_line_fmt / test_cell_label / reg_footer_spec+siblings /
# the fmt-cell builders (reg_gof_cell/reg_pvalue_cell/reg_blank_cell/stat_line_fmt) MOVED to
# R/tab-test-display.R (all `test`-attribute display in one module).


#Methods to print class tabxplor_tab -----------------------------------------------------

# Why this exists: THE one predicate for "does options(tabxplor.print) ask for an html render?".
# "html" is the taught value (the engine has been html-first since Last Phase g renamed tab_kable ->
# tab_html); "kable" is the pre-2.0.0 synonym, kept working. Anything else prints to the console.
tx_print_html <- function() {
  getOption("tabxplor.print") %in% c("html", "kable")
}

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
#' @keywords internal
print.tabxplor_tab <- function(x, width = NULL, ..., n = 100, max_extra_cols = NULL,
                               max_footer_lines = NULL, min_row_var = 30, get_text = FALSE) {
  # Phase 13a: install this table's per-table color_breaks override for the render (no-op otherwise).
  .cb <- push_color_breaks(x); on.exit(pop_color_breaks(.cb), add = TRUE)
  if (tx_print_html()) {
    x <- tab_html(x)
    print(x)
    return(invisible(x))
  }

  # Phase 10i-B: materialise the add_n (in-cell {pct} (n={n})) / add_pct (col_pct) display extras for
  # the console (backend "text"); p-value stays the summary BLOCK (pvalue = FALSE), NOT body rows.
  x <- tab_materialize_extras(x, backend = "text", pvalue = FALSE)

  # Phase 16a (was 10i-B decision 2): the console shows the summary block -- a GFM-aligned table of the
  # `test` attribute (chi2 / ANOVA-F for a crosstab, the GOF footer for a regression), printed above the
  # tibble -- NOT p-value body rows. It sits AFTER the kable branch so `print = "kable"` renders p-value
  # ROWS (via tab_kable -> tab_export_prep materialize) instead. Nothing prints without a test attribute.
  test_render_console(test_summary_grid(x))

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
  # to <fct> in the header line so the displayed type stays correct. The type-tag line is out[3]
  # for a plain tabxplor_tab, out[4] for a grouped_tab (which prints one extra header line) --
  # this ONE method serves both classes (print.tabxplor_grouped_tab is an alias below).
  if (length(n_row_var) != 0) {
    regular_ex <-
      paste0("^(", paste0(rep("[^<]+<", n_row_var), collapse = ""), ")<char>") |>
      stringi::stri_replace_first_regex("<\\)<", ")<")

    hdr <- 3L + inherits(x, "grouped_df")
    out[hdr] <- out[hdr] |> stringi::stri_replace_first_regex(regular_ex, "$1<fct> ")
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
#' @keywords internal
# The grouped print is byte-identical to print.tabxplor_tab except the <char>->
# <fct> header-line index (out[4] vs out[3]), which that method now derives from
# inherits(x, "grouped_df"). So it is the SAME function (Phase 17a merge).
print.tabxplor_grouped_tab <- print.tabxplor_tab


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
#' @keywords internal
print.tabxplor_tabs <- function(x, ...) {
  # Mirror print.tabxplor_tab: honour options("tabxplor.print"). "html" renders all tables joined
  # (routed to the Viewer, like a single tab); otherwise print each element's tibble in sequence.
  if (tx_print_html()) {
    print(tab_html(x))
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
  knitr::knit_print(tab_html(x), ...)
}

# knit_print for a SINGLE tab: without it, knitr's default auto-print captures print()'s html as
# escaped text, so options(tabxplor.print = "html") could never render a bare `tab(...)` chunk as a
# real table in Rmd/Quarto. Honours the option: html/kable -> as-is html; else the default text
# capture (which the fansi output hooks can colour).
#' @exportS3Method knitr::knit_print
knit_print.tabxplor_tab <- function(x, ...) {
  if (tx_print_html()) return(knitr::knit_print(tab_html(x), ...))
  NextMethod()
}

# The grouped class vector does not contain "tabxplor_tab" (separate S3 world) -> own registration.
#' @exportS3Method knitr::knit_print
knit_print.tabxplor_grouped_tab <- function(x, ...) {
  if (tx_print_html()) return(knitr::knit_print(tab_html(x), ...))
  NextMethod()
}


# Phase 16a: print_chi2() + print_reg_footer() were REPLACED by the shared, aligned GFM summary block
# test_render_console(test_summary_grid(x)) in R/tab-test-display.R (called from both print methods).


#' Table headers for class tab
#' @importFrom pillar tbl_sum
#' @param x An object of class tabxplor_tab
#' @param ... Other parameters.
#' @return A table header
#' @export
#' @method tbl_sum tabxplor_tab
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
tbl_format_footer.tabxplor_tab <- function(x, setup, ...) {
  default_footer <- NextMethod()
  # Phase 16e: the whole below-table footer (weight -> Model: -> colour legend -> stars -> user subtext) is
  # ONE shared model now -- tab_footer_streams() builds the ordered typed streams, render_footer() applies
  # the console "# " subtle prefix (role-aware: a legend keeps its colours, the plain lines are subtle whole).
  streams <- suppressWarnings(tab_footer_streams(
    x, style = "terse", subtext = get_subtext(x) |> purrr::discard(\(s) s == "")))
  c(default_footer, render_footer(streams, medium = "console"))
}


#' Table body for class tab
#' @importFrom pillar tbl_format_body
#' @param x An object of class tabxplor_tab
#' @param setup A setup object from the table
#' @param ... Other parameters.
#' @return A character vector.
#' @export
#' @method tbl_format_body tabxplor_tab
#' @keywords internal
tbl_format_body.tabxplor_tab <- function(x, setup, ...) {
  default_body <- NextMethod()

  body_data  <- default_body[-(1:2)]
  ind   <- dplyr::group_indices(setup$x)[1:length(body_data)]
  ind   <- tidyr::replace_na(ind != dplyr::lag(ind, default = 1L), FALSE)
  body_data <- body_data |>
    purrr::map2(ind, function(.x, .y) if (.y) {c("", .x)} else {.x}) |>
    purrr::flatten_chr()

  c(default_body[1:2], body_data) |> `class<-`("pillar_vertical")
}



#' Print a tabxplor table in html
#'
#' @description
#' The HTML exporter behind \code{\link{tab_export}}: `tab_export(x, format = "html")` calls this, and
#' `tab_kable()` is a permanent alias of `tab_html()`. Use it directly for HTML-specific arguments.
#'
#' @param tabs A table made with \code{\link{tab}} or \code{\link{tab_many}},
#'   or a `list` of tab with the same `col_vars` and no `tab_vars`.
#' @param color_type `r lifecycle::badge("deprecated")` Inert since 2.0.0: the text channel always uses
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
#'
#' \code{"print"} (or \code{"bw"}) is the black-and-white **publication** palette: over-represented
#' cells in bold, under-represented ones in italic, an underline for the strongest threshold, and a
#' grey fill for a second colour measure. It exists because a greyscale print loses the colour
#' palette's direction entirely (both background ramps convert to the same shades of grey). The
#' typography is written as real `<b>`/`<i>`/`<u>` markup as well as CSS, so it survives a
#' stylesheet-less destination -- a paste into Word, or GitHub's markdown. You rarely need to ask for
#' it: any coloured table already **prints** in this scheme, see \code{\link{tab_css}}'s
#' `print_rules`.
#' @param html_24_bit `r lifecycle::badge("deprecated")` Inert since 2.0.0: exports are always
#' 24-bit (the OKLCH palettes). Kept only so old calls do not error.
#' @param css `engine = "html"` only: inline the stylesheet with the table, so the output is
#' self-contained (default, from \code{getOption("tabxplor.tab_kable_css")}). Set `FALSE` in a many-table
#' document that emits \code{\link{tab_css}} once at the top -- the stylesheet is table-independent,
#' so one copy styles every table. With `FALSE` and no \code{\link{tab_css}} call, tables render
#' uncoloured.
#' @param tooltips By default, takes \code{getOption("tabxplor.tab_kable_tooltips")}
#' (\code{TRUE} unless set): html tooltips display additional informations at mouse
#' hover. Set to \code{FALSE} to discard (or set the option to \code{FALSE} once per
#' document, e.g. in a vignette or report where every table auto-prints).
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
#' tab_html(tabs, theme = "light")
#' }
tab_html <- function(tabs,
                     theme = NULL, color_type = lifecycle::deprecated(), html_24_bit = NULL,
                     color = TRUE, tooltips = NULL, popover = NULL, color_legend = TRUE,
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
  if (lifecycle::is_present(color_type)) lifecycle::deprecate_soft("2.0.0", "tab_html(color_type)")
  # Phase 13a: install a per-table color_breaks override for the render (no-op otherwise).
  .cb <- push_color_breaks(tabs); on.exit(pop_color_breaks(.cb), add = TRUE)
  # Phase 10j: the theme/color/color_legend preamble is the shared resolver. `html_24_bit` is inert
  # (Phase 13a): exports are always 24-bit, kept only so old calls do not error.
  o <- resolve_export_opts(theme = theme, color = color, color_legend = color_legend,
                           transpose = transpose, var_names = var_names, allow_auto = TRUE)
  theme <- o$theme
  color_legend <- o$color_legend
  compute <- c("refs", "bold")  # "range" DORMANT (retired totcol_range)
  if (o$color) compute <- c(compute, "colors")
  html_font <-
    if (is.null(html_font)) {getOption("tabxplor.kable_html_font")} else {html_font}
  tooltips <- if (is.null(tooltips)) {getOption("tabxplor.tab_kable_tooltips", TRUE)} else {tooltips}
  popover <- if (is.null(popover)) {getOption("tabxplor.kable_popover")} else {popover}
  engine  <- if (is.null(engine)) {getOption("tabxplor.tab_kable_engine", "html")} else {engine}
  engine  <- match.arg(engine, c("kableExtra", "html"))
  css     <- if (is.null(css)) {tx_getOption(c("tabxplor.kable_css", "tabxplor.tab_kable_css"), TRUE)} else {isTRUE(css)}

  # Phase 14o: a transposed table is a render-model flip whose columns are heterogeneous character
  # (see tx_transpose_render()); the kableExtra engine cell_spec()s each fmt column, which no longer
  # exists here, so transpose renders through the home-built html engine.
  if (isTRUE(o$transpose) && !identical(engine, "html")) {
    cli::cli_inform(
      c("!" = 'transpose = TRUE renders through {.code engine = "html"}.',
        "i" = "The kableExtra engine styles each formatted column, which a transposed table has not."),
      .frequency = "once", .frequency_id = "tabxplor_transpose_engine")
    engine <- "html"
  }

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
    color_legend = color_legend, what = "tab_html()"
  )

  # Phase 10e: render each prepared table through the engine seam. The colour legend is CONTENT (a
  # measure summary), so it is prepended per table to `subtext` here; the seam styles everything else.
  in_knitr <- !is.null(knitr::opts_knit$get("out.format"))
  parts <- purrr::map(prep$tables, function(rd) {
    subtext <- character(0)
    if (!isTRUE(rd$vars$degrade)) {
      # Phase 16e: the whole footer (weight -> Model: -> colour legend -> stars -> user subtext) via the ONE
      # shared builder. The html engine ships a tabxplor stylesheet, so its legend break-words carry slot
      # CLASSES (theme-toggle-safe) rather than inline hex; kableExtra does not (classes = engine == "html").
      src         <- if (is.null(rd$color_src)) rd$tab else rd$color_src
      want_legend <- color_legend && length(rd$roles$color_cols) != 0
      # Phase 17g: shared rd_footer(); the html engine ships a stylesheet, so its legend break-words
      # carry slot CLASSES (classes = engine == "html") rather than inline hex.
      subtext <- rd_footer(src, "html", theme = theme[1], want_legend = want_legend,
                           subtext = rd$subtext, lang = lang, classes = identical(engine, "html"))
    }
    # Phase 14w (item 1) / 17b / 17g: user caption= -> stored set_caption() -> reg_title (shared).
    cap <- rd_caption(rd, caption)
    render_kable_html(rd, prep$meta, engine = engine, subtext = subtext, caption = cap,
                      tooltips = tooltips, popover = popover, html_font = html_font,
                      full_width = full_width, get_data = get_data, in_knitr = in_knitr, ...)
  })

  if (get_data) return(if (length(parts) == 1L) parts[[1]] else parts)

  # Phase 13d: the html engine's cells carry slot CLASSES, so the theme lives entirely here. The
  # stylesheet is table-independent (see tab_css()), hence built once per call -- or not at all, when a
  # document emitted tab_css() itself (options("tabxplor.tab_kable_css" = FALSE)). kableExtra styles inline.
  style <- if (css && identical(engine, "html")) {
    tab_css(theme = theme, chrome = TRUE, style_tag = FALSE)
  } else ""
  # Phase 14k: `theme` rides along as an attribute so print.tabxplor_kable() can paint the Viewer's
  # page to match -- and, under "auto", resolve it from the editor (the browser cannot see Positron).
  tab_kable_join(parts, engine, css = style, theme = theme)
}

#' @rdname tab_html
#' @details `tab_kable()` is a permanent alias of `tab_html()` -- the two are identical. `tab_html()`
#'   names the output (an HTML table), while the HTML backend *engine* (home-built or \pkg{kableExtra})
#'   is chosen with `engine =`.
#' @export
tab_kable <- tab_html



#' Print a tabxplor table in html
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Superseded by [tab_html()], which renders any table -- `tabxplor_tab` or plain data.frame --
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
#' if (requireNamespace("kableExtra", quietly = TRUE)) kable_tabxplor_style(tabs)
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
  lifecycle::deprecate_soft("2.0.0", "kable_tabxplor_style()", "tab_html()")

  # kableExtra is now Suggests-only; this superseded renderer is the only public entry point that
  # still requires it (tab_html(engine = "html") does not).
  if (!requireNamespace("kableExtra", quietly = TRUE)) {
    cli::cli_abort(c(
      "{.fn kable_tabxplor_style} needs the {.pkg kableExtra} package.",
      "i" = "Install it, or use {.fn tab_html} (the default {.code engine = \"html\"} needs no extra dependency)."
    ))
  }

  html_font <-
    if (is.null(html_font)) {getOption("tabxplor.kable_html_font")} else {html_font}


  tabs <- tabs |> dplyr::ungroup()

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
    out <- out |> kableExtra::kable_classic(
      lightable_options = "hover", # "striped", ?
      #bootstrap_options = c("hover", "condensed", "responsive", "bordered"), #"striped",
      full_width = full_width,
      html_font = html_font, # "DejaVu Sans Condensed", # row_label_position
      #fixed_thead = TRUE,
      ...
    )

  } else {
    out <- out |> kableExtra::kable_material_dark(
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
    out <- out |> kableExtra::add_footnote(subtext, notation = "none", escape = FALSE)
  }

  totcols <- which(stringi::stri_detect_regex(names(tabs), "^Total|^Ensemble"))
  totrows <- which(stringi::stri_detect_regex(tabs[[1]], "^Total|^Ensemble"))

  out <- out |>
    kableExtra::row_spec(
      0, bold = TRUE, # color = "black"
      extra_css = "border-top: 0px solid ; border-bottom: 1px solid ;font-size: 90%;vertical-align: bottom;line-height: 0.9;padding: 3px;text-align: center;" #
    ) |>
    #kableExtra::row_spec(refs2, bold = TRUE) |>
    kableExtra::row_spec(
      nrow(tabs), extra_css = "border-bottom: 1px solid ;"
    ) |>
     #kableExtra::column_spec(fmt_cols, extra_css = "white-space: nowrap;") |>
    #kableExtra::column_spec(unique(c(new_col_var, ncol(tabs))), border_right = TRUE) |>
    #kableExtra::column_spec(other_cols, border_left = TRUE) |>
    kableExtra::column_spec(1, width_min = 20, border_left = TRUE, border_right = TRUE) |>
    kableExtra::column_spec(ncol(tabs), border_right = TRUE) |>
     #kableExtra::row_spec(new_group, extra_css = "border-bottom: 1px solid;") |>
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
      #stringi::stri_replace_all_regex("<td style", '<td class = "align-top"; style')
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
#   - fmt: vctrs::vec_ptype_common() across the tables reconciles the fmt_col_attrs via the SAME
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
        # The old imap_dfr / vec_rbind cast each column via vec_cast.tabxplor_fmt.tabxplor_fmt, reading
        # fields through the GETTERS; fmt_data_wn() reproduces that frame (only wn needs materialising).
        fmt_data_wn(col)
      })
      common <- do.call(vctrs::vec_ptype_common, pieces)   # L3 reconcile via ptype2, O(#tables)
      meta   <- purrr::set_names(
        lapply(fmt_col_attrs, function(a) attr(common, a, exact = TRUE)), fmt_col_attrs)
      fmt_stack_frames(frames, meta)
    } else {
      # Last Phase z10: stacking several row_vars puts DIFFERENT variables' levels in one display
      # column, so an `ordered` class on it would claim an order across variables that does not exist
      # -- and vctrs rightly refuses to combine two ordered factors with different level sets (or an
      # ordered one with a plain factor). Drop the class here, at the one place the axes are merged;
      # a single-row_var table keeps its ordered column untouched.
      if (length(pieces) > 1L && any(purrr::map_lgl(pieces, is.ordered)))
        pieces <- purrr::map(pieces, function(p)
          if (is.ordered(p)) factor(p, levels = levels(p), ordered = FALSE) else p)
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
    compacted = TRUE,
    wt        = get_vars_attr(tabs[[1]])$wt,  # Phase 16d: the weight survives a compact merge
    # Phase k: the per-tab variable labels (each row_var's + the shared col_vars') survive the merge
    # too -- union across the merged tables, first name wins, so the opt-in name swap still works.
    var_labels = {
      vl <- do.call(c, unname(purrr::map(tabs, ~ get_vars_attr(.)[["var_labels"]])))
      if (length(vl)) vl[!duplicated(names(vl))] else character(0)
    }
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

  if (sum(stringi::stri_detect_regex(names(tabs), "^Total_")) == 1) {
    tabs <- tabs |>
      dplyr::rename_with(~ "Total", .cols = tidyselect::starts_with("Total_"))
  }

  # Phase 10i-B: carry the add_n/add_pct intent through the merge (all per-row_var tabs share it).
  tabs <- new_tab(tabs, subtext = subtext, test = tabs_chi2,
                  meta = list(render_extras = render_extras_first,
                              ci_settings = ci_settings_first, vars = vars_merged)) |>
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
# only, NOT p-value -- the console print methods (the console shows the summary block instead of
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

  # Phase 17c: seed the display-time row-role vector ("total"/"data" from the drift-free is_totrow flag).
  # Each row-adding spec below EXTENDS it (add_n_pct -> "n"/"row_pct", footer -> "pvalue"/"gof"/"blank");
  # collapse_totals slices it. Consumers (export-prep tot_block, collapse) read tab_row_roles() rather
  # than matching an English row label -- the whole point of the role model.
  tab <- set_row_roles(tab, dplyr::if_else(is_totrow(tab), "total", "data"))

  # Phase 17g: the synthetic extras are now DECLARED specs run by tab_materialize(). The add_n / add_pct
  # display intent is read ONCE here into `ctx` (a spec cannot re-read it after add_n_pct clears
  # render_extras), and threaded to every spec. The two former build-then-undo cycles are gone: the
  # add_n `n` COLUMN is built ONLY for xl (text folds directly, no throwaway), and collapse_totals is a
  # declared display slice reading the stored roles.
  re  <- get_render_extras(tab)
  ctx <- list(add_n = isTRUE(re$add_n), add_pct = isTRUE(re$add_pct), pvalue = isTRUE(pvalue),
              common_totrow = isTRUE(re$common_totrow), common_totrow_ref = isTRUE(re$common_totrow_ref))
  tab_materialize(tab, backend, ctx)
}

# Phase 17g: run the applicable materialize specs in order. `ctx` carries the shared add_n/add_pct/
# pvalue intent so specs stay independent of render_extras (which add_n_pct clears mid-run).
#' @keywords internal
#' @noRd
tab_materialize <- function(tab, backend, ctx) {
  for (spec in materialize_specs()) {
    if (spec$when(tab, backend, ctx)) tab <- spec$apply(tab, backend, ctx)
  }
  tab
}

# Phase 17g: the declared inventory of display-time synthetic rows/cols. Each spec names WHAT it adds
# (kind, matching the stored row-role vocabulary where it adds rows), WHEN it applies (a predicate over
# tab + backend + intent), and HOW (apply). Reading this list IS the map of every synthetic extra and
# its per-backend policy -- replacing the old imperative if/else passes.
#' @keywords internal
#' @noRd
materialize_specs <- function() list(
  # add_n / add_pct: the base-n column/row + the col%/row% companions. xl keeps the real `n` COLUMN;
  # text folds the base into the Total cell (mat_add_n_pct). Clears the consumed render_extras intent.
  list(kind = "add_n_pct",
       when  = function(tab, backend, ctx) ctx$add_n || ctx$add_pct,
       apply = mat_add_n_pct),
  # Phase 16c: an OR/RRR table's "100%" total column is meaningless. Console+add_n keeps it as a base-n
  # cell (folded by add_n_pct); Excel exports only the base-n column, and console add_n=FALSE has no base
  # -> drop the % total column in both. No-op on a non-OR table.
  list(kind = "or_total",
       when  = function(tab, backend, ctx) tab_is_or_display(tab),
       apply = function(tab, backend, ctx) tab_or_total_col(tab, backend, ctx$add_n)),
  # Excel-only mean + sd twin column (Phase 13c-v): console/md/kable show sd inline as "mean (sigma sd)".
  list(kind = "sd_twin",
       when  = function(tab, backend, ctx) identical(backend, "xl"),
       apply = function(tab, backend, ctx) mat_sd_twin(tab)),
  # p-value / GOF footer rows from the kept `test` attribute. tab_pvalue_lines no-ops on a regression
  # table, so a crosstab gets its chi2 row and a reg table its GOF footer (Phase 12f).
  list(kind = "footer",
       when  = function(tab, backend, ctx) ctx$pvalue,
       apply = function(tab, backend, ctx) {
         tab <- tab_pvalue_lines(tab)
         if (is_reg_footer(get_test(tab))) tab <- reg_footer_lines(tab)
         tab
       }),
  # Phase 14n / Last Phase m: collapse the redundant per-block Total rows of a compacted several-row_vars
  # table into ONE shared Total, shown in its OWN group (a display slice needing the "as displayed"
  # equality). OPT-IN via `common_totrow` (default FALSE = one Total per row_var, no collapse). Run LAST,
  # so every role recomputes on the collapsed table; the core tab() object keeps every total row.
  list(kind = "collapse_totals",
       when  = function(tab, backend, ctx) isTRUE(ctx$common_totrow),
       apply = function(tab, backend, ctx)
         tab_collapse_total_rows(tab, ref_bold = isTRUE(ctx$common_totrow_ref)))
)

# add_n / add_pct spec apply. Reuses tab_add_n_pct() (byte-identical field construction; its grouped
# outer-mutate reproduces the per-subtable scoping on the final merged / grouped table). `backend` = xl
# builds the real `n` COLUMN; text skips it (tab_add_n_pct) and folds the base into the Total cell from
# its OWN `n` field (tab_fold_addn_incell -- no throwaway column). Clears render_extras (idempotent).
#' @keywords internal
#' @noRd
mat_add_n_pct <- function(tab, backend, ctx) {
  tab <- tab_add_n_pct(list(tab), add_n = ctx$add_n, add_pct = ctx$add_pct, backend = backend)[[1]]
  if (identical(backend, "text") && ctx$add_n) tab <- tab_fold_addn_incell(tab)
  set_render_extras(tab, NULL)
}

# Excel-only sd twin: for each numeric mean column insert an uncoloured sibling "<var>_sd" holding
# sd = sqrt(var) (display "var" -> get_num() IS the sd; tab_xl's numFmt adds the sigma prefix), placed
# directly after its mean column. Purely an Excel layout concern (text backends fold sd inline).
#' @keywords internal
#' @noRd
mat_sd_twin <- function(tab) {
  is_mean_col <- function(col) is_fmt(col) && identical(get_type(col), "mean") &&
    any(get_display(col) %in% c("mean", "mean_ci"))
  means <- names(tab)[purrr::map_lgl(tab, is_mean_col)]
  for (nm in means) {
    sdc <- tab[[nm]]
    tab[[paste0(nm, "_sd")]] <-
      set_color(set_display(set_var(sdc, suppressWarnings(sqrt(get_var(sdc)))), "var"), "no")
  }
  if (length(means) > 0) {                       # place each _sd directly after its mean column
    ord <- names(tab)
    for (nm in rev(means)) {
      sd_nm <- paste0(nm, "_sd")
      rest  <- ord[ord != sd_nm]
      ord   <- append(rest, sd_nm, after = which(rest == nm))
    }
    tab <- tab[ord]
  }
  tab
}

# Phase 14n: on a COMPACTED several-row_vars table (tab_compact() stacked one standalone table per
# row_var, each with its own Total row) the col_var marginal -- and its base n -- is a property of the
# shared population, not the row_var. So under na = "keep"/"drop_all"/"common_base" every block's Total
# is identical by construction; only na = "drop" (each row_var drops its OWN missing values) makes them
# genuinely differ. This DISPLAY-ONLY step drops the redundant Total rows (keeping the LAST block's) when
# every block's total renders identically, else keeps them all + one message naming na = "drop". Called
# as the final step of tab_materialize_extras(), so bold / totblock borders / new_group / references /
# tooltips all recompute on the collapsed table with zero per-backend code. A single-row_var or a
# tab_vars table is never compacted, so the guard leaves both untouched (a tab_vars table's per-subtable
# totals are real, not duplicates).
#
# The comparison unit is the whole TOTAL BLOCK -- the Total row + its trailing add_n base `n` row -- not
# just the Total row: under pct = "col" the Total row is ALWAYS "100%" and the real base lives in the `n`
# row, so comparing the Total row alone would silently collapse col% tables with a genuinely different N.
#' @keywords internal
#' @noRd
tab_collapse_total_rows <- function(tab, ref_bold = FALSE) {
  if (!isTRUE(get_vars_attr(tab)$compacted)) return(tab)   # single row_var / tab_vars: untouched
  is_tot <- is_totrow(tab)
  tot    <- which(is_tot)
  if (length(tot) < 2L) return(tab)

  n_row   <- nrow(tab)
  fmt_nms <- names(tab)[purrr::map_lgl(tab, is_fmt)]

  # A block's total BLOCK = its Total row + the contiguous add_n / add_pct SUMMARY rows that follow it
  # (the add_n / add_pct base/pct rows -- tab_materialize_extras()'s "n" / "row_pct" rows, drawn as
  # "Total | row_pct | n"). A p-value row is block-SPECIFIC (a different test per row_var), so it is NOT
  # swept in and survives the collapse. Phase 17c: read the STORED role (seeded/extended in materialise)
  # instead of matching the English row label; the sweep is still gated to the SAME grouping value so it
  # can never cross into the next block.
  grp_col <- dplyr::group_vars(tab)
  grp     <- if (length(grp_col) >= 1L && grp_col[1] %in% names(tab)) as.character(tab[[grp_col[1]]]) else
    rep(NA_character_, n_row)
  is_summary <- tab_row_roles(tab) %in% c("n", "row_pct")

  block_rows <- function(i) {
    rows <- i; j <- i + 1L
    while (j <= n_row && is_summary[j] && identical(grp[j], grp[i])) { rows <- c(rows, j); j <- j + 1L }
    rows
  }
  blocks <- lapply(tot, block_rows)

  # "As displayed" signature: text format() over EVERY fmt column across the block's rows -- the single
  # canonical predicate for all backends (two totals in one column pad to the same width, so string
  # equality is displayed equality; comparing every column also catches the xl pct="row" case where the
  # base n is a separate column, and any mean/_sd column).
  sig <- vapply(blocks, function(rows)
    paste(unlist(lapply(fmt_nms, function(nm) format(tab[[nm]][rows]))), collapse = "\r"),
    character(1))

  if (length(unique(sig)) > 1L) {                          # genuinely different totals -> keep them all
    cli::cli_inform(
      c("i" = paste0(
        "The variables have different total rows, so every total is shown ",
        "(under {.code na = \"drop\"} each variable drops its own missing values). ",
        "Use {.code na = \"keep\"}, {.code \"drop_all\"} or {.code \"common_base\"} ",
        "for a single total row.")),
      .frequency = "once", .frequency_id = "tabxplor_totrows_differ")
    return(tab)
  }

  drop_rows <- unlist(blocks[-length(blocks)])             # keep the LAST block's total; drop the rest
  keep <- setdiff(seq_len(n_row), drop_rows)
  out  <- tab[keep, ]                                       # global indices -> class/attrs/grouping kept
  rr   <- get_row_roles_raw(tab)                            # slice the row-role vector with the rows
  if (!is.null(rr) && length(rr) == n_row) out <- set_row_roles(out, rr[keep])

  # Last Phase m: the shared Total gets its OWN group (a blank row_var, its level stays "Total") after a
  # blank-line separator -- not tucked under the last row_var. Reassign the surviving total block (Total
  # row + its trailing n/row_pct rows) to a distinct blank sentinel in the grouping column and regroup, so
  # the render-time separator (group_indices) sees it. When the total is a reference for some row_var
  # (ref = "tot" -> ref_bold), mark the Total row bold (in_refrow -- the shared bold anchor signal).
  surv_pos <- match(blocks[[length(blocks)]], keep)
  surv_pos <- surv_pos[!is.na(surv_pos)]
  grp_col  <- dplyr::group_vars(out)
  if (length(surv_pos) && length(grp_col) >= 1L && grp_col[1] %in% names(out)) {
    gc <- grp_col[1]
    if (is.factor(out[[gc]]) && !"" %in% levels(out[[gc]]))
      levels(out[[gc]]) <- c(levels(out[[gc]]), "")
    out[[gc]][surv_pos] <- ""                              # blank row_var -> its own group (Q1)
    out <- dplyr::group_by(out, dplyr::across(tidyselect::all_of(grp_col)))
    if (isTRUE(ref_bold)) {
      tot_pos <- surv_pos[[1]]                             # first block row = the Total row
      for (nm in fmt_nms) {
        v  <- out[[nm]]
        fr <- vctrs::field(v, "in_refrow"); fr[tot_pos] <- TRUE
        vctrs::field(v, "in_refrow") <- fr
        out[[nm]] <- v
      }
    }
  }
  out
}


#' Transform chi2 attribute table of a tabxplor_tab into rows with pvalues.
#'
#' @param tabs A tabxplor_tab (with chi2 table as attribute).
#'
#' @return A tabxplor_tab.
#' @keywords internal
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
  test_tbl <- get_test(tabs)
  if (is.null(test_tbl) || nrow(test_tbl) == 0) return(tabs)

  group_chr <- purrr::map_chr(dplyr::groups(tabs), rlang::as_name)
  gv        <- tab_get_vars(tabs)
  row_var   <- gv$row_var
  # Phase 14n: key the p-value rows by the table's GROUPING columns (its subtable axis) intersected with
  # the test tibble -- the tab_vars for a tab_vars table, the synthetic `row_var` column for a COMPACTED
  # several-row_vars table (== group_chr on a crosstab, so it also drives the per-group placement).
  disc <- intersect(group_chr, names(test_tbl))

  # first-level column of each col_var (where the p-value cell is placed): col_var <-> column name
  first_lv  <- gv$col_vars_levels |> purrr::map_chr(~ rlang::as_name(dplyr::first(.)))
  cv_to_col <- purrr::set_names(unname(first_lv), names(first_lv))
  col_to_cv <- purrr::set_names(names(cv_to_col), unname(cv_to_col))

  # one displayed test per (subtable x col_var): chi2 (factors) / chosen F (means)
  disp <- test_display_rows(test_tbl)
  disp <- dplyr::filter(disp, .data$col_var %in% names(cv_to_col), !is.na(.data$pvalue))
  if (nrow(disp) == 0) return(tabs)

  # Phase 16a: the crosstab footer is now built by the shared tab_append_footer() engine (as the reg
  # GOF footer). Last Phase m: rows in display ORDER = p-value, then effect size; the STATISTIC row is
  # gone from the default (ambiguous once effect size shares the block) -- it returns only under
  # `tabxplor.test_lines = "stat"`/"all". The test TYPE ("Chi2, Welch F; survey-design") and the effect-size
  # MEASURE ("Cramér's V, eta2") move into the row NAMES (per group, via the descriptors), so the p-value
  # CELL is now the bare p (no in-cell "(Chi2)" suffix). Modes: "summary" (default) = p-value + effect
  # size; "all" = + statistic; "stat" = p-value + statistic; "pvalue" = p-value only.
  mode       <- getOption("tabxplor.test_lines", "summary")
  add_stat   <- mode %in% c("stat", "all")
  add_es     <- mode %in% c("all", "summary")
  row_keys   <- c("pvalue", if (add_es) "effect size", if (add_stat) "statistic")
  K          <- length(row_keys)

  # group id per existing row + per displayed-test row (the disc-key tuple; "" when ungrouped)
  gid <- function(df) if (length(disc))
      do.call(paste, c(lapply(disc, function(d) as.character(df[[d]])), sep = "\r"))
    else rep("", nrow(df))
  grp_of      <- gid(tabs)
  disp$.grp   <- gid(disp)
  # a weak chi2 with a Fisher-exact companion (Last Phase j): show the exact p (labelled "Fisher" in the
  # row-name descriptor now, not the cell).
  has_exact   <- if (!is.null(disp[["pvalue_exact"]])) !is.na(disp$pvalue_exact) else rep(FALSE, nrow(disp))
  disp$.pshow <- if (any(has_exact)) ifelse(has_exact, disp$pvalue_exact, disp$pvalue) else disp$pvalue
  key         <- paste(disp$col_var, disp$.grp, sep = "\r")
  # per-group row NAME for each row key (the test type / measure descriptor, computed from that group's
  # displayed tests -- one row per subtable can carry a different mix of factor/numeric col_vars).
  row_label_for <- function(key, g) {
    ing <- disp$.grp == g
    d   <- disp[ing, , drop = FALSE]
    weak <- !is.null(d[["min_e"]]) && any(!is.na(d$min_e) & d$min_e < 5 & !has_exact[ing])
    switch(key,
           "pvalue"      = test_pvalue_descriptor(d$test, any(has_exact[ing]), isTRUE(weak)),
           "effect size" = if (!is.null(d[["es_type"]])) test_es_measure(d$es_type) else "effect size",
           "statistic"   = "statistic")
  }

  # the per-column fill for a non-value / no-test-here position: the column's first display token with
  # n = NA (byte-identical to the pre-16a masked fill, locked by test-golden / export-parity).
  fill_cell <- function(nm) {
    f <- fmt0(dplyr::first(get_display(tabs[[nm]])), type = get_type(tabs[[nm]]))
    vctrs::field(f, "n") <- NA_integer_
    f
  }
  one_cell <- function(nm, g, rl) {
    if (!nm %in% names(col_to_cv)) return(fill_cell(nm))  # not a col_var's first-level column
    cv <- col_to_cv[[nm]]
    r <- disp[key == paste(cv, g, sep = "\r"), , drop = FALSE]
    if (nrow(r) == 0) return(fill_cell(nm))               # this col_var has no displayed test in group g
    if (identical(rl, "pvalue")) pvalue_line_fmt(r$.pshow[1])  # bare p (test type names the row now)
    else if (identical(rl, "effect size")) {
      v <- if (!is.null(r[["effect_size"]])) r$effect_size[1] else NA_real_
      if (is.na(v)) reg_blank_cell() else reg_gof_cell(v, 2L)  # bare number; column type tells V from eta2
    }
    else                         stat_line_fmt(r$statistic[1])
  }
  fmt_cell   <- function(nm, g) do.call(vctrs::vec_c, lapply(row_keys, one_cell, nm = nm, g = g))
  nonfmt_val <- function(nm, g) {
    # the row-label column: the per-group test-type / effect-size descriptors (Last Phase m)
    if (nm == row_var) return(vapply(row_keys, row_label_for, character(1), g = g))
    i <- match(nm, disc)                                  # a grouping column: its group level
    if (!is.na(i)) return(rep(strsplit(g, "\r", fixed = TRUE)[[1]][i], K))
    rep(NA_character_, K)
  }

  # Phase 17b: the whole `meta` list is threaded through the rebuild in one shot (was six getters).
  tab_append_footer(tabs, grp_of, K, fmt_cell, nonfmt_val,
    attrs = list(subtext = get_subtext(tabs), meta = get_meta(tabs)),
    regroup = group_chr,
    footer_groups = unique(disp$.grp),   # only subtables with a displayed test get a p-value row
    row_role = function(g) dplyr::if_else(row_keys == "pvalue", "pvalue", "gof"))  # es/statistic row -> gof
}

# Phase 12f: materialise the regression GOF footer as appended rows (one row per stat, a "Model fit"
# group). Each stat cell is placed under its model column (the fit's first output column; MNL/ordinal
# blank the other category columns), and the row-label column carries the stat label. Phase 16a: a THIN
# config over the shared tab_append_footer() engine (R/tab-test-display.R) -- exactly like the crosstab
# tab_pvalue_lines(); it only supplies `grp_of` (per split group), the per-cell builder and the non-fmt
# labels. Idempotent: `test` is dropped, so a second call no-ops. Renders nothing on a crosstab.
reg_footer_lines <- function(tabs) {
  test_tbl <- get_test(tabs)
  if (!is_reg_footer(test_tbl)) return(tabs)
  spec <- reg_footer_spec()
  reg  <- test_tbl[test_tbl$test %in% names(spec), , drop = FALSE]
  if (nrow(reg) == 0) return(tabs)

  groups    <- dplyr::groups(tabs)
  group_chr <- purrr::map_chr(groups, rlang::as_name)

  nonfmt  <- names(tabs)[!purrr::map_lgl(tabs, is_fmt)]
  # the row-label column = the non-grouping factor (reg groups by `var`; the label column is `levels`).
  rlc     <- setdiff(nonfmt, group_chr)
  row_lab_col <- if (length(rlc) >= 1L) rlc[length(rlc)] else nonfmt[length(nonfmt)]

  # Last Phase z15: one row per (stat, TERM) -- a check / overall-association row is about one
  # predictor, so the plan is the shared reg_footer_plan(), not a bare list of discriminators.
  plan <- reg_footer_plan(reg)
  K    <- if (is.null(plan)) 0L else nrow(plan)
  if (K == 0) return(tabs)
  reg$.term     <- test_term_col(reg)
  footer_labels <- plan$label

  # split_var (Phase 12h): a split table carries per-group GOF (the group level tagged in `reg$row_var`;
  # split_var is the FIRST grouping column). It gets one "Model fit" footer block PER group; a plain
  # table gets one block at the end (a single pseudo-group ""). tab_append_footer interleaves in row order.
  reg_rv    <- if (is.null(reg$row_var)) rep(NA_character_, nrow(reg)) else reg$row_var
  is_split  <- any(nzchar(reg_rv[!is.na(reg_rv)]))
  split_col <- if (is_split) group_chr[[1]] else NA_character_
  grp_of    <- if (is_split) as.character(tabs[[split_col]]) else rep("", nrow(tabs))

  cell_for <- function(nm, k, g) {
    pk  <- plan[k, ]
    sel <- reg$col_var == nm & reg$test == pk$test & reg$.term == pk$term &
      (if (is_split) (!is.na(reg_rv) & reg_rv == g) else TRUE)
    r <- reg[sel, , drop = FALSE]
    if (nrow(r) == 0) return(reg_blank_cell())
    if (identical(pk$kind, "gof")) reg_gof_cell(r$statistic[1], pk$digits)
    else                           reg_pvalue_cell(r$pvalue[1])
  }
  fmt_cell   <- function(nm, g) do.call(vctrs::vec_c, lapply(seq_len(K), cell_for, nm = nm, g = g))
  nonfmt_val <- function(nm, g)
    if (nm == row_lab_col)             footer_labels
    else if (identical(nm, split_col)) rep(g, K)
    else                               rep("Model fit", K)

  # `test` dropped -> idempotent; thread the whole `meta` list through the rebuild (Phase 17b -- was
  # vars / empirical_tips / ci_settings / reg_meta named one by one; is_reg detection must not depend on
  # the dropped `test`, the legend reads reg_meta, and all must survive the footer materialisation).
  # Last Phase z8: `test` is dropped (idempotency), but the pooled interaction rows are NOT rendered as
  # rows -- they feed the table-wide footer LINE, which every backend builds AFTER materialisation. So
  # they are the one part of `test` that must ride through. Re-entry stays a no-op: with only these
  # rows left, `reg` above is empty and this function returns early.
  # (z15: the `global` rows no longer ride through -- they became footer ROWS, so they are consumed
  # here like every other spec'd discriminator.)
  it <- test_tbl[test_tbl$test %in% reg_interaction_types(), , drop = FALSE]
  tab_append_footer(tabs, grp_of, K, fmt_cell, nonfmt_val,
    attrs = list(subtext = get_subtext(tabs), meta = get_meta(tabs),
                 test = if (nrow(it) > 0) it else NULL),
    regroup = group_chr,
    row_role = function(g) plan$kind)                                    # "gof"/"pvalue"
}







#' Print a tabxplor table as plot
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' Superseded (2.0.0): `tab_plot()` renders a \pkg{tabxplor} table as a \pkg{ggpubr} image, but its
#' display is limited and it is no longer actively developed. It keeps working and is retained for a
#' future redesign; prefer \code{\link{tab_kable}} (HTML), \code{\link{tab_md}} (markdown) or
#' \code{\link{tab_xl}} (Excel).
#'
#' @param tabs A table made with \code{\link{tab}} or \code{\link{tab_many}}.
#' @param color_type `r lifecycle::badge("deprecated")` Inert since 2.0.0: the text channel always uses
#' the text palette. The colour CHANNEL is chosen by `color = c(text, background)` (see \code{\link{tab}}).
#' @param theme By default, a white table with black text, Set to \code{"dark"} for a
#' black table with white text.
#'   \code{"print"} (or \code{"bw"}) is the black-and-white **publication** palette: over-represented
#'   cells in bold, under-represented ones in italic, a grey fill for the second colour measure --
#'   readable in a greyscale print, where the colour palette's two directions become the same shade.
#' (\code{tab_plot} draws bold and italic; the underline of the second level has no ggplot2 equivalent.)
#' @param html_24_bit `r lifecycle::badge("deprecated")` Inert since 2.0.0: exports are always
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
  if (lifecycle::is_present(color_type)) lifecycle::deprecate_soft("2.0.0", "tab_plot(color_type)")
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
  compute <- c("refs", "bold")  # "range" DORMANT (retired totcol_range)
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
    if (isTRUE(rd$vars$notify)) tab_degrade_inform(rd$vars$reason)  # batch-aware (see tab_export_prep)
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

  # Last Phase z11: the face comes from the PALETTE, not from guessing at the hex. The old test was
  # `!font %in% c(text_color, grey_color, grey_color2)` -- true exactly where text_hex is non-NA, which
  # is exactly `ann$face_bold` under every colour palette (a palette hex can never equal a chrome hex:
  # fmt_channel_codes upper-cases, tx_chrome_hex is lower-case), so this is byte-identical there. It is
  # NOT `ann$bold`, which folds in the per-CELL keep_black, while tab_plot's structural bolding is the
  # row/column SETS refs2/refs3 -- those two terms are kept verbatim.
  # ggplot2's `fontface` has no underline, so the print palette's second intensity level degrades to
  # its first here (bold / italic only). tab_plot is frozen legacy; that is the accepted loss.
  face_of <- function(field) {
    sel <- purrr::map(rd$ann, field)
    if (length(other_cols) != 0) {
      blanks <- as.list(dplyr::mutate(tabs[other_cols],
                                      dplyr::across(tidyselect::everything(), ~ FALSE)))
      dplyr::bind_cols(blanks, sel)
    } else dplyr::bind_cols(sel)
  }
  bold_sel <- face_of("face_bold")
  ital_sel <- face_of("face_italic")
  face_selection <- purrr::imap(bold_sel, function(b, cn) {
    b <- b | seq_along(b) %in% refs2 | cn %in% refs3
    i <- ital_sel[[cn]]
    dplyr::case_when(b & i ~ "bold.italic", b ~ "bold", i ~ "italic", TRUE ~ "plain")
  }) |> dplyr::bind_cols()

  # Phase 14i: name each block once (the prep's shared run model, as md blanks and html rowspans). No
  # rotation: a ggtexttable cell is a grob, not a table cell. `var_names` (the name column's drop and
  # the col_var span suppression) is already honoured upstream, in the prep.
  for (cl in names(rd$roles$label_cols)) {
    if (!cl %in% names(tabs)) next
    show <- rd$roles$label_runs[[cl]]$show
    tabs[[cl]] <- as.character(tabs[[cl]])
    tabs[[cl]][!show] <- ""
  }
  # Last Phase z15: a graphics device has no block glyphs, so a reg row's sparkline would be one
  # "conversion failure in mbcsToSbcs" per label and a row of garbage. THE plot medium's answer, once,
  # over every text column (the html engine's is the <svg>; every other medium keeps the glyphs).
  for (cl in other_cols) if (cl %in% names(tabs))
    tabs[[cl]] <- tx_spark_strip(as.character(tabs[[cl]]))

  # Phase 14m-ii (rework): a monospace body font ONLY when the table SHOWS significance stars (so the
  # stars align); a plain table keeps the ggpubr default (proportional). WARNING: ggpubr 1.0.0 exposes
  # no per-COLUMN font (table_cell_font() takes no family and replaces the cell gpar), so when it does
  # apply it hits the WHOLE body -- the row labels turn monospace too, a small deviation confined to a
  # STARRED, superseded tab_plot(). Revert with options("tabxplor.plot_num_font" = ""). "Cascadia Mono"
  # must be available to the graphics device (else it substitutes).
  plot_num_font <- if (isTRUE(rd$roles$has_stars))
    getOption("tabxplor.plot_num_font", "Cascadia Mono") else ""
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
        ~ forcats::fct_relabel(., ~ stringi::stri_replace_all_regex(., unbrk, " "))
      ),
      dplyr::across( # otherwise, unbreakable spaces fail in some graphic devices
        where(is.character),
        ~ stringi::stri_replace_all_regex(., unbrk, " ")
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
    # kableExtra::row_spec(refs2, bold = TRUE) |>

    ## wrap
    # kableExtra::column_spec(fmt_cols, extra_css = "white-space: nowrap;") |>



# Phase 16e: the FULL footer below the plot (weight -> Model: -> colour legend -> stars -> user subtext) via
# the ONE shared builder -- previously the plot showed ONLY the colour legend, silently dropping the weight,
# Model:, stars and user subtext lines. Each footer RUN line (text + hex per token) folds into one
# ggtexttable row (a plain line is a single black cell). "runs" is the medium built for this: draw-as-text,
# no fill (a background break-word borrows the darker bg_legend palette, as in Excel). The colour legend is
# included only when colouring is on; the plain lines always.
{
  footer_src  <- if (is.null(rd$color_src)) tabs else rd$color_src
  footer_runs <- rd_footer(footer_src, "runs", theme = theme[1],
                           want_legend = color_legend && length(color_cols) != 0,
                           subtext = subtext, lang = lang)
  color_legend <- purrr::map(footer_runs, function(line) {
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
      dplyr::mutate(text = stringi::stri_replace_all_regex(.data$text, unbrk, " "))
  })
  if (length(color_legend) == 0) color_legend <- NULL
  }

  if (length(color_legend) != 0) {
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
        heights = grid::grobHeight(legendgrob),
        pos = -1
      )
      tabgrob <- gtable::gtable_add_grob(tabgrob, legendgrob,
                                         t = nrow(tabgrob),
                                         b = nrow(tabgrob),
                                         l = 1,
                                         r = ncol(tabgrob))
      tabs_gg <- tab_return_same_class_as_input(tabgrob, input = tabs_gg)
  }


  # Align the whole plot top left
  tabgrob <- get_tablegrob(tabs_gg)
  tabgrob <- justify_grob(tabgrob)
  tabs_gg <- tab_return_same_class_as_input(tabgrob, input = tabs_gg)

  # Phase 16e: draw the caption as a bold title row ABOVE the plot (the `caption` arg was accepted but never
  # drawn). Phase 17g: user caption= -> stored set_caption() -> reg_title via the shared rd_caption().
  cap <- rd_caption(rd, caption)
  if (!is.null(cap) && length(cap) == 1L && !is.na(cap) && nzchar(cap)) {
    titlegrob <- grid::textGrob(cap, x = 0, hjust = 0,
                                gp = grid::gpar(fontface = "bold", fontsize = 11, col = text_color))
    tabgrob <- get_tablegrob(tabs_gg)
    tabgrob <- gtable::gtable_add_rows(
      tabgrob, heights = grid::grobHeight(titlegrob) + grid::unit(4, "mm"), pos = 0)
    tabgrob <- gtable::gtable_add_grob(tabgrob, titlegrob, t = 1, b = 1, l = 1, r = ncol(tabgrob))
    tabs_gg <- tab_return_same_class_as_input(tabgrob, input = tabs_gg)
  }

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
  # Last Phase z10: `shows(field)` = "the cell ALREADY prints this field", tested over the WHOLE
  # template rather than its first token -- so a composite ("{diff} ({pct})", "{or} ({obs})") no longer
  # repeats its own bracket on hover. `disp` stays for the tokens that are not fmt FIELDS (pct_ci,
  # mean_ci, gof, blank), which a template can never contain.
  shows   <- function(field) fmt_display_shows(get_display(x), field)

  # Phase 14b: format() right-pads a column to its widest cell so the numbers align in the TABLE; in
  # a prose tooltip that pad is noise ("ratio:   x1"). Every interpolated value goes through this.
  tip_num <- function(v) stringi::stri_trim(format(v))

  # Phase 14b: diff and ratio are ONE comparison group -- one gate, one "ref" token.
  # `comparable` is the exclusion the diff line always had (a Total-column / total-row cell that IS
  # its own base has nothing to compare itself to); it now gates the ratio line too, which used to
  # print a vacuous "ratio: x1" down every Total column. NA-safe: a contrib table writes onto the
  # Total column, whose pct is NA -- and an NA pct is not a 100% base (mirrors cond_pct / cond_ctr).
  comparable <- !((totcol | totrows) & !is.na(get_pct(x)) & get_pct(x) == 1)
  ok_diff    <- !is.na(get_diff(x))  & comparable
  # `type == "mean"` was excluded, so a mean column showed no ratio line at all -- though under the
  # default color = TRUE the ratio is exactly what colours it.
  ok_rr      <- !is.na(get_ratio(x)) & comparable & !disp %in% "ratio" &
    type %in% c("col", "row", "mean")
  # A reference cell's whole comparison group collapses to ONE "ref": its diff is 0 and its ratio 1
  # by construction, so "diff: ref ; ratio: x1" said nothing, twice. The cell already prints
  # "ref:38%" -- the tooltip only has to name the role, and keep the load-bearing "n:".
  ref_grp    <- ref & (ok_diff | ok_rr)
  show_rr    <- ok_rr & !ref_grp

  out_diff <- if (any(ok_diff | ref_grp)) {
    dplyr::case_when(
      ref_grp ~ gettext("ref"),
      ok_diff ~ paste0(gettext("diff"), ": ", tip_num(set_display(x, "diff"))),
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
                   paste0(gettext("std diff"), ": ", sprintf("%+.2f", std), "sd"), "")
  } else blank

  ci_type  <- get_ci_type(x)
  ci_start <- switch(ci_type, "cell" = "ci: ", "")
  has_ci   <- !is.na(get_ci(x))
  out_ci   <- if (any(has_ci)) {
    dplyr::if_else(
      condition = has_ci,
      true      = paste0(ci_start, tip_num(set_display(x, "ci") |>
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
                     "ratio" = stringi::stri_trim(paste0(out_diff, " ",
                                                        stringi::stri_replace_first_regex(out_ci, "%$", ""))),
                     out_diff)
  out_ci   <- switch(ci_type, "cell" = out_ci, "")

  cond_pct <- type %in% c("col", "row", "all", "all_tabs") &
    !is.na(get_pct(x)) & !shows("pct") & !disp %in% c("pct_ci")
  out_pct <- if (any(cond_pct)) {
    dplyr::if_else(cond_pct, tip_num(set_display(x, "pct")), "")
  } else blank

  cond_mean <- type == "mean" & !is.na(get_mean(x)) & !shows("mean") & !disp %in% c("mean_ci")
  out_mean <- if (any(cond_mean)) {
    dplyr::if_else(cond_mean, tip_num(set_display(x, "mean")), "")
  } else blank

  cond_sd <- type == "mean" & !is.na(get_var(x)) & !shows("var")
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
    dplyr::if_else(show_rr, paste0("ratio: ", tip_num(set_display(x, "ratio")) ), "")
  } else blank

  cond_or <- type %in% c("col", "row") & !is.na(get_or(x)) &
    !shows("or") & !disp %in% c("or_pct", "OR_pct")
  out_or <- if (any(cond_or)) {
    dplyr::if_else(cond_or, paste0("OR: ", tip_num(set_display(x, "or")) ), "")
  } else blank

  # `comparable` (Phase 14b) is the same base-cell exclusion this line had spelled out for itself.
  cond_ctr <- !is.na(get_ctr(x)) & !(get_ctr(x) == Inf) & comparable
  out_ctr <- if (any(cond_ctr)) {
    mctr      <- if (get_comp_all(x)) { totrows & tottabs & !totcol } else { totrows & !totcol }
    ctr_start <- dplyr::if_else(mctr, paste0(gettext("mean_ctr"), ": "), paste0(gettext("contrib"), ": "))
    dplyr::if_else(cond_ctr,
                   paste0(ctr_start, tip_num(set_display(x, "ctr")) |> stringi::stri_replace_first_regex("^-", "")),
                   "")
  } else blank

  # Last Phase z4: the adjusted standardized residual beside the contribution it gates. Derived from
  # the stored p-value (fmt_resid), so it exists exactly where a chi2 contribution was computed --
  # the same cells as `out_ctr`, minus the total rows (a margin has no residual, hence the NA p).
  cond_resid <- is.finite(fmt_resid(x)) & comparable & !shows("resid")
  out_resid <- if (any(cond_resid)) {
    dplyr::if_else(cond_resid,
                   paste0(gettext("std. residual"), ": ", tip_num(set_display(x, "resid"))), "")
  } else blank

  # Last Phase z5: the value this cell is COMPARED TO by `color = "adjustment"` / "between_groups".
  # A stored field, so it exists exactly where tab_reg wrote a counterpart -- NA on every cross-table
  # and on a Constant / compound-formula cell. Last Phase z10: a MULTINOMIAL cell now has one, printed
  # IN-CELL as "{or} ({obs})", so `shows("obs")` suppresses this line there and the `empirical_tips`
  # fragment appended downstream (the crude PERCENTAGE) stays the only extra hover text.
  # The LABEL is read off the column's own stored measure, never guessed from a name.
  cond_obs <- !is.na(get_obs(x)) & !shows("obs")
  out_obs <- if (any(cond_obs)) {
    lbl <- if ("between_groups" %in% c(get_color(x), get_color_bg(x)))
      gettext("ref. group") else gettext("obs")
    dplyr::if_else(cond_obs, paste0(lbl, ": ", tip_num(set_display(x, "obs"))), "")
  } else blank

  # Last Phase z8: the GAP itself -- its size, its confidence interval and its p-value -- wherever
  # tab_reg wrote a `gap_se`. This is where the interval belongs: three numbers are too much for a
  # cell, and the colour IS the display (no `{}` token was added). Read through the very helpers the
  # colour engine reads, so the hover and the fill can never disagree.
  cond_gap <- !is.na(get_gap_se(x)) & !is.na(get_obs(x))
  out_gap <- if (any(cond_gap)) {
    sc   <- fmt_adjustment_score(x)
    bd   <- fmt_gap_bounds(x)
    pv   <- test_fmt_pvalue(fmt_gap_p(x))
    mult <- as.character(get_ci_type(x))[1] %in% c("or", "ratio")
    num  <- function(v) if (mult) paste0("\u00d7", formatC(v, format = "f", digits = 2))
            else sprintf("%+.2f", v)
    dplyr::if_else(cond_gap & is.finite(sc) & is.finite(bd$lo) & !is.na(pv),
                   paste0(gettext("gap"), ": ", num(sc), " [", num(bd$lo), "; ", num(bd$hi),
                          "], p = ", pv),
                   "")
  } else blank

  cond_n <- !is.na(get_n(x)) & !shows("n")
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
                out_ci, out_ctr, out_resid, out_obs, out_gap, out_n)
  out <- rep("", n)
  for (f in frags) {
    k <- !is.na(f) & nzchar(f)
    if (!any(k)) next
    out[k] <- paste0(out[k], ifelse(nzchar(out[k]), " ; ", ""), f[k])
  }

  # Phase 14r (L6): the GOF / blank footer cells carry model-fit numbers in fields never meant to be
  # compared (e.g. AIC 63 785 lives in `diff` -> a nonsense "diff: +6378526%"). No tooltip for them.
  out[disp %in% c("gof", "blank")] <- ""
  # Last Phase w: the field-name labels (ref/diff/ci/OR/n/sd/...) are gettext'd. This builder runs at
  # HTML render, NOT under with_legend_lang(), so they follow the AMBIENT locale (a French-locale user
  # gets French tooltips automatically; the per-call lang= override reaches the footer, not tooltips).
  # enc2utf8 keeps the French accents (e.g. "réf.") well-formed. English is byte-identical.
  enc2utf8(out)
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
      ~ tx_str_wrap(., wrap_cols, exdent = 0, whitespace_only = whitespace_only) |>
        stringi::stri_replace_all_regex("\n", brk)
    ) |>
    dplyr::mutate(
      dplyr::across(
        where(is.factor),
        ~ forcats::fct_relabel(
          ., ~ tx_str_wrap(.,
                                 width           = wrap_rows,
                                 exdent          = exdent,
                                 whitespace_only = whitespace_only) |>
            stringi::stri_replace_all_regex("\n", brk)
        )
      ),
      dplyr::across(
        where(is.character),
        ~ tx_str_wrap(.,
                            width           = wrap_rows,
                            exdent          = exdent,
                            whitespace_only = whitespace_only) |>
          stringi::stri_replace_all_regex("\n", brk)
      )
    )

  if (unbreakable_spaces) {
    tabs <- tabs |>
      dplyr::rename_with(
        ~ stringi::stri_replace_all_regex(., " ", unbrk)
      ) |>
      dplyr::mutate(
        dplyr::across(
          where(is.factor),
          ~ forcats::fct_relabel(., ~ stringi::stri_replace_all_regex(., " ", unbrk) )
        ),
        dplyr::across(
        where(is.character),
        ~ stringi::stri_replace_all_regex(., " ", unbrk)
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
      ~ 1L + stringi::stri_count_regex(., "\n")
    )) |>
    dplyr::rowwise() |>
    dplyr::mutate(n = max(dplyr::c_across(cols = tidyselect::everything()))) |>
    dplyr::pull("n") |> sum()

  #length(get_subtext(tabs)) +

  #length(unique(get_color(tabs)[!get_color(tabs) %in% c("", "no")])) # color legend length

  width <- tabs_with_colnames |>
    purrr::map(
      ~ stringi::stri_split_regex(., "\n") |>
        purrr::flatten_chr() |>
        stringi::stri_length() |>
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
#' @keywords internal
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
#' @keywords internal
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

      # Phase 17c: the synthetic add_n / add_pct / p-value rows are found by their STORED role, injected
      # as a temp column (grouped-transmute needs a per-group-subsettable column, not an env vector) --
      # byte-identical to the old row-label match on a materialised table, but robust to a relabelled UI.
      # Compute the flag OUTSIDE add_column: inside it, `.data` resolves to the rlang pronoun, not the table.
      .srole <- tab_row_roles(.data) %in% c("row_pct", "n", "pvalue")
      .data <- .data |>
        dplyr::select(-tidyselect::any_of(".__srole")) |>
        tibble::add_column(.__srole = .srole)

      if (length(several_displays) > 1) {
        .secondary_display <-
          dplyr::select(.data, !!!groups, ".__srole",
                        tidyselect::all_of(several_displays)) |>
          dplyr::transmute(
            secondary_display = dplyr::if_any(
              tidyselect::all_of(several_displays),
              ~ get_display(.) != dplyr::first(get_display(.))
            ) | .data$.__srole,

            secondary_display = dplyr::if_else(.data$secondary_display,
                                               true  = dplyr::row_number(),
                                               false = 0L
            )
          ) |>
          dplyr::pull("secondary_display")

      } else {
        .secondary_display <-
          dplyr::select(.data, !!!groups, ".__srole") |>
          dplyr::transmute(
            secondary_display = dplyr::if_else(
              .data$.__srole,
              true  = dplyr::row_number(),
              false = 0L
            )
          ) |>
          dplyr::pull("secondary_display")
      }

      .data <- .data |>
        dplyr::select(-tidyselect::any_of(c(".secondary_display", ".__srole"))) |>
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

    tab_restore(out, .data)

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
#' @keywords internal
rowwise.tabxplor_tab <- function(data, ...) {
  out <- NextMethod()
  out <- rlang::exec(new_grouped_tab, out, dplyr::group_data(out), !!!tab_attrs(data))
  `class<-`(out, stringi::stri_replace_first_regex(class(out), "grouped_df", "rowwise_df"))
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
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
dplyr_reconstruct.tabxplor_grouped_tab <- function(data, template) {
  out <- NextMethod()
  tab_restore(out, data)
}
# dplyr:::dplyr_reconstruct.grouped_df

#' subset method for class tabxplor_grouped_tab
#' @param x A tabxplor_grouped_tab object.
#' @param i,j Indices
#' @param drop For matrices and arrays. If TRUE the result is coerced to the lowest
#' possible dimension (see the examples). This only works for extracting elements,
#' not for the replacement.
#' @return An object of class \code{tabxplor_grouped_tab}.
#' @export
#' @keywords internal
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
#' @return An object of class \code{tabxplor_grouped_tab}.
#' @export
#' @keywords internal
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
#' @return An object of class \code{tabxplor_grouped_tab}.
#' @export
#' @keywords internal
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
#' @keywords internal
rowwise.tabxplor_grouped_tab <- function(data, ...) {
  out <- NextMethod()
  groups <- dplyr::group_data(out)

  out <- rlang::exec(new_grouped_tab, out, groups, !!!tab_attrs(data))
  `class<-`(out, stringi::stri_replace_first_regex(class(out), "grouped_df", "rowwise_df"))
}

#' summarise method for class tabxplor_grouped_tab
#' @importFrom dplyr summarise
#' @method summarise tabxplor_grouped_tab
#' @param .data A tibble of class \code{tabxplor_tab}.
#' @param ... Name-value pairs of summary functions. The name will be the name of the
#' variable in the result.
#' @param .groups Grouping structure of the result.
#' @return An object of class \code{tabxplor_grouped_tab}.
#' @export
#' @keywords internal
summarise.tabxplor_grouped_tab <- function(.data, ..., .groups = NULL) {
  out <- NextMethod()
  tab_restore(out, .data)
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
#' @keywords internal
select.tabxplor_grouped_tab <- function(.data, ...) {
  out <- NextMethod()
  tab_restore(out, .data)
}

#' rename method for class tabxplor_grouped_tab
#' @importFrom dplyr rename
#' @method rename tabxplor_grouped_tab
#' @param .data A tibble of class \code{tabxplor_tab}.
#' @param ... Use \code{new_name = old_name} to rename selected variables.
#' @return An object of class \code{tabxplor_grouped_tab}.
#' @export
#' @keywords internal
rename.tabxplor_grouped_tab <- function(.data, ...) {
  out <- NextMethod()
  tab_restore(out, .data)
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
#' @keywords internal
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
  tab_restore(out, .data)
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
#' @keywords internal
relocate.tabxplor_grouped_tab <- function(.data, ...) { #.before = NULL, .after = NULL
  out <- NextMethod()
  tab_restore(out, .data)
} # dplyr:::relocate.grouped_df












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
  rlang::exec(new_grouped_tab, gdf, groups, !!!tab_bind_attrs(x, to))
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
  rlang::exec(new_grouped_tab, gdf, groups, !!!tab_bind_attrs(x, y))
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
  "#0286b1", # oklch(0.58 0.1151 230)
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
  "#002828", # oklch(0.25 0.0423 195)   # "#001b1b", # oklch(0.20 0.0336 195)  # better for color blindness
  "#012d3f", # oklch(0.28 0.0553 230)   # "#002537", # oklch(0.25 0.0526 235)
  "#122e5d", # oklch(0.31 0.09   260)   # "#132d5c", # oklch(0.30 0.0900 261)
  "#202e7a"#,# oklch(0.34 0.13   270)   # "#17226d"#,# oklch(0.30 0.1300 270)
  #"#001c11", # oklch(0.20 0.0418 165)  # "#002115", # oklch(0.22 0.0461 165) # "#001c12", # oklch(0.20 0.0407 167)    # "#e3fcf1", # oklch(0.97 0.0300 167) 
  #"#00272d", # oklch(0.25 0.0429 210)  # "#00272d", # oklch(0.25 0.0429 210) # "#002538", # oklch(0.25 0.0543 236.97)   # "#d7efff", # oklch(0.94 0.0336 235) 
  #"#00314c", # oklch(0.30 0.0684 240)  # "#002c45", # oklch(0.28 0.0640 240) # "#002d5c", # oklch(0.30 0.0961 254.26)   # "#cee3ff", # oklch(0.91 0.0439 255) 
  #"#0d246e"#,# oklch(0.30 0.1300 265)  # "#0d246e"#,# oklch(0.30 0.1300 265) # "#243278"#,# oklch(0.35 0.12 270.4)   # "#bbccff"  # oklch(0.85 0.0733 270) 
)
default_dark_background_colors_neg <- c(
  "#292100", # oklch(0.25 0.051  95)   # "#1c1600", # oklch(0.20 0.0407 95) # "#211a00", # oklch(0.22 0.045 95) # "#1f1400", # oklch(0.2 0.0412 81.48)   # "#fff4e1", # oklch(0.97 0.0271 80) 
  "#3b2300", # oklch(0.28 0.0602 70)   # "#321c00", # oklch(0.25 0.0537 70) # "#321c00", # oklch(0.25 0.0537 70) # "#2f1d0e", # oklch(0.25 0.0374 59.56)   # "#ffe6d3", # oklch(0.94 0.0374 60) 
  "#4f2100", # oklch(0.31 0.0814 50)   # "#4c1f00", # oklch(0.30 0.0792 50) # "#441b00", # oklch(0.28 0.0738 50) # "#511900", # oklch(0.3 0.0906 41.62)   # "#ffd7c8", # oklch(0.91 0.0488 42) 
  "#720119"# # oklch(0.35 0.1401 20)   # "#6b141f"# # oklch(0.35 0.1200 20) # "#6b141f"# # oklch(0.35 0.12 19.39) # "#6c1610"#,# oklch(0.35 0.12 29)   # "#ffbaaf"#,# oklch(0.85 0.082 29)  
)

# ### Color palettes visual tests, with color blind mode ----
# source("~/github/tabxplor/dev/color_palette_tools.R", encoding = "UTF-8")
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
# 8-element slot vectors (4 over + 4 under) and pre-built once into ANSI style functions (cli), stored
# in an internal env and only rebuilt by set_color_palette(). The engine indexes them by the
# integer slot from fmt_color_slots() (1:4 = over intensities, 5:8 = under). See dev/new_colors_UI.md.
#' @keywords internal
tabxplor_palette_env <- new.env(parent = emptyenv())

# Last Phase z11 -- THE black-and-white publication palette (`theme = "print"`).
# WHY it cannot be derived from the colour palettes: converted to CIE L*, the shipped light background
# ramps are 97/93/90/82 (over) and 97/93/89/82 (under) -- THE SAME GREYSCALE RAMP -- and on the text
# channel over-1 and under-2 are both L* 62. Desaturating is exactly that conversion, so it destroys the
# direction. See dev/black_and_white_publication_palette.md SS1.
# DESIGN: CURATED, not user-tunable, and composed independently of `e$base` -- so set_color_palette()
# provably cannot alter print output. Its correctness is a MEASUREMENT (L* separation + WCAG contrast)
# that set_color_palette()'s validator (is.character && length 4) has no way to check; a formal that
# cannot enforce its own invariant would let a user silently reintroduce the very defect this cures.
# A user who wants other greys writes CSS after tab_css() -- the documented restyling contract.
#' @keywords internal
default_print_palette <- function() {
  list(
    # Typographic: every text slot is BLACK -- direction and magnitude ride the FACE (tx_palette_faces).
    # NOT NA/"": fmt_col_ann()'s `font` falls back to grey wherever text_hex is NA, which would grey
    # every coloured cell.
    text_colors     = rep("#000000", 4L),
    text_colors_neg = rep("#000000", 4L),
    # ONE ordered grey ramp, THE SAME on both sides. Greyscale cannot diverge (a diverging scale needs a
    # neutral in the middle, i.e. shading every cell mid-grey), so the fill carries its own measure's
    # MAGNITUDE only and direction is read off the cell's own bold/italic -- Bertin's rule: the ordered
    # variable for quantity, the selective one for direction. L* 96.5/90.6/83.5/74.8 (adjacent dL*
    # 5.9/7.1/8.7, all above the ~5.0 discrimination bar); black on the darkest = 10.6:1 (AAA).
    background_colors     = c("#F5F5F5", "#E4E4E4", "#D0D0D0", "#B8B8B8"),
    background_colors_neg = c("#F5F5F5", "#E4E4E4", "#D0D0D0", "#B8B8B8"),
    # The FONT stand-in where a fill is impossible (an Excel run, a ggpubr label) -- see
    # default_bg_legend_colors. The fill ramp itself is invisible as text on white, so this is a DARK
    # ramp: 4.5 / 7.0 / 10.5 / 17.4 on white.
    bg_legend_colors     = c("#767676", "#595959", "#3F3F3F", "#1A1A1A"),
    bg_legend_colors_neg = c("#767676", "#595959", "#3F3F3F", "#1A1A1A")
  )
}

# THE face fact table: the 8 slot renderings of each (family, theme) in the TYPOGRAPHIC vocabulary,
# beside the 8 hex codes. Last Phase z11.
# WHY it exists: five places used to derive "this cell is bold" from "this cell has a colour hex"
# (tx_css_render's static bold_slots rule, fmt_col_ann's `bold`, tab_xl's hard-wired bold = TRUE,
# tab_plot's hex-membership test, legend_render_line's is_bold_tok). In a palette whose every text hex
# is black they all collapse silently. The palette DECLARES the face now and the backends read it, so
# those five heuristics are gone rather than duplicated.
# The light/dark rows are today's behaviour AS DATA -- `text_*` being all-bold is exactly what makes
# tx_css_render()'s static bold_slots rule correct, which is why tx_face_decls() can treat the light
# face as the CSS baseline and emit only the divergences. Locked by test-print-palette.R.
# `semantic`: emit the face as MARKUP (<b>/<i>/<u>), not only as CSS. TRUE for print because the two
# destinations that matter -- GitHub's markdown sanitizer (strips class AND style) and an HTML -> Word
# paste (keeps character formatting, drops stylesheets) -- carry tags and nothing else.
#' @keywords internal
tx_palette_faces <- function() {
  none  <- list(bold = rep(FALSE, 8L), italic = rep(FALSE, 8L), underline = rep(FALSE, 8L),
                semantic = FALSE)
  bold8 <- list(bold = rep(TRUE,  8L), italic = rep(FALSE, 8L), underline = rep(FALSE, 8L),
                semantic = FALSE)
  list(
    text_light = bold8, text_dark = bold8, bg_light = none, bg_dark = none,
    bg_legend_light = none, bg_legend_dark = none,
    # over = BOLD, under = ITALIC (direction, a selective variable); the second intensity level
    # (slots 3-4 over / 7-8 under) adds an UNDERLINE (magnitude, ordered by convention). Slots 1&2
    # (and 3&4) share a face ON PURPOSE: typography honestly supports 2 levels per side, not 4 -- the
    # legend collapses identically-rendered break-words to match (legend_break_tokens).
    text_print = list(bold      = c(TRUE,  TRUE,  TRUE,  TRUE,  FALSE, FALSE, FALSE, FALSE),
                      italic    = c(FALSE, FALSE, FALSE, FALSE, TRUE,  TRUE,  TRUE,  TRUE ),
                      underline = c(FALSE, FALSE, TRUE,  TRUE,  FALSE, FALSE, TRUE,  TRUE ),
                      semantic  = TRUE),
    bg_print = none, bg_legend_print = none
  )
}

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

# Compose the base palettes into the 8-slot hex vectors + pre-built ANSI style functions (cli). The
# console uses 24-bit OKLCH, except in the RStudio console (no truecolor) where the curated 8-bit
# fallback is used; exports (mode = "color_code") always use the 24-bit hex.
#' @keywords internal
build_palettes <- function() {
  e <- tabxplor_palette_env
  if (is.null(e$base)) e$base <- default_palette_base()
  b <- e$base
  p <- default_print_palette()
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
    bg_legend_dark  = c(b$dark_background_colors,  b$dark_background_colors_neg),
    # Last Phase z11: the print palette reads from its OWN literal, never from `b` -- that is the
    # byte-property making set_color_palette() unable to touch print output.
    text_print      = c(p$text_colors,       p$text_colors_neg),
    bg_print        = c(p$background_colors, p$background_colors_neg),
    bg_legend_print = c(p$bg_legend_colors,  p$bg_legend_colors_neg)
  )
  e$face <- tx_palette_faces()
  bit8 <- isTRUE(Sys.getenv("RSTUDIO") == "1")
  ncol <- if (bit8) 256L else cli::num_ansi_colors()
  mk <- function(key, is_bg) {
    # z11: palette_8bit has no print key -- without the is.null guard the RStudio console would build
    # an EMPTY style list and every slot lookup would abort with "subscript out of bounds".
    src <- if (bit8 && !is.null(palette_8bit[[key]])) palette_8bit[[key]] else e$hex[[key]]
    purrr::map(src, ~ cli::make_ansi_style(., bg = is_bg, colors = ncol))
  }
  e$ansi <- list(
    text_light = mk("text_light", FALSE), text_dark = mk("text_dark", FALSE),
    bg_light   = mk("bg_light",   TRUE),  bg_dark   = mk("bg_dark",   TRUE),
    # Built so get_color_style("crayon", theme = "print") cannot error. The console never SELECTS
    # print (set_color_palette(theme=) stays light/dark/auto and the console reads a different option),
    # but a hand-set options(tabxplor.console_theme = "print") then gets a defensible answer.
    # The FACE is deliberately NOT baked in here: the console applies bold separately through
    # options(tabxplor.console_bold) -- auto-detected because RStudio draws bold wider and breaks
    # column alignment -- so baking it would double-apply and defeat that option.
    text_print = mk("text_print", FALSE), bg_print  = mk("bg_print",  TRUE)
  )
  invisible()
}

#' Define the color palette used to print \code{\link{tab}}
#' @describeIn tab_many customise the color palette used to print \code{\link{tab}}. Each palette
#' is 4 hex codes ordered faint -> strong. Provide only the ones you want to change; the OKLCH
#' defaults are used otherwise. The ANSI styles are (re)built once, not per cell.
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
#' @param html_24_bit `r lifecycle::badge("deprecated")` Inert since 2.0.0 (exports are always 24-bit).
#' @export
set_color_style <- function(type = c("text", "bg"), theme = NULL,
                            html_24_bit = NULL, custom_palette = NULL) {
  lifecycle::deprecate_soft("2.0.0", "set_color_style()", "set_color_palette()")
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

#' @describeIn tab_many get the color palette as terminal (ANSI) style functions or html codes: an
#' 8-element vector (4 over-represented intensities then 4 under-represented), indexed by the engine slot.
#' @param mode By default, \code{get_color_style} returns a list of terminal (ANSI) coloring
#' functions (the historical value \code{"crayon"}, now built with \pkg{cli}). Set to
#' \code{"color_code"} to return html color codes, or \code{"face"} to return the palette's
#' TYPOGRAPHY -- a list \code{bold} / \code{italic} / \code{underline} of 8 logicals each (plus a
#' \code{semantic} flag), which is how \code{theme = "print"} says "over-represented cells are bold,
#' under-represented ones italic". The colour palettes report bold on every text slot and nothing on
#' the background ones, i.e. exactly how they have always been drawn.
#' @param type \code{"text"} (font colour), \code{"bg"} (background fill), or \code{"bg_legend"}
#' (\code{mode = "color_code"} only): the darker FONT stand-in for the background palette, for the
#' media that cannot fill (an Excel rich-text run, a \pkg{ggpubr} text label) -- see the colour legend.
#' @param theme \code{"light"}, \code{"dark"}, or \code{"print"} (the black-and-white publication
#' palette); defaults to the current setting. The export theme \code{"auto"} resolves to
#' \code{"light"} here, a palette being always one definite thing.
#' @param ... Absorbs deprecated arguments (e.g. \code{html_24_bit}); ignored.
#' @return A list of 8 terminal (ANSI) color-style functions, a vector of 8 color html codes, or
#' (\code{mode = "face"}) the palette's typography record.
#' @export
# The public value "crayon" is frozen for back-compat (it once returned crayon functions); the styles
# are now built with cli (crayon is superseded) and stored in the internal `e$ansi` slot.
get_color_style <- function(mode = c("crayon", "color_code", "face"), type = NULL, theme = NULL, ...) {
  # Phase 14l: `type` (the palette-FAMILY selector) stays; the OPTION tabxplor.color_style_type is
  # deprecated -- it never chose a family, it globally repointed the TEXT channel into the FILL
  # palette, i.e. fill-coloured font (the CHANNEL is chosen by `color = c(text, background)`). Warn
  # once per session (deprecate_warn dedups and fires from these nested internal frames; deprecate_soft
  # keys on the USER frame, so it would be silent from pillar_shaft). Only fires for someone who set
  # the option to a non-default value -- for everyone else it is NULL now the seed write is gone.
  opt_type <- getOption("tabxplor.color_style_type")
  if (!is.null(opt_type) && !identical(opt_type, "text")) {
    lifecycle::deprecate_warn(
      "2.0.0", I('The option "tabxplor.color_style_type"'),
      details = 'The colour CHANNEL is chosen by `color = c(text, background)` (see `?tab`).')
  }
  theme <- if (is.null(theme)) tx_getOption(c("tabxplor.console_theme", "tabxplor.color_style_theme")) else theme
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
  if (is.null(e$hex) || is.null(e$face)) build_palettes()
  if (identical(mode[1], "face")) return(e$face[[key]])
  if (identical(mode[1], "crayon")) {
    # bg_legend exists only to substitute for a fill in media that have no fill; a console HAS one.
    if (identical(fam, "bg_legend")) {
      cli::cli_abort('{.arg type} {.val bg_legend} has no terminal styles: use {.code mode = "color_code"},
                      or {.arg type} {.val bg} for a real background.')
    }
    e$ansi[[key]]
  } else e$hex[[key]]
}


#Color breaks for printing fmt in tabs ------------------------------------------------

# PURPOSE: the canonical color-break representation (Phase 13a) and its accessors.
# The stored option "tabxplor.color_breaks" is a named list of the six measure scales
#   pct_diff, pct_ratio, odds_ratio, mean_diff, mean_ratio, contrib, zscore
#   (odds_ratio is the dedicated OR scale, read by the "or" colour measure in fmt_color_plan)
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
# See: dev/new_colors_UI.md ; CLAUDE.md > 2.0.0 roadmap > Phase 13a.

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
  valid <- c("pct_diff", "pct_ratio", "odds_ratio", "mean_diff", "mean_ratio", "contrib", "zscore",
             "adj_ratio", "adj_diff", "adj_diff_std")
  if (!name %in% valid) {
    cli::cli_abort(c("Unknown color-break scale {.val {name}}.",
                     "i" = "Valid scales: {.val {valid}}."))
  }
  center <- if (name %in% c("pct_ratio", "odds_ratio", "mean_ratio", "adj_ratio")) 1 else 0
  strict <- name != "contrib"
  # Which scales express their breaks in SD units. `mean_diff` is standardized only on its NULL-default
  # arm -- supplying data-unit values there is how a user asks for absolute colouring. `adj_diff_std`
  # (Last Phase z13) is standardized BY DEFINITION: it exists so an additive gap on an arbitrary-unit
  # outcome has a ladder meaning the same thing in every table, which raw units cannot express.
  std <- identical(name, "adj_diff_std")

  # NULL / empty: drop the measure, except mean_diff -> standardized default.
  if (is.null(values) || (is.numeric(values) && length(values) == 0L)) {
    if (name == "mean_diff") {
      side <- parse_color_side(c(0.2, 0.5, 0.8), name)
      return(list(center = 0, strict = TRUE, std = TRUE, over = side, under = side))
    }
    empty <- list(breaks = numeric(0), slots = integer(0))
    return(list(center = center, strict = strict, std = std, over = empty, under = empty))
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
    return(list(center = center, strict = strict, std = std, over = over, under = under))
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
  list(center = center, strict = strict, std = std, over = over, under = under)
}

#' @keywords internal
default_color_scales <- function() {
  list(
    pct_diff   = mk_color_scale("pct_diff",   c(0.05, 0.1, 0.2, 0.3)),
    pct_ratio  = mk_color_scale("pct_ratio",  list(over = c(NA, 1.5, 2, 4), under = c(NA, 1.5, 2, 4)) ),
    odds_ratio = mk_color_scale("odds_ratio", list(over = c(1.2, 1.5, 2, 4), under = c(1.2, 1.5, 2, 4)) ),
    mean_diff  = mk_color_scale("mean_diff",  NULL),
    mean_ratio = mk_color_scale("mean_ratio", list(over = c(1.2, 1.5, 2, 4), under = c(1.2, 1.5, 2, 4)) ),
    contrib    = mk_color_scale("contrib",    c(1, 2, 5, 10)),
    # Last Phase z4: the ABSOLUTE z scale, read by color = "contrib" under
    # color_signif = "guaranteed_effect" (the SPSS reading). Written in confidence levels so the
    # ladder documents itself: 95 %, 99 %, 99.99 % and (essentially) certainty -> 1.96, 2.58, 3.89, 6.
    # Unlike `contrib` (a share of the table's own chi2) these thresholds mean the same thing in every
    # table, which is the whole point of the scale.
    zscore     = mk_color_scale("zscore",     conf_level_to_z(c(0.95, 0.99, 0.9999, 1 - 2e-9))),
    # Last Phase z5: the two scales of `color = "adjustment"` / "between_groups" -- how far a model
    # estimate sits from the value it is compared to. SHARED by both measures because they score the
    # same quantity: measured on gss_simple, real between-group effect ratios land at x1.1-x1.75 and
    # adjustment gaps at x1.03-x1.12, so one ladder reads both. The multiplicative anchor is the
    # epidemiological 10 % change-in-estimate rule; the additive one is in the effect's OWN units
    # (2 / 5 / 10 / 20 points on an AME or a risk difference) -- a RELATIVE change would explode near
    # the null (measured: a +0.016 shift on a -0.026 crude AME reads as -60 %).
    adj_ratio  = mk_color_scale("adj_ratio",  list(over  = c(1.10, 1.25, 1.50, 2.00),
                                                   under = c(1.10, 1.25, 1.50, 2.00))),
    adj_diff   = mk_color_scale("adj_diff",   c(0.02, 0.05, 0.10, 0.20)),
    # Last Phase z13 (D2): the additive gap of an outcome whose units are ARBITRARY -- a gaussian beta,
    # a count AME. `adj_diff`'s absolute ladder is calibrated on a PROBABILITY (2/5/10/20 points) and
    # applying it verbatim to a beta made the reading depend on the unit: measured on the same model,
    # tvhours in minutes saturated every cell at the deepest break while the same variable in days left
    # the whole feature dark. Standardized by SD(Y) it means the same thing in every table.
    # The ladder is the probability one re-expressed in SD units: a probability's SD is at most 0.5, so
    # 2/5/10/20 points is 0.04/0.10/0.20/0.40 SD -- rounded to 0.05 at the first step, which keeps the
    # 1:2:4:8 doubling and agrees with `adj_ratio`'s x1.10 anchor (a 10 % move on a typical 0.5 SD
    # effect IS 0.05 SD). NOT Cohen's 0.2/0.5/0.8: that measures an EFFECT, while this measures the gap
    # between two effects, which z5 measured at x1.03-x1.12 -- entirely below Cohen's first break.
    adj_diff_std = mk_color_scale("adj_diff_std", c(0.05, 0.10, 0.20, 0.40))
  )
}
# odds_ratio is the dedicated OR scale (symmetric): OR colour reads it (fmt_color_plan), so pct_ratio /
# mean_ratio are free to be set asymmetrically without changing OR breaks. pct_ratio stays symmetric by
# default here as a design choice, not a constraint.


#' Set the breaks used to print colors
#' @describeIn tab_many set the breaks used to print colors.
#' @description Color breaks are a named list of the ten measure scales \code{pct_diff},
#' \code{pct_ratio}, \code{odds_ratio}, \code{mean_diff}, \code{mean_ratio}, \code{contrib},
#' \code{zscore}, \code{adj_ratio}, \code{adj_diff} and \code{adj_diff_std}. Each is
#' a vector of positive-only thresholds (the under-represented side is mirrored automatically), 1 to 5
#' values, one per color step: \code{pct_diff} colors percentage-point differences,
#' \code{pct_ratio} the relative risk (the "x2 rule"), \code{odds_ratio} the odds ratio (\code{color =
#' "OR"}; symmetric by default), \code{mean_diff} the standardized mean difference (Glass's delta) by
#' default (supply data-unit values for absolute coloring), \code{mean_ratio} the mean ratio,
#' \code{contrib} the chi2 contribution (in multiples of the mean cell contribution) and
#' \code{zscore} an absolute z scale (the adjusted standardized residual) -- the absolute scale
#' \code{color = "contrib"} switches to under \code{color_signif = "guaranteed_effect"}. Its default
#' \code{c(1.96, 2.58, 3.89, 6)} is written as \code{\link{conf_level_to_z}(c(0.95, 0.99, 0.9999,
#' 1 - 2e-9))}, and its FIRST value is re-anchored to the significance threshold at print time, so
#' the remaining ones are read as spacings from it. \code{adj_ratio}, \code{adj_diff} and
#' \code{adj_diff_std} are the
#' \code{\link{tab_reg}}-only scales of \code{color = "adjustment"} / \code{"between_groups"} --
#' how far a modelled effect sits from the observed one (or from the reference group's). Which one a
#' column reads follows the estimate's own scale: \code{adj_ratio} for a multiplicative effect (odds /
#' risk / rate ratio), \code{adj_diff} for a probability-scale marginal effect (in percentage points),
#' and \code{adj_diff_std} for an additive effect in the outcome's own units (a gaussian beta, a count
#' marginal effect), where the gap is divided by SD(Y) so the same threshold means the same thing
#' whatever unit the outcome is recorded in. An empty/\code{NULL} scale
#' drops that measure for its column type.
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
    lifecycle::deprecate_soft("2.0.0", I(paste0("set_color_breaks(", old_args[1], ")")),
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


# --- Per-table color_breaks override (Phase 13a / 17b) --------------------------------------------
# `tab(color_breaks = list(...))` validates the user scales into a PARTIAL canonical list and stores
# it as `meta$color_breaks` (set at the very END of tab()). At render time, push_color_breaks() merges
# that partial list OVER the live global option for the duration of the render, then pop restores.
# Robust by design: a missing / NULL / malformed field simply falls back to the global breaks.
# Phase 17b: color_breaks joined the carried `meta` list, so it now SURVIVES a dplyr chain between build
# and render (was dropped before -> silent global fallback; that was defect 7). Still set last, so the
# change is purely additive survival. The global set_color_breaks() option path is unchanged.

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
get_color_breaks_attr <- function(x) get_meta(x)[["color_breaks"]]

#' @keywords internal
set_color_breaks_attr <- function(x, cb) {
  if (is.null(cb)) return(x)
  if (is.list(x) && !is.data.frame(x)) return(purrr::map(x, set_color_breaks_attr, cb))
  # set_meta_field MERGES into any existing meta (vars / ci_settings / render_extras built earlier).
  set_meta_field(x, "color_breaks", cb)
}

# Install a table's color_breaks attribute as the transient global option; returns a state to restore
# with pop_color_breaks() (NULL when there is no override -> nothing to restore). Each render entry
# point calls: st <- push_color_breaks(tabs); on.exit(pop_color_breaks(st), add = TRUE).
#' @keywords internal
push_color_breaks <- function(tabs) {
  tb <- if (is.list(tabs) && !is.data.frame(tabs)) {
    if (length(tabs) >= 1L) get_color_breaks_attr(tabs[[1]]) else NULL
  } else get_color_breaks_attr(tabs)
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
# pct_breaks         <- pct_breaks     |> c(., -.)
# mean_breaks        <- mean_breaks    |> c(., 1/.)
# contrib_breaks     <- contrib_breaks |> c(., -.)
# pct_ci_breaks      <- pct_ci_breaks  |> c(., -.)
# mean_ci_breaks     <- mean_ci_breaks |> c(., -.) #then - again
#
# pct_brksup      <- pct_brksup     |> c(., -.)
# mean_brksup     <- mean_brksup    |> c(., 1/.)
# contrib_brksup  <- contrib_brksup |> c(., -.)
# pct_ci_brksup   <- pct_ci_brksup  |> c(., -.)
# mean_ci_brksup  <- mean_ci_brksup |> c(., -.) #then - again


#' Get the breaks currently used to print colors
#' @describeIn tab_many get the color breaks currently in use, in the canonical Phase-5 shape.
#' @param brk When missing, return the full named list of break scales (\code{pct_diff},
#' \code{pct_ratio}, \code{odds_ratio}, \code{mean_diff}, \code{mean_ratio}, \code{contrib}, \code{zscore}) -- the same shape
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
