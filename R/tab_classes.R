# PURPOSE: S3 class definitions for tabxplor_tab/grouped_tab, dplyr method dispatch,
#   print methods, tab_kable(), tab_compact(), color palettes and breaks.
# ROLE: Ensures tabxplor tables survive dplyr operations and print with colors.
# KEY CONSTRAINTS:
#   - Every dplyr verb needs an S3 method for tabxplor_grouped_tab. Missing one = silent
#     class downgrade to plain tbl_df (attributes lost, printing breaks).
#   - The dplyr_row_slice/dplyr_col_modify/dplyr_reconstruct trio is the core mechanism.
#     Each calls lv1_group_vars() to decide: downgrade to tabxplor_tab or keep grouped.
#   - Color palettes (6 sets) and break logic live here, shared with fmt_class.R and tab_xl.R.
# See: CLAUDE.md § Design Decisions > dplyr Integration.

# Create class tabxplor_tab --------------------------------------------------------------

#' A constructor for class tabxplor_tab
#'
#' @param tabs A table, stored into a \code{\link[tibble]{tibble}} data.frame.
#' It is generally made with \code{\link{tab}}, \code{\link{tab_reg}}
#' or \code{\link{tab_plain}}.
#' @param subtext A character vector to print legend lines under the table.
#' @param test A tidy tibble storing whole-table test results (Chi2 for factor columns,
#' ANOVA F for mean columns), filled by \code{\link{tab_chi2}}.
#' @param chi2 `r lifecycle::badge("deprecated")` Soft-deprecated alias of \code{test}.
#' @param meta The table's metadata, as a single named list gathering (all optional, \code{NULL}
#' when unset):
#' \itemize{
#'   \item \code{render_extras} -- display-only intent for the base count and the \code{add_pct}
#'   companion, \code{list(n =, add_pct =)}, materialised at print/export time from this attribute
#'   rather than baked into the table.
#'   \item \code{spec} -- the table's identity, \code{list(kind =, vars =, call =)}: its \code{kind}
#'   (\code{"crosstab"} or \code{"regression"}); \code{vars}, what no column can carry
#'   (\code{list(wt =, caption =, outcomes =, var_labels =)} -- see \code{\link{set_caption}}), the rest of the
#'   variable model being derived from the declared index columns and from the columns' own
#'   \code{col_var}; and \code{call}, the producer's own recipe (a regression's model record --
#'   family, outcome, predictors, reference level, and the \code{fit_spec}
#'   \code{\link{reg_check_plots}} refits from).
#'   \item \code{empirical_tips} -- multinomial crude-companion tooltip data (a \code{tibble} keyed by
#'   column, predictor and level), set by \code{tab_reg(empirical = TRUE)}.
#'   \item \code{assumptions} -- one record PER OUTCOME, keyed by it, each holding the observed curve
#'   of every continuous predictor (weighted quantile bins of the outcome on the family's link
#'   scale, one block per \code{tab_vars} group), set by \code{\link{tab_reg}}: the data behind the
#'   sparkline -- drawn in a continuous predictor's \code{n} cell, or in the shape table below the
#'   footer -- and behind \code{\link{reg_check_plots}}'s linearity panel.
#'   \item \code{color_breaks} -- a per-table override of the colour break scales (see
#'   \code{\link{set_color_breaks}}), merged over the global option at render time.
#' }
#' \code{meta} sub-fields left \code{NULL} are dropped, so a table given nothing carries no attribute.
#' @param ... Needed to implement subclasses.
#' @param class Needed to implement subclasses.
#'
#' @return A \code{tibble} of class \code{tabxplor_tab}.
#' @export
new_tab <-
  function(tabs = tibble::tibble(), subtext = "",
           test = new_test_tibble(), chi2 = NULL,
           meta = NULL,
           ..., class = character()) {
    stopifnot(is.data.frame(tabs))

    if (!is.null(chi2)) test <- chi2

    out <- tibble::new_tibble(tabs, subtext = subtext, test = test, ...,
                              nrow = nrow(tabs), class = c(class, "tabxplor_tab"))
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

    if (!is.null(chi2)) test <- chi2

    new_tab(tabs, groups = groups,
            subtext = subtext, test = test, meta = meta,
            ...,
            class = class)
  }



# Functions to work with class tabxplor_tab ----------------------------------------------

#' Is this a tabxplor table?
#' @description
#' \code{TRUE} for a table built by \code{\link{tab}}, \code{\link{tab_reg}} or any of their
#' variants --- i.e. for a \code{tabxplor_tab} (a \code{tabxplor_grouped_tab} with `tab_vars`).
#' \code{\link{tab_structure}} answers the fuller question: what structure is it, and what can be done
#' with it.
#' @param x An object to test.
#' @return A single logical.
#' @seealso [tab_structure()], [tab_get_vars()].
#' @export
is_tab <- function(x) {
  inherits(x, "tabxplor_tab")
}

get_subtext <- purrr::attr_getter("subtext")

get_test <- function(x) attr(x, "test", exact = TRUE)

set_test <- function(x, test) {
  attr(x, "test") <- test
  x
}

# `meta` -- ONE named list gathering every table-level attribute (spec / render_extras / empirical_tips
# / assumptions / color_breaks). NULL when absent.
get_meta <- function(x) attr(x, "meta", exact = TRUE)

# Write ONE meta sub-field. Assigning NULL removes the field, and an emptied meta drops the whole
# attribute -- the "absent when unset" property.
set_meta_field <- function(x, field, value) {
  m <- get_meta(x)
  if (is.null(m)) m <- list()
  m[[field]] <- value
  m <- m[!vapply(m, is.null, logical(1))]
  attr(x, "meta") <- if (length(m)) m else NULL
  x
}

get_render_extras <- function(x) get_meta(x)[["render_extras"]]
set_render_extras <- function(x, render_extras) set_meta_field(x, "render_extras", render_extras)

# `vars` (a slot of `meta$spec`) -- what NO column can carry: the weight name, a caption, variable labels.
# The variable MODEL is NOT stored here but DERIVED from the columns (row/tab vars from the declared
# tabxplor_lvl index columns, col_vars from each fmt column's `col_var`, row roles from `row_kind`).
get_vars_attr <- function(x) get_spec(x)[["vars"]]
set_vars_attr <- function(x, vars) set_spec_field(x, "vars", vars)

get_empirical_tips <- function(x) get_meta(x)[["empirical_tips"]]

get_assumptions <- function(x) get_meta(x)[["assumptions"]]

#' Store a caption on a tabxplor table
#'
#' Records a caption/title on a \code{tabxplor_tab} that survives a dplyr pipeline (it is kept in the
#' table's \code{meta$vars$caption}, carried through every verb) and is read by the exporters
#' (\code{\link{tab_md}}, \code{\link{tab_kable}}, \code{\link{tab_xl}}) as the
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
get_caption <- function(x) get_spec(x)[["vars"]][["caption"]]
new_vars_attr <- function(wt = NA_character_, var_labels = character(0)) {
  out <- list()
  wt <- if (length(wt)) as.character(wt)[1] else NA_character_
  if (!is.na(wt) && nzchar(wt)) out$wt <- wt
  if (length(var_labels) && !is.null(names(var_labels))) {
    keep       <- !is.na(var_labels) & nzchar(names(var_labels))
    var_labels <- var_labels[keep]
    if (length(var_labels)) out$var_labels <- var_labels
  }
  out
}
# === SECTION: the ONE table-attribute carry =======================================================
# Every table-level attribute is listed HERE, once, so a new dplyr S3 method / vctrs reconciler carries
# all three by taking tab_attrs() rather than naming each by hand and dropping one it forgot.
# WARNING: `test` is ROW-BOUND (one row per subtable x col_var), so a bind must vec_rbind it -- which is
# why the vctrs reconcilers still name it explicitly and only take tab_attrs() for the rest.
#' @keywords internal
tab_attrs <- function(from) {
  list(subtext = get_subtext(from),
       test    = get_test(from),
       meta    = get_meta(from))
}

#' @keywords internal
tab_restore <- function(out, from, attrs = tab_attrs(from)) {
  if (lv1_group_vars(out)) {
    rlang::exec(new_tab, out, !!!attrs)
  } else {
    rlang::exec(new_grouped_tab, out, dplyr::group_data(out), !!!attrs)
  }
}

# THE per-sub-field merge rules of `meta`. Any field NOT listed takes the default "first non-NULL, x
# wins" (right for a display-only fact). Declaring the rest here keeps the merge loop exhaustive.
#' @keywords internal
#' @noRd
meta_bind_rules <- list(
  color_breaks = function(x, y) { m <- y %||% list(); for (s in names(x)) m[[s]] <- x[[s]]; m },
  # the table identity reconciles SLOT BY SLOT (kind / vars / call), so a bind can't drop one side's
  # recipe just because the other declared its kind first. A closure, not the bare `spec_bind` symbol:
  # this table is built at LOAD time before R/table-spec.R is sourced, so defer the reference.
  spec = function(x, y) spec_bind(x, y)
)

#' @keywords internal
tab_meta_bind <- function(mx, my) {
  if (is.null(mx) && is.null(my)) return(NULL)
  if (is.null(mx)) mx <- list()
  if (is.null(my)) my <- list()
  out <- list()
  for (nm in union(names(mx), names(my))) {
    rule <- meta_bind_rules[[nm]]
    out[[nm]] <- if (is.null(rule)) mx[[nm]] %||% my[[nm]] else rule(mx[[nm]], my[[nm]])
  }
  out <- out[!vapply(out, is.null, logical(1))]
  if (length(out)) out else NULL
}

# Rebuild a `meta` from SEVERAL inputs: reduce their metas through tab_meta_bind(), then overwrite only
# what the caller recomputes.
# WARNING: a rebuilder (tab_compact / tab_spread / ...) must call THIS, never a fresh `meta = list(...)`
# literal -- a literal silently drops every sub-field it does not name. Locked by test-meta-attr.R.
#' @keywords internal
#' @noRd
tab_meta_merge <- function(metas, ...) {
  out <- purrr::reduce(metas, tab_meta_bind, .init = NULL)
  if (is.null(out)) out <- list()
  ow <- list(...)                        # list() KEEPS NULL elements, unlike modifyList()
  for (nm in names(ow)) out[[nm]] <- ow[[nm]]
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


# Back-compat shim for `tabs$n` / `tabs[["n"]]` (and `col_pct`): both are DISPLAY-only, so the
# built tab has no such column -- reconstruct it from the Total column with a soft-deprecation (only under
# pct="row", where they were columns; pct="col" made them ROWS -> NULL).
#' @keywords internal
tabxplor_deprecated_column <- function(x, name, user_env = rlang::caller_env(2)) {
  if (length(name) != 1L || is.na(name) || !name %in% c("n", "col_pct")) return(NULL)
  re   <- get_render_extras(x)
  want <- (name == "n" && !identical(re$n %||% tx_option("n"), "no")) ||
    (name == "col_pct" && isTRUE(re$add_pct))
  if (!want) return(NULL)
  hyd  <- tryCatch(tab_materialize_extras(x, backend = "xl", pvalue = FALSE),
                   error = function(e) NULL)
  if (is.null(hyd) || !name %in% names(hyd) || nrow(hyd) != nrow(x)) return(NULL)
  lifecycle::deprecate_soft(
    "2.0.0", I(paste0("`$", name, "` on a tabxplor tab")),
    details = c(
      paste0("The `", name, "` column is now added only at display; it is reconstructed here from ",
             "the columns it summarises and will stop being reconstructed in a future version."),
      i = "Read it from the printed / exported table, or with `get_n()` on the columns themselves."),
    user_env = user_env)
  hyd[[name]]
}

#' Extract a column of a tabxplor tab (with the n/add_pct back-compat shim)
#' @param x A \code{tabxplor_tab}.
#' @param i A column name.
#' @param name For \code{$}, a column name. For \code{\link[dplyr:pull]{dplyr::pull}}, the column
#' to use to name the result -- see its documentation.
#' @param ... Passed on.
#' @return The column, or the reconstructed n/add_pct column (deprecated), or the base method's value.
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
  # Only a bare/`"..."` name of a DEPRECATED, ABSENT display-only column is intercepted; everything else
  # delegates to dplyr's pull with the quosure RE-INJECTED (`!!vq`), which preserves tidy-select's NSE
  # (a plain NextMethod() rebinds the quosure environment and breaks it).
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

# The empty-placeholder `test` tibble -- and the schema of the `test` attribute. Tidy: a new test type
# is new ROWS, never a schema change. The key is UNIFORM across both producers: `var` = which variable
# the row is about (crosstab row_var / reg predictor / "" = whole table), `col` = which column it keys
# under (a col_var / the fmt column name); `effect_size` + `es_type` ride each omnibus row as columns.
# The sub-population rides a column NAMED AFTER THE GROUPING VARIABLE, read by both arms through
# test_group_cols() -- which is why every companion column below MUST be declared here: test_group_cols()
# treats any UNdeclared column as a grouping variable, splitting the footer. Memoized: the placeholder is
# stateless and R's copy-on-modify makes the shared cached copy safe.
new_test_tibble <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) {
      cached <<- tibble::tibble(var       = character(), col         = character(), test = character(),
                                statistic = double()   , df1         = double()   ,
                                df2       = double()   , pvalue      = double()   ,
                                n         = double()   , min_e       = double()   ,
                                effect_size = double() , es_type     = character(),
                                pvalue_exact = double(),
                                # `n` is ALWAYS the raw count; `deff` is the mean design effect this
                                # row's test corrected by (NA on basis "n").
                                deff       = double(),
                                # WHICH OUTCOME this row is about (NA on a crosstab row). Declared here,
                                # not a grouping column: test_group_cols() would else split the footer.
                                outcome    = character(),
                                # WHICH SUB-POPULATION BLOCK this row keys under after a spread -- the
                                # twin of the fmt columns' own `col_group`. Declared for the same reason.
                                col_group  = character())
    }
    cached
  }
})

# All `test`-attribute display lives in R/tab-test-display.R.


#Methods to print class tabxplor_tab -----------------------------------------------------

# THE one predicate for "does options(tabxplor.print) ask for an html render?". "html" is the taught
# value; "kable" is the pre-2.0.0 synonym, kept working. Anything else prints to the console.
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
  .cb <- push_color_breaks(x); on.exit(pop_color_breaks(.cb), add = TRUE)
  if (tx_print_html()) {
    x <- tab_html(x)
    print(x)
    return(invisible(x))
  }

  x <- tab_materialize_extras(x, backend = "text", pvalue = FALSE, medium = "console")

  test_render_console(test_summary_grid(x))
  # the observed curves, in a table of their own: the console never puts them in a cell (see
  # tab_wants_shape_table). One blank line separates the two grids.
  if (tab_wants_shape_table(x, "console")) shape_render_console(x)

  rv        <- tab_render_vars(x)
  row_var   <- if (isTRUE(rv$degrade)) character(0) else rv$row_var
  n_row_var <- which(names(x) == row_var)

  out <- dplyr::mutate(x, dplyr::across(
    tidyselect::all_of(row_var),
    ~ pillar::char(as.character(.), min_chars = min_row_var)
  ))

  out <- format(out, width = width, ..., n = n, max_extra_cols = max_extra_cols,
                max_footer_lines = max_footer_lines)

  # DESIGN: THE TYPE-TAG LINE NAMES WHAT EACH COLUMN HOLDS, so the INDEX columns are blanked out of
  # it: "<fct>" says nothing a reader of a crosstab needs, while every fmt column's tag now names its
  # whole cell layout ("<row% (n)>", "<OR (row%)>" -- fmt_display_label()). Blanked in place, keeping
  # the width, so the columns stay aligned.
  # ⚠ FOUND, not counted: the header is a variable number of lines (grouped, and the regression's
  # `Outcome:` line), and a positional guess silently blanks nothing -- or the wrong line -- the day
  # one is added. The tags themselves are plain text, so this survives the ANSI styling too.
  tags <- c("<char>", "<list>", "<fct>", "<ord>", "<chr>")
  tag_re <- paste(tags, collapse = "|")     # no metacharacter among them: one alternation is enough
  head_n <- min(length(out), 8L)
  hdr <- which(vapply(out[seq_len(head_n)],
                      function(l) grepl(tag_re, l, perl = TRUE), logical(1)))
  if (length(hdr)) {
    hdr <- hdr[[1L]]
    for (tg in tags)
      out[hdr] <- gsub(tg, strrep(" ", nchar(tg)), out[hdr], fixed = TRUE)
  }


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
print.tabxplor_grouped_tab <- print.tabxplor_tab


# === SECTION: tabxplor_tabs -- the multi-table list class ========================================

#' @keywords internal
new_tabxplor_tabs <- function(x) {
  structure(x, class = c("tabxplor_tabs", "list"))
}

#' @keywords internal
as_tabxplor_tabs <- function(x) {
  if (is.list(x) && !is.data.frame(x) && !inherits(x, "tabxplor_tabs")) new_tabxplor_tabs(x) else x
}

#' Printing method for a list of tabxplor tables
#'
#' @param x A \code{tabxplor_tabs} object (the list returned by \code{\link{tab}} /
#'   \code{\link{tab}} with \code{output_list = TRUE} for a multi-table result).
#' @param ... Passed to the per-table print method.
#' @return \code{x}, invisibly.
#' @export
#' @keywords internal
print.tabxplor_tabs <- function(x, ...) {
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

#' @exportS3Method knitr::knit_print
knit_print.tabxplor_tabs <- function(x, ...) {
  knitr::knit_print(tab_html(x), ...)
}

# Without this, knitr's default auto-print escapes print()'s html, so options(tabxplor.print = "html")
# could not render a bare `tab(...)` chunk as a real table. Honours the option.
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

# The three above all hand a `tabxplor_kable` back to knit_print(), so this is where every knitted
# table lands. knitr's own knit_print.knitr_kable would emit the markup and nothing else; the `meta`
# is what carries jQuery, bootstrap and the tooltip binding into the DOCUMENT (tx_html_deps()).
# ⚠ NULL meta is fine: knitr accepts it, and the cells' title= attributes are a plain browser
# tooltip on their own.
#' @exportS3Method knitr::knit_print
knit_print.tabxplor_kable <- function(x, ...) {
  knitr::asis_output(paste0(as.character(x), "\n\n"), meta = tx_html_deps())
}



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
  tab_sum_outcome(x, tbl_header)
}

# THE OUTCOME, folded exactly as the base count is: named ONCE above the table where the table rests
# on one, and per COLUMN (the `[outcome]` suffix reg_model_col_name() writes, stripped by every
# exporter) where it rests on several. A reader of a regression table on console had nowhere to find
# what was being modelled -- the footer grid names it, but a column header never did.
#' @keywords internal
#' @noRd
tab_sum_outcome <- function(x, header) {
  lab <- gettext("Outcome")
  out <- reg_outcomes(x)
  # ⚠ the grouped method's NextMethod() reaches the plain one, so both would append it.
  if (length(out) != 1L || lab %in% names(header)) return(header)
  c(header, stats::setNames(out, lab))
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
  tab_sum_outcome(x, grouped_tbl_header)
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
#' @eval tab_args_rd("tab_html")
#' @param theme By default (\code{"light"}) a white table with black text; \code{"dark"} for a black
#' table with white text; \code{"auto"} (opt-in) to follow whoever is **reading** the table:
#' \itemize{
#'   \item in a file or a knitted document, the reader's browser decides -- their operating system,
#'     plus any dark-mode toggle of the host page (Quarto, Bootstrap 5.3, Tailwind);
#'   \item printed to the **Viewer**, your editor decides. Its webview reports the operating system
#'     rather than the editor's colour theme, so the theme is resolved in R instead (RStudio's, or
#'     Positron's, best-effort).
#' }
#' Defaults to \code{getOption("tabxplor.theme")},
#' i.e. \code{"light"} -- a dark table is always a deliberate choice.
#'
#' \code{"print_ready"}, \code{"print_marks"}, \code{"print_emphasis"} and
#' \code{"print_minimalistic"} (\code{"bw"}) are the black-and-white **publication** palettes: a
#' greyscale print loses the colour palette's direction entirely (both ramps convert to the same
#' greys), so each says it with something else --- see \code{\link{tab_css}} for what. Their typography is written as real `<b>`/`<i>`/`<u>` markup as
#' well as CSS, so it survives a stylesheet-less destination (a paste into Word, GitHub's markdown).
#' You rarely need to ask for one: any coloured table already **prints** in the first, see
#' \code{\link{tab_css}}'s `print_rules`.
#' @param css Inline the stylesheet with the table, so the output is
#' self-contained (default, from \code{getOption("tabxplor.tab_kable_css")}). Set `FALSE` in a many-table
#' document that emits \code{\link{tab_css}} once at the top -- the stylesheet is table-independent,
#' so one copy styles every table. With `FALSE` and no \code{\link{tab_css}} call, tables render
#' uncoloured.
#' @param tooltips By default, takes \code{getOption("tabxplor.tab_kable_tooltips")}
#' (\code{TRUE} unless set): html tooltips display additional informations at mouse
#' hover. Set to \code{FALSE} to discard (or set the option to \code{FALSE} once per
#' document, e.g. in a vignette or report where every table auto-prints).
#' @param popover By default, takes \code{getOption("tabxplor.kable_popover")}. When
#' `FALSE`, html tooltips are of the base kind: they can't be used with a floating table of
#' contents in \pkg{rmarkdown} documents. Set to `TRUE` for click popovers instead, which are
#' compatible with a floating toc. Both are bound automatically, in the Viewer and in a knitted
#' document alike, provided \pkg{rmarkdown} and \pkg{htmltools} are installed.
#' @param caption The table caption. For formatting, you need to use a `css`
#' with `caption{}`in rmarkdown.
# @param unbreakable_spaces Set to `FALSE` to keep normal spaces in text (auto-break).
#' @param get_data Get the transformed data instead of the html table.
#' @param ... Retired arguments, accepted and ignored with a deprecation message since 2.0.0:
#'  `color_type`, `html_24_bit`, `engine`, `html_font`, `full_width`. The table is rendered by one
#'  dependency-free `<table>` engine whose every look is a CSS class you can restyle -- font, width
#'  and colour are all \code{\link{tab_css}}'s business now.

#' @return A html table. Printing it opens it in the Viewer, on a page painted to match the table --
#' so a `theme = "dark"` table no longer sits in a white pane. Everything the cell has no room for --
#' the confidence interval, the exact p-value, the other ways of reading the same comparison
#' (difference, ratio, odds ratio), the chi-squared contribution and the base count -- is one hover
#' away, each line named after the field it shows.
#' @export
#'
#' @examples
#' \donttest{
#' tabs <- tab(forcats::gss_cat, race, marital, year, pct = "row", color = "difference")
#' tab_html(tabs, theme = "light")
#' }
tab_html <- function(tabs,
                     theme = NULL,
                     color = TRUE, tooltips = NULL, popover = NULL, color_legend = TRUE,
                     lang = NULL,
                     caption = NULL,
                     transpose = FALSE,
                     var_names = NULL,
                     get_data = FALSE,
                     wrap_rows = 35, wrap_cols = 15,
                     whitespace_only = TRUE,
                     css = NULL,
                     ...) {
  # Retired args (`color_type`/`html_24_bit`/`engine`/`html_font`/`full_width`) are absorbed by `...`,
  # warned about once, never forwarded.
  tx_deprecate_inert(rlang::list2(...), "tab_html")
  # A knitr chunk's `tab.cap` is the caption when the call gives none -- read here rather than as a
  # default argument, so knitr can stay a Suggest (tx_knitr_opt() answers NULL outside a render).
  caption <- caption %||% tx_knitr_opt("tab.cap")
  .cb <- push_color_breaks(tabs); on.exit(pop_color_breaks(.cb), add = TRUE)
  o <- resolve_export_opts(theme = theme, color = color, color_legend = color_legend,
                           transpose = transpose, var_names = var_names, allow_auto = TRUE,
                           tabs = tabs)
  theme <- o$theme
  color_legend <- o$color_legend
  compute <- c("refs", "bold")
  if (o$color) compute <- c(compute, "colors")
  tooltips <- if (is.null(tooltips)) tx_option("tab_kable_tooltips") else tooltips
  popover  <- if (is.null(popover))  tx_option("kable_popover")      else popover
  css      <- if (is.null(css))      isTRUE(tx_option("tab_kable_css")) else isTRUE(css)

  # `list_method = TRUE`: a non-mergeable list is rendered table-after-table instead of erroring.
  prep <- tab_export_prep(
    tabs, backend = "kable", list_method = TRUE, compute = compute, transpose = o$transpose,
    wrap = list(rows = wrap_rows, cols = wrap_cols, exdent = 2,
                whitespace_only = whitespace_only, unbreakable_spaces = TRUE, brk = "<br>"),
    theme = theme, var_names = o$var_names,
    color_legend = color_legend, what = "tab_html()"
  )

  parts <- purrr::map(prep$tables, function(rd) {
    subtext <- character(0)
    if (!isTRUE(rd$vars$degrade)) {
      src         <- if (is.null(rd$color_src)) rd$tab else rd$color_src
      want_legend <- color_legend && length(rd$roles$color_cols) != 0
      subtext <- rd_footer(src, "html", theme = theme[1], want_legend = want_legend,
                           subtext = rd$subtext, lang = lang, classes = TRUE)
    }
    cap <- rd_caption(rd, caption)
    render_kable_html(rd, prep$meta, subtext = subtext, caption = cap,
                      tooltips = tooltips, popover = popover, get_data = get_data)
  })

  if (get_data) return(if (length(parts) == 1L) parts[[1]] else parts)

  # the observed curves, in a table of their own: html keeps them in the base-count cell wherever
  # that works and takes this route only where it cannot (see tab_wants_shape_table).
  # tab_kable_join() already stacks the parts with a blank line between them.
  if (is_tab(tabs) && tab_wants_shape_table(tabs, "kable"))
    parts <- c(parts, list(shape_html_table(tabs)))

  # The cells carry slot CLASSES, so the theme lives entirely here. The stylesheet is table-independent
  # (see tab_css()), built once per call -- or not at all when a document emitted tab_css() itself.
  style <- if (css) tab_css(theme = theme, format = "html", style_tag = FALSE) else ""
  # `theme` rides along as an attribute so print.tabxplor_kable() can paint the Viewer's page to match --
  # and, under "auto", resolve it from the editor (the browser cannot see Positron).
  tab_kable_join(parts, css = style, theme = theme)
}

#' @rdname tab_html
#' @details `tab_kable()` is a permanent alias of `tab_html()` -- the two are identical. `tab_html()`
#'   names the output (an HTML table); `tab_kable()` is the name it had when \pkg{kableExtra} rendered it.
#' @export
tab_kable <- tab_html


#' Print a tabxplor table in html (defunct)
#'
#' @description
#' `r lifecycle::badge("defunct")`
#'
#' Removed in 2.0.0. Use [tab_html()], which renders any table -- a `tabxplor_tab` or a plain
#' data.frame -- through the shared exporter prep, with colours, tooltips and spanning headers.
#'
#' `kable_tabxplor_style()` predated `tab_html()` and never shared its machinery: it found total
#' rows and columns by matching the literal strings `"Total"` / `"Ensemble"`, so it was hardcoded to
#' English and French. Nothing in the package ever called it.
#'
#' @param tabs A data.frame.
#' @param ... Ignored.
#' @return Never returns: it errors.
#' @keywords internal
#' @export
kable_tabxplor_style <- function(tabs, ...) {
  lifecycle::deprecate_stop("2.0.0", "kable_tabxplor_style()", "tab_html()")
}




# Promote a merged sub-table's total row to its reference row when it has no explicit reference, so each
# stacked sub-table colours against its OWN total.
promote_totrow_to_refrow <- function(col) {
  in_refrow <- vctrs::field(col, "in_refrow")
  if (any(in_refrow)) return(col)             # sub-table already has a reference row
  totrow <- is_totrow(col)
  if (!any(totrow)) return(col)
  in_refrow[totrow] <- TRUE
  vctrs::field(col, "in_refrow") <- in_refrow
  col
}

# tab_stack_tables() -- row-bind a list of prepared per-row_var tables on PLAIN field-frames, without the
# per-row tabxplor_fmt reconstruction. Column order = the UNION; a table with fewer columns contributes
# NA cells under the ones it lacks (merge by name, not by list position).
tab_stack_tables <- function(tables) {
  nms  <- unique(unlist(lapply(tables, names)))
  nrows <- purrr::map_int(tables, nrow)
  cols <- purrr::map(purrr::set_names(nms, nms), function(nm) {
    # unname: list names would else be taken as outer names and error ("Can't merge the outer name ...").
    pieces <- unname(purrr::map(tables, ~ .[[nm]]))
    have   <- !purrr::map_lgl(pieces, is.null)
    if (is_fmt(pieces[have][[1]])) {
      common <- do.call(vctrs::vec_ptype_common, pieces[have])
      frames <- purrr::map(seq_along(pieces), function(i) {
        col <- if (have[[i]]) promote_totrow_to_refrow(pieces[[i]]) else
          vctrs::vec_init(common, nrows[[i]])
        fmt_data_wn(col)
      })
      meta   <- purrr::set_names(
        lapply(fmt_col_attrs, function(a) attr(common, a, exact = TRUE)), fmt_col_attrs)
      fmt_stack_frames(frames, meta)
    } else {
      pieces <- purrr::map(seq_along(pieces), function(i)
        if (have[[i]]) pieces[[i]] else vctrs::vec_init(pieces[have][[1]], nrows[[i]]))
      # Stacking several row_vars puts different variables' levels in one column, so an `ordered` class
      # would claim an order across variables that does not exist (and vctrs refuses to combine ordered
      # factors with different level sets). Drop it here; the declared `ordered` map still carries the fact.
      if (length(pieces) > 1L && any(purrr::map_lgl(pieces, is.ordered)))
        pieces <- purrr::map(pieces, function(p)
          if (is.ordered(p)) lvl_restore(factor(p, levels = levels(p), ordered = FALSE), p) else p)
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
#'   tab(c(race, rincome), marital, pct = "row", color = "difference", output_list = TRUE) |>
#'   tab_compact()
#' }
tab_compact <- function(tabs) { # pvalue_lines = FALSE
  tabs_base <- tabs

  if (is.data.frame(tabs)) {tabs <- list(tabs) |> purrr::set_names(names(tabs)[1]) }

  # An already-merged table is a no-op: it declares `compacted`, so guard explicitly or it would merge
  # a second time (col 1 "row_var" -> "levels", a new `row_var` on top).
  if (any(purrr::map_lgl(tabs, ~ isTRUE(tab_declared_vars(.)$compacted)))) return(tabs_base)

  # The structural refusals are DECLARED (TAB_OPS, R/tab-structure.R), so tab_supports(x, "compact") answers them
  # before the call. What is refused: tables that disagree about WHICH tab_vars they have (no common axis).
  if (!tab_check_structure(tabs, "compact")) return(tabs_base)
  merge_tab_vars <- tab_get_vars(tabs[[1]])$tab_vars


  subtext <- get_subtext(tabs[[1]])
  # Captured HERE while `tabs` is still the LIST: tab_stack_tables() below rebinds it to a plain tibble
  # carrying no table attributes, so this is where a `meta` sub-field could be lost (hence tab_meta_merge).
  metas_in <- purrr::map(tabs, get_meta)

  vars_merged <- new_vars_attr(
    wt        = get_vars_attr(tabs[[1]])$wt,
    var_labels = {
      vl <- do.call(c, unname(purrr::map(tabs, ~ get_vars_attr(.)[["var_labels"]])))
      if (length(vl)) vl[!duplicated(names(vl))] else character(0)
    }
  )

  tabs_chi2 <- purrr::map_df(tabs, ~get_test(.) )





  # DESIGN: when a merged sub-table has no explicit reference row, promote its total row to reference
  # (promote_totrow_to_refrow) so each stacked sub-table colours against its OWN total. The per-row_var
  # tables are row-bound on PLAIN field-frames via tab_stack_tables(), the promotion folded onto each
  # field frame there (still per sub-table, so `any(in_refrow)` stays grouped per row_var).
  # The merged table DECLARES its two-column index -- `row_var` (role "var") names per row which variable
  # that row belongs to, `levels` (role "level", `var` NA) holds the levels. `compacted` is the mere
  # presence of the "var"-role column.
  # WARNING: rename the DECLARED level column, never "column 1" -- with tab_vars the first column is a
  # sub-table variable, and `.cols = 1` would rename THAT to "levels".
  prepped <- tabs |> purrr::imap(
    ~ dplyr::rename_with(.x, ~"levels",
                         .cols = tidyselect::all_of(tab_get_vars(.x)$row_var)) |>
      dplyr::mutate(row_var = new_lvl(as.factor(.y), "var")) |>
      dplyr::relocate(tidyselect::all_of(c(merge_tab_vars, "row_var")))
  )
  tabs <- tab_stack_tables(prepped)
  # With tab_vars, the sub-table axis is the OUTER one (one sub-table per tab_var level, each holding
  # every row_var's block) -- the stack is row_var-major, so re-order. order() is stable, so each
  # table's own row order and the table order both survive inside a tab_var level.
  if (length(merge_tab_vars) > 0)
    tabs <- tabs[do.call(order, unname(lapply(merge_tab_vars,
                                              function(v) as.integer(tabs[[v]])))), ]

  # Lone total column -> drop its "_<col_var>" qualifier (via its stored `col_var`, language-independent).
  tot_i <- which(is_totcol(tabs))
  if (length(tot_i) == 1L) {
    nm <- names(tabs)[[tot_i]]
    cv <- get_col_var(tabs[[tot_i]])
    base <- if (length(cv) && nzchar(cv)) sub(paste0("_", cv, "$"), "", nm) else nm
    if (!identical(base, nm) && !base %in% names(tabs)) names(tabs)[[tot_i]] <- base
  }

  tabs <- new_tab(tabs, subtext = subtext, test = tabs_chi2,
                  meta = tab_meta_merge(
                    metas_in,
                    spec = new_spec(tab_kind(tabs[[1]]), vars = vars_merged,
                                    call = tab_call(tabs[[1]])))) |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(c(merge_tab_vars, "row_var"))))


  tabs
}


# The SINGLE display-time materializer. The built tab() is the "core" table (no base-count / add_pct
# columns, no p-value rows): it carries only the INTENT (the `test` attribute and `render_extras`), and
# this is the ONE place every DISPLAY path hydrates it. IDEMPOTENT: each spec clears what it consumed.
#' @keywords internal
#' @noRd
# ⚠ `backend` is a LAYOUT discriminator ("does the count stay a column?"), not a medium: html, md and
# plot all materialise as "text". `medium` is the real destination, for the one spec that needs it
# (reg_spark: whether a glyph run may go in a cell). NULL = unknown, and keeps the permissive route.
tab_materialize_extras <- function(tab, backend = c("text", "xl"), pvalue = TRUE,
                                   transposed = FALSE, medium = NULL) {
  backend <- match.arg(backend)

  # `ctx` reads the display intent ONCE (a spec cannot re-read it after mat_base_n clears it).
  re  <- get_render_extras(tab)
  ctx <- list(base_n = re$n %||% tx_option("n"),
              medium = medium,
              transposed = isTRUE(transposed),
              add_pct = isTRUE(re$add_pct), pvalue = isTRUE(pvalue),
              common_totrow = isTRUE(re$common_totrow), common_totrow_ref = isTRUE(re$common_totrow_ref))
  tab_materialize(tab, backend, ctx)
}

#' @keywords internal
#' @noRd
tab_materialize <- function(tab, backend, ctx) {
  for (spec in materialize_specs()) {
    if (spec$when(tab, backend, ctx)) tab <- spec$apply(tab, backend, ctx)
  }
  tab
}

# The declared inventory of display-time synthetic rows/cols. Each spec says WHEN it applies (a predicate
# over tab + backend + intent) and HOW (apply); the list NAME says what it adds. Reading this list IS the
# map of every synthetic extra and its per-backend policy.
#' @keywords internal
#' @noRd
materialize_specs <- function() list(
  # the base count + the col%/row% companions. xl keeps the real `n` COLUMN; text folds the base
  # into the Total cell, and a regression table gets a column of its own (it has no Total cell).
  # Clears the consumed render_extras intent.
  base_n = list(
    when  = function(tab, backend, ctx) !identical(ctx$base_n, "no") || ctx$add_pct,
    apply = mat_base_n),
  # A continuous predictor has no level population, so its base-count cell is empty by construction
  # -- that is where its OBSERVED SHAPE goes, as a glyph run in the cell's own display template. Runs
  # right after base_n (the columns must exist) and before the footer rows.
  # ⚠ DORMANT (22b-xviii, second round). Every curve goes to the shape table below the footer now
  # (reg_shape_table): a curve plus the range it is read against is ~26 characters, and the count
  # column paid them on every row of every medium that took this route. The writer and its spec are
  # kept whole, so putting the curve back in the cell is this one constant.
  reg_spark = list(
    when  = function(tab, backend, ctx) SPARK_IN_CELL && tab_is_reg(tab) &&
      !is.null(get_assumptions(tab)) &&
      !identical(tx_shape_table_mode(), "no") &&
      any(purrr::map_lgl(tab, ~ is_fmt(.) && get_role(.) == "n")),
    apply = function(tab, backend, ctx) mat_reg_spark(tab)),
  # A Total column whose content has moved: the count into its own column (Excel, or one per block
  # on a spread table) or nowhere at all (n = "no"), leaving a constant behind.
  total_col = list(
    when  = function(tab, backend, ctx) TRUE,
    apply = function(tab, backend, ctx) tab_drop_totcol(tab, backend, ctx$base_n)),
  # Excel-only: a composite cell's ASIDES, each split into a column of its own. Excel writes ONE raw
  # value + a numFmt per cell, so a bracket cannot survive -- the aside was simply LOST on export.
  # Runs after base_n, whose column must exist first. A mean's sd is one of these: it is an ordinary
  # `{sd}` token in the column's own template, not a hand-rolled twin.
  # ⚠ NOT under `transpose = TRUE`, although that path materialises as "xl" (for the `n` COLUMN it
  # needs): a transposed render writes the formatted STRING for every backend, Excel included, so the
  # composite survives there and splitting it would only strip the aside off the cell.
  aside_cols = list(
    when  = function(tab, backend, ctx) identical(backend, "xl") && !isTRUE(ctx$transposed),
    apply = function(tab, backend, ctx) mat_aside_cols(tab)),
  # p-value / GOF footer rows from the kept `test` attribute. tab_pvalue_lines no-ops on a regression
  # table, so a crosstab gets its chi2 row and a reg table its GOF footer.
  footer = list(
    when  = function(tab, backend, ctx) ctx$pvalue,
    apply = function(tab, backend, ctx) {
      tab <- tab_pvalue_lines(tab)
      if (tab_is_reg(tab)) tab <- reg_footer_lines(tab)
      tab
    }),
  # Collapse the redundant per-block Total rows of a compacted several-row_vars table into ONE shared
  # Total in its own group. OPT-IN via `common_totrow` (default FALSE = one Total per row_var). Run LAST,
  # so every role recomputes on the collapsed table; the core tab() object keeps every total row.
  collapse_totals = list(
    when  = function(tab, backend, ctx) isTRUE(ctx$common_totrow),
    apply = function(tab, backend, ctx)
      tab_collapse_total_rows(tab, ref_bold = isTRUE(ctx$common_totrow_ref)))
)

#' @keywords internal
#' @noRd
mat_base_n <- function(tab, backend, ctx) {
  on <- !identical(ctx$base_n, "no")
  if (tab_is_reg(tab)) {
    if (on) tab <- tab_base_n_cols(tab, ctx$base_n)
    return(set_render_extras(tab, NULL))
  }
  tab <- tab_base_n_pct(list(tab), base_n = ctx$base_n, add_pct = ctx$add_pct, backend = backend)[[1]]
  # DESIGN: WHERE the count goes is decided by how many sub-populations the table rests on, not by
  #   the backend. One, and it folds into the Total cell the reader is already looking at (text
  #   only -- Excel wants a real number in a column of its own). Several, and there is no single
  #   Total cell that could hold it: it takes one column per block, the same shape a regression
  #   table has always had, and the per-block Total columns go (tab_drop_totcol).
  if (on) {
    # ... and a PERCENTAGE table with no Total column has no cell to fold into either. That is
    # `levels = "first"` -- a battery of binary items, whose other levels were dropped after the tests
    # -- where the base count simply vanished, on every medium. It takes the column shape instead, the
    # one a regression and a spread table already use. Percentages only: a column of means never had a
    # Total column and never showed a count, so nothing is added there.
    if (tab_base_blocks(tab) > 1L || (!length(tab_row_totcols(tab)) && tab_is_pct(tab)))
      tab <- tab_base_n_cols(tab, ctx$base_n)
    else if (identical(backend, "text")) tab <- tab_fold_base_n(tab, ctx$base_n)
  }
  set_render_extras(tab, NULL)
}

# The `n`-cell route's one switch. See materialize_specs()$reg_spark for why it is off.
#' @keywords internal
#' @noRd
SPARK_IN_CELL <- FALSE

# mat_reg_spark() -- the observed shape of each continuous predictor, drawn into the base-count cell
# its row leaves empty, as a literal in that cell's display template. ONE writer for every medium:
# format() renders the run, the console pads it, Markdown prints it and the html engine upgrades it
# to an inline <svg>. Nothing rendered is ever stored -- the CURVE is the fact, so
# `options(tabxplor.shape_table = )` is a display option like every other.
# WARNING: writes a display, so it may only ever run on the EPHEMERAL materialised copy.
#' @keywords internal
#' @noRd
mat_reg_spark <- function(tab) {
  a  <- get_assumptions(tab)[[1L]]                 # this route is only ever taken with one outcome
  dv <- tab_declared_vars(tab)
  if (is.null(a) || is.null(dv$var_col) || !dv$var_col %in% names(tab)) return(tab)
  vc  <- as.character(tab[[dv$var_col]])
  lv  <- if (length(dv$row_var) && dv$row_var %in% names(tab))
    as.character(tab[[dv$row_var]]) else rep(NA_character_, nrow(tab))
  # the group a row belongs to, when the split is a ROW fact (several models, or multinomial: the
  # groups are stacked rather than spread). "" when it is a COLUMN fact, read per column below.
  rg  <- if (length(dv$tab_vars) && dv$tab_vars[[1]] %in% names(tab))
    as.character(tab[[dv$tab_vars[[1]]]]) else rep("", nrow(tab))
  for (nm in names(tab)[purrr::map_lgl(tab, ~ is_fmt(.) && get_role(.) == "n")]) {
    col <- tab[[nm]]
    d   <- get_display(col)
    cg  <- get_col_group(col)
    for (v in names(a$curves)) {
      # `linear_level` is what makes a curved predictor's SQUARED row keep its empty cell: both rows
      # carry the same `var` and both have no count, so `var` alone cannot tell them apart.
      hit <- vc == v & !is.na(lv) & lv == (a$linear_level[[v]] %||% NA_character_) &
        !grepl("{", d, fixed = TRUE)
      if (!any(hit)) next
      cu <- a$curves[[v]]
      for (i in which(hit)) {
        g  <- if (nzchar(cg)) cg else rg[[i]]
        cg2 <- cu[cu$group == g, , drop = FALSE]
        if (nrow(cg2) == 0L) cg2 <- cu[cu$group == "", , drop = FALSE]
        gl  <- if (nrow(cg2) == 0L) NA_character_ else rd_spark(cg2)
        if (is.na(gl)) next
        # the picture is read against the RANGE it is a picture of, so the two travel together
        # wherever they go -- here, and in the shape table (reg_shape_table).
        rlab <- rd_range_label(cg2, a$kind, a$family)
        # no separator on an empty cell -- there is nothing to separate it from; a non-breaking one
        # where a count is actually printed beside it.
        d[[i]] <- paste0("{n_range}", if (!is.na(get_n(col)[[i]])) "\u00a0",
                         if (nzchar(rlab)) paste0(rlab, "\u00a0"), gl)
      }
    }
    tab[[nm]] <- fmt_set_display(col, d)
  }
  tab
}

# mat_aside_cols() -- Excel cannot print a composite cell, so every ASIDE becomes a column of its own,
# directly after the one it belonged to: the shape the base count and the sd twin already had, now the
# rule for every secondary token. The source column keeps its PRIMARY alone, which is what its cell
# was already exporting -- so nothing a reader saw on screen is missing from the workbook, and the
# unit row names each half exactly once.
# THIS IS EXCEL'S paint_split(). The console splits a composite cell in place -- primary in the
# measure's colour with the stars, aside set back in grey -- and Excel splits it into columns, so the
# same three rules must hold on the new one:
#   * it carries ITS SEGMENT, brackets and all ("(n={n})", "(sigma{sd})"), which xl_fold_literals()
#     folds into the number format, so the workbook shows what the screen shows;
#   * IT KEEPS THE CELL'S READING ORDER: an aside written BEFORE the primary becomes the column
#     before it, one written after becomes the column after. A crude column's template is
#     "({base}) {est}" and the model's "{est} ({base})", so the two ESTIMATES end up side by side --
#     which is the whole point of printing them together, and what the console already shows;
#   * it takes no colour (set_color("no") -> the aside's own grey, applied in tab_xl);
#   * it takes NO SIGNIFICANCE STARS -- set_pvalue(NA), the same clearing the composite renderer does
#     for a non-primary token, with the same `resid` exception (a residual IS derived from the
#     p-value, so clearing it would blank the cell).
# WARNING: display writes -- EPHEMERAL materialised copy only.
#' @keywords internal
#' @noRd
mat_aside_cols <- function(tab) {
  splittable <- function(col) is_fmt(col) && !get_role(col) %in% c("n", "pct") && !fmt_is_aside(col)
  added <- list()                                   # source name -> the new columns, in cell order
  for (nm in names(tab)[purrr::map_lgl(tab, splittable)]) {
    col <- tab[[nm]]
    d   <- unique(get_display(col))
    d   <- d[!is.na(d) & grepl("{", d, fixed = TRUE)]
    if (!length(d)) next
    # the FULLEST template of the column, the same one its name is built from (fmt_display_label)
    tmpl <- d[[which.max(vapply(d, function(t) length(parse_display_template(t)$fields), integer(1)))]]
    seg  <- parse_display_template(tmpl)
    tok  <- fmt_resolve_scale_tokens(seg$fields, fmt_scale_row(col))
    keep <- setdiff(seq_along(tok)[-seg$primary],
                    which(tok %in% c("blank", tok[[seg$primary]])))
    if (!length(keep)) next
    for (i in keep) {
      r_src <- get_role(col) %||% ""
      new   <- set_display(col, display_segment_of(seg, i)) |> set_color("no") |>
        set_role(if (nzchar(r_src)) paste0("aside:", r_src) else "aside")
      if (!identical(tok[[i]], "resid")) new <- set_pvalue(new, NA_real_)
      if (all(is.na(get_num(new)))) next            # nothing to export: no column for it
      side <- if (i < seg$primary) "pre" else "post"
      added[[nm]][[side]] <- c(added[[nm]][[side]],
                               stats::setNames(list(new), paste0(nm, "_", tok[[i]])))
    }
    # ⚠ the primary keeps ITS OWN precision if the template named one ("{est:3}"): written braced,
    # since a bare token has nowhere to carry it.
    pdg <- seg$field_digits[[seg$primary]]
    if (length(added[[nm]])) tab[[nm]] <- fmt_set_display(
      col, if (is.na(pdg)) tok[[seg$primary]] else paste0("{", tok[[seg$primary]], ":", pdg, "}"))
  }
  if (!length(added)) return(tab)
  ord <- names(tab)
  for (nm in names(added)) {
    for (side in c("pre", "post"))
      for (k in names(added[[nm]][[side]])) tab[[k]] <- added[[nm]][[side]][[k]]
    at  <- which(ord == nm)
    ord <- append(ord, names(added[[nm]]$post), after = at)
    ord <- append(ord, names(added[[nm]]$pre),  after = at - 1L)
  }
  tab[ord]
}

# Drops the redundant per-block Total rows of a COMPACTED table when they render identically (only
# na = "drop" makes blocks differ; else keeps them all + a message). WARNING: the comparison unit is the
# whole TOTAL BLOCK (Total row + its trailing base-count `n` row), not the Total row alone -- under pct = "col"
# the Total row is always "100%" and the real base lives in the `n` row. The sweep keys on the declared
# variable column (a tab_vars table can be compacted), never the first grouping variable.
#' @keywords internal
#' @noRd
tab_collapse_total_rows <- function(tab, ref_bold = FALSE) {
  dv <- tab_declared_vars(tab)
  if (!isTRUE(dv$compacted)) return(tab)                      # a single row_var: untouched
  var_col <- dv$var_col                                       # the column naming each row's VARIABLE
  is_tot <- is_totrow(tab)
  tot    <- which(is_tot)
  if (length(tot) < 2L) return(tab)

  n_row   <- nrow(tab)
  fmt_nms <- names(tab)[purrr::map_lgl(tab, is_fmt)]

  # A block's total BLOCK = its Total row + the contiguous base-count / add_pct SUMMARY rows after it. A
  # p-value row is block-SPECIFIC, so it is NOT swept in and survives the collapse. The sweep reads the
  # STORED row role and is gated on the DECLARED variable column (not group_vars()[1], which with tab_vars
  # is the TAB_VAR), so it can never cross into the next variable's block.
  grp <- if (length(var_col) && var_col %in% names(tab)) as.character(tab[[var_col]]) else
    rep(NA_character_, n_row)
  is_summary <- tab_row_roles(tab) %in% c("n", "pct")

  block_rows <- function(i) {
    rows <- i; j <- i + 1L
    while (j <= n_row && is_summary[j] && identical(grp[j], grp[i])) { rows <- c(rows, j); j <- j + 1L }
    rows
  }
  blocks <- lapply(tot, block_rows)

  # The block signature is a KEY over the raw record fields (n / wn / pct / mean), not a rendered format()
  # pass -- so two blocks with genuinely different bases that round to the same printed cell are NOT
  # collapsed. The question is "do these blocks describe the same population?", which these fields answer.
  sig_fields <- c("n", "wn", "pct", "mean")
  sig <- vapply(blocks, function(rows)
    paste(unlist(lapply(fmt_nms, function(nm) {
      cell <- tab[[nm]][rows]
      lapply(sig_fields, function(f) vctrs::field(cell, f))
    })), collapse = "\r"),
    character(1))

  # "The SHARED population" is the SUB-population when there are tab_vars: each sub-table has its own
  # col_var marginal, so blocks are compared and collapsed WITHIN a tab_vars key, never across it.
  tv_key <- if (length(dv$tab_vars))
    do.call(paste, c(lapply(dv$tab_vars, function(v) as.character(tab[[v]])), sep = "\r")) else
      rep("", n_row)
  blk_key   <- vapply(blocks, function(rows) tv_key[[rows[[1]]]], character(1))
  blk_group <- split(seq_along(blocks), factor(blk_key, levels = unique(blk_key)))

  if (any(vapply(blk_group, function(i) length(unique(sig[i])) > 1L, logical(1)))) {
    tx_inform_once("totrows_differ", c(
      "i" = paste0("The variables have different total rows, so every total is shown: under ",
                   '{.code na = "drop"} each variable drops its own missing values.'),
      "i" = '{.code na = "keep"}, {.val drop_all} or {.val common_base} give a single total row.'))
    return(tab)
  }

  surv_blocks <- vapply(blk_group, function(i) i[[length(i)]], integer(1))  # the LAST of each group
  drop_rows <- unlist(blocks[setdiff(seq_along(blocks), surv_blocks)])
  keep <- setdiff(seq_len(n_row), drop_rows)
  out  <- tab[keep, ]                                       # global indices -> class/attrs/grouping kept

  # The shared Total gets its OWN group (a blank row_var, level "Total") after a blank-line separator, not
  # tucked under the last row_var: reassign the surviving total block to a distinct blank sentinel in the
  # grouping column and regroup, so the render-time separator (group_indices) sees it. When the total is a
  # reference for some row_var (ref_bold), mark the Total row bold (in_refrow).
  surv_pos <- match(unlist(blocks[surv_blocks]), keep)
  surv_pos <- surv_pos[!is.na(surv_pos)]
  tot_pos  <- match(vapply(blocks[surv_blocks], function(r) r[[1]], integer(1)), keep)
  tot_pos  <- tot_pos[!is.na(tot_pos)]                     # one surviving Total row per tab_vars group
  # The blank goes in the VARIABLE column; the REGROUP keeps the whole key, which with tab_vars is
  # (tab_var, row_var) -- blanking the tab_var instead would corrupt the sub-table key.
  grp_col  <- dplyr::group_vars(out)
  if (length(surv_pos) && length(var_col) && var_col %in% names(out)) {
    gc <- var_col
    if (is.factor(out[[gc]]) && !"" %in% levels(out[[gc]]))
      levels(out[[gc]]) <- c(levels(out[[gc]]), "")
    out[[gc]][surv_pos] <- ""                              # blank row_var -> its own group (Q1)
    if (length(grp_col)) out <- dplyr::group_by(out, dplyr::across(tidyselect::all_of(grp_col)))
    if (isTRUE(ref_bold)) {
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
tab_pvalue_lines <- function(tabs) {
  test_tbl <- get_test(tabs)
  if (is.null(test_tbl) || nrow(test_tbl) == 0) return(tabs)

  group_chr <- purrr::map_chr(dplyr::groups(tabs), rlang::as_name)
  gv        <- tab_get_vars(tabs)
  row_var   <- gv$row_var
  # Key the p-value rows by the table's GROUPING columns intersected with the test tibble -- the tab_vars,
  # or the declared "var"-role column for a COMPACTED table (which the test tibble keys as `var`). `disc`
  # is the table's spelling, `disc_tt` the test tibble's; they differ in exactly that one slot.
  var_col <- tab_declared_vars(tabs)$var_col
  disc    <- intersect(group_chr, c(names(test_tbl), var_col))
  disc_tt <- ifelse(disc %in% var_col, "var", disc)
  disc    <- disc[disc_tt %in% names(test_tbl)]
  disc_tt <- disc_tt[disc_tt %in% names(test_tbl)]

  first_lv  <- gv$col_vars_levels |> purrr::map_chr(~ rlang::as_name(dplyr::first(.)))
  cv_to_col <- purrr::set_names(unname(first_lv), names(first_lv))
  col_to_cv <- purrr::set_names(names(cv_to_col), unname(cv_to_col))

  disp <- test_display_rows(test_tbl, tab_anova(tabs))
  disp <- dplyr::filter(disp, .data$col %in% names(cv_to_col), !is.na(.data$pvalue))
  if (nrow(disp) == 0) return(tabs)

  # Rows in display ORDER = p-value, then effect size (STATISTIC only under test_lines = "stat"/"all").
  # The test TYPE and the effect-size MEASURE live in the row NAMES (per group, via the descriptors), so
  # the p-value CELL is the bare p. Modes: "summary" (default) = p-value + effect size; "all" = + statistic;
  # "stat" = p-value + statistic; "pvalue" = p-value only.
  mode       <- tx_option("test_lines")
  add_stat   <- mode %in% c("stat", "all")
  add_es     <- mode %in% c("all", "summary")
  row_keys   <- c("pvalue", if (add_es) "effect size", if (add_stat) "statistic")
  K          <- length(row_keys)

  gid <- function(df, cols) if (length(cols))
      do.call(paste, c(lapply(cols, function(d) as.character(df[[d]])), sep = "\r"))
    else rep("", nrow(df))
  grp_of      <- gid(tabs, disc)
  disp$.grp   <- gid(disp, disc_tt)
  # a weak chi2 with a Fisher-exact companion shows the exact p (labelled "Fisher" in the descriptor).
  has_exact   <- if (!is.null(disp[["pvalue_exact"]])) !is.na(disp$pvalue_exact) else rep(FALSE, nrow(disp))
  disp$.pshow <- if (any(has_exact)) ifelse(has_exact, disp$pvalue_exact, disp$pvalue) else disp$pvalue
  key         <- paste(disp$col, disp$.grp, sep = "\r")
  row_label_for <- function(key, g) {
    ing <- disp$.grp == g
    d   <- disp[ing, , drop = FALSE]
    # the threshold is test_weak_min_e (R/tab-test-display.R), not a second literal 5.
    weak <- !is.null(d[["min_e"]]) &&
      any(!is.na(d$min_e) & d$min_e < test_weak_min_e & !has_exact[ing])
    switch(key,
           "pvalue"      = test_pvalue_descriptor(d$test, any(has_exact[ing]), isTRUE(weak)),
           "effect size" = if (!is.null(d[["es_type"]])) test_es_measure(d$es_type) else "effect size",
           "statistic"   = "statistic")
  }

  fill_cell <- function(nm) {
    f <- fmt0(dplyr::first(get_display(tabs[[nm]])), scale = get_scale(tabs[[nm]]))
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
    if (nm == row_var) return(vapply(row_keys, row_label_for, character(1), g = g))
    i <- match(nm, disc)                                  # a grouping column: its group level
    if (!is.na(i)) return(rep(strsplit(g, "\r", fixed = TRUE)[[1]][i], K))
    rep(NA_character_, K)
  }

  tab_append_footer(tabs, grp_of, fmt_cell, nonfmt_val,
    attrs = list(subtext = get_subtext(tabs), meta = get_meta(tabs)),
    regroup = group_chr,
    footer_groups = unique(disp$.grp),   # only subtables with a displayed test get a p-value row
    row_role = function(g) dplyr::if_else(row_keys == "pvalue", "pvalue", "gof"))  # es/statistic row -> gof
}

# The regression GOF footer, as a THIN config over the shared tab_append_footer() engine (like the
# crosstab tab_pvalue_lines()). Idempotent (`test` dropped); renders nothing on a crosstab.
reg_footer_lines <- function(tabs) {
  test_tbl <- get_test(tabs)
  # the stored KIND is the "is this a reg table" guard; the dropped `test` gives idempotency.
  if (!tab_is_reg(tabs) || is.null(test_tbl) || nrow(test_tbl) == 0) return(tabs)
  spec <- reg_footer_spec()
  reg  <- test_tbl[test_tbl$test %in% names(spec), , drop = FALSE]
  if (nrow(reg) == 0) return(tabs)

  groups    <- dplyr::groups(tabs)
  group_chr <- purrr::map_chr(groups, rlang::as_name)

  nonfmt  <- names(tabs)[!purrr::map_lgl(tabs, is_fmt)]
  rlc     <- setdiff(nonfmt, group_chr)
  row_lab_col <- if (length(rlc) >= 1L) rlc[length(rlc)] else nonfmt[length(nonfmt)]

  plan <- reg_footer_plan(reg)
  K    <- if (is.null(plan)) 0L else nrow(plan)
  if (K == 0) return(tabs)
  reg$.term     <- test_key_col(reg, "var")
  footer_labels <- plan$label

  # A split table carries per-group GOF, tagged in the `test` column NAMED after the split variable (the
  # same rule the crosstab arm reads its tab_vars by): one "Model fit" block per group, else one block at
  # the end (pseudo-group ""). tab_append_footer interleaves in row order.
  gcols     <- test_group_cols(reg)
  reg_rv    <- if (!length(gcols)) rep("", nrow(reg)) else test_key_col(reg, gcols[1])
  is_split  <- any(nzchar(reg_rv))
  split_col <- if (is_split) group_chr[[1]] else NA_character_
  grp_of    <- if (is_split) as.character(tabs[[split_col]]) else rep("", nrow(tabs))

  cell_for <- function(nm, k, g) {
    pk  <- plan[k, ]
    sel <- reg$col == nm & reg$test == pk$test & reg$.term == pk$term &
      (if (is_split) reg_rv == g else TRUE)
    r <- reg[sel, , drop = FALSE]
    if (nrow(r) == 0) return(reg_blank_cell())
    if (identical(pk$kind, "gof")) reg_gof_cell(r$statistic[1], pk$digits, pk$flag)
    else                           reg_pvalue_cell(r$pvalue[1])
  }
  fmt_cell   <- function(nm, g) do.call(vctrs::vec_c, lapply(seq_len(K), cell_for, nm = nm, g = g))
  nonfmt_val <- function(nm, g)
    if (nm == row_lab_col)             footer_labels
    else if (identical(nm, split_col)) rep(g, K)
    else                               rep(gettext("Model fit"), K)

  # `test` is dropped for idempotency, but the pooled interaction rows are NOT rendered as rows -- they
  # feed the table-wide footer LINE that every backend builds AFTER materialisation, so they are the one
  # part of `test` that rides through. Re-entry stays a no-op: with only these rows left, `reg` is empty
  # above and this returns early. The whole `meta` list threads through the rebuild (the legend reads it).
  it <- test_tbl[test_tbl$test %in% reg_interaction_types(), , drop = FALSE]
  tab_append_footer(tabs, grp_of, fmt_cell, nonfmt_val,
    attrs = list(subtext = get_subtext(tabs), meta = get_meta(tabs),
                 test = if (nrow(it) > 0) it else NULL),
    regroup = group_chr,
    row_role = function(g) plan$kind)                                    # "gof"/"pvalue"
}







#' Print a tabxplor table as plot (defunct)
#'
#' @description
#' `r lifecycle::badge("defunct")`
#'
#' Removed in 2.0.0. `tab_plot()` drew a *picture of the table* as a \pkg{ggpubr} image. Use
#' [tab_html()], [tab_md()] or [tab_xl()] to export the table itself, and [forest_plot()] for a
#' chart of the numbers -- every estimate with its confidence interval, its significance and its
#' colour.
#'
#' It was the only part of the package needing \pkg{ggpubr}, \pkg{cowplot} and \pkg{gtable}, whose
#' dependency trees every user paid for; its display never matched the other backends'.
#'
#' @param tabs A data.frame.
#' @param ... Ignored.
#' @return Never returns: it errors.
#' @keywords internal
#' @export
tab_plot <- function(tabs, ...) {
  lifecycle::deprecate_stop("2.0.0", "tab_plot()", "tab_export()")
}













#' Wrap column names and character/factor variables.
#' @param tabs A `tabxplor_tab` or a `tibble` .
#' @param wrap_rows Row labels are wrapped past this width (35 by default), as prose --- on
#'   whitespace.
#' @param wrap_cols Column NAMES are wrapped past this width (15 by default). A name is a compound
#'   word, not prose, so it breaks at the seams a name is built from (`_`, `.`, `*`, and a camelCase
#'   boundary) as well as at spaces.
#' @param exdent On the second lines or more, the number or characters to use for indentation.
#' @param whitespace_only Set to `FALSE` to wrap row labels also on non whitespace characters.
#' @param unbreakable_spaces Set to `FALSE` to keep normal spaces in text (auto-break).
#' @param brk The string to use for linebreak : `\n` in text, but `<br>` in html.

#' @return The same `tabxplor_tab` or `tibble`.
#' @export
#'
#' @examples
#' \donttest{
#' tab(forcats::gss_cat, race, marital, pct = "row", color = "difference") |>
#'   tab_wrap_text(wrap_rows = 5L, wrap_cols = 8L)
#' }
#'
tab_wrap_text <- function(tabs, wrap_rows = 35L, wrap_cols = 15L, exdent = 1,
                          whitespace_only = TRUE, unbreakable_spaces = TRUE,
                          brk = "\n") {
  if (wrap_rows == Inf & wrap_cols == Inf) return(tabs)

  tabs <- tabs |>
    dplyr::rename_with(
      # a column name is a NAME, not prose: tx_wrap_name() knows the seams a compound one is built
      # from (`_`, `.`, camelCase), which stri_wrap() -- whitespace only -- could never find. Values
      # below stay on the prose wrapper; only the variable-NAME column is re-wrapped, by the exporter
      # prep, which alone knows the width its block leaves it.
      ~ tx_wrap_name(., wrap_cols, exdent = 0L, brk = brk)
    ) |>
    dplyr::mutate(
      dplyr::across(
        where(is.factor),
        ~ forcats::fct_relabel(
          ., ~ gsub("\n", brk, tx_str_wrap(., width = wrap_rows, exdent = exdent,
                                           whitespace_only = whitespace_only), fixed = TRUE)
        )
      ),
      dplyr::across(
        where(is.character),
        ~ gsub("\n", brk, tx_str_wrap(., width = wrap_rows, exdent = exdent,
                                      whitespace_only = whitespace_only), fixed = TRUE)
      )
    )

  if (unbreakable_spaces) {
    tabs <- tabs |>
      dplyr::rename_with(
        ~ gsub(" ", unbrk, ., perl = TRUE)
      ) |>
      dplyr::mutate(
        dplyr::across(
          where(is.factor),
          ~ forcats::fct_relabel(., ~ gsub(" ", unbrk, ., perl = TRUE) )
        ),
        dplyr::across(
        where(is.character),
        ~ gsub(" ", unbrk, ., perl = TRUE)
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
#' @return A list with the row count and the max character width.
#' @keywords internal
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
        dplyr::ungroup() |>
        dplyr::mutate(dplyr::across(
          tidyselect::everything(),
          format
        )),
      )

  height <- tabs_with_colnames |>
    dplyr::mutate(dplyr::across(
      tidyselect::everything(),
      ~ 1L + lengths(regmatches(., gregexpr("\n", ., perl = TRUE)))
    )) |>
    dplyr::rowwise() |>
    dplyr::mutate(n = max(dplyr::c_across(cols = tidyselect::everything()))) |>
    dplyr::pull("n") |> sum()

  width <- tabs_with_colnames |>
    purrr::map(
      ~ strsplit(., "\n", perl = TRUE) |>
        purrr::flatten_chr() |>
        nchar(type = "chars") |>
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
#'   (typically, at the end of the group). The synthetic `n` / percentage / p-value
#'   rows are found by their stored row kind, so they are kept at the same place too.
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
      several_displays <- purrr::map_lgl(
        dplyr::select(dplyr::ungroup(.data), dplyr::where(is_fmt)),
        ~ length(unique(get_display(.))) > 1
      )
      several_displays <- names(several_displays)[several_displays]

      # the synthetic n / pct / p-value rows are found by their STORED role, injected as a temp column
      # (grouped-transmute needs a subsettable column, not an env vector). Compute the flag OUTSIDE
      # add_column: inside, `.data` is the rlang pronoun, not the table.
      .srole <- tab_row_roles(.data) %in% c("pct", "n", "pvalue")
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

    if (length(groups) > 0) out <- out |> dplyr::group_by(!!!groups)

    tab_restore(out, .data)

}

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
  `class<-`(out, sub("grouped_df", "rowwise_df", class(out), perl = TRUE))
}




# === SECTION: tab coercion wall ===================================================================
# The vctrs ptype2/cast methods that keep a tabxplor_tab (the richer type) through every c()/bind with a
# tibble, data.frame or another tab. All route through tab_cast()/tab_ptype2(), which reconcile the table
# attributes via tab_bind_attrs(). One near-identical @describeIn stub per type pair follows.

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
  rlang::exec(new_tab, out, !!!tab_bind_attrs(x, y))
}


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

# Every dplyr verb used on a grouped_df needs a tabxplor_grouped_tab twin below, or the class silently
# downgrades. (.S3methods(class = "grouped_df") lists them.)

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
  # TRUE when at most one group remains -> caller downgrades grouped_tab to plain tab.
  dplyr::n_groups(tabs) <= 1

}


# DESIGN: dplyr_row_slice + dplyr_col_modify + dplyr_reconstruct are the core trio. Each: NextMethod()
# for the operation, lv1_group_vars() to decide grouped-vs-downgraded, tab_restore() to reattach
# subtext / test / meta. The `/dplyr-method` skill gates changes here.
# WARNING: they do NOT read the attributes off the same argument. row_slice / col_modify dispatch on
# `data`, so `data` is the rich object; dplyr_reconstruct dispatches on `template` and its generic strips
# `data` to names/row.names/class BEFORE dispatch -- so it MUST restore from `template`, or a bind of two
# grouped tabs silently loses subtext / test / meta. It is the ONLY carrier on the bind path: dplyr's own
# vec_ptype2.grouped_df wins once `grouped_df` is in the class vector, so the
# vec_ptype2/vec_cast.tabxplor_grouped_tab.* methods are never reached by a bind.
# WARNING: every dplyr verb a user might call needs its own method following this pattern, or the table
# silently downgrades to a plain tbl_df (losing class, attributes, coloured print). See CLAUDE.md § dplyr.
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
  # attributes come from `template` (`data` is stripped before dispatch -- see the WARNING above).
  tab_restore(out, template)
}

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
  `class<-`(out, sub("grouped_df", "rowwise_df", class(out), perl = TRUE))
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
  # `.cols` is a tidyselect selection, so it cannot go through NextMethod() (which forwards the bare
  # symbol, resolved as an external vector -- deprecated). Re-inject the quosure and dispatch by dropping
  # our own class, the same fix pull.tabxplor_tab() uses for `var`.
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
}












#' @rdname tab_cast
#' @keywords internal
# @export
gtab_cast <- function(x, to, ..., x_arg = "", to_arg = "") {
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
  common <- vctrs::df_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)
  x_vars <- dplyr::group_vars(x)
  y_vars <- dplyr::group_vars(y)
  vars <- union(x_vars, y_vars)
  drop <- dplyr::group_by_drop_default(x) && dplyr::group_by_drop_default(y)
  gdf <-  dplyr::grouped_df(common, vars, drop = drop)

  groups <- dplyr::group_data(gdf)
  rlang::exec(new_grouped_tab, gdf, groups, !!!tab_bind_attrs(x, y))
}

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
  df <- vctrs::df_cast(x, to, ...)
  vars <- dplyr::group_vars(to)
  drop <- dplyr::group_by_drop_default(to)
  dplyr::grouped_df(df, vars, drop = drop)
}

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


# The colour / chrome / typographic PALETTES all live in R/tab-palettes.R -- every literal a slot,
# a cell or a table's chrome is drawn with, and the assembly that keys them by (family, theme).
# What stays here is the user-facing SURFACE that reads and writes them.

#' Colours: palettes, styles and breaks
#' @description
#' Everything that decides what a coloured cell LOOKS like, and at which value it changes shade.
#' [set_color_palette()] sets the hues (and the console's light/dark theme); [set_color_breaks()]
#' sets the thresholds each measure is read on; [get_color_style()] and [get_color_breaks()] read
#' them back. All of them act globally, through `options()`, so one call at the top of a script
#' restyles every table it builds --- see [tabxplor-options]. A single table can override the
#' thresholds with `tab(color_breaks =)`.
#'
#' @details `set_color_palette()` customises the palette used to print \code{\link{tab}}. Each
#' palette is 4 hex codes ordered faint -> strong. Provide only the ones you want to change; the
#' OKLCH defaults are used otherwise. The ANSI styles are (re)built once, not per cell.
#' @param text_colors,text_colors_neg,background_colors,background_colors_neg Light-theme palettes
#' (4 hex each): the text (font) and background (fill) colours for the over- (\code{*_colors}) and
#' under-represented (\code{*_colors_neg}) sides.
#' @param dark_text_colors,dark_text_colors_neg,dark_background_colors,dark_background_colors_neg
#' The dark-theme counterparts (4 hex each).
#' @param bg_legend_colors,bg_legend_colors_neg (4 hex each) The FONT stand-in for
#' \code{background_colors} in the colour legend of media that cannot fill a run (Excel);
#' the defaults are the background colours at -0.2 OKLCH lightness. Setting
#' \code{background_colors} without these makes them follow it unchanged (readable only if your fills
#' already are). There is no dark counterpart: an Excel legend cell is on a white page whatever the
#' theme, and the dark fills read there as-is.
#' @param theme Which palette theme. In \code{set_color_palette()}: \code{"light"} or
#' \code{"dark"} for the console / exports, or \code{"auto"} to detect the console's colour scheme
#' now (the RStudio theme, the Positron theme, or \code{COLORFGBG}; \code{"light"} when it cannot be
#' told). Detection is best-effort and resolved ONCE: call again after changing your editor's theme.
#' (This is the console only --- \code{\link{tab_css}} / \code{\link{tab_html}} take their own
#' \code{theme = "auto"}, which follows the reader's browser.) In \code{get_color_style()}:
#' \code{"light"}, \code{"dark"} or one of the black-and-white publication palettes
#' (\code{"print_minimalistic"}, \code{"print_emphasis"}, \code{"print_marks"}); \code{"auto"}
#' resolves to \code{"light"} there, a palette being always one definite thing. Both default to the
#' current setting.
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
  # A custom background palette must not keep the DEFAULT legend hues; deriving them needs an OKLCH gamut
  # mapper (dev-only), so fall back to the fills themselves (set bg_legend_colors for a readable legend).
  if (!is.null(background_colors)     && is.null(bg_legend_colors))
    e$base$bg_legend_colors <- unname(background_colors)
  if (!is.null(background_colors_neg) && is.null(bg_legend_colors_neg))
    e$base$bg_legend_colors_neg <- unname(background_colors_neg)

  # `theme = "auto"` detects the console's colour scheme and stores the RESOLVED value (no per-print cost;
  # a mid-session theme switch needs another set_color_palette(theme = "auto")). NULL keeps the current
  # setting, detecting only if there is none.
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

# === COMPAT — deprecated colour surface wired to the new behaviour, no error ======================
# Thin shims mapping the removed 1.3.x API onto set_color_palette() / the new options / the new grammar,
# each with lifecycle::deprecate_soft().

#' Set the color style (deprecated)
#' @describeIn set_color_palette `r lifecycle::badge("deprecated")` Superseded by \code{set_color_palette()}.
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
  # `type` stays LOAD-BEARING (it routes `custom_palette` to the text vs background slot) but no longer
  # writes the deprecated `tabxplor.color_style_type` option.
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
# === end COMPAT ==================================================================================

#' @describeIn set_color_palette get the color palette as terminal (ANSI) style functions or html codes: an
#' 8-element vector (4 over-represented intensities then 4 under-represented), indexed by the engine slot.
#' @param mode By default, \code{get_color_style} returns a list of terminal (ANSI) coloring
#' functions (the historical value \code{"crayon"}, now built with \pkg{cli}). Set to
#' \code{"color_code"} to return html color codes, or \code{"face"} to return the palette's
#' TYPOGRAPHY -- a list \code{bold} / \code{italic} / \code{underline} of 8 logicals each (plus a
#' \code{semantic} flag), which is how a publication palette says "over-represented cells are bold,
#' under-represented ones italic". The colour palettes report bold on every text slot and nothing on
#' the background ones, i.e. exactly how they have always been drawn.
#' @param type Which palette, or which half of a break scale --- the word means one thing per
#' function, and both are given here because they share this page. In \code{get_color_style()} and
#' the deprecated \code{set_color_style()}: \code{"text"} (font colour), \code{"bg"} (background
#' fill), or \code{"bg_legend"} (\code{mode = "color_code"} only), the darker FONT stand-in for the
#' background palette, for media that cannot fill a run (an Excel rich-text run) -- see the colour
#' legend. In \code{get_color_breaks()}: \code{"positive"} (the default)
#' returns a readable form -- a plain vector of magnitudes when the scale is symmetric, a
#' \code{list(over =, under =)} otherwise -- and \code{"all"} the signed / reciprocal thresholds
#' the engine actually compares against (\code{c(-x, x)} for additive scales, \code{c(1/x, x)} for
#' multiplicative ones).
#' @param ... Absorbs deprecated arguments (e.g. \code{html_24_bit}); ignored.
#' @return A list of 8 terminal (ANSI) color-style functions, a vector of 8 color html codes, or
#' (\code{mode = "face"}) the palette's typography record.
#' @export
# The public value "crayon" is frozen for back-compat; the styles are now built with cli and stored in
# the internal `e$ansi` slot.
get_color_style <- function(mode = c("crayon", "color_code", "face"), type = NULL, theme = NULL, ...) {
  # `type` (the palette-FAMILY selector) stays; the CHANNEL is chosen by `color = c(text, background)`.
  theme <- if (is.null(theme)) tx_theme_option("console") else theme
  if (is.null(type)  || is.na(type[1]))  type  <- "text"
  if (is.null(theme) || is.na(theme[1])) theme <- "light"
  # a palette is always light/dark. "auto" is an EXPORT render intent that reaches here when a caller
  # forwards its own theme; resolve it here or the key "text_auto" is NULL and errors downstream.
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

# PURPOSE: the canonical color-break representation and its accessors.
#
# THE ONE IDEA: every ladder in the package is the SAME ladder, written in another measure at one
# reference cell of 50 %. 5 / 10 / 20 / 30 percentage points is also 0.1 / 0.2 / 0.4 / 0.8 SD (a
# binary variable at p = 0.5 has SD 0.5), x1.1 / x1.2 / x1.5 / x2 as a ratio and x1.2 / x1.5 / x2 / x4
# as an odds ratio. So a shade means the same size of deviation whichever measure a table is read on,
# and each ladder has exactly ONE free number -- its first rung, the smallest deviation worth
# noticing there. The rest is a shape rule (each rung about twice the previous, in the scale's own
# metric), and both are DECLARED per scale and checked at load by tx_check_color_scales().
#
# The stored option "tabxplor.color_breaks" is a named list of the settable measure scales
#   (odds_ratio is the dedicated OR scale, read by the "odds_ratio" measure in fmt_color_plan)
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
# Input forms:
#   - a plain numeric vector of SIGNED / RECIPROCAL literals: negatives (additive) or values < 1
#     (multiplicative) are the under-represented side; a one-sided vector auto-mirrors, a two-sided
#     one is used as-is. `NA` entries skip an intensity slot (one-sided vectors only).
#   - list(over =, under =): explicit per-side magnitudes, NO mirror; omit a side to switch it off
#     (e.g. list(over = 2) = the "only x2" rule). An optional `std =` says the numbers are SD units.
#   - the canonical record this function returns, so set_color_breaks(get_color_breaks()) is a no-op.
#   - NULL / empty: drop the measure for its column type -- except mean_diff = NULL, which restores
#     the standardized (Glass's delta) default.
#' @keywords internal
mk_color_scale <- function(name, values) {
  sc <- COLOR_SCALES[[name]]
  if (is.null(sc) || !isTRUE(sc$settable)) {
    cli::cli_abort(c("Unknown color-break scale {.val {name}}.",
                     "i" = "Valid scales: {.val {color_scale_names()}}."))
  }
  center <- sc$center; strict <- sc$strict; std <- sc$std

  if (is.null(values) || (is.numeric(values) && length(values) == 0L)) {
    if (!is.null(sc$null_default)) {
      side <- parse_color_side(sc$null_default$breaks, name)
      return(list(center = center, strict = strict, std = sc$null_default$std,
                  over = side, under = side))
    }
    empty <- list(breaks = numeric(0), slots = integer(0))
    return(list(center = center, strict = strict, std = std, over = empty, under = empty))
  }

  if (is.list(values)) {
    # ITS OWN CANONICAL OUTPUT, accepted so that set_color_breaks(get_color_breaks()) and
    # set_color_breaks(default_color_scales()) are true no-ops. `center` / `strict` are the row's,
    # never the record's: they are facts of the scale, not of the value.
    if (is.list(values$over) || is.list(values$under)) {
      side_rec <- function(v) {
        if (is.null(v)) return(list(breaks = numeric(0), slots = integer(0)))
        if (!is.list(v) || !all(c("breaks", "slots") %in% names(v)))
          cli::cli_abort("Color breaks {.arg {name}}: a side record needs {.field breaks} and {.field slots}.")
        list(breaks = as.double(v$breaks), slots = as.integer(v$slots))
      }
      return(list(center = center, strict = strict, std = isTRUE(values$std),
                  over = side_rec(values$over), under = side_rec(values$under)))
    }
    nms <- setdiff(names(values), "std")
    if (is.null(names(values)) || !all(nzchar(names(values))) || !all(nms %in% c("over", "under"))) {
      cli::cli_abort(c("A color scale given as a list must use {.field over} / {.field under}.",
                       "i" = 'e.g. {.code list(over = c(1.5, 2, 4))} for the over-represented side only.'))
    }
    over  <- parse_color_side(if (is.null(values$over))  numeric(0) else values$over,  name)
    under <- parse_color_side(if (is.null(values$under)) numeric(0) else values$under, name)
    # `std =` carries the ONE fact a bare vector cannot: whether these numbers are SD units. Without
    # it, round-tripping a standardized mean_diff ladder would silently make it absolute.
    return(list(center = center, strict = strict,
                std = if (is.null(values$std)) std else isTRUE(values$std),
                over = over, under = under))
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

# THE colour-break scale fact table: one row per measure scale, so adding a scale is a row (a derived
# scale too) rather than edits across mk_color_scale / default_color_scales / set_/get_color_breaks.
#   center     the neutral value the engine folds around: 0 (additive) or 1 (multiplicative).
#   strict     `>` at a break (TRUE) or `>=` (contrib, whose ladder counts multiples of the mean).
#   std        the breaks are in SD units.
#   settable   a user scale (set_color_breaks / the color_breaks argument) rather than a derived one.
#   default    the default breaks, in mk_color_scale()'s own input grammar (NULL = "use null_default").
#   null_default  what an empty/NULL value restores instead of switching the scale off.
#   quantity   WHAT the numbers measure: points / sd / relative / log_odds / z / contrib. A ladder is
#              read in its own metric -- the value itself, except `relative` and `log_odds`, read in log.
#   anchor     where the first rung came from, in prose. Every ladder is the 5-point rung written in
#              another measure at ONE reference cell of 50 %, which is why they can be compared at all.
#   sides      "mirror" (the two sides carry the same thresholds) or "asymmetric". A multiplicative
#              ladder MIRRORS unless the quantity it grades is BOUNDED ABOVE: a percentage ratio is
#              capped at 1/base, so a cell can sit far below its reference and never far above it,
#              while a mean ratio, a rate ratio and a ratio of two estimates have no ceiling.
#   bg_keep    how many LOUD rungs this ladder keeps when carried on the BACKGROUND channel (NA = all).
#              A fill is a secondary, at-a-glance voice: on `color = c("difference", "ratio")` the
#              ratio's faint rungs only restate what the text channel already says.
#   derive     for a NON-settable scale: how the plan builds it from another (`log` = log_odds_scale).
#   legacy     the pre-2.0 flat argument that set it; `alias` the short name get_color_breaks() takes.
#' @keywords internal
COLOR_SCALES <- list(
  pct_diff   = list(center = 0, strict = TRUE,  std = FALSE, settable = TRUE,
                    default = c(0.05, 0.1, 0.2, 0.3), legacy = "pct_breaks", alias = "pct",
                    quantity = "points", sides = "mirror",
                    anchor = "5 points -- the reference rung the whole package is written from"),
  # asymmetric: a percentage ratio is capped at 1/base, so the over side has a ceiling the under side
  # has not. The under rungs are the over rungs read as the same relative deviation, one rung stricter.
  pct_ratio  = list(center = 1, strict = TRUE,  std = FALSE, settable = TRUE,
                    default = list(over = c(1.1, 1.2, 1.5, 2), under = c(1.1, 1.25, 2, 4)),
                    legacy = "pct_breaks",
                    quantity = "relative", sides = "asymmetric", bg_keep = 2L,
                    anchor = "x1.1 = 5 points at a 50 % reference"),
  # odds_ratio is the dedicated OR scale (symmetric): OR colour reads it, so pct_ratio / mean_ratio can
  # be set asymmetrically without changing OR breaks. Its metric is the log-odds, which runs to
  # infinity both ways, so mirroring it is right rather than merely convenient.
  odds_ratio = list(center = 1, strict = TRUE,  std = FALSE, settable = TRUE,
                    default = list(over = c(1.2, 1.5, 2, 4), under = c(1.2, 1.5, 2, 4)),
                    quantity = "log_odds", sides = "mirror",
                    anchor = "x1.2 = 5 points at a 50 % reference (1.22, rounded)"),
  # `mean_diff` is standardized only on its NULL-default arm -- supplying data-unit values is how a user
  # asks for absolute colouring, so `std` is FALSE here and TRUE in null_default.
  mean_diff  = list(center = 0, strict = TRUE,  std = FALSE, settable = TRUE, default = NULL,
                    null_default = list(breaks = c(0.1, 0.2, 0.4, 0.8), std = TRUE),
                    quantity = "sd", sides = "mirror",
                    anchor = "0.1 SD = 5 points at a 50 % reference (Cohen's 0.2 / 0.8 kept as rungs 2 and 4)"),
  # a mean, a count and a rate are unbounded above, so this one mirrors where pct_ratio does not.
  mean_ratio = list(center = 1, strict = TRUE,  std = FALSE, settable = TRUE,
                    default = c(1.1, 1.2, 1.5, 2),
                    legacy = "mean_breaks", alias = "mean",
                    quantity = "relative", sides = "mirror", bg_keep = 2L,
                    anchor = "x1.1 = a 10 % relative deviation"),
  contrib    = list(center = 0, strict = FALSE, std = FALSE, settable = TRUE,
                    default = c(1, 2, 5, 10), legacy = "contrib_breaks",
                    quantity = "contrib", sides = "mirror",
                    anchor = "1x the mean cell contribution"),
  # the ABSOLUTE z scale, read by color = "contrib" under color_signif = "guaranteed_effect". Written in
  # confidence levels (95/99/99.99 % -> 1.96/2.58/3.89/6) so it means the same thing in every table,
  # unlike `contrib` (a share of the table's own chi2). The one ladder with its own published shape.
  zscore     = list(center = 0, strict = TRUE,  std = FALSE, settable = TRUE,
                    default = quote(conf_level_to_z(c(0.95, 0.99, 0.9999, 1 - 2e-9))),
                    quantity = "z", sides = "mirror",
                    anchor = "confidence levels -- a declared convention, exempt from the shape rule"),
  # the two scales of `color = "adjustment"` / "between_groups" -- how far a model estimate sits from
  # the value it is compared to. The multiplicative anchor is the epidemiological 10 % change-in-estimate
  # rule; the additive one is in the effect's OWN units (a RELATIVE change would explode near the null).
  adj_ratio  = list(center = 1, strict = TRUE,  std = FALSE, settable = TRUE,
                    default = list(over = c(1.10, 1.25, 1.50, 2.00),
                                   under = c(1.10, 1.25, 1.50, 2.00)),
                    quantity = "relative", sides = "mirror",
                    anchor = "x1.1 = the 10 % change-in-estimate rule (Mickey & Greenland 1989)"),
  adj_diff   = list(center = 0, strict = TRUE,  std = FALSE, settable = TRUE,
                    default = c(0.02, 0.05, 0.10, 0.20),
                    quantity = "points", sides = "mirror", anchor = "2 points of the estimate"),
  # the additive gap for an outcome whose units are ARBITRARY (a gaussian beta, a count AME): `adj_diff`'s
  # probability ladder would make the reading depend on the unit, so this one is standardized by SD(Y).
  # NOT Cohen's 0.2/0.5/0.8 (that measures an effect; this measures the gap BETWEEN two effects).
  adj_diff_std = list(center = 0, strict = TRUE, std = TRUE, settable = TRUE,
                      default = c(0.05, 0.10, 0.20, 0.40),
                      quantity = "sd", sides = "mirror", anchor = "0.05 SD of the outcome"),
  # DERIVED at plan time from a settable sibling (never stored, never user-settable): the LOG of a
  # multiplicative ladder, so set_color_breaks(odds_ratio=)/(adj_ratio=) reaches the log readings too.
  log_odds     = list(settable = FALSE, derive = list(from = "odds_ratio", how = "log")),
  adj_diff_log = list(settable = FALSE, derive = list(from = "adj_ratio",  how = "log"))
)

# the declared vocabulary of `quantity`, and which of them are read in LOG when the shape rule
# measures a ladder in "its own metric".
#' @keywords internal
COLOR_QUANTITIES <- c("points", "sd", "relative", "log_odds", "z", "contrib")
#' @keywords internal
COLOR_QUANTITIES_LOG <- c("relative", "log_odds")

#' @keywords internal
color_scale_names <- function()
  names(COLOR_SCALES)[vapply(COLOR_SCALES, function(s) isTRUE(s$settable), logical(1))]

#' @keywords internal
default_color_scales <- function() {
  purrr::map(rlang::set_names(color_scale_names()), function(nm) {
    d <- COLOR_SCALES[[nm]]$default
    mk_color_scale(nm, if (is.language(d)) eval(d) else d)
  })
}


# THE LADDERS' OWN SHAPE RULE, checked at load so a drifting default fails the install rather than a
# user's table. (Foreign keys are zzz-fact-keys.R's job; a table's own self-consistency stays beside
# the table, where its operands are in scope.)
#   K2  each rung is about twice the previous ONE IN THE SCALE'S OWN METRIC -- x1.5 to x2.5. `zscore`
#       is exempt: it declares a published convention (confidence levels), and says so in `anchor`.
#   K3  a "mirror" ladder carries the same thresholds on both sides; an "asymmetric" one is stricter
#       below and never laxer. Which a scale is follows from its `quantity` -- see the dictionary.
#' @keywords internal
COLOR_SHAPE_EXEMPT <- "zscore"

#' @keywords internal
tx_check_color_scales <- function(tbl = COLOR_SCALES) {
  bad <- function(nm, ...) stop("tabxplor: color scale `", nm, "`: ", ..., call. = FALSE)
  keys   <- names(tbl)[vapply(tbl, function(s) isTRUE(s$settable), logical(1))]
  scales <- purrr::map(rlang::set_names(keys), function(nm) {
    d <- tbl[[nm]]$default
    mk_color_scale(nm, if (is.language(d)) eval(d) else d)
  })
  for (nm in names(scales)) {
    r <- tbl[[nm]]
    if (!isTRUE(r$quantity %in% COLOR_QUANTITIES))
      bad(nm, "undeclared `quantity` (one of ", paste(COLOR_QUANTITIES, collapse = "/"), ").")
    if (!isTRUE(r$sides %in% c("mirror", "asymmetric")))
      bad(nm, "undeclared `sides` (\"mirror\" or \"asymmetric\").")
    if (!is.character(r$anchor) || !nzchar(r$anchor[1]))
      bad(nm, "no `anchor`: state where the first rung comes from.")

    ov <- scales[[nm]]$over$breaks
    un <- scales[[nm]]$under$breaks
    if (!nm %in% COLOR_SHAPE_EXEMPT && length(ov) > 1L) {
      m <- if (r$quantity %in% COLOR_QUANTITIES_LOG) log(ov) else ov
      step <- m[-1L] / m[-length(m)]
      if (any(!is.finite(step) | step < 1.5 - 1e-8 | step > 2.5 + 1e-8))
        bad(nm, "off the shape rule: rungs step by ",
            paste(sprintf("x%.2f", step), collapse = " "), " (each must be x1.5 to x2.5).")
    }
    mirrored <- identical(ov, un) && identical(scales[[nm]]$over$slots, scales[[nm]]$under$slots)
    if (identical(r$sides, "mirror") && !mirrored)
      bad(nm, "declares `sides = \"mirror\"` but its two sides differ.")
    if (identical(r$sides, "asymmetric")) {
      if (mirrored) bad(nm, "declares `sides = \"asymmetric\"` but its two sides are identical.")
      if (length(ov) == length(un) && any(un < ov))
        bad(nm, "an asymmetric ladder is stricter below, never laxer: ",
            paste(sprintf("%g<%g", un[un < ov], ov[un < ov]), collapse = " "), ".")
    }
  }
  invisible(TRUE)
}

tx_check_color_scales()

#' Set the breaks used to print colors
#' @describeIn set_color_palette set the breaks used to print colors.
#' @description Color breaks are a named list of the ten measure scales \code{pct_diff},
#' \code{pct_ratio}, \code{odds_ratio}, \code{mean_diff}, \code{mean_ratio}, \code{contrib},
#' \code{zscore}, \code{adj_ratio}, \code{adj_diff} and \code{adj_diff_std}. Each is
#' a vector of positive-only thresholds (the under-represented side is mirrored automatically), 1 to 4
#' values, one per color step.
#'
#' Every default is the same ladder in another measure, read at ONE reference cell of 50 %: 5 / 10 /
#' 20 / 30 percentage points is also 0.1 / 0.2 / 0.4 / 0.8 SD, x1.1 / x1.2 / x1.5 / x2 as a ratio and
#' x1.2 / x1.5 / x2 / x4 as an odds ratio -- so a shade means the same size of deviation whichever
#' measure a table is read on. \code{pct_diff} colors percentage-point differences,
#' \code{pct_ratio} the relative risk, \code{odds_ratio} the odds ratio (\code{color =
#' "odds_ratio"}), \code{mean_diff} the standardized mean difference (Glass's delta) by
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
#'
#' Two rules shape a default, and a custom one is free to break them. A ladder is MIRRORED unless the
#' quantity it grades is bounded above: a percentage ratio is capped at \code{1 / base}, so a cell can
#' sit far below its reference and never far above it, and \code{pct_ratio} is stricter below
#' (\code{list(over = c(1.1, 1.2, 1.5, 2), under = c(1.1, 1.25, 2, 4))}) -- a mean ratio, a rate ratio
#' and a ratio of two estimates have no ceiling and stay symmetric. And a fill is read at a glance, so
#' on the BACKGROUND channel the two ratio scales keep their two loudest rungs only: with the default
#' \code{color = TRUE} the text grades every deviation and the background flags the ones whose
#' RELATIVE size is out of proportion.
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
#' set_color_breaks(get_color_breaks())   # a no-op: the shape round-trips
set_color_breaks <- function(breaks = NULL, ...) {
  cur <- getOption("tabxplor.color_breaks")
  if (is.null(cur) || is.null(cur$pct_diff)) cur <- default_color_scales()

  dots <- list(...)
  # COMPAT: the old flat args (pct_breaks / mean_breaks / contrib_breaks) mapped onto the new scales
  # (pct_breaks splits <=1 -> pct_diff, >1 -> pct_ratio) with a soft-deprecation. The legacy names are the
  # scales' own declared `legacy` field, so this cannot drift from COLOR_SCALES.
  old_args <- intersect(names(dots), unique(unlist(purrr::map(COLOR_SCALES, "legacy"))))
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


# --- Per-table color_breaks override -------------------------------------------------------------
# `tab(color_breaks = list(...))` validates the user scales into a PARTIAL canonical list and stores it
# as `meta$color_breaks` (so it survives a dplyr chain). At render time push_color_breaks() merges that
# partial list OVER the live global option for the render, then pop restores. Robust: a missing / NULL /
# malformed field simply falls back to the global breaks.

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
  # set_meta_field MERGES into any existing meta (vars / render_extras built earlier).
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



#' Get the breaks currently used to print colors
#' @describeIn set_color_palette get the color breaks currently in use, in the canonical shape.
#' @param brk When missing, return the full named list of the ten break scales -- the same shape
#' \code{\link{set_color_breaks}} accepts, so it round-trips (an asymmetric scale comes back as
#' \code{list(over =, under =)}, a standardized one with \code{std = TRUE}). Specify one scale name
#' to return only its breaks. The old aliases \code{"pct"} (-> \code{pct_diff}) and \code{"mean"}
#' (-> \code{mean_ratio}) are still accepted.
#' @return The color breaks as a double vector or a \code{list(over =, under =)}, or a named list
#' of these.
#' @export
get_color_breaks <- function(brk, type = c("positive", "all")) {
  scales <- getOption("tabxplor.color_breaks")
  if (is.null(scales) || is.null(scales$pct_diff)) scales <- default_color_scales()

  lit_under <- function(sc) if (isTRUE(sc$center == 1)) 1 / sc$under$breaks else -sc$under$breaks

  # LOSSLESS by construction: a side whose slots are not the default for its length is written back
  # with the `NA` slot-skips that produced them, and a `std` that differs from the scale's declared
  # one is named -- so set_color_breaks(get_color_breaks()) cannot silently move a tint or turn a
  # standardized ladder into an absolute one.
  with_skips <- function(side) {
    b <- side$breaks
    if (length(b) == 0L || identical(side$slots, intensity_slots(length(b)))) return(b)
    out <- rep(NA_real_, max(side$slots)); out[side$slots] <- b; out
  }

  as_form <- function(sc, nm) {
    ob <- sc$over$breaks; ub <- sc$under$breaks
    if (identical(type[1], "all")) return(c(rev(lit_under(sc)), ob))
    if (length(ob) == 0L && length(ub) == 0L) return(numeric(0))
    ov <- with_skips(sc$over); un <- with_skips(sc$under)
    std_named <- !identical(isTRUE(sc$std), isTRUE(COLOR_SCALES[[nm]]$std))
    if (length(ub) == 0L) return(if (std_named) list(over = ov, std = sc$std) else list(over = ov))
    if (length(ob) == 0L) return(if (std_named) list(under = un, std = sc$std) else list(under = un))
    if (identical(ob, ub) && identical(sc$over$slots, sc$under$slots) && !std_named) return(ov)
    if (std_named) list(over = ov, under = un, std = sc$std) else list(over = ov, under = un)
  }

  if (missing(brk)) return(purrr::imap(scales, function(sc, nm) as_form(sc, nm)))

  # the short aliases are the scales' own declared `alias` field.
  aliases <- purrr::compact(purrr::map(COLOR_SCALES, "alias"))
  ali     <- rlang::set_names(names(aliases), unlist(aliases, use.names = FALSE))
  if (brk %in% names(ali)) brk <- unname(ali[[brk]])
  if (!brk %in% names(scales)) {
    cli::cli_abort(c("Unknown color break {.val {brk}}.",
                     "i" = "Valid scales: {.val {names(scales)}} (aliases {.val {names(ali)}})."))
  }
  as_form(scales[[brk]], brk)
}
