# PURPOSE: The single user-facing export facade -- tab_export(x, format = ) dispatches to the
#          format-specific exporters (tab_html / tab_md / tab_xl / forest_plot), sharing ONE set of
#          argument names and defaults (the Phase 10j unification). Mirrors jmvtab_export()'s switch.
# ROLE: Phase 10j. A thin dispatcher: it forwards the canonical shared options + `...` to the chosen
#       exporter, which resolves them (resolve_export_opts). The named exporters stay exported and
#       idiomatic (`x |> tab_html()`); tab_export() is the one-entry alternative for `format = ` code.
# See: dev/tabxplor_phase10_exporters.md (Phase 10j), CLAUDE.md > 2.0.0 roadmap > Phase 10j.

#' Export a tabxplor table to Excel, HTML, Markdown, or a plot
#'
#' A single entry point that dispatches to the format-specific exporters
#' \code{\link{tab_html}} (HTML), \code{\link{tab_md}} (Markdown), \code{\link{tab_xl}} (Excel)
#' and \code{\link{forest_plot}} (a chart of the
#' estimates). They share one set of display-option names and defaults; \code{tab_export()} forwards
#' them and passes any format-specific argument through \code{...}.
#'
#' @eval tab_args_rd("tab_export")
#' @param format One of \code{"html"} (the default), \code{"md"} (Markdown), \code{"xl"} (Excel)
#'   or \code{"forest"} (a forest plot of its estimates, see \code{\link{forest_plot}}). The HTML
#'   backend engine (home-built or kableExtra) is chosen with \code{engine =} (see
#'   \code{\link{tab_html}}).
#' @param path Optional output file. For \code{"xl"} it is the workbook path; for \code{"md"} and
#'   \code{"html"} the rendered text is written to it; ignored for \code{"forest"}.
#' @param theme By default (\code{"light"}) a white table with black text; \code{"dark"} for the
#'   inverse (colours follow the theme). \code{"auto"} follows the reader's colour scheme (their OS,
#'   and any dark-mode toggle of the host page), which needs a stylesheet: it works for
#'   \code{format = "html"} and \code{"md"}, and resolves to
#'   \code{"light"} for the static \code{"xl"} backend.
#'   The black-and-white **publication** palettes render a table for a page that has no colour:
#'   \code{"print_ready"} picks the right one per table, or name it yourself --
#'   \code{"print_marks"}, \code{"print_emphasis"}, \code{"print_minimalistic"} (\code{"bw"}).
#'   See \code{\link{tab_css}} for what each of them says.
#'   Defaults to \code{getOption("tabxplor.theme")}. See \code{\link{tab_css}}.
#' @param caption A single caption / title for the table.
#' @param ... Format-specific arguments passed to the underlying exporter. Retired arguments
#'   (`color_type`, `html_24_bit`, `engine`, `html_font`, `full_width`) are caught here, reported
#'   once, and not forwarded.
#'
#' @return The value of the underlying exporter: an HTML/knitr object (\code{"html"}), a markdown
#'   string (\code{"md"}), \code{x} invisibly with the Excel file written (\code{"xl"}), or a
#'   \code{ggplot} (\code{"forest"}).
#' @export
#'
#' @examples
#' \donttest{
#' tabs <- tab(forcats::gss_cat, race, marital, pct = "row", color = "difference")
#' tab_export(tabs, "md")
#' }
tab_export <- function(x, format = c("html", "md", "xl", "forest"), path = NULL,
                       theme = NULL,
                       color = TRUE, color_legend = TRUE, lang = NULL, transpose = FALSE,
                       caption = NULL, var_names = NULL, ...) {
  format <- match.arg(format)
  # Phase 14l / 19l: a retired argument is reported ONCE here and never forwarded, so the child
  # exporter (which would catch it too) does not report it a second time for one user mistake.
  dots <- tx_deprecate_inert(rlang::list2(...), "tab_export")
  # Each backend is called through do.call() so the FILTERED dots travel: `...` may still hold a
  # retired name, which the child would report a second time (and, for `engine`, would abort on).
  fwd <- function(f, ...) do.call(f, c(list(x), rlang::list2(...), dots))
  switch(
    format,
    html = {
      cap <- caption %||% tx_knitr_opt("tab.cap")
      k <- fwd(tab_html, theme = theme,
               color = color, color_legend = color_legend, lang = lang, caption = cap,
               transpose = transpose, var_names = var_names)
      if (!is.null(path)) writeLines(as.character(k), path)
      k
    },
    md = fwd(tab_md, theme = theme,
             color = color, color_legend = color_legend, lang = lang,
             transpose = transpose, caption = caption, var_names = var_names,
             file = path),
    xl = fwd(tab_xl, path = path, theme = theme,
             color = color, color_legend = color_legend, lang = lang, transpose = transpose,
             caption = caption, var_names = var_names),
    forest = {
      if (!is.null(path))
        cli::cli_warn("{.arg path} is ignored for {.code format = \"forest\"} (returns a ggplot).")
      fwd(forest_plot, theme = theme, color = color, legend = color_legend, lang = lang,
          caption = caption)
    }
  )
}
