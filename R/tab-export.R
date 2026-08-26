# PURPOSE: tab_export(x, format = ) -- the one entry point over the four exporters.
# ROLE: a dispatcher, nothing more. tab_html() / tab_md() / tab_xl() / forest_plot() stay exported
#   and idiomatic (`x |> tab_html()`); this is the alternative for code that carries the format in a
#   variable. They share one set of option names, which each of them resolves in turn.
# KEY CONSTRAINTS:
#   - A retired argument is reported ONCE, here, and never forwarded: the child exporter would catch
#     it too and report one user mistake twice. That is why the dots travel through do.call().
#   - jmvtab_export() mirrors this switch; a new format is added to both.
# See: CLAUDE.md section "tabxplor architecture" (exports and rendering).

#' Export a tabxplor table to Excel, HTML, Markdown, or a plot
#'
#' One entry point over the format-specific exporters \code{\link{tab_html}} (HTML),
#' \code{\link{tab_md}} (Markdown), \code{\link{tab_xl}} (Excel) and \code{\link{forest_plot}}
#' (a chart of the estimates). They share one set of display-option names and defaults;
#' \code{tab_export()} forwards them and passes any format-specific argument through \code{...}.
#'
#' Each exporter is also callable on its own, which reads better in a pipe
#' (\code{x |> tab_xl()}); use \code{tab_export()} when the format comes from a variable.
#'
#' @eval tab_args_rd("tab_export")
#' @param format One of \code{"html"} (the default), \code{"md"} (Markdown), \code{"xl"} (Excel)
#'   or \code{"forest"} (a forest plot of the estimates, see \code{\link{forest_plot}}).
#' @param path Optional output file. For \code{"xl"} it is the workbook path; for \code{"md"} and
#'   \code{"html"} the rendered text is written to it; ignored for \code{"forest"}.
#' @param theme By default (\code{"light"}) a white table with black text; \code{"dark"} for the
#'   inverse (the colours follow the theme). \code{"auto"} follows the reader's colour scheme
#'   (their operating system, and any dark-mode toggle of the host page); it needs a stylesheet,
#'   so it works for \code{format = "html"} and \code{"md"} and resolves to \code{"light"} for the
#'   static \code{"xl"} backend.
#'   The black-and-white **publication** palettes render a table for a page that has no colour:
#'   \code{"print_ready"} picks the right one per table, or name it yourself --
#'   \code{"print_marks"}, \code{"print_emphasis"}, \code{"print_minimalistic"} (\code{"bw"}).
#'   Defaults to \code{getOption("tabxplor.theme")}. See \code{\link{tab_css}} for what each says.
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
  dots <- tx_deprecate_inert(rlang::list2(...), "tab_export")
  # do.call() so the FILTERED dots travel -- `...` may still hold a retired name.
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
