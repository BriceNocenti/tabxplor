# PURPOSE: The single user-facing export facade -- tab_export(x, format = ) dispatches to the four
#          format-specific exporters (tab_kable / tab_md / tab_xl / tab_plot), sharing ONE set of
#          argument names and defaults (the Phase 10j unification). Mirrors jmvtab_export()'s switch.
# ROLE: Phase 10j. A thin dispatcher: it forwards the canonical shared options + `...` to the chosen
#       exporter, which resolves them (resolve_export_opts). The four named exporters stay exported and
#       idiomatic (`x |> tab_kable()`); tab_export() is the one-entry alternative for `format = ` code.
# See: dev/tabxplor_phase10_exporters.md (Phase 10j), CLAUDE.md > 1.4.0 roadmap > Phase 10j.

#' Export a tabxplor table to Excel, HTML, Markdown, or a plot
#'
#' A single entry point that dispatches to the format-specific exporters
#' \code{\link{tab_kable}} (HTML), \code{\link{tab_md}} (Markdown), \code{\link{tab_xl}} (Excel) and
#' \code{\link{tab_plot}} (a \code{ggplot}). The four functions share one set of display-option names
#' and defaults; \code{tab_export()} forwards them and passes any format-specific argument through
#' \code{...}.
#'
#' @param x A table (or list of tables) made with \code{\link{tab}} / \code{\link{tab_many}}.
#' @param format One of \code{"kable"} (HTML, the default), \code{"md"} (Markdown),
#'   \code{"xl"} (Excel) or \code{"plot"} (a \code{ggplot}).
#' @param path Optional output file. For \code{"xl"} it is the workbook path; for \code{"md"} and
#'   \code{"kable"} the rendered text is written to it; ignored for \code{"plot"}.
#' @param theme By default (\code{"light"}) a white table with black text; \code{"dark"} for the
#'   inverse (colours follow the theme). \code{"auto"} follows the reader's colour scheme (their OS,
#'   and any dark-mode toggle of the host page), which needs a stylesheet: it works for
#'   \code{format = "kable"} with \code{engine = "html"} and for \code{"md"}, and resolves to
#'   \code{"light"} for the static \code{"xl"} / \code{"plot"} backends and the kableExtra engine.
#'   Defaults to \code{getOption("tabxplor.theme")}. See \code{\link{tab_css}}.
#' @param color_type By default the text is coloured; set to \code{"bg"} to colour the background.
#' @param html_24_bit `r lifecycle::badge("deprecated")` Inert since 1.4.0 (exports are always 24-bit).
#' @param color Set to \code{FALSE} to render without colours (monochrome).
#' @param color_legend Print the colour legend with the subtext
#'   (\code{"kable"}/\code{"md"}/\code{"xl"}/\code{"plot"}).
#' @param lang Legend language: \code{NULL} (auto from the R/OS locale, English fallback),
#'   \code{"en"} or \code{"fr"}.
#' @param transpose Set to \code{TRUE} to transpose each table before export (rows become columns) --
#'   the col-percentages-with-several-row-variables use case.
#' @param caption A single caption / title for the table.
#' @param var_names Which variable names to write beside the table: `"both"` (the default),
#'   `"rows"`, `"cols"` or `"none"`. Defaults to \code{getOption("tabxplor.var_names", "both")}.
#'   See \code{\link{tab_kable}}.
#' @param ... Format-specific arguments passed to the underlying exporter.
#'
#' @return The value of the underlying exporter: an HTML/knitr object (\code{"kable"}), a markdown
#'   string (\code{"md"}), \code{x} invisibly with the Excel file written (\code{"xl"}), or a
#'   \code{ggplot} (\code{"plot"}).
#' @export
#'
#' @examples
#' \donttest{
#' tabs <- tab(forcats::gss_cat, race, marital, pct = "row", color = "diff")
#' tab_export(tabs, "md")
#' }
tab_export <- function(x, format = c("kable", "md", "xl", "plot"), path = NULL,
                       theme = NULL, color_type = NULL, html_24_bit = NULL,
                       color = TRUE, color_legend = TRUE, lang = NULL, transpose = FALSE,
                       caption = NULL, var_names = NULL, ...) {
  format <- match.arg(format)
  switch(
    format,
    kable = {
      cap <- if (is.null(caption)) knitr::opts_current$get("tab.cap") else caption
      k <- tab_kable(x, theme = theme, color_type = color_type, html_24_bit = html_24_bit,
                     color = color, color_legend = color_legend, lang = lang, caption = cap,
                     transpose = transpose, var_names = var_names, ...)
      if (!is.null(path)) writeLines(as.character(k), path)
      k
    },
    md = tab_md(x, theme = theme, color_type = color_type, html_24_bit = html_24_bit,
                color = color, color_legend = color_legend, lang = lang,
                transpose = transpose, caption = caption, var_names = var_names,
                file = path, ...),
    xl = tab_xl(x, path = path, theme = theme, color_type = color_type, html_24_bit = html_24_bit,
                color = color, color_legend = color_legend, lang = lang, transpose = transpose,
                caption = caption, var_names = var_names, ...),
    plot = {
      if (!is.null(path)) {
        cli::cli_warn("{.arg path} is ignored for {.code format = \"plot\"} (returns a ggplot).")
      }
      tab_plot(x, theme = theme, color_type = color_type, html_24_bit = html_24_bit,
               color = color, color_legend = color_legend, lang = lang, transpose = transpose,
               caption = caption, var_names = var_names, ...)
    }
  )
}
