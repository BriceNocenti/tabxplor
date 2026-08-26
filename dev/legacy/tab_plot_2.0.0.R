# PURPOSE: the frozen source of tab_plot(), made defunct in tabxplor 2.0.0 (Phase 22l).
# ROLE: not part of the package. Kept as live, un-commented R so a future redesign can read, grep
#   and diff it. `dev/` is .Rbuildignore'd, so nothing here reaches the tarball or R CMD check.
#
# WHY IT WENT: it rendered the TABLE as a ggpubr image -- a picture of a table, never a chart -- and
#   it was the sole consumer of ggpubr, cowplot and gtable, whose dependency trees (~15 packages,
#   including rstatix, ggsci, ggsignif, polynom) every user paid for on
#   `install.packages("tabxplor", dependencies = TRUE)`. It was also the most expensive single call
#   in the package at 2.49 s. tab_html() / tab_md() / tab_xl() cover the same need; forest_plot()
#   covers the chart.
#
# TO REVIVE IT, four things have to come back besides this file:
#   1. Suggests: ggpubr (>= 0.6.0), cowplot (>= 1.1.1), gtable.
#   2. The `tabxplor.plot_num_font` option (was R/tab-options.R) and tx_num_font()'s `plot =` branch
#      (was R/tab-export-prep.R) -- both deleted with it.
#   3. "tab_plot" in the producer vectors of TAB_ARGS (R/tab-args.R): tabs, color, color_legend,
#      lang, transpose, var_names, wrap_rows, wrap_cols, whitespace_only, theme, caption.
#   4. The `plot = { ... }` branch and the "plot" value of `format` in tab_export() (R/tab-export.R).
#
# ⚠ It is written against the 2.0.0 render model (tab_export_prep(), rd_caption(), rd_footer()) and
#   against push_color_breaks()/pop_color_breaks(). Any of those may have moved since; this is a
#   starting point for a rewrite, not a drop-in.


# === SECTION: tab_plot() (was R/tab_classes.R) ====================================================

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
#' It is a PICTURE OF THE TABLE, not a chart: for a chart of the numbers -- every estimate with its
#' confidence interval, its significance and its colour -- see \code{\link{forest_plot}}.
#'
#' @eval tab_args_rd("tab_plot")
#' @param theme By default (\code{"light"}) a white table with black text; set to \code{"dark"} for a
#' black table with white text. This backend ships no stylesheet, so it does NOT take \code{"auto"}
#' (which needs one to follow the reader) -- \code{tab_html()}, \code{tab_md()} and
#' \code{\link{tab_css}} do.
#'   The black-and-white **publication** palettes render a table for a page that has no colour:
#'   \code{"print_ready"} picks the right one per table, or name it yourself --
#'   \code{"print_marks"}, \code{"print_emphasis"}, \code{"print_minimalistic"} (\code{"bw"}).
#'   See \code{\link{tab_css}} for what each of them says.
#' (ggplot2 has no underline, so on a plot a publication palette reads its magnitude off a four-step
#' grey ramp instead of the page's black ink, keeping bold and italic beside it.)
#' @param caption The table caption.
# @param unbreakable_spaces Set to `FALSE` to keep normal spaces in text (auto-break).
#' @param ... Retired arguments, accepted and ignored with a deprecation message since 2.0.0
#'   (`color_type`, `html_24_bit`).
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
#'   tab(forcats::gss_cat, race, marital, pct = "row", color = "difference") |>
#'     tab_plot()
#' }
#' }
#'
tab_plot <- function(tabs,
                     theme = NULL,
                     color = TRUE, color_legend = TRUE, lang = NULL, caption = NULL, transpose = FALSE,
                     var_names = NULL,
                     wrap_rows = 35, wrap_cols = 14, # unbreakable_spaces = TRUE
                     whitespace_only = TRUE, ...) {
  tx_deprecate_inert(rlang::list2(...), "tab_plot")
  .cb <- push_color_breaks(tabs); on.exit(pop_color_breaks(.cb), add = TRUE)
  tx_need_pkg(c("ggpubr", "gtable", "ggplot2", "cowplot"), "tab_plot()")
  if (is.list(tabs) && !is.data.frame(tabs) && length(tabs) > 1L) {
    return(purrr::map(tabs, tab_plot, theme = theme,
                      color = color, color_legend = color_legend,
                      caption = caption, transpose = transpose, wrap_rows = wrap_rows,
                      wrap_cols = wrap_cols, whitespace_only = whitespace_only))
  }

  o <- resolve_export_opts(theme = theme, color = color, color_legend = color_legend,
                           transpose = transpose, var_names = var_names, tabs = tabs)
  theme <- o$theme
  color_legend <- o$color_legend
  compute <- c("refs", "bold")
  if (o$color) compute <- c(compute, "colors")

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
  new_col_var <- rd$roles$new_col_var
  any_bg      <- rd$roles$any_bg

  refs2 <- rd$bold_rows   # bold rows (reference/total in every discriminating column)
  refs3 <- rd$bold_cols   # bold columns (all-reference columns)

  text_color  <- prep$meta$theme_cols$text
  grey_color  <- prep$meta$theme_cols$grey
  grey_color2 <- prep$meta$theme_cols$grey2

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

  # The face comes from the PALETTE (`ann$face_bold`), not from guessing at the hex, and NOT from
  # `ann$bold` (which folds in the per-CELL keep_black) -- tab_plot's structural bolding is the row/column
  # SETS refs2/refs3, kept as separate terms below. ggplot2's `fontface` has no underline, so under the
  # print palette -- where the underline IS the over direction -- an over-represented cell is told apart
  # by its ink alone here. Accepted loss: the plot backends are frozen legacy.
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

  for (cl in names(rd$roles$label_cols)) {
    if (!cl %in% names(tabs)) next
    show <- rd$roles$label_runs[[cl]]$show
    tabs[[cl]] <- as.character(tabs[[cl]])
    tabs[[cl]][!show] <- ""
  }
  # a graphics device has no block glyphs, so strip a reg row's sparkline over every text column (else
  # "conversion failure in mbcsToSbcs" and a row of garbage). Only the plot medium needs this.
  for (cl in other_cols) if (cl %in% names(tabs))
    tabs[[cl]] <- tx_spark_strip(as.character(tabs[[cl]]))

  # A monospace body font ONLY when the table SHOWS significance stars (so the stars align); a plain
  # table keeps the ggpubr default. WARNING: ggpubr exposes no per-COLUMN font, so when applied it hits
  # the WHOLE body (row labels turn monospace too) -- a small deviation confined to a starred tab_plot().
  # Revert with options("tabxplor.plot_num_font" = ""). "Cascadia Mono" must be on the graphics device.
  plot_num_font <- tx_num_font("plot", rd$roles$has_stars)
  tbody_args <- list(color = "black", size = 11, fill = "white", linewidth = 0,
                     linecolor = "black", hjust = 0.98, x = 0.95) # x/hjust = right-adjust
  if (nzchar(plot_num_font)) tbody_args$fontfamily <- plot_num_font

  tabs_gg <- tabs |>
    dplyr::mutate(
      dplyr::across(
        where(is_fmt),
        # tx_spark_strip() again: a base-count cell now carries the row sparkline in its own display
        # template, so the glyphs only exist once the fmt column has been rendered -- after the pass
        # over the text columns above.
        ~ tx_spark_strip(format(., special_formatting = TRUE,
                                .ref = ann_ref(rd$ann[[dplyr::cur_column()]])))
      ),
      dplyr::across( # otherwise, unbreakable spaces fail in some graphic devices
        where(is.factor),
        ~ forcats::fct_relabel(., ~ stringi::stri_replace_all_regex(., unbrk, " "))
      ),
      dplyr::across( # otherwise, unbreakable spaces fail in some graphic devices
        where(is.character),
        ~ stringi::stri_replace_all_regex(., unbrk, " ")
      ),
    ) |>

    ggpubr::ggtexttable(
      rows = NULL, # base_size = 11,
      theme = ggpubr::ttheme("blank",
                             padding = grid::unit(c(4, 3), "mm"), # c(h, v)
                             tbody.style = do.call(ggpubr::tbody_style, tbody_args)),
    )



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

  tabs_gg <- tabs_gg |>
    ggpubr::tab_add_border(from.row = 1, linetype = 1, linewidth = 2, linecolor = "black") |>
    ggpubr::tab_add_hline(
      at.row = unique(c(1, totrows, totrows + 1, new_group)), row.side = "bottom",
      linetype = 1, linewidth = 2, linecolor = "black",
    ) |>
    ggpubr::tab_add_vline(
      at.column = unique(c(new_col_var, totcols - 1)), column.side = "right",
      linetype = 1, linewidth = 2, linecolor = "black",
    ) |>
    ggpubr::tab_add_vline(
      at.column = unique(c(other_cols, totcols)), column.side = "left",
      linetype = 1, linewidth = 2, linecolor = "black",
     )

{
  footer_src  <- if (is.null(rd$color_src)) tabs else rd$color_src
  footer_runs <- rd_footer(footer_src, "runs", theme = theme[1],
                           want_legend = color_legend && length(color_cols) != 0,
                           subtext = subtext, lang = lang)
  # tab_plot translates the footer model's per-token typography; ggpubr has no underline, so that
  # face is dropped.
  color_legend <- purrr::map(footer_runs, function(line) {
    text   <- purrr::map_chr(line, "text")
    color  <- purrr::map_chr(line, "color")
    bold   <- purrr::map_lgl(line, ~ isTRUE(.x$bold))
    italic <- purrr::map_lgl(line, ~ isTRUE(.x$italic))
    color[is.na(color)] <- text_color
    face <- dplyr::case_when(bold & italic ~ "bold.italic", bold ~ "bold",
                             italic ~ "italic", TRUE ~ "plain")
    # fold each run of same-looking tokens (same colour AND face) into one cell, else one column per
    # token is wasteful.
    key <- paste(color, face)
    grp <- cumsum(key != dplyr::lag(key, default = ""))
    tibble::tibble(
      text  = vapply(split(text, grp), paste0, character(1), collapse = ""),
      color = color[!duplicated(grp)],
      face  = face[!duplicated(grp)]
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

      tab_legend_face <- color_legend |>
        purrr::map_dfr(
          ~ dplyr::select(., "face") |>
            dplyr::mutate(name = dplyr::row_number()) |>
            tidyr::pivot_wider( names_from = "name", values_from = "face")
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
          fc <- tab_legend_face[[j]][[i]]
          tab_legend_plot <- tab_legend_plot |> ggpubr::table_cell_font(
            row    = i + 1,
            column = j,
            color  = tab_legend_color[[j]][[i]],
            face   = if (is.na(fc)) "plain" else fc
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


  tabgrob <- get_tablegrob(tabs_gg)
  tabgrob <- justify_grob(tabgrob)
  tabs_gg <- tab_return_same_class_as_input(tabgrob, input = tabs_gg)

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


# === SECTION: the vendored ggpubr helpers (was R/utils.R) =========================================
# Every one of these was reachable only from tab_plot().

# ggpubr functions (vendored, for tab_plot() as a tableGrob) ---------------------------------------

#' @keywords internal
is_tablegrob <- function (tab) {
  inherits(tab, "gtable") & inherits(tab, "grob")
}

#' @keywords internal
is_ggtexttable <- function (tab) {
  !is.null(attr(tab, "ggtexttableGrob"))
}

#' @keywords internal
as_ggtexttable <- function (tabgrob) {
  res <- ggpubr::as_ggplot(tabgrob)
  attr(res, "ggtexttableGrob") <- tabgrob
  res
}

#' @keywords internal
get_tablegrob <- function (tab)
{
  if (is_ggtexttable(tab)) {
    tabgrob <- attr(tab, "ggtexttableGrob")
  }
  else if (is_tablegrob(tab)) {
    tabgrob <- tab
  }
  else {
    cli::cli_abort("{.arg tab} must come from {.fn ggpubr::ggtexttable} or {.fn gridExtra::tableGrob}.")
  }
  tabgrob
}

#' @keywords internal
tab_return_same_class_as_input <- function (tabgrob, input) {
  if (is_ggtexttable(input)) {
    return(as_ggtexttable(tabgrob))
  }
  else if (is_tablegrob(input)) {
    return(tabgrob)
  }
  tabgrob
}

### https://stackoverflow.com/questions/32106333/align-grob-at-fixed-top-center-position-regardless-of-size
justify_grob <- function(grob, hjust = "left", vjust = "top", pad = 5){
  w <- sum(grob$widths)
  h <- sum(grob$heights)
  xy <- list(x = switch(hjust,
                        center = 0.5 + grid::unit(pad, "points"),
                        left = 0.5*w + grid::unit(pad, "points"),
                        right = grid::unit(1,"npc") - 0.5*w - grid::unit(pad, "points")),
             y = switch(vjust,
                        center = 0.5 + grid::unit(pad, "points"),
                        bottom = 0.5*h + grid::unit(pad, "points"),
                        top = grid::unit(1,"npc") - 0.5*h - grid::unit(pad, "points") ) )
  if (is.null(grob$vp)) {
    grob$vp <- grid::viewport(x = xy[[1]], y = xy[[2]] )
  } else {
    grob$vp$x <- xy[[1]]
    grob$vp$y <- xy[[2]]
  }

  return(grob)
}
